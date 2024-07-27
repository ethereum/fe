use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::rc::Rc;
use std::sync::mpsc::channel;
use std::sync::{Arc, RwLock};

use futures::channel::mpsc;
use futures::future::LocalBoxFuture;
use futures::FutureExt;
use futures::StreamExt;
use tokio::runtime::Builder;

#[derive(Debug)]
pub enum ActorError {
    HandlerNotFound,
    DispatcherNotFound,
    StateAccessError,
    ExecutionError(Box<dyn std::error::Error + Send + Sync>),
    CustomError(Box<dyn std::error::Error + Send + Sync>),
    SendError,
    DispatchError,
}

impl std::fmt::Display for ActorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ActorError::HandlerNotFound => write!(f, "Handler not found"),
            ActorError::DispatcherNotFound => write!(f, "Dispatcher not found"),
            ActorError::StateAccessError => write!(f, "Failed to access actor state"),
            ActorError::ExecutionError(e) => write!(f, "Execution error: {}", e),
            ActorError::CustomError(e) => write!(f, "Custom error: {}", e),
            ActorError::SendError => write!(f, "Failed to send message"),
            ActorError::DispatchError => write!(f, "Failed to dispatch message"),
        }
    }
}

impl std::error::Error for ActorError {}

type BoxedAny = Box<dyn Any + Send>;
type StateRef<S> = Rc<RefCell<S>>;
type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + Send + 'a>>;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct MessageKey(Arc<str>);

impl MessageKey {
    pub fn new(key: &str) -> Self {
        MessageKey(Arc::from(key))
    }
}

pub enum DispatcherEnvelope {
    Notification(BoxedAny),
    Request(BoxedAny),
}

pub enum Message {
    Notification(MessageKey, BoxedAny),
    Request(
        MessageKey,
        BoxedAny,
        futures::channel::oneshot::Sender<Result<BoxedAny, ActorError>>,
    ),
}

pub trait Dispatcher: Send + Sync + 'static {
    fn dispatch(
        &self,
        key: &MessageKey,
        message: BoxedAny,
    ) -> Result<DispatcherEnvelope, ActorError>;
    fn undispatch(&self, message: BoxedAny) -> Result<BoxedAny, ActorError>;
}

pub struct IdentityDispatcher;

impl Dispatcher for IdentityDispatcher {
    fn dispatch(
        &self,
        _key: &MessageKey,
        message: BoxedAny,
    ) -> Result<DispatcherEnvelope, ActorError> {
        Ok(DispatcherEnvelope::Request(message))
    }

    fn undispatch(&self, message: BoxedAny) -> Result<BoxedAny, ActorError> {
        Ok(message)
    }
}

pub trait AsyncFunc<'a, S, C, R, E>: Fn(&'a mut S, C) -> Self::Fut + Send + Sync
where
    S: 'static,
    C: 'static,
    R: 'static,
    E: std::error::Error + Send + Sync + 'static,
{
    type Fut: Future<Output = Result<R, E>> + Send;
}

impl<'a, F, S, C, R, E, Fut> AsyncFunc<'a, S, C, R, E> for F
where
    F: Fn(&'a mut S, C) -> Fut + Send + Sync,
    S: 'static,
    C: 'static,
    R: 'static,
    E: std::error::Error + Send + Sync + 'static,
    Fut: Future<Output = Result<R, E>> + Send,
{
    type Fut = Fut;
}

type BoxAsyncFunc<S, C, R> =
    Box<dyn for<'a> Fn(&'a mut S, C) -> LocalBoxFuture<'a, Result<R, ActorError>> + Send + Sync>;

struct AsyncFuncHandler<S, C, R>(BoxAsyncFunc<S, C, R>);

impl<S: 'static, C: 'static, R: 'static> AsyncFuncHandler<S, C, R> {
    fn new<F, E>(f: F) -> Self
    where
        F: for<'a> AsyncFunc<'a, S, C, R, E> + 'static,
        E: std::error::Error + Send + Sync + 'static,
    {
        AsyncFuncHandler(Box::new(move |s, c| {
            Box::pin(f(s, c).map(|r| r.map_err(|e| ActorError::CustomError(Box::new(e)))))
        }))
    }
}
trait MessageHandler<S>: Send + Sync {
    fn handle<'a>(
        &'a self,
        state: &'a mut S,
        message: BoxedAny,
    ) -> LocalBoxFuture<'a, Result<BoxedAny, ActorError>>;
}
impl<S, C, R> MessageHandler<S> for AsyncFuncHandler<S, C, R>
where
    S: 'static,
    C: 'static + Send,
    R: 'static + Send,
{
    fn handle<'a>(
        &'a self,
        state: &'a mut S,
        message: BoxedAny,
    ) -> LocalBoxFuture<'a, Result<BoxedAny, ActorError>> {
        Box::pin(async move {
            let params = message.downcast::<C>().map_err(|_| {
                ActorError::CustomError(Box::new(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    "Invalid message type",
                )))
            })?;
            let result = (self.0)(state, *params).await?;
            Ok(Box::new(result) as BoxedAny)
        })
    }
}

pub struct Actor<S: 'static> {
    state: StateRef<S>,
    receiver: mpsc::UnboundedReceiver<Message>,
    handlers: HashMap<MessageKey, Box<dyn MessageHandler<S>>>,
    handler_types: Arc<HandlerTypes>,
    dispatchers: Arc<RwLock<HashMap<MessageKey, Box<dyn Dispatcher>>>>,
}

pub struct ActorRef {
    sender: mpsc::UnboundedSender<Message>,
    handler_types: Arc<HandlerTypes>,
    dispatchers: Arc<RwLock<HashMap<MessageKey, Box<dyn Dispatcher>>>>,
}

struct HandlerTypes {
    handlers: RwLock<HashMap<MessageKey, ()>>,
}

pub struct HandlerRegistration<'a, S: 'static> {
    actor: &'a mut Actor<S>,
    key: MessageKey,
}

impl<'a, S: 'static> HandlerRegistration<'a, S> {
    pub fn with_dispatcher<D: Dispatcher + 'static>(self, dispatcher: D) -> Self {
        self.actor.register_dispatcher(self.key.clone(), dispatcher);
        self
    }

    pub fn handle<C, R, E, F>(self, handler: F)
    where
        C: 'static + Send,
        R: 'static + Send,
        E: std::error::Error + Send + Sync + 'static,
        F: for<'b> AsyncFunc<'b, S, C, R, E> + 'static,
    {
        self.actor.register_handler(self.key, handler);
    }
}

impl<S: 'static> Actor<S> {
    pub fn new(state: S) -> (Self, ActorRef) {
        let (sender, receiver) = mpsc::unbounded();
        let handler_types = Arc::new(HandlerTypes {
            handlers: RwLock::new(HashMap::new()),
        });
        let dispatchers = Arc::new(RwLock::new(HashMap::new()));

        (
            Self {
                state: Rc::new(RefCell::new(state)),
                receiver,
                handlers: HashMap::new(),
                handler_types: handler_types.clone(),
                dispatchers: dispatchers.clone(),
            },
            ActorRef {
                sender,
                handler_types,
                dispatchers,
            },
        )
    }

    pub fn spawn_local<F>(init: F) -> ActorRef
    where
        F: FnOnce() -> (Self, ActorRef),
        F: Send + 'static,
    {
        let runtime = Builder::new_multi_thread()
            .worker_threads(1)
            .enable_all()
            .build()
            .unwrap();
        let (tx, rx) = channel();

        std::thread::spawn(move || {
            let local = tokio::task::LocalSet::new();

            local.block_on(&runtime, async move {
                let (mut actor, actor_ref) = init();
                tx.send(actor_ref.clone()).unwrap();
                actor.run().await;
            });
        });
        rx.recv().unwrap()
    }

    pub async fn run(&mut self) {
        while let Some(message) = self.receiver.next().await {
            match message {
                Message::Notification(key, params) => {
                    if let Some(handler) = self.handlers.get(&key) {
                        let mut state = self.state.borrow_mut();
                        let _ = handler.handle(&mut *state, params).await;
                    } else {
                        println!("Error: No handler found for notification: {:?}", key);
                    }
                }
                Message::Request(key, params, responder) => {
                    if let Some(handler) = self.handlers.get(&key) {
                        let mut state = self.state.borrow_mut();
                        let result = handler.handle(&mut *state, params).await;
                        let _ = responder.send(result);
                    } else {
                        let _ = responder.send(Err(ActorError::HandlerNotFound));
                        println!("Error: No handler found for request: {:?}", key);
                    }
                }
            }
        }
        println!("Actor finished running");
    }

    pub fn register(&mut self, key: MessageKey) -> HandlerRegistration<S> {
        HandlerRegistration { actor: self, key }
    }

    fn register_handler<C, R, E, F>(&mut self, key: MessageKey, handler: F)
    where
        C: 'static + Send,
        R: 'static + Send,
        E: std::error::Error + Send + Sync + 'static,
        F: for<'a> AsyncFunc<'a, S, C, R, E> + 'static,
    {
        let handler = AsyncFuncHandler::new(handler);
        self.handlers.insert(key.clone(), Box::new(handler));
        self.handler_types.handlers.write().unwrap().insert(key, ());
    }

    fn register_dispatcher<D: Dispatcher>(&mut self, key: MessageKey, dispatcher: D) {
        self.dispatchers
            .write()
            .unwrap()
            .insert(key, Box::new(dispatcher));
    }
}

impl ActorRef {
    fn has_handler(&self, key: &MessageKey) -> bool {
        self.handler_types
            .handlers
            .read()
            .unwrap()
            .contains_key(key)
    }

    pub async fn ask<M: Send + 'static, R: Send + 'static>(
        &self,
        key: MessageKey,
        message: M,
    ) -> Result<R, ActorError> {
        if !self.has_handler(&key) {
            println!("Handler not found for key: {:?}", key);
            return Err(ActorError::HandlerNotFound);
        }

        let dispatchers = self.dispatchers.read().unwrap();
        let dispatcher: &dyn Dispatcher = dispatchers
            .get(&key)
            .map(|d| d.as_ref())
            .unwrap_or_else(|| &IdentityDispatcher);

        let dispatched = match dispatcher.dispatch(&key, Box::new(message)) {
            Ok(envelope) => envelope,
            Err(e) => {
                println!("Dispatch error for key {:?}: {:?}", key, e);
                return Err(e);
            }
        };

        match dispatched {
            DispatcherEnvelope::Request(params) => {
                let (sender, receiver) = futures::channel::oneshot::channel();
                self.sender
                    .unbounded_send(Message::Request(key, params, sender))
                    .map_err(|_| ActorError::SendError)?;

                let result = receiver.await.map_err(|_| ActorError::SendError)??;
                let undispatched = dispatcher.undispatch(result)?;
                Ok(*undispatched.downcast().unwrap())
            }
            _ => {
                println!("Unexpected envelope type for key {:?}", key);
                Err(ActorError::DispatchError)
            }
        }
    }

    pub fn tell<M: Send + 'static>(&self, key: MessageKey, message: M) -> Result<(), ActorError> {
        if !self.has_handler(&key) {
            return Err(ActorError::HandlerNotFound);
        }

        let dispatchers = self.dispatchers.read().unwrap();
        let dispatcher: &dyn Dispatcher = dispatchers
            .get(&key)
            .map(|d| d.as_ref())
            .unwrap_or_else(|| &IdentityDispatcher);

        let dispatched = dispatcher.dispatch(&key, Box::new(message))?;

        match dispatched {
            DispatcherEnvelope::Notification(params) => self
                .sender
                .unbounded_send(Message::Notification(key, params))
                .map_err(|_| ActorError::SendError),
            DispatcherEnvelope::Request(_) => Err(ActorError::DispatchError),
        }
    }

    pub fn register_dispatcher<D: Dispatcher>(&self, key: MessageKey, dispatcher: D) {
        self.dispatchers
            .write()
            .unwrap()
            .insert(key, Box::new(dispatcher));
    }
}

impl Clone for ActorRef {
    fn clone(&self) -> Self {
        Self {
            sender: self.sender.clone(),
            handler_types: self.handler_types.clone(),
            dispatchers: self.dispatchers.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::any::TypeId;
    use std::io::Error;

    #[derive(Clone)]
    struct TestState {
        counter: i32,
    }

    #[derive(Clone)]
    struct IncrementMessage(i32);

    #[derive(Clone)]
    struct GetCounterMessage;

    struct TestDispatcher;

    impl Dispatcher for TestDispatcher {
        fn dispatch(
            &self,
            key: &MessageKey,
            message: BoxedAny,
        ) -> Result<DispatcherEnvelope, ActorError> {
            let type_id = (*message).type_id();

            if type_id == TypeId::of::<IncrementMessage>() {
                if let Ok(increment_msg) = message.downcast::<IncrementMessage>() {
                    return Ok(DispatcherEnvelope::Notification(Box::new(increment_msg.0)));
                }
            } else if type_id == TypeId::of::<GetCounterMessage>() {
                return Ok(DispatcherEnvelope::Request(Box::new(())));
            }

            Err(ActorError::DispatchError)
        }

        fn undispatch(&self, message: BoxedAny) -> Result<BoxedAny, ActorError> {
            Ok(message)
        }
    }

    #[tokio::test]
    async fn test_actor_basic_functionality() {
        async fn increment_handler(state: &mut TestState, increment: i32) -> Result<(), Error> {
            state.counter += increment;
            Ok(())
        }

        async fn get_counter_handler(state: &mut TestState, _: ()) -> Result<i32, Error> {
            let counter = state.counter;
            Ok(counter)
        }

        let actor_ref = Actor::spawn_local(|| {
            let initial_state = TestState { counter: 0 };
            let (mut actor, actor_ref) = Actor::new(initial_state);

            actor
                .register(MessageKey::new("increment"))
                .with_dispatcher(TestDispatcher)
                .handle(increment_handler);

            actor
                .register(MessageKey::new("get_counter"))
                .with_dispatcher(TestDispatcher)
                .handle(get_counter_handler);

            (actor, actor_ref)
        });

        // Test increment
        match actor_ref.tell(MessageKey::new("increment"), IncrementMessage(5)) {
            Ok(_) => println!("Successfully sent increment 5"),
            Err(e) => println!("Error sending increment 5: {:?}", e),
        }
        match actor_ref.tell(MessageKey::new("increment"), IncrementMessage(3)) {
            Ok(_) => println!("Successfully sent increment 3"),
            Err(e) => println!("Error sending increment 3: {:?}", e),
        }

        // Test get_counter
        println!("Asking for counter value...");
        let result: Result<i32, ActorError> = actor_ref
            .ask(MessageKey::new("get_counter"), GetCounterMessage)
            .await;

        match result {
            Ok(value) => {
                println!("Got counter value: {}", value);
                assert_eq!(value, 8);
            }
            Err(e) => {
                println!("Error getting counter: {:?}", e);
                panic!("Failed to get counter");
            }
        }
    }
}
