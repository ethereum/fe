use std::any::{Any, TypeId};
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

#[derive(Debug)]
pub enum Message {
    Notification(MessageKey, BoxedAny),
    Request(
        MessageKey,
        BoxedAny,
        futures::channel::oneshot::Sender<Result<BoxedAny, ActorError>>,
    ),
}

pub trait Dispatcher: Send + Sync + 'static {
    fn create_key(&self, message: &dyn Any) -> Result<MessageKey, ActorError>;
    fn wrap(&self, message: BoxedAny) -> Result<DispatcherEnvelope, ActorError>;
    fn unwrap(&self, message: BoxedAny) -> Result<BoxedAny, ActorError>;
}

pub struct IdentityDispatcher;

impl Dispatcher for IdentityDispatcher {
    fn create_key(&self, message: &dyn Any) -> Result<MessageKey, ActorError> {
        println!(
            "Identity dispatcher is creating key for message: {:?}",
            std::any::type_name_of_val(message)
        );
        Ok(MessageKey::new(std::any::type_name_of_val(message)))
    }

    fn wrap(&self, message: BoxedAny) -> Result<DispatcherEnvelope, ActorError> {
        Ok(DispatcherEnvelope::Request(message))
    }

    fn unwrap(&self, message: BoxedAny) -> Result<BoxedAny, ActorError> {
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

struct AsyncFuncHandler<S, C, R> {
    func: BoxAsyncFunc<S, C, R>,
    message_type_id: TypeId,
}

impl<S: 'static, C: 'static, R: 'static> AsyncFuncHandler<S, C, R> {
    fn new<F, E>(f: F) -> Self
    where
        F: for<'a> AsyncFunc<'a, S, C, R, E> + 'static,
        E: std::error::Error + Send + Sync + 'static,
    {
        AsyncFuncHandler {
            func: Box::new(move |s, c| {
                Box::pin(f(s, c).map(|r| r.map_err(|e| ActorError::CustomError(Box::new(e)))))
            }),
            message_type_id: TypeId::of::<C>(),
        }
    }
}

trait MessageHandler<S>: Send + Sync {
    fn message_type_id(&self) -> TypeId;
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
    fn message_type_id(&self) -> TypeId {
        self.message_type_id
    }

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
            let result = (self.func)(state, *params).await?;
            Ok(Box::new(result) as BoxedAny)
        })
    }
}

pub struct Actor<S: 'static> {
    state: StateRef<S>,
    receiver: mpsc::UnboundedReceiver<Message>,
    handlers: HashMap<MessageKey, Box<dyn MessageHandler<S>>>,
    dispatcher: Arc<RwLock<Box<dyn Dispatcher>>>,
    actor_ref: ActorRef,
}

pub struct ActorRef {
    sender: mpsc::UnboundedSender<Message>,
    dispatcher: Arc<RwLock<Box<dyn Dispatcher>>>,
}

pub struct HandlerRegistration<'a, S: 'static> {
    actor: &'a mut Actor<S>,
    key: MessageKey,
}

impl<'a, S: 'static> HandlerRegistration<'a, S> {
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
        let dispatcher: Arc<RwLock<Box<dyn Dispatcher>>> =
            Arc::new(RwLock::new(Box::new(IdentityDispatcher)));

        let actor_ref = ActorRef {
            sender: sender.clone(),
            dispatcher: dispatcher.clone(),
        };

        let actor_ref_clone = actor_ref.clone();
        (
            Self {
                state: Rc::new(RefCell::new(state)),
                receiver,
                handlers: HashMap::new(),
                dispatcher: Arc::clone(&dispatcher),
                actor_ref,
            },
            actor_ref_clone,
        )
    }

    pub fn with_dispatcher<D: Dispatcher + 'static>(self, dispatcher: D) -> Self {
        let mut dispatcher_guard = self.dispatcher.write().unwrap();
        *dispatcher_guard = Box::new(dispatcher);
        drop(dispatcher_guard);
        self
    }

    pub fn spawn_local<F>(init: F) -> ActorRef
    where
        F: FnOnce() -> (Self, ActorRef),
        F: Send + 'static,
    {
        let runtime = Builder::new_current_thread().enable_all().build().unwrap();
        let (tx, rx) = channel();

        std::thread::spawn(move || {
            let local = tokio::task::LocalSet::new();
            local.block_on(&runtime, async move {
                println!("Spawning actor");
                let (mut actor, actor_ref) = init();
                tx.send(actor_ref.clone()).unwrap();
                println!("Starting actor run loop");
                actor.run().await;
                println!("Actor run loop finished");
            });
        });
        rx.recv().unwrap()
    }

    pub async fn run(&mut self) {
        println!("Actor run method started");
        while let Some(message) = self.receiver.next().await {
            println!("Received message: {:?}", message);
            match message {
                Message::Notification(key, params) => {
                    if let Some(handler) = self.handlers.get(&key) {
                        // if handler.message_type_id() == params.type_id() {
                        let mut state = self.state.borrow_mut();
                        let _ = handler.handle(&mut *state, params).await;
                    } else {
                        println!("Error: No handler found for notification: {:?}", key);
                    }
                }
                Message::Request(key, params, responder) => {
                    if let Some(handler) = self.handlers.get(&key) {
                        // if handler.message_type_id() == params.type_id() {
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
        self.handlers.insert(key, Box::new(handler));
    }
}

impl ActorRef {
    pub async fn ask<M: Send + 'static, R: Send + 'static>(
        &self,
        message: M,
    ) -> Result<R, ActorError> {
        let dispatcher = self.dispatcher.read().unwrap();
        let key = dispatcher.create_key(&message)?;
        println!("ask: Dispatched with key: {:?}", key);
        let wrapped = dispatcher.wrap(Box::new(message))?;

        match wrapped {
            DispatcherEnvelope::Request(params) => {
                let (sender, receiver) = futures::channel::oneshot::channel();
                self.sender
                    .unbounded_send(Message::Request(key, params, sender))
                    .map_err(|_| ActorError::SendError)?;

                let result = receiver.await.map_err(|_| ActorError::SendError)??;
                let unwrapped = dispatcher.unwrap(result)?;
                unwrapped.downcast().map(|boxed| *boxed).map_err(|_| {
                    ActorError::CustomError(Box::new(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "Failed to downcast result",
                    )))
                })
            }
            DispatcherEnvelope::Notification(_) => Err(ActorError::DispatchError),
        }
    }

    pub fn tell<M: Send + 'static>(&self, message: M) -> Result<(), ActorError> {
        let dispatcher = self.dispatcher.read().unwrap();
        let key = dispatcher.create_key(&message)?;
        println!("tell: Dispatched with key: {:?}", key);
        let wrapped = dispatcher.wrap(Box::new(message))?;

        match wrapped {
            DispatcherEnvelope::Notification(params) | DispatcherEnvelope::Request(params) => self
                .sender
                .unbounded_send(Message::Notification(key, params))
                .map_err(|_| ActorError::SendError),
        }
    }
}

impl Clone for ActorRef {
    fn clone(&self) -> Self {
        Self {
            sender: self.sender.clone(),
            dispatcher: self.dispatcher.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    // State and message definitions
    #[derive(Clone)]
    struct TestState {
        counter: i32,
        messages: Vec<String>,
    }

    struct IncrementMessage(i32);
    struct AppendMessage(String);
    struct GetCounterMessage;
    struct GetMessagesMessage;

    // Custom dispatcher
    #[derive(Debug)]
    struct TestDispatcher;

    impl Dispatcher for TestDispatcher {
        fn create_key(&self, message: &dyn Any) -> Result<MessageKey, ActorError> {
            if message.is::<IncrementMessage>() {
                Ok(MessageKey::new("increment"))
            } else if message.is::<AppendMessage>() {
                Ok(MessageKey::new("append"))
            } else if message.is::<GetCounterMessage>() {
                Ok(MessageKey::new("get_counter"))
            } else if message.is::<GetMessagesMessage>() {
                Ok(MessageKey::new("get_messages"))
            } else {
                Err(ActorError::DispatchError)
            }
        }

        fn wrap(&self, message: BoxedAny) -> Result<DispatcherEnvelope, ActorError> {
            Ok(DispatcherEnvelope::Request(message))
        }

        fn unwrap(&self, message: BoxedAny) -> Result<BoxedAny, ActorError> {
            Ok(message)
        }
    }

    // Handler functions
    async fn increment_handler(
        state: &mut TestState,
        msg: IncrementMessage,
    ) -> Result<(), ActorError> {
        state.counter += msg.0;
        Ok(())
    }

    async fn append_handler(state: &mut TestState, msg: AppendMessage) -> Result<(), ActorError> {
        state.messages.push(msg.0);
        Ok(())
    }

    async fn get_counter_handler(
        state: &mut TestState,
        _: GetCounterMessage,
    ) -> Result<i32, ActorError> {
        Ok(state.counter)
    }

    async fn get_messages_handler(
        state: &mut TestState,
        _: GetMessagesMessage,
    ) -> Result<Vec<String>, ActorError> {
        Ok(state.messages.clone())
    }

    #[test]
    fn test_with_dispatcher() {
        let (mut actor, actor_ref1) = Actor::new(TestState {
            counter: 0,
            messages: Vec::new(),
        });

        let actor_ref2 = actor_ref1.clone();

        let dispatcher = TestDispatcher;
        actor = actor.with_dispatcher(dispatcher);

        // Check if both actor_ref1 and actor_ref2 point to the same dispatcher
        assert!(
            Arc::ptr_eq(&actor_ref1.dispatcher, &actor_ref2.dispatcher),
            "ActorRef clones do not point to the same dispatcher"
        );

        // Check if the actor and actor_refs point to the same dispatcher
        assert!(
            Arc::ptr_eq(&actor.dispatcher, &actor_ref1.dispatcher),
            "Actor and ActorRef do not point to the same dispatcher"
        );

        // Verify that the dispatcher has been updated
        // let dispatcher = actor.dispatcher.read().unwrap();
        // assert!(
        //     dispatcher.downcast_ref::<TestDispatcher>().is_some(),
        //     "Dispatcher was not updated to TestDispatcher"
        // );
    }

    #[tokio::test]
    async fn test_actor_with_custom_dispatcher() {
        let actor_ref = Actor::spawn_local(|| {
            let initial_state = TestState {
                counter: 0,
                messages: Vec::new(),
            };
            let (mut actor, actor_ref) = Actor::new(initial_state);

            actor = actor.with_dispatcher(TestDispatcher);

            actor
                .register(MessageKey::new("increment"))
                .handle(increment_handler);
            actor
                .register(MessageKey::new("append"))
                .handle(append_handler);
            actor
                .register(MessageKey::new("get_counter"))
                .handle(get_counter_handler);
            actor
                .register(MessageKey::new("get_messages"))
                .handle(get_messages_handler);

            (actor, actor_ref)
        });

        // Test increment
        actor_ref
            .tell(IncrementMessage(5))
            .expect("Failed to send increment 5");
        actor_ref
            .tell(IncrementMessage(3))
            .expect("Failed to send increment 3");

        // Test append
        actor_ref
            .tell(AppendMessage("Hello".to_string()))
            .expect("Failed to send append Hello");
        actor_ref
            .tell(AppendMessage("World".to_string()))
            .expect("Failed to send append World");

        // Test get_counter
        let counter: i32 = actor_ref
            .ask(GetCounterMessage)
            .await
            .expect("Failed to get counter");
        assert_eq!(counter, 8);

        // Test get_messages
        let messages: Vec<String> = actor_ref
            .ask(GetMessagesMessage)
            .await
            .expect("Failed to get messages");
        assert_eq!(messages, vec!["Hello".to_string(), "World".to_string()]);

        println!("All tests passed successfully!");
    }
}
