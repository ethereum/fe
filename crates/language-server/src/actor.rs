use std::any::{Any, TypeId};
use std::cell::RefCell;
use std::collections::HashMap;
use std::future::Future;
use std::rc::Rc;
use std::sync::mpsc::channel;

use futures::channel::mpsc;
use futures::future::LocalBoxFuture;
use futures::FutureExt;
use futures::StreamExt;
use tokio::runtime::Builder;
use tracing::info;

#[derive(Debug)]
pub enum ActorError {
    HandlerNotFound,
    // DispatcherNotFound,
    // StateAccessError,
    // ExecutionError(Box<dyn std::error::Error + Send + Sync>),
    CustomError(Box<dyn std::error::Error + Send + Sync>),
    SendError,
    DispatchError,
}

impl std::fmt::Display for ActorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ActorError::HandlerNotFound => write!(f, "Handler not found"),
            // ActorError::DispatcherNotFound => write!(f, "Dispatcher not found"),
            // ActorError::StateAccessError => write!(f, "Failed to access actor state"),
            // ActorError::ExecutionError(e) => write!(f, "Execution error: {}", e),
            ActorError::CustomError(e) => write!(f, "Custom error: {}", e),
            ActorError::SendError => write!(f, "Failed to send message"),
            ActorError::DispatchError => write!(f, "Failed to dispatch message"),
        }
    }
}

impl std::error::Error for ActorError {}

pub(crate) type BoxedAny = Box<dyn Any + Send>;
type StateRef<S> = Rc<RefCell<S>>;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct MessageKey(pub String);

impl MessageKey {
    pub fn new(key: &str) -> Self {
        MessageKey(key.to_string())
    }
}

pub enum Message {
    Notification(MessageKey, BoxedAny),
    Request(
        MessageKey,
        BoxedAny,
        futures::channel::oneshot::Sender<Result<BoxedAny, ActorError>>,
    ),
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
                println!("Downcast error in handle");
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

pub trait Dispatcher: Send + Sync + 'static {
    fn message_key(&self, message: &dyn Any) -> Result<MessageKey, ActorError>;
    fn wrap(&self, message: BoxedAny) -> Result<BoxedAny, ActorError>;
    fn unwrap(&self, message: BoxedAny) -> Result<BoxedAny, ActorError>;
    fn register_wrapper(
        &mut self,
        key: MessageKey,
        wrapper: Box<dyn Fn(BoxedAny) -> BoxedAny + Send + Sync>,
    );
}

pub struct Actor<S: 'static> {
    state: StateRef<S>,
    handlers: HashMap<MessageKey, Box<dyn MessageHandler<S>>>,
    receiver: mpsc::UnboundedReceiver<Message>,
}

impl<S: 'static> Actor<S> {
    pub fn new(state: S) -> (Self, ActorRef) {
        let (sender, receiver) = mpsc::unbounded();
        (
            Self {
                state: Rc::new(RefCell::new(state)),
                handlers: HashMap::new(),
                receiver,
            },
            ActorRef { sender },
        )
    }

    pub fn register_handler<C, R, E, F>(&mut self, key: MessageKey, handler: F)
    where
        C: 'static + Send,
        R: 'static + Send,
        E: std::error::Error + Send + Sync + 'static,
        F: for<'a> AsyncFunc<'a, S, C, R, E> + 'static,
    {
        let handler = AsyncFuncHandler::new(handler);
        self.handlers.insert(key, Box::new(handler));
    }

    pub fn spawn_local<F, D>(init: F) -> (ActorRef, D)
    where
        F: FnOnce() -> (Self, ActorRef, D),
        F: Send + 'static,
        D: Dispatcher,
    {
        let runtime = Builder::new_current_thread().enable_all().build().unwrap();
        let (tx, rx) = channel();

        std::thread::spawn(move || {
            let local = tokio::task::LocalSet::new();
            local.block_on(&runtime, async move {
                println!("Spawning actor");
                let (mut actor, actor_ref, dispatcher) = init();
                tx.send((actor_ref.clone(), dispatcher)).unwrap();
                println!("Starting actor run loop");
                actor.run().await;
                println!("Actor run loop finished");
            });
        });
        rx.recv().unwrap()
    }

    pub async fn run(&mut self) {
        println!("Actor run loop started");
        while let Some(message) = self.receiver.next().await {
            match message {
                Message::Notification(key, payload) => {
                    if let Some(handler) = self.handlers.get(&key) {
                        let mut state = self.state.borrow_mut();
                        if let Err(e) = handler.handle(&mut *state, payload).await {
                            println!("Handler error: {:?}", e);
                        }
                    } else {
                        println!("No handler found for key: {:?}", key);
                    }
                }
                Message::Request(key, payload, response_tx) => {
                    if let Some(handler) = self.handlers.get(&key) {
                        let mut state = self.state.borrow_mut();
                        let result = handler.handle(&mut *state, payload).await;
                        let _ = response_tx.send(result);
                    } else {
                        println!("No handler found for key: {:?}", key);
                        let _ = response_tx.send(Err(ActorError::HandlerNotFound));
                    }
                }
            }
        }
        println!("Actor run loop finished");
    }
}

#[derive(Clone)]
pub struct ActorRef {
    sender: mpsc::UnboundedSender<Message>,
}

impl ActorRef {
    pub async fn ask<M: Send + 'static, R: Send + 'static, D: Dispatcher>(
        &self,
        dispatcher: &D,
        message: M,
    ) -> Result<R, ActorError> {
        let key = dispatcher.message_key(&message)?;
        let wrapped = dispatcher.wrap(Box::new(message))?;

        let (response_tx, response_rx) = futures::channel::oneshot::channel();

        self.sender
            .unbounded_send(Message::Request(key, wrapped, response_tx))
            .map_err(|_| ActorError::SendError)?;

        let result = response_rx.await.map_err(|_| ActorError::SendError)??;
        info!("Result type: {:?}", std::any::TypeId::of::<R>());
        info!("Actual type: {:?}", result.type_id());
        info!(
            "in our ask handler we unwrapped the result as: {:?}",
            &result
        );
        let unwrapped = dispatcher.unwrap(result)?;
        info!("Unwrapped type: {:?}", unwrapped.type_id());
        info!("And unwrapped it to {:?}", &unwrapped);
        unwrapped.downcast().map(|boxed| *boxed).map_err(|_| {
            ActorError::CustomError(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Failed to downcast result",
            )))
        })
    }

    pub fn tell<M: Send + 'static, D: Dispatcher>(
        &self,
        dispatcher: &D,
        message: M,
    ) -> Result<(), ActorError> {
        let key = dispatcher.message_key(&message)?;
        let wrapped = dispatcher.wrap(Box::new(message))?;
        self.sender
            .unbounded_send(Message::Notification(key, wrapped))
            .map_err(|_| ActorError::SendError)
    }
}

pub struct HandlerRegistration<'a, S: 'static, D: Dispatcher> {
    pub actor: &'a mut Actor<S>,
    pub dispatcher: &'a mut D,
}

impl<'a, S: 'static, D: Dispatcher> HandlerRegistration<'a, S, D> {
    pub fn register<C, R, E, F>(
        &mut self,
        key: MessageKey,
        wrapper: impl Fn(C) -> BoxedAny + Send + Sync + 'static,
        handler: F,
    ) where
        C: 'static + Send,
        R: 'static + Send,
        E: std::error::Error + Send + Sync + 'static,
        F: for<'b> AsyncFunc<'b, S, C, R, E> + 'static,
    {
        self.dispatcher.register_wrapper(
            key.clone(),
            Box::new(move |msg| wrapper(*msg.downcast::<C>().expect("Type mismatch in wrapper"))),
        );
        self.actor.register_handler(key, handler);
    }
}

// Tests
#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[derive(Clone)]
    struct TestState {
        counter: i32,
        messages: Vec<String>,
    }

    struct IncrementMessage(i32);
    struct AppendMessage(String);
    struct GetCounterMessage;
    struct GetMessagesMessage;

    #[derive(Clone)]
    struct TestDispatcher;

    impl Dispatcher for TestDispatcher {
        fn message_key(&self, message: &dyn Any) -> Result<MessageKey, ActorError> {
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

        fn wrap(&self, message: BoxedAny) -> Result<BoxedAny, ActorError> {
            println!("Wrapping message of type: {:?}", message.type_id());
            Ok(message)
        }

        fn unwrap(&self, message: BoxedAny) -> Result<BoxedAny, ActorError> {
            println!("Unwrapping message of type: {:?}", message.type_id());
            Ok(message)
        }

        fn register_wrapper(
            &mut self,
            _key: MessageKey,
            _wrapper: Box<dyn Fn(BoxedAny) -> BoxedAny + Send + Sync>,
        ) {
            // In this simple implementation, we don't need to do anything
        }
    }

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
    #[tokio::test]
    async fn test_actor_with_custom_dispatcher() {
        let (actor_ref, dispatcher) = Actor::spawn_local(|| {
            let mut dispatcher = TestDispatcher;
            let (mut actor, actor_ref) = Actor::new(TestState {
                counter: 0,
                messages: Vec::new(),
            });

            let mut registration = HandlerRegistration {
                actor: &mut actor,
                dispatcher: &mut dispatcher,
            };

            registration.register(
                MessageKey::new("increment"),
                |msg: IncrementMessage| Box::new(msg) as BoxedAny,
                increment_handler,
            );

            registration.register(
                MessageKey::new("append"),
                |msg: AppendMessage| Box::new(msg) as BoxedAny,
                append_handler,
            );

            registration.register(
                MessageKey::new("get_counter"),
                |msg: GetCounterMessage| Box::new(msg) as BoxedAny,
                get_counter_handler,
            );

            registration.register(
                MessageKey::new("get_messages"),
                |msg: GetMessagesMessage| Box::new(msg) as BoxedAny,
                get_messages_handler,
            );

            (actor, actor_ref, dispatcher)
        });

        // Test increment
        actor_ref
            .tell(&dispatcher, IncrementMessage(5))
            .expect("Failed to send increment 5");
        actor_ref
            .tell(&dispatcher, IncrementMessage(3))
            .expect("Failed to send increment 3");

        // Test append
        actor_ref
            .tell(&dispatcher, AppendMessage("Hello".to_string()))
            .expect("Failed to send append Hello");
        actor_ref
            .tell(&dispatcher, AppendMessage("World".to_string()))
            .expect("Failed to send append World");

        // Allow some time for processing
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

        // Test get_counter
        let counter: i32 = actor_ref
            .ask(&dispatcher, GetCounterMessage)
            .await
            .unwrap_or_else(|e| {
                println!("Error getting counter: {:?}", e);
                panic!("Failed to get counter: {:?}", e);
            });
        assert_eq!(counter, 8);

        // Test get_messages
        let messages: Vec<String> = actor_ref
            .ask(&dispatcher, GetMessagesMessage)
            .await
            .expect("Failed to get messages");
        assert_eq!(messages, vec!["Hello".to_string(), "World".to_string()]);

        println!("All tests passed successfully!");
    }
}
