use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::rc::Rc;
use std::sync::mpsc::channel;
use std::sync::Arc;

use futures::channel::mpsc;
use futures::future::LocalBoxFuture;
use futures::FutureExt;
use futures::StreamExt;
use tokio::runtime::Builder;

#[derive(Debug)]
pub enum ActorError {
    HandlerNotFound,
    StateAccessError,
    ExecutionError(Box<dyn std::error::Error + Send + Sync>),
    CustomError(Box<dyn std::error::Error + Send + Sync>),
    SendError,
}

impl std::fmt::Display for ActorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ActorError::HandlerNotFound => write!(f, "Handler not found"),
            ActorError::StateAccessError => write!(f, "Failed to access actor state"),
            ActorError::ExecutionError(e) => write!(f, "Execution error: {}", e),
            ActorError::CustomError(e) => write!(f, "Custom error: {}", e),
            ActorError::SendError => write!(f, "Failed to send message"),
        }
    }
}

impl std::error::Error for ActorError {}

type BoxedAny = Box<dyn Any + Send>;
type StateRef<S> = Rc<RefCell<S>>;
type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + Send + 'a>>;

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
    Box<dyn for<'a> Fn(&'a mut S, C) -> BoxFuture<'a, Result<R, ActorError>> + Send + Sync>;

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

pub enum Message {
    Notification(BoxedAny),
    Request(
        BoxedAny,
        futures::channel::oneshot::Sender<Result<BoxedAny, ActorError>>,
    ),
}

pub struct Actor<S: 'static> {
    state: StateRef<S>,
    receiver: mpsc::UnboundedReceiver<Message>,
    handlers: HashMap<std::any::TypeId, Box<dyn Any + Send + Sync>>,
    handler_types: Arc<HandlerTypes>,
}

pub struct ActorRef {
    sender: mpsc::UnboundedSender<Message>,
    handler_types: Arc<HandlerTypes>,
}

struct HandlerTypes {
    handlers: std::sync::RwLock<HashMap<std::any::TypeId, ()>>,
}

impl<S: 'static> Actor<S> {
    pub fn new(state: S) -> (Self, ActorRef) {
        let (sender, receiver) = mpsc::unbounded();
        let handler_types = Arc::new(HandlerTypes {
            handlers: std::sync::RwLock::new(HashMap::new()),
        });

        (
            Self {
                state: Rc::new(RefCell::new(state)),
                receiver,
                handlers: HashMap::new(),
                handler_types: handler_types.clone(),
            },
            ActorRef {
                sender,
                handler_types,
            },
        )
    }

    pub fn spawn_local<F>(init: F) -> ActorRef
    where
        F: FnOnce() -> (Self, ActorRef),
        F: Send + 'static,
    {
        let runtime = Builder::new_multi_thread()
            .worker_threads(1) // Adjust as needed
            .enable_all()
            .build()
            .unwrap();
        let (tx, rx) = channel();

        std::thread::spawn(move || {
            let local = tokio::task::LocalSet::new();

            let actor_ref = local.spawn_local(async move {
                let (mut actor, actor_ref) = init();
                tx.send(actor_ref);
                actor.run().await;
            });
            runtime.block_on(local);
            actor_ref
        });
        let actor_ref = rx.recv().unwrap();

        actor_ref
    }

    pub async fn run(&mut self) {
        while let Some(message) = self.receiver.next().await {
            match message {
                Message::Notification(params) => {
                    let type_id = (*params).type_id();
                    if let Some(handler) = self.handlers.get(&type_id) {
                        let handler = handler
                            .downcast_ref::<AsyncFuncHandler<S, BoxedAny, ()>>()
                            .unwrap();
                        let mut state = self.state.borrow_mut();
                        let _ = handler.0(&mut *state, params).await;
                    }
                }
                Message::Request(params, responder) => {
                    let type_id = (*params).type_id();
                    if let Some(handler) = self.handlers.get(&type_id) {
                        let handler = handler
                            .downcast_ref::<AsyncFuncHandler<S, BoxedAny, BoxedAny>>()
                            .unwrap();
                        let mut state = self.state.borrow_mut();
                        let result = handler.0(&mut *state, params).await;
                        let _ = responder.send(result);
                    } else {
                        let _ = responder.send(Err(ActorError::HandlerNotFound));
                    }
                }
            }
        }
    }

    pub fn register_request_handler<C, R, E, F>(&mut self, handler: F)
    where
        C: 'static + Send,
        R: 'static + Send,
        E: std::error::Error + Send + Sync + 'static,
        F: for<'a> AsyncFunc<'a, S, C, R, E> + 'static,
    {
        let type_id = std::any::TypeId::of::<C>();
        let handler = AsyncFuncHandler::new(handler);
        self.handlers.insert(type_id, Box::new(handler));
        self.handler_types
            .handlers
            .write()
            .unwrap()
            .insert(type_id, ());
    }
}

impl ActorRef {
    fn has_handler<M: 'static>(&self) -> bool {
        self.handler_types
            .handlers
            .read()
            .unwrap()
            .contains_key(&std::any::TypeId::of::<M>())
    }

    pub async fn ask<M: Send + 'static, R: Send + 'static>(
        &self,
        message: M,
    ) -> Result<R, ActorError> {
        if !self.has_handler::<M>() {
            return Err(ActorError::HandlerNotFound);
        }

        let (responder, receiver) = futures::channel::oneshot::channel();
        let message = Message::Request(Box::new(message), responder);
        self.sender
            .unbounded_send(message)
            .map_err(|_| ActorError::SendError)?;

        receiver
            .await
            .map_err(|_| ActorError::SendError)?
            .and_then(|result| Ok(*result.downcast().unwrap()))
    }

    pub fn tell<M: Send + 'static>(&self, message: M) -> Result<(), ActorError> {
        if !self.has_handler::<M>() {
            return Err(ActorError::HandlerNotFound);
        }

        let message = Message::Notification(Box::new(message));
        self.sender
            .unbounded_send(message)
            .map_err(|_| ActorError::SendError)
    }
}

impl Clone for ActorRef {
    fn clone(&self) -> Self {
        Self {
            sender: self.sender.clone(),
            handler_types: self.handler_types.clone(),
        }
    }
}
