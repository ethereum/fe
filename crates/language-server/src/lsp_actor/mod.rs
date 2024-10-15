pub(crate) mod registration;
pub(crate) mod service;

use async_lsp::{
    lsp_types::{notification::Notification, request::Request},
    AnyEvent, AnyNotification, AnyRequest, ResponseError,
};
use std::collections::HashMap;

use act_locally::{
    dispatcher::Dispatcher,
    handler::AsyncMutatingFunc,
    message::{Message, MessageDowncast, MessageKey, Response},
    types::ActorError,
};

use crate::lsp_actor::service::LspActorKey;

pub struct LspDispatcher {
    pub(super) wrappers: HashMap<
        LspActorKey,
        Box<dyn Fn(Box<dyn Message>) -> Result<Box<dyn Message>, ActorError> + Send + Sync>,
    >,
    pub(super) unwrappers: HashMap<
        LspActorKey,
        Box<dyn Fn(Box<dyn Response>) -> Result<Box<dyn Response>, ActorError> + Send + Sync>,
    >,
}

impl LspDispatcher {
    pub fn new() -> Self {
        Self {
            wrappers: HashMap::new(),
            unwrappers: HashMap::new(),
        }
    }
    fn register_wrapper(
        &mut self,
        key: MessageKey<LspActorKey>,
        wrapper: Box<
            dyn Fn(Box<dyn Message>) -> Result<Box<dyn Message>, ActorError> + Send + Sync,
        >,
    ) {
        let MessageKey(key) = key;
        self.wrappers.insert(key, wrapper);
    }
    pub fn register_unwrapper(
        &mut self,
        key: MessageKey<LspActorKey>,
        unwrapper: Box<
            dyn Fn(Box<dyn Response>) -> Result<Box<dyn Response>, ActorError> + Send + Sync,
        >,
    ) {
        let MessageKey(key) = key;
        self.unwrappers.insert(key, unwrapper);
    }
}

impl Dispatcher<LspActorKey> for LspDispatcher {
    fn message_key(&self, message: &dyn Message) -> Result<MessageKey<LspActorKey>, ActorError> {
        if let Some(request) = message.downcast_ref::<AnyRequest>() {
            Ok(LspActorKey::from(&request.method).into())
        } else if let Some(notification) = message.downcast_ref::<AnyNotification>() {
            Ok(LspActorKey::from(&notification.method).into())
        } else if let Some(event) = message.downcast_ref::<AnyEvent>() {
            Ok(LspActorKey::from(event.inner_type_id()).into())
        } else {
            Err(ActorError::DispatchError)
        }
    }

    fn wrap(
        &self,
        message: Box<dyn Message>,
        key: MessageKey<LspActorKey>,
    ) -> Result<Box<dyn Message>, ActorError> {
        let MessageKey(key) = key;
        if let Some(wrapper) = self.wrappers.get(&key) {
            if let Some(request) = message.downcast_ref::<AnyRequest>() {
                wrapper(Box::new(request.params.clone()))
            } else if let Some(notification) = message.downcast_ref::<AnyNotification>() {
                wrapper(Box::new(notification.params.clone()))
            } else if message.is::<AnyEvent>() {
                wrapper(message)
            } else {
                wrapper(message)
            }
        } else {
            Err(ActorError::HandlerNotFound)
        }
    }

    fn unwrap(
        &self,
        message: Box<dyn Response>,
        key: MessageKey<LspActorKey>,
    ) -> Result<Box<dyn Response>, ActorError> {
        let MessageKey(key) = key;
        if let Some(unwrapper) = self.unwrappers.get(&key) {
            println!("Found an unwrapper for key {}!", &key);
            unwrapper(message)
        } else {
            Err(ActorError::HandlerNotFound)
        }
    }
}

pub trait LspActor<S: 'static> {
    fn handle_request<R: Request>(
        &mut self,
        handler: impl for<'a> AsyncMutatingFunc<'a, S, R::Params, R::Result, ResponseError> + 'static,
    ) -> &mut Self;
    fn handle_notification<N: Notification>(
        &mut self,
        handler: impl for<'a> AsyncMutatingFunc<'a, S, N::Params, (), ResponseError> + 'static,
    ) -> &mut Self;
    fn handle_event<E: Send + Sync + 'static>(
        &mut self,
        handler: impl for<'a> AsyncMutatingFunc<'a, S, E, (), ResponseError> + 'static,
    ) -> &mut Self;
}

#[cfg(test)]
mod tests {

    use super::*;
    use act_locally::builder::ActorBuilder;
    use async_lsp::{
        lsp_types::{InitializeParams, InitializeResult},
        RequestId,
    };
    use serde_json::{json, Value};
    use service::LspActorService;

    #[derive(Debug)]
    enum Initialize {}

    impl Request for Initialize {
        type Params = InitializeParams;
        type Result = InitializeResult;
        const METHOD: &'static str = "initialize";
    }

    #[derive(Debug)]
    enum Initialized {}

    impl Notification for Initialized {
        type Params = ();
        const METHOD: &'static str = "initialized";
    }

    struct TestState {
        initialized: bool,
    }

    #[tokio::test]
    async fn test_lsp_actor() {
        let actor_ref = ActorBuilder::new()
            .with_state_init(|| {
                let initial_state = TestState { initialized: false };
                Ok(initial_state)
            })
            .spawn()
            .expect("Failed to spawn actor");

        async fn handle_initialize(
            state: &mut TestState,
            _: InitializeParams,
        ) -> Result<InitializeResult, ResponseError> {
            println!("Handling initialize request");
            state.initialized = true;
            Ok(InitializeResult::default())
        }

        let mut service = LspActorService::new(actor_ref.clone());
        let dispatcher = service.dispatcher.clone();
        let dispatcher = dispatcher.as_ref();
        let actor_ref = service.actor_ref.clone();

        service.handle_request::<Initialize>(handle_initialize);

        async fn handle_initialized(state: &mut TestState, _: ()) -> Result<(), ResponseError> {
            println!("Handling initialized notification");
            assert!(state.initialized, "State should be initialized");
            Ok(())
        }
        service.handle_notification::<Initialized>(handle_initialized);

        // Test initialize request
        let init_params = InitializeParams::default();
        let init_request = AnyRequest::stub(
            RequestId::Number(1),
            Initialize::METHOD.to_string(),
            serde_json::to_value(init_params).unwrap(),
        );

        println!("Sending initialize request");

        let init_result: Value = match actor_ref.ask(dispatcher, init_request).await {
            Ok(res) => res,
            Err(e) => {
                panic!("Failed to get InitializeResult: {:?}", e);
            }
        };

        let init_result_deserialized: InitializeResult =
            serde_json::from_value(init_result).unwrap();

        assert_eq!(init_result_deserialized, InitializeResult::default());

        // Test initialized notification
        let init_notification = AnyNotification::stub(Initialized::METHOD.to_string(), json!(null));

        println!("Sending initialized notification");
        if let Err(e) = actor_ref.tell(dispatcher, init_notification) {
            panic!("Failed to send Initialized notification: {:?}", e);
        }

        // Wait a bit to ensure the notification is processed
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }
}
