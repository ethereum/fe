use async_lsp::{
    lsp_types::{notification::Notification, request::Request},
    AnyNotification, AnyRequest, ResponseError,
};
use std::collections::HashMap;

use act_locally::{
    actor::HandlerRegistration,
    dispatcher::Dispatcher,
    handler::AsyncMutatingFunc,
    message::{Message, MessageDowncast, MessageKey, Response, ResponseDowncast},
    types::ActorError,
};

pub struct LspDispatcher {
    pub(super) wrappers: HashMap<
        String,
        Box<dyn Fn(Box<dyn Message>) -> Result<Box<dyn Message>, ActorError> + Send + Sync>,
    >,
    pub(super) unwrappers: HashMap<
        String,
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
        key: MessageKey,
        wrapper: Box<
            dyn Fn(Box<dyn Message>) -> Result<Box<dyn Message>, ActorError> + Send + Sync,
        >,
    ) {
        let MessageKey(key) = key;
        self.wrappers.insert(key, wrapper);
    }
    pub fn register_unwrapper(
        &mut self,
        key: MessageKey,
        unwrapper: Box<
            dyn Fn(Box<dyn Response>) -> Result<Box<dyn Response>, ActorError> + Send + Sync,
        >,
    ) {
        let MessageKey(key) = key;
        self.unwrappers.insert(key, unwrapper);
    }
}

impl Dispatcher for LspDispatcher {
    fn message_key(&self, message: &dyn Message) -> Result<MessageKey, ActorError> {
        if let Some(request) = message.downcast_ref::<AnyRequest>() {
            Ok(MessageKey::new(&request.method))
        } else if let Some(notification) = message.downcast_ref::<AnyNotification>() {
            Ok(MessageKey::new(&notification.method))
        } else {
            Err(ActorError::DispatchError)
        }
    }

    fn wrap(
        &self,
        message: Box<dyn Message>,
        key: MessageKey,
    ) -> Result<Box<dyn Message>, ActorError> {
        let MessageKey(key) = key;
        if let Some(wrapper) = self.wrappers.get(&key) {
            if let Some(request) = message.downcast_ref::<AnyRequest>() {
                wrapper(Box::new(request.params.clone()))
            } else if let Some(notification) = message.downcast_ref::<AnyNotification>() {
                wrapper(Box::new(notification.params.clone()))
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
        key: MessageKey,
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
}

impl<'a, S: 'static> LspActor<S> for HandlerRegistration<'a, S, LspDispatcher> {
    fn handle_request<R: Request>(
        &mut self,
        handler: impl for<'b> AsyncMutatingFunc<'b, S, R::Params, R::Result, ResponseError> + 'static,
    ) -> &mut Self {
        let param_handler = Box::new(
            move |params: Box<dyn Message>| -> Result<Box<dyn Message>, ActorError> {
                let params = params.downcast::<serde_json::Value>().map_err(|_| {
                    println!("Failed to downcast params to serde_json::Value");
                    ActorError::DowncastError
                })?;
                let typed_params: R::Params = serde_json::from_value(*params).map_err(|e| {
                    println!("Deserialization error: {:?}", e);
                    ActorError::CustomError(Box::new(e))
                })?;

                Ok(Box::new(typed_params) as Box<dyn Message>)
            },
        );

        self.dispatcher
            .register_wrapper(MessageKey::new(R::METHOD), param_handler);

        self.actor_ref
            .register_handler_async_mutating(MessageKey::new(R::METHOD), handler);

        let result_unwrapper = Box::new(
            move |result: Box<dyn Response>| -> Result<Box<dyn Response>, ActorError> {
                let lsp_result = *result
                    .downcast::<R::Result>()
                    .map_err(|_| ActorError::DowncastError)?;

                // println!("Unwrapped result: {:?}", &lsp_result);
                // let lsp_result = typed_result.map_err(|e| ActorError::CustomError(Box::new(e)))?;

                let json_value = serde_json::to_value(lsp_result)
                    .map_err(|e| ActorError::CustomError(Box::new(e)))?;
                // println!("Unwrapped json result: {:?}", &json_value);
                Ok(Box::new(json_value) as Box<dyn Response>)
            },
        );
        self.dispatcher
            .register_unwrapper(MessageKey::new(R::METHOD), result_unwrapper);

        self
    }

    fn handle_notification<N: Notification>(
        &mut self,
        handler: impl for<'b> AsyncMutatingFunc<'b, S, N::Params, (), ResponseError> + 'static,
    ) -> &mut Self {
        let param_handler = Box::new(
            move |params: Box<dyn Message>| -> Result<Box<dyn Message>, ActorError> {
                let params = params.downcast::<serde_json::Value>().map_err(|_| {
                    println!("Failed to downcast params to serde_json::Value");
                    ActorError::DowncastError
                })?;
                let typed_params: N::Params = serde_json::from_value(*params).map_err(|e| {
                    println!("Deserialization error: {:?}", e);
                    ActorError::CustomError(Box::new(e))
                })?;
                Ok(Box::new(typed_params) as Box<dyn Message>)
            },
        );

        self.dispatcher
            .register_wrapper(MessageKey::new(N::METHOD), param_handler);

        self.actor_ref
            .register_handler_async_mutating(MessageKey::new(N::METHOD), handler);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use act_locally::{actor::Actor, builder::ActorBuilder};
    use async_lsp::{
        lsp_types::{InitializeParams, InitializeResult},
        RequestId,
    };
    use serde_json::{json, Value};

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

        let mut dispatcher = LspDispatcher::new();

        let mut registration = HandlerRegistration {
            actor_ref: &actor_ref,
            dispatcher: &mut dispatcher,
        };

        async fn handle_initialize(
            state: &mut TestState,
            _: InitializeParams,
        ) -> Result<InitializeResult, ResponseError> {
            println!("Handling initialize request");
            state.initialized = true;
            Ok(InitializeResult::default())
        }

        registration.handle_request::<Initialize>(handle_initialize);

        async fn handle_initialized(state: &mut TestState, _: ()) -> Result<(), ResponseError> {
            println!("Handling initialized notification");
            assert!(state.initialized, "State should be initialized");
            Ok(())
        }
        registration.handle_notification::<Initialized>(handle_initialized);

        // Test initialize request
        let init_params = InitializeParams::default();
        let init_request = AnyRequest {
            id: RequestId::Number(1),
            method: Initialize::METHOD.to_string(),
            params: serde_json::to_value(init_params).unwrap(),
        };

        let init_request2 = init_request.clone();
        println!("Sending initialize request");
        let init_result: Value = match actor_ref.ask(&dispatcher, init_request).await {
            Ok(res) => res,
            Err(e) => {
                panic!("Failed to get InitializeResult: {:?}", e);
            }
        };

        let init_result_deserialized: InitializeResult =
            serde_json::from_value(init_result).unwrap();

        assert_eq!(init_result_deserialized, InitializeResult::default());

        // Test initialized notification
        let init_notification = AnyNotification {
            method: Initialized::METHOD.to_string(),
            params: json!(null),
        };

        println!("Sending initialized notification");
        if let Err(e) = actor_ref.tell(&dispatcher, init_notification) {
            panic!("Failed to send Initialized notification: {:?}", e);
        }

        // Wait a bit to ensure the notification is processed
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }
}
