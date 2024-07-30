use async_lsp::{
    lsp_types::{notification::Notification, request::Request},
    AnyNotification, AnyRequest,
};
use serde::de::DeserializeOwned;
use std::{any::Any, collections::HashMap};

use crate::actor::{
    Actor, ActorError, AsyncFunc, BoxedAny, Dispatcher, HandlerRegistration, MessageKey,
};

pub struct LspDispatcher {
    wrappers: HashMap<String, Box<dyn Fn(BoxedAny) -> Result<BoxedAny, ActorError> + Send + Sync>>,
}

impl LspDispatcher {
    pub fn new() -> Self {
        Self {
            wrappers: HashMap::new(),
        }
    }

    pub fn dispatch_request(&self, request: &AnyRequest) -> Result<BoxedAny, ActorError> {
        let handler = self
            .wrappers
            .get(&request.method)
            .ok_or(ActorError::HandlerNotFound)?;
        handler(Box::new(request.params.clone()))
    }

    pub fn dispatch_notification(
        &self,
        notification: &AnyNotification,
    ) -> Result<BoxedAny, ActorError> {
        let handler = self
            .wrappers
            .get(&notification.method)
            .ok_or(ActorError::HandlerNotFound)?;
        handler(Box::new(notification.params.clone()))
    }
}

impl Dispatcher for LspDispatcher {
    fn generate_key(&self, message: &dyn Any) -> Result<MessageKey, ActorError> {
        if let Some(request) = message.downcast_ref::<AnyRequest>() {
            Ok(MessageKey::new(&request.method))
        } else if let Some(notification) = message.downcast_ref::<AnyNotification>() {
            Ok(MessageKey::new(&notification.method))
        } else {
            Err(ActorError::DispatchError)
        }
    }

    fn wrap(&self, message: BoxedAny) -> Result<BoxedAny, ActorError> {
        if let Some(request) = message.downcast_ref::<AnyRequest>() {
            self.dispatch_request(&request.clone())
        } else if let Some(notification) = message.downcast_ref::<AnyNotification>() {
            self.dispatch_notification(&notification.clone())
        } else {
            Err(ActorError::DispatchError)
        }
    }

    fn unwrap(&self, message: BoxedAny) -> Result<BoxedAny, ActorError> {
        Ok(message)
    }

    fn register_wrapper(
        &mut self,
        key: MessageKey,
        wrapper: Box<dyn Fn(BoxedAny) -> BoxedAny + Send + Sync>,
    ) {
        let wrapper = Box::new(move |params: BoxedAny| -> Result<BoxedAny, ActorError> {
            Ok(wrapper(params))
        });
        let MessageKey(key) = key;
        self.wrappers.insert(key, wrapper);
    }
}

pub trait LspActor<S: 'static> {
    fn handle_request<R: Request>(
        &mut self,
        handler: impl for<'a> AsyncFunc<'a, S, R::Params, R::Result, ActorError> + 'static,
    );
    fn handle_notification<N: Notification>(
        &mut self,
        handler: impl for<'a> AsyncFunc<'a, S, N::Params, (), ActorError> + 'static,
    );
}

impl<'a, S: 'static> LspActor<S> for HandlerRegistration<'a, S, LspDispatcher> {
    fn handle_request<R: Request>(
        &mut self,
        handler: impl for<'b> AsyncFunc<'b, S, R::Params, R::Result, ActorError> + 'static,
    ) {
        let param_handler = Box::new(move |params: BoxedAny| -> BoxedAny {
            let params = params.downcast::<serde_json::Value>().unwrap();
            let typed_params: R::Params = serde_json::from_value(*params).unwrap();
            Box::new(typed_params) as BoxedAny
        });

        self.dispatcher
            .register_wrapper(MessageKey::new(R::METHOD), param_handler);

        self.actor
            .register_handler(MessageKey::new(R::METHOD), handler);
    }

    fn handle_notification<N: Notification>(
        &mut self,
        handler: impl for<'b> AsyncFunc<'b, S, N::Params, (), ActorError> + 'static,
    ) {
        let param_handler = Box::new(move |params: BoxedAny| -> BoxedAny {
            let params = params.downcast::<serde_json::Value>().unwrap();
            let typed_params: N::Params = serde_json::from_value(*params).unwrap();
            Box::new(typed_params) as BoxedAny
        });

        self.dispatcher
            .register_wrapper(MessageKey::new(N::METHOD), param_handler);

        self.actor
            .register_handler(MessageKey::new(N::METHOD), handler);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_lsp::{
        lsp_types::{InitializeParams, InitializeResult},
        RequestId,
    };
    use serde_json::json;

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
        let (actor_ref, dispatcher) = Actor::spawn_local(|| {
            let initial_state = TestState { initialized: false };
            let (mut actor, actor_ref) = Actor::new(initial_state);
            let mut dispatcher = LspDispatcher::new();

            let mut registration = HandlerRegistration {
                actor: &mut actor,
                dispatcher: &mut dispatcher,
            };

            async fn handle_initialize(
                state: &mut TestState,
                _: InitializeParams,
            ) -> Result<InitializeResult, ActorError> {
                println!("Handling initialize request");
                state.initialized = true;
                Ok(InitializeResult::default())
            }

            registration.handle_request::<Initialize>(handle_initialize);

            async fn handle_initialized(state: &mut TestState, _: ()) -> Result<(), ActorError> {
                println!("Handling initialized notification");
                assert!(state.initialized, "State should be initialized");
                Ok(())
            }
            registration.handle_notification::<Initialized>(handle_initialized);

            (actor, actor_ref, dispatcher)
        });

        // Test initialize request
        let init_params = InitializeParams::default();
        let init_request = AnyRequest {
            id: RequestId::Number(1),
            method: Initialize::METHOD.to_string(),
            params: serde_json::to_value(init_params).unwrap(),
        };

        let init_result: InitializeResult = actor_ref.ask(&dispatcher, init_request).await.unwrap();
        assert_eq!(init_result, InitializeResult::default());

        // Test initialized notification
        let init_notification = AnyNotification {
            method: Initialized::METHOD.to_string(),
            params: json!(null),
        };

        actor_ref.tell(&dispatcher, init_notification).unwrap();

        // Wait a bit to ensure the notification is processed
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }
}
