// use async_lsp::{
//     lsp_types::{notification::Notification, request::Request},
//     AnyNotification, AnyRequest,
// };
// use serde::de::DeserializeOwned;
// use std::{any::Any, collections::HashMap};

// use crate::actor::{
//     Actor, ActorError, AsyncFunc, BoxedAny, Dispatcher, DispatcherEnvelope, MessageKey,
// };

// pub struct LspDispatcher {
//     request_handlers: HashMap<
//         String,
//         Box<dyn Fn(serde_json::Value) -> Result<BoxedAny, ActorError> + Send + Sync>,
//     >,
//     notification_handlers: HashMap<
//         String,
//         Box<dyn Fn(serde_json::Value) -> Result<BoxedAny, ActorError> + Send + Sync>,
//     >,
// }

// impl LspDispatcher {
//     pub fn new() -> Self {
//         Self {
//             request_handlers: HashMap::new(),
//             notification_handlers: HashMap::new(),
//         }
//     }

//     pub fn register_request<R: Request>(
//         &mut self,
//         handler: Box<dyn Fn(serde_json::Value) -> Result<BoxedAny, ActorError> + Send + Sync>,
//     ) {
//         self.request_handlers.insert(R::METHOD.to_string(), handler);
//     }

//     pub fn register_notification<N: Notification>(
//         &mut self,
//         handler: Box<dyn Fn(serde_json::Value) -> Result<BoxedAny, ActorError> + Send + Sync>,
//     ) {
//         self.notification_handlers
//             .insert(N::METHOD.to_string(), handler);
//     }

//     pub fn dispatch_request(&self, request: AnyRequest) -> Result<BoxedAny, ActorError> {
//         let handler = self
//             .request_handlers
//             .get(&request.method)
//             .ok_or(ActorError::HandlerNotFound)?;
//         handler(request.params)
//     }

//     pub fn dispatch_notification(
//         &self,
//         notification: AnyNotification,
//     ) -> Result<BoxedAny, ActorError> {
//         let handler = self
//             .notification_handlers
//             .get(&notification.method)
//             .ok_or(ActorError::HandlerNotFound)?;
//         handler(notification.params)
//     }
// }

// impl Dispatcher for LspDispatcher {
//     fn create_key(&self, message: &dyn Any) -> Result<MessageKey, ActorError> {
//         if let Some(request) = message.downcast_ref::<AnyRequest>() {
//             Ok(MessageKey::new(&request.method))
//         } else if let Some(notification) = message.downcast_ref::<AnyNotification>() {
//             Ok(MessageKey::new(&notification.method))
//         } else {
//             Err(ActorError::DispatchError)
//         }
//     }

//     fn wrap(&self, message: BoxedAny) -> Result<DispatcherEnvelope, ActorError> {
//         if let Some(request) = message.downcast_ref::<AnyRequest>() {
//             let dispatched = self.dispatch_request(request.clone())?;
//             Ok(DispatcherEnvelope::Request(Box::new(dispatched)))
//         } else if let Some(notification) = message.downcast_ref::<AnyNotification>() {
//             let dispatched = self.dispatch_notification(notification.clone())?;
//             Ok(DispatcherEnvelope::Notification(Box::new(dispatched)))
//         } else {
//             Err(ActorError::DispatchError)
//         }
//     }

//     fn unwrap(&self, message: BoxedAny) -> Result<BoxedAny, ActorError> {
//         Ok(message)
//     }
// }

// pub trait LspActor<S: 'static> {
//     fn handle_request<R: Request>(
//         &mut self,
//         handler: impl for<'a> AsyncFunc<'a, S, R::Params, R::Result, ActorError> + 'static,
//     );
//     fn handle_notification<N: Notification>(
//         &mut self,
//         handler: impl for<'a> AsyncFunc<'a, S, N::Params, (), ActorError> + 'static,
//     );
// }

// impl<S: 'static> LspActor<S> for Actor<S, LspDispatcher> {
//     fn handle_request<R: Request>(
//         &mut self,
//         handler: impl for<'a> AsyncFunc<'a, S, R::Params, R::Result, ActorError> + 'static,
//     ) {
//         let mut dispatcher = self.dispatcher.write().unwrap();

//         let param_handler = Box::new(
//             move |params: serde_json::Value| -> Result<BoxedAny, ActorError> {
//                 let typed_params: R::Params = serde_json::from_value(params)
//                     .map_err(|e| ActorError::CustomError(Box::new(e)))?;
//                 Ok(Box::new(typed_params) as BoxedAny)
//             },
//         );

//         dispatcher.register_request::<R>(param_handler);
//         drop(dispatcher); // Release the write lock

//         self.register(MessageKey::new(R::METHOD)).handle(handler);
//     }

//     fn handle_notification<N: Notification>(
//         &mut self,
//         handler: impl for<'a> AsyncFunc<'a, S, N::Params, (), ActorError> + 'static,
//     ) {
//         let mut dispatcher = self.dispatcher.write().unwrap();

//         let param_handler = Box::new(
//             move |params: serde_json::Value| -> Result<BoxedAny, ActorError> {
//                 let typed_params: N::Params = serde_json::from_value(params)
//                     .map_err(|e| ActorError::CustomError(Box::new(e)))?;
//                 Ok(Box::new(typed_params) as BoxedAny)
//             },
//         );

//         dispatcher.register_notification::<N>(param_handler);
//         drop(dispatcher); // Release the write lock

//         self.register(MessageKey::new(N::METHOD)).handle(handler);
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use async_lsp::lsp_types::{InitializeParams, InitializeResult};
//     use serde_json::json;

//     #[derive(Debug)]
//     enum Initialize {}

//     impl Request for Initialize {
//         type Params = InitializeParams;
//         type Result = InitializeResult;
//         const METHOD: &'static str = "initialize";
//     }

//     #[derive(Debug)]
//     enum Initialized {}

//     impl Notification for Initialized {
//         type Params = ();
//         const METHOD: &'static str = "initialized";
//     }

//     struct TestState {
//         initialized: bool,
//     }

//     #[tokio::test]
//     async fn test_lsp_actor() {
//         let actor_ref = Actor::spawn_local(|| {
//             let initial_state = TestState { initialized: false };
//             let (mut actor, actor_ref) =
//                 Actor::new_with_dispatcher(initial_state, LspDispatcher::new());

//             async fn handle_initialize(
//                 state: &mut TestState,
//                 _: InitializeParams,
//             ) -> Result<InitializeResult, ActorError> {
//                 println!("Handling initialize request");
//                 state.initialized = true;
//                 Ok(InitializeResult::default())
//             }

//             actor.handle_request::<Initialize>(handle_initialize);

//             async fn handle_initialized(state: &mut TestState, _: ()) -> Result<(), ActorError> {
//                 println!("Handling initialized notification");
//                 assert!(state.initialized, "State should be initialized");
//                 Ok(())
//             }
//             actor.handle_notification::<Initialized>(handle_initialized);

//             (actor, actor_ref)
//         });

//         // Test initialize request
//         // let init_params = InitializeParams::default();
//         // let init_request = AnyRequest {
//         //     id: RequestId::from(1),
//         //     method: Initialize::METHOD.to_string(),
//         //     params: serde_json::to_value(init_params).unwrap(),
//         // };

//         // let init_result: InitializeResult = actor_ref.ask(init_request).await.unwrap();
//         // assert_eq!(init_result, InitializeResult::default());

//         // // Test initialized notification
//         // let init_notification = AnyNotification {
//         //     method: Initialized::METHOD.to_string(),
//         //     params: json!(null),
//         // };

//         // actor_ref.tell(init_notification).unwrap();

//         // Wait a bit to ensure the notification is processed
//         tokio::time::sleep(std::time::Duration::from_millis(100)).await;
//     }
// }
