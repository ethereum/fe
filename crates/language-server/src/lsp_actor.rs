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

//     fn as_any(&self) -> &dyn Any {
//         self
//     }

//     fn as_any_mut(&mut self) -> &mut dyn Any {
//         self
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
//         if message.is::<AnyRequest>() {
//             Ok(DispatcherEnvelope::Request(message))
//         } else if message.is::<AnyNotification>() {
//             Ok(DispatcherEnvelope::Notification(message))
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

// impl<S: 'static> LspActor<S> for Actor<S> {
//     fn handle_request<R: Request>(
//         &mut self,
//         handler: impl for<'a> AsyncFunc<'a, S, R::Params, R::Result, ActorError> + 'static,
//     ) {
//         let mut dispatcher = self.dispatcher.write().unwrap();
//         let lsp_dispatcher = dispatcher
//             .as_any_mut()
//             .downcast_mut::<LspDispatcher>()
//             .expect("LspDispatcher not found");

//         let param_handler = Box::new(
//             move |params: serde_json::Value| -> Result<BoxedAny, ActorError> {
//                 let typed_params: R::Params = serde_json::from_value(params)
//                     .map_err(|e| ActorError::CustomError(Box::new(e)))?;
//                 Ok(Box::new(typed_params) as BoxedAny)
//             },
//         );

//         lsp_dispatcher.register_request::<R>(param_handler);
//         drop(dispatcher); // Release the write lock

//         self.register(MessageKey::new(R::METHOD)).handle(handler);
//     }

//     fn handle_notification<N: Notification>(
//         &mut self,
//         handler: impl for<'a> AsyncFunc<'a, S, N::Params, (), ActorError> + 'static,
//     ) {
//         let mut dispatcher = self.dispatcher.write().unwrap();
//         let lsp_dispatcher = dispatcher
//             .as_mut()
//             .as_any_mut()
//             .downcast_mut::<LspDispatcher>()
//             .expect("LspDispatcher not found");

//         let param_handler = Box::new(
//             move |params: serde_json::Value| -> Result<BoxedAny, ActorError> {
//                 let typed_params: N::Params = serde_json::from_value(params)
//                     .map_err(|e| ActorError::CustomError(Box::new(e)))?;
//                 Ok(Box::new(typed_params) as BoxedAny)
//             },
//         );

//         lsp_dispatcher.register_notification::<N>(param_handler);
//         drop(dispatcher); // Release the write lock

//         self.register(MessageKey::new(N::METHOD)).handle(handler);
//     }
// }
