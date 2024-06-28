use async_lsp::lsp_types::{notification, request};
use async_lsp::router::Router;
use async_lsp::{ErrorCode, ResponseError};

use crate::actor::{ActorError, ActorRef};

pub trait ActOnRequest {
    fn act_on_request<R>(&mut self, actor_ref: &ActorRef)
    where
        R: request::Request + Send + 'static;
}

pub trait ActOnNotification {
    fn act_on_notification<N>(&mut self, actor_ref: &ActorRef)
    where
        N: notification::Notification;
}

impl<State> ActOnRequest for Router<State> {
    fn act_on_request<R>(&mut self, actor_ref: &ActorRef)
    where
        R: request::Request + Send + 'static,
    {
        let actor_ref = actor_ref.clone();
        self.request::<R, _>(move |_, params| {
            let actor_ref = actor_ref.clone();
            async move {
                actor_ref
                    .ask::<R::Params, R::Result>(params)
                    .await
                    .map_err(|e| match e {
                        ActorError::HandlerNotFound => {
                            ResponseError::new(ErrorCode::METHOD_NOT_FOUND, "Method not found")
                        }
                        _ => ResponseError::new(ErrorCode::INTERNAL_ERROR, format!("{:?}", e)),
                    })
            }
        });
    }
}

impl<State> ActOnNotification for Router<State> {
    fn act_on_notification<N>(&mut self, actor_ref: &ActorRef)
    where
        N: notification::Notification,
    {
        let actor_ref = actor_ref.clone();
        self.notification::<N>(move |_, params| {
            let actor_ref = actor_ref.clone();
            if let Err(e) = actor_ref.tell::<N::Params>(params) {
                eprintln!("Error sending notification: {:?}", e);
            }
            std::ops::ControlFlow::Continue(())
        });
    }
}
