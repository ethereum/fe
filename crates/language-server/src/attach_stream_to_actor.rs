use futures::{channel::oneshot, Future, Stream, StreamExt, TryFutureExt, TryStreamExt};
use std::convert::TryInto;

use crate::actor::{ActorError, ActorRef};

pub trait ActorStreamExt: Stream {
    fn attach_to_actor<M>(self, actor: ActorRef) -> impl Future<Output = Result<(), ActorError>>
    where
        Self: Sized + TryStreamExt,
        Self::Error: Into<ActorError>,
        Self::Ok: TryInto<M>,
        M: Send + 'static,
        <Self::Ok as TryInto<M>>::Error: std::fmt::Debug,
    {
        self.try_for_each(move |item| {
            let actor = actor.clone();
            async move {
                match item.try_into() {
                    Ok(message) => {
                        if let Err(e) = actor.tell(message) {
                            eprintln!("Error sending message to actor: {:?}", e);
                        }
                        Ok(())
                    }
                    Err(e) => {
                        eprintln!("Error converting stream item to actor message: {:?}", e);
                        Ok(())
                    }
                }
            }
        })
        .map_err(|e| e.into())
    }

    fn attach_requests_to_actor<C, R, E>(
        self,
        actor: ActorRef,
    ) -> impl Future<Output = Result<(), ActorError>>
    where
        Self: Sized + TryStreamExt,
        Self::Error: Into<ActorError>,
        Self::Ok: Into<(C, oneshot::Sender<Result<R, E>>)>,
        C: Send + 'static,
        R: Send + 'static,
        E: From<ActorError> + Send + 'static,
    {
        self.try_for_each(move |item| {
            let actor = actor.clone();
            async move {
                let (message, response_sender) = item.into();
                match actor.ask::<C, R>(message).await {
                    Ok(result) => {
                        let _ = response_sender.send(Ok(result));
                        Ok(())
                    }
                    Err(e) => {
                        eprintln!("Error sending request to actor: {:?}", e);
                        let _ = response_sender.send(Err(E::from(e)));
                        Ok(())
                    }
                }
            }
        })
        .map_err(|e| e.into())
    }
}

impl<T: ?Sized> ActorStreamExt for T where T: Stream + TryStreamExt {}
