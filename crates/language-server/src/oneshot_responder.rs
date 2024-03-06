use std::fmt::Debug;

use log::{debug, error};
#[derive(Debug)]
pub struct OneshotResponder<T: Debug + Clone> {
    pub(crate) sender: std::sync::Arc<std::sync::Mutex<Option<tokio::sync::oneshot::Sender<T>>>>,
}

impl<T: Debug + Clone> Clone for OneshotResponder<T> {
    fn clone(&self) -> OneshotResponder<T> {
        Self {
            sender: self.sender.clone(),
        }
    }
}

impl<T: Debug + Clone> OneshotResponder<T> {
    pub fn from(sender: tokio::sync::oneshot::Sender<T>) -> Self {
        Self {
            sender: std::sync::Arc::new(std::sync::Mutex::new(Some(sender))),
        }
    }
    pub fn respond(self, response: T) {
        debug!("responding with: {:?}", response);
        let mut sender = self.sender.lock().unwrap();

        match sender.take() {
            Some(sender) => {
                debug!("sending response: {:?} and {:?}", response, sender);
                match sender.send(response) {
                    Ok(_) => {
                        debug!("Response sent successfully")
                    }
                    Err(e) => error!("Failed to send response: {:?}", e),
                }
            }
            None => {
                error!("OneshotResponder already responded");
            }
        }
    }
}
