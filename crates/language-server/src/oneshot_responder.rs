use std::fmt::Debug;

use log::{error, info};
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
        info!("responding with: {:?}", response);
        let mut sender = self.sender.lock().unwrap();

        // sender.send(response.clone());
        if let Some(sender) = sender.take() {
            info!("sending response: {:?} and {:?}", response, sender);
            match sender.send(response) {
                Ok(_) => info!("Response sent successfully"),
                Err(e) => error!("Failed to send response: {:?}", e),
            }
        }
    }
}
