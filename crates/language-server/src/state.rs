use anyhow::Result;
use crossbeam_channel::{Receiver, Sender};
use lsp_server::Message;
use lsp_types::{notification::Notification};

pub struct ServerState {
    sender: Sender<Message>,
}

impl ServerState {
    pub fn new(sender: Sender<Message>) -> Self {
        ServerState {
           sender 
        }
    }
    
    pub fn run(&mut self, receiver: Receiver<lsp_server::Message>) -> Result<()> {
        while let Some(msg) = self.next_message(&receiver) {
            if let lsp_server::Message::Notification(notification) = &msg {
                if notification.method == lsp_types::notification::Exit::METHOD {
                    return Ok(());
                }
            }

            self.handle_message(msg)?;
        }
        Ok(())
    }
    
    fn next_message(&self, receiver: &Receiver<Message>) -> Option<Message> {
        crossbeam_channel::select! {
            recv(receiver) -> msg => msg.ok()
        }
    }
    
    fn handle_message(&mut self, msg: lsp_server::Message) -> Result<()> {
        Ok(())
    }
}
