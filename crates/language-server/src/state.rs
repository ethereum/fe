use anyhow::Result;
use crossbeam_channel::{Receiver, Sender};
use lsp_server::{Message, Response};
use lsp_types::{notification::Notification, request::Request};
use serde::Deserialize;

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
        if let lsp_server::Message::Request(req) = msg {
            // log the request to the console
            println!("request: {:?}", req);

            // handle hover request
            if req.method == lsp_types::request::HoverRequest::METHOD {
                let params = lsp_types::HoverParams::deserialize(req.params)?;
                // for now let's just return "hi"
                
                let result = lsp_types::Hover {
                    contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(String::from("hi"))),
                    range: None,
                };

                let response_message = lsp_server::Message::Response(Response {
                    id: req.id,
                    result: Some(serde_json::to_value(result)?),
                    error: None,
                });

                self.sender.send(response_message)?;
            }
        }

        Ok(())
    }
}
