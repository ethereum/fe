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
            
            // debugging spam
            // if (std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_secs() % 1) == 0 {
            //     self.log_info(String::from("hi"))?;
            // }
        }
        Ok(())
    }
    
    fn next_message(&self, receiver: &Receiver<Message>) -> Option<Message> {
        crossbeam_channel::select! {
            recv(receiver) -> msg => msg.ok()
        }
    }
    
    fn handle_message(&mut self, msg: lsp_server::Message) -> Result<()> {
        // log the message with `self.log_info`
        self.log_info(format!("MESSAGE: {:?}", msg))?;

        if let lsp_server::Message::Request(req) = msg {
            // log the request to the console

            // handle hover request
            if req.method == lsp_types::request::HoverRequest::METHOD {
                // log the hover request to the console
                let params = lsp_types::HoverParams::deserialize(req.params)?;
                // for now let's just return "hi"
                
                let result = lsp_types::Hover {
                    contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(format!("{:?}", params))),
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
    
    fn log_info(&mut self, message: String) -> Result<()> {
        self.sender.send(
            lsp_server::Message::Notification(lsp_server::Notification {
                method: String::from("window/logMessage"),
                params: serde_json::to_value(lsp_types::LogMessageParams {
                    typ: lsp_types::MessageType::INFO,
                    message: message,
                }).unwrap()
            })
        )?;
        Ok(())
    }
}
