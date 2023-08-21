use anyhow::Result;
use crossbeam_channel::{Receiver, Sender};
use lsp_server::Message;
use lsp_types::notification::Notification;
use lsp_types::request::Request;
use crate::db::LanguageServerDatabase;

use crate::handlers::request::handle_goto_definition;
use crate::handlers::{
    request::handle_hover,
    notifications::handle_document_did_open
};

pub struct ServerState {
    pub sender: Sender<Message>,
    pub db: LanguageServerDatabase,
}

impl ServerState {
    pub fn new(sender: Sender<Message>) -> Self {
        ServerState { sender, db: LanguageServerDatabase::default() }
    }

    pub fn run(&mut self, receiver: Receiver<lsp_server::Message>) -> Result<()> {
        while let Some(msg) = self.next_message(&receiver) {
            if let lsp_server::Message::Notification(notification) = &msg {
                if notification.method == lsp_types::notification::Exit::METHOD {
                    return Ok(());
                }
            }

            let _ = self.handle_message(msg);
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
            self.log_info(format!("REQUEST: {:?}", req))?;

            match req.method.as_str() {
                // TODO: implement actually useful hover handler
                lsp_types::request::HoverRequest::METHOD => handle_hover(self, req)?,
                // goto definition
                lsp_types::request::GotoDefinition::METHOD => handle_goto_definition(self, req)?,
                lsp_types::request::GotoTypeDefinition::METHOD => handle_goto_definition(self, req)?,
                lsp_types::request::GotoImplementation::METHOD => handle_goto_definition(self, req)?,
                lsp_types::request::GotoDeclaration::METHOD => handle_goto_definition(self, req)?,
                _ => {}
            }
        
            
        } else if let lsp_server::Message::Notification(note) = msg {
            // log the notification to the console
            self.log_info(format!("NOTIFICATION: {:?}", note))?;
            
            match note.method.as_str() {
                lsp_types::notification::DidOpenTextDocument::METHOD => handle_document_did_open(self, note)?,
                lsp_types::notification::DidChangeTextDocument::METHOD => handle_document_did_open(self, note)?,
                _ => {}
            }
        }

        Ok(())
    }

    pub(crate) fn send_response(&mut self, response: lsp_server::Response) -> Result<()> {
        self.sender.send(lsp_server::Message::Response(response))?;
        Ok(())
    }

    pub(crate) fn log_info(&mut self, message: String) -> Result<()> {
        self.sender.send(lsp_server::Message::Notification(
            lsp_server::Notification {
                method: String::from("window/logMessage"),
                params: serde_json::to_value(lsp_types::LogMessageParams {
                    typ: lsp_types::MessageType::INFO,
                    message: message,
                })
                .unwrap(),
            },
        ))?;
        Ok(())
    }
}
