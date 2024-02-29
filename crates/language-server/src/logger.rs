use std::sync::Arc;

use log::{Level, LevelFilter, Metadata, Record, SetLoggerError};
use tower_lsp::Client;

use crate::{backend::Backend, language_server::Server};

pub struct Logger {
    pub(crate) level: Level,
    pub(crate) client: Arc<tokio::sync::Mutex<Client>>,
}

impl log::Log for Logger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        let logger = self;
        metadata.level() <= logger.level
    }

    // TODO: investigate performance implications of spawning tasks for each log message
    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            let message = format!("{} - {}", record.level(), record.args());
            let level = record.level();
            let client = self.client.clone();
            tokio::spawn(async move {
                let client = client.lock().await;
                client
                    .log_message(
                        match level {
                            log::Level::Error => lsp_types::MessageType::ERROR,
                            log::Level::Warn => lsp_types::MessageType::WARNING,
                            log::Level::Info => lsp_types::MessageType::INFO,
                            log::Level::Debug => lsp_types::MessageType::LOG,
                            log::Level::Trace => lsp_types::MessageType::LOG,
                        },
                        message,
                    )
                    .await;
            });
        }
    }

    fn flush(&self) {}
}

impl Server {
    pub fn init_logger(&self, level: Level) -> Result<(), SetLoggerError> {
        let logger = Logger {
            level,
            client: self.client.clone(),
        };
        let static_logger = Box::leak(Box::new(logger));
        log::set_logger(static_logger)?;
        log::set_max_level(LevelFilter::Debug);
        Ok(())
    }
}
