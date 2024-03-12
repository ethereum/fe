use log::{Level, LevelFilter, Metadata, Record, SetLoggerError};
use lsp_types::MessageType;
use tokio::task::yield_now;
use tower_lsp::Client;

pub struct Logger {
    pub(crate) level: Level,
    log_sender: tokio::sync::mpsc::UnboundedSender<(String, MessageType)>,
}

impl log::Log for Logger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        let logger = self;
        metadata.level() <= logger.level
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            let message = format!("{} - {}", record.level(), record.args());
            let message_type = match record.level() {
                log::Level::Error => MessageType::ERROR,
                log::Level::Warn => MessageType::WARNING,
                log::Level::Info => MessageType::INFO,
                log::Level::Debug => MessageType::LOG,
                log::Level::Trace => MessageType::LOG,
            };
            self.log_sender.send((message, message_type)).unwrap();
        }
    }

    fn flush(&self) {}
}

pub fn setup_logger(
    level: Level,
) -> Result<tokio::sync::mpsc::UnboundedReceiver<(String, MessageType)>, SetLoggerError> {
    let (log_sender, log_receiver) =
        tokio::sync::mpsc::unbounded_channel::<(String, MessageType)>();
    let logger = Logger { level, log_sender };
    let static_logger = Box::leak(Box::new(logger));
    log::set_logger(static_logger)?;
    log::set_max_level(LevelFilter::Debug);
    Ok(log_receiver)
}

pub async fn handle_log_messages(
    mut rx: tokio::sync::mpsc::UnboundedReceiver<(String, MessageType)>,
    client: Client,
) -> tokio::sync::mpsc::UnboundedReceiver<String> {
    loop {
        let (message, message_type) = rx.recv().await.unwrap();
        client.log_message(message_type, message).await;
        yield_now().await;
    }
}
