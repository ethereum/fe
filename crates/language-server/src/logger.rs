use std::io::Write;

use async_lsp::lsp_types::MessageType;
use async_lsp::{ClientSocket, LanguageClient};
use tokio::task::yield_now;
// use tower_lsp::Client;
use tracing_subscriber::fmt::writer::MakeWriterExt;
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::prelude::*;

pub async fn handle_log_messages(
    mut rx: tokio::sync::mpsc::UnboundedReceiver<(String, MessageType)>,
    client: ClientSocket,
) -> tokio::sync::mpsc::UnboundedReceiver<String> {
    loop {
        if let Some((message, message_type)) = rx.recv().await {
            client.log_message(message_type, message).await;
            yield_now().await;
        }
    }
}

#[derive(Clone)]
pub struct LoggerLayer {
    log_sender: tokio::sync::mpsc::UnboundedSender<(String, MessageType)>,
}

impl Write for LoggerLayer {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let message = String::from_utf8_lossy(buf).to_string();
        let _ = self.log_sender.send((message, MessageType::LOG));
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl MakeWriter<'_> for LoggerLayer {
    type Writer = Self;
    fn make_writer(&self) -> Self::Writer {
        self.clone()
    }
}

pub fn setup_logger(
    level: tracing::Level,
) -> Result<tokio::sync::mpsc::UnboundedReceiver<(String, MessageType)>, Box<dyn std::error::Error>>
{
    let (log_sender, log_receiver) =
        tokio::sync::mpsc::unbounded_channel::<(String, MessageType)>();
    let logger = LoggerLayer { log_sender };
    let logger = logger
        .with_filter(|metadata| {
            metadata
                .module_path()
                .map_or(false, |path| path.starts_with("fe_language_server"))
        })
        .with_max_level(level);

    let pretty_logger = tracing_subscriber::fmt::layer()
        .event_format(tracing_subscriber::fmt::format::format().pretty())
        .with_ansi(false)
        .with_writer(logger);

    #[cfg(tokio_unstable)]
    let console_layer = console_subscriber::spawn();

    #[cfg(tokio_unstable)]
    tracing_subscriber::registry()
        .with(pretty_logger)
        .with(console_layer)
        .init();

    #[cfg(not(tokio_unstable))]
    tracing_subscriber::registry().with(pretty_logger).init();

    Ok(log_receiver)
}
