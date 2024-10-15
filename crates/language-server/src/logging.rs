use async_lsp::{
    lsp_types::{LogMessageParams, MessageType},
    ClientSocket, LanguageClient,
};
use tracing::{subscriber::set_default, Level, Metadata};
use tracing_subscriber::{fmt::MakeWriter, layer::SubscriberExt};
use tracing_tree::HierarchicalLayer;

use std::sync::Arc;

pub fn setup(client: ClientSocket) -> impl FnOnce() -> Option<tracing::subscriber::DefaultGuard> {
    move || {
        let client_socket_writer = ClientSocketWriterMaker::new(client);
        let subscriber = tracing_subscriber::registry()
            .with(
                HierarchicalLayer::new(2)
                    .with_thread_ids(true)
                    .with_thread_names(true)
                    .with_indent_lines(true)
                    .with_bracketed_fields(true)
                    .with_ansi(false)
                    .with_writer(client_socket_writer),
            )
            .with(
                HierarchicalLayer::new(2)
                    .with_thread_ids(true)
                    .with_thread_names(true)
                    .with_indent_lines(true)
                    .with_bracketed_fields(true)
                    .with_writer(std::io::stderr),
            );
        Some(set_default(subscriber))
    }
}

pub(crate) struct ClientSocketWriterMaker {
    pub(crate) client_socket: Arc<ClientSocket>,
}

impl ClientSocketWriterMaker {
    pub fn new(client_socket: ClientSocket) -> Self {
        ClientSocketWriterMaker {
            client_socket: Arc::new(client_socket),
        }
    }
}

pub(crate) struct ClientSocketWriter {
    client_socket: Arc<ClientSocket>,
    typ: MessageType,
}

impl std::io::Write for ClientSocketWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let message = String::from_utf8_lossy(buf).to_string();
        let params = LogMessageParams {
            typ: self.typ,
            message,
        };

        let mut client_socket = self.client_socket.as_ref();
        _ = client_socket.log_message(params);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl<'a> MakeWriter<'a> for ClientSocketWriterMaker {
    type Writer = ClientSocketWriter;

    fn make_writer(&'a self) -> Self::Writer {
        ClientSocketWriter {
            client_socket: self.client_socket.clone(),
            typ: MessageType::LOG,
        }
    }

    fn make_writer_for(&'a self, meta: &Metadata<'_>) -> Self::Writer {
        let typ = match *meta.level() {
            Level::ERROR => MessageType::ERROR,
            Level::WARN => MessageType::WARNING,
            Level::INFO => MessageType::INFO,
            Level::DEBUG => MessageType::LOG,
            Level::TRACE => MessageType::LOG,
        };

        ClientSocketWriter {
            client_socket: self.client_socket.clone(),
            typ,
        }
    }
}
