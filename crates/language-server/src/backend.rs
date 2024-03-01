use std::sync::Arc;
use tokio::sync::Mutex;

// use tokio::sync::oneshot::Receiver;

use crate::capabilities::server_capabilities;
use crate::db::LanguageServerDatabase;

use crate::language_server::Server;
use crate::workspace::Workspace;

use log::info;

use tokio_stream::wrappers::BroadcastStream;
use tokio_stream::StreamExt;
use tower_lsp::Client;

pub struct Backend<'a> {
    // pub(crate) client: &'a Client,
    pub(crate) server: &'a Server,
    pub(crate) client: Arc<Mutex<Client>>,
    pub(crate) db: LanguageServerDatabase,
    pub(crate) workspace: Workspace,
}

impl<'a> Backend<'a> {
    pub fn new(client: Arc<Mutex<Client>>, server: &'a Server) -> Self {
        let workspace = Workspace::default();
        let db = LanguageServerDatabase::default();
        let backend = Self {
            server,
            client,
            db,
            workspace,
        };

        backend
    }
    pub fn setup_streams(mut self) {
        let mut stream = BroadcastStream::new(self.server.dispatch.subscribe_initialize());
        tokio::spawn(async move {
            while let Some(result) = stream.next().await {
                if let Ok((initialization_params, responder)) = result {
                    info!("initializing language server: {:?}", initialization_params);
                    // setup workspace
                    let capabilities = server_capabilities();
                    let initialize_result = lsp_types::InitializeResult {
                        capabilities,
                        server_info: Some(lsp_types::ServerInfo {
                            name: String::from("fe-language-server"),
                            version: Some(String::from(env!("CARGO_PKG_VERSION"))),
                        }),
                    };
                    let _ = self.workspace.set_workspace_root(
                        &mut self.db,
                        initialization_params
                            .root_uri
                            .unwrap()
                            .to_file_path()
                            .ok()
                            .unwrap(),
                    );

                    responder.respond(Ok(initialize_result));
                }
            }
        });
    }
}
