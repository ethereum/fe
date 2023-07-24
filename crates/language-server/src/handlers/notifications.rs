// use anyhow::Result;
// use fe_analyzer::{namespace::items::ModuleId, TestDb};
// use serde::Deserialize;

// use crate::{state::ServerState, util::diag_to_lsp};

// fn string_diagnostics(path: &str, src: &str) -> Vec<common::diagnostics::CompleteDiagnostic> {
//     let mut db = TestDb::default();
//     let module = ModuleId::new_standalone(&mut db, path, src);

//     module.diagnostics(&db)
// }

// pub(crate) fn handle_document_did_change(state: &mut ServerState, req: lsp_server::Request) -> Result<(), Error> {
//     let params = lsp_types::DidChangeTextDocumentParams::deserialize(req.params)?;
//     let text = params.text_document.text;

// }

// pub(crate) fn handle_document_did_open(
//     state: &mut ServerState,
//     note: lsp_server::Notification,
// ) -> Result<(), anyhow::Error> {
//     let params = lsp_types::DidOpenTextDocumentParams::deserialize(note.params)?;
//     let text = params.text_document.text;

//     let diags = string_diagnostics(
//         params.text_document.uri.to_file_path().unwrap().to_str().unwrap(),
//         text.as_str(),
//     );
    
//     state.log_info(format!("diagnostics: {:?}", diags))?;
    
//     // send diagnostics using `state.send_response` for each diagnostic
    
//     let diagnostics = diags.into_iter().flat_map(|diag| {
//         diag_to_lsp(diag, text.as_str()).iter().map(|x| x.clone()).collect::<Vec<_>>()
//     });
    
//     let result = lsp_types::PublishDiagnosticsParams {
//         uri: params.text_document.uri.clone(),
//         diagnostics: diagnostics.collect(),
//         version: None,
//     };
//     let response = lsp_server::Message::Notification(lsp_server::Notification {
//         method: String::from("textDocument/publishDiagnostics"),
//         params: serde_json::to_value(result).unwrap(),
//     });

//     state.sender.send(response)?;
    
//     Ok(())
// }
