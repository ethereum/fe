use anyhow::{Result, Error};
use serde::Deserialize;

use crate::{state::ServerState, util::diag_to_lsp, db::LanguageServerDatabase};

fn string_diagnostics(db: &mut LanguageServerDatabase, path: &str, src: &str) -> Vec<common::diagnostics::CompleteDiagnostic> {
    let file_path = std::path::Path::new(path);
    let top_mod = db.top_mod_from_file(file_path, src);
    db.run_on_top_mod(top_mod);
    db.finalize_diags()
}

// pub(crate) fn handle_document_did_change(state: &mut ServerState, req: lsp_server::Request) -> Result<(), Error> {
// todo: incremental parsing and diagnostics
// }

pub(crate) fn handle_document_did_open(
    state: &mut ServerState,
    note: lsp_server::Notification,
) -> Result<(), Error> {
    let params = lsp_types::DidOpenTextDocumentParams::deserialize(note.params)?;
    let text = params.text_document.text;

    let diags = string_diagnostics(
        &mut state.db,
        params.text_document.uri.to_file_path().unwrap().to_str().unwrap(),
        text.as_str(),
    );
    
    state.log_info(format!("diagnostics: {:?}", diags))?;
    
    // send diagnostics using `state.send_response` for each diagnostic
    
    let diagnostics = diags.into_iter().flat_map(|diag| {
        diag_to_lsp(diag, &state.db).iter().map(|x| x.clone()).collect::<Vec<_>>()
    });
    
    let result = lsp_types::PublishDiagnosticsParams {
        uri: params.text_document.uri.clone(),
        diagnostics: diagnostics.collect(),
        version: None,
    };
    let response = lsp_server::Message::Notification(lsp_server::Notification {
        method: String::from("textDocument/publishDiagnostics"),
        params: serde_json::to_value(result).unwrap(),
    });

    state.sender.send(response)?;
    
    Ok(())
}
