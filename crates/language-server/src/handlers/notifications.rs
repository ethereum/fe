

use anyhow::{Error, Result};
use fxhash::FxHashMap;
use serde::Deserialize;

use crate::{state::ServerState, util::diag_to_lsp, workspace::{IngotFileContext, SyncableInputFile}};

fn string_diagnostics(
    state: &mut ServerState,
    path: &str,
    text: String,
) -> Vec<common::diagnostics::CompleteDiagnostic> {
    let db = &mut state.db;
    let workspace = &mut state.workspace;
    let file_path = path;
    let input = workspace.input_from_file_path(db, file_path).unwrap();
    input.set_text(db).to(text);
    let top_mod = workspace.top_mod_from_file_path(db, file_path).unwrap();
    db.run_on_top_mod(top_mod);
    db.finalize_diags()
}

// todo: handle diagnostics referencing multiple files
pub fn get_diagnostics(
    state: &mut ServerState,
    text: String,
    uri: lsp_types::Url,
) -> Result<FxHashMap<lsp_types::Url, lsp_types::Diagnostic>, Error> {
    let diags = string_diagnostics(
        state,
        uri.to_file_path().unwrap().to_str().unwrap(),
        text,
    );

    let diagnostics = diags
        .into_iter()
        .flat_map(|diag| diag_to_lsp(diag, &state.db).clone());

    Ok(diagnostics.collect())
}

pub fn handle_document_did_open(
    state: &mut ServerState,
    note: lsp_server::Notification,
) -> Result<(), Error> {
    let params = lsp_types::DidOpenTextDocumentParams::deserialize(note.params)?;
    let text = params.text_document.text;
    let diagnostics = get_diagnostics(state, text, params.text_document.uri.clone())?;
    send_diagnostics(state, diagnostics)
}

pub fn handle_document_did_change(
    state: &mut ServerState,
    note: lsp_server::Notification,
) -> Result<(), Error> {
    let params = lsp_types::DidChangeTextDocumentParams::deserialize(note.params)?;
    let text = params.content_changes[0].text.clone();
    let diagnostics = get_diagnostics(state, text, params.text_document.uri.clone())?;
    send_diagnostics(state, diagnostics)
}

fn send_diagnostics(
    state: &mut ServerState,
    diagnostics: FxHashMap<lsp_types::Url, lsp_types::Diagnostic>
) -> Result<(), Error> {
    let results = diagnostics.into_iter().map(|(uri, diag)| {
        let result = lsp_types::PublishDiagnosticsParams {
            uri,
            diagnostics: vec![diag],
            version: None,
        };
        lsp_server::Message::Notification(lsp_server::Notification {
            method: String::from("textDocument/publishDiagnostics"),
            params: serde_json::to_value(result).unwrap(),
        })
    });
    
    results.for_each(|result| {
        let sender = state.sender.lock().unwrap();
        let _ = sender.send(result);
    });

    Ok(())
}
