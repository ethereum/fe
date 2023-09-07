use anyhow::{Error, Result};
use serde::Deserialize;

use crate::{state::ServerState, util::diag_to_lsp};

fn string_diagnostics(
    state: &mut ServerState,
    path: &str,
    src: &str,
) -> Vec<common::diagnostics::CompleteDiagnostic> {
    let db = &mut state.db;
    let workspace = &mut state.workspace;
    let file_path = std::path::Path::new(path);
    let top_mod = workspace.top_mod_from_file(db, file_path, src);
    db.run_on_top_mod(top_mod.unwrap());
    db.finalize_diags()
}

pub(crate) fn get_diagnostics(
    state: &mut ServerState,
    text: String,
    uri: lsp_types::Url,
) -> Result<Vec<lsp_types::Diagnostic>, Error> {
    let diags = string_diagnostics(
        state,
        uri.to_file_path().unwrap().to_str().unwrap(),
        text.as_str(),
    );

    let diagnostics = diags.into_iter().flat_map(|diag| {
        diag_to_lsp(diag, &state.db)
            .iter()
            .map(|x| x.clone())
            .collect::<Vec<_>>()
    });

    Ok(diagnostics.collect())
}

pub(crate) fn handle_document_did_open(
    state: &mut ServerState,
    note: lsp_server::Notification,
) -> Result<(), Error> {
    let params = lsp_types::DidOpenTextDocumentParams::deserialize(note.params)?;
    let text = params.text_document.text;
    let diagnostics = get_diagnostics(state, text, params.text_document.uri.clone())?;
    send_diagnostics(state, diagnostics, params.text_document.uri.clone())
}

pub(crate) fn handle_document_did_change(
    state: &mut ServerState,
    note: lsp_server::Notification,
) -> Result<(), Error> {
    let params = lsp_types::DidChangeTextDocumentParams::deserialize(note.params)?;
    let text = params.content_changes[0].text.clone();
    let diagnostics = get_diagnostics(state, text, params.text_document.uri.clone())?;
    send_diagnostics(state, diagnostics, params.text_document.uri.clone())
}

// pub(crate) fn handle_workspace_did_change_folders(
//     state: &mut ServerState,
//     note: lsp_server::Notification,
// ) -> Result<(), Error> {
//     let params = lsp_types::DidChangeWorkspaceFoldersParams::deserialize(note.params)?;

//     let mut workspace = &mut state.workspace;
//     let mut db = &mut state.db;

//     let workspace_folder = params.event.added[0].uri.to_file_path().unwrap();

//     workspace.sync(&mut db, workspace_folder.as_path().to_str().unwrap().into());

//     Ok(())
// }

fn send_diagnostics(
    state: &mut ServerState,
    diagnostics: Vec<lsp_types::Diagnostic>,
    uri: lsp_types::Url,
) -> Result<(), Error> {
    let result = lsp_types::PublishDiagnosticsParams {
        uri: uri,
        diagnostics: diagnostics,
        version: None,
    };
    let response = lsp_server::Message::Notification(lsp_server::Notification {
        method: String::from("textDocument/publishDiagnostics"),
        params: serde_json::to_value(result).unwrap(),
    });

    let sender = state.sender.lock().unwrap();
    sender.send(response)?;

    Ok(())
}
