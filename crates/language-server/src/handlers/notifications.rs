use anyhow::{Error, Result};
use fxhash::FxHashMap;
// use log::info;
use serde::Deserialize;

use crate::{
    state::ServerState,
    util::diag_to_lsp,
    workspace::{IngotFileContext, SyncableIngotFileContext, SyncableInputFile},
};

fn run_diagnostics(
    state: &mut ServerState,
    path: &str,
) -> Vec<common::diagnostics::CompleteDiagnostic> {
    let db = &mut state.db;
    let workspace = &mut state.workspace;
    let file_path = path;
    let top_mod = workspace.top_mod_from_file_path(db, file_path).unwrap();
    db.analyze_top_mod(top_mod);
    db.finalize_diags()
}

pub fn get_diagnostics(
    state: &mut ServerState,
    uri: lsp_types::Url,
) -> Result<FxHashMap<lsp_types::Url, Vec<lsp_types::Diagnostic>>, Error> {
    let diags = run_diagnostics(state, uri.to_file_path().unwrap().to_str().unwrap());

    let diagnostics = diags
        .into_iter()
        .flat_map(|diag| diag_to_lsp(diag, &state.db).clone());

    // we need to reduce the diagnostics to a map from URL to Vec<Diagnostic>
    let mut result = FxHashMap::<lsp_types::Url, Vec<lsp_types::Diagnostic>>::default();

    // add a null diagnostic to the result for the given URL
    let _ = result.entry(uri.clone()).or_insert_with(Vec::new);

    diagnostics.for_each(|(uri, more_diags)| {
        let diags = result.entry(uri).or_insert_with(Vec::new);
        diags.extend(more_diags);
    });

    Ok(result)
}

pub fn handle_document_did_open(
    state: &mut ServerState,
    note: lsp_server::Notification,
) -> Result<(), Error> {
    let params = lsp_types::DidOpenTextDocumentParams::deserialize(note.params)?;
    let input = state
        .workspace
        .input_from_file_path(
            &mut state.db,
            params
                .text_document
                .uri
                .to_file_path()
                .unwrap()
                .to_str()
                .unwrap(),
        )
        .unwrap();
    let _ = input.sync(&mut state.db, None);
    let diagnostics = get_diagnostics(state, params.text_document.uri.clone())?;
    send_diagnostics(state, diagnostics)
}

pub fn handle_document_did_change(
    state: &mut ServerState,
    note: lsp_server::Notification,
) -> Result<(), Error> {
    let params = lsp_types::DidChangeTextDocumentParams::deserialize(note.params)?;
    let input = state
        .workspace
        .input_from_file_path(
            &mut state.db,
            params
                .text_document
                .uri
                .to_file_path()
                .unwrap()
                .to_str()
                .unwrap(),
        )
        .unwrap();
    let _ = input.sync(&mut state.db, Some(params.content_changes[0].text.clone()));
    let diagnostics = get_diagnostics(state, params.text_document.uri.clone())?;
    // info!("sending diagnostics... {:?}", diagnostics);
    send_diagnostics(state, diagnostics)
}

fn send_diagnostics(
    state: &mut ServerState,
    diagnostics: FxHashMap<lsp_types::Url, Vec<lsp_types::Diagnostic>>,
) -> Result<(), Error> {
    let results = diagnostics.into_iter().map(|(uri, diags)| {
        let result = lsp_types::PublishDiagnosticsParams {
            uri,
            diagnostics: diags,
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

pub fn handle_watched_file_changes(
    state: &mut ServerState,
    note: lsp_server::Notification,
) -> Result<(), Error> {
    let params = lsp_types::DidChangeWatchedFilesParams::deserialize(note.params)?;
    let changes = params.changes;
    let mut diagnostics = FxHashMap::<lsp_types::Url, Vec<lsp_types::Diagnostic>>::default();
    for change in changes {
        let uri = change.uri;
        let path = uri.to_file_path().unwrap();

        match change.typ {
            lsp_types::FileChangeType::CREATED => {
                let input = state
                    .workspace
                    .input_from_file_path(&mut state.db, path.to_str().unwrap())
                    .unwrap();
                let _ = input.sync(&mut state.db, None);
            }
            lsp_types::FileChangeType::CHANGED => {
                let input = state
                    .workspace
                    .input_from_file_path(&mut state.db, path.to_str().unwrap())
                    .unwrap();
                let _ = input.sync(&mut state.db, None);
            }
            // TODO: handle this more carefully!
            lsp_types::FileChangeType::DELETED => {
                // let input = state.workspace.input_from_file_path(&mut state.db, path.to_str().unwrap()).unwrap();
                // let _ = input.sync(&mut state.db, None);
                // this is inefficient, a hack for now
                let _ = state.workspace.sync(&mut state.db);
            }
            _ => {}
        }
        // collect diagnostics for the file
        let diags = get_diagnostics(state, uri.clone())?;
        for (uri, more_diags) in diags {
            let diags = diagnostics.entry(uri).or_insert_with(Vec::new);
            diags.extend(more_diags);
        }
    }
    // info!("sending diagnostics... {:?}", diagnostics);
    send_diagnostics(state, diagnostics)
    // Ok(())
}
