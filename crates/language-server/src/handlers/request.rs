use std::io::BufRead;

use hir_analysis::name_resolution::EarlyResolvedPath;
use lsp_server::Response;
use serde::Deserialize;

use crate::{
    goto::{goto_enclosing_path, Cursor},
    state::ServerState,
    util::{to_offset_from_position, to_lsp_location_from_scope},
};

pub(crate) fn handle_hover(
    state: &mut ServerState,
    req: lsp_server::Request,
) -> Result<(), anyhow::Error> {
    // TODO: get more relevant information for the hover
    let params = lsp_types::HoverParams::deserialize(req.params)?;
    let file_path = &params
        .text_document_position_params
        .text_document
        .uri
        .path();
    let file = std::fs::File::open(file_path)?;
    let reader = std::io::BufReader::new(file);
    let line = reader
        .lines()
        .nth(params.text_document_position_params.position.line as usize)
        .unwrap()
        .unwrap();

    let file_text = std::fs::read_to_string(file_path)?;

    // let cursor: Cursor = params.text_document_position_params.position.into();
    let cursor: Cursor = to_offset_from_position(
        params.text_document_position_params.position,
        file_text.as_str(),
    );
    let file_path = std::path::Path::new(file_path);
    let top_mod = state.workspace.top_mod_from_file(&mut state.db, file_path, file_text.as_str());
    let goto_info = goto_enclosing_path(&mut state.db, top_mod, cursor);

    let goto_info = match goto_info {
        Some(EarlyResolvedPath::Full(bucket)) => bucket
            .iter()
            .map(|x| x.pretty_path(&state.db).unwrap())
            .collect::<Vec<_>>()
            .join("\n"),
        Some(EarlyResolvedPath::Partial {
            res,
            unresolved_from: _,
        }) => res.pretty_path(&state.db).unwrap(),
        None => String::from("No goto info available"),
    };

    let result = lsp_types::Hover {
        contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent::from(
            lsp_types::MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: format!(
                    "### Hovering over:\n```{}```\n\n{}\n\n### Goto Info: \n\n{}",
                    &line,
                    serde_json::to_string_pretty(&params).unwrap(),
                    goto_info,
                ),
            },
        )),
        range: None,
    };
    let response_message = Response {
        id: req.id,
        result: Some(serde_json::to_value(result)?),
        error: None,
    };

    state.send_response(response_message)?;
    Ok(())
}

use lsp_types::TextDocumentPositionParams;

pub(crate) fn handle_goto_definition(
    state: &mut ServerState,
    req: lsp_server::Request,
) -> Result<(), anyhow::Error> {
    let params = TextDocumentPositionParams::deserialize(req.params)?;

    // Convert the position to an offset in the file
    let file_text = std::fs::read_to_string(params.text_document.uri.path())?;
    let cursor: Cursor = to_offset_from_position(params.position, file_text.as_str());

    // Get the module and the goto info
    let file_path = std::path::Path::new(params.text_document.uri.path());
    let top_mod = state.workspace.top_mod_from_file(&mut state.db, file_path, file_text.as_str());
    let goto_info = goto_enclosing_path(&mut state.db, top_mod, cursor);

    // Convert the goto info to a Location
    let scopes = match goto_info {
        Some(EarlyResolvedPath::Full(bucket)) => {
            bucket.iter().map(|x| x.scope()).collect::<Vec<_>>()
        }
        Some(EarlyResolvedPath::Partial {
            res,
            unresolved_from: _,
        }) => {
            vec![res.scope()]
        }
        None => return Ok(()),
    };

    let locations = scopes
        .into_iter()
        .filter_map(|scope| scope)
        .map(|scope| to_lsp_location_from_scope(scope, &state.db))
        .filter_map(|location| location.ok())
        .collect::<Vec<_>>();

    // Send the response
    let response_message = Response {
        id: req.id,
        result: Some(serde_json::to_value(
            lsp_types::GotoDefinitionResponse::Array(locations),
        )?),
        error: None,
    };

    state.send_response(response_message)?;
    Ok(())
}