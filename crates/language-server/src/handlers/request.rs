use std::io::BufRead;

use common::input::IngotKind;
use hir_analysis::name_resolution::{EarlyResolvedPath, NameRes};
use log::info;
use lsp_server::{Response, ResponseError};
use serde::Deserialize;

use crate::{
    backend::Backend, db::LanguageServerDatabase, goto::{goto_enclosing_path, Cursor}, util::{to_lsp_location_from_scope, to_offset_from_position}, workspace::{IngotFileContext, Workspace}
};

pub fn handle_hover(
    db: &mut LanguageServerDatabase,
    workspace: &mut Workspace,
    req: lsp_server::Request) -> Result<(), anyhow::Error> {
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
    // let file_path = std::path::Path::new(file_path);
    info!("getting hover info for file_path: {:?}", file_path);
    let ingot = workspace
        .input_from_file_path(db, file_path)
        .map(|input| input.ingot(db));

    // info!("got ingot: {:?} of type {:?}", ingot, ingot.map(|ingot| ingot.kind(&mut state.db)));

    let ingot_info: Option<String> = {
        let ingot_type = match ingot {
            Some(ingot) => match ingot.kind(db) {
                IngotKind::StandAlone => None,
                IngotKind::Local => Some("Local ingot"),
                IngotKind::External => Some("External ingot"),
                IngotKind::Std => Some("Standard library"),
            },
            None => Some("No ingot information available"),
        };
        let ingot_file_count = ingot.unwrap().files(db).len();
        let ingot_path = ingot
            .unwrap()
            .path(db)
            .strip_prefix(workspace.root_path.clone().unwrap_or("".into()))
            .ok();

        ingot_type.map(|ingot_type| {
            format!("{ingot_type} with {ingot_file_count} files at path: {ingot_path:?}")
        })
    };

    let top_mod = workspace.top_mod_from_file_path(db, file_path).unwrap();
    let early_resolution = goto_enclosing_path(db, top_mod, cursor);

    let goto_info = match early_resolution {
        Some(EarlyResolvedPath::Full(bucket)) => bucket
            .iter()
            .map(|x| x.pretty_path(db).unwrap())
            .collect::<Vec<_>>()
            .join("\n"),
        Some(EarlyResolvedPath::Partial {
            res,
            unresolved_from: _,
        }) => res.pretty_path(db).unwrap(),
        None => String::from("No goto info available"),
    };

    let result = lsp_types::Hover {
        contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: format!(
                    "### Hovering over:\n```{}```\n\n{}\n\n### Goto Info: \n\n{}\n\n### Ingot info: \n\n{:?}",
                    &line,
                    serde_json::to_string_pretty(&params).unwrap(),
                    goto_info,
                    ingot_info,
                ),
            }),
        range: None,
    };
    let _response_message = Response {
        id: req.id,
        result: Some(serde_json::to_value(result)?),
        error: None,
    };

    // state.send_response(response_message)?;
    Ok(())
}

use lsp_types::TextDocumentPositionParams;

pub fn handle_goto_definition(
    db: &mut LanguageServerDatabase,
    workspace: &mut Workspace,
    req: lsp_server::Request,
) -> Result<(), anyhow::Error> {
    info!("handling goto definition request: {:?}", req);
    let params = TextDocumentPositionParams::deserialize(req.params)?;

    // Convert the position to an offset in the file
    let file_text = std::fs::read_to_string(params.text_document.uri.path())?;
    let cursor: Cursor = to_offset_from_position(params.position, file_text.as_str());

    // Get the module and the goto info
    let file_path = params.text_document.uri.path();
    let top_mod = workspace.top_mod_from_file_path(db, file_path).unwrap();
    let goto_info = goto_enclosing_path(db, top_mod, cursor);

    // Convert the goto info to a Location
    let scopes = match goto_info {
        Some(EarlyResolvedPath::Full(bucket)) => {
            bucket.iter().map(NameRes::scope).collect::<Vec<_>>()
        }
        Some(EarlyResolvedPath::Partial {
            res,
            unresolved_from: _,
        }) => {
            vec![res.scope()]
        }
        None => return Ok(()),
    };

    // info!("scopes: {:?}", scopes);

    let locations = scopes
        .iter()
        .filter_map(|scope| *scope)
        .map(|scope| to_lsp_location_from_scope(scope, db))
        .collect::<Vec<_>>();

    let errors = scopes
        .iter()
        .filter_map(|scope| *scope)
        .map(|scope| to_lsp_location_from_scope(scope, db))
        .filter_map(std::result::Result::err)
        .map(|err| err.to_string())
        .collect::<Vec<_>>()
        .join("\n");

    let error = (!errors.is_empty()).then_some(ResponseError {
        code: lsp_types::error_codes::SERVER_CANCELLED as i32,
        message: errors,
        data: None,
    });

    // Send the response
    let response_message = Response {
        id: req.id,
        result: Some(serde_json::to_value(
            lsp_types::GotoDefinitionResponse::Array(
                locations
                    .into_iter()
                    .filter_map(std::result::Result::ok)
                    .collect(),
            ),
        )?),
        error,
    };

    info!("goto definition response: {:?}", response_message);

    // state.send_response(response_message)?;
    Ok(())
}
