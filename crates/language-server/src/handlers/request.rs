use std::io::BufRead;

use lsp_server::Response;
use serde::Deserialize;

use crate::state::ServerState;

pub(crate) fn handle_hover(
    state: &mut ServerState,
    req: lsp_server::Request,
) -> Result<(), anyhow::Error> {
    // TODO: get more relevant information for the hover
    let params = lsp_types::HoverParams::deserialize(req.params)?;
    let file = std::fs::File::open(
        &params
            .text_document_position_params
            .text_document
            .uri
            .path(),
    )?;
    let reader = std::io::BufReader::new(file);
    let line = reader
        .lines()
        .nth(params.text_document_position_params.position.line as usize)
        .unwrap()
        .unwrap();
    let result = lsp_types::Hover {
        contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent::from(
            lsp_types::MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: format!(
                    "### Hovering over:\n```{}```\n\n{}",
                    &line,
                    serde_json::to_string_pretty(&params).unwrap()
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
