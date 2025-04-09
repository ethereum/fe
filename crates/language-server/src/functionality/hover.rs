use anyhow::Error;
use async_lsp::lsp_types::Hover;
use common::{InputFile, InputIngot};
use hir::lower::map_file_to_mod;
use tracing::info;

use super::{
    goto::{get_goto_target_scopes_for_cursor, Cursor},
    item_info::{get_docstring, get_item_definition_markdown, get_item_path_markdown},
};
use crate::{backend::db::LanguageServerDb, util::to_offset_from_position};

pub fn hover_helper(
    db: &dyn LanguageServerDb,
    ingot: InputIngot,
    file: InputFile,
    params: async_lsp::lsp_types::HoverParams,
) -> Result<Option<Hover>, Error> {
    info!("handling hover");
    let file_text = file.text(db);

    let cursor: Cursor = to_offset_from_position(
        params.text_document_position_params.position,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(db, ingot, file);
    let goto_info = &get_goto_target_scopes_for_cursor(db, top_mod, cursor).unwrap_or_default();

    let scopes_info = goto_info
        .iter()
        .map(|scope| {
            let item = scope.item();
            let pretty_path = get_item_path_markdown(item, db);
            let definition_source = get_item_definition_markdown(item, db);
            let docs = get_docstring(*scope, db);

            let result = [pretty_path, definition_source, docs]
                .iter()
                .filter_map(|info| info.clone().map(|info| format!("{}\n", info)))
                .collect::<Vec<String>>()
                .join("\n");

            result
        })
        .collect::<Vec<String>>();

    let info = scopes_info.join("\n---\n");

    let result = async_lsp::lsp_types::Hover {
        contents: async_lsp::lsp_types::HoverContents::Markup(
            async_lsp::lsp_types::MarkupContent {
                kind: async_lsp::lsp_types::MarkupKind::Markdown,
                value: info,
            },
        ),
        range: None,
    };
    Ok(Some(result))
}
