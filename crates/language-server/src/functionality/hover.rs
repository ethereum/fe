use common::{InputDb, InputFile};
use hir::lower::map_file_to_mod;
use hir::LowerHirDb;
use hir::{HirDb, SpannedHirDb};

use lsp_types::Hover;
use tracing::info;

use salsa::Snapshot;

use tower_lsp::jsonrpc::Result;

use crate::{backend::db::LanguageServerDatabase, util::to_offset_from_position};

use super::goto::{get_goto_target_scopes_for_cursor, Cursor};
use super::item_info::{get_item_definition_markdown, get_item_docstring, get_item_path_markdown};

pub fn hover_helper(
    db: Snapshot<LanguageServerDatabase>,
    input: InputFile,
    params: lsp_types::HoverParams,
) -> Result<Option<Hover>> {
    info!("handling hover");
    let file_text = input.text(db.as_input_db());

    let cursor: Cursor = to_offset_from_position(
        params.text_document_position_params.position,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(db.as_lower_hir_db(), input);
    let goto_info = &get_goto_target_scopes_for_cursor(db.as_language_server_db(), top_mod, cursor)
        .unwrap_or_default();

    let hir_db = db.as_hir_db();
    let scopes_info = goto_info
        .iter()
        .map(|scope| {
            let item = &scope.item();
            let pretty_path = get_item_path_markdown(item, hir_db);
            let definition_source = get_item_definition_markdown(item, db.as_spanned_hir_db());
            let docs = get_item_docstring(item, hir_db);

            let result = [pretty_path, definition_source, docs]
                .iter()
                .filter_map(|info| info.clone().map(|info| format!("{}\n", info)))
                .collect::<Vec<String>>()
                .join("\n");

            result
        })
        .collect::<Vec<String>>();

    let info = scopes_info.join("\n---\n");

    let result = lsp_types::Hover {
        contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: info,
        }),
        range: None,
    };
    Ok(Some(result))
}
