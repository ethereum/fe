use std::sync::Arc;

use common::{input::IngotKind, InputDb};
use hir::LowerHirDb;
use hir_analysis::{name_resolution::EarlyResolvedPath, HirAnalysisDb};
use lsp_types::Hover;
use tracing::info;

use salsa::Snapshot;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;

use crate::{
    backend::db::LanguageServerDatabase,
    backend::workspace::{IngotFileContext, Workspace},
    util::to_offset_from_position,
};

use super::goto::{goto_enclosing_path, Cursor};

pub async fn hover_helper(
    db: Snapshot<LanguageServerDatabase>,
    workspace: Arc<RwLock<Workspace>>,
    params: lsp_types::HoverParams,
) -> Result<Option<Hover>> {
    let workspace = workspace.read().await;
    info!("handling hover");
    let file_path = &params
        .text_document_position_params
        .text_document
        .uri
        .path();
    info!("getting hover info for file_path: {:?}", file_path);
    let input = workspace.get_input_for_file_path(file_path);
    let ingot = input.map(|input| input.ingot(db.as_input_db()));

    let file_text = input.unwrap().text(db.as_input_db());
    let line = file_text
        .lines()
        .nth(params.text_document_position_params.position.line as usize)
        .unwrap();

    let cursor: Cursor = to_offset_from_position(
        params.text_document_position_params.position,
        file_text.as_str(),
    );

    let ingot_info: Option<String> = {
        let ingot_type = match ingot {
            Some(ingot) => match ingot.kind(db.as_input_db()) {
                IngotKind::StandAlone => None,
                IngotKind::Local => Some("Local ingot"),
                IngotKind::External => Some("External ingot"),
                IngotKind::Std => Some("Standard library"),
            },
            None => Some("No ingot information available"),
        };
        let ingot_file_count = ingot.unwrap().files(db.as_input_db()).len();
        let ingot_path = ingot
            .unwrap()
            .path(db.as_input_db())
            .strip_prefix(workspace.root_path.clone().unwrap_or("".into()))
            .ok();

        ingot_type.map(|ingot_type| {
            format!("{ingot_type} with {ingot_file_count} files at path: {ingot_path:?}")
        })
    };

    let top_mod = workspace
        .top_mod_from_file_path(db.as_lower_hir_db(), file_path)
        .unwrap();
    let early_resolution = goto_enclosing_path(&db, top_mod, cursor);

    let goto_info = match early_resolution {
        Some(EarlyResolvedPath::Full(bucket)) => bucket
            .iter()
            .map(|x| x.pretty_path(db.as_hir_analysis_db()).unwrap())
            .collect::<Vec<_>>()
            .join("\n"),
        Some(EarlyResolvedPath::Partial {
            res,
            unresolved_from: _,
        }) => res.pretty_path(db.as_hir_analysis_db()).unwrap(),
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
    Ok(Some(result))
}
