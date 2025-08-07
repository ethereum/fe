use anyhow::Error;
use async_lsp::lsp_types::Hover;

use common::file::File;
use hir::{lower::map_file_to_mod, span::LazySpan};
use tracing::info;

use super::goto::Cursor;
use crate::{
    hir_integration::{lazy_hir_for_cursor, LazyHirResult},
    util::to_offset_from_position,
};
use driver::DriverDataBase;

/// Hover using HIR synthesis architecture
pub fn hover_helper(
    db: &DriverDataBase,
    file: File,
    params: async_lsp::lsp_types::HoverParams,
) -> Result<Option<Hover>, Error> {
    info!("handling hover with HIR synthesis");
    let file_text = file.text(db);

    let cursor: Cursor = to_offset_from_position(
        params.text_document_position_params.position,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(db, file);

    // Use HIR synthesis approach for precise and fast hover information
    let hir_result = lazy_hir_for_cursor(db, top_mod, cursor);

    match hir_result {
        LazyHirResult::Expr(body, expr_id, _) => create_expression_hover(db, body, expr_id),
        LazyHirResult::Stmt(body, stmt_id, _) => create_statement_hover(db, body, stmt_id),
        LazyHirResult::Pat(body, pat_id) => create_pattern_hover(db, body, pat_id),
        LazyHirResult::None => {
            // No HIR node found at this position
            Ok(None)
        }
    }
}

/// Create hover information for an expression HIR node
fn create_expression_hover(
    db: &DriverDataBase,
    body: hir::hir_def::Body,
    expr_id: hir::hir_def::ExprId,
) -> Result<Option<Hover>, Error> {
    let expr_data = &body.exprs(db)[expr_id];

    // Get type information if available
    let type_info = format!("Expression: {:?}", expr_data);

    // Get span information
    let location_info = if let Some(span) = expr_id.span(body).resolve(db) {
        format!("Location: {:?}:{:?}", span.range.start(), span.range.end())
    } else {
        "Location: unknown".to_string()
    };

    let contents = format!("```fe\n{}\n```\n\n{}", type_info, location_info);

    Ok(Some(Hover {
        contents: async_lsp::lsp_types::HoverContents::Markup(
            async_lsp::lsp_types::MarkupContent {
                kind: async_lsp::lsp_types::MarkupKind::Markdown,
                value: contents,
            },
        ),
        range: None,
    }))
}

/// Create hover information for a statement HIR node
fn create_statement_hover(
    db: &DriverDataBase,
    body: hir::hir_def::Body,
    stmt_id: hir::hir_def::StmtId,
) -> Result<Option<Hover>, Error> {
    let stmt_data = &body.stmts(db)[stmt_id];

    let type_info = format!("Statement: {:?}", stmt_data);
    let location_info = if let Some(span) = stmt_id.span(body).resolve(db) {
        format!("Location: {:?}:{:?}", span.range.start(), span.range.end())
    } else {
        "Location: unknown".to_string()
    };

    let contents = format!("```fe\n{}\n```\n\n{}", type_info, location_info);

    Ok(Some(Hover {
        contents: async_lsp::lsp_types::HoverContents::Markup(
            async_lsp::lsp_types::MarkupContent {
                kind: async_lsp::lsp_types::MarkupKind::Markdown,
                value: contents,
            },
        ),
        range: None,
    }))
}

/// Create hover information for a pattern HIR node
fn create_pattern_hover(
    db: &DriverDataBase,
    body: hir::hir_def::Body,
    pat_id: hir::hir_def::PatId,
) -> Result<Option<Hover>, Error> {
    let pat_data = &body.pats(db)[pat_id];

    let type_info = format!("Pattern: {:?}", pat_data);
    let location_info = if let Some(span) = pat_id.span(body).resolve(db) {
        format!("Location: {:?}:{:?}", span.range.start(), span.range.end())
    } else {
        "Location: unknown".to_string()
    };

    let contents = format!("```fe\n{}\n```\n\n{}", type_info, location_info);

    Ok(Some(Hover {
        contents: async_lsp::lsp_types::HoverContents::Markup(
            async_lsp::lsp_types::MarkupContent {
                kind: async_lsp::lsp_types::MarkupKind::Markdown,
                value: contents,
            },
        ),
        range: None,
    }))
}
