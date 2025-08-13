use anyhow::Error;
use async_lsp::lsp_types::Hover;

use common::file::File;
use hir::lower::map_file_to_mod;
use hir_analysis::ty::ty_check::check_func_body;
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
        LazyHirResult::ItemPath(item, path, _ctx, seg) => {
            // Resolve the item-level path to a scope and show a concise label
            let scope = hir::hir_def::scope_graph::ScopeId::from_item(item);
            let res = if let Some(i) = seg {
                hir_analysis::name_resolution::resolve_path_segment(db, path, i, scope, true)
                    .ok()
                    .and_then(|r| r.as_scope(db))
            } else {
                hir_analysis::name_resolution::resolve_path(db, path, scope, true)
                    .ok()
                    .and_then(|r| r.as_scope(db))
            };

            if let Some(s) = res {
                let label = s.pretty_path(db).unwrap_or_default();
                let contents = format!("```fe\n{}\n```", label);
                Ok(Some(Hover {
                    contents: async_lsp::lsp_types::HoverContents::Markup(
                        async_lsp::lsp_types::MarkupContent {
                            kind: async_lsp::lsp_types::MarkupKind::Markdown,
                            value: contents,
                        },
                    ),
                    range: None,
                }))
            } else {
                Ok(None)
            }
        }
        LazyHirResult::ItemType(_item, _ty, _ctx) => Ok(None),
        LazyHirResult::ItemGenericParam(item, idx) => {
            let scope = hir::hir_def::scope_graph::ScopeId::GenericParam(item, idx);
            let label = scope.pretty_path(db).unwrap_or_else(|| format!("generic param #{}", idx));
            let contents = format!("```fe\n{}\n```", label);
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
        LazyHirResult::None => Ok(None),
    }
}

/// Create hover information for an expression HIR node
fn create_expression_hover(
    db: &DriverDataBase,
    body: hir::hir_def::Body,
    expr_id: hir::hir_def::ExprId,
) -> Result<Option<Hover>, Error> {
    // Try to get type information by finding the containing function
    let type_info = if let Some(func) = find_containing_func(db, body) {
        // Get the typed body from type checking
        let (_, typed_body) = check_func_body(db, func);
        
        // Get the type of this expression
        let ty = typed_body.expr_ty(db, expr_id);
        
        // Use pretty_print to get a human-readable type
        ty.pretty_print(db).to_string()
    } else {
        // Fallback for non-function bodies
        "<type information unavailable>".to_string()
    };

    // Build the hover content
    let contents = format!("```fe\n{}\n```", type_info);

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
    use hir::hir_def::{Partial, Stmt};
    
    let stmt_data = &body.stmts(db)[stmt_id];
    
    // Extract type information based on statement kind
    let type_info = match stmt_data {
        Partial::Present(Stmt::Let(pat_id, _type_annotation, _init_expr)) => {
            if let Some(func) = find_containing_func(db, body) {
                // Get the typed body from type checking
                let (_, typed_body) = check_func_body(db, func);
                
                // Get the type of the pattern (variable binding)
                let ty = typed_body.pat_ty(db, *pat_id);
                
                // Use pretty_print to get a human-readable type
                format!("let: {}", ty.pretty_print(db))
            } else {
                "let: <type information unavailable>".to_string()
            }
        }
        _ => "<statement>".to_string(),
    };

    let contents = format!("```fe\n{}\n```", type_info);

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
    // Try to get type information from the parent function
    let type_info = if let Some(func) = find_containing_func(db, body) {
        // Get the typed body from type checking
        let (_, typed_body) = check_func_body(db, func);
        
        // Get the type of this pattern
        let ty = typed_body.pat_ty(db, pat_id);
        
        // Use pretty_print to get a human-readable type
        ty.pretty_print(db).to_string()
    } else {
        "<type information unavailable>".to_string()
    };

    let contents = format!("```fe\n{}\n```", type_info);

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

/// Find the function that contains a given body
fn find_containing_func<'db>(
    db: &'db DriverDataBase,
    body: hir::hir_def::Body<'db>,
) -> Option<hir::hir_def::Func<'db>> {
    use hir::hir_def::{ItemKind, BodyKind};
    
    // Check if this is a function body
    if body.body_kind(db) != BodyKind::FuncBody {
        return None;
    }
    
    // Get all items in the module and find the function with this body
    let top_mod = body.top_mod(db);
    let items = top_mod.scope_graph(db).items_dfs(db);
    
    for item in items {
        if let ItemKind::Func(func) = item {
            if func.body(db) == Some(body) {
                return Some(func);
            }
        }
    }
    
    None
}
