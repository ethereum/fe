use hir::{
    hir_def::{scope_graph::ScopeId, ItemKind},
    span::LazySpan,
    HirDb, SpannedHirDb,
};

pub fn get_docstring(db: &dyn HirDb, scope: ScopeId) -> Option<String> {
    scope
        .attrs(db)?
        .data(db)
        .iter()
        .filter_map(|attr| match attr {
            hir::hir_def::Attr::DocComment(doc) => Some(doc.text.data(db).clone()),
            _ => None,
        })
        .reduce(|a, b| a + "\n" + &b)
}

pub fn get_item_path_markdown(db: &dyn HirDb, item: ItemKind) -> Option<String> {
    item.scope()
        .pretty_path(db)
        .map(|path| format!("```fe\n{path}\n```"))
}

pub fn get_item_definition_markdown(db: &dyn SpannedHirDb, item: ItemKind) -> Option<String> {
    let span = item.span().resolve(db)?;

    let mut start: usize = span.range.start().into();
    let mut end: usize = span.range.end().into();

    let body_start = match item {
        ItemKind::Func(func) => Some(func.body(db)?.span().resolve(db)?.range.start()),
        ItemKind::Mod(module) => Some(module.scope().name_span(db)?.resolve(db)?.range.end()),
        _ => None,
    };
    if let Some(body_start) = body_start {
        end = body_start.into();
    }

    if let Some(name_span) = item.name_span()?.resolve(db) {
        let mut name_line_start: usize = name_span.range.start().into();
        let file_text = span.file.text(db).as_str();
        while name_line_start > 0
            && file_text.chars().nth(name_line_start - 1).unwrap_or('\n') != '\n'
        {
            name_line_start -= 1;
        }
        start = name_line_start;
    }

    let item_definition = span.file.text(db).as_str()[start..end].to_string();
    Some(format!("```fe\n{}\n```", item_definition.trim()))
}

pub fn hover_markdown_for_scope(
    db_hir: &dyn HirDb,
    db_span: &dyn SpannedHirDb,
    scope: ScopeId,
) -> Option<String> {
    // Prefer a definition snippet when scope is an item
    if let Some(item) = scope.to_item() {
        let header = get_item_definition_markdown(db_span, item);
        let mut lines = Vec::new();
        if let Some(h) = header {
            lines.push(h);
        }
        if let Some(doc) = get_docstring(db_hir, scope) {
            lines.push(doc);
        }
        if lines.is_empty() {
            get_item_path_markdown(db_hir, item)
        } else {
            Some(lines.join("\n\n"))
        }
    } else {
        // Fallback: show the pretty path for non-item scopes
        scope
            .pretty_path(db_hir)
            .map(|p| format!("```fe\n{}\n```", p))
    }
}
