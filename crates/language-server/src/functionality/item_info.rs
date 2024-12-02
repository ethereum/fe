use hir::{
    hir_def::{Attr, ItemKind},
    span::LazySpan,
    HirDb, SpannedHirDb,
};

pub fn get_item_docstring(item: ItemKind, hir_db: &dyn HirDb) -> Option<String> {
    let docstring = match item {
        ItemKind::Func(func) => func.attributes(hir_db).data(hir_db),
        ItemKind::Mod(mod_) => mod_.attributes(hir_db).data(hir_db),
        ItemKind::Struct(struct_) => struct_.attributes(hir_db).data(hir_db),
        ItemKind::Enum(enum_) => enum_.attributes(hir_db).data(hir_db),
        ItemKind::TypeAlias(type_alias) => type_alias.attributes(hir_db).data(hir_db),
        ItemKind::Trait(trait_) => trait_.attributes(hir_db).data(hir_db),
        ItemKind::Impl(impl_) => impl_.attributes(hir_db).data(hir_db),
        // ItemKind::Body(body) => body.attributes(hir_db).data(hir_db).clone(),
        // ItemKind::Const(const_) => const_.attributes(hir_db).data(hir_db).clone(),
        // ItemKind::Use(use_) => use_.attributes(hir_db).data(hir_db).clone(),
        ItemKind::Contract(contract) => contract.attributes(hir_db).data(hir_db),
        _ => return None,
    }
    .iter()
    .filter_map(|attr| {
        if let Attr::DocComment(doc) = attr {
            Some(doc.text.data(hir_db).clone())
        } else {
            None
        }
    })
    .collect::<Vec<_>>();

    if docstring.is_empty() {
        None
    } else {
        Some(docstring.join("\n"))
    }
}

pub fn get_item_path_markdown(item: ItemKind, hir_db: &dyn HirDb) -> Option<String> {
    item.scope()
        .pretty_path(hir_db)
        .map(|path| format!("```fe\n{}\n```", path))
}

pub fn get_item_definition_markdown(item: ItemKind, db: &dyn SpannedHirDb) -> Option<String> {
    // TODO: use pending AST features to get the definition without all this text manipulation
    let hir_db = db.as_hir_db();
    let span = item.lazy_span().resolve(db)?;

    let mut start: usize = span.range.start().into();
    let mut end: usize = span.range.end().into();

    // if the item has a body or children, cut that stuff out
    let body_start = match item {
        ItemKind::Func(func) => Some(func.body(hir_db)?.lazy_span().resolve(db)?.range.start()),
        ItemKind::Mod(module) => Some(module.scope().name_span(hir_db)?.resolve(db)?.range.end()),
        // TODO: handle other item types
        _ => None,
    };
    if let Some(body_start) = body_start {
        end = body_start.into();
    }

    // let's start at the beginning of the line where the name is defined
    let name_span = item.name_span()?.resolve(db);
    if let Some(name_span) = name_span {
        let mut name_line_start = name_span.range.start().into();
        let file_text = span.file.text(db.as_input_db()).as_str();
        while name_line_start > 0 && file_text.chars().nth(name_line_start - 1).unwrap() != '\n' {
            name_line_start -= 1;
        }
        start = name_line_start;
    }

    let item_definition = span.file.text(db.as_input_db()).as_str()[start..end].to_string();
    Some(format!("```fe\n{}\n```", item_definition.trim()))
}
