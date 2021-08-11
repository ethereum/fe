use crate::db::Analysis;
use crate::errors::{self, TypeError};
use crate::namespace::items::TypeAliasId;
use crate::namespace::scopes::ItemScope;
use crate::namespace::types;
use crate::traversal::types::type_desc;
use crate::AnalyzerDb;
use std::rc::Rc;

pub fn type_alias_type(
    db: &dyn AnalyzerDb,
    alias: TypeAliasId,
) -> Analysis<Result<types::Type, TypeError>> {
    let mut scope = ItemScope::new(db, alias.data(db).module);
    let typ = type_desc(&mut scope, &alias.data(db).ast.kind.typ);

    Analysis {
        value: typ,
        diagnostics: Rc::new(scope.diagnostics),
    }
}

pub fn type_alias_type_cycle(
    db: &dyn AnalyzerDb,
    _cycle: &[String],
    alias: &TypeAliasId,
) -> Analysis<Result<types::Type, TypeError>> {
    Analysis {
        value: Err(TypeError),
        diagnostics: Rc::new(vec![errors::error(
            "recursive type definition",
            alias.data(db).ast.span,
            "",
        )]),
    }
}
