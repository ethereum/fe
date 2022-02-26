use crate::context::{AnalyzerContext, TempContext};
use crate::db::Analysis;
use crate::errors::TypeError;
use crate::namespace::items::TypeAliasId;
use crate::namespace::scopes::ItemScope;
use crate::namespace::types;
use crate::traversal::types::type_desc;
use crate::AnalyzerDb;

pub fn type_alias_type(
    db: &dyn AnalyzerDb,
    alias: TypeAliasId,
) -> Analysis<Result<types::Type, TypeError>> {
    let mut scope = ItemScope::new(db, alias.data(db).module);
    let typ = type_desc(&mut scope, &alias.data(db).ast.kind.typ);

    Analysis::new(typ, scope.diagnostics.take().into())
}

pub fn type_alias_type_cycle(
    db: &dyn AnalyzerDb,
    _cycle: &[String],
    alias: &TypeAliasId,
) -> Analysis<Result<types::Type, TypeError>> {
    let mut context = TempContext::default();
    let err = Err(TypeError::new(context.error(
        "recursive type definition",
        alias.data(db).ast.span,
        "",
    )));

    Analysis::new(err, context.diagnostics.take().into())
}
