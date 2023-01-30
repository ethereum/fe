use std::rc::Rc;

use smol_str::SmolStr;

use crate::context::{AnalyzerContext, TempContext};
use crate::db::Analysis;
use crate::errors::TypeError;
use crate::namespace::items::{FunctionSigId, ImplId, TraitId, TypeAliasId};
use crate::namespace::scopes::ItemScope;
use crate::namespace::types::{self, TypeId};
use crate::traversal::types::type_desc;
use crate::AnalyzerDb;

/// Returns all `impl` for the given type from the current ingot as well as
/// dependency ingots
pub fn all_impls(db: &dyn AnalyzerDb, ty: TypeId) -> Rc<[ImplId]> {
    let ty = ty.deref(db);

    let ingot_modules = db
        .root_ingot()
        .all_modules(db)
        .iter()
        .flat_map(|module_id| module_id.all_impls(db).to_vec())
        .collect::<Vec<_>>();
    db.ingot_external_ingots(db.root_ingot())
        .values()
        .flat_map(|ingot| ingot.all_modules(db).to_vec())
        .flat_map(|module_id| module_id.all_impls(db).to_vec())
        .chain(ingot_modules)
        .filter(|val| val.receiver(db) == ty)
        .collect()
}

pub fn impl_for(db: &dyn AnalyzerDb, ty: TypeId, treit: TraitId) -> Option<ImplId> {
    db.all_impls(ty)
        .iter()
        .find(|impl_| impl_.trait_id(db) == treit)
        .cloned()
}

pub fn function_sigs(db: &dyn AnalyzerDb, ty: TypeId, name: SmolStr) -> Rc<[FunctionSigId]> {
    db.all_impls(ty)
        .iter()
        .filter_map(|impl_| impl_.function(db, &name))
        .map(|fun| fun.sig(db))
        .chain(ty.function_sig(db, &name).map_or(vec![], |fun| vec![fun]))
        .collect()
}

pub fn type_alias_type(
    db: &dyn AnalyzerDb,
    alias: TypeAliasId,
) -> Analysis<Result<types::TypeId, TypeError>> {
    let mut scope = ItemScope::new(db, alias.data(db).module);
    let typ = type_desc(&mut scope, &alias.data(db).ast.kind.typ, None);

    Analysis::new(typ, scope.diagnostics.take().into())
}

pub fn type_alias_type_cycle(
    db: &dyn AnalyzerDb,
    _cycle: &[String],
    alias: &TypeAliasId,
) -> Analysis<Result<types::TypeId, TypeError>> {
    let context = TempContext::default();
    let err = Err(TypeError::new(context.error(
        "recursive type definition",
        alias.data(db).ast.span,
        "",
    )));

    Analysis::new(err, context.diagnostics.take().into())
}
