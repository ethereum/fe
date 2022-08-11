use std::rc::Rc;

use smol_str::SmolStr;

use crate::context::{AnalyzerContext, TempContext};
use crate::db::Analysis;
use crate::errors::TypeError;
use crate::namespace::items::{FunctionSigId, ImplId, TraitId, TypeAliasId};
use crate::namespace::scopes::ItemScope;
use crate::namespace::types::{self, Array, Base, FeString, Tuple, Type, TypeId};
use crate::traversal::types::type_desc;
use crate::AnalyzerDb;

/// Returns all `impl` for the given type from the current ingot as well as dependency ingots
pub fn all_impls(db: &dyn AnalyzerDb, ty: TypeId) -> Rc<[ImplId]> {
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
    ty.get_all_impls(db)
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
    let typ = type_desc(&mut scope, &alias.data(db).ast.kind.typ);

    Analysis::new(typ, scope.diagnostics.take().into())
}

pub fn type_alias_type_cycle(
    db: &dyn AnalyzerDb,
    _cycle: &[String],
    alias: &TypeAliasId,
) -> Analysis<Result<types::TypeId, TypeError>> {
    let mut context = TempContext::default();
    let err = Err(TypeError::new(context.error(
        "recursive type definition",
        alias.data(db).ast.span,
        "",
    )));

    Analysis::new(err, context.diagnostics.take().into())
}

pub fn is_zero_sized(db: &dyn AnalyzerDb, ty: types::TypeId) -> bool {
    let typ = db.lookup_intern_type(ty);
    match typ {
        Type::Base(base) => matches!(base, Base::Unit),
        Type::Array(Array { size, inner }) => size == 0 || inner.is_zero_sized(db),
        Type::Tuple(Tuple { items }) => {
            for item in items.iter() {
                if !db.is_zero_sized(*item) {
                    return false;
                }
            }
            true
        }
        Type::String(FeString { max_size }) => max_size == 0,
        Type::Struct(struct_id) => {
            for field_type_id in db.struct_all_fields(struct_id).iter() {
                // assume all type is correct.
                let field_type = db.struct_field_type(*field_type_id).value.unwrap();
                if !(field_type).is_zero_sized(db) {
                    return false;
                }
            }
            true
        }
        Type::Contract(contract_id) => {
            for field_type_id in db.contract_all_fields(contract_id).iter() {
                let field_type = db.contract_field_type(*field_type_id).value.unwrap();
                if !(field_type).is_zero_sized(db) {
                    return false;
                }
            }
            true
        }
        Type::Generic(_) | Type::Map(_) | Type::SelfContract(_) => false,
    }
}

// because we already catch this error so this should be empty
pub fn is_zero_sized_cycle(_db: &dyn AnalyzerDb, _cycle: &[String], _ty: &types::TypeId) -> bool {
    true
}
