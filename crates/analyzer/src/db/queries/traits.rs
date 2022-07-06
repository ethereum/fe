use indexmap::map::Entry;
use indexmap::IndexMap;
use smol_str::SmolStr;

use crate::context::{Analysis, AnalyzerContext};
use crate::namespace::items::{FunctionSig, FunctionSigId, Item, TraitId};
use crate::namespace::scopes::ItemScope;
use crate::namespace::types::TypeId;
use crate::AnalyzerDb;
use std::rc::Rc;

pub fn trait_all_functions(db: &dyn AnalyzerDb, trait_: TraitId) -> Rc<[FunctionSigId]> {
    let trait_data = trait_.data(db);
    trait_data
        .ast
        .kind
        .functions
        .iter()
        .map(|node| {
            db.intern_function_sig(Rc::new(FunctionSig {
                ast: node.clone(),
                module: trait_.module(db),
                parent: Some(Item::Trait(trait_)),
            }))
        })
        .collect()
}

pub fn trait_function_map(
    db: &dyn AnalyzerDb,
    trait_: TraitId,
) -> Analysis<Rc<IndexMap<SmolStr, FunctionSigId>>> {
    let scope = ItemScope::new(db, trait_.module(db));
    let mut map = IndexMap::<SmolStr, FunctionSigId>::new();

    for func in db.trait_all_functions(trait_).iter() {
        let def_name = func.name(db);

        match map.entry(def_name) {
            Entry::Occupied(entry) => {
                scope.duplicate_name_error(
                    &format!("duplicate function names in `trait {}`", trait_.name(db)),
                    entry.key(),
                    entry.get().name_span(db),
                    func.name_span(db),
                );
            }
            Entry::Vacant(entry) => {
                entry.insert(*func);
            }
        }
    }
    Analysis::new(Rc::new(map), scope.diagnostics.take().into())
}

pub fn trait_is_implemented_for(db: &dyn AnalyzerDb, trait_: TraitId, ty: TypeId) -> bool {
    trait_
        .module(db)
        .all_impls(db)
        .iter()
        .any(|val| val.trait_id(db) == trait_ && val.receiver(db) == ty)
}
