use indexmap::map::Entry;
use indexmap::IndexMap;
use smol_str::SmolStr;

use crate::context::{Analysis, AnalyzerContext};
use crate::namespace::items::{Function, FunctionId, ImplId, Item};
use crate::namespace::scopes::ItemScope;
use crate::AnalyzerDb;
use std::rc::Rc;

pub fn impl_all_functions(db: &dyn AnalyzerDb, impl_: ImplId) -> Rc<[FunctionId]> {
    let impl_data = impl_.data(db);
    impl_data
        .ast
        .kind
        .functions
        .iter()
        .map(|node| {
            db.intern_function(Rc::new(Function::new(
                db,
                node,
                Some(Item::Impl(impl_)),
                impl_data.module,
            )))
        })
        .collect()
}

pub fn impl_function_map(
    db: &dyn AnalyzerDb,
    impl_: ImplId,
) -> Analysis<Rc<IndexMap<SmolStr, FunctionId>>> {
    let scope = ItemScope::new(db, impl_.module(db));
    let mut map = IndexMap::<SmolStr, FunctionId>::new();

    for func in db.impl_all_functions(impl_).iter() {
        let def_name = func.name(db);

        match map.entry(def_name) {
            Entry::Occupied(entry) => {
                scope.duplicate_name_error(
                    "duplicate function names in `impl` block",
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
