use crate::namespace::items::TraitId;
use crate::namespace::types;
use crate::AnalyzerDb;
use std::rc::Rc;

pub fn trait_type(db: &dyn AnalyzerDb, trait_: TraitId) -> Rc<types::Trait> {
    Rc::new(types::Trait {
        name: trait_.name(db),
        id: trait_,
    })
}
