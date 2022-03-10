use crate::db::CodegenDb;
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    rc::Rc,
};

pub fn string_symbol_name(_db: &dyn CodegenDb, data: String) -> Rc<String> {
    let mut hasher = DefaultHasher::new();
    data.hash(&mut hasher);
    format! {"{}", hasher.finish()}.into()
}
