use std::{fmt::Debug, hash::Hash};

use ena::unify::{InPlaceUnificationTable, UnifyKey};
use rustc_hash::FxHashMap;

/// Represents a definition that contains a direct reference to itself.
///
/// Recursive definitions are not valid and must be reported to the user.
/// It is preferable to group definitions together such that recursions
/// are reported in-whole rather than separately. `RecursiveDef` can be
/// used with `RecursiveDefHelper` to perform this grouping operation.
///
/// The fields `from` and `to` are the relevant identifiers and `site` can
/// be used to carry diagnostic information.
#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct RecursiveDef<T, U>
where
    T: PartialEq + Copy,
{
    pub from: T,
    pub to: T,
    pub site: U,
}

impl<T, U> RecursiveDef<T, U>
where
    T: PartialEq + Copy,
{
    pub fn new(from: T, to: T, site: U) -> Self {
        Self { from, to, site }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
struct RecursiveDefKey(u32);

impl UnifyKey for RecursiveDefKey {
    type Value = ();

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(idx: u32) -> Self {
        Self(idx)
    }

    fn tag() -> &'static str {
        "RecursiveDefKey"
    }
}

pub struct RecursiveDefHelper<T, U>
where
    T: Eq + Clone + Debug + Copy,
{
    defs: Vec<RecursiveDef<T, U>>,
    table: InPlaceUnificationTable<RecursiveDefKey>,
    keys: FxHashMap<T, RecursiveDefKey>,
}

impl<T, U> RecursiveDefHelper<T, U>
where
    T: Eq + Clone + Debug + Copy + Hash,
{
    pub fn new(defs: Vec<RecursiveDef<T, U>>) -> Self {
        let mut table = InPlaceUnificationTable::new();
        let keys: FxHashMap<_, _> = defs
            .iter()
            .map(|def| (def.from, table.new_key(())))
            .collect();

        for def in defs.iter() {
            table.union(keys[&def.from], keys[&def.to])
        }

        Self { defs, table, keys }
    }

    /// Removes a disjoint set of recursive definitions from the helper
    /// and returns it, if one exists.
    pub fn remove_disjoint_set(&mut self) -> Option<Vec<RecursiveDef<T, U>>> {
        let mut disjoint_set = vec![];
        let mut remaining_set = vec![];
        let mut union_key: Option<&RecursiveDefKey> = None;

        while let Some(def) = self.defs.pop() {
            let cur_key = &self.keys[&def.from];

            if union_key.is_none() || self.table.unioned(*union_key.unwrap(), *cur_key) {
                disjoint_set.push(def)
            } else {
                remaining_set.push(def)
            }

            if union_key.is_none() {
                union_key = Some(cur_key)
            }
        }

        self.defs = remaining_set;

        if union_key.is_some() {
            Some(disjoint_set)
        } else {
            None
        }
    }
}

#[test]
fn one_recursion() {
    let defs = vec![RecursiveDef::new(0, 1, ()), RecursiveDef::new(1, 0, ())];
    let mut helper = RecursiveDefHelper::new(defs);
    assert!(helper.remove_disjoint_set().is_some());
    assert!(helper.remove_disjoint_set().is_none());
}

#[test]
fn two_recursions() {
    let defs = vec![
        RecursiveDef::new(0, 1, ()),
        RecursiveDef::new(1, 0, ()),
        RecursiveDef::new(2, 3, ()),
        RecursiveDef::new(3, 4, ()),
        RecursiveDef::new(4, 2, ()),
    ];
    let mut helper = RecursiveDefHelper::new(defs);
    assert!(helper.remove_disjoint_set().is_some());
    assert!(helper.remove_disjoint_set().is_some());
    assert!(helper.remove_disjoint_set().is_none());
}
