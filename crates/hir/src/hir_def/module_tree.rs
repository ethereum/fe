use std::collections::BTreeMap;

use common::{InputFile, InputIngot};
use cranelift_entity::{entity_impl, PrimaryMap};

use crate::HirDb;

use super::IdentId;

/// This tree represents the structure of an ingot.
///
/// This is used in later name resolution phase.
/// The tree is file contents agnostic, i.e., **only** depends on project
/// structure and crate dependency.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IngotModuleTree {
    pub(crate) root: ToplevelModuleId,
    pub(crate) module_tree: PrimaryMap<ToplevelModuleId, ToplevelModule>,
    pub(crate) file_map: BTreeMap<InputFile, ToplevelModuleId>,

    pub(crate) ingot: InputIngot,
}

/// A top level module that is one-to-one mapped to a file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ToplevelModule {
    /// A name of the module.
    pub(crate) name: IdentId,
    /// A file that this module is defined by.
    pub(crate) file: InputFile,
    /// A parent top level module.
    pub(crate) parent: Option<ToplevelModuleId>,
    /// A list of child top level module.
    pub(crate) children: BTreeMap<IdentId, Vec<ToplevelModuleId>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ToplevelModuleId(u32);
entity_impl!(ToplevelModuleId);

#[salsa::tracked(return_ref)]
pub fn ingot_module_tree(db: &dyn HirDb, ingot: InputIngot) -> IngotModuleTree {
    todo!()
}
