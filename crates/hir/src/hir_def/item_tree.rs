use std::collections::BTreeMap;

use common::{InputFile, InputIngot};

use crate::HirDb;

use super::{IdentId, ItemKind, MaybeInvalid, Mod, ToplevelModuleId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleItemTree {
    pub(crate) file: InputFile,
    pub(crate) root_mod: Mod,
    pub(crate) item_tree: BTreeMap<ItemKind, ItemTreeNode>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ItemTreeNode {
    pub(crate) parent: Option<ItemKind>,
    pub(crate) children: BTreeMap<MaybeInvalid<IdentId>, Vec<ItemKind>>,
}

#[salsa::tracked(return_ref)]
pub(crate) fn module_item_tree(db: &dyn HirDb, file: InputFile) -> ModuleItemTree {
    todo!()
}
