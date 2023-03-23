use std::collections::{BTreeMap, BTreeSet};

use common::InputFile;
use parser::{
    ast::{self, prelude::*},
    SyntaxNode,
};

use crate::{
    hir_def::{module_tree, TopLevelMod},
    lower, HirDb,
};

use super::ItemKind;

/// This tree represents the item hierarchy inside a file.
/// The root node of the tree is the top level module, which corresponds to the
/// `module_tree::TopLevelModule`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleItemTree {
    pub(crate) file: InputFile,
    pub(crate) top_mod: TopLevelMod,
    pub(crate) item_tree: BTreeMap<ItemKind, ItemTreeNode>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ItemTreeNode {
    pub(crate) parent: Option<ItemKind>,
    pub(crate) children: BTreeSet<ItemKind>,
}

#[salsa::tracked(return_ref)]
pub fn module_item_tree(db: &dyn HirDb, file: InputFile) -> ModuleItemTree {
    let node = SyntaxNode::new_root(crate::parse_file(db, file));
    let module_tree = module_tree::ingot_module_tree(db, file.ingot(db.upcast()));

    // This cast never fails even if the file content is empty.
    let ast_root = ast::Root::cast(node).unwrap();
    let top_mod_name = module_tree.module_name(file);
    lower::lower_file(db, file, top_mod_name, ast_root)
}
