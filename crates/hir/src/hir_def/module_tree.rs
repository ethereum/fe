use std::collections::BTreeMap;

use camino::Utf8Path;
use common::{InputFile, InputIngot};
use cranelift_entity::{entity_impl, PrimaryMap};

use crate::{lower::map_file_to_mod_impl, HirDb};

use super::{IdentId, TopLevelMod};

/// This tree represents the structure of an ingot.
/// Internal modules are not included in this tree, instead, they are included
/// in [ModuleItemTree](crate::hir_def::item_tree::ModuleItemTree).
///
/// This is used in later name resolution phase.
/// The tree is file contents agnostic, i.e., **only** depends on project
/// structure and crate dependency.
///  
///
/// Example:
/// ```text
/// ingot/
/// ├─ main.fe
/// ├─ mod1.fe
/// ├─ mod1/
/// │  ├─ foo.fe
/// ├─ mod2.fe
/// ├─ mod2
/// │  ├─ bar.fe
/// ├─ mod3
/// │  ├─ baz.fe
/// ```
///
/// The resulting tree would be like below.
///
/// ```text
///           +------+
///     *---- | main |----*
///     |     +------+    |         +------+
///     |                 |         | baz  |
///     |                 |         +------+
///     v                 v         
///  +------+          +------+     
///  | mod2 |          | mod1 |
///  +------+          +------+
///     |                 |
///     |                 |
///     v                 v
///  +------+          +------+
///  | bar  |          | foo  |
///  +------+          +------+
///  ```
///
/// **NOTE:** `mod3` is not included in the main tree because it doesn't have a corresponding file.
/// As a result, `baz` is represented as a "floating" node.
/// In this case, the tree is actually a forest. But we don't need to care about it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IngotModuleTree {
    pub(crate) root: ModuleTreeNodeId,
    pub(crate) module_tree: PrimaryMap<ModuleTreeNodeId, ModuleTreeNode>,
    pub(crate) mod_map: BTreeMap<TopLevelMod, ModuleTreeNodeId>,

    pub(crate) ingot: InputIngot,
}

impl IngotModuleTree {
    /// Returns the tree node data of the given id.
    pub fn tree_node_data(&self, id: ModuleTreeNodeId) -> &ModuleTreeNode {
        &self.module_tree[id]
    }

    /// Returns the tree node id of the given top level module.
    pub fn tree_node(&self, top_mod: TopLevelMod) -> ModuleTreeNodeId {
        self.mod_map[&top_mod]
    }

    /// Returns the root of the tree, which corresponds to the ingot root file.
    pub fn root(&self) -> ModuleTreeNodeId {
        self.root
    }

    /// Returns an iterator of all top level modules in this ingot.
    pub fn all_modules(&self) -> impl Iterator<Item = TopLevelMod> + '_ {
        self.mod_map.keys().copied()
    }
}

/// Returns a module tree of the given ingot. The resulted tree only includes
/// top level modules. This function only depends on an ingot structure and
/// external ingot dependency, and not depends on file contents.
#[salsa::tracked(return_ref)]
pub fn ingot_module_tree_impl(db: &dyn HirDb, ingot: InputIngot) -> IngotModuleTree {
    IngotModuleTreeBuilder::new(db, ingot).build()
}

/// A top level module that is one-to-one mapped to a file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleTreeNode {
    pub top_mod: TopLevelMod,
    /// A parent of the top level module.
    /// This is `None` if
    /// 1. the module is a root module or
    /// 2. the module is a "floating" module.
    pub parent: Option<ModuleTreeNodeId>,
    /// A list of child top level module.
    pub children: BTreeMap<IdentId, Vec<ModuleTreeNodeId>>,
}

impl ModuleTreeNode {
    fn new(top_mod: TopLevelMod) -> Self {
        Self {
            top_mod,
            parent: None,
            children: BTreeMap::new(),
        }
    }
    fn name(&self, db: &dyn HirDb) -> IdentId {
        self.top_mod.name(db)
    }
}

/// An opaque identifier for a module tree node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleTreeNodeId(u32);
entity_impl!(ModuleTreeNodeId);

struct IngotModuleTreeBuilder<'db> {
    db: &'db dyn HirDb,
    ingot: InputIngot,
    module_tree: PrimaryMap<ModuleTreeNodeId, ModuleTreeNode>,
    mod_map: BTreeMap<TopLevelMod, ModuleTreeNodeId>,
    path_map: BTreeMap<&'db Utf8Path, ModuleTreeNodeId>,
}

impl<'db> IngotModuleTreeBuilder<'db> {
    fn new(db: &'db dyn HirDb, ingot: InputIngot) -> Self {
        Self {
            db,
            ingot,
            module_tree: PrimaryMap::default(),
            mod_map: BTreeMap::default(),
            path_map: BTreeMap::default(),
        }
    }

    fn build(mut self) -> IngotModuleTree {
        self.set_modules();
        self.build_tree();

        let top_mod = map_file_to_mod_impl(self.db, self.ingot.root_file(self.db.upcast()));
        let root = self.mod_map[&top_mod];
        IngotModuleTree {
            root,
            module_tree: self.module_tree,
            mod_map: self.mod_map,
            ingot: self.ingot,
        }
    }

    fn set_modules(&mut self) {
        for &file in self.ingot.files(self.db.upcast()) {
            let top_mod = map_file_to_mod_impl(self.db, file);

            let module_id = self.module_tree.push(ModuleTreeNode::new(top_mod));
            self.path_map.insert(file.path(self.db.upcast()), module_id);
            self.mod_map.insert(top_mod, module_id);
        }
    }

    fn build_tree(&mut self) {
        let root = self.ingot.root_file(self.db.upcast());

        for &child in self.ingot.files(self.db.upcast()) {
            // Ignore the root file because it has no parent.
            if child == root {
                continue;
            }

            let root_path = root.path(self.db.upcast());
            let root_mod = map_file_to_mod_impl(self.db, root);
            let child_path = child.path(self.db.upcast());
            let child_mod = map_file_to_mod_impl(self.db, child);

            // If the file is in the same directory as the root file, the file is a direct
            // child of the root.
            if child_path.parent() == root_path.parent() {
                let root_mod = self.mod_map[&root_mod];
                let cur_mod = self.mod_map[&child_mod];
                self.add_branch(root_mod, cur_mod);
                continue;
            }

            assert!(child_path
                .parent()
                .unwrap()
                .starts_with(root_path.parent().unwrap()));

            if let Some(parent_mod) = self.parent_module(child) {
                let cur_mod = self.mod_map[&child_mod];
                self.add_branch(parent_mod, cur_mod);
            }
        }
    }

    fn parent_module(&self, file: InputFile) -> Option<ModuleTreeNodeId> {
        let file_path = file.path(self.db.upcast());
        let file_dir = file_path.parent()?;
        let parent_dir = file_dir.parent()?;

        let parent_mod_stem = file_dir.into_iter().next_back()?;
        let parent_mod_path = parent_dir.join(parent_mod_stem).with_extension("fe");
        self.path_map.get(parent_mod_path.as_path()).copied()
    }

    fn add_branch(&mut self, parent: ModuleTreeNodeId, child: ModuleTreeNodeId) {
        let child_name = self.module_tree[child].name(self.db);
        self.module_tree[parent]
            .children
            .entry(child_name)
            .or_default()
            .push(child);

        self.module_tree[child].parent = Some(parent);
    }
}
