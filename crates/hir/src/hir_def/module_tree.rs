use std::collections::BTreeMap;

use camino::Utf8Path;
use common::{InputFile, InputIngot};
use cranelift_entity::{entity_impl, PrimaryMap};

use crate::{lower::map_file_to_mod_impl, HirDb};

use super::{IdentId, IngotId, TopLevelMod};

/// This tree represents the structure of an ingot.
/// Internal modules are not included in this tree, instead, they are included
/// in [ScopeGraph](crate::hir_def::scope_graph::ScopeGraph).
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
pub struct ModuleTree {
    pub(crate) root: ModuleTreeNodeId,
    pub(crate) module_tree: PrimaryMap<ModuleTreeNodeId, ModuleTreeNode>,
    pub(crate) mod_map: BTreeMap<TopLevelMod, ModuleTreeNodeId>,

    pub ingot: IngotId,
}

impl ModuleTree {
    /// Returns the tree node data of the given id.
    pub fn node_data(&self, id: ModuleTreeNodeId) -> &ModuleTreeNode {
        &self.module_tree[id]
    }

    /// Returns the tree node id of the given top level module.
    pub fn tree_node(&self, top_mod: TopLevelMod) -> ModuleTreeNodeId {
        self.mod_map[&top_mod]
    }

    /// Returns the tree node data of the given top level module.
    pub fn tree_node_data(&self, top_mod: TopLevelMod) -> &ModuleTreeNode {
        &self.module_tree[self.tree_node(top_mod)]
    }

    /// Returns the root of the tree, which corresponds to the ingot root file.
    pub fn root(&self) -> ModuleTreeNodeId {
        self.root
    }

    pub fn root_data(&self) -> &ModuleTreeNode {
        self.node_data(self.root)
    }

    /// Returns an iterator of all top level modules in this ingot.
    pub fn all_modules(&self) -> impl Iterator<Item = TopLevelMod> + '_ {
        self.mod_map.keys().copied()
    }

    pub fn parent(&self, top_mod: TopLevelMod) -> Option<TopLevelMod> {
        let node = self.tree_node_data(top_mod);
        node.parent.map(|id| self.module_tree[id].top_mod)
    }

    pub fn children(&self, top_mod: TopLevelMod) -> impl Iterator<Item = TopLevelMod> + '_ {
        self.tree_node_data(top_mod)
            .children
            .iter()
            .map(move |&id| {
                let node = &self.module_tree[id];
                node.top_mod
            })
    }
}

/// Returns a module tree of the given ingot. The resulted tree only includes
/// top level modules. This function only depends on an ingot structure and
/// external ingot dependency, and not depends on file contents.
#[salsa::tracked(return_ref)]
pub(crate) fn module_tree_impl(db: &dyn HirDb, ingot: InputIngot) -> ModuleTree {
    ModuleTreeBuilder::new(db, ingot).build()
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
    pub children: Vec<ModuleTreeNodeId>,
}

impl ModuleTreeNode {
    fn new(top_mod: TopLevelMod) -> Self {
        Self {
            top_mod,
            parent: None,
            children: Vec::new(),
        }
    }
    pub fn name(&self, db: &dyn HirDb) -> IdentId {
        self.top_mod.name(db)
    }
}

/// An opaque identifier for a module tree node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleTreeNodeId(u32);
entity_impl!(ModuleTreeNodeId);

struct ModuleTreeBuilder<'db> {
    db: &'db dyn HirDb,
    input_ingot: InputIngot,
    ingot: IngotId,
    module_tree: PrimaryMap<ModuleTreeNodeId, ModuleTreeNode>,
    mod_map: BTreeMap<TopLevelMod, ModuleTreeNodeId>,
    path_map: BTreeMap<&'db Utf8Path, ModuleTreeNodeId>,
}

impl<'db> ModuleTreeBuilder<'db> {
    fn new(db: &'db dyn HirDb, ingot: InputIngot) -> Self {
        Self {
            db,
            input_ingot: ingot,
            ingot: IngotId::new(db, ingot),
            module_tree: PrimaryMap::default(),
            mod_map: BTreeMap::default(),
            path_map: BTreeMap::default(),
        }
    }

    fn build(mut self) -> ModuleTree {
        self.set_modules();
        self.build_tree();

        let root_mod = map_file_to_mod_impl(
            self.db,
            self.ingot,
            self.input_ingot.root_file(self.db.as_input_db()),
        );
        let root = self.mod_map[&root_mod];
        ModuleTree {
            root,
            module_tree: self.module_tree,
            mod_map: self.mod_map,
            ingot: self.ingot,
        }
    }

    fn set_modules(&mut self) {
        for &file in self.input_ingot.files(self.db.as_input_db()) {
            let top_mod = map_file_to_mod_impl(self.db, self.ingot, file);

            let module_id = self.module_tree.push(ModuleTreeNode::new(top_mod));
            self.path_map
                .insert(file.path(self.db.as_input_db()), module_id);
            self.mod_map.insert(top_mod, module_id);
        }
    }

    fn build_tree(&mut self) {
        let root = self.input_ingot.root_file(self.db.as_input_db());

        for &child in self.input_ingot.files(self.db.as_input_db()) {
            // Ignore the root file because it has no parent.
            if child == root {
                continue;
            }

            let root_path = root.path(self.db.as_input_db());
            let root_mod = map_file_to_mod_impl(self.db, self.ingot, root);
            let child_path = child.path(self.db.as_input_db());
            let child_mod = map_file_to_mod_impl(self.db, self.ingot, child);

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
        let file_path = file.path(self.db.as_input_db());
        let file_dir = file_path.parent()?;
        let parent_dir = file_dir.parent()?;

        let parent_mod_stem = file_dir.into_iter().next_back()?;
        let parent_mod_path = parent_dir.join(parent_mod_stem).with_extension("fe");
        self.path_map.get(parent_mod_path.as_path()).copied()
    }

    fn add_branch(&mut self, parent: ModuleTreeNodeId, child: ModuleTreeNodeId) {
        self.module_tree[parent].children.push(child);

        self.module_tree[child].parent = Some(parent);
    }
}

#[cfg(test)]
mod tests {
    use common::input::{IngotKind, Version};

    use super::*;
    use crate::{lower, test_db::TestDb};

    #[test]
    fn module_tree() {
        let mut db = TestDb::default();

        let local_ingot = InputIngot::new(
            &db,
            "/foo/fargo",
            IngotKind::Local,
            Version::new(0, 0, 1),
            [].into(),
        );
        let local_root = InputFile::new(&db, local_ingot, "src/lib.fe".into(), "".into());
        let mod1 = InputFile::new(&db, local_ingot, "src/mod1.fe".into(), "".into());
        let mod2 = InputFile::new(&db, local_ingot, "src/mod2.fe".into(), "".into());
        let foo = InputFile::new(&db, local_ingot, "src/mod1/foo.fe".into(), "".into());
        let bar = InputFile::new(&db, local_ingot, "src/mod2/bar.fe".into(), "".into());
        let baz = InputFile::new(&db, local_ingot, "src/mod2/baz.fe".into(), "".into());
        let floating = InputFile::new(&db, local_ingot, "src/mod3/floating.fe".into(), "".into());
        local_ingot.set_root_file(&mut db, local_root);
        local_ingot.set_files(
            &mut db,
            [local_root, mod1, mod2, foo, bar, baz, floating].into(),
        );

        let local_root_mod = lower::map_file_to_mod(&db, local_root);
        let mod1_mod = lower::map_file_to_mod(&db, mod1);
        let mod2_mod = lower::map_file_to_mod(&db, mod2);
        let foo_mod = lower::map_file_to_mod(&db, foo);
        let bar_mod = lower::map_file_to_mod(&db, bar);
        let baz_mod = lower::map_file_to_mod(&db, baz);

        let local_tree = lower::module_tree(&db, local_ingot);
        let root_node = local_tree.root_data();
        assert_eq!(root_node.top_mod, local_root_mod);
        assert_eq!(root_node.children.len(), 2);

        for &child in &root_node.children {
            if child == local_tree.tree_node(mod1_mod) {
                let child = local_tree.node_data(child);
                assert_eq!(child.parent, Some(local_tree.root()));
                assert_eq!(child.children.len(), 1);
                assert_eq!(child.children[0], local_tree.tree_node(foo_mod));
            } else if child == local_tree.tree_node(mod2_mod) {
                let child = local_tree.node_data(child);
                assert_eq!(child.parent, Some(local_tree.root()));
                assert_eq!(child.children.len(), 2);
                assert_eq!(child.children[0], local_tree.tree_node(bar_mod));
                assert_eq!(child.children[1], local_tree.tree_node(baz_mod));
            } else {
                panic!("unexpected child")
            }
        }
    }
}
