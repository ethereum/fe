use camino::{Utf8Path, Utf8PathBuf};
use common::{indexmap::IndexMap, input::ingot_root_file, InputFile, InputIngot};
use cranelift_entity::{entity_impl, EntityRef, PrimaryMap};
use salsa::Update;

use super::{IdentId, IngotId, TopLevelMod};
use crate::{lower::map_file_to_mod_impl, HirDb};

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
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub struct ModuleTree<'db> {
    pub(crate) root: ModuleTreeNodeId,
    pub(crate) module_tree: PMap<ModuleTreeNodeId, ModuleTreeNode<'db>>,
    pub(crate) mod_map: IndexMap<TopLevelMod<'db>, ModuleTreeNodeId>,

    pub ingot: IngotId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PMap<K: EntityRef, V>(PrimaryMap<K, V>);

unsafe impl<K, V> Update for PMap<K, V>
where
    K: EntityRef,
    V: Update,
{
    unsafe fn maybe_update(old_pointer: *mut Self, new_vec: Self) -> bool {
        let old_vec: &mut PMap<K, V> = unsafe { &mut *old_pointer };

        if old_vec.0.len() != new_vec.0.len() {
            return true;
        }

        let mut changed = false;
        for (old_element, new_element) in old_vec
            .0
            .values_mut()
            .zip(new_vec.0.into_iter().map(|(_, v)| v))
        {
            changed |= V::maybe_update(old_element, new_element);
        }

        changed
    }
}

impl ModuleTree<'_> {
    /// Returns the tree node data of the given id.
    pub fn node_data(&self, id: ModuleTreeNodeId) -> &ModuleTreeNode {
        &self.module_tree.0[id]
    }

    /// Returns the tree node id of the given top level module.
    pub fn tree_node(&self, top_mod: TopLevelMod) -> ModuleTreeNodeId {
        self.mod_map[&top_mod]
    }

    /// Returns the tree node data of the given top level module.
    pub fn tree_node_data(&self, top_mod: TopLevelMod) -> &ModuleTreeNode {
        &self.module_tree.0[self.tree_node(top_mod)]
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
        node.parent.map(|id| self.module_tree.0[id].top_mod)
    }

    pub fn children(&self, top_mod: TopLevelMod) -> impl Iterator<Item = TopLevelMod> + '_ {
        self.tree_node_data(top_mod)
            .children
            .iter()
            .map(move |&id| {
                let node = &self.module_tree.0[id];
                node.top_mod
            })
    }
}

/// Returns a module tree of the given ingot. The resulted tree only includes
/// top level modules. This function only depends on an ingot structure and
/// external ingot dependency, and not depends on file contents.
#[salsa::tracked(return_ref)]
#[allow(elided_named_lifetimes)]
pub(crate) fn module_tree_impl(db: &dyn HirDb, ingot: InputIngot) -> ModuleTree<'_> {
    ModuleTreeBuilder::new(db, ingot).build()
}

/// A top level module that is one-to-one mapped to a file.
#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ModuleTreeNode<'db> {
    pub top_mod: TopLevelMod<'db>,
    /// A parent of the top level module.
    /// This is `None` if
    /// 1. the module is a root module or
    /// 2. the module is a "floating" module.
    pub parent: Option<ModuleTreeNodeId>,
    /// A list of child top level module.
    pub children: Vec<ModuleTreeNodeId>,
}

impl<'db> ModuleTreeNode<'db> {
    fn new(top_mod: TopLevelMod<'db>) -> Self {
        Self {
            top_mod,
            parent: None,
            children: Vec::new(),
        }
    }
    pub fn name(&self, db: &'db dyn HirDb) -> IdentId<'db> {
        self.top_mod.name(db)
    }
}

/// An opaque identifier for a module tree node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct ModuleTreeNodeId(u32);
entity_impl!(ModuleTreeNodeId);

struct ModuleTreeBuilder<'db> {
    db: &'db dyn HirDb,
    input_ingot: InputIngot,
    ingot: IngotId<'db>,
    module_tree: PrimaryMap<ModuleTreeNodeId, ModuleTreeNode<'db>>,
    mod_map: IndexMap<TopLevelMod<'db>, ModuleTreeNodeId>,
    path_map: IndexMap<&'db Utf8Path, ModuleTreeNodeId>,
}

impl<'db> ModuleTreeBuilder<'db> {
    fn new(db: &'db dyn HirDb, ingot: InputIngot) -> Self {
        Self {
            db,
            input_ingot: ingot,
            ingot: IngotId::new(db, ingot),
            module_tree: PrimaryMap::default(),
            mod_map: IndexMap::default(),
            path_map: IndexMap::default(),
        }
    }

    fn build(mut self) -> ModuleTree<'db> {
        self.set_modules();
        self.build_tree();

        let root_mod = map_file_to_mod_impl(
            self.db,
            self.ingot,
            ingot_root_file(self.db, self.ingot.inner(self.db)),
        );
        let root = self.mod_map[&root_mod];
        ModuleTree {
            root,
            module_tree: PMap(self.module_tree),
            mod_map: self.mod_map,
            ingot: self.ingot,
        }
    }

    fn set_modules(&mut self) {
        for file in self.input_ingot.input_files(self.db) {
            // Get the path reference directly, which has the 'db lifetime.
            let path: &'db Utf8Path = file.path(self.db);
            let top_mod = map_file_to_mod_impl(self.db, self.ingot, file);

            let module_id = self.module_tree.push(ModuleTreeNode::new(top_mod));
            // Insert the path reference directly into the map.
            self.path_map.insert(path, module_id);
            self.mod_map.insert(top_mod, module_id);
        }
    }

    fn build_tree(&mut self) {
        let db = self.db.clone();
        let root = ingot_root_file(self.db, self.ingot.inner(self.db)).clone();
        let input_ingot = self.ingot.inner(db).clone();
        let input_files = input_ingot.input_files(self.db);

        for child in input_files {
            // Ignore the root file because it has no parent.
            if child == root {
                continue;
            }

            let root_path = root.path(self.db);
            let root_mod = map_file_to_mod_impl(self.db, self.ingot, root.clone());
            let child_path = child.path(self.db);
            let child_mod = map_file_to_mod_impl(self.db, self.ingot, child.clone());

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
        let file_path = file.path(self.db);
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
    use common::input::{IngotFiles, IngotKind, Version};

    use super::*;
    use crate::{lower, test_db::TestDb};

    #[test]
    fn module_tree() {
        let db = TestDb::default();

        let local_root_path = Utf8PathBuf::from("src/lib.fe");
        let mod1_path = Utf8PathBuf::from("src/mod1.fe");
        let mod2_path = Utf8PathBuf::from("src/mod2.fe");
        let foo_path = Utf8PathBuf::from("src/mod1/foo.fe");
        let bar_path = Utf8PathBuf::from("src/mod2/bar.fe");
        let baz_path = Utf8PathBuf::from("src/mod2/baz.fe");
        let floating_path = Utf8PathBuf::from("src/mod3/floating.fe");

        let ingot_files = IngotFiles::from_contents(
            &db,
            vec![
                (local_root_path.clone(), ""),
                (mod1_path.clone(), ""),
                (mod2_path.clone(), ""),
                (foo_path.clone(), ""),
                (bar_path.clone(), ""),
                (baz_path.clone(), ""),
                (floating_path.clone(), ""),
            ],
        );

        let local_ingot = InputIngot::new(
            &db,
            "/foo/fargo".into(),
            IngotKind::Local,
            Version::new(0, 0, 1),
            Default::default(),
            ingot_files,
            // Default::default(),
            // None,
        );

        let local_root_mod =
            lower::map_file_path_to_mod(&db, local_ingot, local_root_path).unwrap();
        let mod1_mod = lower::map_file_path_to_mod(&db, local_ingot, mod1_path).unwrap();
        let mod2_mod = lower::map_file_path_to_mod(&db, local_ingot, mod2_path).unwrap();
        let foo_mod = lower::map_file_path_to_mod(&db, local_ingot, foo_path).unwrap();
        let bar_mod = lower::map_file_path_to_mod(&db, local_ingot, bar_path).unwrap();
        let baz_mod = lower::map_file_path_to_mod(&db, local_ingot, baz_path).unwrap();

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
