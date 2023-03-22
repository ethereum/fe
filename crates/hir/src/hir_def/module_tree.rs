use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

use camino::{Utf8Path, Utf8PathBuf};
use common::{InputFile, InputIngot};
use cranelift_entity::{entity_impl, PrimaryMap};

use crate::HirDb;

use super::IdentId;

/// This tree represents the structure of an ingot.
/// Internal modules are not included in this tree, instead, they are included
/// in [`crate::item_tree::ModuleItemTree`].
///
/// This is used in later name resolution phase.
/// The tree is file contents agnostic, i.e., **only** depends on project
/// structure and crate dependency.
///  
///
/// Example:
/// ```
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
/// The resulting tree would be like below.
/// ```
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
    /// A parent of top level module.
    /// This is `None` if 1. the module is a root module or 2. the module is a
    /// "floating" module.
    pub(crate) parent: Option<ToplevelModuleId>,
    /// A list of child top level module.
    pub(crate) children: BTreeMap<IdentId, Vec<ToplevelModuleId>>,
}

impl ToplevelModule {
    fn new(name: IdentId, file: InputFile) -> Self {
        Self {
            name,
            file,
            parent: None,
            children: BTreeMap::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ToplevelModuleId(u32);
entity_impl!(ToplevelModuleId);

/// Returns a module tree of the given ingot. The resulted tree only includes
/// top level modules. This function only depends on an ingot structure and
/// external ingot dependency, and not depends on file contents.
#[salsa::tracked(return_ref)]
pub fn ingot_module_tree(db: &dyn HirDb, ingot: InputIngot) -> IngotModuleTree {
    IngotModuleTreeBuilder::new(db, ingot).build()
}

struct IngotModuleTreeBuilder<'db> {
    db: &'db dyn HirDb,
    ingot: InputIngot,
    module_tree: PrimaryMap<ToplevelModuleId, ToplevelModule>,
    file_map: BTreeMap<InputFile, ToplevelModuleId>,
    path_map: BTreeMap<&'db Utf8Path, ToplevelModuleId>,
}

impl<'db> IngotModuleTreeBuilder<'db> {
    fn new(db: &'db dyn HirDb, ingot: InputIngot) -> Self {
        Self {
            db,
            ingot,
            module_tree: PrimaryMap::default(),
            file_map: BTreeMap::default(),
            path_map: BTreeMap::default(),
        }
    }

    fn build(mut self) -> IngotModuleTree {
        self.set_modules();
        self.build_tree();

        let root_file = self.ingot.root_file(self.db.upcast());
        let root = self.file_map[&root_file];
        IngotModuleTree {
            root,
            module_tree: self.module_tree,
            file_map: self.file_map,
            ingot: self.ingot,
        }
    }

    fn set_modules(&mut self) {
        for &file in self.ingot.files(self.db.upcast()) {
            let name = self.module_name(file);

            let module_id = self.module_tree.push(ToplevelModule::new(name, file));
            self.path_map.insert(file.path(self.db.upcast()), module_id);
            self.file_map.insert(file, module_id);
        }
    }

    fn module_name(&self, file: InputFile) -> IdentId {
        let path = file.path(self.db.upcast());
        let name = path.file_stem().unwrap();
        IdentId::new(self.db, name.to_string())
    }

    fn build_tree(&mut self) {
        let root = self.ingot.root_file(self.db.upcast());

        for &file in self.ingot.files(self.db.upcast()) {
            // Ignore the root file because it has no parent.
            if file == root {
                continue;
            }

            let file_path = file.path(self.db.upcast());
            let root_path = root.path(self.db.upcast());

            // If the file is in the same directory as the root file, the file is a direct
            // child of the root.
            if file_path.parent() == root_path.parent() {
                let root_mod = self.file_map[&root];
                let cur_mod = self.file_map[&file];
                self.add_branch(root_mod, cur_mod);
                continue;
            }

            assert!(file_path
                .parent()
                .unwrap()
                .starts_with(root_path.parent().unwrap()));

            if let Some(parent_mod) = self.parent_module(file) {
                let cur_mod = self.file_map[&file];
                self.add_branch(parent_mod, cur_mod);
            }
        }
    }

    fn parent_module(&self, file: InputFile) -> Option<ToplevelModuleId> {
        let file_path = file.path(self.db.upcast());
        let file_dir = file_path.parent()?;
        let parent_dir = file_dir.parent()?;

        let parent_mod_stem = file_dir.into_iter().next_back()?;
        let parent_mod_path = parent_dir.join(parent_mod_stem).with_extension("fe");
        self.path_map.get(parent_mod_path.as_path()).copied()
    }

    fn add_branch(&mut self, parent: ToplevelModuleId, child: ToplevelModuleId) {
        let child_name = self.module_tree[child].name;
        self.module_tree[parent]
            .children
            .entry(child_name)
            .or_default()
            .push(child);

        self.module_tree[child].parent = Some(parent);
    }
}
