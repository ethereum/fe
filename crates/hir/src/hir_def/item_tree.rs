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
pub struct ItemTree {
    pub file: InputFile,
    pub top_mod: TopLevelMod,
    pub(crate) item_tree: BTreeMap<ItemKind, ItemTreeNode>,
}

impl ItemTree {
    /// Returns the depth-first iterator of the item tree.
    pub fn dfs(&self) -> impl Iterator<Item = ItemKind> + '_ {
        let mut stack = vec![self.top_mod.into()];
        std::iter::from_fn(move || {
            let item = stack.pop()?;
            stack.extend(self.item_tree[&item].children.iter().rev());
            Some(item)
        })
    }

    /// Returns the parent of the item.
    pub fn parent(&self, item: ItemKind) -> Option<ItemKind> {
        self.item_tree[&item].parent
    }

    /// Returns the children of the item.
    pub fn children(&self, item: ItemKind) -> impl Iterator<Item = ItemKind> + '_ {
        self.item_tree[&item].children.iter().copied()
    }

    /// Returns the number of items in the tree.
    pub fn len(&self) -> usize {
        self.item_tree.len()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ItemTreeNode {
    pub(crate) parent: Option<ItemKind>,
    pub(crate) children: BTreeSet<ItemKind>,
}

#[salsa::tracked(return_ref)]
pub fn module_item_tree(db: &dyn HirDb, file: InputFile) -> ItemTree {
    let node = SyntaxNode::new_root(crate::parse_file(db, file));
    let module_tree = module_tree::ingot_module_tree(db, file.ingot(db.upcast()));

    // This cast never fails even if the file content is empty.
    let ast_root = ast::Root::cast(node).unwrap();
    let top_mod_name = module_tree.module_name(file);
    lower::lower_file(db, file, top_mod_name, ast_root)
}

#[cfg(test)]
mod tests {

    use crate::{hir_def::ItemKind, test_db::TestDb};

    #[test]
    fn item_tree() {
        let mut db = TestDb::default();

        let text = r#"
            mod foo {
                fn bar() {}
                extern {
                    fn baz()
                }
            }
        
            enum MyEnum {}

            mod baz {
                struct MyS {}
            }
        "#;

        let (_, item_tree) = db.parse_source(text);
        let top_mod = item_tree.top_mod;
        assert_eq!(item_tree.len(), 8);

        let inner_items: Vec<_> = item_tree.children(top_mod.into()).collect();
        assert!(matches!(inner_items[0], ItemKind::Mod(_)));
        assert!(matches!(inner_items[1], ItemKind::Mod(_)));
        assert!(matches!(inner_items[2], ItemKind::Enum(_)));

        let foo_children: Vec<_> = item_tree.children(inner_items[0]).collect();
        assert!(matches!(foo_children[0], ItemKind::Func(_)));
        assert!(matches!(foo_children[1], ItemKind::ExternFunc(_)));

        let baz_children: Vec<_> = item_tree.children(inner_items[1]).collect();
        assert!(matches!(baz_children[0], ItemKind::Struct(_)));
    }
}
