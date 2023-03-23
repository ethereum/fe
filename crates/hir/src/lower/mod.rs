use std::collections::{BTreeMap, BTreeSet};

use common::InputFile;
use num_bigint::BigUint;
use num_traits::Num;
use parser::{ast, SyntaxToken};

use crate::{
    hir_def::{
        IdentId, IntegerId, ItemKind, ItemTreeNode, LitKind, MaybeInvalid, ModuleItemTree,
        StringId, TopLevelMod,
    },
    HirDb,
};

mod attr;
mod body;
mod expr;
mod item;
mod params;
mod pat;
mod path;
mod stmt;
mod types;
mod use_tree;

pub(super) fn lower_file(
    db: &dyn HirDb,
    file: InputFile,
    top_mod_name: IdentId,
    root_node: ast::Root,
) -> ModuleItemTree {
    let mut ctxt = FileLowerCtxt::new(db, file);
    let top_mod = TopLevelMod::lower_ast(&mut ctxt, top_mod_name, root_node);
    ctxt.build(top_mod)
}

pub struct FileLowerCtxt<'db> {
    db: &'db dyn HirDb,
    file: InputFile,
    scope_stack: Vec<BTreeSet<ItemKind>>,
    item_tree: BTreeMap<ItemKind, ItemTreeNode>,
}

impl<'db> FileLowerCtxt<'db> {
    pub(super) fn new(db: &'db dyn HirDb, file: InputFile) -> Self {
        Self {
            db,
            file,
            scope_stack: vec![],
            item_tree: BTreeMap::new(),
        }
    }

    pub(super) fn build(self, top_mod: TopLevelMod) -> ModuleItemTree {
        ModuleItemTree {
            file: self.file,
            top_mod,
            item_tree: self.item_tree,
        }
    }

    /// Creates a new scope for an item.
    fn enter_scope(&mut self) {
        self.scope_stack.push(BTreeSet::default());
    }

    /// Leaves the current scope, `item` should be the generated item which owns
    /// the scope.
    fn leave_scope<I>(&mut self, item: I) -> I
    where
        I: Into<ItemKind> + Copy,
    {
        let item_kind = item.into();
        let item_scope = self.scope_stack.pop().unwrap();

        for item in &item_scope {
            self.item_tree.get_mut(&item).unwrap().parent = Some(item_kind);
        }

        self.item_tree.insert(
            item_kind,
            ItemTreeNode {
                parent: None,
                children: item_scope,
            },
        );

        self.scope_stack.last_mut().unwrap().insert(item.into());
        item
    }
}

impl IdentId {
    fn lower_token(ctxt: &mut FileLowerCtxt<'_>, token: SyntaxToken) -> Self {
        Self::new(ctxt.db, token.text().to_string())
    }

    fn maybe_lower_token(
        ctxt: &mut FileLowerCtxt<'_>,
        token: Option<SyntaxToken>,
    ) -> MaybeInvalid<Self> {
        token.map(|token| Self::lower_token(ctxt, token)).into()
    }
}

impl LitKind {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Lit) -> Self {
        match ast.kind() {
            ast::LitKind::Int(int) => Self::Int(IntegerId::lower_ast(ctxt, int)),
            ast::LitKind::String(string) => {
                let text = string.token().text();
                Self::String(StringId::new(ctxt.db, text[1..text.len() - 1].to_string()))
            }
            ast::LitKind::Bool(bool) => match bool.token().text() {
                "true" => Self::Bool(true),
                "false" => Self::Bool(false),
                _ => unreachable!(),
            },
        }
    }
}

impl IntegerId {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::LitInt) -> Self {
        let text = ast.token().text();
        // Parser ensures that the text is valid pair with a radix and a number.
        if text.len() < 2 {
            return Self::new(ctxt.db, BigUint::from_str_radix(&text, 10).unwrap());
        }

        let int = match &text[0..2] {
            "0x" | "0X" => BigUint::from_str_radix(&text[2..], 16).unwrap(),
            "0o" | "0O" => BigUint::from_str_radix(&text[2..], 8).unwrap(),
            "0b" | "0B" => BigUint::from_str_radix(&text[2..], 2).unwrap(),
            _ => BigUint::from_str_radix(&text, 10).unwrap(),
        };

        Self::new(ctxt.db, int)
    }
}
