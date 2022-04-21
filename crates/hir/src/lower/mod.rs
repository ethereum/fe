use std::collections::{BTreeMap, BTreeSet};

use common::{InputFile, InputIngot};
use num_bigint::BigUint;
use num_traits::Num;
use parser::{
    ast::{self, prelude::*},
    GreenNode, SyntaxNode, SyntaxToken,
};

use crate::{
    hir_def::{
        module_tree_impl, IdentId, IntegerId, ItemKind, ItemTree, ItemTreeNode, LitKind,
        ModuleTree, Partial, StringId, TopLevelMod, TrackedItemId,
    },
    HirDb, LowerHirDb, ParseDiagnostic,
};

use self::{
    item::lower_module_items,
    parse::{parse_file_impl, ParseDiagnosticAccumulator},
};

pub(crate) mod parse;

mod attr;
mod body;
mod expr;
mod item;
mod params;
mod pat;
mod path;
mod scope_builder;
mod stmt;
mod types;
mod use_tree;

/// Maps the given file to a top-level module.
/// This function just maps the file to a top-level module, and doesn't perform
/// any parsing or lowering.
/// To perform the actual lowering, use `module_item_tree` function.
pub fn map_file_to_mod(db: &dyn LowerHirDb, file: InputFile) -> TopLevelMod {
    map_file_to_mod_impl(db.upcast(), file)
}

/// Returns the item tree of the given top-level module.
pub fn item_tree(db: &dyn LowerHirDb, top_mod: TopLevelMod) -> &ItemTree {
    item_tree_impl(db.upcast(), top_mod)
}

/// Returns the root node of the given top-level module.
/// This function also returns the diagnostics produced by parsing the file.
pub fn parse_file_with_diag(
    db: &dyn LowerHirDb,
    top_mod: TopLevelMod,
) -> (GreenNode, Vec<ParseDiagnostic>) {
    (
        parse_file_impl(db.upcast(), top_mod),
        parse_file_impl::accumulated::<ParseDiagnosticAccumulator>(db.upcast(), top_mod),
    )
}

/// Returns the root node of the given top-level module.
/// If diagnostics are needed, use [`parse_file_with_diag`] instead.
pub fn parse_file(db: &dyn LowerHirDb, top_mod: TopLevelMod) -> GreenNode {
    parse_file_impl(db.upcast(), top_mod)
}

/// Returns the ingot module tree of the given ingot.
pub fn module_tree(db: &dyn LowerHirDb, ingot: InputIngot) -> &ModuleTree {
    module_tree_impl(db.upcast(), ingot)
}

#[salsa::tracked]
pub(crate) fn map_file_to_mod_impl(db: &dyn HirDb, file: InputFile) -> TopLevelMod {
    let path = file.path(db.upcast());
    let name = path.file_stem().unwrap();
    let mod_name = IdentId::new(db, name.to_string());
    let ingot = file.ingot(db.upcast());
    TopLevelMod::new(db, mod_name, ingot, file)
}

#[salsa::tracked(return_ref)]
pub(crate) fn item_tree_impl(db: &dyn HirDb, top_mod: TopLevelMod) -> ItemTree {
    let ast = top_mod_ast(db, top_mod);
    let mut ctxt = FileLowerCtxt::new(db, top_mod);

    ctxt.enter_scope();
    let id = TrackedItemId::TopLevelMod(top_mod.name(db));
    if let Some(items) = ast.items() {
        lower_module_items(&mut ctxt, id, items);
    }
    ctxt.leave_scope(top_mod);

    ctxt.build()
}

pub(crate) fn top_mod_ast(db: &dyn HirDb, top_mod: TopLevelMod) -> ast::Root {
    let node = SyntaxNode::new_root(parse_file_impl(db, top_mod));
    // This cast never fails even if the file content is empty.
    ast::Root::cast(node).unwrap()
}

pub struct FileLowerCtxt<'db> {
    db: &'db dyn HirDb,
    scope_stack: Vec<BTreeSet<ItemKind>>,
    item_tree: BTreeMap<ItemKind, ItemTreeNode>,
    top_mod: TopLevelMod,
}

impl<'db> FileLowerCtxt<'db> {
    pub(super) fn new(db: &'db dyn HirDb, top_mod: TopLevelMod) -> Self {
        Self {
            db,
            scope_stack: vec![],
            item_tree: BTreeMap::new(),
            top_mod,
        }
    }

    pub(super) fn build(self) -> ItemTree {
        ItemTree {
            top_mod: self.top_mod,
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
            self.item_tree.get_mut(item).unwrap().parent = Some(item_kind);
        }

        self.item_tree.insert(
            item_kind,
            ItemTreeNode {
                parent: None,
                children: item_scope,
            },
        );

        if !matches!(item_kind, ItemKind::TopMod(_)) {
            self.scope_stack.last_mut().unwrap().insert(item.into());
        }
        item
    }
}

impl IdentId {
    fn lower_token(ctxt: &mut FileLowerCtxt<'_>, token: SyntaxToken) -> Self {
        Self::new(ctxt.db, token.text().to_string())
    }

    fn lower_token_partial(
        ctxt: &mut FileLowerCtxt<'_>,
        token: Option<SyntaxToken>,
    ) -> Partial<Self> {
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
            return Self::new(ctxt.db, BigUint::from_str_radix(text, 10).unwrap());
        }

        let int = match &text[0..2] {
            "0x" | "0X" => BigUint::from_str_radix(&text[2..], 16).unwrap(),
            "0o" | "0O" => BigUint::from_str_radix(&text[2..], 8).unwrap(),
            "0b" | "0B" => BigUint::from_str_radix(&text[2..], 2).unwrap(),
            _ => BigUint::from_str_radix(text, 10).unwrap(),
        };

        Self::new(ctxt.db, int)
    }
}
