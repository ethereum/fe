use std::collections::BTreeSet;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{hir_def::GenericParamOwner, span::DynLazySpan, HirDb};

use super::{Enum, Func, FuncParamLabel, IdentId, IngotId, ItemKind, TopLevelMod, Use, Visibility};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeGraph {
    pub top_mod: TopLevelMod,
    pub scopes: FxHashMap<ScopeId, Scope>,
    pub unresolved_uses: FxHashSet<Use>,
}

impl ScopeGraph {
    pub fn items_dfs<'a>(&'a self, db: &'a dyn HirDb) -> impl Iterator<Item = ItemKind> + 'a {
        ScopeGraphItemIterDfs {
            db,
            graph: self,
            visited: Default::default(),
            stack: vec![self.top_mod.into()],
        }
    }

    /// Returns the direct child items of the scope.
    pub fn child_items(&self, scope: ScopeId) -> impl Iterator<Item = ItemKind> + '_ {
        self.edges(scope).filter_map(|edge| match edge.kind {
            EdgeKind::Lex(_) | EdgeKind::Super(_) | EdgeKind::Ingot(_) | EdgeKind::SelfTy(_) => {
                None
            }

            _ => edge.dest.to_item(),
        })
    }

    pub fn edges(&self, scope: ScopeId) -> impl Iterator<Item = &ScopeEdge> + '_ {
        self.scopes[&scope].edges.iter()
    }

    pub fn scope_data(&self, scope: &ScopeId) -> &Scope {
        &self.scopes[scope]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScopeId {
    Item(ItemKind),
    GenericParam(ItemKind, usize),
    FuncParam(ItemKind, usize),
    Field(ItemKind, usize),
    Variant(ItemKind, usize),
}
impl ScopeId {
    pub fn top_mod(&self, db: &dyn HirDb) -> TopLevelMod {
        match self {
            ScopeId::Item(item) => item.top_mod(db),
            ScopeId::GenericParam(item, _) => item.top_mod(db),
            ScopeId::FuncParam(item, _) => item.top_mod(db),
            ScopeId::Field(item, _) => item.top_mod(db),
            ScopeId::Variant(item, _) => item.top_mod(db),
        }
    }

    pub fn from_item(item: ItemKind) -> Self {
        Self::Item(item)
    }

    pub fn to_item(self) -> Option<ItemKind> {
        match self {
            ScopeId::Item(item) => Some(item),
            _ => None,
        }
    }

    pub fn root(top_mod: TopLevelMod) -> Self {
        Self::Item(top_mod.into())
    }

    /// Returns the scope graph containing this scope.
    pub fn scope_graph(self, db: &dyn HirDb) -> &ScopeGraph {
        self.top_mod(db).scope_graph(db)
    }

    pub fn edges(self, db: &dyn HirDb) -> impl Iterator<Item = &ScopeEdge> {
        self.scope_graph(db).edges(self)
    }

    /// Returns `true` if `scope` is reachable from `self` by following only
    /// lexical edges.
    pub fn is_lex_child(self, db: &dyn HirDb, scope: &ScopeId) -> bool {
        if self.top_mod(db) != scope.top_mod(db) {
            return false;
        }

        match self.lex_parent(db) {
            Some(lex_parent) => {
                if &lex_parent == scope {
                    return true;
                }
                lex_parent.is_lex_child(db, scope)
            }
            None => false,
        }
    }

    /// Returns true if `self` is a transitive reflexive child of `of`.
    pub fn is_transitive_child_of(self, db: &dyn HirDb, of: ScopeId) -> bool {
        let mut current = Some(self);

        while let Some(scope) = current {
            if scope == of {
                return true;
            }
            current = scope.parent(db);
        }

        false
    }

    /// Return the `IngotId` containing the scope.
    pub fn ingot(self, db: &dyn HirDb) -> IngotId {
        self.top_mod(db).ingot(db)
    }

    pub fn data(self, db: &dyn HirDb) -> &Scope {
        self.top_mod(db).scope_graph(db).scope_data(&self)
    }

    pub fn parent(self, db: &dyn HirDb) -> Option<Self> {
        let mut super_dest = None;
        for edge in self.edges(db) {
            if let EdgeKind::Lex(_) = edge.kind {
                return Some(edge.dest);
            }
            if let EdgeKind::Super(_) = edge.kind {
                super_dest = Some(edge.dest);
            }
        }
        super_dest
    }

    pub fn lex_parent(self, db: &dyn HirDb) -> Option<Self> {
        self.data(db)
            .edges
            .iter()
            .find(|e| matches!(e.kind, EdgeKind::Lex(_)))
            .map(|e| e.dest)
    }

    pub fn parent_module(self, db: &dyn HirDb) -> Option<Self> {
        let parent_item = self.parent_item(db)?;
        match parent_item {
            ItemKind::Mod(_) | ItemKind::TopMod(_) => Some(Self::Item(parent_item)),
            _ => {
                let parent_id = Self::from_item(parent_item);
                parent_id.parent_module(db)
            }
        }
    }

    pub fn is_type(self, db: &dyn HirDb) -> bool {
        match self.data(db).id {
            ScopeId::Item(item) => item.is_type(),
            ScopeId::GenericParam(..) => true,
            _ => false,
        }
    }

    pub fn name(self, db: &dyn HirDb) -> Option<IdentId> {
        match self.data(db).id {
            ScopeId::Item(item) => item.name(db),

            ScopeId::Variant(parent, idx) => {
                let enum_: Enum = parent.try_into().unwrap();
                enum_.variants(db).data(db)[idx].name.to_opt()
            }

            ScopeId::Field(parent, idx) => match parent {
                ItemKind::Struct(s) => s.fields(db).data(db)[idx].name.to_opt(),
                ItemKind::Contract(c) => c.fields(db).data(db)[idx].name.to_opt(),
                _ => unreachable!(),
            },

            ScopeId::FuncParam(parent, idx) => {
                let func: Func = parent.try_into().unwrap();
                let param = &func.params(db).to_opt()?.data(db)[idx];
                if let Some(FuncParamLabel::Ident(ident)) = param.label {
                    Some(ident)
                } else {
                    param.name()
                }
            }

            ScopeId::GenericParam(parent, idx) => {
                let parent = GenericParamOwner::from_item_opt(parent).unwrap();

                let params = &parent.params(db).data(db)[idx];
                params.name().to_opt()
            }
        }
    }

    pub fn parent_item(self, db: &dyn HirDb) -> Option<ItemKind> {
        let data = self.data(db);
        match data.id {
            ScopeId::Item(item) => Some(item),
            _ => {
                let parent = data.parent_scope?;
                parent.parent_item(db)
            }
        }
    }

    pub fn name_span(self, db: &dyn HirDb) -> Option<DynLazySpan> {
        match self.data(db).id {
            ScopeId::Item(item) => item.name_span(),

            ScopeId::Variant(parent, idx) => {
                let enum_: Enum = parent.try_into().unwrap();
                Some(enum_.lazy_span().variants().variant(idx).name().into())
            }

            ScopeId::Field(parent, idx) => match parent {
                ItemKind::Struct(s) => Some(s.lazy_span().fields().field(idx).name().into()),
                ItemKind::Contract(c) => Some(c.lazy_span().fields().field(idx).name().into()),
                _ => unreachable!(),
            },

            ScopeId::FuncParam(parent, idx) => {
                let func: Func = parent.try_into().unwrap();
                let param = &func.params(db).to_opt()?.data(db)[idx];
                let param_span = func.lazy_span().params().param(idx);
                if let Some(FuncParamLabel::Ident(_)) = param.label {
                    Some(param_span.label().into())
                } else {
                    Some(param_span.name().into())
                }
            }

            ScopeId::GenericParam(parent, idx) => {
                let parent = GenericParamOwner::from_item_opt(parent).unwrap();

                Some(parent.params_span().param(idx).into())
            }
        }
    }

    pub fn pretty_path(self, db: &dyn HirDb) -> Option<String> {
        if let Some(parent) = self.parent(db) {
            let parent_path = parent.pretty_path(db)?;
            Some(format!("{}::{}", parent_path, self.name(db)?.data(db)))
        } else {
            self.name(db).map(|name| name.data(db))
        }
    }
}

struct ScopeGraphItemIterDfs<'a> {
    db: &'a dyn HirDb,
    graph: &'a ScopeGraph,
    visited: FxHashSet<ItemKind>,
    stack: Vec<ItemKind>,
}

impl<'a> std::iter::Iterator for ScopeGraphItemIterDfs<'a> {
    type Item = ItemKind;

    fn next(&mut self) -> Option<ItemKind> {
        let item = self.stack.pop()?;
        self.visited.insert(item);
        let scope_id = ScopeId::from_item(item);

        for edge in self.graph.edges(scope_id) {
            let top_mod = edge.dest.top_mod(self.db);
            if top_mod != self.graph.top_mod {
                continue;
            }
            if let Some(item) = edge.dest.to_item() {
                if !self.visited.contains(&item) {
                    self.stack.push(item);
                }
            }
        }
        Some(item)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    pub id: ScopeId,
    pub edges: BTreeSet<ScopeEdge>,
    pub parent_scope: Option<ScopeId>,
    pub vis: Visibility,
}

impl Scope {
    pub fn new(kind: ScopeId, parent_scope: Option<ScopeId>, vis: Visibility) -> Self {
        Self {
            id: kind,
            edges: Default::default(),
            parent_scope,
            vis,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeEdge {
    pub dest: ScopeId,
    pub kind: EdgeKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub enum EdgeKind {
    Lex(LexEdge),
    Mod(ModEdge),
    Type(TypeEdge),
    Trait(TraitEdge),
    GenericParam(GenericParamEdge),
    Value(ValueEdge),
    Field(FieldEdge),
    Variant(VariantEdge),
    Super(SuperEdge),
    Ingot(IngotEdge),
    Self_(SelfEdge),
    SelfTy(SelfTyEdge),
    Anon(AnonEdge),
}

impl EdgeKind {
    pub fn lex() -> Self {
        EdgeKind::Lex(LexEdge())
    }

    pub fn mod_(ident: IdentId) -> Self {
        EdgeKind::Mod(ident.into())
    }

    pub fn type_(ident: IdentId) -> Self {
        EdgeKind::Type(ident.into())
    }

    pub fn trait_(ident: IdentId) -> Self {
        EdgeKind::Trait(ident.into())
    }

    pub fn generic_param(ident: IdentId) -> Self {
        EdgeKind::GenericParam(ident.into())
    }

    pub fn value(ident: IdentId) -> Self {
        EdgeKind::Value(ident.into())
    }

    pub fn field(ident: IdentId) -> Self {
        EdgeKind::Field(ident.into())
    }

    pub fn variant(ident: IdentId) -> Self {
        EdgeKind::Variant(ident.into())
    }

    pub fn super_() -> Self {
        EdgeKind::Super(SuperEdge())
    }

    pub fn ingot() -> Self {
        EdgeKind::Ingot(IngotEdge())
    }

    pub fn self_ty() -> Self {
        EdgeKind::SelfTy(SelfTyEdge())
    }

    pub fn self_() -> Self {
        EdgeKind::Self_(SelfEdge())
    }

    pub fn anon() -> Self {
        EdgeKind::Anon(AnonEdge())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LexEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub struct ModEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub struct TypeEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub struct TraitEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub struct ValueEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub struct GenericParamEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub struct FieldEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub struct VariantEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SuperEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IngotEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub struct SelfTyEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub struct SelfEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AnonEdge();

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

        let file = db.standalone_file(text);
        let scope_graph = db.parse_source(file);
        assert_eq!(scope_graph.items_dfs(&db).count(), 8);

        for (i, item) in scope_graph.items_dfs(&db).enumerate() {
            match i {
                0 => assert!(matches!(item, ItemKind::TopMod(_))),
                1 => assert!(matches!(item, ItemKind::Enum(_))),
                2 => assert!(matches!(item, ItemKind::Mod(_))),
                3 => assert!(matches!(item, ItemKind::Struct(_))),
                4 => assert!(matches!(item, ItemKind::Mod(_))),
                5 => assert!(matches!(item, ItemKind::Func(_))),
                6 => assert!(matches!(item, ItemKind::Func(_))),
                7 => assert!(matches!(item, ItemKind::Body(_))),
                _ => unreachable!(),
            }
        }
    }
}
