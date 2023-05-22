use cranelift_entity::{entity_impl, PrimaryMap};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{hir_def::GenericParamOwner, span::DynLazySpan, HirDb};

use super::{Enum, Func, IdentId, IngotId, ItemKind, Struct, TopLevelMod, Use, Visibility};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeGraph {
    pub top_mod: TopLevelMod,
    pub scopes: PrimaryMap<LocalScopeId, LocalScope>,
    pub item_map: FxHashMap<ItemKind, LocalScopeId>,
    pub unresolved_uses: Vec<Use>,
}

impl ScopeGraph {
    pub fn items_dfs(&self) -> impl Iterator<Item = ItemKind> + '_ {
        ScopeGraphItemIterDfs {
            graph: self,
            visited: Default::default(),
            stack: vec![self.top_mod.into()],
        }
    }

    pub fn edges(&self, scope: LocalScopeId) -> &[ScopeEdge] {
        &self.scopes[scope].edges
    }

    pub fn scope_data(&self, scope: LocalScopeId) -> &LocalScope {
        &self.scopes[scope]
    }

    pub fn scope_item(&self, scope: LocalScopeId) -> Option<ItemKind> {
        if let ScopeKind::Item(item) = self.scope_data(scope).kind {
            Some(item)
        } else {
            None
        }
    }

    pub fn item_scope(&self, item: ItemKind) -> LocalScopeId {
        self.item_map[&item]
    }
}

struct ScopeGraphItemIterDfs<'a> {
    graph: &'a ScopeGraph,
    visited: FxHashSet<ItemKind>,
    stack: Vec<ItemKind>,
}

impl<'a> std::iter::Iterator for ScopeGraphItemIterDfs<'a> {
    type Item = ItemKind;

    fn next(&mut self) -> Option<ItemKind> {
        let item = self.stack.pop()?;
        self.visited.insert(item);
        let scope_id = self.graph.item_scope(item);

        for edge in self.graph.edges(scope_id) {
            let ScopeId { top_mod, local_id } = edge.dest;
            if top_mod != self.graph.top_mod {
                continue;
            }
            if let Some(item) = self.graph.scope_item(local_id) {
                if !self.visited.contains(&item) {
                    self.stack.push(item);
                }
            }
        }
        Some(item)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocalScope {
    pub kind: ScopeKind,
    pub edges: Vec<ScopeEdge>,
    pub parent_module: Option<ScopeId>,
    pub parent_scope: Option<LocalScopeId>,
    pub vis: Visibility,
}

impl LocalScope {
    pub fn new(
        kind: ScopeKind,
        parent_module: Option<ScopeId>,
        parent_scope: Option<LocalScopeId>,
        vis: Visibility,
    ) -> Self {
        Self {
            kind,
            edges: vec![],
            parent_module,
            parent_scope,
            vis,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKind {
    Item(ItemKind),
    GenericParam(usize),
    FnParam(usize),
    Field(usize),
    Variant(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ScopeEdge {
    pub dest: ScopeId,
    pub kind: EdgeKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId {
    top_mod: TopLevelMod,
    local_id: LocalScopeId,
}

impl ScopeId {
    pub fn new(top_mod: TopLevelMod, local_id: LocalScopeId) -> Self {
        Self { top_mod, local_id }
    }

    pub fn from_item(db: &dyn HirDb, item: ItemKind) -> Self {
        let top_mod = item.top_mod(db);
        let scope_graph = top_mod.scope_graph(db);
        Self::new(top_mod, scope_graph.item_scope(item))
    }

    pub fn root(top_mod: TopLevelMod) -> Self {
        Self::new(top_mod, LocalScopeId::root())
    }

    /// Returns the scope graph containing this scope.
    pub fn scope_graph(self, db: &dyn HirDb) -> &ScopeGraph {
        self.top_mod.scope_graph(db)
    }

    /// Returns the local id of the scope graph.
    pub fn to_local(self) -> LocalScopeId {
        self.local_id
    }

    pub fn edges(self, db: &dyn HirDb) -> &[ScopeEdge] {
        self.scope_graph(db).edges(self.local_id)
    }

    /// Returns `true` if `scope` is reachable from `self` by following only
    /// lexical edges.
    pub fn is_lex_child(self, db: &dyn HirDb, parent: ScopeId) -> bool {
        if self.top_mod != parent.top_mod {
            return false;
        }

        let scope_graph = self.scope_graph(db);
        self.local_id.is_lex_child(scope_graph, parent.local_id)
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

    /// Returns the `TopLevelMod` containing the scope .
    pub fn top_mod(self) -> TopLevelMod {
        self.top_mod
    }

    /// Return the `IngotId` containing the scope.
    pub fn ingot(self, db: &dyn HirDb) -> IngotId {
        self.top_mod.ingot(db)
    }

    pub fn data(self, db: &dyn HirDb) -> &LocalScope {
        self.top_mod.scope_graph(db).scope_data(self.local_id)
    }

    pub fn kind(self, db: &dyn HirDb) -> ScopeKind {
        self.data(db).kind
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
        self.data(db).parent_module
    }

    pub fn is_type(self, db: &dyn HirDb) -> bool {
        match self.data(db).kind {
            ScopeKind::Item(item) => item.is_type(),
            ScopeKind::GenericParam(_) => true,
            _ => false,
        }
    }

    pub fn name(self, db: &dyn HirDb) -> Option<IdentId> {
        let s_graph = self.top_mod.scope_graph(db);
        self.local_id.name(db, s_graph)
    }

    pub fn name_span(self, db: &dyn HirDb) -> Option<DynLazySpan> {
        let s_graph = self.top_mod.scope_graph(db);
        self.local_id.name_span(s_graph)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LexEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct ModEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct TypeEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct TraitEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct ValueEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct GenericParamEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct FieldEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct VariantEdge(pub IdentId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SuperEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IngotEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct SelfTyEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub struct SelfEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AnonEdge();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalScopeId(u32);
entity_impl!(LocalScopeId);

impl LocalScopeId {
    pub fn to_global(self, top_mod: TopLevelMod) -> ScopeId {
        ScopeId::new(top_mod, self)
    }

    /// Returns `true` if `scope` is reachable from `self` by following only
    /// lexical edges.
    pub fn is_lex_child(self, s_graph: &ScopeGraph, scope: LocalScopeId) -> bool {
        let data = self.data(s_graph);
        match data.parent_scope {
            Some(parent) => {
                if parent == scope {
                    return true;
                }
                parent.is_lex_child(s_graph, scope)
            }
            None => false,
        }
    }

    pub fn data(self, s_graph: &ScopeGraph) -> &LocalScope {
        &s_graph.scopes[self]
    }

    pub fn name(self, db: &dyn HirDb, s_graph: &ScopeGraph) -> Option<IdentId> {
        match self.data(s_graph).kind {
            ScopeKind::Item(item) => item.name(db),

            ScopeKind::Variant(idx) => {
                let parent: Enum = self.parent_item(s_graph).unwrap().try_into().unwrap();
                parent.variants(db).data(db)[idx].name.to_opt()
            }

            ScopeKind::Field(idx) => {
                let parent: Struct = self.parent_item(s_graph).unwrap().try_into().unwrap();
                parent.fields(db).data(db)[idx].name.to_opt()
            }

            ScopeKind::FnParam(idx) => {
                let parent: Func = self.parent_item(s_graph).unwrap().try_into().unwrap();
                parent.params(db).to_opt()?.data(db)[idx].name()
            }

            ScopeKind::GenericParam(idx) => {
                let parent =
                    GenericParamOwner::from_item_opt(self.parent_item(s_graph).unwrap()).unwrap();

                let params = &parent.params(db).data(db)[idx];
                params.name().to_opt()
            }
        }
    }

    pub fn name_span(self, s_graph: &ScopeGraph) -> Option<DynLazySpan> {
        match self.data(s_graph).kind {
            ScopeKind::Item(item) => item.name_span(),

            ScopeKind::Variant(idx) => {
                let parent: Enum = self.parent_item(s_graph).unwrap().try_into().unwrap();
                Some(parent.lazy_span().variants().variant(idx).name().into())
            }

            ScopeKind::Field(idx) => {
                let parent: Struct = self.parent_item(s_graph).unwrap().try_into().unwrap();
                Some(parent.lazy_span().fields().field(idx).name().into())
            }

            ScopeKind::FnParam(idx) => {
                let parent: Func = self.parent_item(s_graph).unwrap().try_into().unwrap();
                Some(parent.lazy_span().params().param(idx).name().into())
            }

            ScopeKind::GenericParam(idx) => {
                let parent =
                    GenericParamOwner::from_item_opt(self.parent_item(s_graph).unwrap()).unwrap();

                Some(parent.params_span().param(idx).into())
            }
        }
    }

    pub fn parent(self, s_graph: &ScopeGraph) -> Option<LocalScopeId> {
        self.data(s_graph).parent_scope
    }

    pub fn parent_item(self, s_graph: &ScopeGraph) -> Option<ItemKind> {
        match self.data(s_graph).kind {
            ScopeKind::Item(item) => Some(item),
            _ => {
                let parent = self.parent(s_graph)?;
                parent.parent_item(s_graph)
            }
        }
    }

    pub(crate) fn root() -> Self {
        LocalScopeId(0)
    }
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

        let scope_graph = db.parse_source(text);
        assert_eq!(scope_graph.items_dfs().count(), 8);

        for (i, item) in scope_graph.items_dfs().enumerate() {
            match i {
                0 => assert!(matches!(item, ItemKind::TopMod(_))),
                1 => assert!(matches!(item, ItemKind::Mod(_))),
                2 => assert!(matches!(item, ItemKind::Struct(_))),
                3 => assert!(matches!(item, ItemKind::Enum(_))),
                4 => assert!(matches!(item, ItemKind::Mod(_))),
                5 => assert!(matches!(item, ItemKind::Func(_))),
                6 => assert!(matches!(item, ItemKind::Func(_))),
                7 => assert!(matches!(item, ItemKind::Body(_))),
                _ => unreachable!(),
            }
        }
    }
}
