use cranelift_entity::{entity_impl, PrimaryMap};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{span::DynLazySpan, HirDb};

use super::{IdentId, ItemKind, TopLevelMod, Use, Visibility};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeGraph {
    pub top_mod: TopLevelMod,
    pub scopes: PrimaryMap<LocalScopeId, LocalScope>,
    pub item_map: FxHashMap<ItemKind, LocalScopeId>,
    pub unresolved_imports: FxHashMap<LocalScopeId, Vec<Use>>,
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
}

impl LocalScope {
    pub fn new(kind: ScopeKind, parent_module: Option<ScopeId>) -> Self {
        Self {
            kind,
            edges: vec![],
            parent_module,
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
    pub vis: Visibility,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId {
    pub top_mod: TopLevelMod,
    pub local_id: LocalScopeId,
}

impl ScopeId {
    pub fn span(self, _db: &dyn HirDb) -> DynLazySpan {
        todo!()
    }

    pub fn invalid() -> Self {
        Self {
            top_mod: TopLevelMod::invalid(),
            local_id: LocalScopeId::invalid(),
        }
    }

    pub fn is_valid(self) -> bool {
        self != Self::invalid()
    }
}

impl ScopeId {
    pub fn new(top_mod: TopLevelMod, local_id: LocalScopeId) -> Self {
        Self { top_mod, local_id }
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
    pub(crate) fn root() -> Self {
        LocalScopeId(0)
    }

    pub fn invalid() -> Self {
        LocalScopeId(u32::MAX)
    }

    pub fn is_valid(self) -> bool {
        self != Self::invalid()
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
