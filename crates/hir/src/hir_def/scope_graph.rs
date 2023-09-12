use std::{collections::BTreeSet, io};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    hir_def::{BodyKind, GenericParamOwner},
    span::DynLazySpan,
    HirDb,
};

use super::{
    scope_graph_viz::ScopeGraphFormatter, Body, Enum, ExprId, Func, FuncParamLabel, IdentId,
    IngotId, ItemKind, TopLevelMod, Use, VariantKind, Visibility,
};

/// Represents a scope relation graph in a top-level module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeGraph {
    /// The top-level module containing the scope graph.
    pub top_mod: TopLevelMod,
    /// ///
    /// The scopes in the graph.
    pub scopes: FxHashMap<ScopeId, Scope>,
    /// The all unresolved uses in the graph, this is used in name resolution.
    pub unresolved_uses: FxHashSet<Use>,
}

impl ScopeGraph {
    /// Represents all item scopes in a top-level module in depth-first order.
    pub fn items_dfs<'a>(&'a self, db: &'a dyn HirDb) -> impl Iterator<Item = ItemKind> + 'a {
        ScopeGraphItemIterDfs {
            db,
            graph: self,
            visited: Default::default(),
            stack: vec![self.top_mod.into()],
        }
    }

    /// Returns the direct child items of the given `scope`.
    pub fn child_items(&self, scope: ScopeId) -> impl Iterator<Item = ItemKind> + '_ {
        self.children(scope).filter_map(|child| child.to_item())
    }

    /// Returns the direct child scopes of the given `scope`
    pub fn children(&self, scope: ScopeId) -> impl Iterator<Item = ScopeId> + '_ {
        self.edges(scope).filter_map(|edge| match edge.kind {
            EdgeKind::Lex(_)
            | EdgeKind::Super(_)
            | EdgeKind::Ingot(_)
            | EdgeKind::SelfTy(_)
            | EdgeKind::Self_(_) => None,

            _ => Some(edge.dest),
        })
    }

    /// Returns the all edges outgoing from the given `scope`.
    pub fn edges(&self, scope: ScopeId) -> impl Iterator<Item = &ScopeEdge> + '_ {
        self.scopes[&scope].edges.iter()
    }

    /// Write a scope graph as a dot file format to given `w`.
    pub fn write_as_dot(&self, db: &dyn HirDb, w: &mut impl io::Write) -> io::Result<()> {
        ScopeGraphFormatter::new(db, self).render(w)
    }

    pub fn scope_data(&self, scope: &ScopeId) -> &Scope {
        &self.scopes[scope]
    }
}

/// An reference to a `[ScopeData]` in a `ScopeGraph`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScopeId {
    /// An item scope.
    Item(ItemKind),

    /// A generic parameter scope.
    GenericParam(ItemKind, usize),

    /// A function parameter scope.
    FuncParam(ItemKind, usize),

    /// A field scope.
    Field(FieldParent, usize),

    /// A variant scope.
    Variant(ItemKind, usize),

    /// A block scope.
    Block(Body, ExprId),
}
impl ScopeId {
    /// Returns the top level module containing this scope.
    pub fn top_mod(&self, db: &dyn HirDb) -> TopLevelMod {
        match self {
            ScopeId::Item(item) => item.top_mod(db),
            ScopeId::GenericParam(item, _) => item.top_mod(db),
            ScopeId::FuncParam(item, _) => item.top_mod(db),
            ScopeId::Field(FieldParent::Item(item), _) => item.top_mod(db),
            ScopeId::Field(FieldParent::Variant(item, _), _) => item.top_mod(db),
            ScopeId::Variant(item, _) => item.top_mod(db),
            ScopeId::Block(body, _) => body.top_mod(db),
        }
    }

    /// Convert an item to a scope id.
    pub fn from_item(item: ItemKind) -> Self {
        Self::Item(item)
    }

    /// Convert a scope id to an item if the scope is an item.
    pub fn to_item(self) -> Option<ItemKind> {
        match self {
            ScopeId::Item(item) => Some(item),
            _ => None,
        }
    }

    /// Returns the nearest item that contains this scope.
    /// If the scope is item itself, returns the item.
    pub fn item(self) -> ItemKind {
        match self {
            ScopeId::Item(item) => item,
            ScopeId::GenericParam(item, _) => item,
            ScopeId::FuncParam(item, _) => item,
            ScopeId::Field(FieldParent::Item(item), _) => item,
            ScopeId::Field(FieldParent::Variant(item, _), _) => item,
            ScopeId::Variant(item, _) => item,
            ScopeId::Block(body, _) => body.into(),
        }
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

    /// Returns the `Scope` data for this scope.
    pub fn data(self, db: &dyn HirDb) -> &Scope {
        self.top_mod(db).scope_graph(db).scope_data(&self)
    }

    /// Returns the parent scope of this scope.
    /// The parent scope is
    /// 1. the lexical parent if it exists
    /// 2. the parent module if 1. does not exist
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

    /// Returns the lexical parent scope of this scope.
    pub fn lex_parent(self, db: &dyn HirDb) -> Option<Self> {
        self.data(db)
            .edges
            .iter()
            .find(|e| matches!(e.kind, EdgeKind::Lex(_)))
            .map(|e| e.dest)
    }

    /// Returns the parent module of this scope.
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

    /// Returns `true` if the scope is a type.
    pub fn is_type(self) -> bool {
        match self {
            ScopeId::Item(item) => item.is_type(),
            ScopeId::GenericParam(..) => true,
            _ => false,
        }
    }

    pub fn is_enum(self) -> bool {
        matches!(self, ScopeId::Item(ItemKind::Enum(_)))
    }

    pub fn is_mod(self) -> bool {
        matches!(self, ScopeId::Item(ItemKind::Mod(_) | ItemKind::TopMod(_)))
    }

    /// Returns `true` if the scope is a trait definition.
    pub fn is_trait(self) -> bool {
        matches!(self, ScopeId::Item(ItemKind::Trait(_)))
    }

    /// Returns the item that contains this scope.
    pub fn parent_item(self, db: &dyn HirDb) -> Option<ItemKind> {
        let mut parent = self.parent(db)?;
        loop {
            match parent {
                ScopeId::Item(item) => return Some(item),
                _ => {
                    parent = parent.parent(db)?;
                }
            }
        }
    }

    pub fn name(self, db: &dyn HirDb) -> Option<IdentId> {
        match self.data(db).id {
            ScopeId::Item(item) => item.name(db),

            ScopeId::Variant(parent, idx) => {
                let enum_: Enum = parent.try_into().unwrap();
                enum_.variants(db).data(db)[idx].name.to_opt()
            }

            ScopeId::Field(FieldParent::Item(parent), idx) => match parent {
                ItemKind::Struct(s) => s.fields(db).data(db)[idx].name.to_opt(),
                ItemKind::Contract(c) => c.fields(db).data(db)[idx].name.to_opt(),
                _ => unreachable!(),
            },
            ScopeId::Field(FieldParent::Variant(parent, vidx), fidx) => {
                let enum_: Enum = parent.try_into().unwrap();
                match enum_.variants(db).data(db)[vidx].kind {
                    VariantKind::Record(fields) => fields.data(db)[fidx].name.to_opt(),
                    _ => unreachable!(),
                }
            }

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

            ScopeId::Block(..) => None,
        }
    }

    pub fn name_span(self, db: &dyn HirDb) -> Option<DynLazySpan> {
        match self.data(db).id {
            ScopeId::Item(item) => item.name_span(),

            ScopeId::Variant(parent, idx) => {
                let enum_: Enum = parent.try_into().unwrap();
                Some(enum_.lazy_span().variants().variant(idx).name().into())
            }

            ScopeId::Field(FieldParent::Item(parent), idx) => match parent {
                ItemKind::Struct(s) => Some(s.lazy_span().fields().field(idx).name().into()),
                ItemKind::Contract(c) => Some(c.lazy_span().fields().field(idx).name().into()),
                _ => unreachable!(),
            },
            ScopeId::Field(FieldParent::Variant(parent, vidx), fidx) => {
                let enum_: Enum = parent.try_into().unwrap();
                Some(
                    enum_
                        .lazy_span()
                        .variants()
                        .variant(vidx)
                        .fields()
                        .field(fidx)
                        .name()
                        .into(),
                )
            }

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

            ScopeId::Block(..) => None,
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            ScopeId::Item(item) => item.kind_name(),
            ScopeId::GenericParam(_, _) => "type",
            ScopeId::FuncParam(_, _) => "value",
            ScopeId::Field(_, _) => "field",
            ScopeId::Variant(_, _) => "value",
            ScopeId::Block(_, _) => "block",
        }
    }
    pub fn pretty_path(self, db: &dyn HirDb) -> Option<String> {
        let name = match self {
            ScopeId::Block(body, expr) => format!("{{block{}}}", body.block_order(db)[&expr]),
            ScopeId::Item(ItemKind::Body(body)) => match body.body_kind(db) {
                BodyKind::FuncBody => "{fn_body}".to_string(),
                BodyKind::Anonymous => "{anonymous_body}".to_string(),
            },
            _ => self.name(db)?.data(db).clone(),
        };

        if let Some(parent) = self.parent(db) {
            let parent_path = parent.pretty_path(db)?;
            Some(format!("{}::{}", parent_path, name))
        } else {
            Some(name)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FieldParent {
    Item(ItemKind),
    Variant(ItemKind, usize),
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
    pub vis: Visibility,
}

impl Scope {
    pub fn new(kind: ScopeId, vis: Visibility) -> Self {
        Self {
            id: kind,
            edges: Default::default(),
            vis,
        }
    }
}

/// An edge of the scope graph.
/// The edge contains the destination of the edge and the kind of the edge.
/// [`EdgeKind`] is contains supplementary information about the destination
/// scope, which is used for name resolution.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeEdge {
    pub dest: ScopeId,
    pub kind: EdgeKind,
}

/// A specific edge property definitions.
///
/// NOTE: The internal types of each variants contains very small amount of
/// information, the reason why we need to prepare each internal types is to
/// allow us to implement traits to each edges directly.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub enum EdgeKind {
    /// An edge to a lexical parent scope.
    Lex(LexEdge),
    /// An edge to a module.
    Mod(ModEdge),
    /// An edge to a type.
    Type(TypeEdge),
    /// An edge to a trait.
    Trait(TraitEdge),
    /// An edge from a scope to a generic parameter.
    GenericParam(GenericParamEdge),
    /// An edge to a value. The value is either a function or a
    /// constant.
    Value(ValueEdge),
    /// An edge to a field definition scope.
    Field(FieldEdge),
    /// An edge to a enum variant definition scope.
    Variant(VariantEdge),
    /// An edge to a module that is referenced by a `super` keyword.
    Super(SuperEdge),
    /// An edge to an ingot that is referenced by a `ingot` keyword.
    Ingot(IngotEdge),
    /// An edge to a scope that is referenced by a `self` keyword.
    Self_(SelfEdge),
    /// An edge to a scope that is referenced by a `Self` keyword.
    SelfTy(SelfTyEdge),
    /// An edge to an anonymous scope, e.g., `impl` or function body.
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

    use crate::{
        hir_def::{
            scope_graph::{FieldParent, ScopeId},
            ItemKind,
        },
        test_db::TestDb,
    };

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

    #[test]
    fn enum_record_fields() {
        let mut db = TestDb::default();

        let text = r#"
            enum Foo {
                X { a: i8, b: i8 },
            }
        "#;

        let file = db.standalone_file(text);
        let scope_graph = db.parse_source(file);
        let root = scope_graph.top_mod.scope();
        let enum_ = scope_graph.children(root).next().unwrap();
        assert!(matches!(enum_.item(), ItemKind::Enum(_)));

        let variant = scope_graph.children(enum_).next().unwrap();
        assert!(matches!(variant, ScopeId::Variant(_, _)));

        let field = scope_graph.children(variant).next().unwrap();
        assert!(matches!(
            field,
            ScopeId::Field(FieldParent::Variant(_, _), _)
        ));
    }
}
