// This is necessary because `salsa::tracked` structs generates a
// constructor
// that may take many arguments depending on the number of fields in the struct.
#![allow(clippy::too_many_arguments)]

use common::InputFile;
use parser::ast;

use crate::{
    hir_def::TraitRef,
    lower,
    span::{
        item::{
            LazyConstSpan, LazyContractSpan, LazyEnumSpan, LazyFuncSpan, LazyImplSpan,
            LazyImplTraitSpan, LazyModSpan, LazyStructSpan, LazyTopLevelModSpan, LazyTraitSpan,
            LazyTypeAliasSpan, LazyUseSpan,
        },
        params::LazyGenericParamListSpan,
        DynLazySpan, HirOrigin,
    },
    HirDb,
};

use super::{
    scope_graph::{ScopeGraph, ScopeId},
    AttrListId, Body, FnParamListId, GenericParamListId, IdentId, IngotId, Partial, TypeId,
    UseAlias, WhereClauseId,
};

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    derive_more::From,
    derive_more::TryInto,
)]
pub enum ItemKind {
    TopMod(TopLevelMod),
    Mod(Mod),
    Func(Func),
    Struct(Struct),
    Contract(Contract),
    Enum(Enum),
    TypeAlias(TypeAlias),
    Impl(Impl),
    Trait(Trait),
    ImplTrait(ImplTrait),
    Const(Const),
    Use(Use),
    /// Body is not an `Item`, but this makes it easier for analyzers to handle
    /// it.
    Body(Body),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, derive_more::From)]
pub enum GenericParamOwner {
    Func(Func),
    Struct(Struct),
    Enum(Enum),
    TypeAlias(TypeAlias),
    Impl(Impl),
    Trait(Trait),
    ImplTrait(ImplTrait),
}

impl GenericParamOwner {
    pub fn top_mod(&self, db: &dyn HirDb) -> TopLevelMod {
        match self {
            GenericParamOwner::Func(func) => func.top_mod(db),
            GenericParamOwner::Struct(struct_) => struct_.top_mod(db),
            GenericParamOwner::Enum(enum_) => enum_.top_mod(db),
            GenericParamOwner::TypeAlias(type_alias) => type_alias.top_mod(db),
            GenericParamOwner::Impl(impl_) => impl_.top_mod(db),
            GenericParamOwner::Trait(trait_) => trait_.top_mod(db),
            GenericParamOwner::ImplTrait(impl_trait) => impl_trait.top_mod(db),
        }
    }

    pub fn params(&self, db: &dyn HirDb) -> GenericParamListId {
        match self {
            GenericParamOwner::Func(func) => func.generic_params(db),
            GenericParamOwner::Struct(struct_) => struct_.generic_params(db),
            GenericParamOwner::Enum(enum_) => enum_.generic_params(db),
            GenericParamOwner::TypeAlias(type_alias) => type_alias.generic_params(db),
            GenericParamOwner::Impl(impl_) => impl_.generic_params(db),
            GenericParamOwner::Trait(trait_) => trait_.generic_params(db),
            GenericParamOwner::ImplTrait(impl_trait) => impl_trait.generic_params(db),
        }
    }

    pub fn params_span(&self) -> LazyGenericParamListSpan {
        match self {
            GenericParamOwner::Func(func) => func.lazy_span().generic_params(),
            GenericParamOwner::Struct(struct_) => struct_.lazy_span().generic_params(),
            GenericParamOwner::Enum(enum_) => enum_.lazy_span().generic_params(),
            GenericParamOwner::TypeAlias(type_alias) => type_alias.lazy_span().generic_params(),
            GenericParamOwner::Impl(impl_) => impl_.lazy_span().generic_params(),
            GenericParamOwner::Trait(trait_) => trait_.lazy_span().generic_params(),
            GenericParamOwner::ImplTrait(impl_trait) => impl_trait.lazy_span().generic_params(),
        }
    }

    pub fn from_item_opt(item: ItemKind) -> Option<Self> {
        match item {
            ItemKind::Func(func) => Some(GenericParamOwner::Func(func)),
            ItemKind::Struct(struct_) => Some(GenericParamOwner::Struct(struct_)),
            ItemKind::Enum(enum_) => Some(GenericParamOwner::Enum(enum_)),
            ItemKind::TypeAlias(type_alias) => Some(GenericParamOwner::TypeAlias(type_alias)),
            ItemKind::Impl(impl_) => Some(GenericParamOwner::Impl(impl_)),
            ItemKind::Trait(trait_) => Some(GenericParamOwner::Trait(trait_)),
            ItemKind::ImplTrait(impl_trait) => Some(GenericParamOwner::ImplTrait(impl_trait)),
            _ => None,
        }
    }
}

impl ItemKind {
    pub fn name(self, db: &dyn HirDb) -> Option<IdentId> {
        use ItemKind::*;
        match self {
            TopMod(top_mod) => Some(top_mod.name(db)),
            Mod(mod_) => mod_.name(db).to_opt(),
            Func(func_) => func_.name(db).to_opt(),
            Struct(struct_) => struct_.name(db).to_opt(),
            Contract(contract_) => contract_.name(db).to_opt(),
            Enum(enum_) => enum_.name(db).to_opt(),
            TypeAlias(alias) => alias.name(db).to_opt(),
            Trait(trait_) => trait_.name(db).to_opt(),
            Const(const_) => const_.name(db).to_opt(),
            Use(_) | Body(_) | Impl(_) | ImplTrait(_) => None,
        }
    }

    pub fn name_span(self) -> Option<DynLazySpan> {
        use ItemKind::*;
        match self {
            Mod(mod_) => Some(mod_.lazy_span().name().into()),
            Func(func_) => Some(func_.lazy_span().name().into()),
            Struct(struct_) => Some(struct_.lazy_span().name().into()),
            Contract(contract_) => Some(contract_.lazy_span().name().into()),
            Enum(enum_) => Some(enum_.lazy_span().name().into()),
            TypeAlias(alias) => Some(alias.lazy_span().alias().into()),
            Trait(trait_) => Some(trait_.lazy_span().name().into()),
            Const(const_) => Some(const_.lazy_span().name().into()),
            TopMod(_) | Use(_) | Body(_) | Impl(_) | ImplTrait(_) => None,
        }
    }

    pub fn vis(self, db: &dyn HirDb) -> Visibility {
        use ItemKind::*;
        match self {
            TopMod(top_mod) => top_mod.vis(db),
            Mod(mod_) => mod_.vis(db),
            Func(func) => func.vis(db),
            Struct(struct_) => struct_.vis(db),
            Contract(contract) => contract.vis(db),
            Enum(enum_) => enum_.vis(db),
            TypeAlias(type_) => type_.vis(db),
            Trait(trait_) => trait_.vis(db),
            Const(const_) => const_.vis(db),
            Use(use_) => use_.vis(db),
            Impl(_) | ImplTrait(_) | Body(_) => Visibility::Private,
        }
    }

    pub fn ingot(self, db: &dyn HirDb) -> IngotId {
        let top_mod = self.top_mod(db);
        top_mod.ingot(db)
    }

    pub fn top_mod(self, db: &dyn HirDb) -> TopLevelMod {
        match self {
            ItemKind::TopMod(top_mod) => top_mod,
            ItemKind::Mod(mod_) => mod_.top_mod(db),
            ItemKind::Func(func) => func.top_mod(db),
            ItemKind::Struct(struct_) => struct_.top_mod(db),
            ItemKind::Contract(contract) => contract.top_mod(db),
            ItemKind::Enum(enum_) => enum_.top_mod(db),
            ItemKind::TypeAlias(type_) => type_.top_mod(db),
            ItemKind::Trait(trait_) => trait_.top_mod(db),
            ItemKind::Impl(impl_) => impl_.top_mod(db),
            ItemKind::ImplTrait(impl_trait) => impl_trait.top_mod(db),
            ItemKind::Const(const_) => const_.top_mod(db),
            ItemKind::Use(use_) => use_.top_mod(db),
            ItemKind::Body(body) => body.top_mod(db),
        }
    }

    pub fn is_type(self) -> bool {
        matches!(
            self,
            Self::Struct(_) | Self::Enum(_) | Self::Contract(_) | Self::TypeAlias(_)
        )
    }
}

#[salsa::tracked]
pub struct TopLevelMod {
    // No #[id] here, because `TopLevelMod` is always unique to a `InputFile` that is an argument
    // of `module_scope_graph`.
    pub name: IdentId,

    pub ingot: IngotId,
    pub(crate) file: InputFile,
}
impl TopLevelMod {
    pub fn lazy_span(self) -> LazyTopLevelModSpan {
        LazyTopLevelModSpan::new(self)
    }

    pub fn scope_graph(self, db: &dyn HirDb) -> &ScopeGraph {
        lower::scope_graph_impl(db, self)
    }

    /// Returns the child top level modules of `self`.
    pub fn child_top_mods(self, db: &dyn HirDb) -> impl Iterator<Item = TopLevelMod> + '_ {
        let module_tree = self.ingot(db).module_tree(db);
        module_tree.children(self)
    }

    /// Returns the top level children of this module.
    /// If you need all the children, use [`children_nested`] instead.
    pub fn children_non_nested(self, db: &dyn HirDb) -> impl Iterator<Item = ItemKind> + '_ {
        let s_graph = self.scope_graph(db);
        let scope = ScopeId::from_item(self.into());
        s_graph.child_items(scope)
    }

    /// Returns all the children of this module, including nested items.
    pub fn children_nested(self, db: &dyn HirDb) -> impl Iterator<Item = ItemKind> + '_ {
        let s_graph = self.scope_graph(db);
        s_graph.items_dfs(db)
    }

    pub fn parent(self, db: &dyn HirDb) -> Option<TopLevelMod> {
        let module_tree = self.ingot(db).module_tree(db);
        module_tree.parent(self)
    }

    pub fn vis(self, _db: &dyn HirDb) -> Visibility {
        // We don't have a way to specify visibility of a top level module.
        // Please change here if we introduce it.
        Visibility::Public
    }
}

#[salsa::tracked]
pub struct Mod {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub attributes: AttrListId,
    pub vis: Visibility,

    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Mod>,
}
impl Mod {
    pub fn lazy_span(self) -> LazyModSpan {
        LazyModSpan::new(self)
    }

    pub fn children_non_nested(self, db: &dyn HirDb) -> impl Iterator<Item = ItemKind> + '_ {
        let s_graph = self.top_mod(db).scope_graph(db);
        let scope = ScopeId::from_item(self.into());
        s_graph.child_items(scope)
    }
}

#[salsa::tracked]
pub struct Func {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub attributes: AttrListId,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,
    pub params: Partial<FnParamListId>,
    pub ret_ty: Option<TypeId>,
    pub modifier: ItemModifier,
    pub body: Option<Body>,
    pub is_extern: bool,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Fn>,
}
impl Func {
    pub fn lazy_span(self) -> LazyFuncSpan {
        LazyFuncSpan::new(self)
    }

    pub fn vis(self, db: &dyn HirDb) -> Visibility {
        self.modifier(db).to_visibility()
    }
}

#[salsa::tracked]
pub struct Struct {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub attributes: AttrListId,
    pub vis: Visibility,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,
    pub fields: RecordFieldListId,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Struct>,
}
impl Struct {
    pub fn lazy_span(self) -> LazyStructSpan {
        LazyStructSpan::new(self)
    }
}

#[salsa::tracked]
pub struct Contract {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub attributes: AttrListId,
    pub vis: Visibility,
    pub fields: RecordFieldListId,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Contract>,
}
impl Contract {
    pub fn lazy_span(self) -> LazyContractSpan {
        LazyContractSpan::new(self)
    }
}

#[salsa::tracked]
pub struct Enum {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub attributes: AttrListId,
    pub vis: Visibility,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,
    pub variants: EnumVariantListId,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Enum>,
}
impl Enum {
    pub fn lazy_span(self) -> LazyEnumSpan {
        LazyEnumSpan::new(self)
    }
}

#[salsa::tracked]
pub struct TypeAlias {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub attributes: AttrListId,
    pub vis: Visibility,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,
    pub ty: Partial<TypeId>,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::TypeAlias>,
}
impl TypeAlias {
    pub fn lazy_span(self) -> LazyTypeAliasSpan {
        LazyTypeAliasSpan::new(self)
    }
}

#[salsa::tracked]
pub struct Impl {
    #[id]
    id: TrackedItemId,

    pub ty: super::Partial<TypeId>,
    pub attributes: AttrListId,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Impl>,
}
impl Impl {
    pub fn lazy_span(self) -> LazyImplSpan {
        LazyImplSpan::new(self)
    }
}

#[salsa::tracked]
pub struct Trait {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,

    pub attributes: AttrListId,
    pub vis: Visibility,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Trait>,
}
impl Trait {
    pub fn lazy_span(self) -> LazyTraitSpan {
        LazyTraitSpan::new(self)
    }
}

#[salsa::tracked]
pub struct ImplTrait {
    #[id]
    id: TrackedItemId,

    pub trait_ref: Partial<TraitRef>,
    pub ty: Partial<TypeId>,
    pub attributes: AttrListId,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::ImplTrait>,
}
impl ImplTrait {
    pub fn lazy_span(self) -> LazyImplTraitSpan {
        LazyImplTraitSpan::new(self)
    }
}

#[salsa::tracked]
pub struct Const {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub body: Partial<Body>,
    pub vis: Visibility,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Const>,
}
impl Const {
    pub fn lazy_span(self) -> LazyConstSpan {
        LazyConstSpan::new(self)
    }
}

#[salsa::tracked]
pub struct Use {
    #[id]
    id: TrackedItemId,

    pub path: Partial<super::UsePathId>,
    pub alias: Option<Partial<UseAlias>>,
    pub vis: Visibility,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Use>,
}
impl Use {
    pub fn lazy_span(self) -> LazyUseSpan {
        LazyUseSpan::new(self)
    }

    /// Returns imported name if it is present and not a glob.
    pub fn imported_name(&self, db: &dyn HirDb) -> Option<IdentId> {
        if let Some(alias) = self.alias(db) {
            return match alias {
                Partial::Present(UseAlias::Ident(name)) => Some(name),
                _ => None,
            };
        }

        self.path(db).to_opt()?.last_ident(db)
    }

    /// Returns the span of imported name span if the use is not a glob.
    /// The returned span is
    /// 1. If the use has an alias, the span of the alias.
    /// 2. If the use has no alias, the span of the last segment of the path.
    pub fn imported_name_span(&self, db: &dyn HirDb) -> Option<DynLazySpan> {
        if self.is_glob(db) {
            return None;
        }

        if self.alias(db).is_some() {
            Some(self.lazy_span().alias().into())
        } else {
            let segment_len = self.path(db).to_opt()?.segment_len(db);
            Some(self.lazy_span().path().segment(segment_len - 1).into())
        }
    }

    pub fn glob_span(&self, db: &dyn HirDb) -> Option<DynLazySpan> {
        if !self.is_glob(db) {
            return None;
        }

        let segment_len = self.path(db).to_opt()?.segment_len(db);
        Some(self.lazy_span().path().segment(segment_len - 1).into())
    }

    pub fn is_glob(&self, db: &dyn HirDb) -> bool {
        self.path(db)
            .to_opt()
            .map_or(false, |path| path.is_glob(db))
    }

    pub fn is_unnamed(&self, db: &dyn HirDb) -> bool {
        if let Some(alias) = self.alias(db) {
            !matches!(alias, Partial::Present(UseAlias::Ident(_)))
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemModifier {
    Pub,
    Unsafe,
    PubAndUnsafe,
    None,
}

impl ItemModifier {
    pub fn to_visibility(self) -> Visibility {
        match self {
            ItemModifier::Pub | ItemModifier::PubAndUnsafe => Visibility::Public,
            ItemModifier::Unsafe | ItemModifier::None => Visibility::Private,
        }
    }
}

#[salsa::interned]
pub struct RecordFieldListId {
    #[return_ref]
    pub data: Vec<RecordField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordField {
    pub name: Partial<IdentId>,
    pub ty: Partial<TypeId>,
    pub vis: Visibility,
}

#[salsa::interned]
pub struct EnumVariantListId {
    #[return_ref]
    pub data: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub name: Partial<IdentId>,
    pub ty: Option<TypeId>,
}

#[salsa::interned]
pub struct ImplItemListId {
    #[return_ref]
    pub items: Vec<Func>,
}

pub type TraitItemListId = ImplItemListId;
pub type ImplTraitItemListId = ImplItemListId;
pub type ExternItemListId = ImplItemListId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Visibility {
    Public,
    Private,
}

impl Visibility {
    pub fn is_pub(self) -> bool {
        self == Self::Public
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TrackedItemId {
    TopLevelMod(IdentId),
    Mod(Partial<IdentId>),
    Fn(Partial<IdentId>),
    Struct(Partial<IdentId>),
    Contract(Partial<IdentId>),
    Enum(Partial<IdentId>),
    TypeAlias(Partial<IdentId>),
    Impl(Partial<TypeId>),
    Trait(Partial<IdentId>),
    ImplTrait(Partial<TraitRef>, Partial<TypeId>),
    Const(Partial<IdentId>),
    Use(Partial<super::UsePathId>),
    Extern,
    Joined(Box<Self>, Box<Self>),
}

impl TrackedItemId {
    pub(crate) fn join(self, rhs: Self) -> Self {
        Self::Joined(self.into(), rhs.into())
    }
}
