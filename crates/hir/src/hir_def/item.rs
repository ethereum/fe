// This is necessary because `salsa::tracked` structs generates a
// constructor
// that may take many arguments depending on the number of fields in the struct.
#![allow(clippy::too_many_arguments)]

use common::InputFile;
use parser::ast;

use super::{
    kw,
    scope_graph::{ScopeGraph, ScopeId},
    AttrListId, Body, FuncParamListId, GenericParamListId, IdentId, IngotId, Partial, TupleTypeId,
    TypeId, UseAlias, WhereClauseId,
};
use crate::{
    hir_def::TraitRefId,
    lower,
    span::{
        item::{
            LazyConstSpan, LazyContractSpan, LazyEnumSpan, LazyFuncSpan, LazyImplSpan,
            LazyImplTraitSpan, LazyItemSpan, LazyModSpan, LazyStructSpan, LazyTopModSpan,
            LazyTraitSpan, LazyTypeAliasSpan, LazyUseSpan,
        },
        params::{LazyGenericParamListSpan, LazyWhereClauseSpan},
        DynLazySpan, HirOrigin,
    },
    HirDb,
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

impl ItemKind {
    pub fn lazy_span(self) -> LazyItemSpan {
        LazyItemSpan::new(self)
    }

    pub fn scope(self) -> ScopeId {
        ScopeId::from_item(self)
    }

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

    pub fn kind_name(self) -> &'static str {
        use ItemKind::*;
        match self {
            TopMod(_) => "mod",
            Mod(_) => "mod",
            Func(_) => "fn",
            Struct(_) => "struct",
            Contract(_) => "contract",
            Enum(_) => "enum",
            TypeAlias(_) => "type",
            Trait(_) => "trait",
            Impl(_) => "impl",
            ImplTrait(_) => "impl trait",
            Const(_) => "const",
            Use(_) => "use",
            Body(_) => "body",
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

    pub fn is_trait(self) -> bool {
        matches!(self, Self::Trait(_))
    }
}

impl From<GenericParamOwner> for ItemKind {
    fn from(owner: GenericParamOwner) -> Self {
        match owner {
            GenericParamOwner::Func(func) => ItemKind::Func(func),
            GenericParamOwner::Struct(struct_) => ItemKind::Struct(struct_),
            GenericParamOwner::Enum(enum_) => ItemKind::Enum(enum_),
            GenericParamOwner::TypeAlias(type_alias) => ItemKind::TypeAlias(type_alias),
            GenericParamOwner::Impl(impl_) => ItemKind::Impl(impl_),
            GenericParamOwner::Trait(trait_) => ItemKind::Trait(trait_),
            GenericParamOwner::ImplTrait(impl_trait) => ItemKind::ImplTrait(impl_trait),
        }
    }
}

impl From<WhereClauseOwner> for ItemKind {
    fn from(owner: WhereClauseOwner) -> Self {
        match owner {
            WhereClauseOwner::Func(func) => ItemKind::Func(func),
            WhereClauseOwner::Struct(struct_) => ItemKind::Struct(struct_),
            WhereClauseOwner::Enum(enum_) => ItemKind::Enum(enum_),
            WhereClauseOwner::Impl(impl_) => ItemKind::Impl(impl_),
            WhereClauseOwner::Trait(trait_) => ItemKind::Trait(trait_),
            WhereClauseOwner::ImplTrait(impl_trait) => ItemKind::ImplTrait(impl_trait),
        }
    }
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
    pub fn top_mod(self, db: &dyn HirDb) -> TopLevelMod {
        ItemKind::from(self).top_mod(db)
    }

    pub fn params(self, db: &dyn HirDb) -> GenericParamListId {
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

    pub fn params_span(self) -> LazyGenericParamListSpan {
        match self {
            GenericParamOwner::Func(func) => func.lazy_span().generic_params_moved(),
            GenericParamOwner::Struct(struct_) => struct_.lazy_span().generic_params_moved(),
            GenericParamOwner::Enum(enum_) => enum_.lazy_span().generic_params_moved(),
            GenericParamOwner::TypeAlias(type_alias) => {
                type_alias.lazy_span().generic_params_moved()
            }
            GenericParamOwner::Impl(impl_) => impl_.lazy_span().generic_params_moved(),
            GenericParamOwner::Trait(trait_) => trait_.lazy_span().generic_params_moved(),
            GenericParamOwner::ImplTrait(impl_trait) => {
                impl_trait.lazy_span().generic_params_moved()
            }
        }
    }

    pub fn scope(self) -> ScopeId {
        ItemKind::from(self).scope()
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

    pub fn where_clause_owner(self) -> Option<WhereClauseOwner> {
        let item = ItemKind::from(self);
        WhereClauseOwner::from_item_opt(item)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, derive_more::From)]
pub enum WhereClauseOwner {
    Func(Func),
    Struct(Struct),
    Enum(Enum),
    Impl(Impl),
    Trait(Trait),
    ImplTrait(ImplTrait),
}

impl WhereClauseOwner {
    pub fn top_mod(self, db: &dyn HirDb) -> TopLevelMod {
        ItemKind::from(self).top_mod(db)
    }

    pub fn where_clause(self, db: &dyn HirDb) -> WhereClauseId {
        match self {
            Self::Func(func) => func.where_clause(db),
            Self::Struct(struct_) => struct_.where_clause(db),
            Self::Enum(enum_) => enum_.where_clause(db),
            Self::Impl(impl_) => impl_.where_clause(db),
            Self::Trait(trait_) => trait_.where_clause(db),
            Self::ImplTrait(impl_trait) => impl_trait.where_clause(db),
        }
    }

    pub fn where_clause_span(self) -> LazyWhereClauseSpan {
        match self {
            Self::Func(func) => func.lazy_span().where_clause_moved(),
            Self::Struct(struct_) => struct_.lazy_span().where_clause_moved(),
            Self::Enum(enum_) => enum_.lazy_span().where_clause_moved(),
            Self::Impl(impl_) => impl_.lazy_span().where_clause_moved(),
            Self::Trait(trait_) => trait_.lazy_span().where_clause_moved(),
            Self::ImplTrait(impl_trait) => impl_trait.lazy_span().where_clause_moved(),
        }
    }

    pub fn scope(self) -> ScopeId {
        ItemKind::from(self).scope()
    }

    pub fn from_item_opt(item: ItemKind) -> Option<Self> {
        match item {
            ItemKind::Func(func) => Some(Self::Func(func)),
            ItemKind::Struct(struct_) => Some(Self::Struct(struct_)),
            ItemKind::Enum(enum_) => Some(Self::Enum(enum_)),
            ItemKind::Impl(impl_) => Some(Self::Impl(impl_)),
            ItemKind::Trait(trait_) => Some(Self::Trait(trait_)),
            ItemKind::ImplTrait(impl_trait) => Some(Self::ImplTrait(impl_trait)),
            _ => None,
        }
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
    pub fn lazy_span(self) -> LazyTopModSpan {
        LazyTopModSpan::new(self)
    }

    pub fn scope(self) -> ScopeId {
        ScopeId::from_item(self.into())
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
    /// If you need all the children, use
    /// [`children_nested`](Self::children_nested) instead.
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

    /// Returns all items in the top level module including ones in nested
    /// modules.
    pub fn all_items(self, db: &dyn HirDb) -> &Vec<ItemKind> {
        all_items_in_top_mod(db, self)
    }

    /// Returns all structs in the top level module including ones in nested
    /// modules.
    pub fn all_structs(self, db: &dyn HirDb) -> &Vec<Struct> {
        all_structs_in_top_mod(db, self)
    }

    /// Returns all enums in the top level module including ones in nested
    /// modules.
    pub fn all_enums(self, db: &dyn HirDb) -> &Vec<Enum> {
        all_enums_in_top_mod(db, self)
    }

    /// Returns all contracts in the top level module including ones in nested
    /// modules.
    pub fn all_contracts(self, db: &dyn HirDb) -> &Vec<Contract> {
        all_contracts_in_top_mod(db, self)
    }

    /// Returns all type aliases in the top level module including ones in
    /// nested modules.
    pub fn all_type_aliases(self, db: &dyn HirDb) -> &Vec<TypeAlias> {
        all_type_aliases_in_top_mod(db, self)
    }

    /// Returns all traits in the top level module including ones in nested
    /// modules.
    pub fn all_traits(self, db: &dyn HirDb) -> &Vec<Trait> {
        all_traits_in_top_mod(db, self)
    }

    pub fn all_funcs(self, db: &dyn HirDb) -> &Vec<Func> {
        all_funcs_in_top_mod(db, self)
    }

    /// Returns all traits in the top level module including ones in nested
    /// modules.
    pub fn all_impl_traits(self, db: &dyn HirDb) -> &Vec<ImplTrait> {
        all_impl_trait_in_top_mod(db, self)
    }

    /// Returns all impls in the top level module including ones in nested
    /// modules.
    pub fn all_impls(self, db: &dyn HirDb) -> &Vec<Impl> {
        all_impl_in_top_mod(db, self)
    }
}

#[salsa::tracked(return_ref)]
pub fn all_top_modules_in_ingot(db: &dyn HirDb, ingot: IngotId) -> Vec<TopLevelMod> {
    let tree = ingot.module_tree(db);
    tree.all_modules().collect()
}

#[salsa::tracked(return_ref)]
pub fn all_impl_traits_in_ingot(db: &dyn HirDb, ingot: IngotId) -> Vec<ImplTrait> {
    ingot
        .all_modules(db)
        .iter()
        .flat_map(|top_mod| top_mod.all_impl_traits(db).iter().copied())
        .collect()
}

#[salsa::tracked(return_ref)]
pub fn all_impls_in_ingot(db: &dyn HirDb, ingot: IngotId) -> Vec<Impl> {
    ingot
        .all_modules(db)
        .iter()
        .flat_map(|top_mod| top_mod.all_impls(db).iter().copied())
        .collect()
}

#[salsa::tracked(return_ref)]
pub fn all_items_in_top_mod(db: &dyn HirDb, top_mod: TopLevelMod) -> Vec<ItemKind> {
    top_mod.children_nested(db).collect()
}

#[salsa::tracked(return_ref)]
pub fn all_structs_in_top_mod(db: &dyn HirDb, top_mod: TopLevelMod) -> Vec<Struct> {
    all_items_in_top_mod(db, top_mod)
        .iter()
        .filter_map(|item| match item {
            ItemKind::Struct(struct_) => Some(*struct_),
            _ => None,
        })
        .collect()
}

#[salsa::tracked(return_ref)]
pub fn all_enums_in_top_mod(db: &dyn HirDb, top_mod: TopLevelMod) -> Vec<Enum> {
    all_items_in_top_mod(db, top_mod)
        .iter()
        .filter_map(|item| match item {
            ItemKind::Enum(enum_) => Some(*enum_),
            _ => None,
        })
        .collect()
}

#[salsa::tracked(return_ref)]
pub fn all_type_aliases_in_top_mod(db: &dyn HirDb, top_mod: TopLevelMod) -> Vec<TypeAlias> {
    all_items_in_top_mod(db, top_mod)
        .iter()
        .filter_map(|item| match item {
            ItemKind::TypeAlias(alias) => Some(*alias),
            _ => None,
        })
        .collect()
}

#[salsa::tracked(return_ref)]
pub fn all_contracts_in_top_mod(db: &dyn HirDb, top_mod: TopLevelMod) -> Vec<Contract> {
    all_items_in_top_mod(db, top_mod)
        .iter()
        .filter_map(|item| match item {
            ItemKind::Contract(contract) => Some(*contract),
            _ => None,
        })
        .collect()
}

#[salsa::tracked(return_ref)]
pub fn all_traits_in_top_mod(db: &dyn HirDb, top_mod: TopLevelMod) -> Vec<Trait> {
    all_items_in_top_mod(db, top_mod)
        .iter()
        .filter_map(|item| match item {
            ItemKind::Trait(trait_) => Some(*trait_),
            _ => None,
        })
        .collect()
}

#[salsa::tracked(return_ref)]
pub fn all_funcs_in_top_mod(db: &dyn HirDb, top_mod: TopLevelMod) -> Vec<Func> {
    all_items_in_top_mod(db, top_mod)
        .iter()
        .filter_map(|item| match item {
            ItemKind::Func(func_) => Some(*func_),
            _ => None,
        })
        .collect()
}

#[salsa::tracked(return_ref)]
pub fn all_impl_in_top_mod(db: &dyn HirDb, top_mod: TopLevelMod) -> Vec<Impl> {
    all_items_in_top_mod(db, top_mod)
        .iter()
        .filter_map(|item| match item {
            ItemKind::Impl(impl_) => Some(*impl_),
            _ => None,
        })
        .collect()
}

#[salsa::tracked(return_ref)]
pub fn all_impl_trait_in_top_mod(db: &dyn HirDb, top_mod: TopLevelMod) -> Vec<ImplTrait> {
    all_items_in_top_mod(db, top_mod)
        .iter()
        .filter_map(|item| match item {
            ItemKind::ImplTrait(impl_trait) => Some(*impl_trait),
            _ => None,
        })
        .collect()
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

    pub fn scope(self) -> ScopeId {
        ScopeId::from_item(self.into())
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
    pub params: Partial<FuncParamListId>,
    pub ret_ty: Option<TypeId>,
    pub modifier: ItemModifier,
    pub body: Option<Body>,
    pub is_extern: bool,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Func>,
}
impl Func {
    pub fn lazy_span(self) -> LazyFuncSpan {
        LazyFuncSpan::new(self)
    }

    pub fn scope(self) -> ScopeId {
        ScopeId::from_item(self.into())
    }

    pub fn vis(self, db: &dyn HirDb) -> Visibility {
        self.modifier(db).to_visibility()
    }

    pub fn is_method(self, db: &dyn HirDb) -> bool {
        let Some(params) = self.params(db).to_opt() else {
            return false;
        };

        let Some(first_param) = params.data(db).first() else {
            return false;
        };

        first_param.name.to_opt().and_then(|name| name.ident()) == Some(kw::SELF)
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
    pub fields: FieldDefListId,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Struct>,
}
impl Struct {
    pub fn lazy_span(self) -> LazyStructSpan {
        LazyStructSpan::new(self)
    }

    pub fn scope(self) -> ScopeId {
        ScopeId::from_item(self.into())
    }
}

#[salsa::tracked]
pub struct Contract {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub attributes: AttrListId,
    pub vis: Visibility,
    pub fields: FieldDefListId,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Contract>,
}
impl Contract {
    pub fn lazy_span(self) -> LazyContractSpan {
        LazyContractSpan::new(self)
    }

    pub fn scope(self) -> ScopeId {
        ScopeId::from_item(self.into())
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
    pub variants: VariantDefListId,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Enum>,
}
impl Enum {
    pub fn lazy_span(self) -> LazyEnumSpan {
        LazyEnumSpan::new(self)
    }

    pub fn scope(self) -> ScopeId {
        ScopeId::from_item(self.into())
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
    pub ty: Partial<TypeId>,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::TypeAlias>,
}
impl TypeAlias {
    pub fn lazy_span(self) -> LazyTypeAliasSpan {
        LazyTypeAliasSpan::new(self)
    }

    pub fn scope(self) -> ScopeId {
        ScopeId::from_item(self.into())
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

    pub fn children_non_nested(self, db: &dyn HirDb) -> impl Iterator<Item = ItemKind> + '_ {
        let s_graph = self.top_mod(db).scope_graph(db);
        let scope = ScopeId::from_item(self.into());
        s_graph.child_items(scope)
    }

    pub fn funcs(self, db: &dyn HirDb) -> impl Iterator<Item = Func> + '_ {
        let s_graph = self.top_mod(db).scope_graph(db);
        let scope = ScopeId::from_item(self.into());
        s_graph.child_items(scope).filter_map(|item| match item {
            ItemKind::Func(func) => Some(func),
            _ => None,
        })
    }

    pub fn scope(self) -> ScopeId {
        ScopeId::from_item(self.into())
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
    #[return_ref]
    pub super_traits: Vec<TraitRefId>,
    pub where_clause: WhereClauseId,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Trait>,
}
impl Trait {
    pub fn lazy_span(self) -> LazyTraitSpan {
        LazyTraitSpan::new(self)
    }

    pub fn children_non_nested(self, db: &dyn HirDb) -> impl Iterator<Item = ItemKind> + '_ {
        let s_graph = self.top_mod(db).scope_graph(db);
        let scope = ScopeId::from_item(self.into());
        s_graph.child_items(scope)
    }

    pub fn scope(self) -> ScopeId {
        ScopeId::from_item(self.into())
    }

    pub fn methods(self, db: &dyn HirDb) -> impl Iterator<Item = Func> + '_ {
        let s_graph = self.top_mod(db).scope_graph(db);
        let scope = ScopeId::from_item(self.into());
        s_graph.child_items(scope).filter_map(|item| match item {
            ItemKind::Func(func) => Some(func),
            _ => None,
        })
    }
}

#[salsa::tracked]
pub struct ImplTrait {
    #[id]
    id: TrackedItemId,

    pub trait_ref: Partial<TraitRefId>,
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

    pub fn children_non_nested(self, db: &dyn HirDb) -> impl Iterator<Item = ItemKind> + '_ {
        let s_graph = self.top_mod(db).scope_graph(db);
        let scope = ScopeId::from_item(self.into());
        s_graph.child_items(scope)
    }

    pub fn scope(self) -> ScopeId {
        ScopeId::from_item(self.into())
    }

    pub fn methods(self, db: &dyn HirDb) -> impl Iterator<Item = Func> + '_ {
        self.children_non_nested(db).filter_map(|item| match item {
            ItemKind::Func(func) => Some(func),
            _ => None,
        })
    }
}

#[salsa::tracked]
pub struct Const {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub ty: Partial<TypeId>,
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

    pub fn scope(self) -> ScopeId {
        ScopeId::from_item(self.into())
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

    pub fn scope(self) -> ScopeId {
        ScopeId::from_item(self.into())
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

    pub(crate) fn pretty_path(&self, db: &dyn HirDb) -> String {
        self.path(db)
            .to_opt()
            .map_or_else(|| "{invalid}".to_string(), |path| path.pretty_path(db))
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
pub struct FieldDefListId {
    #[return_ref]
    pub data: Vec<FieldDef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldDef {
    pub name: Partial<IdentId>,
    pub ty: Partial<TypeId>,
    pub vis: Visibility,
}

#[salsa::interned]
pub struct VariantDefListId {
    #[return_ref]
    pub data: Vec<VariantDef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariantDef {
    pub name: Partial<IdentId>,
    pub kind: VariantKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VariantKind {
    Unit,
    Tuple(TupleTypeId),
    Record(FieldDefListId),
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
    Func(Partial<IdentId>),
    Struct(Partial<IdentId>),
    Contract(Partial<IdentId>),
    Enum(Partial<IdentId>),
    TypeAlias(Partial<IdentId>),
    Impl(Partial<TypeId>),
    Trait(Partial<IdentId>),
    ImplTrait(Partial<TraitRefId>, Partial<TypeId>),
    Const(Partial<IdentId>),
    Use(Partial<super::UsePathId>),
    Extern,
    FuncBody,
    NamelessBody,
    Joined(Box<Self>, Box<Self>),
}

impl TrackedItemId {
    pub(crate) fn join(self, rhs: Self) -> Self {
        Self::Joined(self.into(), rhs.into())
    }
}
