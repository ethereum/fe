// This is necessary because `salsa::tracked` structs generates a
// constructor
// that may take many arguments depending on the number of fields in the struct.
#![allow(clippy::too_many_arguments)]

use common::{InputFile, InputIngot};
use parser::ast;

use crate::{
    hir_def::TraitRef,
    lower,
    span::{
        item::{
            LazyConstSpan, LazyContractSpan, LazyEnumSpan, LazyExternFuncSpan, LazyFuncSpan,
            LazyImplSpan, LazyImplTraitSpan, LazyModSpan, LazyStructSpan, LazyTopLevelModSpan,
            LazyTraitSpan, LazyTypeAliasSpan, LazyUseSpan,
        },
        HirOrigin,
    },
    HirDb,
};

use super::{
    ingot_module_tree_impl, AttrListId, Body, FnParamListId, GenericParamListId, IdentId,
    IngotModuleTree, ItemTree, Partial, TypeId, WhereClauseId,
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
    ExternFunc(ExternFunc),
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

#[salsa::tracked]
pub struct TopLevelMod {
    // No #[id] here, because `TopLevelMod` is always unique to a `InputFile` that is an argument
    // of `module_item_tree`.
    pub name: IdentId,

    pub(crate) ingot: InputIngot,
    pub(crate) file: InputFile,
}
impl TopLevelMod {
    pub fn lazy_span(self) -> LazyTopLevelModSpan {
        LazyTopLevelModSpan::new(self)
    }

    pub fn module_item_tree(self, db: &dyn HirDb) -> &ItemTree {
        lower::module_item_tree_impl(db, self)
    }

    pub fn ingot_module_tree(self, db: &dyn HirDb) -> &IngotModuleTree {
        ingot_module_tree_impl(db, self.ingot(db))
    }
}

#[salsa::tracked]
pub struct Mod {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub attributes: AttrListId,
    pub is_pub: bool,

    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Mod>,
}
impl Mod {
    pub fn lazy_span(self) -> LazyModSpan {
        LazyModSpan::new(self)
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
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Fn>,
}
impl Func {
    pub fn lazy_span(self) -> LazyFuncSpan {
        LazyFuncSpan::new(self)
    }
}

#[salsa::tracked]
pub struct ExternFunc {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub attributes: AttrListId,
    pub params: Partial<FnParamListId>,
    pub ret_ty: Option<TypeId>,
    pub modifier: ItemModifier,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Fn>,
}
impl ExternFunc {
    pub fn lazy_span(self) -> LazyExternFuncSpan {
        LazyExternFuncSpan::new(self)
    }
}

#[salsa::tracked]
pub struct Struct {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub attributes: AttrListId,
    pub is_pub: bool,
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
    pub is_pub: bool,
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
    pub is_pub: bool,
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
    pub is_pub: bool,
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
    pub is_pub: bool,
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

    pub tree: Partial<super::UseTreeId>,
    pub top_mod: TopLevelMod,

    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Use>,
}
impl Use {
    pub fn lazy_span(self) -> LazyUseSpan {
        LazyUseSpan::new(self)
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
    pub fn is_pub(self) -> bool {
        match self {
            ItemModifier::Pub | ItemModifier::PubAndUnsafe => true,
            ItemModifier::Unsafe | ItemModifier::None => false,
        }
    }
}

#[salsa::interned]
pub struct RecordFieldListId {
    #[return_ref]
    pub fields: Vec<RecordField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordField {
    pub name: Partial<IdentId>,
    pub ty: Partial<TypeId>,
    pub is_pub: bool,
}

#[salsa::interned]
pub struct EnumVariantListId {
    #[return_ref]
    pub variants: Vec<EnumVariant>,
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
    Use(Partial<super::UseTreeId>),
    Extern,
    Joined(Box<Self>, Box<Self>),
}

impl TrackedItemId {
    pub(crate) fn join(self, rhs: Self) -> Self {
        Self::Joined(self.into(), rhs.into())
    }
}
