// This is necessary because `salsa::tracked` structs generates a
// constructor
// that may take many arguments depending on the number of fields in the struct.
#![allow(clippy::too_many_arguments)]

use parser::ast;

use crate::{hir_def::TraitRef, span::HirOrigin};

use super::{
    AttrListId, Body, FnParamListId, GenericParamListId, IdentId, Partial, TypeId, WhereClauseId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From, PartialOrd, Ord)]
pub enum ItemKind {
    TopMod(TopLevelMod),
    Mod(Mod),
    Fn(Fn),
    ExternFn(ExternFn),
    Struct(Struct),
    Contract(Contract),
    Enum(Enum),
    TypeAlias(TypeAlias),
    Impl(Impl),
    Trait(Trait),
    ImplTrait(ImplTrait),
    Const(Const),
    Use(Use),
    /// Body is not an `Item`, but this makes it easier to analyze items.
    Body(Body),
}

#[salsa::tracked]
pub struct TopLevelMod {
    // No #[id] here, because `TopLevelMod` is always unique to a `InputFile` that is an argument
    // of `module_item_tree`.
    pub name: IdentId,
    pub(crate) origin: HirOrigin<ast::Root>,
}

#[salsa::tracked]
pub struct Mod {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub attributes: AttrListId,
    pub is_pub: bool,

    pub(crate) origin: HirOrigin<ast::Mod>,
}

#[salsa::tracked]
pub struct Fn {
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

    pub(crate) origin: HirOrigin<ast::Fn>,
}

#[salsa::tracked]
pub struct ExternFn {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub attributes: AttrListId,
    pub params: Partial<FnParamListId>,
    pub ret_ty: Option<TypeId>,
    pub modifier: ItemModifier,

    pub(crate) origin: HirOrigin<ast::Fn>,
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

    pub(crate) origin: HirOrigin<ast::Struct>,
}

#[salsa::tracked]
pub struct Contract {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub attributes: AttrListId,
    pub is_pub: bool,
    pub fields: RecordFieldListId,

    pub(crate) origin: HirOrigin<ast::Contract>,
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

    pub(crate) origin: HirOrigin<ast::Enum>,
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

    pub(crate) origin: HirOrigin<ast::TypeAlias>,
}

#[salsa::tracked]
pub struct Impl {
    #[id]
    id: TrackedItemId,

    pub ty: super::Partial<TypeId>,
    pub attributes: AttrListId,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,
    pub(crate) origin: HirOrigin<ast::Impl>,
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
    pub(crate) origin: HirOrigin<ast::Trait>,
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
    pub(crate) origin: HirOrigin<ast::ImplTrait>,
}

#[salsa::tracked]
pub struct Const {
    #[id]
    id: TrackedItemId,

    pub name: Partial<IdentId>,
    pub body: Partial<Body>,
    pub(crate) origin: HirOrigin<ast::Const>,
}

#[salsa::tracked]
pub struct Use {
    #[id]
    id: TrackedItemId,

    pub tree: Partial<super::UseTreeId>,
    pub(crate) origin: HirOrigin<ast::Use>,
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
    pub items: Vec<Fn>,
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
