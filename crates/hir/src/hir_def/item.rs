// This is necessary because `salsa::tracked` structs generates a constructor
// that may take many arguments depending on the number of fields in the struct.
#![allow(clippy::too_many_arguments)]

use parser::ast;

use crate::{hir_def::TraitRef, span::HirOrigin};

use super::{
    AttrListId, Body, FnParamListId, GenericParamListId, IdentId, MaybeInvalid, TypeId,
    WhereClauseId,
};

#[salsa::tracked]
pub struct Mod {
    #[id]
    id: TrackedItemId,

    pub name: MaybeInvalid<IdentId>,
    pub attributes: AttrListId,
    pub is_pub: bool,

    pub(crate) origin: HirOrigin<ast::Mod>,
}

#[salsa::tracked]
pub struct Fn {
    #[id]
    id: TrackedItemId,

    pub name: MaybeInvalid<IdentId>,
    pub attributes: AttrListId,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,
    pub params: MaybeInvalid<FnParamListId>,
    pub ret_ty: Option<TypeId>,
    pub modifier: ItemModifier,
    pub body: Option<Body>,

    pub(crate) origin: HirOrigin<ast::Fn>,
}

#[salsa::tracked]
pub struct ExternFn {
    #[id]
    id: TrackedItemId,

    pub name: MaybeInvalid<IdentId>,
    pub attributes: AttrListId,
    pub params: MaybeInvalid<FnParamListId>,
    pub ret_ty: Option<TypeId>,
    pub modifier: ItemModifier,

    pub(crate) origin: HirOrigin<ast::Fn>,
}

#[salsa::tracked]
pub struct Struct {
    #[id]
    id: TrackedItemId,

    pub name: MaybeInvalid<IdentId>,
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

    pub name: MaybeInvalid<IdentId>,
    pub attributes: AttrListId,
    pub is_pub: bool,
    pub fields: RecordFieldListId,

    pub(crate) origin: HirOrigin<ast::Contract>,
}

#[salsa::tracked]
pub struct Enum {
    #[id]
    id: TrackedItemId,

    pub name: MaybeInvalid<IdentId>,
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

    pub name: MaybeInvalid<IdentId>,
    pub attributes: AttrListId,
    pub is_pub: bool,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,
    pub ty: MaybeInvalid<TypeId>,

    pub(crate) origin: HirOrigin<ast::TypeAlias>,
}

#[salsa::tracked]
pub struct Impl {
    #[id]
    id: TrackedItemId,

    pub ty: super::MaybeInvalid<TypeId>,
    pub attributes: AttrListId,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,
    pub(crate) origin: HirOrigin<ast::Impl>,
}

#[salsa::tracked]
pub struct Trait {
    #[id]
    id: TrackedItemId,

    pub name: MaybeInvalid<IdentId>,

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

    pub trait_ref: MaybeInvalid<TraitRef>,
    pub ty: MaybeInvalid<TypeId>,
    pub attributes: AttrListId,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,
    pub(crate) origin: HirOrigin<ast::ImplTrait>,
}

#[salsa::tracked]
pub struct Const {
    #[id]
    id: TrackedItemId,

    pub name: MaybeInvalid<IdentId>,
    pub body: MaybeInvalid<Body>,
    pub(crate) origin: HirOrigin<ast::Const>,
}

#[salsa::tracked]
pub struct Use {
    #[id]
    id: TrackedItemId,

    pub tree: MaybeInvalid<super::UseTreeId>,
    pub(crate) origin: HirOrigin<ast::Use>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From, PartialOrd, Ord)]
pub enum ItemKind {
    Module(Mod),
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
        matches!(self, ItemModifier::Pub | ItemModifier::PubAndUnsafe)
    }
}

#[salsa::interned]
pub struct RecordFieldListId {
    #[return_ref]
    pub fields: Vec<RecordField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordField {
    pub name: MaybeInvalid<IdentId>,
    pub ty: MaybeInvalid<TypeId>,
    pub is_pub: bool,
}

#[salsa::interned]
pub struct EnumVariantListId {
    #[return_ref]
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub name: MaybeInvalid<IdentId>,
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
    Mod(MaybeInvalid<IdentId>),
    Fn(MaybeInvalid<IdentId>),
    Struct(MaybeInvalid<IdentId>),
    Contract(MaybeInvalid<IdentId>),
    Enum(MaybeInvalid<IdentId>),
    TypeAlias(MaybeInvalid<IdentId>),
    Impl(MaybeInvalid<TypeId>),
    Trait(MaybeInvalid<IdentId>),
    ImplTrait(MaybeInvalid<TraitRef>, MaybeInvalid<TypeId>),
    Const(MaybeInvalid<IdentId>),
    Use(MaybeInvalid<super::UseTreeId>),
    Extern,
    Joined(Box<Self>, Box<Self>),
}

impl TrackedItemId {
    pub(crate) fn join(self, rhs: Self) -> Self {
        Self::Joined(self.into(), rhs.into())
    }

    pub(crate) fn join_opt(self, rhs: Option<Self>) -> Self {
        if let Some(rhs) = rhs {
            self.join(rhs)
        } else {
            self
        }
    }
}
