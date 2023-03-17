// This is necessary because `salsa::tracked` structs generates a constructor
// that may take many arguments depending on the number of fields in the struct.
#![allow(clippy::too_many_arguments)]

use fe_parser2::ast;

use crate::{hir_def::TraitRef, span::HirOrigin};

use super::{
    AttrListId, FnParamListId, GenericParamListId, IdentId, MaybeInvalid, TypeId, WhereClauseId,
};

#[salsa::tracked]
pub struct Fn {
    #[id]
    pub name: MaybeInvalid<IdentId>,

    pub attributes: AttrListId,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,
    pub params: MaybeInvalid<FnParamListId>,
    pub ret_ty: Option<TypeId>,
    pub modifier: ItemModifier,

    pub(crate) origin: HirOrigin<ast::Fn>,
}

#[salsa::tracked]
pub struct Struct {
    #[id]
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
    pub name: MaybeInvalid<IdentId>,

    pub attributes: AttrListId,
    pub is_pub: bool,
    pub fields: RecordFieldListId,

    pub(crate) origin: HirOrigin<ast::Contract>,
}

#[salsa::tracked]
pub struct Enum {
    #[id]
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
    pub ty: super::MaybeInvalid<TypeId>,

    pub attributes: AttrListId,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,

    pub(crate) origin: HirOrigin<ast::Impl>,
}

#[salsa::tracked]
pub struct Trait {
    #[id]
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
    pub trait_ref: MaybeInvalid<TraitRef>,
    #[id]
    pub ty: MaybeInvalid<TypeId>,

    pub attributes: AttrListId,
    pub generic_params: GenericParamListId,
    pub where_clause: WhereClauseId,

    pub(crate) origin: HirOrigin<ast::ImplTrait>,
}

#[salsa::tracked]
pub struct Const {
    #[id]
    pub name: MaybeInvalid<IdentId>,

    pub(crate) origin: HirOrigin<ast::Const>,
}

#[salsa::tracked]
pub struct Use {
    #[id]
    pub tree: MaybeInvalid<super::UseTreeId>,

    pub(crate) origin: HirOrigin<ast::Use>,
}

#[salsa::tracked]
pub struct Extern {
    pub(crate) origin: HirOrigin<ast::Extern>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub enum ItemKind {
    Fn(Fn),
    Struct(Struct),
    Contract(Contract),
    Enum(Enum),
    TypeAlias(TypeAlias),
    Impl(Impl),
    Trait(Trait),
    ImplTrait(ImplTrait),
    Const(Const),
    Use(Use),
    Extern(Extern),
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
