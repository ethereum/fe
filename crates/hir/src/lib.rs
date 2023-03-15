pub mod hir_def;
pub mod span;

#[salsa::jar(db = Db)]
pub struct Jar(
    // Tracked Hir items.
    hir_def::Fn,
    hir_def::Struct,
    hir_def::Contract,
    hir_def::Enum,
    hir_def::TypeAlias,
    hir_def::Impl,
    hir_def::Trait,
    hir_def::ImplTrait,
    hir_def::Const,
    hir_def::Use,
    hir_def::Extern,
    hir_def::Body,
    // Interned structs.
    hir_def::IdentId,
    hir_def::IntegerId,
    hir_def::StringId,
    hir_def::PathId,
    hir_def::FnParamListId,
    hir_def::AttrListId,
    hir_def::WhereClauseId,
    hir_def::GenericArgListId,
    hir_def::GenericParamListId,
    hir_def::RecordFieldListId,
    hir_def::EnumVariantListId,
    hir_def::ImplItemListId,
    hir_def::TypeId,
    hir_def::UseTreeId,
);

pub trait Db: salsa::DbWithJar<Jar> {}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}
