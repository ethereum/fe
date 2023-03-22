pub mod hir_def;
pub mod input;
pub mod lower;
pub mod span;

#[salsa::jar(db = HirDb)]
pub struct Jar(
    /// Inputs
    input::Ingot,
    input::File,
    // Tracked Hir items.
    hir_def::Fn,
    hir_def::ExternFn,
    hir_def::Struct,
    hir_def::Contract,
    hir_def::Enum,
    hir_def::TypeAlias,
    hir_def::Impl,
    hir_def::Trait,
    hir_def::ImplTrait,
    hir_def::Const,
    hir_def::Use,
    // Interned structs.
    hir_def::Body,
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

pub trait HirDb: salsa::DbWithJar<Jar> {}
impl<DB> HirDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}
