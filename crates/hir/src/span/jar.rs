use common::Upcast;
use parser::ast;

use crate::{
    hir_def::{
        Const, Contract, Enum, ExternFn, Fn, Impl, ImplTrait, Mod, Struct, TopLevelMod, Trait,
        TypeAlias, Use,
    },
    HirDb,
};

use super::HirOrigin;

#[salsa::jar(db = SpannedHirDb)]
pub struct SpanJar();

/// `SpannedHirDb` is a feature gate for extracting span-dependent information
/// from HIR Items. All code that requires [`SpannedHirDb`] is considered to
/// invalidate the cache in salsa when a revision is updated.
/// Therefore, implementations relying on `SpannedHirDb` are prohibited in all
/// Analysis phases.
///
/// SpanDb is mainly used to inject information about [`SyntaxNode`] to generate
/// [`FullDiagnostic`] from [`DiagnosticVoucher`].
pub trait SpannedHirDb: HirDb + salsa::DbWithJar<SpanJar> + Upcast<dyn HirDb> {
    fn toplevel_ast(&self, item: TopLevelMod) -> &HirOrigin<ast::Root> {
        item.origin(self.upcast())
    }

    fn mod_ast(&self, item: Mod) -> &HirOrigin<ast::Mod> {
        item.origin(self.upcast())
    }

    fn fn_ast(&self, item: Fn) -> &HirOrigin<ast::Fn> {
        item.origin(self.upcast())
    }

    fn extern_fn_ast(&self, item: ExternFn) -> &HirOrigin<ast::Fn> {
        item.origin(self.upcast())
    }

    fn struct_ast(&self, item: Struct) -> &HirOrigin<ast::Struct> {
        item.origin(self.upcast())
    }

    fn contract_ast(&self, item: Contract) -> &HirOrigin<ast::Contract> {
        item.origin(self.upcast())
    }

    fn enum_ast(&self, item: Enum) -> &HirOrigin<ast::Enum> {
        item.origin(self.upcast())
    }

    fn type_alias_ast(&self, item: TypeAlias) -> &HirOrigin<ast::TypeAlias> {
        item.origin(self.upcast())
    }

    fn impl_ast(&self, item: Impl) -> &HirOrigin<ast::Impl> {
        item.origin(self.upcast())
    }

    fn trait_ast(&self, item: Trait) -> &HirOrigin<ast::Trait> {
        item.origin(self.upcast())
    }

    fn impl_trait_ast(&self, item: ImplTrait) -> &HirOrigin<ast::ImplTrait> {
        item.origin(self.upcast())
    }

    fn const_ast(&self, item: Const) -> &HirOrigin<ast::Const> {
        item.origin(self.upcast())
    }

    fn use_ast(&self, item: Use) -> &HirOrigin<ast::Use> {
        item.origin(self.upcast())
    }
}
