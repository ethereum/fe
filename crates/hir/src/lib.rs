use common::{InputDb, Upcast};
use hir_def::module_tree_impl;
pub use lower::parse::ParseDiagnostic;

use lower::{
    item_tree_impl, map_file_to_mod_impl,
    parse::{parse_file_impl, ParseDiagnosticAccumulator},
};

pub mod diagnostics;
pub mod hir_def;
pub mod lower;
pub mod span;

#[salsa::jar(db = HirDb)]
pub struct Jar(
    // Tracked Hir items.
    hir_def::TopLevelMod,
    hir_def::Mod,
    hir_def::Func,
    hir_def::ExternFunc,
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
    /// Accumulated diagnostics.
    ParseDiagnosticAccumulator,
    /// Private tracked functions. These are not part of the public API, and
    /// thus, can't be accessed from outside of the crate without implementing
    /// [`LowerHirDb`] marker trait.
    module_tree_impl,
    item_tree_impl,
    map_file_to_mod_impl,
    parse_file_impl,
);

pub trait HirDb: salsa::DbWithJar<Jar> + InputDb + Upcast<dyn InputDb> {}
impl<DB> HirDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> + InputDb + Upcast<dyn InputDb> {}

/// `LowerHirDb` is a marker trait for lowering AST to HIR items.
/// All code that requires [`LowerHirDb`] is considered have a possibility to
/// invalidate the cache in salsa when a revision is updated. Therefore,
/// implementations relying on `LowerHirDb` are prohibited in all
/// Analysis phases.
pub trait LowerHirDb: HirDb + Upcast<dyn HirDb> {}

/// `SpannedHirDb` is a marker trait for extracting span-dependent information
/// from HIR Items.
/// All code that requires [`SpannedHirDb`] is considered have a possibility to
/// invalidate the cache in salsa when a revision is updated. Therefore,
/// implementations relying on `SpannedHirDb` are prohibited in all
/// Analysis phases.
///
/// This marker is mainly used to inject [HirOrigin](crate::span::HirOrigin) to
/// generate [CompleteDiagnostic](common::diagnostics::CompleteDiagnostic) from
/// [DiagnosticVoucher](crate::diagnostics::DiagnosticVoucher).
/// See also `[LazySpan]`[`crate::span::LazySpan`] for more details.
pub trait SpannedHirDb: HirDb + Upcast<dyn HirDb> {}

#[cfg(test)]
mod test_db {
    use std::collections::BTreeSet;

    use common::{
        input::{IngotKind, Version},
        InputFile, InputIngot, Upcast,
    };

    use crate::{
        hir_def::{ItemKind, ItemTree, TopLevelMod},
        lower::{item_tree, map_file_to_mod},
        span::LazySpan,
        LowerHirDb, SpannedHirDb,
    };

    #[derive(Default)]
    #[salsa::db(common::Jar, crate::Jar)]
    pub(crate) struct TestDb {
        storage: salsa::Storage<Self>,
    }
    impl SpannedHirDb for TestDb {}
    impl LowerHirDb for TestDb {}
    impl salsa::Database for TestDb {
        fn salsa_event(&self, _: salsa::Event) {}
    }
    /// Implements `ParallelDatabase` to check the all tracked
    /// structs/functions are `Send`.
    impl salsa::ParallelDatabase for TestDb {
        fn snapshot(&self) -> salsa::Snapshot<Self> {
            salsa::Snapshot::new(TestDb {
                storage: salsa::Storage::default(),
            })
        }
    }
    impl Upcast<dyn common::InputDb> for TestDb {
        fn upcast(&self) -> &(dyn common::InputDb + 'static) {
            self
        }
    }
    impl Upcast<dyn crate::HirDb> for TestDb {
        fn upcast(&self) -> &(dyn crate::HirDb + 'static) {
            self
        }
    }

    impl TestDb {
        pub fn parse_source(&mut self, text: &str) -> &ItemTree {
            let file = self.standalone_file(text);
            let top_mod = map_file_to_mod(self, file);
            item_tree(self, top_mod)
        }

        /// Parses the given source text and returns the first inner item in the
        /// file.
        pub fn expect_item<T>(&mut self, text: &str) -> T
        where
            ItemKind: TryInto<T, Error = &'static str>,
        {
            let tree = self.parse_source(text);
            tree.dfs().find_map(|it| it.try_into().ok()).unwrap()
        }

        pub fn text_at(&self, top_mod: TopLevelMod, span: &impl LazySpan) -> &str {
            let range = span.resolve(self).range;
            let file = top_mod.file(self.upcast());
            let text = file.text(self.upcast());
            &text[range.start().into()..range.end().into()]
        }

        fn standalone_file(&mut self, text: &str) -> InputFile {
            let path = "hir_test";
            let kind = IngotKind::StandAlone;
            let version = Version::new(0, 0, 1);
            let ingot = InputIngot::new(self, path, kind, version, BTreeSet::default());
            let file = InputFile::new(self, ingot, "test_file.fe".into(), text.to_string());
            ingot.set_root_file(self, file);
            ingot.set_files(self, [file].into());
            file
        }
    }
}
