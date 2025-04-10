use common::InputDb;
pub use lower::parse::ParserError;

pub mod hir_def;
pub mod lower;
pub mod span;
pub mod visitor;

#[salsa::db]
pub trait HirDb: salsa::Database + InputDb {}

#[salsa::db]
impl<T> HirDb for T where T: salsa::Database + InputDb {}

/// `LowerHirDb` is a marker trait for lowering AST to HIR items.
/// All code that requires [`LowerHirDb`] is considered have a possibility to
/// invalidate the cache in salsa when a revision is updated. Therefore,
/// implementations relying on `LowerHirDb` are prohibited in all
/// Analysis phases.
#[salsa::db]
pub trait LowerHirDb: salsa::Database + HirDb {}
#[salsa::db]
impl<T> LowerHirDb for T where T: HirDb {}

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
#[salsa::db]
pub trait SpannedHirDb: salsa::Database + HirDb {}
#[salsa::db]
impl<T> SpannedHirDb for T where T: HirDb {}

#[cfg(test)]
mod test_db {
    use common::{
        indexmap::IndexSet,
        input::{IngotKind, Version},
        InputFile, InputIngot,
    };
    use derive_more::TryIntoError;

    use crate::{
        hir_def::{scope_graph::ScopeGraph, ItemKind, TopLevelMod},
        lower::{map_file_to_mod, scope_graph},
        span::LazySpan,
    };

    #[derive(Clone, Default)]
    #[salsa::db]
    pub(crate) struct TestDb {
        storage: salsa::Storage<Self>,
    }
    #[salsa::db]
    impl salsa::Database for TestDb {
        fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
    }

    impl TestDb {
        pub fn parse_source(&self, ingot: InputIngot, file: InputFile) -> &ScopeGraph {
            let top_mod = map_file_to_mod(self, ingot, file);
            scope_graph(self, top_mod)
        }

        /// Parses the given source text and returns the first inner item in the
        /// file.
        pub fn expect_item<'db, T>(&'db self, ingot: InputIngot, input: InputFile) -> T
        where
            ItemKind<'db>: TryInto<T, Error = TryIntoError<ItemKind<'db>>>,
        {
            let tree = self.parse_source(ingot, input);
            tree.items_dfs(self)
                .find_map(|it| it.try_into().ok())
                .unwrap()
        }

        pub fn expect_items<'db, T>(&'db self, ingot: InputIngot, input: InputFile) -> Vec<T>
        where
            ItemKind<'db>: TryInto<T, Error = TryIntoError<ItemKind<'db>>>,
        {
            let tree = self.parse_source(ingot, input);
            tree.items_dfs(self)
                .filter_map(|it| it.try_into().ok())
                .collect()
        }

        pub fn text_at(&self, top_mod: TopLevelMod, span: &impl LazySpan) -> &str {
            let range = span.resolve(self).unwrap().range;
            let file = top_mod.file(self);
            let text = file.text(self);
            &text[range.start().into()..range.end().into()]
        }

        pub fn standalone_file(&mut self, text: &str) -> (InputIngot, InputFile) {
            let path = "hir_test";
            let kind = IngotKind::StandAlone;
            let version = Version::new(0, 0, 1);
            let ingot = InputIngot::new(
                self,
                path.into(),
                kind,
                version,
                IndexSet::default(),
                IndexSet::default(),
                None,
            );
            let file = InputFile::new(self, "test_file.fe".into(), text.to_string());
            ingot.set_root_file(self, file);
            ingot.set_files(self, [file].into_iter().collect());
            (ingot, file)
        }
    }
}
