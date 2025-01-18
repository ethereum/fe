use analysis_pass::ModuleAnalysisPass;
use common::{InputDb, InputIngot};
use hir_def::{module_tree_impl, IdentId, IngotId, TopLevelMod};
pub use lower::parse::ParserError;
use lower::parse::{parse_file_impl, ParseErrorAccumulator};
use parser::GreenNode;

pub mod analysis_pass;
pub mod diagnostics;
pub mod hir_def;
pub mod lower;
pub mod span;
pub mod visitor;

#[derive(Clone, Copy)]
pub struct ParsingPass<'db> {
    db: &'db dyn HirDb,
}

impl<'db> ParsingPass<'db> {
    pub fn new(db: &'db dyn HirDb) -> Self {
        Self { db }
    }

    pub fn green_node(self, top_mod: TopLevelMod) -> GreenNode {
        parse_file_impl(self.db, top_mod)
    }
}

impl<'db> ModuleAnalysisPass<'db> for ParsingPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn diagnostics::DiagnosticVoucher<'db> + 'db>> {
        parse_file_impl::accumulated::<ParseErrorAccumulator>(self.db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.0) as _)
            .collect::<Vec<_>>()
    }
}

/// Returns the root modules and names of external ingots that the given `ingot`
/// depends on.
/// From the outside of the crate, this functionality can be accessed via
/// [`TopLevelMod::external_ingots`](crate::TopLevelMod::external_ingots).
// The reason why this function is not a public API is that we want to prohibit users of `HirDb` to
// access `InputIngot` directly.
#[salsa::tracked(return_ref)]
#[allow(elided_named_lifetimes)]
pub(crate) fn external_ingots_impl(
    db: &dyn HirDb,
    ingot: InputIngot,
) -> Vec<(IdentId<'_>, IngotId<'_>)> {
    let mut res = Vec::new();
    for dep in ingot.external_ingots(db.as_input_db()) {
        let name = IdentId::new(db, dep.name.to_string());
        let ingot = module_tree_impl(db, dep.ingot)
            .root_data()
            .top_mod
            .ingot(db);
        res.push((name, ingot))
    }
    res
}

#[salsa::db]
pub trait HirDb: salsa::Database + InputDb {
    fn as_hir_db(&self) -> &dyn HirDb;
}

/// `LowerHirDb` is a marker trait for lowering AST to HIR items.
/// All code that requires [`LowerHirDb`] is considered have a possibility to
/// invalidate the cache in salsa when a revision is updated. Therefore,
/// implementations relying on `LowerHirDb` are prohibited in all
/// Analysis phases.
#[salsa::db]
pub trait LowerHirDb: salsa::Database + HirDb {
    fn as_lower_hir_db(&self) -> &dyn LowerHirDb;
}

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
pub trait SpannedHirDb: salsa::Database + HirDb {
    fn as_spanned_hir_db(&self) -> &dyn SpannedHirDb;
}

#[cfg(test)]
mod test_db {
    use common::{
        impl_db_traits,
        indexmap::IndexSet,
        input::{IngotKind, Version},
        InputDb, InputFile, InputIngot,
    };

    use super::HirDb;
    use crate::{
        hir_def::{scope_graph::ScopeGraph, ItemKind, TopLevelMod},
        lower::{map_file_to_mod, scope_graph},
        span::LazySpan,
        LowerHirDb, SpannedHirDb,
    };

    #[derive(Clone, Default)]
    #[salsa::db]
    pub(crate) struct TestDb {
        storage: salsa::Storage<Self>,
    }
    impl_db_traits!(TestDb, InputDb, HirDb, LowerHirDb, SpannedHirDb);

    impl TestDb {
        pub fn parse_source(&self, ingot: InputIngot, file: InputFile) -> &ScopeGraph {
            let top_mod = map_file_to_mod(self, ingot, file);
            scope_graph(self, top_mod)
        }

        /// Parses the given source text and returns the first inner item in the
        /// file.
        pub fn expect_item<'db, T>(&'db self, ingot: InputIngot, input: InputFile) -> T
        where
            ItemKind<'db>: TryInto<T, Error = &'static str>,
        {
            let tree = self.parse_source(ingot, input);
            tree.items_dfs(self)
                .find_map(|it| it.try_into().ok())
                .unwrap()
        }

        pub fn expect_items<'db, T>(&'db self, ingot: InputIngot, input: InputFile) -> Vec<T>
        where
            ItemKind<'db>: TryInto<T, Error = &'static str>,
        {
            let tree = self.parse_source(ingot, input);
            tree.items_dfs(self)
                .filter_map(|it| it.try_into().ok())
                .collect()
        }

        pub fn text_at(&self, top_mod: TopLevelMod, span: &impl LazySpan) -> &str {
            let range = span.resolve(self).unwrap().range;
            let file = top_mod.file(self.as_hir_db());
            let text = file.text(self.as_hir_db().as_input_db());
            &text[range.start().into()..range.end().into()]
        }

        pub fn standalone_file(&mut self, text: &str) -> (InputIngot, InputFile) {
            let path = "hir_test";
            let kind = IngotKind::StandAlone;
            let version = Version::new(0, 0, 1);
            let ingot = InputIngot::new(self, path, kind, version, IndexSet::default());
            let file = InputFile::new(self, "test_file.fe".into(), text.to_string());
            ingot.set_root_file(self, file);
            ingot.set_files(self, [file].into_iter().collect());
            (ingot, file)
        }
    }
}
