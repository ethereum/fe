use common::{InputDb, InputIngot};
use hir_def::{module_tree_impl, IdentId, TopLevelMod};
pub use lower::parse::ParserError;
use lower::{
    map_file_to_mod_impl,
    parse::{parse_file_impl, ParseErrorAccumulator},
    scope_graph_impl,
};

pub mod analysis_pass;
pub mod diagnostics;
pub mod hir_def;
pub mod lower;
pub mod span;
pub mod visitor;

#[salsa::jar(db = HirDb)]
pub struct Jar(
    hir_def::IngotId,
    // Tracked Hir items.
    hir_def::TopLevelMod,
    hir_def::Mod,
    hir_def::Func,
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
    hir_def::FuncParamListId,
    hir_def::AttrListId,
    hir_def::WhereClauseId,
    hir_def::GenericArgListId,
    hir_def::GenericParamListId,
    hir_def::FieldDefListId,
    hir_def::VariantDefListId,
    hir_def::ImplItemListId,
    hir_def::TypeId,
    hir_def::UsePathId,
    /// Accumulated diagnostics.
    ParseErrorAccumulator,
    /// Private tracked functions. These are not part of the public API, and
    /// thus, can't be accessed from outside of the crate without implementing
    /// [`LowerHirDb`] marker trait.
    module_tree_impl,
    scope_graph_impl,
    map_file_to_mod_impl,
    parse_file_impl,
    external_ingots_impl,
);

/// Returns the root modules and names of external ingots that the given `ingot`
/// depends on.
/// From the outside of the crate, this functionality can be accessed via
/// [`TopLevelMod::external_ingots`](crate::TopLevelMod::external_ingots).
// The reason why this function is not a public API is that we want to prohibit users of `HirDb` to
// access `InputIngot` directly.
#[salsa::tracked(return_ref)]
pub(crate) fn external_ingots_impl(
    db: &dyn HirDb,
    ingot: InputIngot,
) -> Vec<(IdentId, TopLevelMod)> {
    let mut res = Vec::new();
    for dep in ingot.external_ingots(db.as_input_db()) {
        let name = IdentId::new(db, dep.name.to_string());
        let root = module_tree_impl(db, dep.ingot).root_data().top_mod;
        res.push((name, root))
    }
    res
}

pub trait HirDb: salsa::DbWithJar<Jar> + InputDb {
    fn prefill(&self)
    where
        Self: Sized,
    {
        IdentId::prefill(self)
    }

    fn as_hir_db(&self) -> &dyn HirDb {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db::<'_>(self)
    }
}

/// `LowerHirDb` is a marker trait for lowering AST to HIR items.
/// All code that requires [`LowerHirDb`] is considered have a possibility to
/// invalidate the cache in salsa when a revision is updated. Therefore,
/// implementations relying on `LowerHirDb` are prohibited in all
/// Analysis phases.
pub trait LowerHirDb: HirDb {
    fn as_lower_hir_db(&self) -> &dyn LowerHirDb
    where
        Self: Sized,
    {
        self
    }
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
pub trait SpannedHirDb: HirDb {
    fn as_spanned_hir_db(&self) -> &dyn SpannedHirDb
    where
        Self: Sized,
    {
        self
    }
}

#[cfg(test)]
mod test_db {
    use super::HirDb;
    use std::collections::BTreeSet;

    use common::{
        input::{IngotKind, Version},
        InputFile, InputIngot,
    };

    use crate::{
        hir_def::{scope_graph::ScopeGraph, ItemKind, TopLevelMod},
        lower::{map_file_to_mod, scope_graph},
        span::LazySpan,
        LowerHirDb, SpannedHirDb,
    };

    #[salsa::db(common::Jar, crate::Jar)]
    pub(crate) struct TestDb {
        storage: salsa::Storage<Self>,
    }

    impl Default for TestDb {
        fn default() -> Self {
            let db = Self {
                storage: Default::default(),
            };
            db.prefill();
            db
        }
    }
    impl HirDb for TestDb {}
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

    impl TestDb {
        pub fn parse_source(&self, file: InputFile) -> &ScopeGraph {
            let top_mod = map_file_to_mod(self, file);
            scope_graph(self, top_mod)
        }

        /// Parses the given source text and returns the first inner item in the
        /// file.
        pub fn expect_item<T>(&mut self, text: &str) -> T
        where
            ItemKind: TryInto<T, Error = &'static str>,
        {
            let file = self.standalone_file(text);
            let tree = self.parse_source(file);
            tree.items_dfs(self)
                .find_map(|it| it.try_into().ok())
                .unwrap()
        }

        pub fn expect_items<T>(&mut self, text: &str) -> Vec<T>
        where
            ItemKind: TryInto<T, Error = &'static str>,
        {
            let file = self.standalone_file(text);
            let tree = self.parse_source(file);
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

        pub fn standalone_file(&mut self, text: &str) -> InputFile {
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
