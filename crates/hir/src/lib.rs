use common::{InputDb, Upcast};
pub use lower::parse::ParseDiagnostic;

use lower::parse::{parse_file, ParseDiagnosticAccumulator};

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
    /// Tracked functions
    hir_def::ingot_module_tree,
    hir_def::module_item_tree,
    parse_file,
);

pub trait HirDb: salsa::DbWithJar<Jar> + InputDb + Upcast<dyn InputDb> {
    /// Returns the diagnostics produced by parsing the given file.
    fn diagnostics_for_parse(&self, file: common::InputFile) -> Vec<ParseDiagnostic>
    where
        Self: Sized,
    {
        parse_file(self, file);
        parse_file::accumulated::<ParseDiagnosticAccumulator>(self, file)
    }
}
impl<DB> HirDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> + InputDb + Upcast<dyn InputDb> {}

#[cfg(test)]
mod test_db {
    use std::collections::BTreeSet;

    use common::{
        input::{IngotKind, Version},
        InputFile, InputIngot, Upcast,
    };

    use crate::{
        hir_def::{module_item_tree, ItemKind, ItemTree},
        span::{db::SpannedHirDb, LazySpan},
    };

    #[derive(Default)]
    #[salsa::db(common::Jar, crate::Jar)]
    pub(crate) struct TestDb {
        storage: salsa::Storage<Self>,
    }

    impl SpannedHirDb for TestDb {}

    impl salsa::Database for TestDb {
        fn salsa_event(&self, _: salsa::Event) {}
    }

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
        pub fn parse_source(&mut self, text: &str) -> (InputFile, &ItemTree) {
            let file = self.standalone_file(text);
            (file, module_item_tree(self, file))
        }

        /// Parses the given source text and returns the first inner item in the
        /// file.
        pub fn parse_source_to_first_item<T>(&mut self, text: &str) -> (InputFile, T)
        where
            ItemKind: TryInto<T, Error = &'static str>,
        {
            let (file, tree) = self.parse_source(text);
            let top_mod = tree.top_mod;
            (
                file,
                tree.children(top_mod).next().unwrap().try_into().unwrap(),
            )
        }

        pub fn text_at(&self, file: InputFile, span: &impl LazySpan) -> &str {
            let range = span.resolve(self).range;
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
            ingot.set_files(self).to([file].into());
            file
        }
    }
}
