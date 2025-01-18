use std::collections::BTreeMap;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{BufferWriter, ColorChoice},
    },
};
use common::{
    diagnostics::Span,
    impl_db_traits,
    indexmap::{IndexMap, IndexSet},
    input::{IngotKind, Version},
    InputDb, InputFile, InputIngot,
};
use driver::{diagnostics::ToCsDiag, CsDbWrapper, DriverDb};
use fe_hir_analysis::{
    name_resolution::{DefConflictAnalysisPass, ImportAnalysisPass, PathAnalysisPass},
    ty::{
        AdtDefAnalysisPass, BodyAnalysisPass, FuncAnalysisPass, ImplAnalysisPass,
        ImplTraitAnalysisPass, TraitAnalysisPass, TypeAliasAnalysisPass,
    },
    HirAnalysisDb,
};
use hir::{
    analysis_pass::AnalysisPassManager,
    hir_def::TopLevelMod,
    lower,
    span::{DynLazySpan, LazySpan},
    HirDb, LowerHirDb, ParsingPass, SpannedHirDb,
};
use rustc_hash::FxHashMap;

type CodeSpanFileId = usize;

#[derive(Default, Clone)]
#[salsa::db]
pub struct HirAnalysisTestDb {
    storage: salsa::Storage<Self>,
}
impl_db_traits!(
    HirAnalysisTestDb,
    InputDb,
    HirDb,
    LowerHirDb,
    SpannedHirDb,
    HirAnalysisDb,
    DriverDb,
);

// https://github.com/rust-lang/rust/issues/46379
#[allow(dead_code)]
impl HirAnalysisTestDb {
    pub fn new_stand_alone(&mut self, file_name: &str, text: &str) -> (InputIngot, InputFile) {
        let kind = IngotKind::StandAlone;
        let version = Version::new(0, 0, 1);
        let ingot = InputIngot::new(self, file_name, kind, version, IndexSet::default());
        let root = InputFile::new(self, file_name.into(), text.to_string());
        ingot.set_root_file(self, root);
        ingot.set_files(self, [root].into_iter().collect());
        (ingot, root)
    }

    pub fn top_mod(
        &self,
        ingot: InputIngot,
        input: InputFile,
    ) -> (TopLevelMod, HirPropertyFormatter) {
        let mut prop_formatter = HirPropertyFormatter::default();
        let top_mod = self.register_file(&mut prop_formatter, ingot, input);
        (top_mod, prop_formatter)
    }

    pub fn assert_no_diags(&self, top_mod: TopLevelMod) {
        let mut manager = initialize_analysis_pass(self);
        let diags = manager.run_on_module(top_mod);

        if !diags.is_empty() {
            let writer = BufferWriter::stderr(ColorChoice::Auto);
            let mut buffer = writer.buffer();
            let config = term::Config::default();

            // copied from driver
            let mut diags: Vec<_> = diags.iter().map(|d| d.to_complete(self)).collect();
            diags.sort_by(|lhs, rhs| match lhs.error_code.cmp(&rhs.error_code) {
                std::cmp::Ordering::Equal => lhs.primary_span().cmp(&rhs.primary_span()),
                ord => ord,
            });

            for diag in diags {
                let cs_diag = &diag.to_cs(self);
                term::emit(
                    &mut buffer,
                    &config,
                    &CsDbWrapper(self.as_driver_db()),
                    cs_diag,
                )
                .unwrap();
            }
            eprintln!("{}", std::str::from_utf8(buffer.as_slice()).unwrap());

            panic!("this module contains errors");
        }
    }

    fn register_file<'db>(
        &'db self,
        prop_formatter: &mut HirPropertyFormatter<'db>,
        ingot: InputIngot,
        input_file: InputFile,
    ) -> TopLevelMod<'db> {
        let top_mod = lower::map_file_to_mod(self, ingot, input_file);
        let path = input_file.path(self);
        let text = input_file.text(self);
        prop_formatter.register_top_mod(path.as_str(), text, top_mod);
        top_mod
    }
}

pub struct HirPropertyFormatter<'db> {
    // https://github.com/rust-lang/rust/issues/46379
    #[allow(dead_code)]
    properties: IndexMap<TopLevelMod<'db>, Vec<(String, DynLazySpan<'db>)>>,
    top_mod_to_file: FxHashMap<TopLevelMod<'db>, CodeSpanFileId>,
    code_span_files: SimpleFiles<String, String>,
}

// https://github.com/rust-lang/rust/issues/46379
#[allow(dead_code)]
impl<'db> HirPropertyFormatter<'db> {
    pub fn push_prop(&mut self, top_mod: TopLevelMod<'db>, span: DynLazySpan<'db>, prop: String) {
        self.properties
            .entry(top_mod)
            .or_default()
            .push((prop, span));
    }

    pub fn finish(&mut self, db: &'db dyn SpannedHirDb) -> String {
        let writer = BufferWriter::stderr(ColorChoice::Never);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        for top_mod in self.top_mod_to_file.keys() {
            if !self.properties.contains_key(top_mod) {
                continue;
            }

            let diags = self.properties[top_mod]
                .iter()
                .map(|(prop, span)| {
                    let (span, diag) = self.property_to_diag(db, *top_mod, prop, span.clone());
                    ((span.file, (span.range.start(), span.range.end())), diag)
                })
                .collect::<BTreeMap<_, _>>();

            for diag in diags.values() {
                term::emit(&mut buffer, &config, &self.code_span_files, diag).unwrap();
            }
        }

        std::str::from_utf8(buffer.as_slice()).unwrap().to_string()
    }

    fn property_to_diag(
        &self,
        db: &'db dyn SpannedHirDb,
        top_mod: TopLevelMod<'db>,
        prop: &str,
        span: DynLazySpan<'db>,
    ) -> (Span, Diagnostic<usize>) {
        let file_id = self.top_mod_to_file[&top_mod];
        let span = span.resolve(db).unwrap();
        let diag = Diagnostic::note()
            .with_labels(vec![Label::primary(file_id, span.range).with_message(prop)]);
        (span, diag)
    }

    fn register_top_mod(&mut self, path: &str, text: &str, top_mod: TopLevelMod<'db>) {
        let file_id = self.code_span_files.add(path.to_string(), text.to_string());
        self.top_mod_to_file.insert(top_mod, file_id);
    }
}

impl Default for HirPropertyFormatter<'_> {
    fn default() -> Self {
        Self {
            properties: Default::default(),
            top_mod_to_file: Default::default(),
            code_span_files: SimpleFiles::new(),
        }
    }
}

fn initialize_analysis_pass(db: &HirAnalysisTestDb) -> AnalysisPassManager<'_> {
    let mut pass_manager = AnalysisPassManager::new();
    pass_manager.add_module_pass(Box::new(ParsingPass::new(db)));
    pass_manager.add_module_pass(Box::new(DefConflictAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(ImportAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(PathAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(AdtDefAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(TypeAliasAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(TraitAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(ImplAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(ImplTraitAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(FuncAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(BodyAnalysisPass::new(db)));
    pass_manager
}
