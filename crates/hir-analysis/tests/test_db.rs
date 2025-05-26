use std::collections::BTreeMap;

use camino::Utf8PathBuf;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{BufferWriter, ColorChoice},
    },
};
use common::{
    core::HasBuiltinCore, define_input_db, diagnostics::Span, file::File, indexmap::IndexMap,
    InputDb,
};
use driver::diagnostics::{CsDbWrapper, ToCsDiag};
use fe_hir_analysis::{
    analysis_pass::{AnalysisPassManager, ParsingPass},
    name_resolution::ImportAnalysisPass,
    ty::{
        AdtDefAnalysisPass, BodyAnalysisPass, DefConflictAnalysisPass, FuncAnalysisPass,
        ImplAnalysisPass, ImplTraitAnalysisPass, TraitAnalysisPass, TypeAliasAnalysisPass,
    },
};
use hir::{
    hir_def::TopLevelMod,
    lower,
    span::{DynLazySpan, LazySpan},
    SpannedHirDb,
};
use rustc_hash::FxHashMap;

type CodeSpanFileId = usize;

define_input_db!(HirAnalysisTestDb);

// https://github.com/rust-lang/rust/issues/46379
#[allow(dead_code)]
impl HirAnalysisTestDb {
    pub fn new_stand_alone(&mut self, file_path: Utf8PathBuf, text: &str) -> File {
        let file_name = if file_path.is_relative() {
            format!("/{}", file_path)
        } else {
            file_path.to_string()
        };
        // Use the index from the database and reinitialize it with core files
        let index = self.workspace();
        self.initialize_builtin_core();
        index.touch(
            self,
            url::Url::from_file_path(&file_name).expect("Failed to create URL from file path"),
            Some(text.to_string()),
        )
    }

    pub fn top_mod(&self, input: File) -> (TopLevelMod, HirPropertyFormatter) {
        let mut prop_formatter = HirPropertyFormatter::default();
        let top_mod = self.register_file(&mut prop_formatter, input);
        (top_mod, prop_formatter)
    }

    pub fn assert_no_diags(&self, top_mod: TopLevelMod) {
        let mut manager = initialize_analysis_pass();
        let diags = manager.run_on_module(self, top_mod);

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
                term::emit(&mut buffer, &config, &CsDbWrapper(self), cs_diag).unwrap();
            }
            eprintln!("{}", std::str::from_utf8(buffer.as_slice()).unwrap());

            panic!("this module contains errors");
        }
    }

    fn register_file<'db>(
        &'db self,
        prop_formatter: &mut HirPropertyFormatter<'db>,
        input_file: File,
    ) -> TopLevelMod<'db> {
        let top_mod = lower::map_file_to_mod(self, input_file);
        let path = input_file
            .path(self)
            .as_ref()
            .expect("Failed to get file path");
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

    pub fn register_top_mod(&mut self, path: &str, text: &str, top_mod: TopLevelMod<'db>) {
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

fn initialize_analysis_pass() -> AnalysisPassManager {
    let mut pass_manager = AnalysisPassManager::new();
    pass_manager.add_module_pass(Box::new(ParsingPass {}));
    pass_manager.add_module_pass(Box::new(DefConflictAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(ImportAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(AdtDefAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(TypeAliasAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(TraitAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(ImplAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(ImplTraitAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(FuncAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(BodyAnalysisPass {}));
    pass_manager
}
