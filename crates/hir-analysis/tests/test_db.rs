use std::collections::{BTreeMap, BTreeSet};

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
    input::{IngotKind, Version},
    InputFile, InputIngot,
};
use hir::{
    hir_def::TopLevelMod,
    lower,
    span::{DynLazySpan, LazySpan},
    HirDb, SpannedHirDb,
};
use rustc_hash::FxHashMap;

type CodeSpanFileId = usize;

#[salsa::db(
    common::Jar,
    hir::Jar,
    hir::SpannedJar,
    hir::LowerJar,
    fe_hir_analysis::Jar
)]
pub struct HirAnalysisTestDb {
    storage: salsa::Storage<Self>,
}

impl HirAnalysisTestDb {
    pub fn new_stand_alone(
        &mut self,
        file_name: &str,
        text: &str,
    ) -> (TopLevelMod, HirPropertyFormatter) {
        let kind = IngotKind::StandAlone;
        let version = Version::new(0, 0, 1);
        let ingot = InputIngot::new(self, file_name, kind, version, BTreeSet::default());
        let root = InputFile::new(self, ingot, "test_file.fe".into(), text.to_string());
        ingot.set_root_file(self, root);
        ingot.set_files(self, [root].into());

        let mut prop_formatter = HirPropertyFormatter::default();
        let top_mod = self.register_file(&mut prop_formatter, root);
        (top_mod, prop_formatter)
    }

    fn register_file(
        &self,
        prop_formatter: &mut HirPropertyFormatter,
        input_file: InputFile,
    ) -> TopLevelMod {
        let top_mod = lower::map_file_to_mod(self, input_file);
        let path = input_file.path(self);
        let text = input_file.text(self);
        prop_formatter.register_top_mod(path.as_str(), text, top_mod);
        top_mod
    }
}

impl Default for HirAnalysisTestDb {
    fn default() -> Self {
        let db = Self {
            storage: Default::default(),
        };
        db.prefill();
        db
    }
}

pub struct HirPropertyFormatter {
    properties: BTreeMap<TopLevelMod, Vec<(String, DynLazySpan)>>,
    top_mod_to_file: FxHashMap<TopLevelMod, CodeSpanFileId>,
    code_span_files: SimpleFiles<String, String>,
}

impl HirPropertyFormatter {
    pub fn push_prop(&mut self, top_mod: TopLevelMod, span: DynLazySpan, prop: String) {
        self.properties
            .entry(top_mod)
            .or_default()
            .push((prop, span));
    }

    pub fn finish(&mut self, db: &dyn SpannedHirDb) -> String {
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
                    ((span.file, span.range.start()), diag)
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
        db: &dyn SpannedHirDb,
        top_mod: TopLevelMod,
        prop: &str,
        span: DynLazySpan,
    ) -> (Span, Diagnostic<usize>) {
        let file_id = self.top_mod_to_file[&top_mod];
        let span = span.resolve(db).unwrap();
        let diag = Diagnostic::note()
            .with_labels(vec![Label::primary(file_id, span.range).with_message(prop)]);
        (span, diag)
    }

    fn register_top_mod(&mut self, path: &str, text: &str, top_mod: TopLevelMod) {
        let file_id = self.code_span_files.add(path.to_string(), text.to_string());
        self.top_mod_to_file.insert(top_mod, file_id);
    }
}

impl Default for HirPropertyFormatter {
    fn default() -> Self {
        Self {
            properties: Default::default(),
            top_mod_to_file: Default::default(),
            code_span_files: SimpleFiles::new(),
        }
    }
}

impl salsa::Database for HirAnalysisTestDb {
    fn salsa_event(&self, _: salsa::Event) {}
}
