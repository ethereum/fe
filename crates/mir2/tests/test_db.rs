use std::collections::{BTreeMap, BTreeSet};

// use codespan_reporting::{
//     diagnostic::{Diagnostic, Label},
//     files::SimpleFiles,
//     term::{
//         self,
//         termcolor::{BufferWriter, ColorChoice},
//     },
// };
use common::{
    diagnostics::Span,
    input::{IngotKind, Version},
    InputFile, InputIngot,
};
use hir::{analysis_pass::AnalysisPassManager, hir_def::TopLevelMod, ParsingPass};
use hir_analysis::{
    name_resolution::{DefConflictAnalysisPass, ImportAnalysisPass, PathAnalysisPass},
    ty::{
        FuncAnalysisPass, ImplAnalysisPass, ImplTraitAnalysisPass, TraitAnalysisPass,
        TypeAliasAnalysisPass, TypeDefAnalysisPass,
    },
};
// use hir::{
//     hir_def::TopLevelMod,
//     lower,
//     span::{DynLazySpan, LazySpan},
//     HirDb, SpannedHirDb,
// };
// use rustc_hash::FxHashMap;

type CodeSpanFileId = usize;

#[salsa::db(
    common::Jar,
    hir::Jar,
    hir::SpannedJar,
    hir::LowerJar,
    hir_analysis::Jar
)]
pub struct LowerMirTestDb {
    storage: salsa::Storage<Self>,
}

impl LowerMirTestDb {
    pub fn new_stand_alone(&mut self, file_name: &str, text: &str) {
        let kind = IngotKind::StandAlone;
        let version = Version::new(0, 0, 1);
        let ingot = InputIngot::new(self, file_name, kind, version, BTreeSet::default());
        let root = InputFile::new(self, ingot, "test_file.fe".into(), text.to_string());
        ingot.set_root_file(self, root);
        ingot.set_files(self, [root].into());

        // let top_mod = lower::map_file_to_mod(self, input_file);

        // let mut prop_formatter = HirPropertyFormatter::default();
        // let top_mod = self.register_file(&mut prop_formatter, root);
        // let top_mod = self.register_file(root);
        // top_mod
    }

    pub fn new_std_lib(&mut self) -> TopLevelMod {
        let input = library::std_lib_input_ingot(self);
        lower::map_file_to_mod(self, input_file)
    }

    fn register_file(&self, input_file: InputFile) {
        // let top_mod = lower::map_file_to_mod(self, input_file);
        // let path = input_file.path(self);
        // let text = input_file.text(self);
        // prop_formatter.register_top_mod(path.as_str(), text, top_mod);
        // top_mod
    }
}

impl Default for LowerMirTestDb {
    fn default() -> Self {
        let db = Self {
            storage: Default::default(),
        };
        // db.prefill();
        db
    }
}

// pub struct HirPropertyFormatter {
//     properties: BTreeMap<TopLevelMod, Vec<(String, DynLazySpan)>>,
//     top_mod_to_file: FxHashMap<TopLevelMod, CodeSpanFileId>,
//     code_span_files: SimpleFiles<String, String>,
// }

// impl HirPropertyFormatter {
//     pub fn push_prop(&mut self, top_mod: TopLevelMod, span: DynLazySpan, prop: String) {
//         self.properties
//             .entry(top_mod)
//             .or_default()
//             .push((prop, span));
//     }

//     pub fn finish(&mut self, db: &dyn SpannedHirDb) -> String {
//         let writer = BufferWriter::stderr(ColorChoice::Never);
//         let mut buffer = writer.buffer();
//         let config = term::Config::default();

//         for top_mod in self.top_mod_to_file.keys() {
//             if !self.properties.contains_key(top_mod) {
//                 continue;
//             }

//             let diags = self.properties[top_mod]
//                 .iter()
//                 .map(|(prop, span)| {
//                     let (span, diag) = self.property_to_diag(db, *top_mod, prop, span.clone());
//                     ((span.file, span.range.start()), diag)
//                 })
//                 .collect::<BTreeMap<_, _>>();

//             for diag in diags.values() {
//                 term::emit(&mut buffer, &config, &self.code_span_files, diag).unwrap();
//             }
//         }

//         std::str::from_utf8(buffer.as_slice()).unwrap().to_string()
//     }

//     fn property_to_diag(
//         &self,
//         db: &dyn SpannedHirDb,
//         top_mod: TopLevelMod,
//         prop: &str,
//         span: DynLazySpan,
//     ) -> (Span, Diagnostic<usize>) {
//         let file_id = self.top_mod_to_file[&top_mod];
//         let span = span.resolve(db).unwrap();
//         let diag = Diagnostic::note()
//             .with_labels(vec![Label::primary(file_id, span.range).with_message(prop)]);
//         (span, diag)
//     }

//     fn register_top_mod(&mut self, path: &str, text: &str, top_mod: TopLevelMod) {
//         let file_id = self.code_span_files.add(path.to_string(), text.to_string());
//         self.top_mod_to_file.insert(top_mod, file_id);
//     }
// }

// impl Default for HirPropertyFormatter {
//     fn default() -> Self {
//         Self {
//             properties: Default::default(),
//             top_mod_to_file: Default::default(),
//             code_span_files: SimpleFiles::new(),
//         }
//     }
// }

impl salsa::Database for LowerMirTestDb {
    fn salsa_event(&self, _: salsa::Event) {}
}

pub fn initialize_analysis_pass(db: &LowerMirTestDb) -> AnalysisPassManager<'_> {
    let mut pass_manager = AnalysisPassManager::new();
    pass_manager.add_module_pass(Box::new(ParsingPass::new(db)));
    pass_manager.add_module_pass(Box::new(DefConflictAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(ImportAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(PathAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(TypeDefAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(TypeAliasAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(TraitAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(ImplAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(ImplTraitAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(FuncAnalysisPass::new(db)));
    pass_manager
}