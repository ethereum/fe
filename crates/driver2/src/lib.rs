pub mod diagnostics;

use std::{collections::BTreeSet, path};

use codespan_reporting::term::{
    self,
    termcolor::{BufferWriter, ColorChoice},
};
use common::{
    input::{IngotKind, Version},
    InputDb, InputFile, InputIngot,
};
use hir::{
    analysis_pass::AnalysisPassManager, diagnostics::DiagnosticVoucher, lower::map_file_to_mod,
    HirDb, LowerHirDb, ParsingPass, SpannedHirDb,
};
use hir_analysis::{
    name_resolution::{DefConflictAnalysisPass, ImportAnalysisPass},
    HirAnalysisDb,
};

use crate::diagnostics::IntoCsDiag;

#[salsa::jar(db = DriverDb)]
pub struct Jar(diagnostics::file_line_starts);

pub trait DriverDb:
    salsa::DbWithJar<Jar> + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{
}

impl<DB> DriverDb for DB where
    DB: Sized + salsa::DbWithJar<Jar> + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{
}

#[salsa::db(common::Jar, hir::Jar, hir_analysis::Jar, Jar)]
pub struct DriverDataBase {
    storage: salsa::Storage<Self>,
    diags: Vec<Box<dyn DiagnosticVoucher>>,
}

impl DriverDataBase {
    // TODO: An temporary implementation for ui testing.
    pub fn run_on_file(&mut self, file_path: &path::Path) {
        let kind = IngotKind::StandAlone;
        // We set the ingot version to 0.0.0 for stand-alone file.
        let version = Version::new(0, 0, 0);
        let root_file = file_path;
        let ingot = InputIngot::new(
            self,
            file_path.parent().unwrap().as_os_str().to_str().unwrap(),
            kind,
            version,
            BTreeSet::new(),
        );

        let file_name = root_file.file_name().unwrap().to_str().unwrap();
        let file_content = std::fs::read_to_string(root_file).unwrap();
        let file = InputFile::new(self, ingot, file_name.into(), file_content);

        ingot.set_root_file(self, file);
        ingot.set_files(self, [file].into());

        let top_mod = map_file_to_mod(self, file);

        self.diags = {
            let mut pass_manager = initialize_analysis_pass(self);
            pass_manager.run_on_module(top_mod)
        };
    }

    /// Prints accumulated diagnostics to stderr.
    pub fn emit_diags(&self) {
        let writer = BufferWriter::stderr(ColorChoice::Auto);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        for diag in &self.diags {
            term::emit(&mut buffer, &config, self, &diag.into_cs(self)).unwrap();
        }

        eprintln!("{}", std::str::from_utf8(buffer.as_slice()).unwrap());
    }

    /// Format the accumulated diagnostics to a string.
    pub fn format_diags(&self) -> String {
        let writer = BufferWriter::stderr(ColorChoice::Never);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        for diag in &self.diags {
            term::emit(&mut buffer, &config, self, &diag.into_cs(self))
                .expect("failed to emit diagnostic");
        }
        std::str::from_utf8(buffer.as_slice()).unwrap().to_string()
    }
}

impl HirDb for DriverDataBase {}
impl SpannedHirDb for DriverDataBase {}
impl LowerHirDb for DriverDataBase {}
impl salsa::Database for DriverDataBase {
    fn salsa_event(&self, _: salsa::Event) {}
}

impl Default for DriverDataBase {
    fn default() -> Self {
        let db = Self {
            storage: Default::default(),
            diags: Vec::new(),
        };
        db.prefill();
        db
    }
}

fn initialize_analysis_pass<'db>(db: &'db DriverDataBase) -> AnalysisPassManager<'db> {
    let mut pass_manager = AnalysisPassManager::new();
    pass_manager.add_module_pass(Box::new(ParsingPass::new(db)));
    pass_manager.add_module_pass(Box::new(DefConflictAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(ImportAnalysisPass::new(db)));
    pass_manager
}
