pub mod diagnostics;

use std::path;

use camino::Utf8PathBuf;
use codespan_reporting::term::{
    self,
    termcolor::{BufferWriter, ColorChoice},
};
use common::{
    diagnostics::CompleteDiagnostic,
    indexmap::{IndexMap, IndexSet},
    input::{IngotDependency, IngotKind, Version},
    InputDb, InputFile, InputIngot,
};
use hir::{
    analysis_pass::AnalysisPassManager,
    diagnostics::DiagnosticVoucher,
    hir_def::TopLevelMod,
    lower::{map_file_to_mod, module_tree},
    HirDb, LowerHirDb, ParsingPass, SpannedHirDb,
};
use hir_analysis::{
    name_resolution::{DefConflictAnalysisPass, ImportAnalysisPass, PathAnalysisPass},
    ty::{
        AdtDefAnalysisPass, BodyAnalysisPass, FuncAnalysisPass, ImplAnalysisPass,
        ImplTraitAnalysisPass, TraitAnalysisPass, TypeAliasAnalysisPass,
    },
    HirAnalysisDb,
};
use resolver::ingot::{dependency_graph::DependencyGraph, src_files::SourceFiles};

use crate::diagnostics::ToCsDiag;

#[salsa::jar(db = DriverDb)]
pub struct Jar(diagnostics::file_line_starts);

pub trait DriverDb:
    salsa::DbWithJar<Jar> + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{
}

impl<DB> DriverDb for DB where
    DB: salsa::DbWithJar<Jar> + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{
}

#[derive(Default)]
#[salsa::db(
    common::Jar,
    hir::Jar,
    hir::LowerJar,
    hir::SpannedJar,
    hir_analysis::Jar,
    Jar
)]
pub struct DriverDataBase {
    storage: salsa::Storage<Self>,
}

impl DriverDataBase {
    // TODO: An temporary implementation for ui testing.
    pub fn run_on_top_mod<'db>(&'db self, top_mod: TopLevelMod<'db>) -> DiagnosticsCollection<'db> {
        self.run_on_file_with_pass_manager(top_mod, initialize_analysis_pass)
    }

    pub fn run_on_file_with_pass_manager<'db, F>(
        &'db self,
        top_mod: TopLevelMod<'db>,
        pm_builder: F,
    ) -> DiagnosticsCollection<'db>
    where
        F: FnOnce(&'db DriverDataBase) -> AnalysisPassManager<'db>,
    {
        let mut pass_manager = pm_builder(self);
        DiagnosticsCollection(pass_manager.run_on_module(top_mod))
    }

    pub fn run_on_ingot<'db>(&'db mut self, ingot: InputIngot) -> DiagnosticsCollection<'db> {
        self.run_on_ingot_with_pass_manager(ingot, initialize_analysis_pass)
    }

    pub fn run_on_ingot_with_pass_manager<'db, F>(
        &'db mut self,
        ingot: InputIngot,
        pm_builder: F,
    ) -> DiagnosticsCollection<'db>
    where
        F: FnOnce(&'db DriverDataBase) -> AnalysisPassManager<'db>,
    {
        let tree = module_tree(self, ingot);
        let mut pass_manager = pm_builder(self);
        DiagnosticsCollection(pass_manager.run_on_module_tree(tree))
    }

    pub fn local_ingot(
        &mut self,
        core_ingot: InputIngot,
        dependency_graph: &DependencyGraph,
    ) -> (InputIngot, IndexMap<Utf8PathBuf, InputIngot>) {
        let mut all_ingots = IndexMap::new();

        for path in dependency_graph.reverse_toposort() {
            let external_ingots = dependency_graph
                .dependencies(&path)
                .into_iter()
                .map(|dependency| IngotDependency {
                    name: dependency.name,
                    ingot: all_ingots[&dependency.target_path],
                })
                .collect();

            all_ingots[&path] = InputIngot::new(
                self,
                &path.to_string(),
                IngotKind::External,
                Version::new(0, 0, 0),
                external_ingots,
            );
        }

        let local_ingot = all_ingots
            .shift_remove(&dependency_graph.local_path)
            .expect("local is missing from input ingots");
        (local_ingot, all_ingots)
    }

    pub fn core_ingot(&mut self, path: &Utf8PathBuf) -> InputIngot {
        todo!();
    }

    pub fn set_ingot_files(&mut self, ingot: InputIngot, files: SourceFiles) {
        todo!()
    }

    pub fn standalone(&mut self, file_path: &path::Path, source: &str) -> InputFile {
        let kind = IngotKind::StandAlone;

        // We set the ingot version to 0.0.0 for stand-alone file.
        let version = Version::new(0, 0, 0);
        let root_file = file_path;
        let ingot = InputIngot::new(
            self,
            file_path.parent().unwrap().as_os_str().to_str().unwrap(),
            kind,
            version,
            IndexSet::new(),
        );

        let file_name = root_file.file_name().unwrap().to_str().unwrap();
        let input_file = InputFile::new(self, ingot, file_name.into(), source.to_string());
        ingot.set_root_file(self, input_file);
        ingot.set_files(self, [input_file].into_iter().collect());
        input_file
    }

    pub fn top_mod(&self, input: InputFile) -> TopLevelMod {
        map_file_to_mod(self, input)
    }
}

impl salsa::Database for DriverDataBase {
    fn salsa_event(&self, _: salsa::Event) {}
}

pub struct DiagnosticsCollection<'db>(Vec<Box<dyn DiagnosticVoucher<'db> + 'db>>);
impl<'db> DiagnosticsCollection<'db> {
    pub fn emit(&self, db: &'db DriverDataBase) {
        let writer = BufferWriter::stderr(ColorChoice::Auto);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        for diag in self.finalize(db) {
            term::emit(&mut buffer, &config, db, &diag.to_cs(db)).unwrap();
        }

        eprintln!("{}", std::str::from_utf8(buffer.as_slice()).unwrap());
    }

    /// Format the accumulated diagnostics to a string.
    pub fn format_diags(&self, db: &'db DriverDataBase) -> String {
        let writer = BufferWriter::stderr(ColorChoice::Never);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        for diag in self.finalize(db) {
            term::emit(&mut buffer, &config, db, &diag.to_cs(db)).unwrap();
        }

        std::str::from_utf8(buffer.as_slice()).unwrap().to_string()
    }

    fn finalize(&self, db: &'db DriverDataBase) -> Vec<CompleteDiagnostic> {
        let mut diags: Vec<_> = self.0.iter().map(|d| d.to_complete(db)).collect();
        diags.sort_by(|lhs, rhs| match lhs.error_code.cmp(&rhs.error_code) {
            std::cmp::Ordering::Equal => lhs.primary_span().cmp(&rhs.primary_span()),
            ord => ord,
        });
        diags
    }
}

fn initialize_analysis_pass(db: &DriverDataBase) -> AnalysisPassManager<'_> {
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
