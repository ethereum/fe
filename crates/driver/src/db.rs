use std::str::FromStr;

use crate::diagnostics::CsDbWrapper;
use camino::{Utf8Path, Utf8PathBuf};
use codespan_reporting::term::{
    self,
    termcolor::{BufferWriter, ColorChoice},
};
use common::{
    diagnostics::CompleteDiagnostic,
    impl_db_traits,
    indexmap::IndexSet,
    input::{input_for_file_path, FilePath, IngotDependency, IngotKind, Version},
    InputDb, InputFile, InputIngot,
};
use hir::{
    hir_def::TopLevelMod,
    lower::{map_file_to_mod, module_tree},
    HirDb, LowerHirDb, SpannedHirDb,
};
use hir_analysis::{
    analysis_pass::{AnalysisPassManager, ParsingPass},
    diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
    name_resolution::{DefConflictAnalysisPass, ImportAnalysisPass, PathAnalysisPass},
    ty::{
        AdtDefAnalysisPass, BodyAnalysisPass, FuncAnalysisPass, ImplAnalysisPass,
        ImplTraitAnalysisPass, TraitAnalysisPass, TypeAliasAnalysisPass,
    },
    HirAnalysisDb,
};
use include_dir::{include_dir, Dir};
use salsa::Setter;

use crate::diagnostics::ToCsDiag;

static LIBRARY: Dir = include_dir!("$CARGO_MANIFEST_DIR/../../library");

#[salsa::db]
pub trait DriverDb:
    salsa::Database + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb + SpannedHirAnalysisDb
{
    fn as_driver_db(&self) -> &dyn DriverDb;
}

#[derive(Default, Clone)]
#[salsa::db]
pub struct DriverDataBase {
    storage: salsa::Storage<Self>,
}
impl_db_traits!(
    DriverDataBase,
    InputDb,
    HirDb,
    LowerHirDb,
    SpannedHirDb,
    HirAnalysisDb,
    SpannedHirAnalysisDb,
    DriverDb,
);

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

    pub fn run_on_ingot(&self, ingot: InputIngot) -> DiagnosticsCollection {
        self.run_on_ingot_with_pass_manager(ingot, initialize_analysis_pass)
    }

    pub fn run_on_ingot_with_pass_manager<'db, F>(
        &'db self,
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

    pub fn standalone(
        &mut self,
        file_path: &Utf8Path,
        source: &str,
        core_ingot: InputIngot,
    ) -> (InputIngot, InputFile) {
        let kind = IngotKind::StandAlone;

        // We set the ingot version to 0.0.0 for stand-alone file.
        let version = Version::new(0, 0, 0);
        let root_file = file_path;
        let core_dependency = IngotDependency::new("core", core_ingot);
        let mut external_ingots = IndexSet::default();
        external_ingots.insert(core_dependency);

        let ingot = InputIngot::new(
            self,
            file_path.parent().unwrap().as_str(),
            kind,
            version,
            external_ingots,
        );

        let file_name = root_file.file_name().unwrap();
        let input_file = input_for_file_path(self.as_input_db(), FilePath::from(self, file_name));
        input_file.set_text(self).to(source.to_string());
        ingot.set_root_file(self, input_file);
        ingot.set_files(self, [input_file].into_iter().collect());
        (ingot, input_file)
    }

    pub fn standalone_no_core(
        &mut self,
        file_path: &Utf8Path,
        source: &str,
    ) -> (InputIngot, InputFile) {
        let kind = IngotKind::StandAlone;

        // We set the ingot version to 0.0.0 for stand-alone file.
        let version = Version::new(0, 0, 0);
        let root_file = file_path;

        let ingot = InputIngot::new(
            self,
            file_path.parent().unwrap().as_str(),
            kind,
            version,
            IndexSet::default(),
        );

        let file_name = root_file.file_name().unwrap();
        let input_file = InputFile::new(self, file_name.into());
        input_file.set_text(self).to(source.to_string());
        ingot.set_root_file(self, input_file);
        ingot.set_files(self, [input_file].into_iter().collect());
        (ingot, input_file)
    }

    pub fn local_ingot(
        &mut self,
        path: &Utf8Path,
        version: &Version,
        source_root: &Utf8Path,
        source_files: Vec<(Utf8PathBuf, String)>,
        core_ingot: InputIngot,
    ) -> (InputIngot, IndexSet<InputFile>) {
        let core_dependency = IngotDependency::new("core", core_ingot);
        let mut external_ingots = IndexSet::default();
        external_ingots.insert(core_dependency);
        let input_ingot = InputIngot::new(
            self,
            path.as_str(),
            IngotKind::Local,
            version.clone(),
            external_ingots,
        );

        let input_files = self.set_ingot_source_files(input_ingot, source_root, source_files);
        (input_ingot, input_files)
    }

    pub fn core_ingot(
        &mut self,
        path: &Utf8Path,
        version: &Version,
        source_root: &Utf8Path,
        source_files: Vec<(Utf8PathBuf, String)>,
    ) -> (InputIngot, IndexSet<InputFile>) {
        let input_ingot = InputIngot::new(
            self,
            path.as_str(),
            IngotKind::Core,
            version.clone(),
            IndexSet::default(),
        );

        let input_files = self.set_ingot_source_files(input_ingot, source_root, source_files);
        (input_ingot, input_files)
    }

    pub fn static_core_ingot(&mut self) -> (InputIngot, IndexSet<InputFile>) {
        let src = LIBRARY
            .get_dir("core/src")
            .expect("static core error. use cli `--core` arg to debug");

        let mut files = vec![];
        write_files_recursive(src, &mut files);

        let input_ingot = InputIngot::new(
            self,
            "core",
            IngotKind::Core,
            Version::new(0, 0, 0),
            IndexSet::default(),
        );

        let input_files = self.set_ingot_source_files(
            input_ingot,
            &Utf8PathBuf::from_str("core/src/lib.fe").unwrap(),
            files,
        );

        (input_ingot, input_files)
    }

    fn set_ingot_source_files(
        &mut self,
        ingot: InputIngot,
        root: &Utf8Path,
        files: Vec<(Utf8PathBuf, String)>,
    ) -> IndexSet<InputFile> {
        let input_files = files
            .into_iter()
            .map(|(path, content)| {
                let input_file = InputFile::new(self, path);
                input_file.set_text(self).to(content);
                input_file
            })
            .collect::<IndexSet<_>>();

        let root_file = *input_files
            .iter()
            .find(|input_file| input_file.path(self) == root)
            .expect("missing root source file");

        ingot.set_files(self, input_files.clone());
        ingot.set_root_file(self, root_file);

        input_files
    }

    pub fn top_mod(&self, ingot: InputIngot, input: InputFile) -> TopLevelMod {
        map_file_to_mod(self, ingot, input)
    }
}

pub struct DiagnosticsCollection<'db>(Vec<Box<dyn DiagnosticVoucher<'db> + 'db>>);
impl<'db> DiagnosticsCollection<'db> {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn emit(&self, db: &'db DriverDataBase) {
        let writer = BufferWriter::stderr(ColorChoice::Auto);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        for diag in self.finalize(db) {
            term::emit(&mut buffer, &config, &CsDbWrapper(db), &diag.to_cs(db)).unwrap();
        }

        eprintln!("{}", std::str::from_utf8(buffer.as_slice()).unwrap());
    }

    /// Format the accumulated diagnostics to a string.
    pub fn format_diags(&self, db: &'db DriverDataBase) -> String {
        let writer = BufferWriter::stderr(ColorChoice::Never);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        for diag in self.finalize(db) {
            term::emit(&mut buffer, &config, &CsDbWrapper(db), &diag.to_cs(db)).unwrap();
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

fn write_files_recursive(dir: &Dir, files: &mut Vec<(Utf8PathBuf, String)>) {
    for file in dir.files() {
        files.push((
            Utf8PathBuf::from_path_buf(file.path().to_path_buf())
                .expect("static core error. use cli `--core` arg  to debug"),
            std::str::from_utf8(file.contents())
                .expect("static core error. use cli `--core` arg  to debug")
                .to_string(),
        ));
    }

    for subdir in dir.dirs() {
        write_files_recursive(subdir, files);
    }
}
