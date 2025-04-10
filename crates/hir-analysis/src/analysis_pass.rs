use crate::diagnostics::DiagnosticVoucher;
use hir::{
    hir_def::{ModuleTree, TopLevelMod},
    lower::parse_file_impl,
    HirDb, ParserError,
};

/// All analysis passes that run analysis on the HIR top level module
/// granularity should implement this trait.
pub trait ModuleAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>>;
}

#[derive(Default)]
pub struct AnalysisPassManager<'db> {
    module_passes: Vec<Box<dyn ModuleAnalysisPass<'db> + 'db>>,
}

impl<'db> AnalysisPassManager<'db> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_module_pass(&mut self, pass: Box<dyn ModuleAnalysisPass<'db> + 'db>) {
        self.module_passes.push(pass);
    }

    pub fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        let mut diags = vec![];
        for pass in self.module_passes.iter_mut() {
            diags.extend(pass.run_on_module(top_mod));
        }
        diags
    }

    pub fn run_on_module_tree(
        &mut self,
        tree: &'db ModuleTree<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        let mut diags = vec![];
        for module in tree.all_modules() {
            for pass in self.module_passes.iter_mut() {
                diags.extend(pass.run_on_module(module));
            }
        }
        diags
    }
}

#[derive(Clone, Copy)]
pub struct ParsingPass<'db> {
    db: &'db dyn HirDb,
}

impl<'db> ParsingPass<'db> {
    pub fn new(db: &'db dyn HirDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass<'db> for ParsingPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        parse_file_impl::accumulated::<ParserError>(self.db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>()
    }
}
