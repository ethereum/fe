use crate::{
    diagnostics::DiagnosticVoucher,
    hir_def::{Func, TopLevelMod},
};

/// All analysis passes that run analysis on the HIR function granularity should
/// implement this trait.
pub trait FuncAnalysisPass {
    fn run_on_func(&mut self, func: Func) -> Vec<Box<dyn DiagnosticVoucher>>;
}

/// All analysis passes that run analysis on the HIR top level module
/// granularity should implement this trait.
pub trait ModuleAnalysisPass {
    fn run_on_module(&mut self, top_mod: TopLevelMod) -> Vec<Box<dyn DiagnosticVoucher>>;
}

pub struct AnalysisPassManager<'db> {
    module_passes: Vec<Box<dyn ModuleAnalysisPass + 'db>>,
    func_passes: Vec<Box<dyn FuncAnalysisPass + 'db>>,
}

impl<'db> AnalysisPassManager<'db> {
    pub fn new() -> Self {
        Self {
            module_passes: vec![],
            func_passes: vec![],
        }
    }

    pub fn add_module_pass(&mut self, pass: Box<dyn ModuleAnalysisPass + 'db>) {
        self.module_passes.push(pass);
    }

    pub fn add_func_pass<T>(&mut self, pass: Box<dyn FuncAnalysisPass + 'db>) {
        self.func_passes.push(pass);
    }

    pub fn run_on_module(&mut self, top_mod: TopLevelMod) -> Vec<Box<dyn DiagnosticVoucher>> {
        let mut diags = vec![];
        for pass in self.module_passes.iter_mut() {
            diags.extend(pass.run_on_module(top_mod.clone()));
        }
        diags
    }

    pub fn run_on_func(&mut self, func: Func) -> Vec<Box<dyn DiagnosticVoucher>> {
        let mut diags = vec![];
        for pass in self.func_passes.iter_mut() {
            diags.extend(pass.run_on_func(func.clone()));
        }
        diags
    }
}
