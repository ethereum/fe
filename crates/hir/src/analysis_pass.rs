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
