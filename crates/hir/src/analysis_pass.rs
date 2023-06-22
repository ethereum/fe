use crate::{
    diagnostics::DiagnosticVoucher,
    hir_def::{Func, TopLevelMod},
};

/// All analysis passes that run analysis on the HIR function granularity should
/// implement this trait.
pub trait FunctionPass {
    fn run_on_func(&mut self, function: Func) -> Vec<Box<dyn DiagnosticVoucher>>;
}

/// All analysis passes that run analysis on the HIR top level module
/// granularity should implement this trait.
pub trait ModulePass {
    fn run_on_module(&mut self, module: TopLevelMod) -> Vec<Box<dyn DiagnosticVoucher>>;
}
