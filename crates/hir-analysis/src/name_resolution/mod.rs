use hir::hir_def::IngotId;

use crate::HirAnalysisDb;

use self::{diagnostics::ImportErrorAccumulator, import_resolver::ResolvedImports};

pub mod diagnostics;
pub mod import_resolver;
pub mod name_resolver;
pub mod visibility_checker;

#[salsa::tracked(return_ref)]
pub fn resolve_imports(db: &dyn HirAnalysisDb, ingot: IngotId) -> ResolvedImports {
    let resolver = import_resolver::ImportResolver::new(db, ingot);
    let (imports, import_error) = resolver.resolve_imports();
    for error in import_error {
        ImportErrorAccumulator::push(db, error);
    }

    imports
}

pub fn resolve_imports_with_diag(
    db: &dyn HirAnalysisDb,
    ingot: IngotId,
) -> (&ResolvedImports, Vec<diagnostics::ImportError>) {
    let imports = resolve_imports(db, ingot);
    let diagnostics = resolve_imports::accumulated::<ImportErrorAccumulator>(db, ingot);
    (imports, diagnostics)
}
