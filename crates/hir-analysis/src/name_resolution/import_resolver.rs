#![allow(dead_code)]
use hir::hir_def::{
    scope_graph::{ScopeEdge, ScopeId},
    IdentId,
};
use rustc_hash::FxHashMap;

use super::name_resolver::{NameResolutionError, ResolvedNameSet};

pub struct ResolvedImports {
    pub resolved: FxHashMap<ScopeId, ScopeEdge>,
}

pub struct ImportResolver {
    resolved: FxHashMap<ScopeId, ResolvedImportSet>,
    glob_resolved: FxHashMap<ScopeId, ResolvedImportSet>,
    states: FxHashMap<ScopeId, ScopeState>,
}

pub trait Importer {
    fn named_imports(&self, scope: ScopeId) -> Option<&ResolvedImportSet>;
    fn glob_imports(&self, scope: ScopeId) -> Option<&ResolvedImportSet>;
}

pub(super) type ResolvedImportSet =
    FxHashMap<IdentId, Result<ResolvedNameSet, NameResolutionError>>;

/// This is the state of import resolution for a given scope.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ScopeState {
    // The scope is open, meaning that the scope needs further processing.
    Open,
    // The scope is closed, meaning that the scope is fully resolved.
    Close,
}

impl Importer for ImportResolver {
    fn named_imports(&self, scope: ScopeId) -> Option<&ResolvedImportSet> {
        self.resolved.get(&scope)
    }

    fn glob_imports(&self, scope: ScopeId) -> Option<&ResolvedImportSet> {
        self.glob_resolved.get(&scope)
    }
}
