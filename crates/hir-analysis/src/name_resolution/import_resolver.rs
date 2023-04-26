#![allow(dead_code)]
use hir::hir_def::scope_graph::{ScopeEdge, ScopeId};
use rustc_hash::FxHashMap;

use crate::Spanned;

pub struct ResolvedImports {
    pub resolved: FxHashMap<ScopeId, ScopeEdge>,
}

pub struct ImportResolver {
    resolved: FxHashMap<ScopeId, Vec<Spanned<ScopeEdge>>>,
    glob_resolved: FxHashMap<ScopeId, Vec<Spanned<ScopeEdge>>>,
    states: FxHashMap<ScopeId, ScopeState>,
}

pub trait Importer {
    fn glob_imports(&self, scope: ScopeId) -> &[Spanned<ScopeEdge>];
    fn named_imports(&self, scope: ScopeId) -> &[Spanned<ScopeEdge>];
}

/// This is the state of import resolution for a given scope.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ScopeState {
    // The scope is open, meaning that the scope needs further processing.
    Open,
    // The scope is closed, meaning that the scope is fully resolved.
    Close,
}

impl Importer for ImportResolver {
    fn glob_imports(&self, scope: ScopeId) -> &[Spanned<ScopeEdge>] {
        &self.glob_resolved[&scope]
    }

    fn named_imports(&self, scope: ScopeId) -> &[Spanned<ScopeEdge>] {
        &self.resolved[&scope]
    }
}
