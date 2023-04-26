use either::Either;
use hir::{
    hir_def::{
        scope_graph::{
            AnonEdge, EdgeKind, FieldEdge, GenericParamEdge, IngotEdge, LexEdge, ModEdge,
            ScopeEdge, ScopeId, SelfEdge, SelfTyEdge, SuperEdge, TraitEdge, TypeEdge, ValueEdge,
            VariantEdge,
        },
        IdentId, PathId,
    },
    span::DynLazySpan,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::HirAnalysisDb;

use super::import_resolver::Importer;

pub struct NameResolver<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    importer: &'a dyn Importer,
    cache_store: ResolvedQueryCacheStore,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedPath {
    FullResolved(ScopeId),
    PartialResolved {
        resolved: ScopeId,
        unresolved_from: usize,
    },
    Failed {
        failed_at: usize,
        cause: NameResolutionFailure,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NameResolutionFailure {
    Conflict,
    Missing,
}

impl<'db, 'a> NameResolver<'db, 'a> {
    pub fn resolve_path(
        &mut self,
        _path: PathId,
        _scope: ScopeId,
        _context: NameContext,
    ) -> ResolvedPath {
        todo!()
    }

    pub fn resolve_query(&mut self, scope: ScopeId, query: NameQuery) -> Vec<ResolvedName> {
        // If the query is already resolved, return the cached result.
        if let Some(resolved) = self.cache_store.get(scope, query) {
            return resolved;
        };

        // The shadowing rule is
        // `$ = NamedImports > GlobImports > Lex > external ingot > builtin types`,
        // where `$` means current scope. This ordering means that
        // greater scope shadows lower scopes having the same name in the same
        // domain and

        // 1. Look for the name in the current scope and named imports.
        let mut results = Vec::new();
        let mut found_scopes = FxHashSet::default();
        let mut parent = None;
        for edge in self.edges(scope) {
            match edge.kind.propagate(self.db, query) {
                PropagatedQuery::Terminated => {
                    if found_scopes.insert(edge.dest) {
                        results.push(ResolvedName::new(edge.dest, None));
                    }
                }

                PropagatedQuery::Continuation => {
                    debug_assert!(parent.is_none());
                    parent = Some(edge.dest);
                }

                PropagatedQuery::UnPropagated => {}
            }
        }

        for named_import in self.importer.named_imports(scope) {
            let edge = &named_import.data;
            match edge.kind.propagate(self.db, query) {
                PropagatedQuery::Terminated => {
                    if found_scopes.insert(edge.dest) {
                        results.push(ResolvedName::new(
                            edge.dest,
                            Some(named_import.span.clone()),
                        ));
                    }
                }
                PropagatedQuery::Continuation | PropagatedQuery::UnPropagated => {}
            }
        }
        if !results.is_empty() {
            self.cache_store
                .cache_resolved(scope, query, results.clone());
            return results;
        }

        // 2. Look for the name in the glob imports.
        for glob_import in self.importer.glob_imports(scope) {
            let edge = &glob_import.data;
            match edge.kind.propagate(self.db, query) {
                PropagatedQuery::Terminated => {
                    if found_scopes.insert(edge.dest) {
                        results.push(ResolvedName::new(edge.dest, Some(glob_import.span.clone())));
                    }
                }
                PropagatedQuery::Continuation | PropagatedQuery::UnPropagated => {}
            }
        }
        if !results.is_empty() {
            self.cache_store
                .cache_resolved(scope, query, results.clone());
            return results;
        }

        // 3. Look for the name in the lexical scope if it exists.
        if let Some(parent) = parent {
            self.cache_store.cache_delegated(scope, query, parent);
            return self.resolve_query(parent, query);
        }

        // 4. Look for the name in the external ingots.
        if query.domain == NameContext::Item {
            for (name, root_mod) in scope.top_mod.external_ingots(self.db.upcast()) {
                if *name == query.name {
                    results.push(ResolvedName::new(ScopeId::root(*root_mod), None));
                }
            }
            // Ensure that all names of external ingots don't conflict with each other.
            debug_assert!(results.len() < 2);
        }
        if !results.is_empty() {
            self.cache_store
                .cache_resolved(scope, query, results.clone());
            return results;
        }

        // 5. Look for the name in the builtin types.
        // TODO: Think about how to handle builtin types.
        self.cache_store
            .cache_resolved(scope, query, results.clone());

        results
    }

    fn edges(&self, scope: ScopeId) -> &'db [ScopeEdge] {
        let graph = scope.top_mod.module_scope_graph(self.db.upcast());
        graph.edges(scope.local_id)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NameQuery {
    name: IdentId,
    domain: NameContext,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ResolvedName {
    pub scope: ScopeId,
    pub import_span: Option<DynLazySpan>,
}

impl ResolvedName {
    pub fn new(scope: ScopeId, import_span: Option<DynLazySpan>) -> Self {
        Self { scope, import_span }
    }
}

impl ResolvedName {
    pub fn is_valid(&self) -> bool {
        self.scope.is_valid()
    }
}

#[derive(Default)]
struct ResolvedQueryCacheStore {
    cache: FxHashMap<(ScopeId, NameQuery), Either<Vec<ResolvedName>, ScopeId>>,
    no_cache: bool,
}

impl ResolvedQueryCacheStore {
    fn cache_resolved(&mut self, scope: ScopeId, query: NameQuery, resolved: Vec<ResolvedName>) {
        if self.no_cache {
            return;
        }
        self.cache.insert((scope, query), Either::Left(resolved));
    }

    fn cache_delegated(&mut self, scope: ScopeId, query: NameQuery, parent: ScopeId) {
        if self.no_cache {
            return;
        }
        self.cache.insert((scope, query), Either::Right(parent));
    }

    fn get(&self, scope: ScopeId, query: NameQuery) -> Option<Vec<ResolvedName>> {
        match self.cache.get(&(scope, query)) {
            Some(Either::Left(resolved)) => Some(resolved.clone()),
            Some(Either::Right(delegated)) => Some(self.get(*delegated, query)?),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NameContext {
    Item,
    Value,
    Type,
    Field,
    Variant,
}

trait QueryPropagator {
    // TODO: `db` is not necessary if we implement prefilled keywords.
    fn propagate(&self, db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PropagatedQuery {
    Terminated,
    Continuation,
    UnPropagated,
}

impl QueryPropagator for LexEdge {
    fn propagate(&self, _db: &dyn HirAnalysisDb, _query: NameQuery) -> PropagatedQuery {
        PropagatedQuery::Continuation
    }
}

impl QueryPropagator for ModEdge {
    fn propagate(&self, _db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery {
        match query.domain {
            NameContext::Item if self.0 == query.name => PropagatedQuery::Terminated,
            _ => PropagatedQuery::UnPropagated,
        }
    }
}

impl QueryPropagator for TypeEdge {
    fn propagate(&self, _db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Item | NameContext::Type) {
            return PropagatedQuery::UnPropagated;
        }

        if self.0 == query.name {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for TraitEdge {
    fn propagate(&self, _db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Item | NameContext::Type) {
            return PropagatedQuery::UnPropagated;
        }

        if self.0 == query.name {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for ValueEdge {
    fn propagate(&self, _db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Item | NameContext::Value) {
            return PropagatedQuery::UnPropagated;
        }

        if self.0 == query.name {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for GenericParamEdge {
    fn propagate(&self, _db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Item | NameContext::Type) {
            return PropagatedQuery::UnPropagated;
        }

        if self.0 == query.name {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for FieldEdge {
    fn propagate(&self, _db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Field) {
            return PropagatedQuery::UnPropagated;
        }

        if self.0 == query.name {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for VariantEdge {
    fn propagate(&self, _db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Variant) {
            return PropagatedQuery::UnPropagated;
        }

        if self.0 == query.name {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for SuperEdge {
    fn propagate(&self, db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Item) {
            return PropagatedQuery::UnPropagated;
        }

        if query.name.is_super(db.upcast()) {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for IngotEdge {
    fn propagate(&self, db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Item) {
            return PropagatedQuery::UnPropagated;
        }

        if query.name.is_ingot(db.upcast()) {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for SelfTyEdge {
    fn propagate(&self, db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Item | NameContext::Type) {
            return PropagatedQuery::UnPropagated;
        }

        if query.name.is_self_ty(db.upcast()) {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for SelfEdge {
    fn propagate(&self, db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Item | NameContext::Value) {
            return PropagatedQuery::UnPropagated;
        }

        if query.name.is_self(db.upcast()) {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for AnonEdge {
    fn propagate(&self, _db: &dyn HirAnalysisDb, _query: NameQuery) -> PropagatedQuery {
        PropagatedQuery::UnPropagated
    }
}

impl QueryPropagator for EdgeKind {
    fn propagate(&self, db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery {
        match self {
            EdgeKind::Lex(edge) => edge.propagate(db, query),
            EdgeKind::Mod(edge) => edge.propagate(db, query),
            EdgeKind::Type(edge) => edge.propagate(db, query),
            EdgeKind::Trait(edge) => edge.propagate(db, query),
            EdgeKind::GenericParam(edge) => edge.propagate(db, query),
            EdgeKind::Value(edge) => edge.propagate(db, query),
            EdgeKind::Field(edge) => edge.propagate(db, query),
            EdgeKind::Variant(edge) => edge.propagate(db, query),
            EdgeKind::Super(edge) => edge.propagate(db, query),
            EdgeKind::Ingot(edge) => edge.propagate(db, query),
            EdgeKind::Self_(edge) => edge.propagate(db, query),
            EdgeKind::SelfTy(edge) => edge.propagate(db, query),
            EdgeKind::Anon(edge) => edge.propagate(db, query),
        }
    }
}
