use either::Either;
use hir::{
    hir_def::{
        scope_graph::{
            AnonEdge, EdgeKind, FieldEdge, GenericParamEdge, IngotEdge, LexEdge, ModEdge,
            ScopeEdge, ScopeId, SelfEdge, SelfTyEdge, SuperEdge, TraitEdge, TypeEdge, ValueEdge,
            VariantEdge,
        },
        IdentId, Visibility,
    },
    span::DynLazySpan,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{HirAnalysisDb, Spanned};

pub mod import_resolver;
pub mod vis_checker;

pub struct NameResolver<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    importer: &'a dyn Importer,
    cache_store: ResolvedQueryCacheStore,
}

impl<'db, 'a> NameResolver<'db, 'a> {
    pub fn resolve_query(&mut self, scope: ScopeId, query: NameQuery) -> Vec<QueryAnswer> {
        // If the query is already resolved, return the cached result.
        if let Some(answer) = self.cache_store.get(scope, query) {
            return answer.clone();
        };

        // The shadowing rule is `$ = NamedImports > GlobImports > Lex`, where `$` means
        // current scope. This ordering means that greater scope shadows lower
        // scopes having the same name in the same domain and

        // 1. Look for the name in the current scope and named imports.
        let mut results = Vec::new();
        let mut found_scopes = FxHashSet::default();
        let mut parent = None;
        for edge in self.edges(scope) {
            match edge.kind.propagate(self.db, query) {
                PropagatedQuery::Terminated => {
                    if found_scopes.insert(edge.dest) {
                        results.push(QueryAnswer::new(edge.dest, edge.vis, None));
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
                        results.push(QueryAnswer::new(
                            edge.dest,
                            edge.vis,
                            Some(named_import.span.clone()),
                        ));
                    }
                }
                PropagatedQuery::Continuation | PropagatedQuery::UnPropagated => {}
            }
        }

        // If the name is found in the current scope or named imports, we don't need to
        // look for it further.
        if !results.is_empty() {
            self.cache_store.cache_answer(scope, query, results.clone());
            return results;
        }

        // 2. Look for the name in the glob imports.
        for glob_import in self.importer.glob_imports(scope) {
            let edge = &glob_import.data;
            match edge.kind.propagate(self.db, query) {
                PropagatedQuery::Terminated => {
                    if found_scopes.insert(edge.dest) {
                        results.push(QueryAnswer::new_glob(
                            edge.dest,
                            edge.vis,
                            Some(glob_import.span.clone()),
                        ));
                    }
                }
                PropagatedQuery::Continuation | PropagatedQuery::UnPropagated => {}
            }
        }

        // If the name is found in the glob imports, we don't need to look for it
        // further.
        if !results.is_empty() {
            self.cache_store.cache_answer(scope, query, results.clone());
            return results;
        }

        // 3. Look for the name in the lexical scope.
        if let Some(parent) = parent {
            self.cache_store.cache_delegated(scope, query, parent);
            self.resolve_query(parent, query)
        } else {
            self.cache_store.cache_answer(scope, query, vec![]);
            vec![]
        }
    }

    fn edges(&self, scope: ScopeId) -> &'db [ScopeEdge] {
        let graph = scope.top_mod.module_scope_graph(self.db.upcast());
        graph.edges(scope.local_id)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NameQuery {
    name: IdentId,
    domain: NameDomain,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct QueryAnswer {
    pub scope: ScopeId,
    pub vis: Visibility,
    pub import_span: Option<DynLazySpan>,
    pub via_glob_import: bool,
}

impl QueryAnswer {
    pub fn new(scope: ScopeId, vis: Visibility, import_span: Option<DynLazySpan>) -> Self {
        Self {
            scope,
            vis,
            import_span,
            via_glob_import: false,
        }
    }

    pub fn new_glob(scope: ScopeId, vis: Visibility, import_span: Option<DynLazySpan>) -> Self {
        Self {
            scope,
            vis,
            import_span,
            via_glob_import: true,
        }
    }
}

impl QueryAnswer {
    pub fn is_valid(&self) -> bool {
        self.scope.is_valid()
    }
}

#[derive(Default)]
struct ResolvedQueryCacheStore {
    cache: FxHashMap<(ScopeId, NameQuery), Either<Vec<QueryAnswer>, ScopeId>>,
    no_cache: bool,
}

impl ResolvedQueryCacheStore {
    fn cache_answer(&mut self, scope: ScopeId, query: NameQuery, answer: Vec<QueryAnswer>) {
        if self.no_cache {
            return;
        }
        self.cache.insert((scope, query), Either::Left(answer));
    }

    fn cache_delegated(&mut self, scope: ScopeId, query: NameQuery, parent: ScopeId) {
        if self.no_cache {
            return;
        }
        self.cache.insert((scope, query), Either::Right(parent));
    }

    fn get(&self, scope: ScopeId, query: NameQuery) -> Option<Vec<QueryAnswer>> {
        match self.cache.get(&(scope, query)) {
            Some(Either::Left(answers)) => Some(answers.clone()),
            Some(Either::Right(delegated)) => Some(self.get(*delegated, query)?),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NameDomain {
    Item,
    Value,
    Type,
    Field,
    Variant,
}

pub trait Importer {
    fn glob_imports(&self, scope: ScopeId) -> &[Spanned<ScopeEdge>];
    fn named_imports(&self, scope: ScopeId) -> &[Spanned<ScopeEdge>];
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
            NameDomain::Item if self.0 == query.name => PropagatedQuery::Terminated,
            _ => PropagatedQuery::UnPropagated,
        }
    }
}

impl QueryPropagator for TypeEdge {
    fn propagate(&self, _db: &dyn HirAnalysisDb, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameDomain::Item | NameDomain::Type) {
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
        if !matches!(query.domain, NameDomain::Item | NameDomain::Type) {
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
        if !matches!(query.domain, NameDomain::Item | NameDomain::Value) {
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
        if !matches!(query.domain, NameDomain::Item | NameDomain::Type) {
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
        if !matches!(query.domain, NameDomain::Field) {
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
        if !matches!(query.domain, NameDomain::Variant) {
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
        if !matches!(query.domain, NameDomain::Item) {
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
        if !matches!(query.domain, NameDomain::Item) {
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
        if !matches!(query.domain, NameDomain::Item | NameDomain::Type) {
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
        if !matches!(query.domain, NameDomain::Item | NameDomain::Value) {
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
