use either::Either;
use hir::{
    hir_def::{
        kw,
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

    /// The path is partially resolved; this means that the `resolved` is a type
    /// and the following segments depend on type to resolve.
    /// These unresolved parts are resolved in the later type inference and
    /// trait solving phases.
    PartialResolved {
        resolved: ScopeId,
        unresolved_from: usize,
    },

    /// The path resolution failed at the given segment.
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
        // where `$` means current scope.
        // This ordering means that greater one shadows lower ones having the same name
        // in the same domain.

        let mut results = Vec::new();
        let mut parent = None;
        // 1. Look for the name in the current scope and named imports.
        let mut found_scopes = FxHashSet::default();
        for edge in self.edges(scope) {
            match edge.kind.propagate(query) {
                PropagatedQuery::Terminated => {
                    if found_scopes.insert(edge.dest) {
                        results.push(ResolvedName::scope(edge.dest, None));
                    }
                }

                PropagatedQuery::Continuation if query.option.allow_lex => {
                    debug_assert!(parent.is_none());
                    parent = Some(edge.dest);
                }

                _ => {}
            }
        }

        for named_import in self.importer.named_imports(scope) {
            let edge = &named_import.data;
            match edge.kind.propagate(query) {
                PropagatedQuery::Terminated => {
                    if found_scopes.insert(edge.dest) {
                        results.push(ResolvedName::scope(
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
            match edge.kind.propagate(query) {
                PropagatedQuery::Terminated => {
                    if found_scopes.insert(edge.dest) {
                        results.push(ResolvedName::scope(
                            edge.dest,
                            Some(glob_import.span.clone()),
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

        // 3. Look for the name in the lexical scope if it exists.
        if let Some(parent) = parent {
            self.cache_store.cache_delegated(scope, query, parent);
            return self.resolve_query(parent, query);
        }

        // 4. Look for the name in the external ingots.
        if query.domain == NameContext::Item {
            for (name, root_mod) in scope.top_mod.external_ingots(self.db.upcast()) {
                if *name == query.name {
                    results.push(ResolvedName::scope(ScopeId::root(*root_mod), None));
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
        if let Some(builtin) = BuiltinName::lookup_for(query.name) {
            results.push(ResolvedName::Builtin(builtin));
        }
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
    option: QueryOption,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ResolvedName {
    Builtin(BuiltinName),
    Scope {
        scope: ScopeId,
        import_span: Option<DynLazySpan>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct QueryOption {
    allow_lex: bool,
}

impl QueryOption {
    pub fn new() -> Self {
        Self { allow_lex: true }
    }

    pub fn disallow_lex(&mut self) -> &mut Self {
        self.allow_lex = false;
        self
    }
}

impl ResolvedName {
    pub fn scope(scope: ScopeId, import_span: Option<DynLazySpan>) -> Self {
        Self::Scope { scope, import_span }
    }

    pub fn builtin(builtin: BuiltinName) -> Self {
        Self::Builtin(builtin)
    }

    pub fn is_valid(&self) -> bool {
        match self {
            Self::Scope { scope, .. } => scope.is_valid(),
            Self::Builtin(_) => true,
        }
    }
}

impl Default for QueryOption {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinName {
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
}

impl BuiltinName {
    /// Returns the builtin name if the `name` is a builtin name.
    pub fn lookup_for(name: IdentId) -> Option<Self> {
        match name {
            kw::BOOL => Self::Bool,
            kw::U8 => Self::U8,
            kw::U16 => Self::U16,
            kw::U32 => Self::U32,
            kw::U64 => Self::U64,
            kw::U128 => Self::U128,
            kw::U256 => Self::U256,
            kw::I8 => Self::I8,
            kw::I16 => Self::I16,
            kw::I32 => Self::I32,
            kw::I64 => Self::I64,
            kw::I128 => Self::I128,
            kw::I256 => Self::I256,
            _ => return None,
        }
        .into()
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
    fn propagate(&self, query: NameQuery) -> PropagatedQuery;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PropagatedQuery {
    Terminated,
    Continuation,
    UnPropagated,
}

impl QueryPropagator for LexEdge {
    fn propagate(&self, _query: NameQuery) -> PropagatedQuery {
        PropagatedQuery::Continuation
    }
}

impl QueryPropagator for ModEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        match query.domain {
            NameContext::Item if self.0 == query.name => PropagatedQuery::Terminated,
            _ => PropagatedQuery::UnPropagated,
        }
    }
}

impl QueryPropagator for TypeEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
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
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
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
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
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
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
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
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
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
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
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
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Item) {
            return PropagatedQuery::UnPropagated;
        }

        if query.name.is_super() {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for IngotEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Item) {
            return PropagatedQuery::UnPropagated;
        }

        if query.name.is_ingot() {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for SelfTyEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Item | NameContext::Type) {
            return PropagatedQuery::UnPropagated;
        }

        if query.name.is_self_ty() {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for SelfEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if !matches!(query.domain, NameContext::Item | NameContext::Value) {
            return PropagatedQuery::UnPropagated;
        }

        if query.name.is_self() {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
}

impl QueryPropagator for AnonEdge {
    fn propagate(&self, _query: NameQuery) -> PropagatedQuery {
        PropagatedQuery::UnPropagated
    }
}

impl QueryPropagator for EdgeKind {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        match self {
            EdgeKind::Lex(edge) => edge.propagate(query),
            EdgeKind::Mod(edge) => edge.propagate(query),
            EdgeKind::Type(edge) => edge.propagate(query),
            EdgeKind::Trait(edge) => edge.propagate(query),
            EdgeKind::GenericParam(edge) => edge.propagate(query),
            EdgeKind::Value(edge) => edge.propagate(query),
            EdgeKind::Field(edge) => edge.propagate(query),
            EdgeKind::Variant(edge) => edge.propagate(query),
            EdgeKind::Super(edge) => edge.propagate(query),
            EdgeKind::Ingot(edge) => edge.propagate(query),
            EdgeKind::Self_(edge) => edge.propagate(query),
            EdgeKind::SelfTy(edge) => edge.propagate(query),
            EdgeKind::Anon(edge) => edge.propagate(query),
        }
    }
}
