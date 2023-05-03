use std::{collections::hash_map::IntoValues, fmt};

use either::Either;
use hir::{
    hir_def::{
        kw,
        scope_graph::{
            AnonEdge, EdgeKind, FieldEdge, GenericParamEdge, IngotEdge, LexEdge, ModEdge,
            ScopeEdge, ScopeId, ScopeKind, SelfEdge, SelfTyEdge, SuperEdge, TraitEdge, TypeEdge,
            ValueEdge, VariantEdge,
        },
        IdentId, ItemKind, Partial, PathId,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedPath {
    Full(ResolvedNameSet),

    /// The path is partially resolved; this means that the `resolved` is a type
    /// and the following segments depend on type to resolve.
    /// These unresolved parts are resolved in the later type inference and
    /// trait solving phases.
    Partial {
        resolved: ResolvedName,
        unresolved_from: usize,
    },
}

impl ResolvedPath {
    pub fn partial(resolved: ResolvedName, unresolved_from: usize) -> Self {
        Self::Partial {
            resolved,
            unresolved_from,
        }
    }
}

#[derive(Debug, derive_more::Display, Clone, PartialEq, Eq, Hash, derive_more::Error)]
#[display(fmt = "failed_at: {failed_at}, kind: {kind}")]
pub struct PathResolutionError {
    pub kind: NameResolutionError,
    pub failed_at: usize,
}

impl PathResolutionError {
    fn new(kind: NameResolutionError, failed_at: usize) -> Self {
        Self { kind, failed_at }
    }

    fn not_found(failed_at: usize) -> Self {
        Self::new(NameResolutionError::NotFound, failed_at)
    }

    fn invalid(failed_at: usize) -> Self {
        Self::new(NameResolutionError::Invalid, failed_at)
    }
}

impl<'db, 'a> NameResolver<'db, 'a> {
    /// Resolve the path to a set of possible resolutions.
    /// A path can be resolved to multiple resolutions because we have multiple
    /// name domains.
    ///
    /// For example, the `foo::FOO` can be resolved to both `const
    /// FOO` and `struct FOO` in the following code:
    /// ```fe
    /// use foo::FOO
    ///
    /// mod foo {
    ///     const FOO: i32 = 1
    ///     struct FOO {}
    /// }
    /// ```
    pub fn resolve_path(
        &mut self,
        path: PathId,
        scope: ScopeId,
    ) -> Result<ResolvedPath, PathResolutionError> {
        let segments = path.segments(self.db.upcast());
        if segments.is_empty() {
            return Err(PathResolutionError::invalid(0));
        }

        // Set pred segment to the current scope.
        let mut pred = ResolvedName::new_scope(scope, None, NameDomain::from_scope(self.db, scope));

        let seg_len = segments.len();
        for (i, seg) in segments[0..seg_len - 1].iter().enumerate() {
            pred = match self.resolve_segment(pred, *seg, i, false)? {
                Either::Left(resolved) => {
                    return Ok(resolved);
                }
                Either::Right(resolved) => resolved,
            }
        }

        match self.resolve_segment(pred, *segments.last().unwrap(), seg_len - 1, true)? {
            Either::Left(resolved) => Ok(resolved),
            Either::Right(_) => {
                unreachable!()
            }
        }
    }

    pub fn resolve_query(
        &mut self,
        scope: ScopeId,
        query: NameQuery,
    ) -> Result<ResolvedNameSet, NameResolutionError> {
        // If the query is already resolved, return the cached result.
        if let Some(resolved) = self.cache_store.get(scope, query) {
            return resolved;
        };

        // The shadowing rule is
        // `$ = NamedImports > GlobImports > Lex > external ingot > builtin types`,
        // where `$` means current scope.
        // This ordering means that greater one shadows lower ones having the same name
        // in the same name context.
        let mut resolutions = Vec::new();
        let mut parent = None;
        // 1. Look for the name in the current scope and named imports.
        let mut found_scopes = FxHashSet::default();
        for edge in self.edges(scope) {
            match edge.kind.propagate(query) {
                PropagatedQuery::Terminated => {
                    if found_scopes.insert(edge.dest) {
                        resolutions.push(ResolvedName::new_scope(
                            edge.dest,
                            None,
                            NameDomain::from_scope(self.db, edge.dest),
                        ));
                    }
                }

                PropagatedQuery::Continuation => {
                    debug_assert!(parent.is_none());
                    parent = Some(edge.dest);
                }

                PropagatedQuery::UnPropagated => {}
            }
        }
        if let Some(imported) = self
            .importer
            .named_imports(scope)
            .and_then(|imports| imports.get(&query.name))
        {
            match imported {
                Ok(imported) => {
                    resolutions.extend(imported.iter().cloned());
                }
                Err(_) => {
                    let err = NameResolutionError::InvalidImport;
                    self.cache_store.cache_result(scope, query, Err(err));
                    return Err(err);
                }
            }
        }
        if let Some(result) = self.store_result_opt(scope, query, resolutions) {
            return result;
        }

        // 2. Look for the name in the glob imports.
        if let Some(imported) = self
            .importer
            .named_imports(scope)
            .and_then(|imports| imports.get(&query.name))
        {
            let imported = imported
                .clone()
                .map_err(|_| NameResolutionError::InvalidImport);
            self.cache_store
                .cache_result(scope, query, imported.clone());
            return imported;
        }

        // 3. Look for the name in the lexical scope if it exists.
        if let Some(parent) = parent {
            self.cache_store.cache_delegated(scope, query, parent);
            return self.resolve_query(parent, query);
        }

        // 4. Look for the name in the external ingots.
        let resolutions: Vec<_> = scope
            .top_mod
            .external_ingots(self.db.upcast())
            .iter()
            .filter_map(|(name, root_mod)| {
                if *name == query.name {
                    Some(ResolvedName::new_scope(
                        ScopeId::root(*root_mod),
                        None,
                        NameDomain::Item,
                    ))
                } else {
                    None
                }
            })
            .collect();

        // Ensure that all names of external ingots don't conflict with each other.
        debug_assert!(resolutions.len() < 2);
        if let Some(result) = self.store_result_opt(scope, query, resolutions) {
            return result;
        }

        // 5. Look for the name in the builtin types.
        let result = if let Some(builtin) = BuiltinName::lookup_for(query.name) {
            Ok(ResolvedName::new_builtin(builtin, builtin.domain()).into())
        } else {
            Err(NameResolutionError::NotFound)
        };

        self.cache_store.cache_result(scope, query, result.clone());
        result
    }

    /// Resolve the `segment`. `pred` is the resolution for the previous
    /// segment, and `is_last` indicates the segment is the last segment of the
    /// path.
    ///
    /// If the method returns `Right`, it means the path resolution is work in
    /// progress and we need to continue look for the next segment. If the
    /// method returns `Left`, that means the resolution for the entire path
    /// is done.
    ///
    /// Even if the `is_last` is `false` the method may return `Left`, this will
    /// happen if both 1. and 2. are satisfied:
    /// 1. The `pred` is a type.
    /// 2. The lookup for the `segment` results in `NotFound`.
    /// This indicates we need further resolution for the `segment` in the later
    /// trait solving phase.
    /// In case the `is_last` is `true`, the function is guaranteed to return
    /// `Ok(Left)` or `Error`.
    ///
    ///
    /// We can return an error immediately in case the `is_last` is `false` and
    /// multiple resolutions for the `segment` are found.
    /// The reasoning is
    /// 1. Our language allows only `Item` domain to have associated items.
    /// 2. By 1., the middle segments should be resolved to the `Item`
    ///    domain. Otherwise, the following segment can't be resolved.
    /// 3. By 2., if we obtain multiple resolutions from a middle segment, this
    ///    can be divided into two cases:
    ///    a. Name conflict occurs. We can immediately return `Conflict` error
    ///       in this case.
    ///    b. All resolutions belong to different domains. This
    ///       means that at least one of the resolutions belongs to non-`Item`
    ///       domain. This case can be regarded as `NotFound` error because the
    ///       following segment of the non-`Item` domain resolution can't be
    ///       resolved.
    fn resolve_segment(
        &mut self,
        pred: ResolvedName,
        segment: Partial<IdentId>,
        seg_idx: usize,
        is_last: bool,
    ) -> Result<Either<ResolvedPath, ResolvedName>, PathResolutionError> {
        let Partial::Present(seg) = segment else {
            return Err(PathResolutionError::invalid(seg_idx));
        };

        let Some(scope) = pred.scope()
            else {
                // If pred is a builtin type, then the path resolution is done.
                if pred.is_type(self.db) {
                    return Ok(Either::Left(ResolvedPath::partial(pred, seg_idx)));
                }  else {
                    return Err(PathResolutionError::not_found(seg_idx));
                }
        };

        let query = NameQuery::new(seg);
        let resolved_set = match self.resolve_query(scope, query) {
            Ok(resolved) => resolved,
            Err(NameResolutionError::NotFound) if pred.is_type(self.db) => {
                // If the parent scope of the current segment is a type and the segment is not
                // found, then it should be resolved in the trait solving phase.
                return Ok(Either::Left(ResolvedPath::partial(pred, seg_idx)));
            }
            Err(e) => {
                return Err(PathResolutionError::new(e, seg_idx));
            }
        };

        if is_last {
            Ok(Either::Left(ResolvedPath::Full(resolved_set)))
        } else if resolved_set.num() > 1 {
            // Case a. is already handled above.
            // Handles case b. here.
            return Err(PathResolutionError::not_found(seg_idx));
        } else {
            Ok(Either::Right(resolved_set.into_iter().next().unwrap()))
        }
    }

    /// Convert the `resolutions` into `ResolvedNameSet` and store it to the
    /// cache store.
    /// If the `resolutions` is empty return `None` instead of
    /// returning an error.
    fn store_result_opt(
        &mut self,
        scope: ScopeId,
        query: NameQuery,
        resolutions: Vec<ResolvedName>,
    ) -> Option<Result<ResolvedNameSet, NameResolutionError>> {
        if !resolutions.is_empty() {
            let result = ResolvedNameSet::from_resolutions(resolutions);
            self.cache_store.cache_result(scope, query, result.clone());
            Some(result)
        } else {
            None
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
    option: QueryOption,
}

impl NameQuery {
    /// Create a new name query with the default query option.
    pub fn new(name: IdentId) -> Self {
        Self {
            name,
            option: Default::default(),
        }
    }

    /// Create a new name query with the given query option.
    pub fn with_option(name: IdentId, option: QueryOption) -> Self {
        Self { name, option }
    }
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct QueryOption {
    /// If `allow_lex` is true, then the query will be propagated to the lexical
    /// scope if the name is not found in the current scope.
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

impl Default for QueryOption {
    fn default() -> Self {
        Self::new()
    }
}

/// The struct contains the lookup result of a name query.
/// The results can contain more than one resolved names which belong to
/// different name domains.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ResolvedNameSet {
    names: FxHashMap<NameDomain, ResolvedName>,
}

impl ResolvedNameSet {
    /// Returns the number of resolved names.
    pub fn num(&self) -> usize {
        self.names.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &ResolvedName> {
        self.names.values()
    }

    /// Returns the resolved name of the given `domain`.
    pub fn name_by_domain(&self, domain: NameDomain) -> Option<&ResolvedName> {
        self.names.get(&domain)
    }

    fn from_resolutions(resolutions: Vec<ResolvedName>) -> Result<Self, NameResolutionError> {
        if resolutions.is_empty() {
            return Err(NameResolutionError::NotFound);
        }
        let mut names = FxHashMap::default();
        for resolution in resolutions {
            let domain = resolution.domain;
            if names.insert(domain, resolution).is_some() {
                return Err(NameResolutionError::Conflict);
            }
        }

        Ok(Self { names })
    }
}

impl IntoIterator for ResolvedNameSet {
    type Item = ResolvedName;
    type IntoIter = IntoValues<NameDomain, ResolvedName>;

    fn into_iter(self) -> Self::IntoIter {
        self.names.into_values()
    }
}

impl From<ResolvedName> for ResolvedNameSet {
    fn from(resolution: ResolvedName) -> Self {
        let mut names = FxHashMap::default();
        names.insert(resolution.domain, resolution);
        Self { names }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ResolvedName {
    pub kind: ResolvedNameKind,
    pub domain: NameDomain,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameResolutionError {
    /// The name is not found.
    NotFound,
    /// Multiple candidates are found, no need to report the error from the use
    /// site because it's should be emitted from a def site.
    /// The associated value is the first candidate.
    Conflict,

    /// The name is found as the imported name, but the name resolution for the
    /// import itself failed. No need to report the error from the use site
    /// because it's should be emitted from a import resolution phase.
    InvalidImport,

    /// The name is invalid in parsing. Basically, no need to report it because
    /// the error is already emitted from parsing phase.
    Invalid,
}

impl ResolvedName {
    pub fn is_type(&self, db: &dyn HirAnalysisDb) -> bool {
        match self.kind {
            ResolvedNameKind::Builtin(builtin) => builtin.is_type(),
            ResolvedNameKind::Scope { scope, .. } => scope.is_type(db.upcast()),
        }
    }

    pub fn scope(&self) -> Option<ScopeId> {
        match self.kind {
            ResolvedNameKind::Builtin(_) => None,
            ResolvedNameKind::Scope { scope, .. } => Some(scope),
        }
    }

    fn new_scope(scope: ScopeId, import_span: Option<DynLazySpan>, domain: NameDomain) -> Self {
        Self {
            kind: ResolvedNameKind::Scope { scope, import_span },
            domain,
        }
    }

    fn new_builtin(builtin: BuiltinName, domain: NameDomain) -> Self {
        Self {
            kind: ResolvedNameKind::Builtin(builtin),
            domain,
        }
    }
}

impl fmt::Display for NameResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NameResolutionError::NotFound => write!(f, "name not found"),
            NameResolutionError::Conflict => write!(f, "multiple candidates found"),
            NameResolutionError::InvalidImport => write!(f, "invalid import"),
            NameResolutionError::Invalid => write!(f, "invalid name"),
        }
    }
}

impl std::error::Error for NameResolutionError {}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ResolvedNameKind {
    Builtin(BuiltinName),
    Scope {
        scope: ScopeId,
        import_span: Option<DynLazySpan>,
    },
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

    pub fn domain(self) -> NameDomain {
        // Currently all builtin belong to the item domain.
        match self {
            Self::Bool
            | Self::U8
            | Self::U16
            | Self::U32
            | Self::U64
            | Self::U128
            | Self::U256
            | Self::I8
            | Self::I16
            | Self::I32
            | Self::I64
            | Self::I128
            | Self::I256 => NameDomain::Item,
        }
    }

    pub fn is_type(self) -> bool {
        // Currently all builtin names are types.
        match self {
            Self::Bool
            | Self::U8
            | Self::U16
            | Self::U32
            | Self::U64
            | Self::U128
            | Self::U256
            | Self::I8
            | Self::I16
            | Self::I32
            | Self::I64
            | Self::I128
            | Self::I256 => true,
        }
    }
}

#[derive(Default)]
struct ResolvedQueryCacheStore {
    cache: FxHashMap<
        (ScopeId, NameQuery),
        Either<Result<ResolvedNameSet, NameResolutionError>, ScopeId>,
    >,
    no_cache: bool,
}

impl ResolvedQueryCacheStore {
    fn cache_result(
        &mut self,
        scope: ScopeId,
        query: NameQuery,
        result: Result<ResolvedNameSet, NameResolutionError>,
    ) {
        if self.no_cache {
            return;
        }
        self.cache.insert((scope, query), Either::Left(result));
    }

    fn cache_delegated(&mut self, scope: ScopeId, query: NameQuery, parent: ScopeId) {
        if self.no_cache {
            return;
        }
        self.cache.insert((scope, query), Either::Right(parent));
    }

    fn get(
        &self,
        scope: ScopeId,
        query: NameQuery,
    ) -> Option<Result<ResolvedNameSet, NameResolutionError>> {
        match self.cache.get(&(scope, query)) {
            Some(Either::Left(resolved)) => Some(resolved.clone()),
            Some(Either::Right(delegated)) => Some(self.get(*delegated, query)?),
            _ => None,
        }
    }
}

/// Each resolved name is associated with a domain that indicates which domain
/// the name belongs to.
/// The multiple same names can be introduced in a same scope as long as they
/// are in a different domain.
///
/// E.g., A `Foo` can be introduced in a same scope as a type and variant at the
/// same time. This means the code below is valid.
/// ```fe
/// struct Foo {}
/// enum MyEnum {
///     Foo
/// }
/// use MyEnum::Foo
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NameDomain {
    /// The domain is associated with all items except for items that belongs to
    /// the `Value` domain.
    Item,
    /// The domain is associated with a local variable and items that are
    /// guaranteed not to have associated names. e.g., `fn` or `const`.
    Value,
    /// The domain is associated with struct fields.
    Field,
    /// The domain is associated with enum variants.
    Variant,
}

impl NameDomain {
    fn from_scope(db: &dyn HirAnalysisDb, scope: ScopeId) -> Self {
        match scope.data(db.upcast()).kind {
            ScopeKind::Item(ItemKind::Func(_) | ItemKind::Const(_)) | ScopeKind::FnParam(_) => {
                Self::Value
            }
            ScopeKind::Item(_) | ScopeKind::GenericParam(_) => Self::Item,
            ScopeKind::Field(_) => Self::Field,
            ScopeKind::Variant(_) => Self::Variant,
        }
    }
}

trait QueryPropagator {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery;
    fn propagate_glob(&self) -> PropagatedQuery;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PropagatedQuery {
    Terminated,
    Continuation,
    UnPropagated,
}

impl QueryPropagator for LexEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if query.option.allow_lex {
            PropagatedQuery::Continuation
        } else {
            PropagatedQuery::UnPropagated
        }
    }

    fn propagate_glob(&self) -> PropagatedQuery {
        PropagatedQuery::UnPropagated
    }
}

impl QueryPropagator for ModEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if self.0 == query.name {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }

    fn propagate_glob(&self) -> PropagatedQuery {
        PropagatedQuery::Terminated
    }
}

impl QueryPropagator for TypeEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if self.0 == query.name {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }

    fn propagate_glob(&self) -> PropagatedQuery {
        PropagatedQuery::Terminated
    }
}

impl QueryPropagator for TraitEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if self.0 == query.name {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
    fn propagate_glob(&self) -> PropagatedQuery {
        PropagatedQuery::Terminated
    }
}

impl QueryPropagator for ValueEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if self.0 == query.name {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }
    fn propagate_glob(&self) -> PropagatedQuery {
        PropagatedQuery::Terminated
    }
}

impl QueryPropagator for GenericParamEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if self.0 == query.name {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }

    fn propagate_glob(&self) -> PropagatedQuery {
        PropagatedQuery::UnPropagated
    }
}

impl QueryPropagator for FieldEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if self.0 == query.name {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }

    fn propagate_glob(&self) -> PropagatedQuery {
        PropagatedQuery::UnPropagated
    }
}

impl QueryPropagator for VariantEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if self.0 == query.name {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }

    fn propagate_glob(&self) -> PropagatedQuery {
        PropagatedQuery::Terminated
    }
}

impl QueryPropagator for SuperEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if query.name.is_super() {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }

    fn propagate_glob(&self) -> PropagatedQuery {
        PropagatedQuery::UnPropagated
    }
}

impl QueryPropagator for IngotEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if query.name.is_ingot() {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }

    fn propagate_glob(&self) -> PropagatedQuery {
        PropagatedQuery::UnPropagated
    }
}

impl QueryPropagator for SelfTyEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if query.name.is_self_ty() {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }

    fn propagate_glob(&self) -> PropagatedQuery {
        PropagatedQuery::UnPropagated
    }
}

impl QueryPropagator for SelfEdge {
    fn propagate(&self, query: NameQuery) -> PropagatedQuery {
        if query.name.is_self() {
            PropagatedQuery::Terminated
        } else {
            PropagatedQuery::UnPropagated
        }
    }

    fn propagate_glob(&self) -> PropagatedQuery {
        PropagatedQuery::UnPropagated
    }
}

impl QueryPropagator for AnonEdge {
    fn propagate(&self, _query: NameQuery) -> PropagatedQuery {
        PropagatedQuery::UnPropagated
    }

    fn propagate_glob(&self) -> PropagatedQuery {
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

    fn propagate_glob(&self) -> PropagatedQuery {
        match self {
            EdgeKind::Lex(edge) => edge.propagate_glob(),
            EdgeKind::Mod(edge) => edge.propagate_glob(),
            EdgeKind::Type(edge) => edge.propagate_glob(),
            EdgeKind::Trait(edge) => edge.propagate_glob(),
            EdgeKind::GenericParam(edge) => edge.propagate_glob(),
            EdgeKind::Value(edge) => edge.propagate_glob(),
            EdgeKind::Field(edge) => edge.propagate_glob(),
            EdgeKind::Variant(edge) => edge.propagate_glob(),
            EdgeKind::Super(edge) => edge.propagate_glob(),
            EdgeKind::Ingot(edge) => edge.propagate_glob(),
            EdgeKind::Self_(edge) => edge.propagate_glob(),
            EdgeKind::SelfTy(edge) => edge.propagate_glob(),
            EdgeKind::Anon(edge) => edge.propagate_glob(),
        }
    }
}
