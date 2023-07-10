use std::{
    cmp,
    collections::hash_map::{Entry, IntoValues},
    fmt, mem,
};

use either::Either;
use hir::{
    hir_def::{
        prim_ty::PrimTy,
        scope_graph::{
            AnonEdge, EdgeKind, FieldEdge, GenericParamEdge, IngotEdge, LexEdge, ModEdge, ScopeId,
            SelfEdge, SelfTyEdge, SuperEdge, TraitEdge, TypeEdge, ValueEdge, VariantEdge,
        },
        IdentId, ItemKind, Use,
    },
    span::DynLazySpan,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::HirAnalysisDb;

use super::{
    import_resolver::Importer,
    visibility_checker::{is_scope_visible_from, is_use_visible},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NameQuery {
    /// The name to be resolved.
    name: IdentId,
    /// The scope where the name is resolved.
    scope: ScopeId,
    directive: QueryDirective,
}

impl NameQuery {
    /// Create a new name query with the default query directive.
    pub fn new(name: IdentId, scope: ScopeId) -> Self {
        Self {
            name,
            scope,
            directive: Default::default(),
        }
    }

    /// Create a new name query with the given query directive.
    pub fn with_directive(name: IdentId, scope: ScopeId, directive: QueryDirective) -> Self {
        Self {
            name,
            scope,
            directive,
        }
    }

    pub fn name(&self) -> IdentId {
        self.name
    }
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct QueryDirective {
    /// If `allow_lex` is `true`, then the query will be propagated to the
    /// lexical scope if the name is not found in the current scope.
    allow_lex: bool,

    /// If `allow_external` is `true`, then the query will be propagated to the
    /// external ingot and builtin types as well.
    allow_external: bool,

    /// If `allow_glob` is `true`, then the resolver uses the glob import to
    /// resolve the name.
    allow_glob: bool,
}

impl QueryDirective {
    /// Make a new query directive with the default settings.
    /// The default setting is to lookup the name in the lexical scope and all
    /// imports and external ingots.
    pub fn new() -> Self {
        Self {
            allow_lex: true,
            allow_external: true,
            allow_glob: true,
        }
    }

    /// Disallow lexical scope lookup.
    pub fn disallow_lex(&mut self) -> &mut Self {
        self.allow_lex = false;
        self
    }

    pub(super) fn disallow_external(&mut self) -> &mut Self {
        self.allow_external = false;
        self
    }

    pub(super) fn disallow_glob(&mut self) -> &mut Self {
        self.allow_glob = false;
        self
    }
}

impl Default for QueryDirective {
    fn default() -> Self {
        Self::new()
    }
}

/// The struct contains the lookup result of a name query.
/// The results can contain more than one name resolution which belong to
/// different name domains.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ResBucket {
    pub(super) bucket: FxHashMap<NameDomain, NameResolutionResult<NameRes>>,
}

impl ResBucket {
    /// Returns the number of resolutions in the bucket.
    pub fn len(&self) -> usize {
        self.iter().count()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = &NameRes> {
        self.bucket.values().filter_map(|res| res.as_ref().ok())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut NameRes> {
        self.bucket.values_mut().filter_map(|res| res.as_mut().ok())
    }

    pub fn errors(&self) -> impl Iterator<Item = &NameResolutionError> {
        self.bucket.values().filter_map(|res| res.as_ref().err())
    }

    /// Returns the resolution of the given `domain`.
    pub fn res_by_domain(&self, domain: NameDomain) -> &NameResolutionResult<NameRes> {
        self.bucket
            .get(&domain)
            .unwrap_or(&Err(NameResolutionError::NotFound))
    }

    pub fn filter_by_domain(&mut self, domain: NameDomain) {
        self.bucket.retain(|d, _| *d == domain);
    }

    /// Merge the `resolutions` into the set. If name conflict happens, the old
    /// resolution will be returned, otherwise `None` will be returned.
    pub(super) fn merge<'a>(&mut self, resolutions: impl Iterator<Item = &'a NameRes>) {
        for res in resolutions {
            self.push(res);
        }
    }

    pub(super) fn set_derivation(&mut self, derivation: NameDerivation) {
        for res in self.iter_mut() {
            res.derivation = derivation.clone();
        }
    }

    /// Push the `res` into the set.
    fn push(&mut self, res: &NameRes) {
        let domain = res.domain;
        match self.bucket.entry(domain) {
            Entry::Occupied(mut e) => {
                let old_res = match e.get_mut() {
                    Ok(res) => res,
                    Err(NameResolutionError::NotFound) => {
                        e.insert(Ok(res.clone())).ok();
                        return;
                    }
                    Err(NameResolutionError::Ambiguous(ambiguous_set)) => {
                        if ambiguous_set[0].derivation == res.derivation {
                            ambiguous_set.push(res.clone());
                        }
                        return;
                    }
                    Err(_) => {
                        return;
                    }
                };

                let old_derivation = old_res.derivation.clone();
                match res.derivation.cmp(&old_derivation) {
                    cmp::Ordering::Less => {}
                    cmp::Ordering::Equal => {
                        if old_res.kind != res.kind {
                            let old_res_cloned = old_res.clone();
                            let res = res.clone();
                            e.insert(Err(NameResolutionError::Ambiguous(vec![
                                old_res_cloned,
                                res,
                            ])))
                            .ok();
                        }
                    }
                    cmp::Ordering::Greater => {
                        e.insert(Ok(res.clone())).ok();
                    }
                }
            }

            Entry::Vacant(e) => {
                e.insert(Ok(res.clone()));
            }
        }
    }

    fn set_lexed_derivation(&mut self) {
        for res in self.iter_mut() {
            res.derivation.lexed()
        }
    }
}

impl IntoIterator for ResBucket {
    type Item = NameResolutionResult<NameRes>;
    type IntoIter = IntoValues<NameDomain, NameResolutionResult<NameRes>>;

    fn into_iter(self) -> Self::IntoIter {
        self.bucket.into_values()
    }
}

impl From<NameRes> for ResBucket {
    fn from(res: NameRes) -> Self {
        let mut names = FxHashMap::default();
        names.insert(res.domain, Ok(res));
        Self { bucket: names }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NameRes {
    pub kind: NameResKind,
    pub domain: NameDomain,
    pub derivation: NameDerivation,
}

impl NameRes {
    pub fn is_type(&self, db: &dyn HirAnalysisDb) -> bool {
        match self.kind {
            NameResKind::Prim(_) => true,
            NameResKind::Scope(scope) => scope.is_type(db.as_hir_db()),
        }
    }

    /// Returns the scope of the name resolution if the name is not a builtin
    /// type.
    pub fn scope(&self) -> Option<ScopeId> {
        match self.kind {
            NameResKind::Scope(scope) => Some(scope),
            NameResKind::Prim(_) => None,
        }
    }

    pub fn is_visible(&self, db: &dyn HirAnalysisDb, from: ScopeId) -> bool {
        let scope_or_use = match self.derivation {
            NameDerivation::Def | NameDerivation::Prim | NameDerivation::External => {
                match self.kind {
                    NameResKind::Scope(scope) => Either::Left(scope),
                    NameResKind::Prim(_) => return true,
                }
            }
            NameDerivation::NamedImported(use_) | NameDerivation::GlobImported(use_) => {
                Either::Right(use_)
            }
            NameDerivation::Lex(ref inner) => {
                let mut inner = inner;
                while let NameDerivation::Lex(parent) = inner.as_ref() {
                    inner = parent;
                }

                return Self {
                    derivation: inner.as_ref().clone(),
                    ..self.clone()
                }
                .is_visible(db, from);
            }
        };

        match scope_or_use {
            Either::Left(target_scope) => is_scope_visible_from(db, target_scope, from),
            Either::Right(use_) => is_use_visible(db, from, use_),
        }
    }

    pub fn pretty_path(&self, db: &dyn HirAnalysisDb) -> Option<String> {
        match self.kind {
            NameResKind::Scope(scope) => scope.pretty_path(db.as_hir_db()),
            NameResKind::Prim(prim) => prim.name().data(db.as_hir_db()).clone().into(),
        }
    }

    pub(super) fn derived_from(&self, db: &dyn HirAnalysisDb) -> Option<DynLazySpan> {
        match self.derivation {
            NameDerivation::Def | NameDerivation::Prim | NameDerivation::External => {
                self.kind.name_span(db)
            }
            NameDerivation::NamedImported(use_) => use_.imported_name_span(db.as_hir_db()),
            NameDerivation::GlobImported(use_) => use_.glob_span(db.as_hir_db()),
            NameDerivation::Lex(ref inner) => {
                let mut inner = inner;
                while let NameDerivation::Lex(parent) = inner.as_ref() {
                    inner = parent;
                }
                Self {
                    derivation: inner.as_ref().clone(),
                    ..self.clone()
                }
                .derived_from(db)
            }
        }
    }

    pub(super) fn new_from_scope(
        scope: ScopeId,
        domain: NameDomain,
        derivation: NameDerivation,
    ) -> Self {
        Self {
            kind: scope.into(),
            derivation,
            domain,
        }
    }

    fn new_prim(prim: PrimTy) -> Self {
        Self {
            kind: prim.into(),
            derivation: NameDerivation::Prim,
            domain: NameDomain::Item,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, derive_more::From)]
pub enum NameResKind {
    Scope(ScopeId),
    Prim(PrimTy),
}

impl NameResKind {
    pub fn name_span(self, db: &dyn HirAnalysisDb) -> Option<DynLazySpan> {
        match self {
            NameResKind::Scope(scope) => scope.name_span(db.as_hir_db()),
            NameResKind::Prim(_) => None,
        }
    }

    pub fn name(self, db: &dyn HirAnalysisDb) -> Option<IdentId> {
        match self {
            NameResKind::Scope(scope) => scope.name(db.as_hir_db()),
            NameResKind::Prim(prim) => prim.name().into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NameDerivation {
    Def,
    NamedImported(Use),
    GlobImported(Use),
    Lex(Box<NameDerivation>),
    External,
    Prim,
}

impl NameDerivation {
    fn lexed(&mut self) {
        let inner = mem::replace(self, NameDerivation::Def);
        *self = NameDerivation::Lex(Box::new(inner));
    }
}

impl PartialOrd for NameDerivation {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        match (self, other) {
            (NameDerivation::Def, NameDerivation::Def) => Some(cmp::Ordering::Equal),
            (NameDerivation::Def, _) => Some(cmp::Ordering::Greater),
            (_, NameDerivation::Def) => Some(cmp::Ordering::Less),

            (NameDerivation::NamedImported(_), NameDerivation::NamedImported(_)) => {
                Some(cmp::Ordering::Equal)
            }
            (NameDerivation::NamedImported(_), _) => Some(cmp::Ordering::Greater),
            (_, NameDerivation::NamedImported(_)) => Some(cmp::Ordering::Less),

            (NameDerivation::GlobImported(_), NameDerivation::GlobImported(_)) => {
                Some(cmp::Ordering::Equal)
            }
            (NameDerivation::GlobImported(_), _) => Some(cmp::Ordering::Greater),
            (_, NameDerivation::GlobImported(_)) => Some(cmp::Ordering::Less),

            (NameDerivation::Lex(lhs), NameDerivation::Lex(rhs)) => lhs.partial_cmp(rhs),
            (NameDerivation::Lex(_), _) => Some(cmp::Ordering::Greater),
            (_, NameDerivation::Lex(_)) => Some(cmp::Ordering::Less),

            (NameDerivation::External, NameDerivation::External) => Some(cmp::Ordering::Equal),
            (NameDerivation::External, _) => Some(cmp::Ordering::Greater),
            (_, NameDerivation::External) => Some(cmp::Ordering::Less),

            (NameDerivation::Prim, NameDerivation::Prim) => Some(cmp::Ordering::Equal),
        }
    }
}

pub(crate) struct NameResolver<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    importer: &'a dyn Importer,
    cache_store: ResolvedQueryCacheStore,
}

impl<'db, 'a> NameResolver<'db, 'a> {
    pub(super) fn new(db: &'db dyn HirAnalysisDb, importer: &'a dyn Importer) -> Self {
        Self {
            db,
            importer,
            cache_store: Default::default(),
        }
    }

    pub(super) fn new_no_cache(db: &'db dyn HirAnalysisDb, importer: &'a dyn Importer) -> Self {
        let cache_store = ResolvedQueryCacheStore {
            no_cache: true,
            ..Default::default()
        };
        Self {
            db,
            importer,
            cache_store,
        }
    }

    pub(super) fn into_cache_store(self) -> ResolvedQueryCacheStore {
        self.cache_store
    }

    pub(crate) fn resolve_query(&mut self, query: NameQuery) -> ResBucket {
        // If the query is already resolved, return the cached result.
        if let Some(resolved) = self.cache_store.get(query) {
            return resolved.clone();
        };

        let mut bucket = ResBucket::default();

        // The shadowing rule is
        // `$ > NamedImports > GlobImports > Lex > external ingot > builtin types`,
        // where `$` means current scope.
        // This ordering means that greater one shadows lower ones in the same domain.
        let mut parent = None;

        // 1. Look for the name in the current scope.
        let mut found_scopes = FxHashSet::default();
        for edge in query.scope.edges(self.db.as_hir_db()) {
            match edge.kind.propagate(&query) {
                PropagationResult::Terminated => {
                    if found_scopes.insert(edge.dest) {
                        let res = NameRes::new_from_scope(
                            edge.dest,
                            NameDomain::from_scope(edge.dest),
                            NameDerivation::Def,
                        );
                        bucket.push(&res);
                    }
                }

                PropagationResult::Continuation => {
                    debug_assert!(parent.is_none());
                    parent = Some(edge.dest);
                }

                PropagationResult::UnPropagated => {}
            }
        }

        // 2. Look for the name in the named imports of the current scope.
        if let Some(imported) = self
            .importer
            .named_imports(self.db, query.scope)
            .and_then(|imports| imports.get(&query.name))
        {
            bucket.merge(imported.iter());
        }

        // 3. Look for the name in the glob imports.
        if query.directive.allow_glob {
            if let Some(imported) = self.importer.glob_imports(self.db, query.scope) {
                for res in imported.name_res_for(query.name) {
                    bucket.push(res);
                }
            }
        }

        // 4. Look for the name in the lexical scope if it exists.
        if let Some(parent) = parent {
            let mut query_for_parent = query;
            query_for_parent.scope = parent;
            query_for_parent.directive.disallow_external();

            let mut resolved = self.resolve_query(query_for_parent);
            resolved.set_lexed_derivation();
            bucket.merge(resolved.iter());
        }

        if !query.directive.allow_external {
            return self.finalize_query_result(query, bucket);
        }

        // 5. Look for the name in the external ingots.
        query
            .scope
            .top_mod(self.db.as_hir_db())
            .ingot(self.db.as_hir_db())
            .external_ingots(self.db.as_hir_db())
            .iter()
            .for_each(|(name, root_mod)| {
                if *name == query.name {
                    // We don't care about the result of `push` because we assume ingots are
                    // guaranteed to be unique.
                    bucket.push(&NameRes::new_from_scope(
                        ScopeId::root(*root_mod),
                        NameDomain::Item,
                        NameDerivation::External,
                    ))
                }
            });

        // 6. Look for the name in the builtin types.
        for &prim in PrimTy::all_types() {
            // We don't care about the result of `push` because we assume builtin types are
            // guaranteed to be unique.
            if query.name == prim.name() {
                bucket.push(&NameRes::new_prim(prim));
            }
        }

        self.finalize_query_result(query, bucket)
    }

    /// Collect all visible resolutions in the given `target` scope.
    ///
    /// The function follows the shadowing rule, meaning the same name in the
    /// same domain is properly shadowed. Also, this function guarantees that
    /// the collected resolutions are unique in terms of its name and resolved
    /// scope.
    ///
    /// On the other hand, the function doesn't cause any error and collect all
    /// resolutions even if they are in the same domain. The reason
    /// for this is
    /// - Ambiguous error should be reported lazily, meaning it should be
    ///   reported when the resolution is actually used.
    /// - The function is used for glob imports, so it's necessary to return
    ///   monotonously increasing results. Also, we can't arbitrarily choose the
    ///   possible resolution from multiple candidates to avoid hiding
    ///   ambiguity. That's also the reason why we can't use [`ResBucket`] and
    ///   [`ResBucket::merge`] in this function.
    ///
    /// The below examples demonstrates the second point.
    /// We need to report ambiguous error at `const C: S = S` because `S` is
    /// ambiguous, on the other hand, we need NOT to report ambiguous error in
    /// `foo` modules because `S` is not referred to in the module.
    ///
    /// ```fe
    /// use foo::*
    /// const C: S = S
    ///
    /// mod foo {
    ///     pub use inner1::*
    ///     pub use inner2::*
    ///
    ///     mod inner1 {
    ///           pub struct S {}
    ///     }
    ///     mod inner2 {
    ///        pub struct S {}
    ///     }
    /// }
    /// ```
    pub(super) fn collect_all_resolutions_for_glob(
        &mut self,
        target: ScopeId,
        ref_scope: ScopeId,
        unresolved_named_imports: FxHashSet<IdentId>,
    ) -> FxHashMap<IdentId, Vec<NameRes>> {
        let mut res_collection: FxHashMap<IdentId, Vec<NameRes>> = FxHashMap::default();
        let mut found_domains: FxHashMap<IdentId, u8> = FxHashMap::default();
        let mut found_kinds: FxHashSet<(IdentId, NameResKind)> = FxHashSet::default();

        for edge in target.edges(self.db.as_hir_db()) {
            let scope = match edge.kind.propagate_glob() {
                PropagationResult::Terminated => edge.dest,
                _ => {
                    continue;
                }
            };

            let name = scope.name(self.db.as_hir_db()).unwrap();
            if !found_kinds.insert((name, scope.into())) {
                continue;
            }
            let res =
                NameRes::new_from_scope(scope, NameDomain::from_scope(scope), NameDerivation::Def);

            *found_domains.entry(name).or_default() |= res.domain as u8;
            res_collection.entry(name).or_default().push(res);
        }

        let mut found_domains_after_named = found_domains.clone();
        if let Some(named_imports) = self.importer.named_imports(self.db, target) {
            for (&name, import) in named_imports {
                let found_domain = found_domains.get(&name).copied().unwrap_or_default();
                for res in import.iter().filter(|res| {
                    if let NameDerivation::NamedImported(use_) = res.derivation {
                        is_use_visible(self.db, ref_scope, use_)
                    } else {
                        false
                    }
                }) {
                    if (found_domain & res.domain as u8 != 0)
                        || !found_kinds.insert((name, res.kind))
                    {
                        continue;
                    }

                    *found_domains_after_named.entry(name).or_default() |= res.domain as u8;
                    res_collection.entry(name).or_default().push(res.clone());
                }
            }
        }

        if let Some(glob_imports) = self.importer.glob_imports(self.db, target) {
            for (&use_, resolutions) in glob_imports.iter() {
                if !is_use_visible(self.db, ref_scope, use_) {
                    continue;
                }
                for (&name, res_for_name) in resolutions.iter() {
                    if unresolved_named_imports.contains(&name) {
                        continue;
                    }

                    for res in res_for_name.iter() {
                        let seen_domain = found_domains_after_named
                            .get(&name)
                            .copied()
                            .unwrap_or_default();

                        if (seen_domain & res.domain as u8 != 0)
                            || !found_kinds.insert((name, res.kind))
                        {
                            continue;
                        }
                        res_collection.entry(name).or_default().push(res.clone());
                    }
                }
            }
        }

        res_collection
    }

    /// Finalize the query result and cache it to the cache store.
    fn finalize_query_result(&mut self, query: NameQuery, bucket: ResBucket) -> ResBucket {
        self.cache_store.cache_result(query, bucket.clone());
        bucket
    }
}

impl Ord for NameDerivation {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NameResolutionError {
    /// The name is not found.
    NotFound,

    /// The name is invalid in parsing. Basically, no need to report it because
    /// the error is already emitted from parsing phase.
    Invalid,

    /// The name is found, but it's not visible from the reference site.
    Invisible(Option<DynLazySpan>),

    /// The name is found, but it's ambiguous.
    Ambiguous(Vec<NameRes>),
}

pub type NameResolutionResult<T> = Result<T, NameResolutionError>;

impl fmt::Display for NameResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NameResolutionError::NotFound => write!(f, "name not found"),
            NameResolutionError::Invalid => write!(f, "invalid name"),
            NameResolutionError::Invisible(_) => write!(f, "name is not visible"),
            NameResolutionError::Ambiguous(_) => write!(f, "name is ambiguous"),
        }
    }
}

impl std::error::Error for NameResolutionError {}

#[derive(Default, Debug, PartialEq, Eq)]
pub(crate) struct ResolvedQueryCacheStore {
    cache: FxHashMap<NameQuery, ResBucket>,
    no_cache: bool,
}

impl ResolvedQueryCacheStore {
    pub(super) fn get(&self, query: NameQuery) -> Option<&ResBucket> {
        self.cache.get(&query)
    }

    fn cache_result(&mut self, query: NameQuery, result: ResBucket) {
        if self.no_cache {
            return;
        }
        self.cache.insert(query, result);
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
    Item = 0b1,
    /// The domain is associated with a local variable and items that are
    /// guaranteed not to have associated names. e.g., `fn`, `const` or enum
    /// variables.
    Value = 0b10,
    /// The domain is associated with struct fields.
    Field = 0b100,
}

impl NameDomain {
    pub(super) fn from_scope(scope: ScopeId) -> Self {
        match scope {
            ScopeId::Item(ItemKind::Func(_) | ItemKind::Const(_)) | ScopeId::FuncParam(..) => {
                Self::Value
            }
            ScopeId::Item(_) | ScopeId::GenericParam(..) => Self::Item,
            ScopeId::Field(..) => Self::Field,
            ScopeId::Variant(..) => Self::Value,
        }
    }
}

trait QueryPropagator {
    fn propagate(self, query: &NameQuery) -> PropagationResult;
    fn propagate_glob(self) -> PropagationResult;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PropagationResult {
    Terminated,
    Continuation,
    UnPropagated,
}

impl QueryPropagator for LexEdge {
    fn propagate(self, query: &NameQuery) -> PropagationResult {
        if query.directive.allow_lex {
            PropagationResult::Continuation
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for ModEdge {
    fn propagate(self, query: &NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl QueryPropagator for TypeEdge {
    fn propagate(self, query: &NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl QueryPropagator for TraitEdge {
    fn propagate(self, query: &NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl QueryPropagator for ValueEdge {
    fn propagate(self, query: &NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl QueryPropagator for GenericParamEdge {
    fn propagate(self, query: &NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for FieldEdge {
    fn propagate(self, query: &NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for VariantEdge {
    fn propagate(self, query: &NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl QueryPropagator for SuperEdge {
    fn propagate(self, query: &NameQuery) -> PropagationResult {
        if query.name.is_super() {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for IngotEdge {
    fn propagate(self, query: &NameQuery) -> PropagationResult {
        if query.name.is_ingot() {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for SelfTyEdge {
    fn propagate(self, query: &NameQuery) -> PropagationResult {
        if query.name.is_self_ty() {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for SelfEdge {
    fn propagate(self, query: &NameQuery) -> PropagationResult {
        if query.name.is_self() {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for AnonEdge {
    fn propagate(self, _query: &NameQuery) -> PropagationResult {
        PropagationResult::UnPropagated
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for EdgeKind {
    fn propagate(self, query: &NameQuery) -> PropagationResult {
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

    fn propagate_glob(self) -> PropagationResult {
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
