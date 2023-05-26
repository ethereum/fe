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
        IdentId, ItemKind, Partial, PathId, Use,
    },
    span::DynLazySpan,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::HirAnalysisDb;

use super::{
    import_resolver::Importer,
    visibility_checker::{is_scope_visible, is_use_visible},
};

pub struct NameResolver<'db, 'a> {
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedPath {
    Full(NameBinding),

    /// The path is partially resolved; this means that the `resolved` is a type
    /// and the following segments depend on type to resolve.
    /// These unresolved parts are resolved in the later type inference and
    /// trait solving phases.
    Partial {
        resolved: NameRes,
        unresolved_from: usize,
    },
}

impl ResolvedPath {
    pub fn partial(resolved: NameRes, unresolved_from: usize) -> Self {
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
    /// FOO` and `struct FOO` in the following code without any error:
    /// ```fe
    /// use foo::FOO
    ///
    /// mod foo {
    ///     pub const FOO: i32 = 1
    ///     pub struct FOO {}
    /// }
    /// ```
    pub fn resolve_path(
        &mut self,
        path: PathId,
        scope: ScopeId,
    ) -> Result<ResolvedPath, PathResolutionError> {
        let segments = path.data(self.db.as_hir_db());
        if segments.is_empty() {
            return Err(PathResolutionError::invalid(0));
        }

        // Set pred segment to the current scope.
        let mut pred =
            NameRes::new_scope(scope, NameDomain::from_scope(scope), NameDerivation::Def);

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

    pub fn resolve_query(&mut self, query: NameQuery) -> Result<NameBinding, NameResolutionError> {
        // If the query is already resolved, return the cached result.
        if let Some(resolved) = self.cache_store.get(query) {
            return resolved.clone();
        };

        let mut binding = NameBinding::default();

        // The shadowing rule is
        // `$ > NamedImports > GlobImports > Lex > external ingot > builtin types`,
        // where `$` means current scope.
        // This ordering means that greater one shadows lower ones in the same domain.
        let mut parent = None;

        // 1. Look for the name in the current scope.
        let mut found_scopes = FxHashSet::default();
        for edge in query.scope.edges(self.db.as_hir_db()) {
            match edge.kind.propagate(query) {
                PropagationResult::Terminated => {
                    if found_scopes.insert(edge.dest) {
                        let res = NameRes::new_scope(
                            edge.dest,
                            NameDomain::from_scope(edge.dest),
                            NameDerivation::Def,
                        );
                        if binding.push(&res).is_some() {
                            return Err(NameResolutionError::Ambiguous);
                        }
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
            self.try_merge(&mut binding, &imported.binding, query)?;
        }

        // 3. Look for the name in the glob imports.
        if query.directive.allow_glob {
            if let Some(imported) = self.importer.glob_imports(self.db, query.scope) {
                for res in imported.name_res_for(query.name) {
                    self.try_push(&mut binding, res, query)?;
                }
            }
        }

        // 4. Look for the name in the lexical scope if it exists.
        if let Some(parent) = parent {
            let mut query_for_parent = query;
            query_for_parent.scope = parent;
            query_for_parent.directive.disallow_external();

            match self.resolve_query(query_for_parent) {
                Ok(mut resolved) => {
                    resolved.lexed();
                    self.try_merge(&mut binding, &resolved, query)?;
                }

                Err(NameResolutionError::NotFound) => {}
                Err(err) => {
                    self.cache_store.cache_result(query, Err(err.clone()));
                    return Err(err);
                }
            }
        }

        if !query.directive.allow_external {
            return self.finalize_query_result(query, binding);
        }

        // 5. Look for the name in the external ingots.
        if query.directive.is_allowed_domain(NameDomain::Item as u8) {
            query
                .scope
                .top_mod(self.db.as_hir_db())
                .ingot(self.db.as_hir_db())
                .external_ingots(self.db.as_hir_db())
                .iter()
                .for_each(|(name, root_mod)| {
                    if *name == query.name {
                        binding.push(&NameRes::new_scope(
                            ScopeId::root(*root_mod),
                            NameDomain::Item,
                            NameDerivation::External,
                        ));
                    }
                });
        }

        // 6. Look for the name in the builtin types.
        for &prim in PrimTy::all_types() {
            if query.name == prim.name() {
                binding.push(&NameRes::new_prim(prim));
            }
        }

        self.finalize_query_result(query, binding)
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
    ///   ambiguity. That's also the reason why we can't use `NameBinding` and
    ///   `NameBinding::merge` in this function.
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
        directive: QueryDirective,
        unresolved_named_imports: FxHashSet<IdentId>,
    ) -> FxHashMap<IdentId, Vec<NameRes>> {
        let mut res_collection: FxHashMap<IdentId, Vec<NameRes>> = FxHashMap::default();
        let mut found_domains: FxHashMap<IdentId, u8> = FxHashMap::default();
        let mut found_kinds: FxHashSet<(IdentId, NameResKind)> = FxHashSet::default();

        for edge in target.edges(self.db.as_hir_db()) {
            let scope = match edge.kind.propagate_glob(directive) {
                PropagationResult::Terminated => edge.dest,
                _ => {
                    continue;
                }
            };

            let name = scope.name(self.db.as_hir_db()).unwrap();
            if !found_kinds.insert((name, scope.into())) {
                continue;
            }
            let res = NameRes::new_scope(scope, NameDomain::from_scope(scope), NameDerivation::Def);

            *found_domains.entry(name).or_default() |= res.domain as u8;
            res_collection.entry(name).or_default().push(res);
        }

        let mut found_domains_after_named = found_domains.clone();
        if let Some(named_imports) = self.importer.named_imports(self.db, target) {
            for (&name, import) in named_imports {
                if !is_use_visible(self.db, ref_scope, import.use_) {
                    continue;
                }

                let found_domain = found_domains.get(&name).copied().unwrap_or_default();
                for res in import.binding.iter() {
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
    fn finalize_query_result(
        &mut self,
        query: NameQuery,
        resolved_set: NameBinding,
    ) -> Result<NameBinding, NameResolutionError> {
        let result = if resolved_set.is_empty() {
            Err(NameResolutionError::NotFound)
        } else {
            Ok(resolved_set)
        };
        self.cache_store.cache_result(query, result.clone());
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
        _pred: NameRes,
        _segment: Partial<IdentId>,
        _seg_idx: usize,
        _is_last: bool,
    ) -> Result<Either<ResolvedPath, NameRes>, PathResolutionError> {
        todo!()
        // let Partial::Present(seg) = segment else {
        //     return Err(PathResolutionError::invalid(seg_idx));
        // };

        // let scope = pred.scope;
        // let query = NameQuery::new(seg, scope);
        // let resolved_set = match self.resolve_query(query) {
        //     Ok(resolved) => resolved,
        //     Err(NameResolutionError::NotFound) if pred.is_type(self.db) => {
        //         // If the parent scope of the current segment is a type and
        // the segment is not         // found, then it should be
        // resolved in the trait solving phase.         return
        // Ok(Either::Left(ResolvedPath::partial(pred, seg_idx)));     }
        //     Err(e) => {
        //         return Err(PathResolutionError::new(e, seg_idx));
        //     }
        // };

        // if is_last {
        //     Ok(Either::Left(ResolvedPath::Full(resolved_set)))
        // } else if resolved_set.len() > 1 {
        //     // Case a. is already handled above.
        //     // Handles case b. here.
        //     return Err(PathResolutionError::not_found(seg_idx));
        // } else {
        //     Ok(Either::Right(resolved_set.into_iter().next().unwrap()))
        // }
    }

    fn try_merge(
        &mut self,
        target: &mut NameBinding,
        from: &NameBinding,
        query: NameQuery,
    ) -> Result<(), NameResolutionError> {
        if target
            .merge(from.filter_by_domain(query.directive.domain))
            .is_none()
        {
            Ok(())
        } else {
            let err = NameResolutionError::Ambiguous;
            self.cache_store.cache_result(query, Err(err.clone()));
            Err(err)
        }
    }

    fn try_push(
        &mut self,
        target: &mut NameBinding,
        res: &NameRes,
        query: NameQuery,
    ) -> Result<(), NameResolutionError> {
        if target.push(res).is_none() {
            Ok(())
        } else {
            let err = NameResolutionError::Ambiguous;
            self.cache_store.cache_result(query, Err(err.clone()));
            Err(err)
        }
    }
}

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

    domain: u8,
}

impl QueryDirective {
    /// Make a new query directive with the default settings.
    /// The default setting is to allow lexical scope lookup and look up names
    /// in the `Item` domain.
    pub fn new() -> Self {
        Self {
            allow_lex: true,
            allow_external: true,
            allow_glob: true,
            domain: NameDomain::Item as u8,
        }
    }

    /// Set the `domain` to lookup, the allowed domain set that are already set
    /// will be overwritten.
    pub fn set_domain(&mut self, domain: NameDomain) -> &mut Self {
        self.domain = domain as u8;
        self
    }

    /// Append the `domain` to the allowed domain set.
    pub fn add_domain(&mut self, domain: NameDomain) -> &mut Self {
        self.domain |= domain as u8;
        self
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

    /// Returns true if the `domain` is allowed to lookup in the current
    /// setting.
    pub(super) fn is_allowed_domain(&self, domain: u8) -> bool {
        self.domain & domain != 0
    }
}

impl Default for QueryDirective {
    fn default() -> Self {
        Self::new()
    }
}

/// The struct contains the lookup result of a name query.
/// The results can contain more than one resolved items which belong to
/// different name domains.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct NameBinding {
    pub(super) resolutions: FxHashMap<NameDomain, NameRes>,
}

impl NameBinding {
    /// Returns the number of resolutions.
    pub fn len(&self) -> usize {
        self.resolutions.len()
    }

    pub fn is_empty(&self) -> bool {
        self.resolutions.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &NameRes> {
        self.resolutions.values()
    }

    pub fn filter_by_visibility(&self, db: &dyn HirAnalysisDb, from: ScopeId) -> Self {
        let mut resolutions = FxHashMap::default();
        for (domain, res) in &self.resolutions {
            if res.is_visible(db, from) {
                resolutions.insert(*domain, res.clone());
            }
        }
        Self { resolutions }
    }

    /// Returns the resolution of the given `domain`.
    pub fn name_by_domain(&self, domain: NameDomain) -> Option<&NameRes> {
        self.resolutions.get(&domain)
    }

    /// Merge the `resolutions` into the set. If name conflict happens, the old
    /// resolution will be returned, otherwise `None` will be returned.
    pub(super) fn merge<'a>(
        &mut self,
        resolutions: impl Iterator<Item = &'a NameRes>,
    ) -> Option<NameRes> {
        for res in resolutions {
            if let Some(conflict) = self.push(res) {
                return Some(conflict);
            }
        }

        None
    }

    pub(super) fn set_derivation(&mut self, derivation: NameDerivation) {
        for res in self.resolutions.values_mut() {
            res.derivation = derivation.clone();
        }
    }

    /// Push the `res` into the set.
    fn push(&mut self, res: &NameRes) -> Option<NameRes> {
        let domain = res.domain;
        match self.resolutions.entry(domain) {
            Entry::Occupied(mut e) => {
                let old_derivation = e.get().derivation.clone();
                match res.derivation.cmp(&old_derivation) {
                    cmp::Ordering::Less => None,
                    cmp::Ordering::Equal => {
                        if e.get().kind == res.kind {
                            None
                        } else {
                            Some(res.clone())
                        }
                    }
                    cmp::Ordering::Greater => {
                        e.insert(res.clone());
                        None
                    }
                }
            }

            Entry::Vacant(e) => {
                e.insert(res.clone());
                None
            }
        }
    }

    fn filter_by_domain(&self, domain: u8) -> impl Iterator<Item = &NameRes> {
        self.resolutions
            .values()
            .filter(move |res| ((res.domain as u8) & domain) != 0)
    }

    fn lexed(&mut self) {
        for res in self.resolutions.values_mut() {
            res.derivation.lexed()
        }
    }
}

impl IntoIterator for NameBinding {
    type Item = NameRes;
    type IntoIter = IntoValues<NameDomain, NameRes>;

    fn into_iter(self) -> Self::IntoIter {
        self.resolutions.into_values()
    }
}

impl From<NameRes> for NameBinding {
    fn from(resolution: NameRes) -> Self {
        let mut names = FxHashMap::default();
        names.insert(resolution.domain, resolution);
        Self { resolutions: names }
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
            Either::Left(target_scope) => is_scope_visible(db, from, target_scope),
            Either::Right(use_) => is_use_visible(db, from, use_),
        }
    }

    pub fn pretty_path(&self, db: &dyn HirAnalysisDb) -> Option<String> {
        match self.kind {
            NameResKind::Scope(scope) => scope.pretty_path(db.as_hir_db()),
            NameResKind::Prim(prim) => prim.name().data(db.as_hir_db()).into(),
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

    fn new_scope(scope: ScopeId, domain: NameDomain, derivation: NameDerivation) -> Self {
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

impl Ord for NameDerivation {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NameResolutionError {
    /// The name is not found.
    NotFound,

    /// Multiple candidates are found.
    Conflict,

    /// The name is invalid in parsing. Basically, no need to report it because
    /// the error is already emitted from parsing phase.
    Invalid,

    /// The name is found, but it's not visible from the reference site.
    Invisible,

    /// The name is found, but it's ambiguous.
    Ambiguous,
}

impl fmt::Display for NameResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NameResolutionError::NotFound => write!(f, "name not found"),
            NameResolutionError::Conflict => write!(f, "multiple candidates found"),
            NameResolutionError::Invalid => write!(f, "invalid name"),
            NameResolutionError::Invisible => write!(f, "name is not visible"),
            NameResolutionError::Ambiguous => write!(f, "name is ambiguous"),
        }
    }
}

impl std::error::Error for NameResolutionError {}

#[derive(Default)]
struct ResolvedQueryCacheStore {
    cache: FxHashMap<NameQuery, Result<NameBinding, NameResolutionError>>,
    no_cache: bool,
}

impl ResolvedQueryCacheStore {
    fn cache_result(&mut self, query: NameQuery, result: Result<NameBinding, NameResolutionError>) {
        if self.no_cache {
            return;
        }
        self.cache.insert(query, result);
    }

    fn get(&self, query: NameQuery) -> Option<&Result<NameBinding, NameResolutionError>> {
        self.cache.get(&query)
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
    fn from_scope(scope: ScopeId) -> Self {
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
    fn propagate(&self, query: NameQuery) -> PropagationResult {
        if query.directive.is_allowed_domain(Self::ALLOWED_DOMAIN) {
            self.propagate_impl(query)
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(&self, directive: QueryDirective) -> PropagationResult {
        if directive.is_allowed_domain(Self::ALLOWED_DOMAIN) {
            self.propagate_glob_impl()
        } else {
            PropagationResult::UnPropagated
        }
    }

    const ALLOWED_DOMAIN: u8;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult;
    fn propagate_glob_impl(&self) -> PropagationResult;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PropagationResult {
    Terminated,
    Continuation,
    UnPropagated,
}

impl QueryPropagator for LexEdge {
    const ALLOWED_DOMAIN: u8 = ALL_DOMAINS;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult {
        if query.directive.allow_lex {
            PropagationResult::Continuation
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob_impl(&self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for ModEdge {
    const ALLOWED_DOMAIN: u8 = NameDomain::Item as u8;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob_impl(&self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl QueryPropagator for TypeEdge {
    const ALLOWED_DOMAIN: u8 = NameDomain::Item as u8;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob_impl(&self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl QueryPropagator for TraitEdge {
    const ALLOWED_DOMAIN: u8 = NameDomain::Item as u8;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }
    fn propagate_glob_impl(&self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl QueryPropagator for ValueEdge {
    const ALLOWED_DOMAIN: u8 = NameDomain::Value as u8;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }
    fn propagate_glob_impl(&self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl QueryPropagator for GenericParamEdge {
    const ALLOWED_DOMAIN: u8 = NameDomain::Item as u8;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob_impl(&self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for FieldEdge {
    const ALLOWED_DOMAIN: u8 = NameDomain::Field as u8;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob_impl(&self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for VariantEdge {
    const ALLOWED_DOMAIN: u8 = NameDomain::Item as u8;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult {
        if self.0 == query.name {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob_impl(&self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl QueryPropagator for SuperEdge {
    const ALLOWED_DOMAIN: u8 = NameDomain::Item as u8;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult {
        if query.name.is_super() {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob_impl(&self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for IngotEdge {
    const ALLOWED_DOMAIN: u8 = NameDomain::Item as u8;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult {
        if query.name.is_ingot() {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob_impl(&self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for SelfTyEdge {
    const ALLOWED_DOMAIN: u8 = NameDomain::Item as u8;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult {
        if query.name.is_self_ty() {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob_impl(&self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for SelfEdge {
    const ALLOWED_DOMAIN: u8 = NameDomain::Item as u8;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult {
        if query.name.is_self() {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob_impl(&self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for AnonEdge {
    const ALLOWED_DOMAIN: u8 = 0;

    fn propagate_impl(&self, _query: NameQuery) -> PropagationResult {
        PropagationResult::UnPropagated
    }

    fn propagate_glob_impl(&self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl QueryPropagator for EdgeKind {
    const ALLOWED_DOMAIN: u8 = ALL_DOMAINS;

    fn propagate_impl(&self, query: NameQuery) -> PropagationResult {
        match self {
            EdgeKind::Lex(edge) => edge.propagate_impl(query),
            EdgeKind::Mod(edge) => edge.propagate_impl(query),
            EdgeKind::Type(edge) => edge.propagate_impl(query),
            EdgeKind::Trait(edge) => edge.propagate_impl(query),
            EdgeKind::GenericParam(edge) => edge.propagate_impl(query),
            EdgeKind::Value(edge) => edge.propagate_impl(query),
            EdgeKind::Field(edge) => edge.propagate_impl(query),
            EdgeKind::Variant(edge) => edge.propagate_impl(query),
            EdgeKind::Super(edge) => edge.propagate_impl(query),
            EdgeKind::Ingot(edge) => edge.propagate_impl(query),
            EdgeKind::Self_(edge) => edge.propagate_impl(query),
            EdgeKind::SelfTy(edge) => edge.propagate_impl(query),
            EdgeKind::Anon(edge) => edge.propagate_impl(query),
        }
    }

    fn propagate_glob_impl(&self) -> PropagationResult {
        match self {
            EdgeKind::Lex(edge) => edge.propagate_glob_impl(),
            EdgeKind::Mod(edge) => edge.propagate_glob_impl(),
            EdgeKind::Type(edge) => edge.propagate_glob_impl(),
            EdgeKind::Trait(edge) => edge.propagate_glob_impl(),
            EdgeKind::GenericParam(edge) => edge.propagate_glob_impl(),
            EdgeKind::Value(edge) => edge.propagate_glob_impl(),
            EdgeKind::Field(edge) => edge.propagate_glob_impl(),
            EdgeKind::Variant(edge) => edge.propagate_glob_impl(),
            EdgeKind::Super(edge) => edge.propagate_glob_impl(),
            EdgeKind::Ingot(edge) => edge.propagate_glob_impl(),
            EdgeKind::Self_(edge) => edge.propagate_glob_impl(),
            EdgeKind::SelfTy(edge) => edge.propagate_glob_impl(),
            EdgeKind::Anon(edge) => edge.propagate_glob_impl(),
        }
    }
}

const ALL_DOMAINS: u8 = NameDomain::Item as u8 | NameDomain::Value as u8 | NameDomain::Field as u8;
