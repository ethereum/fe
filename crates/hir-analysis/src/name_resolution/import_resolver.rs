//! This module implements import and export resolution for HIR.
use std::{
    collections::{hash_map::Entry, VecDeque},
    mem,
};

use hir::{
    hir_def::{scope_graph::ScopeId, IdentId, IngotId, Use},
    span::DynLazySpan,
};
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{name_resolution::visibility_checker::is_use_visible, HirAnalysisDb};

use super::{
    diagnostics::ImportError,
    name_resolver::{
        NameBinding, NameDerivation, NameDomain, NameQuery, NameRes, NameResKind,
        NameResolutionError, NameResolver, QueryDirective,
    },
};

pub struct ImportResolver<'db> {
    db: &'db dyn HirAnalysisDb,

    /// The ingot that is being resolved.
    ingot: IngotId,

    /// The set of imports that have been resolved.
    resolved_imports: IntermediateResolvedImports,

    /// The uses that have resolution is work in progress.
    intermediate_uses: FxHashMap<ScopeId, VecDeque<IntermediateUse>>,

    /// The errors that have been accumulated during the import resolution.
    accumulated_errors: Vec<ImportError>,

    /// The number of imported resolutions.
    /// This is used to judge if a import resolution doesn't change in each
    /// iteration of fixed point calculation.
    /// This check rely on the fact that the number of resolutions is
    /// monotonically increasing.
    num_imported_res: FxHashMap<Use, usize>,

    /// The set of imports that are suspicious to be ambiguous.
    /// In this case, the use will turns out to be ambiguous after the import
    /// resolution reaches the fixed point.
    suspicious_imports: FxHashSet<Use>,
}
impl<'db> ImportResolver<'db> {
    pub(crate) fn new(db: &'db dyn HirAnalysisDb, ingot: IngotId) -> Self {
        Self {
            db,
            ingot,
            resolved_imports: IntermediateResolvedImports::new(ingot),
            intermediate_uses: FxHashMap::default(),
            accumulated_errors: Vec::default(),
            num_imported_res: FxHashMap::default(),
            suspicious_imports: FxHashSet::default(),
        }
    }

    pub(crate) fn resolve_imports(mut self) -> (ResolvedImports, Vec<ImportError>) {
        self.initialize_i_uses();

        let mut changed = true;
        let mut unresolved_scope: VecDeque<_> =
            self.intermediate_uses.keys().copied().dedup().collect();
        while changed {
            changed = false;
            let n_unresolved_scope = unresolved_scope.len();
            let mut scope_counter = 0;

            while scope_counter < n_unresolved_scope {
                scope_counter += 1;
                let scope = unresolved_scope.pop_front().unwrap();

                let n_i_uses = self.intermediate_uses[&scope].len();
                let mut i_use_counter = 0;
                while i_use_counter < n_i_uses {
                    i_use_counter += 1;
                    let i_use = self
                        .intermediate_uses
                        .get_mut(&scope)
                        .unwrap()
                        .pop_front()
                        .unwrap();

                    match self.resolve_i_use(i_use) {
                        (Some(updated_i_use), resolved) => {
                            changed |= resolved;
                            self.intermediate_uses
                                .get_mut(&scope)
                                .unwrap()
                                .push_back(updated_i_use);
                        }

                        (None, resolved) => {
                            changed |= resolved;
                        }
                    }
                }

                if !self.scope_state(scope).is_closed() {
                    unresolved_scope.push_back(scope);
                }
            }
        }

        for i_use in std::mem::take(&mut self.intermediate_uses)
            .into_values()
            .flatten()
        {
            // If the unresolved use is a glob and the base path is fully resolved, then we
            // can regard the resolution for the glob as completed.
            // This happens if the scope that glob is referring to is not closed, e.g., if
            // there is a cycle in the import graph.
            if i_use.is_glob(self.db) && i_use.is_base_resolved(self.db) {
                continue;
            }

            // If the unresolved use is not a glob and the number of imported bindings is
            // not 0, then we can regard the resolution for the use as completed.
            // This happens if the scope that the use is referring to is not closed.
            if !i_use.is_glob(self.db) && *self.num_imported_res.entry(i_use.use_).or_default() != 0
            {
                continue;
            }

            self.register_error(&i_use, NameResolutionError::NotFound);
        }

        for suspicious in mem::take(&mut self.suspicious_imports) {
            self.verify_ambiguity(suspicious);
        }

        (
            self.resolved_imports.resolved_imports,
            self.accumulated_errors,
        )
    }

    /// Try to resolve the given `IntermediateUse`.
    ///
    /// The first value of the returned tuple is the updated `IntermediateUse`
    /// if the resolution is not fully completed.
    ///
    /// The second value of the returned tuple indicates whether the resolution
    /// is progressed from the passed `IntermediateUse`.
    fn resolve_i_use(&mut self, i_use: IntermediateUse) -> (Option<IntermediateUse>, bool) {
        if i_use.is_glob(self.db) {
            self.resolve_glob(i_use)
        } else {
            self.resolve_named(i_use)
        }
    }

    /// Try to resolve the given named `IntermediateUse`.
    fn resolve_named(&mut self, i_use: IntermediateUse) -> (Option<IntermediateUse>, bool) {
        let Some(i_use_res) = self.resolve_base_path(i_use.clone()) else {
            return (None, true);
        };

        match i_use_res {
            IUseResolution::Full(_) => unreachable!(),

            IUseResolution::BasePath(base_path_resolved) => {
                if self.try_finalize_named_use(base_path_resolved) {
                    (None, true)
                } else {
                    let changed = !i_use.is_base_resolved(self.db);
                    (Some(i_use), changed)
                }
            }

            IUseResolution::Partial(i_use) => (Some(i_use), true),

            IUseResolution::Unchanged(i_use) => (Some(i_use), false),
        }
    }

    /// Try to resolve the given glob `IntermediateUse`.
    fn resolve_glob(&mut self, i_use: IntermediateUse) -> (Option<IntermediateUse>, bool) {
        let (base_path_resolved, changed) = {
            if i_use.is_base_resolved(self.db) {
                (i_use, false)
            } else {
                let Some(i_use_res) = self.resolve_base_path(i_use) else {
                    return (None, true);
                };
                match i_use_res {
                    IUseResolution::Full(_) => unreachable!(),

                    IUseResolution::BasePath(resolved) => (resolved, true),

                    IUseResolution::Partial(i_use) => {
                        return (Some(i_use), true);
                    }

                    IUseResolution::Unchanged(i_use) => {
                        return (Some(i_use), false);
                    }
                }
            }
        };

        let Some(target_scope) = base_path_resolved.current_scope() else {
            return (None, true);
        };

        let original_scope = base_path_resolved.original_scope;
        let use_ = base_path_resolved.use_;

        // Collect all unresolved named imports in the target scope to avoid binding a
        // name to a wrong resolution being brought by a glob.
        let unresolved_named_imports = match self.intermediate_uses.get(&target_scope) {
            Some(i_uses) => i_uses
                .iter()
                .filter_map(|i_use_in_target| {
                    if !i_use_in_target.is_glob(self.db)
                        && is_use_visible(self.db, original_scope, use_)
                    {
                        i_use_in_target.imported_name(self.db)
                    } else {
                        None
                    }
                })
                .collect(),

            None => FxHashSet::default(),
        };

        // Collect all bindings in the target scope.
        let mut resolver = NameResolver::new(self.db, &self.resolved_imports);
        let mut directive = QueryDirective::default();
        directive
            .add_domain(NameDomain::Value)
            .disallow_lex()
            .disallow_external();
        let resolutions = resolver.collect_all_resolutions_for_glob(
            target_scope,
            original_scope,
            directive,
            unresolved_named_imports,
        );

        let is_decidable = self.is_decidable(&base_path_resolved);
        let n_res = resolutions.iter().fold(0, |acc, bind| acc + bind.1.len());
        if *self.num_imported_res.entry(use_).or_default() == n_res {
            if is_decidable {
                return (None, true);
            } else {
                return (Some(base_path_resolved), changed);
            }
        }

        self.num_imported_res.insert(base_path_resolved.use_, n_res);
        self.resolved_imports
            .set_glob_resolutions(&base_path_resolved, resolutions);

        if is_decidable {
            (None, true)
        } else {
            (Some(base_path_resolved), true)
        }
    }

    /// Resolves all segments of the given `IntermediateUse` except for the last
    /// segment.
    /// NOTE: `IUseResolution::Full` is never returned from this function.
    ///
    /// # Returns
    /// - `Some(IUseResolution::BasePath(_))` if the base path is fully
    ///   resolved.
    /// - `Some(IUseResolution::Partial(_))` if the base path is partially
    ///   resolved and the `IntermediateUse` is updated.
    /// - `Some(IUseResolution::Unchanged(_))` if the resulted `IntermediateUse`
    ///   is unchanged.
    /// - `None` if the error happens during the resolution, the error is
    ///   accumulated in the function.
    fn resolve_base_path(&mut self, mut i_use: IntermediateUse) -> Option<IUseResolution> {
        let mut changed = true;
        if i_use.is_base_resolved(self.db) {
            return Some(IUseResolution::BasePath(i_use));
        }

        loop {
            match self.resolve_segment(&i_use)? {
                IUseResolution::Full(_) => unreachable!(),

                IUseResolution::BasePath(resolved) => {
                    return Some(IUseResolution::BasePath(resolved));
                }

                IUseResolution::Partial(updated_i_use) => {
                    changed = true;
                    i_use = updated_i_use;
                }

                IUseResolution::Unchanged(i_use) => {
                    return if changed {
                        Some(IUseResolution::Partial(i_use))
                    } else {
                        Some(IUseResolution::Unchanged(i_use))
                    };
                }
            }
        }
    }

    /// Resolves the segments of the given `IntermediateUse` one by one.
    ///
    /// # Returns
    /// - `Some(IUseResolution::Full(_))` if the given use is fully resolved.
    /// - `Some(IUseResolution::BasePath(_))` if the base path is fully
    ///   resolved.
    /// - `Some(IUseResolution::Partial(_))` if the base path is partially
    ///   resolved and the `IntermediateUse` is updated.
    /// - `Some(IUseResolution::Unchanged(_))` if the resulted `IntermediateUse`
    ///   is unchanged.
    /// - `None` if the error happens during the resolution, the error is
    ///   accumulated in the function.
    fn resolve_segment(&mut self, i_use: &IntermediateUse) -> Option<IUseResolution> {
        // The segment is syntactically invalid. We can't perform name resolution
        // anymore.
        // We don't need to report the error here because the parser should have already
        // reported it.
        let query = match self.make_query(i_use) {
            Ok(query) => query,
            Err(err) => {
                self.register_error(i_use, err);
                return None;
            }
        };

        let mut resolver = NameResolver::new_no_cache(self.db, &self.resolved_imports);
        let resolved = match resolver.resolve_query(query) {
            Ok(resolved) => resolved,

            Err(NameResolutionError::NotFound) if !self.is_decidable(i_use) => {
                return Some(IUseResolution::Unchanged(i_use.clone()))
            }

            Err(err) => {
                self.register_error(i_use, err);
                return None;
            }
        };

        if i_use.is_base_resolved(self.db) {
            return Some(IUseResolution::Full(resolved));
        }

        if resolved.contains_external(self.db, i_use) || resolved.contains_glob_imported() {
            self.suspicious_imports.insert(i_use.use_);
        }

        let next_i_use = i_use.proceed(resolved);
        if next_i_use.is_base_resolved(self.db) {
            Some(IUseResolution::BasePath(next_i_use))
        } else {
            Some(IUseResolution::Partial(next_i_use))
        }
    }

    fn initialize_i_uses(&mut self) {
        let m_tree = self.ingot.module_tree(self.db.as_hir_db());

        for top_mod in m_tree.all_modules() {
            let s_graph = top_mod.scope_graph(self.db.as_hir_db());
            for &use_ in &s_graph.unresolved_uses {
                let i_use = IntermediateUse::new(self.db, use_);
                self.intermediate_uses
                    .entry(i_use.original_scope)
                    .or_default()
                    .push_back(i_use);
            }
        }
    }

    /// Returns `true` if the given `IntermediateUse` reaches the fixed point.
    fn try_finalize_named_use(&mut self, i_use: IntermediateUse) -> bool {
        debug_assert!(i_use.is_base_resolved(self.db));

        let binding = match self.resolve_segment(&i_use) {
            Some(IUseResolution::Full(binding)) => binding,
            Some(IUseResolution::Unchanged(_)) => {
                return false;
            }

            Some(_) => unreachable!(),

            None => {
                return true;
            }
        };

        let filtered = binding.filter_by_visibility(self.db, i_use.original_scope);
        let n_res = filtered.len();
        let is_decidable = self.is_decidable(&i_use);

        if n_res == 0 && is_decidable {
            self.register_error(&i_use, NameResolutionError::NotFound);
            return true;
        }

        if *self.num_imported_res.entry(i_use.use_).or_default() == n_res {
            return is_decidable;
        }

        self.num_imported_res.insert(i_use.use_, n_res);
        if let Err(err) = self
            .resolved_imports
            .set_named_binds(self.db, &i_use, filtered)
        {
            self.accumulated_errors.push(err);
        }

        is_decidable
    }

    /// Check the ambiguity of the given suspicious `IntermediateUse` and report
    /// an error if it is ambiguous.
    /// An additional ambiguity check should be performed after the import
    /// resolution reaches a fixed point.
    fn verify_ambiguity(&mut self, use_: Use) {
        let i_use = IntermediateUse::new(self.db, use_);
        let first_segment_ident = i_use.current_segment_ident(self.db).unwrap();
        let scope = i_use.original_scope;
        let ingot = scope.ingot(self.db.as_hir_db());

        // The ambiguity in the first segment possibly occurs when the segment is
        // resolved to either a glob imported binding or an external ingot in the
        // `i_use` resolution.
        //
        // This is because:
        // 1. the resolution of the first segment changes depending on whether the
        //    dependent glob is resolved or not at the time of `i_use` resolution,
        // 2. the order in which uses are resolved is nondeterministic.
        //
        // In normal name resolution rules, the name brought in by a glob always shadows
        // the external ingot, so this ambiguity is inherent in import resolution.
        // As a result, we need to add additional verification to check this kind of
        // ambiguity.
        match self.resolve_segment(&i_use) {
            Some(IUseResolution::Full(_)) => {
                // The ambiguity about the final segment of the path is already verified during
                // the fixed point calculation, so verification is not
                // necessary.
                return;
            }

            Some(IUseResolution::BasePath(resolved) | IUseResolution::Partial(resolved)) => {
                if matches!(
                    resolved.current_res.unwrap().derivation,
                    NameDerivation::GlobImported(_)
                ) && ingot
                    .external_ingots(self.db.as_hir_db())
                    .iter()
                    .any(|(ingot_name, _)| ingot_name == &first_segment_ident)
                {
                    // The resolved scope is shadowed by an glob imports while originally
                    // the use might be resolved to an external ingot. This means there is an
                    // ambiguity between the external ingot and the name
                    // imported by the glob import.
                    self.register_error(&i_use, NameResolutionError::Ambiguous);
                }
            }

            Some(IUseResolution::Unchanged(_)) => {}

            None => {
                return;
            }
        }

        // The ambiguity in the base path arises when multiple items of the same name
        // are glob imported into the same scope. It is necessary to verify this
        // after the fixed point is reached, since it cannot be assumed that all
        // globs in that scope have been resolved at the time of `i_use` name
        // resolution.
        //
        // This ambiguity can be detected by the normal shadowing rules , so it can be
        // verified by calling `resolve_base_path`.
        //
        // The ambiguity about the final segment of the path can be verified during the
        // fixed point calculation, so verification is not necessary.
        self.resolve_base_path(i_use);
    }

    fn register_error(&mut self, i_use: &IntermediateUse, err: NameResolutionError) {
        match err {
            // We treat `Conflict` as the same as `NotFound`.
            // NOTE: The conflict error is happen in the `resolve_query` method, this means that the
            // name conflict happens in the scope that is being imported.
            NameResolutionError::NotFound | NameResolutionError::Conflict => {
                self.accumulated_errors.push(ImportError::not_found(
                    i_use.current_segment_span(),
                    i_use.current_segment_ident(self.db).unwrap(),
                ));
            }

            NameResolutionError::Invalid => {
                // Do nothing because the error is already reported in the
                // parsing phase.
            }

            NameResolutionError::Ambiguous => {
                self.accumulated_errors.push(ImportError::ambiguous(
                    i_use.current_segment_span(),
                    i_use.current_segment_ident(self.db).unwrap(),
                ));
            }

            // `Invisible` is not expected to be returned from `resolve_query` since `NameResolver`
            // doesn't care about visibility.
            NameResolutionError::Invisible => {
                unreachable!()
            }
        }
    }

    /// Makes a query for the current segment of the intermediate use to be
    /// resolved.
    fn make_query(&self, i_use: &IntermediateUse) -> Result<NameQuery, NameResolutionError> {
        let Some(seg_name) = i_use.current_segment_ident(self.db)  else {
            return Err(NameResolutionError::Invalid);
        };

        let mut directive = QueryDirective::new();
        let Some(current_scope) = i_use.current_scope() else {
            return Err(NameResolutionError::NotFound);
        };

        // In the middle of the use path, disallow lexically scoped names and
        // external names.
        if !i_use.is_first_segment() {
            directive.disallow_lex().disallow_external();
        }

        if self.contains_unresolved_named_use(seg_name, current_scope, i_use.is_first_segment()) {
            directive.disallow_glob().disallow_external();
        }

        if i_use.is_base_resolved(self.db) {
            directive.add_domain(NameDomain::Value);
        }

        Ok(NameQuery::with_directive(
            seg_name,
            current_scope,
            directive,
        ))
    }

    /// Returns `true` if there is an unresolved named import for the given name
    /// in the given scope or its lexical parents(if `allow_lex` is `true`).
    fn contains_unresolved_named_use(
        &self,
        name: IdentId,
        scope: ScopeId,
        allow_lex: bool,
    ) -> bool {
        let mut current_scope = Some(scope);

        while let Some(scope) = current_scope {
            for i_use in self.intermediate_uses.get(&scope).into_iter().flatten() {
                if i_use.imported_name(self.db) == Some(name) {
                    return true;
                }
            }
            if !allow_lex {
                break;
            }
            current_scope = scope.lex_parent(self.db.as_hir_db());
        }

        false
    }

    /// Returns the current state of the scope.
    fn scope_state(&self, scope: ScopeId) -> ScopeState {
        if scope.ingot(self.db.as_hir_db()) != self.ingot {
            return ScopeState::Closed;
        }

        let Some(i_uses) = self.intermediate_uses.get(&scope) else {
            return ScopeState::Closed;
        };

        if i_uses.is_empty() {
            return ScopeState::Closed;
        }
        for i_use in i_uses {
            if i_use.is_exported(self.db) {
                return ScopeState::Open;
            }
        }

        ScopeState::Semi
    }

    /// Returns `true` if the next segment of the intermediate use is
    /// deterministically resolvable.
    fn is_decidable(&self, i_use: &IntermediateUse) -> bool {
        let Some(target_scope) = i_use.current_scope() else {
            return true;
        };

        if i_use.is_first_segment() {
            let mut target_scope = Some(target_scope);
            while let Some(scope) = target_scope {
                if self.scope_state(scope) != ScopeState::Closed {
                    return false;
                }
                target_scope = scope.lex_parent(self.db.as_hir_db());
            }
            true
        } else {
            self.scope_state(target_scope) != ScopeState::Open
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ResolvedImports {
    pub named_resolved: FxHashMap<ScopeId, NamedImportSet>,
    pub glob_resolved: FxHashMap<ScopeId, GlobImportSet>,
    pub unnamed_resolved: Vec<NameBinding>,
}

pub(super) trait Importer {
    fn named_imports<'a>(
        &'a self,
        db: &'a dyn HirAnalysisDb,
        scope: ScopeId,
    ) -> Option<&'a NamedImportSet>;

    fn glob_imports<'a>(
        &'a self,
        db: &'a dyn HirAnalysisDb,
        scope: ScopeId,
    ) -> Option<&'a GlobImportSet>;

    fn unnamed_imports<'a>(
        &'a self,
        db: &'a dyn HirAnalysisDb,
        scope: ScopeId,
    ) -> &'a [NameBinding];
}

pub(super) struct DefaultImporter;

impl Importer for DefaultImporter {
    fn named_imports<'a>(
        &'a self,
        db: &'a dyn HirAnalysisDb,
        scope: ScopeId,
    ) -> Option<&'a NamedImportSet> {
        resolved_imports_for_scope(db, scope)
            .named_resolved
            .get(&scope)
    }

    fn glob_imports<'a>(
        &'a self,
        db: &'a dyn HirAnalysisDb,
        scope: ScopeId,
    ) -> Option<&'a GlobImportSet> {
        resolved_imports_for_scope(db, scope)
            .glob_resolved
            .get(&scope)
    }

    fn unnamed_imports<'a>(
        &'a self,
        db: &'a dyn HirAnalysisDb,
        scope: ScopeId,
    ) -> &'a [NameBinding] {
        &resolved_imports_for_scope(db, scope).unnamed_resolved
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportedBinding {
    pub binding: NameBinding,
    pub use_: Use,
}

pub type NamedImportSet = FxHashMap<IdentId, ImportedBinding>;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct GlobImportSet {
    imported: FxHashMap<Use, FxHashMap<IdentId, Vec<NameRes>>>,
}
impl GlobImportSet {
    /// Returns imported resolutions for the given `name`.
    pub fn name_res_for(&self, name: IdentId) -> impl Iterator<Item = &NameRes> {
        self.imported
            .values()
            .flat_map(move |v| v.get(&name).into_iter().flatten())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Use, &FxHashMap<IdentId, Vec<NameRes>>)> {
        self.imported.iter()
    }
}

/// This is the state of import resolution for a given scope.
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
enum ScopeState {
    // The scope is open, meaning that the scope needs further resolution.
    Open,

    /// The scope is partially resolved, meaning that the exports in the scope
    /// is fully resolved but the imports are partially resolved.
    Semi,

    /// The scope is closed, meaning that the all imports in the scope is fully
    /// resolved.
    Closed,
}

impl ScopeState {
    fn is_closed(self) -> bool {
        matches!(self, ScopeState::Closed)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct IntermediateUse {
    use_: Use,
    current_res: Option<NameRes>,
    original_scope: ScopeId,
    unresolved_from: usize,
}

impl IntermediateUse {
    fn new(db: &dyn HirAnalysisDb, use_: Use) -> Self {
        let scope = ScopeId::from_item(use_.into())
            .lex_parent(db.as_hir_db())
            .unwrap();
        Self {
            use_,
            current_res: None,
            original_scope: scope,
            unresolved_from: 0,
        }
    }

    /// Returns the scope that the current resolution is pointed to.
    fn current_scope(&self) -> Option<ScopeId> {
        if let Some(current_res) = self.current_res.as_ref() {
            match current_res.kind {
                NameResKind::Scope(scope) => Some(scope),
                NameResKind::Prim(_) => None,
            }
        } else {
            self.original_scope.into()
        }
    }

    fn is_exported(&self, db: &dyn HirAnalysisDb) -> bool {
        self.use_.vis(db.as_hir_db()).is_pub()
    }

    fn is_glob(&self, db: &dyn HirAnalysisDb) -> bool {
        self.use_.is_glob(db.as_hir_db())
    }

    /// Proceed the resolution of the use path to the next segment.
    /// The binding must contain exactly one resolution.
    fn proceed(&self, binding: NameBinding) -> Self {
        debug_assert_eq!(binding.len(), 1);
        let current_res = binding.into_iter().next();
        Self {
            use_: self.use_,
            current_res,
            original_scope: self.original_scope,
            unresolved_from: self.unresolved_from + 1,
        }
    }

    /// Returns the span of the current segment of the use.
    fn current_segment_span(&self) -> DynLazySpan {
        self.use_
            .lazy_span()
            .path()
            .segment(self.unresolved_from)
            .into()
    }

    fn current_segment_ident(&self, db: &dyn HirAnalysisDb) -> Option<IdentId> {
        let segments = self
            .use_
            .path(db.as_hir_db())
            .to_opt()?
            .data(db.as_hir_db());

        let seg_idx = self.unresolved_from;
        let segment = segments[seg_idx].to_opt()?;
        segment.ident()
    }

    fn imported_name(&self, db: &dyn HirAnalysisDb) -> Option<IdentId> {
        self.use_.imported_name(db.as_hir_db())
    }

    fn segment_len(&self, db: &dyn HirAnalysisDb) -> Option<usize> {
        self.use_
            .path(db.as_hir_db())
            .to_opt()
            .map(|p| p.segment_len(db.as_hir_db()))
    }

    /// Returns `true` if the segment that should be resolved next is the first
    /// segment.
    fn is_first_segment(&self) -> bool {
        self.unresolved_from == 0
    }

    /// Returns `true` if the use path except the last segment is fully
    /// resolved.
    fn is_base_resolved(&self, db: &dyn HirAnalysisDb) -> bool {
        let Some(segment_len) = self.segment_len(db) else {
            return false;
        };

        self.unresolved_from + 1 == segment_len
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum IUseResolution {
    /// The all segments are resolved.
    Full(NameBinding),

    /// The all path segments except the last one are resolved.
    BasePath(IntermediateUse),

    /// The intermediate use was partially resolved, but still needs further
    /// resolution.
    Partial(IntermediateUse),

    /// There was no change to the intermediate use.
    Unchanged(IntermediateUse),
}

struct IntermediateResolvedImports {
    resolved_imports: ResolvedImports,
    ingot: IngotId,
}

impl IntermediateResolvedImports {
    fn new(ingot: IngotId) -> Self {
        Self {
            resolved_imports: ResolvedImports::default(),
            ingot,
        }
    }

    fn set_named_binds(
        &mut self,
        db: &dyn HirAnalysisDb,
        i_use: &IntermediateUse,
        mut bind: NameBinding,
    ) -> Result<(), ImportError> {
        let scope = i_use.original_scope;
        bind.set_derivation(NameDerivation::NamedImported(i_use.use_));

        let imported_name = match i_use.imported_name(db) {
            Some(name) => name,
            None => {
                self.resolved_imports.unnamed_resolved.push(bind);
                return Ok(());
            }
        };

        let imported_set = self
            .resolved_imports
            .named_resolved
            .entry(scope)
            .or_default();

        match imported_set.entry(imported_name) {
            Entry::Occupied(mut e) => match e.get_mut().binding.merge(bind.iter()) {
                Some(already_found) => {
                    return Err(ImportError::conflict(
                        i_use.use_.imported_name_span(db.as_hir_db()).unwrap(),
                        already_found.derived_from(db).unwrap(),
                    ))
                }
                None => Ok(()),
            },

            Entry::Vacant(e) => {
                let import_bind = ImportedBinding {
                    binding: bind,
                    use_: i_use.use_,
                };
                e.insert(import_bind);
                Ok(())
            }
        }
    }

    fn set_glob_resolutions(
        &mut self,
        i_use: &IntermediateUse,
        mut resolutions: FxHashMap<IdentId, Vec<NameRes>>,
    ) {
        let scope = i_use.original_scope;
        for res in resolutions.values_mut().flatten() {
            res.derivation = NameDerivation::GlobImported(i_use.use_);
        }

        self.resolved_imports
            .glob_resolved
            .entry(scope)
            .or_default()
            .imported
            .insert(i_use.use_, resolutions);
    }
}

impl Importer for IntermediateResolvedImports {
    fn named_imports<'a>(
        &'a self,
        db: &'a dyn HirAnalysisDb,
        scope: ScopeId,
    ) -> Option<&'a NamedImportSet> {
        if scope.top_mod(db.as_hir_db()).ingot(db.as_hir_db()) != self.ingot {
            resolved_imports_for_scope(db, scope)
                .named_resolved
                .get(&scope)
        } else {
            self.resolved_imports.named_resolved.get(&scope)
        }
    }

    fn glob_imports<'a>(
        &'a self,
        db: &'a dyn HirAnalysisDb,
        scope: ScopeId,
    ) -> Option<&'a GlobImportSet> {
        if scope.top_mod(db.as_hir_db()).ingot(db.as_hir_db()) != self.ingot {
            resolved_imports_for_scope(db, scope)
                .glob_resolved
                .get(&scope)
        } else {
            self.resolved_imports.glob_resolved.get(&scope)
        }
    }

    fn unnamed_imports<'a>(
        &'a self,
        db: &'a dyn HirAnalysisDb,
        scope: ScopeId,
    ) -> &'a [NameBinding] {
        if scope.top_mod(db.as_hir_db()).ingot(db.as_hir_db()) != self.ingot {
            &resolved_imports_for_scope(db, scope).unnamed_resolved
        } else {
            &self.resolved_imports.unnamed_resolved
        }
    }
}

fn resolved_imports_for_scope(db: &dyn HirAnalysisDb, scope: ScopeId) -> &ResolvedImports {
    let ingot = scope.ingot(db.as_hir_db());
    super::resolve_imports(db, ingot)
}

impl NameBinding {
    /// Returns true if the binding contains an resolution that is not in the
    /// same ingot as the current resolution of the `i_use`.
    fn contains_external(&self, db: &dyn HirAnalysisDb, i_use: &IntermediateUse) -> bool {
        let Some(current_ingot) = i_use.current_scope().map(|scope| scope.ingot(db.as_hir_db())) else {
            return false;
        };
        self.resolutions.values().any(|r| match r.kind {
            NameResKind::Scope(scope) => scope.ingot(db.as_hir_db()) != current_ingot,
            NameResKind::Prim(_) => true,
        })
    }

    /// Returns true if the binding contains a glob import.
    fn contains_glob_imported(&self) -> bool {
        self.resolutions
            .values()
            .any(|r| matches!(r.derivation, NameDerivation::GlobImported(_)))
    }
}
