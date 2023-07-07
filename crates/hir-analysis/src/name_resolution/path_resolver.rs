#![allow(unused)]
use hir::hir_def::{scope_graph::ScopeId, IdentId, Partial, PathId};

use crate::{name_resolution::QueryDirective, HirAnalysisDb};

use super::{
    name_resolver::{
        NameBinding, NameRes, NameResolutionError, NameResolver, ResolvedQueryCacheStore,
    },
    NameDomain, NameQuery,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EarlyResolvedPath {
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

pub(super) struct EarlyResolvedPathWithTrajectory {
    pub(super) resolved: EarlyResolvedPath,
    /// The trajectory of the resolution, which starts with the original scope
    /// of the resolution, then goes through the resolution of each segment.
    /// This trajectory doesn't include the final resolution of the path, which
    /// is stored in `resolved`. e.g., for the query `std::foo::Bar` from
    /// `crate::baz`, the trajectory is `crate::baz -> std -> foo`.
    pub(super) trajectory: Vec<NameRes>,
}

pub type PathResolutionResult<T> = Result<T, PathResolutionError>;

#[derive(Debug, derive_more::Display, Clone, PartialEq, Eq, Hash, derive_more::Error)]
#[display(fmt = "failed_at: {failed_at}, kind: {kind}")]
pub struct PathResolutionError {
    pub(crate) kind: NameResolutionError,
    pub(crate) failed_at: usize,
}
impl PathResolutionError {
    fn new(kind: NameResolutionError, failed_at: usize) -> Self {
        Self { kind, failed_at }
    }
}

pub(super) struct EarlyPathResolver<'db, 'a, 'b> {
    db: &'db dyn HirAnalysisDb,
    name_resolver: &'a mut NameResolver<'db, 'a>,
    cache_store: &'b ResolvedQueryCacheStore,
}

impl<'db, 'a, 'b> EarlyPathResolver<'db, 'a, 'b> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        name_resolver: &'a mut NameResolver<'db, 'a>,
        cache_store: &'b ResolvedQueryCacheStore,
    ) -> Self {
        Self {
            db,
            name_resolver,
            cache_store,
        }
    }

    /// Resolves the given `path` in the given `scope`.
    pub(super) fn resolve_path(
        &mut self,
        path: PathId,
        scope: ScopeId,
    ) -> PathResolutionResult<EarlyResolvedPathWithTrajectory> {
        self.resolve_segments(path.segments(self.db.as_hir_db()), scope)
    }

    /// Resolves the given `segments` in the given `scope`.
    pub(super) fn resolve_segments(
        &mut self,
        segments: &[Partial<IdentId>],
        scope: ScopeId,
    ) -> PathResolutionResult<EarlyResolvedPathWithTrajectory> {
        let mut i_path = IntermediatePath::new(segments, scope);
        loop {
            match i_path.state(self.db) {
                IntermediatePathState::ReadyToFinalize => {
                    let binding = self.resolve_last_segment(&i_path)?;
                    return Ok(i_path.finalize_as_full(binding));
                }

                IntermediatePathState::TypeDependent => return Ok(i_path.finalize_as_partial()),

                IntermediatePathState::Unresolved => {
                    self.resolve_segment(&mut i_path)?;
                }
            }
        }
    }

    fn resolve_segment(&mut self, i_path: &mut IntermediatePath) -> PathResolutionResult<()> {
        let query = i_path.make_query(self.db)?;
        let binding = self.resolve_query(query);
        i_path.proceed(binding)
    }

    fn resolve_last_segment(
        &mut self,
        i_path: &IntermediatePath,
    ) -> PathResolutionResult<NameBinding> {
        let query = i_path.make_query(self.db)?;
        Ok(self.resolve_query(query))
    }

    fn resolve_query(&mut self, query: NameQuery) -> NameBinding {
        if let Some(binding) = self.cache_store.get(query) {
            binding.clone()
        } else {
            self.name_resolver.resolve_query(query)
        }
    }
}

struct IntermediatePath<'a> {
    path: &'a [Partial<IdentId>],
    idx: usize,
    current_res: NameRes,
    trajectory: Vec<NameRes>,
}

impl<'a> IntermediatePath<'a> {
    fn new(path: &'a [Partial<IdentId>], scope: ScopeId) -> Self {
        let domain = NameDomain::from_scope(scope);
        Self {
            path,
            idx: 0,
            current_res: NameRes::new_from_scope(
                scope,
                NameDomain::from_scope(scope),
                super::NameDerivation::Def,
            ),
            trajectory: vec![],
        }
    }

    fn make_query(&self, db: &dyn HirAnalysisDb) -> PathResolutionResult<NameQuery> {
        debug_assert!(self.state(db) != IntermediatePathState::TypeDependent);
        let Partial::Present(name) = self.path[self.idx] else {
            return Err(PathResolutionError::new(
                NameResolutionError::Invalid,
                self.idx
            ));
        };

        let Some(scope) = self.current_res.scope() else {
            return Err(PathResolutionError::new(
                NameResolutionError::NotFound,
                self.idx,
            ))
        };

        let mut directive = QueryDirective::new();
        if self.idx != 0 {
            directive.disallow_external();
            directive.disallow_lex();
        }

        Ok(NameQuery::with_directive(name, scope, directive))
    }

    fn finalize_as_partial(self) -> EarlyResolvedPathWithTrajectory {
        let resolved = EarlyResolvedPath::Partial {
            resolved: self.current_res,
            unresolved_from: self.idx,
        };

        EarlyResolvedPathWithTrajectory {
            resolved,
            trajectory: self.trajectory,
        }
    }

    fn finalize_as_full(mut self, binding: NameBinding) -> EarlyResolvedPathWithTrajectory {
        let resolved = EarlyResolvedPath::Full(binding);
        let mut trajectory = self.trajectory;
        let current_res = self.current_res;
        trajectory.push(current_res);

        EarlyResolvedPathWithTrajectory {
            resolved,
            trajectory,
        }
    }

    fn proceed(&mut self, binding: NameBinding) -> PathResolutionResult<()> {
        let next_res = binding
            .res_by_domain(NameDomain::Item)
            .clone()
            .map_err(|err| PathResolutionError::new(err, self.idx))?;

        let old_res = std::mem::replace(&mut self.current_res, next_res);
        self.idx += 1;
        self.trajectory.push(old_res);
        Ok(())
    }

    fn state(&self, db: &dyn HirAnalysisDb) -> IntermediatePathState {
        debug_assert!(self.idx < self.path.len());

        if self.current_res.is_type(db) {
            IntermediatePathState::TypeDependent
        } else if self.idx == self.path.len() - 1 {
            IntermediatePathState::ReadyToFinalize
        } else {
            IntermediatePathState::Unresolved
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IntermediatePathState {
    /// The intermediate path points to the last segment of the path and need to
    /// be resolved to finalize the path resolution.
    ReadyToFinalize,

    /// The intermediate path points to a type and the next segment need to be
    /// resolved in the type context.
    TypeDependent,

    /// The path resolution need to be continued further.
    Unresolved,
}
