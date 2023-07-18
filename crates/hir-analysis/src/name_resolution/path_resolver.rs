#![allow(unused)]
use hir::hir_def::{scope_graph::ScopeId, IdentId, Partial, PathId};

use crate::{name_resolution::QueryDirective, HirAnalysisDb};

use super::{
    name_resolver::{
        NameRes, NameResBucket, NameResolutionError, NameResolver, ResolvedQueryCacheStore,
    },
    NameDomain, NameQuery,
};

/// The result of early path resolution.
/// There are two kinds of early resolution results:
/// 1. Fully resolved path, which is a path that is fully resolved to concrete
///    items.
/// 2. Partially resolved path. This happens when the path is partially resolved
///    to a type, and the rest of the path depends on the type to resolve.
///    Type/Trait context is needed to resolve the rest of the path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EarlyResolvedPath {
    Full(NameResBucket),

    /// The path is partially resolved; this means that the `resolved` is a type
    /// and the following segments depend on type to resolve.
    /// These unresolved parts are resolved in the later type inference and
    /// trait solving phases.
    Partial {
        res: NameRes,
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

impl EarlyResolvedPathWithTrajectory {
    pub(super) fn find_invisible_segment(
        &self,
        db: &dyn HirAnalysisDb,
    ) -> Option<(usize, &NameRes)> {
        let original_scope = self.trajectory.first().unwrap().scope().unwrap();
        for (i, res) in self.trajectory[1..].iter().enumerate() {
            if !res.is_visible(db, original_scope) {
                return Some((i, res));
            }
        }

        None
    }

    pub(super) fn resolved_at(&self, index: usize) -> &NameRes {
        &self.trajectory[index]
    }
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

pub(super) struct EarlyPathResolver<'db, 'a, 'b, 'c> {
    db: &'db dyn HirAnalysisDb,
    name_resolver: &'a mut NameResolver<'db, 'b>,
    cache_store: &'c ResolvedQueryCacheStore,
}

impl<'db, 'a, 'b, 'c> EarlyPathResolver<'db, 'a, 'b, 'c> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        name_resolver: &'a mut NameResolver<'db, 'b>,
        cache_store: &'c ResolvedQueryCacheStore,
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
                    let bucket = self.resolve_last_segment(&i_path)?;
                    return Ok(i_path.finalize_as_full(bucket));
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
        let bucket = self.resolve_query(query);
        i_path.proceed(bucket)
    }

    fn resolve_last_segment(
        &mut self,
        i_path: &IntermediatePath,
    ) -> PathResolutionResult<NameResBucket> {
        let query = i_path.make_query(self.db)?;
        Ok(self.resolve_query(query))
    }

    fn resolve_query(&mut self, query: NameQuery) -> NameResBucket {
        if let Some(bucket) = self.cache_store.get(query) {
            bucket.clone()
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

    /// Make a `NameQuery` to resolve the current segment.
    fn make_query(&self, db: &dyn HirAnalysisDb) -> PathResolutionResult<NameQuery> {
        debug_assert!(self.state(db) != IntermediatePathState::TypeDependent);
        let Partial::Present(name) = self.path[self.idx] else {
            return Err(PathResolutionError::new(
                NameResolutionError::Invalid,
                self.idx,
            ));
        };

        let Some(scope) = self.current_res.scope() else {
            return Err(PathResolutionError::new(
                NameResolutionError::NotFound,
                self.idx,
            ));
        };

        let mut directive = QueryDirective::new();
        if self.idx != 0 {
            directive.disallow_external();
            directive.disallow_lex();
        }

        Ok(NameQuery::with_directive(name, scope, directive))
    }

    /// Finalizes the `IntermediatePath` as a `EarlyResolvedPath::Partial`.
    fn finalize_as_partial(self) -> EarlyResolvedPathWithTrajectory {
        let resolved = EarlyResolvedPath::Partial {
            res: self.current_res.clone(),
            unresolved_from: self.idx,
        };

        let mut trajectory = self.trajectory;
        let current_res = self.current_res;
        trajectory.push(current_res);

        EarlyResolvedPathWithTrajectory {
            resolved,
            trajectory,
        }
    }

    /// Finalizes the `IntermediatePath` as a `EarlyResolvedPath::Full`.
    fn finalize_as_full(mut self, bucket: NameResBucket) -> EarlyResolvedPathWithTrajectory {
        let resolved = EarlyResolvedPath::Full(bucket);
        let mut trajectory = self.trajectory;
        let current_res = self.current_res;
        trajectory.push(current_res);

        EarlyResolvedPathWithTrajectory {
            resolved,
            trajectory,
        }
    }

    /// Proceeds to the next segment with the given `bucket`.
    /// If the `bucket` doesn't contain proper resolution, then an error is
    /// returned.
    fn proceed(&mut self, bucket: NameResBucket) -> PathResolutionResult<()> {
        let next_res = bucket
            .pick(NameDomain::Type)
            .clone()
            .map_err(|err| PathResolutionError::new(err, self.idx))?;

        let old_res = std::mem::replace(&mut self.current_res, next_res);
        self.idx += 1;
        self.trajectory.push(old_res);
        Ok(())
    }

    fn state(&self, db: &dyn HirAnalysisDb) -> IntermediatePathState {
        debug_assert!(self.idx < self.path.len());

        let is_type_dependent =
            (self.current_res.is_type() || self.current_res.is_trait()) && self.idx != 0;

        if (self.idx == self.path.len() - 1) && !is_type_dependent {
            IntermediatePathState::ReadyToFinalize
        } else if is_type_dependent {
            IntermediatePathState::TypeDependent
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
    /// resolved with the type context.
    TypeDependent,

    /// The path resolution need to be continued further.
    Unresolved,
}
