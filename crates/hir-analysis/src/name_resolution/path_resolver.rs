use hir::hir_def::{scope_graph::ScopeId, IdentId, Partial, PathId};

use super::{
    name_resolver::{NameRes, NameResBucket, NameResolutionError},
    resolve_query, EarlyNameQueryId, NameDomain,
};
use crate::{name_resolution::QueryDirective, HirAnalysisDb};

/// The result of early path resolution.
/// There are two kinds of early resolution results:
/// 1. Fully resolved path, which is a path that is fully resolved to concrete
///    items.
/// 2. Partially resolved path. This happens when the path is partially resolved
///    to a type, and the rest of the path depends on the type to resolve.
///    Type/Trait context is needed to resolve the rest of the path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EarlyResolvedPath<'db> {
    Full(&'db NameResBucket<'db>),

    /// The path is partially resolved; this means that the `resolved` is a type
    /// and the following segments depend on type to resolve.
    /// These unresolved parts are resolved in the later type inference and
    /// trait solving phases.
    Partial {
        res: NameRes<'db>,
        unresolved_from: usize,
    },
}

pub(super) struct EarlyResolvedPathWithTrajectory<'db> {
    pub(super) resolved: EarlyResolvedPath<'db>,
    /// The trajectory of the resolution, which starts with the original scope
    /// of the resolution, then goes through the resolution of each segment.
    /// This trajectory doesn't include the final resolution of the path, which
    /// is stored in `resolved`. e.g., for the query `std::foo::Bar` from
    /// `crate::baz`, the trajectory is `crate::baz -> std -> foo`.
    pub(super) trajectory: Vec<NameRes<'db>>,
}

impl<'db> EarlyResolvedPathWithTrajectory<'db> {
    pub(super) fn find_invisible_segment(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(usize, &NameRes<'db>)> {
        let original_scope = self.trajectory.first().unwrap().scope().unwrap();
        for (i, res) in self.trajectory[1..].iter().enumerate() {
            if !res.is_visible(db, original_scope) {
                return Some((i, res));
            }
        }

        None
    }
}

pub type PathResolutionResult<'db, T> = Result<T, PathResolutionError<'db>>;

#[derive(Debug, derive_more::Display, Clone, PartialEq, Eq, Hash, derive_more::Error)]
#[display(fmt = "failed_at: {failed_at}, kind: {kind}")]
pub struct PathResolutionError<'db> {
    pub(crate) kind: NameResolutionError<'db>,
    pub(crate) failed_at: usize,
}
impl<'db> PathResolutionError<'db> {
    fn new(kind: NameResolutionError<'db>, failed_at: usize) -> Self {
        Self { kind, failed_at }
    }
}

pub(super) struct EarlyPathResolver<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> EarlyPathResolver<'db> {
    pub(super) fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }

    /// Resolves the given `path` in the given `scope`.
    pub(super) fn resolve_path(
        &mut self,
        path: PathId<'db>,
        scope: ScopeId<'db>,
    ) -> PathResolutionResult<'db, EarlyResolvedPathWithTrajectory<'db>> {
        self.resolve_segments(path.segments(self.db.as_hir_db()), scope)
    }

    /// Resolves the given `segments` in the given `scope`.
    pub(super) fn resolve_segments(
        &mut self,
        segments: &[Partial<IdentId<'db>>],
        scope: ScopeId<'db>,
    ) -> PathResolutionResult<'db, EarlyResolvedPathWithTrajectory<'db>> {
        let mut i_path = IntermediatePath::new(self.db, segments, scope);
        loop {
            match i_path.state(self.db) {
                IntermediatePathState::Full => {
                    let bucket = self.resolve_last_segment(&i_path)?;
                    return Ok(i_path.finalize_as_full(bucket));
                }

                IntermediatePathState::Partial => return Ok(i_path.finalize_as_partial()),

                IntermediatePathState::Unresolved => {
                    self.resolve_segment(&mut i_path)?;
                }
            }
        }
    }

    fn resolve_segment(
        &mut self,
        i_path: &mut IntermediatePath<'db, '_>,
    ) -> PathResolutionResult<'db, ()> {
        let query = i_path.make_query(self.db)?;
        let bucket = self.resolve_query(query);
        i_path.proceed(bucket)
    }

    fn resolve_last_segment(
        &mut self,
        i_path: &IntermediatePath<'db, '_>,
    ) -> PathResolutionResult<'db, &'db NameResBucket<'db>> {
        let query = i_path.make_query(self.db)?;
        Ok(self.resolve_query(query))
    }

    fn resolve_query(&mut self, query: EarlyNameQueryId<'db>) -> &'db NameResBucket<'db> {
        resolve_query(self.db, query)
    }
}

struct IntermediatePath<'db, 'a> {
    path: &'a [Partial<IdentId<'db>>],
    idx: usize,
    current_res: NameRes<'db>,
    trajectory: Vec<NameRes<'db>>,
}

impl<'db, 'a> IntermediatePath<'db, 'a> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        path: &'a [Partial<IdentId<'db>>],
        scope: ScopeId<'db>,
    ) -> Self {
        Self {
            path,
            idx: 0,
            current_res: NameRes::new_from_scope(
                scope,
                NameDomain::from_scope(db, scope),
                super::NameDerivation::Def,
            ),
            trajectory: vec![],
        }
    }

    fn starts_with(&self, ident: IdentId) -> bool {
        let Some(Partial::Present(first_seg)) = self.path.first() else {
            return false;
        };

        *first_seg == ident
    }

    /// Make a `NameQuery` to resolve the current segment.
    fn make_query(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> PathResolutionResult<'db, EarlyNameQueryId<'db>> {
        debug_assert!(self.state(db) != IntermediatePathState::Partial);
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
            directive = directive.disallow_external();
            directive = directive.disallow_lex();
        }

        Ok(EarlyNameQueryId::new(db, name, scope, directive))
    }

    /// Finalizes the `IntermediatePath` as a `EarlyResolvedPath::Partial`.
    fn finalize_as_partial(self) -> EarlyResolvedPathWithTrajectory<'db> {
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
    fn finalize_as_full(
        self,
        bucket: &'db NameResBucket<'db>,
    ) -> EarlyResolvedPathWithTrajectory<'db> {
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
    fn proceed(&mut self, bucket: &NameResBucket<'db>) -> PathResolutionResult<'db, ()> {
        let next_res = match bucket.pick(NameDomain::TYPE) {
            Ok(res) => Ok(res.clone()),
            Err(NameResolutionError::NotFound) => {
                if let Some(res) = bucket.iter_ok().next() {
                    Err(PathResolutionError::new(
                        NameResolutionError::InvalidPathSegment(res.clone()),
                        self.idx,
                    ))
                } else {
                    Err(PathResolutionError::new(
                        NameResolutionError::NotFound,
                        self.idx,
                    ))
                }
            }
            Err(err) => Err(PathResolutionError::new(err.clone(), self.idx)),
        }?;

        let old_res = std::mem::replace(&mut self.current_res, next_res);
        self.idx += 1;
        self.trajectory.push(old_res);
        Ok(())
    }

    fn state(&self, db: &'db dyn HirAnalysisDb) -> IntermediatePathState {
        debug_assert!(self.idx < self.path.len());

        let should_partial = (self.current_res.is_type()
            || self.current_res.is_trait()
            || self.starts_with(IdentId::make_self_ty(db.as_hir_db())))
            && self.idx != 0;

        if (self.idx == self.path.len() - 1) && !should_partial {
            IntermediatePathState::Full
        } else if should_partial {
            IntermediatePathState::Partial
        } else {
            IntermediatePathState::Unresolved
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IntermediatePathState {
    /// The intermediate path points to the last segment of the path and need to
    /// be resolved to finalize the path resolution.
    Full,

    /// The intermediate path points to a type and the next segment need to be
    /// resolved with the type context.
    Partial,

    /// The path resolution need to be continued further.
    Unresolved,
}
