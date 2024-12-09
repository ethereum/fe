use hir::hir_def::{scope_graph::ScopeId, IdentId, Partial, PathId};
use smallvec::SmallVec;

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

    // xxx update comment
    /// The path is partially resolved; this means that the `resolved` is a type
    /// and the following segments depend on type to resolve.
    /// These unresolved parts are resolved in the later type inference and
    /// trait solving phases.
    Partial {
        path: PathId<'db>,
        res: NameRes<'db>,
    },
}

#[derive(Debug)]
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
#[display(fmt = "failed_at: xxx, kind: {kind}")]
pub struct PathResolutionError<'db> {
    pub(crate) kind: NameResolutionError<'db>,
    pub(crate) failed_at: PathId<'db>,
}
impl<'db> PathResolutionError<'db> {
    fn new(kind: NameResolutionError<'db>, failed_at: PathId<'db>) -> Self {
        Self { kind, failed_at }
    }
}

pub(super) fn resolve_path<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> PathResolutionResult<'db, EarlyResolvedPathWithTrajectory<'db>> {
    let mut trajectory = vec![NameRes::new_from_scope(
        scope,
        NameDomain::from_scope(db, scope),
        super::NameDerivation::Def,
    )];
    let resolved = resolve_path_rec(db, path, scope, &mut trajectory)?;

    Ok(EarlyResolvedPathWithTrajectory {
        resolved,
        trajectory,
    })
}

fn resolve_path_rec<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    mut scope: ScopeId<'db>,
    trajectory: &mut Vec<NameRes<'db>>,
) -> PathResolutionResult<'db, EarlyResolvedPath<'db>> {
    let hir_db = db.as_hir_db();
    let parent = path.parent(hir_db);
    if let Some(parent) = parent {
        match resolve_path_rec(db, parent, scope, trajectory)? {
            part @ EarlyResolvedPath::Partial { .. } => return Ok(part),
            EarlyResolvedPath::Full(bucket) => {
                let res = bucket
                    .pick(NameDomain::TYPE)
                    .clone()
                    .map_err(|err| match err {
                        NameResolutionError::NotFound => {
                            if let Some(res) = bucket.iter_ok().next() {
                                PathResolutionError::new(
                                    NameResolutionError::InvalidPathSegment(res.clone()),
                                    path,
                                )
                            } else {
                                PathResolutionError::new(NameResolutionError::NotFound, path)
                            }
                        }
                        err => PathResolutionError::new(err.clone(), path),
                    })?;

                let should_partial = parent.parent(hir_db).is_some()
                    && (res.is_type()
                        || res.is_trait()
                        || parent.root_ident(hir_db) == Some(IdentId::make_self_ty(hir_db)));
                if should_partial {
                    trajectory.push(res.clone());
                    return Ok(EarlyResolvedPath::Partial { path: parent, res });
                }
                scope = res.scope().ok_or_else(|| {
                    PathResolutionError::new(
                        NameResolutionError::InvalidPathSegment(res.clone()),
                        path,
                    )
                })?;
                trajectory.push(res);
            }
        }
    }
    let query = make_query(db, path, parent.is_none(), scope)?;
    Ok(EarlyResolvedPath::Full(resolve_query(db, query)))
}

pub fn resolve_path_tail_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> Option<EarlyResolvedPath<'db>> {
    let query = make_query(db, path, false, scope).ok()?;
    Some(EarlyResolvedPath::Full(resolve_query(db, query)))
}

fn make_query<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    is_root: bool,
    scope: ScopeId<'db>,
) -> PathResolutionResult<'db, EarlyNameQueryId<'db>> {
    let Partial::Present(name) = path.ident(db.as_hir_db()) else {
        return Err(PathResolutionError::new(NameResolutionError::Invalid, path));
    };

    let mut directive = QueryDirective::new();
    if !is_root {
        directive = directive.disallow_external();
        directive = directive.disallow_lex();
    }

    Ok(EarlyNameQueryId::new(db, name, scope, directive))
}

pub trait Resolver<'db> {
    type Output;

    fn resolve_path(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        path: PathId<'db>,
        scope: ScopeId<'db>,
    ) -> Self::Output;
}

pub struct ResolveEarly {}
impl<'db> Resolver<'db> for ResolveEarly {
    type Output = PathResolutionResult<'db, EarlyResolvedPath<'db>>;

    fn resolve_path(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        path: PathId<'db>,
        scope: ScopeId<'db>,
    ) -> Self::Output {
        todo!()
    }
}

pub struct ResolveToBucket {}
impl<'db> Resolver<'db> for ResolveToBucket {
    type Output = PathResolutionResult<'db, (&'db NameResBucket<'db>, Option<&'db NameRes<'db>>)>;

    fn resolve_path(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        path: PathId<'db>,
        mut scope: ScopeId<'db>,
    ) -> Self::Output {
        let hir_db = db.as_hir_db();
        let parent = path.parent(hir_db);
        let parent_res = parent
            .map(|parent| {
                let res = ResolveToTypeDomain::default().resolve_path(db, parent, scope)?;
                scope = res.scope().ok_or_else(|| {
                    PathResolutionError::new(
                        NameResolutionError::InvalidPathSegment(res.clone()),
                        parent,
                    )
                })?;
                Ok(res)
            })
            .transpose()?;

        let query = make_query(db, path, parent.is_none(), scope)?;
        let bucket = resolve_query(db, query);

        Ok((bucket, parent_res))
    }
}

#[derive(Default)]
pub struct ResolveToTypeDomain<'db> {
    trajectory: SmallVec<[&'db NameRes<'db>; 4]>,
}
impl<'db> ResolveToTypeDomain<'db> {
    fn trajectory(self) -> SmallVec<[&'db NameRes<'db>; 4]> {
        self.trajectory
    }
}

impl<'db> Resolver<'db> for ResolveToTypeDomain<'db> {
    type Output = PathResolutionResult<'db, &'db NameRes<'db>>;

    fn resolve_path(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        path: PathId<'db>,
        mut scope: ScopeId<'db>,
    ) -> Self::Output {
        let hir_db = db.as_hir_db();
        let parent = path.parent(hir_db);
        if let Some(parent) = parent {
            let res = self.resolve_path(db, path, scope)?;
            scope = as_scope(parent, res)?;
        }

        let query = make_query(db, path, parent.is_none(), scope)?;
        let bucket = resolve_query(db, query);
        let res = bucket
            .pick(NameDomain::TYPE)
            .as_ref()
            .map_err(|err| match err {
                NameResolutionError::NotFound => {
                    if let Some(res) = bucket.iter_ok().next() {
                        PathResolutionError::new(
                            NameResolutionError::InvalidPathSegment(res.clone()),
                            path,
                        )
                    } else {
                        PathResolutionError::new(NameResolutionError::NotFound, path)
                    }
                }
                err => PathResolutionError::new(err.clone(), path),
            })?;
        self.trajectory.push(res);
        Ok(res)
    }
}

fn as_scope<'db>(
    path: PathId<'db>,
    res: &'db NameRes<'db>,
) -> PathResolutionResult<'db, ScopeId<'db>> {
    res.scope().ok_or_else(|| {
        PathResolutionError::new(NameResolutionError::InvalidPathSegment(res.clone()), path)
    })
}
