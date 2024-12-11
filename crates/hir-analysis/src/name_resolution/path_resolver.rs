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
pub enum EarlyResolvedPath<'db, T> {
    Full(T),

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

// xxx check is_root behavior
// xxx resolve_ident_in_scope
pub fn resolve_path_tail_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> Option<&'db NameResBucket<'db>> {
    let query = make_query(db, path, scope).ok()?;
    Some(resolve_query(db, query))
}

fn make_query<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> PathResolutionResult<'db, EarlyNameQueryId<'db>> {
    let Partial::Present(name) = path.ident(db.as_hir_db()) else {
        return Err(PathResolutionError::new(NameResolutionError::Invalid, path));
    };

    let mut directive = QueryDirective::new();

    if path.segment_index(db.as_hir_db()) != 0 {
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

pub trait Observer<'db> {
    fn did_resolve(&mut self, path: PathId<'db>, scope: ScopeId<'db>, res: &NameRes<'db>);
}
pub struct ResolveEarly<T> {
    pub observer: T,
}
impl<T> ResolveEarly<T> {
    pub fn into_inner(self) -> T {
        self.observer
    }
}
impl ResolveEarly<()> {
    pub fn new() -> Self {
        Self { observer: () }
    }
}

impl ResolveEarly<Trajectory<'_>> {
    pub fn with_trajectory() -> Self {
        Self {
            observer: Trajectory::default(),
        }
    }
}

#[derive(Default)]
pub struct Trajectory<'db> {
    root_scope: Option<ScopeId<'db>>,
    resolutions: Vec<(PathId<'db>, NameRes<'db>)>,
}
impl<'db> Trajectory<'db> {
    pub(super) fn find_invisible_segment(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(PathId<'db>, &NameRes<'db>)> {
        for (path, res) in &self.resolutions {
            if !res.is_visible(db, self.root_scope.unwrap()) {
                return Some((*path, res));
            }
        }
        None
    }
}
impl<'db> Observer<'db> for Trajectory<'db> {
    fn did_resolve(&mut self, path: PathId<'db>, scope: ScopeId<'db>, res: &NameRes<'db>) {
        if self.root_scope.is_none() {
            self.root_scope = Some(scope);
        }
        self.resolutions.push((path, res.clone()));
    }
}

impl<'db> Observer<'db> for () {
    fn did_resolve(&mut self, _path: PathId, _scope: ScopeId, _res: &NameRes) {}
}

impl<'db, T: Observer<'db>> Resolver<'db> for ResolveEarly<T> {
    type Output = PathResolutionResult<'db, EarlyResolvedPath<'db, &'db NameResBucket<'db>>>;

    fn resolve_path(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        path: PathId<'db>,
        mut scope: ScopeId<'db>,
    ) -> Self::Output {
        let hir_db = db.as_hir_db();
        let parent = path.parent(hir_db);

        if let Some(parent) = parent {
            match self.resolve_path(db, parent, scope)? {
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

                    let should_partial = res.is_type()
                        || res.is_trait()
                        || parent.root_ident(hir_db) == Some(IdentId::make_self_ty(hir_db));

                    if should_partial {
                        return Ok(EarlyResolvedPath::Partial { path: parent, res });
                    }
                    self.observer.did_resolve(path, scope, &res);

                    scope = res.scope().ok_or_else(|| {
                        PathResolutionError::new(
                            NameResolutionError::InvalidPathSegment(res.clone()), // xxx used to be notfound?
                            path,
                        )
                    })?;
                }
            }
        }
        let query = make_query(db, path, scope)?;
        Ok(EarlyResolvedPath::Full(resolve_query(db, query)))
    }
}

#[derive(Default)]
pub struct ResolveToTypeDomain {}
impl<'db> Resolver<'db> for ResolveToTypeDomain {
    type Output = Option<EarlyResolvedPath<'db, NameRes<'db>>>;

    fn resolve_path(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        path: PathId<'db>,
        scope: ScopeId<'db>,
    ) -> Self::Output {
        match ResolveEarly::new().resolve_path(db, path, scope).ok()? {
            EarlyResolvedPath::Full(bucket) => {
                let res = bucket.pick(NameDomain::TYPE).clone().ok()?;
                Some(EarlyResolvedPath::Full(res))
            }
            EarlyResolvedPath::Partial { path, res } => {
                Some(EarlyResolvedPath::Partial { path, res })
            }
        }
    }
}
