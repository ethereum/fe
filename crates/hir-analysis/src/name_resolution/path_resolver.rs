use hir::{
    hir_def::{
        scope_graph::ScopeId, Enum, EnumVariant, GenericParamOwner, ItemKind, Partial, PathId,
        TypeId, VariantKind,
    },
    span::DynLazySpan,
};
use thin_vec::ThinVec;

use super::{
    diagnostics::NameResDiag,
    is_scope_visible_from,
    name_resolver::{NameRes, NameResBucket, NameResolutionError},
    resolve_query,
    visibility_checker::is_ty_visible_from,
    EarlyNameQueryId, ExpectedPathKind, NameDomain,
};
use crate::{
    name_resolution::{NameResKind, QueryDirective},
    ty::{
        adt_def::{lower_adt, AdtRef},
        binder::Binder,
        func_def::{lower_func, FuncDef, HirFuncDefKind},
        trait_def::TraitDef,
        trait_lower::lower_trait,
        ty_def::{InvalidCause, TyId},
        ty_lower::{
            collect_generic_params, lower_generic_arg_list, lower_hir_ty, lower_type_alias, TyAlias,
        },
    },
    HirAnalysisDb,
};

pub type PathResolutionResult<'db, T> = Result<T, PathResError<'db>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct PathResError<'db> {
    pub kind: PathResErrorKind<'db>,
    pub failed_at: PathId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum PathResErrorKind<'db> {
    /// The name is not found.
    NotFound(NameResBucket<'db>),

    /// The name is invalid in parsing. Basically, no need to report it because
    /// the error is already emitted from parsing phase.
    ParseError,

    /// The name is found, but it's ambiguous.
    Ambiguous(ThinVec<NameRes<'db>>),

    /// The name is found, but it can't be used in the middle of a use path.
    InvalidPathSegment(PathRes<'db>),

    /// The definition conflicts with other definitions.
    Conflict(ThinVec<DynLazySpan<'db>>),

    TooManyGenericArgs {
        expected: u16,
        given: u16,
    },

    TraitMethodNotFound(TraitDef<'db>),

    AssocTy(TyId<'db>), // TyId is parent type.
}

impl<'db> PathResError<'db> {
    pub fn new(kind: PathResErrorKind<'db>, failed_at: PathId<'db>) -> Self {
        Self { kind, failed_at }
    }

    pub fn not_found(path: PathId<'db>, bucket: NameResBucket<'db>) -> Self {
        Self::new(PathResErrorKind::NotFound(bucket), path)
    }

    pub fn parse_err(path: PathId<'db>) -> Self {
        Self::new(PathResErrorKind::ParseError, path)
    }

    pub fn from_name_res_error(err: NameResolutionError<'db>, path: PathId<'db>) -> Self {
        let kind = match err {
            NameResolutionError::NotFound => PathResErrorKind::NotFound(NameResBucket::default()),
            NameResolutionError::Invalid => PathResErrorKind::ParseError,
            NameResolutionError::Ambiguous(vec) => PathResErrorKind::Ambiguous(vec),
            NameResolutionError::Conflict(_ident, vec) => PathResErrorKind::Conflict(vec),
            NameResolutionError::Invisible(_) => unreachable!(),
            NameResolutionError::InvalidPathSegment(_) => unreachable!(),
        };
        Self::new(kind, path)
    }

    pub fn print(&self) -> String {
        match &self.kind {
            PathResErrorKind::NotFound(_) => "Not found".to_string(),
            PathResErrorKind::ParseError => "Parse error".to_string(),
            PathResErrorKind::Ambiguous(v) => format!("Ambiguous; {} options.", v.len()),
            PathResErrorKind::InvalidPathSegment(_) => "Invalid path segment".to_string(),
            PathResErrorKind::Conflict(..) => "Conflicting definitions".to_string(),
            PathResErrorKind::TooManyGenericArgs {
                expected,
                given: actual,
            } => {
                format!("Incorrect number of generic args; expected {expected}, given {actual}.")
            }
            PathResErrorKind::TraitMethodNotFound(_) => "Trait method not found".to_string(),
            PathResErrorKind::AssocTy(_) => "Types cannot be nested inside other types".to_string(),
        }
    }

    pub fn into_diag(
        self,
        db: &'db dyn HirAnalysisDb,
        path: PathId<'db>,
        span: DynLazySpan<'db>,
        expected: ExpectedPathKind,
    ) -> Option<NameResDiag<'db>> {
        let failed_at = self.failed_at;
        let ident = failed_at.ident(db).to_opt()?;

        let diag = match self.kind {
            PathResErrorKind::ParseError => unreachable!(),
            PathResErrorKind::NotFound(bucket) => {
                if let Some(nr) = bucket.iter_ok().next() {
                    if path != self.failed_at {
                        NameResDiag::InvalidPathSegment(span, ident, nr.kind.name_span(db))
                    } else {
                        match expected {
                            ExpectedPathKind::Record | ExpectedPathKind::Type => {
                                NameResDiag::ExpectedType(span, ident, nr.kind_name())
                            }
                            ExpectedPathKind::Trait => {
                                NameResDiag::ExpectedTrait(span, ident, nr.kind_name())
                            }
                            ExpectedPathKind::Value => {
                                NameResDiag::ExpectedValue(span, ident, nr.kind_name())
                            }
                            _ => NameResDiag::NotFound(span, ident),
                        }
                    }
                } else {
                    NameResDiag::NotFound(span, ident)
                }
            }

            PathResErrorKind::Ambiguous(cands) => NameResDiag::ambiguous(db, span, ident, cands),

            PathResErrorKind::AssocTy(_) => todo!(),
            PathResErrorKind::TraitMethodNotFound(_) => todo!(),
            PathResErrorKind::TooManyGenericArgs { expected, given } => {
                NameResDiag::TooManyGenericArgs {
                    span,
                    expected,
                    given,
                }
            }

            PathResErrorKind::InvalidPathSegment(res) => {
                NameResDiag::InvalidPathSegment(span, ident, res.name_span(db))
            }

            PathResErrorKind::Conflict(spans) => NameResDiag::Conflict(ident, spans),
        };
        Some(diag)
    }
}

/// Panics if `path` has more than one segment.
pub fn resolve_ident_to_bucket<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> &'db NameResBucket<'db> {
    assert!(path.parent(db).is_none());
    let query = make_query(db, path, scope);
    resolve_query(db, query)
}

/// Panics if path.ident is `Absent`
fn make_query<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> EarlyNameQueryId<'db> {
    let mut directive = QueryDirective::new();

    if path.segment_index(db) != 0 {
        directive = directive.disallow_external();
        directive = directive.disallow_lex();
    }

    let name = *path.ident(db).unwrap();
    EarlyNameQueryId::new(db, name, scope, directive)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum PathRes<'db> {
    Ty(TyId<'db>),
    TyAlias(TyAlias<'db>, TyId<'db>),
    Func(TyId<'db>),
    FuncParam(ItemKind<'db>, u16),
    Trait(TraitDef<'db>),
    EnumVariant(ResolvedVariant<'db>),
    Const(TyId<'db>),
    Mod(ScopeId<'db>),
    TypeMemberTbd(TyId<'db>),
}

impl<'db> PathRes<'db> {
    pub fn map_over_ty<F>(self, mut f: F) -> Self
    where
        F: FnMut(TyId<'db>) -> TyId<'db>,
    {
        match self {
            PathRes::Ty(ty) => PathRes::Ty(f(ty)),
            PathRes::TyAlias(alias, ty) => PathRes::TyAlias(alias, f(ty)),
            PathRes::Func(ty) => PathRes::Func(f(ty)),
            PathRes::Const(ty) => PathRes::Const(f(ty)),
            PathRes::EnumVariant(v) => PathRes::EnumVariant(ResolvedVariant { ty: f(v.ty), ..v }),
            PathRes::TypeMemberTbd(parent_ty) => PathRes::TypeMemberTbd(f(parent_ty)),
            r @ (PathRes::Trait(_) | PathRes::Mod(_) | PathRes::FuncParam(..)) => r,
        }
    }

    pub fn as_scope(&self, db: &'db dyn HirAnalysisDb) -> Option<ScopeId<'db>> {
        match self {
            PathRes::Ty(ty)
            | PathRes::Func(ty)
            | PathRes::Const(ty)
            | PathRes::TypeMemberTbd(ty) => ty.as_scope(db),
            PathRes::TyAlias(alias, _) => Some(alias.alias.scope()),
            PathRes::Trait(trait_) => Some(trait_.trait_(db).scope()),
            PathRes::EnumVariant(variant) => Some(variant.enum_(db).scope()),
            PathRes::FuncParam(item, idx) => Some(ScopeId::FuncParam(*item, *idx)),
            PathRes::Mod(scope) => Some(*scope),
        }
    }

    pub fn is_visible_from(&self, db: &'db dyn HirAnalysisDb, from_scope: ScopeId<'db>) -> bool {
        match self {
            PathRes::Ty(ty)
            | PathRes::Func(ty)
            | PathRes::Const(ty)
            | PathRes::TypeMemberTbd(ty) => is_ty_visible_from(db, *ty, from_scope),
            r => is_scope_visible_from(db, r.as_scope(db).unwrap(), from_scope),
        }
    }

    pub fn name_span(&self, db: &'db dyn HirAnalysisDb) -> Option<DynLazySpan<'db>> {
        self.as_scope(db)?.name_span(db)
    }

    pub fn pretty_path(&self, db: &'db dyn HirAnalysisDb) -> Option<String> {
        let ty_path = |ty: TyId<'db>| {
            if let Some(scope) = ty.as_scope(db) {
                scope.pretty_path(db)
            } else {
                Some(ty.pretty_print(db).to_string())
            }
        };

        match self {
            PathRes::Ty(ty) | PathRes::Func(ty) | PathRes::Const(ty) => ty_path(*ty),
            PathRes::TyAlias(alias, _) => alias.alias.scope().pretty_path(db),
            PathRes::EnumVariant(v) => Some(format!(
                "{}::{}",
                ty_path(v.ty).unwrap_or_else(|| "<missing>".into()),
                v.variant.def(db).name.to_opt()?.data(db)
            )),
            r @ (PathRes::Trait(..) | PathRes::Mod(..) | PathRes::FuncParam(..)) => {
                r.as_scope(db).unwrap().pretty_path(db)
            }
            PathRes::TypeMemberTbd(parent_ty) => Some(format!(
                "<TBD member of {}>",
                ty_path(*parent_ty).unwrap_or_else(|| "<missing>".into())
            )),
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            PathRes::Ty(_) => "type",
            PathRes::TyAlias(..) => "type alias",
            PathRes::Func(_) => "function",
            PathRes::FuncParam(..) => "function parameter",
            PathRes::Trait(_) => "trait",
            PathRes::EnumVariant(_) => "enum variant",
            PathRes::Const(_) => "constant",
            PathRes::Mod(_) => "module",
            PathRes::TypeMemberTbd(_) => "method",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct ResolvedVariant<'db> {
    pub ty: TyId<'db>,
    pub variant: EnumVariant<'db>,
    pub path: PathId<'db>,
}

impl<'db> ResolvedVariant<'db> {
    pub fn enum_(&self, db: &'db dyn HirAnalysisDb) -> Enum<'db> {
        self.ty.as_enum(db).unwrap()
    }

    pub fn kind(&self, db: &'db dyn HirAnalysisDb) -> VariantKind<'db> {
        self.variant.kind(db)
    }

    pub fn iter_field_types(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = Binder<TyId<'db>>> {
        self.ty
            .adt_def(db)
            .unwrap()
            .fields(db)
            .get(self.variant.idx as usize)
            .unwrap()
            .iter_types(db)
    }

    pub fn constructor_func_ty(&self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let mut ty = TyId::func(db, self.to_funcdef(db)?);

        for &arg in self.ty.generic_args(db) {
            if ty.applicable_ty(db).is_some() {
                ty = TyId::app(db, ty, arg);
            }
        }
        Some(ty)
    }

    pub fn to_funcdef(&self, db: &'db dyn HirAnalysisDb) -> Option<FuncDef<'db>> {
        if !matches!(self.variant.kind(db), VariantKind::Tuple(_)) {
            return None;
        }

        let arg_tys = self.iter_field_types(db).collect();
        let adt = self.ty.adt_def(db).unwrap();

        let mut ret_ty = TyId::adt(db, adt);
        ret_ty = TyId::foldl(db, ret_ty, adt.param_set(db).params(db));

        Some(FuncDef::new(
            db,
            HirFuncDefKind::VariantCtor(self.variant),
            *self.variant.def(db).name.unwrap(),
            *adt.param_set(db),
            arg_tys,
            Binder::bind(ret_ty),
        ))
    }
}

pub fn resolve_path<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    resolve_tail_as_value: bool,
) -> PathResolutionResult<'db, PathRes<'db>> {
    resolve_path_impl(db, path, scope, resolve_tail_as_value, true, &mut |_, _| {})
}

/// Resolve a specific path segment by index, returning the definition at that segment.
pub fn resolve_path_segment<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    seg_idx: usize,
    scope: ScopeId<'db>,
    resolve_tail_as_value: bool,
) -> PathResolutionResult<'db, PathRes<'db>> {
    if seg_idx > path.segment_index(db) { return Err(PathResError::parse_err(path)); }
    let mut current_parent: Option<PathRes<'db>> = None;
    let mut parent_scope = scope;
    for i in 0..=seg_idx {
        let segment = path.segment(db, i).unwrap();
        let query_scope = match &current_parent {
            Some(res) => res.as_scope(db).unwrap_or(parent_scope),
            None => parent_scope,
        };
        let query = make_query(db, segment, query_scope);
        let bucket = resolve_query(db, query);
        let nameres = if i == path.segment_index(db) && resolve_tail_as_value {
            match bucket.pick(NameDomain::VALUE) {
                Ok(res) => res.clone(),
                Err(_) => pick_type_domain_from_bucket(bucket, segment)?,
            }
        } else {
            pick_type_domain_from_bucket(bucket, segment)?
        };
        let reso = resolve_name_res(db, &nameres, current_parent.clone(), segment, query_scope)?;
        current_parent = Some(reso);
        parent_scope = query_scope;
    }
    Ok(current_parent.unwrap())
}

/// Resolve and return the raw ScopeId for the specified path segment if the name directly maps to a scope
pub fn resolve_path_segment_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    seg_idx: usize,
    scope: ScopeId<'db>,
    resolve_tail_as_value: bool,
) -> Option<ScopeId<'db>> {
    if seg_idx > path.segment_index(db) { return None; }
    let mut current_parent: Option<PathRes<'db>> = None;
    let mut parent_scope = scope;
    for i in 0..=seg_idx {
        let segment = path.segment(db, i).unwrap();
        let query_scope = current_parent.as_ref().and_then(|r| r.as_scope(db)).unwrap_or(parent_scope);
        let query = make_query(db, segment, query_scope);
        let bucket = resolve_query(db, query);
        let nameres = if i == path.segment_index(db) && resolve_tail_as_value {
            match bucket.pick(NameDomain::VALUE) {
                Ok(res) => res.clone(),
                Err(_) => match bucket.pick(NameDomain::TYPE) { Ok(res) => res.clone(), Err(_) => return None },
            }
        } else {
            match bucket.pick(NameDomain::TYPE) { Ok(res) => res.clone(), Err(_) => return None }
        };
        if i == seg_idx {
            if let super::name_resolver::NameResKind::Scope(s) = nameres.kind { return Some(s); }
        }
        let reso = resolve_name_res(db, &nameres, current_parent.clone(), segment, query_scope).ok()?;
        current_parent = Some(reso);
        parent_scope = query_scope;
    }
    None
}

pub fn resolve_path_with_observer<'db, F>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    resolve_tail_as_value: bool,
    observer: &mut F,
) -> PathResolutionResult<'db, PathRes<'db>>
where
    F: FnMut(PathId<'db>, &PathRes<'db>),
{
    resolve_path_impl(db, path, scope, resolve_tail_as_value, true, observer)
}

/// Resolve the tail segment of a path in VALUE domain and return its concrete item scope if any.
pub fn resolve_tail_value_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> Option<ScopeId<'db>> {
    let parent_scope = path
        .parent(db)
        .and_then(|p| resolve_path(db, p, scope, false).ok().and_then(|r| r.as_scope(db)))
        .unwrap_or(scope);
    let query = make_query(db, path, parent_scope);
    let bucket = resolve_query(db, query);
    if let Ok(res) = bucket.pick(NameDomain::VALUE) {
        match res.kind {
            super::name_resolver::NameResKind::Scope(s) => Some(s),
            _ => None,
        }
    } else {
        None
    }
}

fn resolve_path_impl<'db, F>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    resolve_tail_as_value: bool,
    is_tail: bool,
    observer: &mut F,
) -> PathResolutionResult<'db, PathRes<'db>>
where
    F: FnMut(PathId<'db>, &PathRes<'db>),
{
    let parent_res = path
        .parent(db)
        .map(|path| resolve_path_impl(db, path, scope, resolve_tail_as_value, false, observer))
        .transpose()?;

    if !path.ident(db).is_present() {
        return Err(PathResError::parse_err(path));
    }

    let parent_scope = parent_res
        .as_ref()
        .and_then(|r| r.as_scope(db))
        .unwrap_or(scope);

    match parent_res {
        Some(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => {
            // Try to resolve as an enum variant
            if let Some(enum_) = ty.as_enum(db) {
                // We need to use the concrete enum scope instead of
                // parent_scope to resolve the variants in all cases,
                // eg when parent is `Self`. I'm not really sure why this is.
                let query = make_query(db, path, enum_.scope());
                let bucket = resolve_query(db, query);

                if let Ok(res) = bucket.pick(NameDomain::VALUE) {
                    if let Some(var) = res.enum_variant() {
                        let reso = PathRes::EnumVariant(ResolvedVariant {
                            ty,
                            variant: var,
                            path,
                        });
                        observer(path, &reso);
                        return Ok(reso);
                    }
                }
            }
            if is_tail {
                let r = PathRes::TypeMemberTbd(ty);
                observer(path, &r);
                return Ok(r);
            } else {
                todo!() // assoc type error
            }
        }

        Some(PathRes::Func(_) | PathRes::EnumVariant(..)) => {
            return Err(PathResError::new(
                PathResErrorKind::InvalidPathSegment(parent_res.unwrap()),
                path,
            ));
        }
        Some(PathRes::TypeMemberTbd(_) | PathRes::FuncParam(..)) => unreachable!(),
        Some(PathRes::Const(_) | PathRes::Mod(_) | PathRes::Trait(_)) | None => {}
    };

    let query = make_query(db, path, parent_scope);
    let bucket = resolve_query(db, query);

    let res = if is_tail && resolve_tail_as_value {
        match bucket.pick(NameDomain::VALUE) {
            Ok(res) => res.clone(),
            Err(_) => pick_type_domain_from_bucket(bucket, path)?,
        }
    } else {
        pick_type_domain_from_bucket(bucket, path)?
    };
    let reso = resolve_name_res(db, &res, parent_res, path, scope)?;

    observer(path, &reso);
    Ok(reso)
}

pub fn resolve_name_res<'db>(
    db: &'db dyn HirAnalysisDb,
    nameres: &NameRes<'db>,
    parent_ty: Option<PathRes<'db>>,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> PathResolutionResult<'db, PathRes<'db>> {
    let args = &lower_generic_arg_list(db, path.generic_args(db), scope);
    let res = match nameres.kind {
        NameResKind::Prim(prim) => {
            let ty = TyId::from_hir_prim_ty(db, prim);
            PathRes::Ty(TyId::foldl(db, ty, args))
        }
        NameResKind::Scope(scope_id) => match scope_id {
            ScopeId::Item(item) => match item {
                ItemKind::Struct(_) | ItemKind::Contract(_) | ItemKind::Enum(_) => {
                    let adt_ref = AdtRef::try_from_item(item).unwrap();
                    PathRes::Ty(ty_from_adtref(db, adt_ref, args)?)
                }

                ItemKind::TopMod(_) | ItemKind::Mod(_) => PathRes::Mod(scope_id),

                ItemKind::Func(func) => {
                    let func_def = lower_func(db, func).unwrap();
                    let ty = TyId::func(db, func_def);
                    PathRes::Func(TyId::foldl(db, ty, args))
                }
                ItemKind::Const(const_) => {
                    // TODO err if any args
                    let ty = if let Some(ty) = const_.ty(db).to_opt() {
                        lower_hir_ty(db, ty, scope)
                    } else {
                        TyId::invalid(db, InvalidCause::Other)
                    };
                    PathRes::Const(ty)
                }

                ItemKind::TypeAlias(type_alias) => {
                    let alias = lower_type_alias(db, type_alias);
                    if args.len() < alias.params(db).len() {
                        PathRes::TyAlias(
                            alias.clone(),
                            TyId::invalid(
                                db,
                                InvalidCause::UnboundTypeAliasParam {
                                    alias: type_alias,
                                    n_given_args: args.len(),
                                },
                            ),
                        )
                    } else {
                        PathRes::TyAlias(alias.clone(), alias.alias_to.instantiate(db, args))
                    }
                }

                ItemKind::Impl(impl_) => {
                    PathRes::Ty(impl_typeid_to_ty(db, path, impl_.ty(db), scope, args)?)
                }
                ItemKind::ImplTrait(impl_) => {
                    PathRes::Ty(impl_typeid_to_ty(db, path, impl_.ty(db), scope, args)?)
                }

                ItemKind::Trait(t) => {
                    if path.is_self_ty(db) {
                        let params = collect_generic_params(db, t.into());
                        let ty = params.trait_self(db).unwrap();
                        let ty = TyId::foldl(db, ty, args);
                        PathRes::Ty(ty)
                    } else {
                        PathRes::Trait(lower_trait(db, t))
                    }
                }

                ItemKind::Use(_) | ItemKind::Body(_) => unreachable!(),
            },
            ScopeId::GenericParam(parent, idx) => {
                let owner = GenericParamOwner::from_item_opt(parent).unwrap();
                let param_set = collect_generic_params(db, owner);
                let ty = param_set.param_by_original_idx(db, idx as usize).unwrap();
                let ty = TyId::foldl(db, ty, args);
                PathRes::Ty(ty)
            }

            ScopeId::Variant(var) => {
                let enum_ty = if let Some(PathRes::Ty(ty)) = parent_ty {
                    ty
                } else {
                    // The variant was imported via `use`.
                    debug_assert!(path.parent(db).is_none());
                    ty_from_adtref(db, var.enum_.into(), &[])?
                };
                // TODO report error if args isn't empty
                PathRes::EnumVariant(ResolvedVariant {
                    ty: enum_ty,
                    variant: var,
                    path,
                })
            }
            ScopeId::FuncParam(item, idx) => PathRes::FuncParam(item, idx),
            ScopeId::Field(..) => unreachable!(),
            ScopeId::Block(..) => unreachable!(),
        },
    };
    Ok(res)
}

fn impl_typeid_to_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    hir_ty: Partial<TypeId<'db>>,
    scope: ScopeId<'db>,
    args: &[TyId<'db>],
) -> PathResolutionResult<'db, TyId<'db>> {
    if let Some(hir_ty) = hir_ty.to_opt() {
        let ty = lower_hir_ty(db, hir_ty, scope); // root scope!
        Ok(TyId::foldl(db, ty, args))
    } else {
        Err(PathResError::parse_err(path))
    }
}

fn ty_from_adtref<'db>(
    db: &'db dyn HirAnalysisDb,
    adt_ref: AdtRef<'db>,
    args: &[TyId<'db>],
) -> PathResolutionResult<'db, TyId<'db>> {
    let adt = lower_adt(db, adt_ref);
    let ty = TyId::adt(db, adt);
    Ok(TyId::foldl(db, ty, args))
}

fn pick_type_domain_from_bucket<'db>(
    bucket: &NameResBucket<'db>,
    path: PathId<'db>,
) -> PathResolutionResult<'db, NameRes<'db>> {
    bucket
        .pick(NameDomain::TYPE)
        .clone()
        .map_err(|err| match err {
            NameResolutionError::NotFound => PathResError::not_found(path, bucket.clone()),
            err => PathResError::from_name_res_error(err, path),
        })
}
