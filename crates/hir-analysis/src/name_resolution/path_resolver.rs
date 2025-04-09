use hir::{
    hir_def::{
        scope_graph::ScopeId, Enum, GenericParamOwner, IdentId, ItemKind, Partial, PathId, TypeId,
        VariantDef, VariantKind,
    },
    span::DynLazySpan,
};
use smallvec::SmallVec;

use super::{
    is_scope_visible_from,
    method_selection::{select_method_candidate, Candidate, MethodSelectionError},
    name_resolver::{NameRes, NameResBucket, NameResolutionError},
    resolve_query,
    visibility_checker::is_ty_visible_from,
    EarlyNameQueryId, NameDomain,
};
use crate::{
    name_resolution::{NameResKind, QueryDirective},
    ty::{
        adt_def::{lower_adt, AdtRef},
        binder::Binder,
        canonical::{Canonical, Canonicalized},
        func_def::{lower_func, FuncDef, HirFuncDefKind},
        trait_def::{impls_for_ty, TraitDef},
        trait_lower::lower_trait,
        trait_resolution::PredicateListId,
        ty_def::{InvalidCause, TyId},
        ty_lower::{
            collect_generic_params, lower_generic_arg_list, lower_hir_ty, lower_type_alias,
        },
    },
    HirAnalysisDb,
};

pub type PathResolutionResult<'db, T> = Result<T, PathResError<'db>>;

#[derive(Debug)]
pub struct PathResError<'db> {
    pub kind: PathResErrorKind<'db>,
    pub failed_at: PathId<'db>,
}

#[derive(Debug)]
pub enum PathResErrorKind<'db> {
    /// The name is not found.
    NotFound(NameResBucket<'db>),

    /// The name is invalid in parsing. Basically, no need to report it because
    /// the error is already emitted from parsing phase.
    ParseError,

    /// The name is found, but it's ambiguous.
    Ambiguous(Vec<NameRes<'db>>),

    /// The name is found, but it can't be used in the middle of a use path.
    InvalidPathSegment(PathRes<'db>),

    /// The definition conflicts with other definitions.
    Conflict(Vec<DynLazySpan<'db>>),

    TooManyGenericArgs {
        expected: usize,
        given: usize,
    },

    MethodSelection(MethodSelectionError<'db>),

    TraitMethodNotFound(TraitDef<'db>),
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

    pub fn method_selection(err: MethodSelectionError<'db>, path: PathId<'db>) -> Self {
        Self::new(PathResErrorKind::MethodSelection(err), path)
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
            PathResErrorKind::MethodSelection(..) => todo!(),
        }
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

#[derive(Debug, Clone)]
pub enum PathRes<'db> {
    Ty(TyId<'db>),
    Func(TyId<'db>),
    FuncParam(ItemKind<'db>, usize),
    Trait(TraitDef<'db>),
    EnumVariant(ResolvedVariant<'db>),
    Const(TyId<'db>),
    Mod(ScopeId<'db>),
    Method(TyId<'db>, Candidate<'db>),
}

impl<'db> PathRes<'db> {
    pub fn map_over_ty<F>(self, mut f: F) -> Self
    where
        F: FnMut(TyId<'db>) -> TyId<'db>,
    {
        match self {
            PathRes::Ty(ty) => PathRes::Ty(f(ty)),
            PathRes::Func(ty) => PathRes::Func(f(ty)),
            PathRes::Const(ty) => PathRes::Const(f(ty)),
            PathRes::EnumVariant(v) => {
                PathRes::EnumVariant(ResolvedVariant::new(f(v.ty), v.idx, v.path))
            }
            // xxx map over candidate ty?
            PathRes::Method(ty, candidate) => PathRes::Method(f(ty), candidate),
            r @ (PathRes::Trait(_) | PathRes::Mod(_) | PathRes::FuncParam(..)) => r,
        }
    }

    pub fn as_scope(&self, db: &'db dyn HirAnalysisDb) -> Option<ScopeId<'db>> {
        match self {
            PathRes::Ty(ty) | PathRes::Func(ty) | PathRes::Const(ty) => ty.as_scope(db),
            PathRes::Trait(trait_) => Some(trait_.trait_(db).scope()),
            PathRes::EnumVariant(variant) => Some(variant.enum_(db).scope()),
            PathRes::FuncParam(item, idx) => Some(ScopeId::FuncParam(*item, *idx)),
            PathRes::Mod(scope) => Some(*scope),
            PathRes::Method(ty, _) => ty.as_scope(db),
        }
    }

    pub fn is_visible_from(&self, db: &'db dyn HirAnalysisDb, from_scope: ScopeId<'db>) -> bool {
        match self {
            PathRes::Ty(ty) | PathRes::Func(ty) | PathRes::Const(ty) | PathRes::Method(ty, _) => {
                is_ty_visible_from(db, *ty, from_scope)
            }
            r => is_scope_visible_from(db, r.as_scope(db).unwrap(), from_scope),
        }
    }

    pub fn name_span(&self, db: &'db dyn HirAnalysisDb) -> Option<DynLazySpan<'db>> {
        self.as_scope(db)?.name_span(db)
    }

    pub fn pretty_path(&self, db: &'db dyn HirAnalysisDb) -> Option<String> {
        let hir_db = db;

        let ty_path = |ty: TyId<'db>| {
            if let Some(scope) = ty.as_scope(db) {
                scope.pretty_path(hir_db)
            } else {
                Some(ty.pretty_print(db).to_string())
            }
        };

        match self {
            PathRes::Ty(ty) | PathRes::Func(ty) | PathRes::Const(ty) => ty_path(*ty),

            PathRes::EnumVariant(v) => {
                let variant_idx = v.idx;
                Some(format!(
                    "{}::{}",
                    ty_path(v.ty).unwrap_or_else(|| "<missing>".into()),
                    v.enum_(db).variants(db).data(db)[variant_idx]
                        .name
                        .to_opt()?
                        .data(db)
                ))
            }
            r @ (PathRes::Trait(..) | PathRes::Mod(..) | PathRes::FuncParam(..)) => {
                r.as_scope(db).unwrap().pretty_path(db)
            }

            PathRes::Method(ty, cand) => Some(format!(
                "{}::{}",
                ty_path(*ty).unwrap_or_else(|| "<missing>".into()),
                cand.name(db).data(db)
            )),
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            PathRes::Ty(_) => "type",
            PathRes::Func(_) => "function",
            PathRes::FuncParam(..) => "function parameter",
            PathRes::Trait(_) => "trait",
            PathRes::EnumVariant(_) => "enum variant",
            PathRes::Const(_) => "constant",
            PathRes::Mod(_) => "module",
            PathRes::Method(..) => "method",
        }
    }
}

#[derive(Clone, Debug)]
pub struct ResolvedVariant<'db> {
    pub ty: TyId<'db>,
    pub idx: usize,
    pub path: PathId<'db>,
}

impl<'db> ResolvedVariant<'db> {
    pub fn new(ty: TyId<'db>, idx: usize, path: PathId<'db>) -> Self {
        Self { ty, idx, path }
    }

    pub fn variant_def(&self, db: &'db dyn HirAnalysisDb) -> &'db VariantDef<'db> {
        &self.enum_(db).variants(db).data(db)[self.idx]
    }

    pub fn variant_kind(&self, db: &'db dyn HirAnalysisDb) -> VariantKind<'db> {
        self.variant_def(db).kind
    }

    pub fn enum_(&self, db: &'db dyn HirAnalysisDb) -> Enum<'db> {
        self.ty.as_enum(db).unwrap()
    }

    pub fn iter_field_types(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = Binder<TyId<'db>>> {
        self.ty
            .adt_def(db)
            .unwrap()
            .fields(db)
            .get(self.idx)
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
        if !matches!(self.variant_kind(db), VariantKind::Tuple(_)) {
            return None;
        }

        let adt = self.ty.adt_def(db).unwrap();
        let arg_tys = adt
            .fields(db)
            .get(self.idx)
            .unwrap()
            .iter_types(db)
            .collect();

        let adt_param_set = adt.param_set(db);

        let mut ret_ty = TyId::adt(db, adt);
        ret_ty = TyId::foldl(db, ret_ty, adt.param_set(db).params(db));

        Some(FuncDef::new(
            db,
            HirFuncDefKind::VariantCtor(self.enum_(db), self.idx),
            *self.variant_def(db).name.unwrap(),
            *adt_param_set,
            arg_tys,
            Binder::bind(ret_ty),
        ))
    }
}

pub fn resolve_path<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: Option<PredicateListId<'db>>,
    resolve_tail_as_value: bool,
) -> PathResolutionResult<'db, PathRes<'db>> {
    resolve_path_impl(
        db,
        path,
        scope,
        assumptions,
        resolve_tail_as_value,
        true,
        &mut |_, _| {},
    )
}

pub fn resolve_path_with_observer<'db, F>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: Option<PredicateListId<'db>>,
    resolve_tail_as_value: bool,
    observer: &mut F,
) -> PathResolutionResult<'db, PathRes<'db>>
where
    F: FnMut(PathId<'db>, &PathRes<'db>),
{
    resolve_path_impl(
        db,
        path,
        scope,
        assumptions,
        resolve_tail_as_value,
        true,
        observer,
    )
}

fn resolve_path_impl<'db, F>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: Option<PredicateListId<'db>>,
    resolve_tail_as_value: bool,
    is_tail: bool,
    observer: &mut F,
) -> PathResolutionResult<'db, PathRes<'db>>
where
    F: FnMut(PathId<'db>, &PathRes<'db>),
{
    let hir_db = db;

    let parent_res = path
        .parent(hir_db)
        .map(|path| {
            resolve_path_impl(
                db,
                path,
                scope,
                assumptions,
                resolve_tail_as_value,
                false,
                observer,
            )
        })
        .transpose()?;

    let Some(ident) = path.ident(hir_db).to_opt() else {
        return Err(PathResError::parse_err(path));
    };

    let parent_scope = parent_res
        .as_ref()
        .and_then(|r| r.as_scope(db))
        .unwrap_or(scope);

    match parent_res {
        Some(PathRes::Ty(ty)) => {
            // Try to resolve as an enum variant
            if let Some(enum_) = ty.as_enum(db) {
                // We need to use the concrete enum scope instead of
                // parent_scope to resolve the variants in all cases,
                // eg when parent is `Self`. I'm not really sure why this is.
                let query = make_query(db, path, enum_.scope());
                let bucket = resolve_query(db, query);

                if let Ok(res) = bucket.pick(NameDomain::VALUE) {
                    if let Some((_, idx)) = res.enum_variant() {
                        let reso = PathRes::EnumVariant(ResolvedVariant::new(ty, idx, path));
                        observer(path, &reso);
                        return Ok(reso);
                    }
                }
            }

            if is_tail && resolve_tail_as_value {
                let receiver_ty = Canonicalized::new(db, ty);
                match select_method_candidate(
                    db,
                    receiver_ty.value,
                    ident,
                    parent_scope,
                    assumptions.unwrap_or_else(|| PredicateListId::empty_list(db)),
                ) {
                    Ok(cand) => {
                        let r = PathRes::Method(ty, cand);
                        observer(path, &r);
                        return Ok(r);
                    }
                    Err(MethodSelectionError::NotFound) => {}
                    Err(err) => {
                        return Err(PathResError::method_selection(err, path));
                    }
                }
            }

            let assoc_tys = find_associated_type(db, scope, Canonical::new(db, ty), ident);
            let Some(assoc) = assoc_tys.first() else {
                return Err(PathResError::not_found(path, NameResBucket::default()));
            };
            // xxx ambiguous associated type error
            let r = PathRes::Ty(*assoc);
            observer(path, &r);
            return Ok(r);
        }

        Some(PathRes::Func(_) | PathRes::EnumVariant(..)) => {
            return Err(PathResError::new(
                PathResErrorKind::InvalidPathSegment(parent_res.unwrap()),
                path,
            ));
        }
        Some(PathRes::FuncParam(..) | PathRes::Method(..)) => unreachable!(),
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
    let r = resolve_name_res(db, &res, parent_res, path, scope)?;
    observer(path, &r);
    Ok(r)
}

fn find_associated_type<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: Canonical<TyId<'db>>,
    name: IdentId<'db>,
) -> SmallVec<TyId<'db>, 4> {
    let ingot = scope.ingot(db);
    impls_for_ty(db, ingot, ty)
        .iter()
        .filter_map(|i| {
            let t = i.skip_binder().types(db).get(&name);
            t.copied()
        })
        .collect()
}

pub fn resolve_name_res<'db>(
    db: &'db dyn HirAnalysisDb,
    nameres: &NameRes<'db>,
    parent_ty: Option<PathRes<'db>>,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> PathResolutionResult<'db, PathRes<'db>> {
    let hir_db = db;

    let args = &lower_generic_arg_list(db, path.generic_args(hir_db), scope);
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
                    let ty = if let Some(ty) = const_.ty(hir_db).to_opt() {
                        lower_hir_ty(db, ty, scope)
                    } else {
                        TyId::invalid(db, InvalidCause::Other)
                    };
                    PathRes::Const(ty)
                }

                ItemKind::TypeAlias(type_alias) => {
                    let alias = lower_type_alias(db, type_alias);
                    if args.len() < alias.params(db).len() {
                        PathRes::Ty(TyId::invalid(
                            db,
                            InvalidCause::UnboundTypeAliasParam {
                                alias: type_alias,
                                n_given_args: args.len(),
                            },
                        ))
                    } else {
                        PathRes::Ty(alias.alias_to.instantiate(db, args))
                    }
                }

                ItemKind::Impl(impl_) => {
                    PathRes::Ty(impl_typeid_to_ty(db, path, impl_.ty(hir_db), scope, args)?)
                }
                ItemKind::ImplTrait(impl_) => {
                    PathRes::Ty(impl_typeid_to_ty(db, path, impl_.ty(hir_db), scope, args)?)
                }

                ItemKind::Trait(t) => {
                    if path.is_self_ty(hir_db) {
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
                let ty = param_set.param_by_original_idx(db, idx).unwrap();
                let ty = TyId::foldl(db, ty, args);
                PathRes::Ty(ty)
            }

            ScopeId::Variant(enum_, idx) => {
                let enum_ty = if let Some(PathRes::Ty(ty)) = parent_ty {
                    ty
                } else {
                    // The variant was imported via `use`.
                    debug_assert!(path.parent(hir_db).is_none());
                    let enum_: Enum = enum_.try_into().unwrap();
                    ty_from_adtref(db, enum_.into(), &[])?
                };
                // TODO report error if args isn't empty
                PathRes::EnumVariant(ResolvedVariant::new(enum_ty, idx, path))
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
