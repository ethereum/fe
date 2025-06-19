use common::indexmap::{IndexMap, IndexSet};
use either::Either;
use hir::{
    hir_def::{
        scope_graph::ScopeId, Enum, EnumVariant, GenericParamOwner, IdentId, ItemKind, Partial,
        PathId, TypeId, VariantKind,
    },
    span::DynLazySpan,
};
use if_chain::if_chain;
use smallvec::SmallVec;
use thin_vec::ThinVec;

use super::{
    diagnostics::NameResDiag,
    is_scope_visible_from,
    method_selection::{select_method_candidate, Candidate, MethodSelectionError},
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
        canonical::{Canonical, Canonicalized},
        fold::TyFoldable,
        func_def::{lower_func, FuncDef, HirFuncDefKind},
        trait_def::{impls_for_ty, TraitInstId},
        trait_lower::lower_trait,
        trait_resolution::PredicateListId,
        ty_def::{AssocTy, InvalidCause, TyData, TyId},
        ty_lower::{
            collect_generic_params, lower_generic_arg_list, lower_hir_ty, lower_type_alias, TyAlias,
        },
        unify::UnificationTable,
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
    NotFound {
        parent: Option<PathRes<'db>>,
        bucket: NameResBucket<'db>,
    },

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
        ty: TyId<'db>,
        expected: u16,
        given: u16,
    },

    MethodSelection(MethodSelectionError<'db>),
}

impl<'db> PathResError<'db> {
    pub fn new(kind: PathResErrorKind<'db>, failed_at: PathId<'db>) -> Self {
        Self { kind, failed_at }
    }
    pub fn parse_err(path: PathId<'db>) -> Self {
        Self::new(PathResErrorKind::ParseError, path)
    }

    pub fn method_selection(err: MethodSelectionError<'db>, path: PathId<'db>) -> Self {
        Self::new(PathResErrorKind::MethodSelection(err), path)
    }

    pub fn from_name_res_error(err: NameResolutionError<'db>, path: PathId<'db>) -> Self {
        let kind = match err {
            NameResolutionError::NotFound => PathResErrorKind::NotFound {
                parent: None,
                bucket: NameResBucket::default(),
            },
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
            PathResErrorKind::NotFound { .. } => "Not found".to_string(),
            PathResErrorKind::ParseError => "Parse error".to_string(),
            PathResErrorKind::Ambiguous(v) => format!("Ambiguous; {} options.", v.len()),
            PathResErrorKind::InvalidPathSegment(_) => "Invalid path segment".to_string(),
            PathResErrorKind::Conflict(..) => "Conflicting definitions".to_string(),
            PathResErrorKind::TooManyGenericArgs {
                ty: _,
                expected,
                given,
            } => {
                format!("Incorrect number of generic args; expected {expected}, given {given}.")
            }
            PathResErrorKind::MethodSelection(..) => todo!(),
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
            PathResErrorKind::NotFound { parent, bucket } => {
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
                            ExpectedPathKind::Function => func_not_found_err(span, ident, parent),
                            _ => NameResDiag::NotFound(span, ident),
                        }
                    }
                } else if expected == ExpectedPathKind::Function {
                    func_not_found_err(span, ident, parent)
                } else {
                    NameResDiag::NotFound(span, ident)
                }
            }

            PathResErrorKind::Ambiguous(cands) => NameResDiag::ambiguous(db, span, ident, cands),

            PathResErrorKind::TooManyGenericArgs {
                ty,
                expected,
                given,
            } => NameResDiag::TooManyGenericArgs {
                span,
                ty,
                expected,
                given,
            },

            PathResErrorKind::InvalidPathSegment(res) => {
                NameResDiag::InvalidPathSegment(span, ident, res.name_span(db))
            }

            PathResErrorKind::Conflict(spans) => NameResDiag::Conflict(ident, spans),
            PathResErrorKind::MethodSelection(_) => todo!(),
        };
        Some(diag)
    }
}

fn func_not_found_err<'db>(
    span: DynLazySpan<'db>,
    ident: IdentId<'db>,
    parent: Option<PathRes<'db>>,
) -> NameResDiag<'db> {
    match parent {
        Some(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => NameResDiag::MethodNotFound {
            primary: span,
            method_name: ident,
            receiver: Either::Left(ty),
        },
        Some(PathRes::Trait(t)) => NameResDiag::MethodNotFound {
            primary: span,
            method_name: ident,
            receiver: Either::Right(t),
        },
        _ => NameResDiag::NotFound(span, ident),
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
    Trait(TraitInstId<'db>),
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
            PathRes::TyAlias(alias, ty) => PathRes::TyAlias(alias, f(ty)),
            PathRes::Func(ty) => PathRes::Func(f(ty)),
            PathRes::Const(ty) => PathRes::Const(f(ty)),
            PathRes::EnumVariant(v) => PathRes::EnumVariant(ResolvedVariant { ty: f(v.ty), ..v }),
            // xxx map over candidate ty?
            PathRes::Method(ty, candidate) => PathRes::Method(f(ty), candidate),
            r @ (PathRes::Trait(_) | PathRes::Mod(_) | PathRes::FuncParam(..)) => r,
        }
    }

    pub fn as_scope(&self, db: &'db dyn HirAnalysisDb) -> Option<ScopeId<'db>> {
        match self {
            PathRes::Ty(ty) | PathRes::Func(ty) | PathRes::Const(ty) => ty.as_scope(db),
            PathRes::TyAlias(alias, _) => Some(alias.alias.scope()),
            PathRes::Trait(trait_) => Some(trait_.def(db).trait_(db).scope()),
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
            PathRes::TyAlias(..) => "type alias",
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
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
    assumptions: PredicateListId<'db>,
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
    assumptions: PredicateListId<'db>,
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
    assumptions: PredicateListId<'db>,
    resolve_tail_as_value: bool,
    is_tail: bool,
    observer: &mut F,
) -> PathResolutionResult<'db, PathRes<'db>>
where
    F: FnMut(PathId<'db>, &PathRes<'db>),
{
    let parent_res = path
        .parent(db)
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

    let Some(ident) = path.ident(db).to_opt() else {
        return Err(PathResError::parse_err(path));
    };

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

            if is_tail && resolve_tail_as_value {
                let receiver_ty = Canonicalized::new(db, ty);
                match select_method_candidate(
                    db,
                    receiver_ty.value,
                    ident,
                    parent_scope,
                    assumptions,
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

            let assoc_tys =
                find_associated_type(db, scope, Canonical::new(db, ty), ident, assumptions);
            let Some(assoc) = assoc_tys.first() else {
                return Err(PathResError::new(
                    PathResErrorKind::NotFound {
                        parent: parent_res,
                        bucket: NameResBucket::default(),
                    },
                    path,
                ));
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

    let parent_ty = parent_res.as_ref().and_then(|res| match res {
        PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => Some(*ty),
        _ => None,
    });

    let res = if_chain! {
        if is_tail && resolve_tail_as_value;
        if let Ok(res) = bucket.pick(NameDomain::VALUE);
        then {
            res.clone()
        } else {
            pick_type_domain_from_bucket(parent_res, bucket, path)?
        }
    };

    let r = resolve_name_res(db, &res, parent_ty, path, scope, assumptions)?;
    observer(path, &r);
    Ok(r)
}

fn find_associated_type<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: Canonical<TyId<'db>>,
    name: IdentId<'db>,
    assumptions: PredicateListId<'db>,
) -> SmallVec<TyId<'db>, 4> {
    let mut candidates: IndexSet<TyId<'db>> = IndexSet::new();
    let ingot = scope.ingot(db);

    // Case 1: ty is a concrete type or can be resolved to concrete impls.
    // This part attempts to find associated types from concrete `impl` blocks.
    for implementor in impls_for_ty(db, ingot, ty).iter() {
        // Create a fresh unification table for this specific implementor candidate.
        let mut table = UnificationTable::new(db);

        // Instantiate the LHS type `ty` with fresh unification variables if it's generic.
        let lhs_ty_instance = ty.extract_identity(&mut table);

        // Instantiate the implementor with fresh unification variables for its generic parameters.
        let implementor_instance = table.instantiate_with_fresh_vars(*implementor);

        // Try to unify the LHS type with the self type of the implementor.
        if table
            .unify(lhs_ty_instance, implementor_instance.self_ty(db))
            .is_ok()
        {
            // If unification succeeds, the implementor's generic parameters are now (potentially)
            // bound to parts of `lhs_ty_instance`.
            if let Some(assoc_ty_template) = implementor_instance.types(db).get(&name) {
                // The associated type from the `impl` block might itself contain generic parameters
                // from the `impl` block. These need to be substituted based on the unification outcome.
                candidates.insert(assoc_ty_template.fold_with(&mut table));
            }
        }
    }

    // Case 2: The LHS `ty` is a generic type parameter (e.g., `A` in `A::Encoder` where `A: Abi`).
    // We look for the associated type in its trait bounds.
    if let TyData::TyParam(_) = ty.value.data(db) {
        // Check if the canonical value is a TyParam

        for &predicate_trait_inst in assumptions.list(db).iter() {
            // `predicate_trait_inst` is a specific trait bound, e.g., `A: Abi` or `S<A>: SomeTrait`.
            // It has `def` (the TraitDef) and `args` (the actual types for Self, generics, and resolved associated types).

            let mut table = UnificationTable::new(db);
            // Instantiate the LHS `ty` (e.g., A) with fresh variables for unification.
            let lhs_ty_instance = ty.extract_identity(&mut table);

            // Instantiate the `self_ty` from the predicate (e.g., the `A` in `A: Abi`) also with fresh variables.
            let predicate_self_ty_instance =
                table.instantiate_with_fresh_vars(Binder::bind(predicate_trait_inst.self_ty(db)));

            // Check if the current trait bound applies to our LHS type `ty`.
            if table
                .unify(lhs_ty_instance, predicate_self_ty_instance)
                .is_ok()
            {
                let trait_def_of_bound = predicate_trait_inst.def(db);

                // Look for the associated type by name in the trait bindings
                if let Some(&actual_assoc_ty_in_bound) =
                    predicate_trait_inst.assoc_type_bindings(db).get(&name)
                {
                    // This `actual_assoc_ty_in_bound` needs to be processed by the unification table
                    // to substitute any type variables based on the `lhs_ty_instance` unification.
                    candidates.insert(actual_assoc_ty_in_bound.fold_with(&mut table));
                } else {
                    // If no explicit binding is found, check if the trait declares this associated type
                    for trait_type in trait_def_of_bound.trait_(db).types(db).iter() {
                        if trait_type.name.to_opt() == Some(name) {
                            // Create an associated type for this trait bound
                            let assoc_ty_id = TyId::new(
                                db,
                                TyData::AssocTy(AssocTy {
                                    trait_: predicate_trait_inst,
                                    name,
                                }),
                            );
                            candidates.insert(assoc_ty_id.fold_with(&mut table));
                            break;
                        }
                    }
                }
            }
        }
    }

    candidates.into_iter().collect()
}

pub fn resolve_name_res<'db>(
    db: &'db dyn HirAnalysisDb,
    nameres: &NameRes<'db>,
    parent_ty: Option<TyId<'db>>,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
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
                    PathRes::Ty(ty_from_adtref(db, path, adt_ref, args)?)
                }

                ItemKind::TopMod(_) | ItemKind::Mod(_) => PathRes::Mod(scope_id),

                ItemKind::Func(func) => {
                    let func_def = lower_func(db, func, assumptions).unwrap();
                    let ty = TyId::func(db, func_def);
                    PathRes::Func(TyId::foldl(db, ty, args))
                }
                ItemKind::Const(const_) => {
                    // TODO err if any args
                    let ty = if let Some(ty) = const_.ty(db).to_opt() {
                        lower_hir_ty(db, ty, scope, assumptions)
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

                ItemKind::Impl(impl_) => PathRes::Ty(impl_typeid_to_ty(
                    db,
                    path,
                    impl_.ty(db),
                    scope,
                    args,
                    assumptions,
                )?),
                ItemKind::ImplTrait(impl_) => PathRes::Ty(impl_typeid_to_ty(
                    db,
                    path,
                    impl_.ty(db),
                    scope,
                    args,
                    assumptions,
                )?),

                ItemKind::Trait(t) => {
                    if path.is_self_ty(db) {
                        let params = collect_generic_params(db, t.into());
                        let ty = params.trait_self(db).unwrap();
                        let ty = TyId::foldl(db, ty, args);
                        PathRes::Ty(ty)
                    } else {
                        let trait_def = lower_trait(db, t);
                        let trait_inst = TraitInstId::new(db, trait_def, args, IndexMap::new());
                        PathRes::Trait(trait_inst)
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

            ScopeId::TraitType(t, idx) => {
                let _ty = &t.types(db)[idx as usize];
                todo!() // xxx
            }

            ScopeId::Variant(var) => {
                let enum_ty = if let Some(ty) = parent_ty {
                    ty
                } else {
                    // The variant was imported via `use`.
                    debug_assert!(path.parent(db).is_none());
                    ty_from_adtref(db, path, var.enum_.into(), &[])?
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
    assumptions: PredicateListId<'db>,
) -> PathResolutionResult<'db, TyId<'db>> {
    if let Some(hir_ty) = hir_ty.to_opt() {
        let ty = lower_hir_ty(db, hir_ty, scope, assumptions); // root scope!
        Ok(TyId::foldl(db, ty, args))
    } else {
        Err(PathResError::parse_err(path))
    }
}

fn ty_from_adtref<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    adt_ref: AdtRef<'db>,
    args: &[TyId<'db>],
) -> PathResolutionResult<'db, TyId<'db>> {
    let adt = lower_adt(db, adt_ref);
    let ty = TyId::adt(db, adt);
    let applied = TyId::foldl(db, ty, args);
    if let TyData::Invalid(InvalidCause::TooManyGenericArgs { expected, given }) = applied.data(db)
    {
        Err(PathResError::new(
            PathResErrorKind::TooManyGenericArgs {
                ty,
                expected: *expected as u16,
                given: *given as u16,
            },
            path,
        ))
    } else {
        Ok(applied)
    }
}

fn pick_type_domain_from_bucket<'db>(
    parent: Option<PathRes<'db>>,
    bucket: &NameResBucket<'db>,
    path: PathId<'db>,
) -> PathResolutionResult<'db, NameRes<'db>> {
    bucket
        .pick(NameDomain::TYPE)
        .clone()
        .map_err(|err| match err {
            NameResolutionError::NotFound => PathResError::new(
                PathResErrorKind::NotFound {
                    parent: parent.clone(),
                    bucket: bucket.clone(),
                },
                path,
            ),
            err => PathResError::from_name_res_error(err, path),
        })
}
