//! This module contains the type definitions for the Fe type system.

use std::{collections::BTreeSet, fmt};

use hir::{
    hir_def::{
        kw,
        prim_ty::{IntTy as HirIntTy, PrimTy as HirPrimTy, UintTy as HirUintTy},
        scope_graph::ScopeId,
        Body, Contract, Enum, Func, FuncParam as HirFuncParam, IdentId, IngotId, ItemKind, Partial,
        Struct, TypeAlias as HirTypeAlias, TypeId as HirTyId, VariantKind,
    },
    span::DynLazySpan,
};
use rustc_hash::FxHashMap;

use super::{
    constraint::{
        collect_adt_constraints, collect_func_def_constraints, AssumptionListId, ConstraintListId,
    },
    dependent_ty::{DependentTyData, DependentTyId},
    diagnostics::{TraitConstraintDiag, TyDiagCollection, TyLowerDiag},
    ty_lower::{lower_hir_ty, GenericParamOwnerId},
    unify::{InferenceKey, UnificationTable},
    visitor::TyVisitor,
};
use crate::{
    ty::constraint_solver::{check_ty_app_sat, GoalSatisfiability},
    HirAnalysisDb,
};

#[salsa::interned]
pub struct TyId {
    #[return_ref]
    pub data: TyData,
}

impl TyId {
    /// Returns the kind of the type.
    pub fn kind(self, db: &dyn HirAnalysisDb) -> &Kind {
        ty_kind(db, self)
    }

    /// Returns `true` if the type is invalid, see [`contains_invalid`] if you
    /// want to check if the type contains any invalid types as a part of the
    /// type.
    pub fn is_invalid(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.data(db), TyData::Invalid(_))
    }

    /// Returns `true` if the type is an integral type(like `u32`, `i32` etc.)
    pub fn is_integral(self, db: &dyn HirAnalysisDb) -> bool {
        match self.data(db) {
            TyData::TyBase(ty_base) => ty_base.is_integral(),
            _ => false,
        }
    }

    /// Returns `IngotId` that declares the type.
    pub fn ingot(self, db: &dyn HirAnalysisDb) -> Option<IngotId> {
        match self.data(db) {
            TyData::TyBase(TyBase::Adt(adt)) => adt.ingot(db).into(),
            TyData::TyBase(TyBase::Func(def)) => def.ingot(db).into(),
            TyData::TyApp(lhs, _) => lhs.ingot(db),
            _ => None,
        }
    }

    pub fn invalid_cause(self, db: &dyn HirAnalysisDb) -> Option<InvalidCause> {
        match self.data(db) {
            TyData::Invalid(cause) => Some(cause.clone()),
            _ => None,
        }
    }

    /// Returns `true` if the type contains invalid types as an argument.
    pub fn contains_invalid(self, db: &dyn HirAnalysisDb) -> bool {
        match self.data(db) {
            TyData::Invalid(_) => true,
            TyData::TyApp(lhs, rhs) => lhs.contains_invalid(db) || rhs.contains_invalid(db),
            TyData::DependentTy(dependent_ty) => dependent_ty.ty(db).contains_invalid(db),
            _ => false,
        }
    }

    /// Returns `true` if the type has a `*` kind.
    pub fn has_star_kind(self, db: &dyn HirAnalysisDb) -> bool {
        !matches!(self.kind(db), Kind::Abs(_, _))
    }

    pub fn pretty_print(self, db: &dyn HirAnalysisDb) -> &str {
        pretty_print_ty(db, self)
    }

    /// Decompose type application into the base type and type arguments, this
    /// doesn't perform deconstruction recursively. e.g.,
    /// `App(App(T, U), App(V, W))` -> `(T, [U, App(V, W)])`
    pub(super) fn decompose_ty_app(self, db: &dyn HirAnalysisDb) -> (TyId, Vec<TyId>) {
        decompose_ty_app(db, self)
    }

    pub(super) fn base_ty(self, db: &dyn HirAnalysisDb) -> Option<TyBase> {
        match self.decompose_ty_app(db).0.data(db) {
            TyData::TyBase(concrete) => Some(*concrete),
            _ => None,
        }
    }

    pub(super) fn ptr(db: &dyn HirAnalysisDb) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Ptr)))
    }

    pub(super) fn tuple(db: &dyn HirAnalysisDb, n: usize) -> Self {
        Self::new(db, TyData::TyBase(TyBase::tuple(n)))
    }

    pub(super) fn array(db: &dyn HirAnalysisDb) -> Self {
        let base = TyBase::Prim(PrimTy::Array);
        Self::new(db, TyData::TyBase(base))
    }

    pub(super) fn unit(db: &dyn HirAnalysisDb) -> Self {
        Self::tuple(db, 0)
    }

    pub(super) fn dependent_ty(db: &dyn HirAnalysisDb, dependent_ty: DependentTyId) -> Self {
        Self::new(db, TyData::DependentTy(dependent_ty))
    }

    pub(super) fn adt(db: &dyn HirAnalysisDb, adt: AdtDef) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Adt(adt)))
    }

    pub(super) fn func(db: &dyn HirAnalysisDb, func: FuncDef) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Func(func)))
    }

    pub(super) fn is_trait_self(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.data(db), TyData::TyParam(ty_param) if ty_param.is_trait_self())
    }

    pub(super) fn is_dependent_ty(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.data(db), TyData::DependentTy(_))
    }

    pub(super) fn contains_ty_param(self, db: &dyn HirAnalysisDb) -> bool {
        !self.type_params(db).is_empty()
    }

    pub(super) fn contains_trait_self(self, db: &dyn HirAnalysisDb) -> bool {
        match self.data(db) {
            TyData::TyParam(ty_param) => ty_param.is_trait_self(),
            TyData::TyApp(lhs, rhs) => lhs.contains_trait_self(db) || rhs.contains_trait_self(db),
            _ => false,
        }
    }

    pub(super) fn generalize(self, db: &dyn HirAnalysisDb, table: &mut UnificationTable) -> Self {
        let params = self.type_params(db);
        let mut subst = FxHashMap::default();
        for &param in params.iter() {
            let new_var = table.new_var_from_param(db, param);
            subst.insert(param, new_var);
        }

        self.apply_subst(db, &mut subst)
    }

    /// Emit diagnostics for the type if the type contains invalid types.
    pub(super) fn emit_diag(
        self,
        db: &dyn HirAnalysisDb,
        span: DynLazySpan,
    ) -> Option<TyDiagCollection> {
        struct EmitDiagVisitor {
            diag: Option<TyDiagCollection>,
            span: DynLazySpan,
        }

        impl TyVisitor for EmitDiagVisitor {
            fn visit_invalid(&mut self, db: &dyn HirAnalysisDb, cause: &InvalidCause) {
                let span = self.span.clone();
                let diag = match cause {
                    InvalidCause::NotFullyApplied => TyLowerDiag::non_concrete_ty(span).into(),

                    InvalidCause::KindMismatch { expected, given } => {
                        TyLowerDiag::invalid_type_arg_kind(db, span, expected.clone(), *given)
                            .into()
                    }

                    InvalidCause::InvalidConstParamTy { ty } => {
                        TyLowerDiag::invalid_const_param_ty(db, span, *ty).into()
                    }

                    InvalidCause::RecursiveConstParamTy => {
                        TyLowerDiag::RecursiveConstParamTy(span).into()
                    }

                    InvalidCause::DependentTyMismatch { expected, given } => {
                        TyLowerDiag::dependent_ty_mismatch(db, span, *expected, *given).into()
                    }

                    InvalidCause::DependentTyExpected { expected } => {
                        TyLowerDiag::dependent_ty_expected(db, span, *expected).into()
                    }

                    InvalidCause::NormalTypeExpected { given } => {
                        TyLowerDiag::normal_type_expected(db, span, *given).into()
                    }

                    InvalidCause::UnboundTypeAliasParam {
                        alias,
                        n_given_args: n_given_arg,
                    } => TyLowerDiag::unbound_type_alias_param(span, *alias, *n_given_arg).into(),

                    InvalidCause::AssocTy => TyLowerDiag::assoc_ty(span).into(),

                    InvalidCause::InvalidDependentTyExpr { body } => {
                        TyLowerDiag::InvalidDependentTyExpr(body.lazy_span().into()).into()
                    }

                    InvalidCause::Other => return,
                };

                self.diag.get_or_insert(diag);
            }
        }

        let mut visitor = EmitDiagVisitor { diag: None, span };
        visitor.visit_ty(db, self);
        visitor.diag
    }

    pub(super) fn emit_sat_diag(
        self,
        db: &dyn HirAnalysisDb,
        assumptions: AssumptionListId,
        span: DynLazySpan,
    ) -> Option<TyDiagCollection> {
        match check_ty_app_sat(db, self, assumptions) {
            GoalSatisfiability::Satisfied => None,
            GoalSatisfiability::NotSatisfied(goal) => {
                Some(TraitConstraintDiag::trait_bound_not_satisfied(db, span, goal).into())
            }
            GoalSatisfiability::InfiniteRecursion(goal) => {
                Some(TraitConstraintDiag::infinite_bound_recursion(db, span, goal).into())
            }
        }
    }

    pub(super) fn is_ty_var(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.data(db), TyData::TyVar(_))
    }

    /// Returns all inference keys in the type.
    pub(super) fn free_inference_keys(self, db: &dyn HirAnalysisDb) -> &BTreeSet<InferenceKey> {
        free_inference_keys(db, self)
    }

    /// Returns all generics parameters in the type.
    pub(super) fn type_params(self, db: &dyn HirAnalysisDb) -> &BTreeSet<TyId> {
        collect_type_params(db, self)
    }

    pub(super) fn ty_var(
        db: &dyn HirAnalysisDb,
        universe: TyVarUniverse,
        kind: Kind,
        key: InferenceKey,
    ) -> Self {
        Self::new(
            db,
            TyData::TyVar(TyVar {
                universe,
                kind,
                key,
            }),
        )
    }

    pub(super) fn dependent_ty_var(db: &dyn HirAnalysisDb, ty: TyId, key: InferenceKey) -> Self {
        let ty_var = TyVar {
            universe: TyVarUniverse::General,
            kind: ty.kind(db).clone(),
            key,
        };

        let data = DependentTyData::TyVar(ty_var, ty);
        Self::new(db, TyData::DependentTy(DependentTyId::new(db, data)))
    }

    /// Perform type level application.
    pub(super) fn app(db: &dyn HirAnalysisDb, abs: Self, arg: Self) -> TyId {
        let k_abs = abs.kind(db);
        let k_arg = arg.kind(db);

        let arg = arg
            .evaluate_dep_ty(db, abs.expected_dependent_ty(db))
            .unwrap_or_else(|cause| Self::invalid(db, cause));

        let arg = match k_abs {
            Kind::Abs(k_expected, _) if k_expected.as_ref().does_match(k_arg) => arg,
            Kind::Abs(k_expected, _) => Self::invalid(
                db,
                InvalidCause::kind_mismatch(k_expected.as_ref().into(), arg),
            ),
            Kind::Star => Self::invalid(db, InvalidCause::kind_mismatch(None, arg)),
            Kind::Any => arg,
        };

        Self::new(db, TyData::TyApp(abs, arg))
    }

    /// Apply type arguments to the type.
    pub(crate) fn apply_subst<S>(self, db: &dyn HirAnalysisDb, subst: &mut S) -> TyId
    where
        S: Subst + ?Sized,
    {
        if let Some(to) = subst.get(self) {
            return to;
        }

        match self.data(db) {
            TyData::TyApp(lhs, rhs) => {
                let lhs = lhs.apply_subst(db, subst);
                let rhs = rhs.apply_subst(db, subst);
                TyId::app(db, lhs, rhs)
            }
            _ => self,
        }
    }

    /// Returns `true` if the type is a pointer or a pointer application.
    pub(super) fn is_ptr(self, db: &dyn HirAnalysisDb) -> bool {
        match self.data(db) {
            TyData::TyBase(TyBase::Prim(PrimTy::Ptr)) => true,
            TyData::TyApp(abs, _) => abs.is_ptr(db),
            _ => false,
        }
    }

    /// Returns `true` if the type is an indirect wrapper type like a pointer or
    /// reference(when we introduce it).
    pub(super) fn is_indirect(self, db: &dyn HirAnalysisDb) -> bool {
        // TODO: FiX here when reference type is introduced.
        self.is_ptr(db)
    }

    pub(super) fn invalid(db: &dyn HirAnalysisDb, cause: InvalidCause) -> Self {
        Self::new(db, TyData::Invalid(cause))
    }

    pub(super) fn from_hir_prim_ty(db: &dyn HirAnalysisDb, hir_prim: HirPrimTy) -> Self {
        match hir_prim {
            HirPrimTy::Bool => Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Bool))),

            HirPrimTy::Int(int_ty) => match int_ty {
                HirIntTy::I8 => Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::I8))),
                HirIntTy::I16 => Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::I16))),
                HirIntTy::I32 => Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::I32))),
                HirIntTy::I64 => Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::I64))),
                HirIntTy::I128 => Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::I128))),
                HirIntTy::I256 => Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::I256))),
            },

            HirPrimTy::Uint(uint_ty) => match uint_ty {
                HirUintTy::U8 => Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U8))),
                HirUintTy::U16 => Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U16))),
                HirUintTy::U32 => Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U32))),
                HirUintTy::U64 => Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U64))),
                HirUintTy::U128 => Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U128))),
                HirUintTy::U256 => Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256))),
            },
        }
    }

    pub(super) fn dependent_ty_param(self, db: &dyn HirAnalysisDb) -> Option<TyId> {
        if let TyData::DependentTy(dependent_ty) = self.data(db) {
            Some(dependent_ty.ty(db))
        } else {
            None
        }
    }

    pub(super) fn evaluate_dep_ty(
        self,
        db: &dyn HirAnalysisDb,
        expected_ty: Option<TyId>,
    ) -> Result<TyId, InvalidCause> {
        match (expected_ty, self.data(db)) {
            (Some(expected_dep_ty), TyData::DependentTy(dep_ty)) => {
                if expected_dep_ty.is_invalid(db) {
                    Err(InvalidCause::Other)
                } else {
                    let evaluated_dep_ty = dep_ty.evaluate(db, expected_dep_ty.into());
                    let evaluated_dep_ty_ty = evaluated_dep_ty.ty(db);
                    if let Some(cause) = evaluated_dep_ty_ty.invalid_cause(db) {
                        Err(cause)
                    } else {
                        Ok(TyId::dependent_ty(db, evaluated_dep_ty))
                    }
                }
            }

            (Some(expected_dependent_ty), _) => {
                if expected_dependent_ty.is_invalid(db) {
                    Err(InvalidCause::Other)
                } else {
                    Err(InvalidCause::DependentTyExpected {
                        expected: expected_dependent_ty,
                    })
                }
            }

            (None, TyData::DependentTy(dep_ty)) => {
                let evaluated_dep_ty = dep_ty.evaluate(db, None);
                Err(InvalidCause::NormalTypeExpected {
                    given: TyId::dependent_ty(db, evaluated_dep_ty),
                })
            }

            (None, _) => Ok(self),
        }
    }

    /// Returns the next expected type of a type argument.
    fn expected_dependent_ty(self, db: &dyn HirAnalysisDb) -> Option<TyId> {
        let (base, args) = self.decompose_ty_app(db);
        match base.data(db) {
            TyData::TyBase(ty_base) => ty_base.expected_dependent_ty(db, args.len()),
            _ => None,
        }
    }
}

/// Represents a ADT type definition.
#[salsa::tracked]
pub struct AdtDef {
    pub adt_ref: AdtRefId,

    /// Type parameters of the ADT.
    #[return_ref]
    pub params: Vec<TyId>,

    /// Fields of the ADT, if the ADT is an enum, this represents variants.
    #[return_ref]
    pub fields: Vec<AdtField>,
}

impl AdtDef {
    pub(crate) fn name(self, db: &dyn HirAnalysisDb) -> IdentId {
        self.adt_ref(db).name(db)
    }

    pub(crate) fn variant_ty_span(
        self,
        db: &dyn HirAnalysisDb,
        field_idx: usize,
        ty_idx: usize,
    ) -> DynLazySpan {
        match self.adt_ref(db).data(db) {
            AdtRef::Enum(e) => {
                let span = e.lazy_span().variants_moved().variant_moved(field_idx);
                match e.variants(db.as_hir_db()).data(db.as_hir_db())[field_idx].kind {
                    VariantKind::Tuple(_) => span.tuple_type_moved().elem_ty_moved(ty_idx).into(),
                    VariantKind::Record(_) => {
                        span.fields_moved().field_moved(ty_idx).ty_moved().into()
                    }
                    VariantKind::Unit => unreachable!(),
                }
            }

            AdtRef::Struct(s) => s
                .lazy_span()
                .fields_moved()
                .field_moved(field_idx)
                .ty_moved()
                .into(),

            AdtRef::Contract(c) => c
                .lazy_span()
                .fields_moved()
                .field_moved(field_idx)
                .ty_moved()
                .into(),
        }
    }

    pub(crate) fn ingot(self, db: &dyn HirAnalysisDb) -> IngotId {
        let hir_db = db.as_hir_db();
        match self.adt_ref(db).data(db) {
            AdtRef::Enum(e) => e.top_mod(hir_db).ingot(hir_db),
            AdtRef::Struct(s) => s.top_mod(hir_db).ingot(hir_db),
            AdtRef::Contract(c) => c.top_mod(hir_db).ingot(hir_db),
        }
    }

    pub(crate) fn as_generic_param_owner(
        self,
        db: &dyn HirAnalysisDb,
    ) -> Option<GenericParamOwnerId> {
        self.adt_ref(db).generic_owner_id(db)
    }

    pub(super) fn constraints(self, db: &dyn HirAnalysisDb) -> ConstraintListId {
        collect_adt_constraints(db, self)
    }

    /// Returns the expected type of the `idx`-th type parameter when the
    /// parameter is declared as a dependent type.
    fn expected_dependent_ty(self, db: &dyn HirAnalysisDb, idx: usize) -> Option<TyId> {
        let TyData::DependentTy(dependent_ty) = self.params(db).get(idx)?.data(db) else {
            return None;
        };

        Some(dependent_ty.ty(db))
    }
}

#[salsa::tracked]
pub struct FuncDef {
    pub hir_func: Func,

    pub name: IdentId,

    /// Generic parameters of the function.
    #[return_ref]
    pub params: Vec<TyId>,

    /// Argument types of the function.
    #[return_ref]
    pub arg_tys: Vec<TyId>,

    /// Return types of the function.
    pub ret_ty: TyId,
}

impl FuncDef {
    pub fn ingot(self, db: &dyn HirAnalysisDb) -> IngotId {
        self.hir_func(db)
            .top_mod(db.as_hir_db())
            .ingot(db.as_hir_db())
    }

    pub fn receiver_ty(self, db: &dyn HirAnalysisDb) -> Option<TyId> {
        if self.hir_func(db).is_method(db.as_hir_db()) {
            self.arg_tys(db).first().copied()
        } else {
            None
        }
    }

    pub(super) fn constraints(self, db: &dyn HirAnalysisDb) -> ConstraintListId {
        collect_func_def_constraints(db, self)
    }

    pub(super) fn hir_params(self, db: &dyn HirAnalysisDb) -> &[HirFuncParam] {
        const EMPTY: &[HirFuncParam] = &[];
        self.hir_func(db)
            .params(db.as_hir_db())
            .to_opt()
            .map(|list| list.data(db.as_hir_db()).as_ref())
            .unwrap_or(EMPTY)
    }

    /// Returns the expected type of the `idx`-th type parameter when the
    /// parameter is declared as a dependent type.
    fn expected_dependent_ty(self, db: &dyn HirAnalysisDb, idx: usize) -> Option<TyId> {
        let TyData::DependentTy(dependent_ty) = self.params(db).get(idx)?.data(db) else {
            return None;
        };

        Some(dependent_ty.ty(db))
    }
}

/// This struct represents a field of an ADT. If the ADT is an enum, this
/// represents a variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AdtField {
    name: Partial<IdentId>,

    /// Fields of the variant.
    /// If the adt is an struct or contract,
    /// the length of the vector is always 1.
    ///
    /// To allow recursive types, the type of the field is represented as a HIR
    /// type and.
    tys: Vec<Partial<HirTyId>>,

    scope: ScopeId,
}
impl AdtField {
    pub fn ty(&self, db: &dyn HirAnalysisDb, i: usize) -> TyId {
        if let Some(ty) = self.tys[i].to_opt() {
            lower_hir_ty(db, ty, self.scope)
        } else {
            TyId::invalid(db, InvalidCause::Other)
        }
    }

    /// Iterates all fields types of the `field`.
    pub fn iter_types<'a>(&'a self, db: &'a dyn HirAnalysisDb) -> impl Iterator<Item = TyId> + 'a {
        (0..self.num_types()).map(|i| self.ty(db, i))
    }

    pub fn num_types(&self) -> usize {
        self.tys.len()
    }

    pub(super) fn new(name: Partial<IdentId>, tys: Vec<Partial<HirTyId>>, scope: ScopeId) -> Self {
        Self { name, tys, scope }
    }
}

#[salsa::tracked(return_ref)]
pub fn ty_kind(db: &dyn HirAnalysisDb, ty: TyId) -> Kind {
    ty.data(db).kind(db)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyData {
    /// Type variable.
    TyVar(TyVar),

    /// Type Parameter.
    TyParam(TyParam),

    // Type application,
    // e.g., `Option<i32>` is represented as `TApp(TyConst(Option), TyConst(i32))`.
    TyApp(TyId, TyId),

    /// A concrete type, e.g., `i32`, `u32`, `bool`, `String`, `Result` etc.
    TyBase(TyBase),

    DependentTy(DependentTyId),

    // Invalid type which means the type is ill-formed.
    // This type can be unified with any other types.
    // NOTE: For type soundness check in this level, we don't consider trait satisfiability.
    Invalid(InvalidCause),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InvalidCause {
    /// Type is not fully applied where it is required.
    NotFullyApplied,

    /// Kind mismatch between two types.
    KindMismatch {
        expected: Option<Kind>,
        given: TyId,
    },

    InvalidConstParamTy {
        ty: TyId,
    },

    RecursiveConstParamTy,

    /// The given type doesn't match the expected dependent type.
    DependentTyMismatch {
        expected: TyId,
        given: TyId,
    },

    /// The given type is not a dependent type where it is required.
    DependentTyExpected {
        expected: TyId,
    },

    /// The given type is dependent type where it is *NOT* required.
    NormalTypeExpected {
        given: TyId,
    },

    /// Type alias parameter is not bound.
    /// NOTE: In our type system, type alias is a macro, so we can't perform
    /// partial application to type alias.
    UnboundTypeAliasParam {
        alias: HirTypeAlias,
        n_given_args: usize,
    },

    /// Associated Type is not allowed at the moment.
    AssocTy,

    // The given expression is not supported yet in the dependent type context.
    // TODO: Remove this error kind and introduce a new error kind for more specific cause when
    // type inference is implemented.
    InvalidDependentTyExpr {
        body: Body,
    },

    // TraitConstraintNotSat(PredicateId),
    /// `Other` indicates the cause is already reported in other analysis
    /// passes, e.g., parser or name resolution.
    Other,
}

impl InvalidCause {
    pub(super) fn kind_mismatch(expected: Option<&Kind>, ty: TyId) -> Self {
        Self::KindMismatch {
            expected: expected.cloned(),
            given: ty,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind {
    /// Represents monotypes, `*`.
    Star,

    /// Represents higher order types.
    /// e.g.,
    /// `* -> *` or `(* -> *) -> *`
    Abs(Box<Kind>, Box<Kind>),

    /// `Any` kind is set to the type iff the type is `Invalid`.
    Any,
}

impl Kind {
    fn abs(lhs: Kind, rhs: Kind) -> Self {
        Kind::Abs(Box::new(lhs), Box::new(rhs))
    }

    pub(super) fn does_match(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Star, Self::Star) => true,
            (Self::Abs(lhs1, rhs1), Self::Abs(lhs2, rhs2)) => {
                lhs1.does_match(lhs2) && rhs1.does_match(rhs2)
            }
            (Self::Any, _) => true,
            (_, Self::Any) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Star => write!(f, "*"),
            Self::Abs(lhs, rhs) => write!(f, "({} -> {})", lhs, rhs),
            Self::Any => write!(f, "Any"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyVar {
    pub universe: TyVarUniverse,
    pub kind: Kind,
    pub(super) key: InferenceKey,
}

/// Represents the universe of a type variable that indicates what type domain
/// can be unified with the type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyVarUniverse {
    /// Type variable that can be unified with any other types.
    General,

    /// Type variable that can be unified with only integral types.
    Integral,
}

impl TyVar {
    pub(super) fn pretty_print(&self) -> String {
        match self.universe {
            TyVarUniverse::General => format!("${}", self.key.0),
            TyVarUniverse::Integral => "<integer>".to_string(),
        }
    }
}

/// Type generics parameter. We also treat `Self` type in a trait definition as
/// a special type parameter.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyParam {
    pub name: IdentId,
    /// If the type parameter is not a`Self` type in a trait definition, this
    /// field is always `Some`.
    pub idx: Option<usize>,
    pub kind: Kind,
}

impl TyParam {
    pub(super) fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        self.name.data(db.as_hir_db()).to_string()
    }

    pub fn self_ty_param(kind: Kind) -> Self {
        Self {
            name: kw::SELF_TY,
            idx: None,
            kind,
        }
    }

    pub fn is_trait_self(&self) -> bool {
        self.idx.is_none()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyBase {
    Prim(PrimTy),
    Adt(AdtDef),
    Func(FuncDef),
}

impl TyBase {
    pub fn is_integral(self) -> bool {
        match self {
            Self::Prim(prim) => prim.is_integral(),
            _ => false,
        }
    }

    pub fn is_bool(self) -> bool {
        match self {
            Self::Prim(prim) => prim.is_bool(),
            _ => false,
        }
    }

    pub(super) fn tuple(n: usize) -> Self {
        Self::Prim(PrimTy::Tuple(n))
    }

    pub(super) fn bool() -> Self {
        Self::Prim(PrimTy::Bool)
    }

    fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        match self {
            Self::Prim(prim) => match prim {
                PrimTy::Bool => "bool",
                PrimTy::U8 => "u8",
                PrimTy::U16 => "u16",
                PrimTy::U32 => "u32",
                PrimTy::U64 => "u64",
                PrimTy::U128 => "u128",
                PrimTy::U256 => "u256",
                PrimTy::I8 => "i8",
                PrimTy::I16 => "i16",
                PrimTy::I32 => "i32",
                PrimTy::I64 => "i64",
                PrimTy::I128 => "i128",
                PrimTy::I256 => "i256",
                PrimTy::String => "String",
                PrimTy::Array => "[]",
                PrimTy::Tuple(_) => "()",
                PrimTy::Ptr => "*",
            }
            .to_string(),

            Self::Adt(adt) => adt.name(db).data(db.as_hir_db()).to_string(),

            Self::Func(func) => func
                .hir_func(db)
                .name(db.as_hir_db())
                .to_opt()
                .map(|name| name.data(db.as_hir_db()).as_str())
                .unwrap_or_else(|| "<invalid>")
                .to_string(),
        }
    }

    /// Returns the expected type of the `idx`-th type parameter when the
    /// parameter is declared as a dependent type.
    fn expected_dependent_ty(&self, db: &dyn HirAnalysisDb, idx: usize) -> Option<TyId> {
        match self {
            Self::Prim(prim_ty) => prim_ty.expected_dependent_ty(db, idx),
            Self::Adt(adt) => adt.expected_dependent_ty(db, idx),
            Self::Func(func) => func.expected_dependent_ty(db, idx),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy, Eq, Hash)]
pub enum PrimTy {
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
    String,
    Array,
    Tuple(usize),
    Ptr,
}

impl PrimTy {
    pub fn is_integral(self) -> bool {
        matches!(
            self,
            Self::U8
                | Self::U16
                | Self::U32
                | Self::U64
                | Self::U128
                | Self::U256
                | Self::I8
                | Self::I16
                | Self::I32
                | Self::I64
                | Self::I128
                | Self::I256
        )
    }

    pub fn is_bool(self) -> bool {
        matches!(self, Self::Bool)
    }

    /// Returns the expected type of the `idx`-th type parameter when the
    /// parameter is declared as a dependent type.
    fn expected_dependent_ty(self, db: &dyn HirAnalysisDb, idx: usize) -> Option<TyId> {
        match self {
            Self::Array if idx == 1 => {
                // FIXME: Change `U256` to `usize` when we add `usize` type.
                Some(TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256))))
            }
            Self::Tuple(_) => None,
            _ => None,
        }
    }
}

#[salsa::interned]
pub struct AdtRefId {
    pub data: AdtRef,
}

impl AdtRefId {
    pub fn scope(self, db: &dyn HirAnalysisDb) -> ScopeId {
        self.data(db).scope()
    }

    pub fn as_item(self, db: &dyn HirAnalysisDb) -> ItemKind {
        match self.data(db) {
            AdtRef::Enum(e) => e.into(),
            AdtRef::Struct(s) => s.into(),
            AdtRef::Contract(c) => c.into(),
        }
    }

    pub fn name(self, db: &dyn HirAnalysisDb) -> IdentId {
        let hir_db = db.as_hir_db();
        match self.data(db) {
            AdtRef::Enum(e) => e.name(hir_db),
            AdtRef::Struct(s) => s.name(hir_db),
            AdtRef::Contract(c) => c.name(hir_db),
        }
        .to_opt()
        .unwrap_or_else(|| IdentId::new(hir_db, "<unknown>".to_string()))
    }

    pub fn name_span(self, db: &dyn HirAnalysisDb) -> DynLazySpan {
        self.scope(db)
            .name_span(db.as_hir_db())
            .unwrap_or_else(DynLazySpan::invalid)
    }

    pub fn from_enum(db: &dyn HirAnalysisDb, enum_: Enum) -> Self {
        Self::new(db, AdtRef::Enum(enum_))
    }

    pub fn from_struct(db: &dyn HirAnalysisDb, struct_: Struct) -> Self {
        Self::new(db, AdtRef::Struct(struct_))
    }

    pub fn from_contract(db: &dyn HirAnalysisDb, contract: Contract) -> Self {
        Self::new(db, AdtRef::Contract(contract))
    }

    pub(crate) fn generic_owner_id(self, db: &dyn HirAnalysisDb) -> Option<GenericParamOwnerId> {
        match self.data(db) {
            AdtRef::Enum(e) => Some(GenericParamOwnerId::new(db, e.into())),
            AdtRef::Struct(s) => Some(GenericParamOwnerId::new(db, s.into())),
            AdtRef::Contract(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AdtRef {
    Enum(Enum),
    Struct(Struct),
    Contract(Contract),
}

impl AdtRef {
    pub fn scope(self) -> ScopeId {
        match self {
            Self::Enum(e) => e.scope(),
            Self::Struct(s) => s.scope(),
            Self::Contract(c) => c.scope(),
        }
    }
}

pub trait Subst {
    fn get(&mut self, from: TyId) -> Option<TyId>;

    fn apply(&mut self, db: &dyn HirAnalysisDb, ty: TyId) -> TyId {
        ty.apply_subst(db, self)
    }
}

impl Subst for FxHashMap<TyId, TyId> {
    fn get(&mut self, from: TyId) -> Option<TyId> {
        FxHashMap::get(self, &from).copied()
    }
}

pub(super) trait HasKind {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind;
}

impl HasKind for TyData {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        match self {
            TyData::TyVar(ty_var) => ty_var.kind(db),
            TyData::TyParam(ty_param) => ty_param.kind.clone(),
            TyData::TyBase(ty_const) => ty_const.kind(db),
            TyData::TyApp(abs, _) => match abs.kind(db) {
                // `TyId::app` method handles the kind mismatch, so we don't need to verify it again
                // here.
                Kind::Abs(_, ret) => ret.as_ref().clone(),
                _ => Kind::Any,
            },

            TyData::DependentTy(dependent_ty) => dependent_ty.ty(db).kind(db).clone(),

            TyData::Invalid(_) => Kind::Any,
        }
    }
}

impl HasKind for TyVar {
    fn kind(&self, _db: &dyn HirAnalysisDb) -> Kind {
        self.kind.clone()
    }
}

impl HasKind for TyBase {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        match self {
            TyBase::Prim(prim) => prim.kind(db),
            TyBase::Adt(adt) => adt.kind(db),
            TyBase::Func(func) => func.kind(db),
        }
    }
}

impl HasKind for PrimTy {
    fn kind(&self, _: &dyn HirAnalysisDb) -> Kind {
        match self {
            Self::Array => (0..2).fold(Kind::Star, |acc, _| Kind::abs(Kind::Star, acc)),
            Self::Tuple(n) => (0..*n).fold(Kind::Star, |acc, _| Kind::abs(Kind::Star, acc)),
            Self::Ptr => Kind::abs(Kind::Star, Kind::Star),
            _ => Kind::Star,
        }
    }
}

impl HasKind for AdtDef {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        let mut kind = Kind::Star;
        for param in self.params(db).iter().rev() {
            kind = Kind::abs(ty_kind(db, *param).clone(), kind);
        }

        kind
    }
}

impl HasKind for FuncDef {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        let mut kind = Kind::Star;
        for param in self.params(db).iter().rev() {
            kind = Kind::abs(ty_kind(db, *param).clone(), kind);
        }

        kind
    }
}

#[salsa::tracked(return_ref)]
pub(crate) fn free_inference_keys(db: &dyn HirAnalysisDb, ty: TyId) -> BTreeSet<InferenceKey> {
    struct FreeInferenceKeyCollector(BTreeSet<InferenceKey>);
    impl TyVisitor for FreeInferenceKeyCollector {
        fn visit_var(&mut self, _db: &dyn HirAnalysisDb, var: &TyVar) {
            self.0.insert(var.key);
        }
    }

    let mut collector = FreeInferenceKeyCollector(BTreeSet::new());
    collector.visit_ty(db, ty);
    collector.0
}

#[salsa::tracked(return_ref)]
pub(crate) fn collect_type_params(db: &dyn HirAnalysisDb, ty: TyId) -> BTreeSet<TyId> {
    struct TypeParamCollector(BTreeSet<TyId>);
    impl TyVisitor for TypeParamCollector {
        fn visit_param(&mut self, _db: &dyn HirAnalysisDb, param: &TyParam) {
            let param_ty = TyId::new(_db, TyData::TyParam(param.clone()));
            self.0.insert(param_ty);
        }

        fn visit_dependent_param(
            &mut self,
            db: &dyn HirAnalysisDb,
            param: &TyParam,
            dep_ty_ty: TyId,
        ) {
            let dep_ty = DependentTyId::new(db, DependentTyData::TyParam(param.clone(), dep_ty_ty));
            let dep_param_ty = TyId::new(db, TyData::DependentTy(dep_ty));
            self.0.insert(dep_param_ty);
        }
    }

    let mut collector = TypeParamCollector(BTreeSet::new());
    collector.visit_ty(db, ty);
    collector.0
}

#[salsa::tracked(return_ref)]
pub(crate) fn pretty_print_ty(db: &dyn HirAnalysisDb, ty: TyId) -> String {
    match ty.data(db) {
        TyData::TyVar(var) => var.pretty_print(),
        TyData::TyParam(param) => param.pretty_print(db),
        TyData::TyApp(_, _) => pretty_print_ty_app(db, ty),
        TyData::TyBase(ty_con) => ty_con.pretty_print(db),
        TyData::DependentTy(dependent_ty) => dependent_ty.pretty_print(db),
        _ => "<invalid>".to_string(),
    }
}

fn pretty_print_ty_app(db: &dyn HirAnalysisDb, ty: TyId) -> String {
    use PrimTy::*;
    use TyBase::*;

    let (base, args) = decompose_ty_app(db, ty);
    match base.data(db) {
        TyData::TyBase(Prim(Array)) => {
            let elem_ty = args[0].pretty_print(db);
            let len = args[1].pretty_print(db);
            format!("[{}; {}]", elem_ty, len)
        }

        TyData::TyBase(Prim(Tuple(_))) => {
            let mut args = args.into_iter();
            let mut s = ("(").to_string();
            if let Some(first) = args.next() {
                s.push_str(first.pretty_print(db));
                for arg in args {
                    s.push_str(", ");
                    s.push_str(arg.pretty_print(db));
                }
            }
            s.push(')');
            s
        }

        _ => {
            let mut args = args.into_iter();
            let mut s = (base.pretty_print(db)).to_string();
            if let Some(first) = args.next() {
                s.push('<');
                s.push_str(first.pretty_print(db));
                for arg in args {
                    s.push_str(", ");
                    s.push_str(arg.pretty_print(db));
                }
                s.push('>');
            }
            s
        }
    }
}

/// Decompose type application into the base type and type arguments.
/// e.g., `App(App(T, U), App(V, W))` -> `(T, [U, App(V, W)])`
fn decompose_ty_app(db: &dyn HirAnalysisDb, ty: TyId) -> (TyId, Vec<TyId>) {
    struct TyAppDecomposer {
        base: Option<TyId>,
        args: Vec<TyId>,
    }

    impl TyVisitor for TyAppDecomposer {
        fn visit_ty(&mut self, db: &dyn HirAnalysisDb, ty: TyId) {
            match ty.data(db) {
                TyData::TyApp(lhs, rhs) => {
                    self.visit_ty(db, *lhs);
                    self.args.push(*rhs);
                }
                _ => self.base = Some(ty),
            }
        }

        fn visit_app(&mut self, db: &dyn HirAnalysisDb, lhs: TyId, rhs: TyId) {
            self.visit_ty(db, lhs);
            self.args.push(rhs);
        }
    }

    match ty.data(db) {
        TyData::TyApp(lhs, rhs) => {
            let mut decomposer = TyAppDecomposer {
                base: None,
                args: Vec::new(),
            };
            decomposer.visit_app(db, *lhs, *rhs);
            (
                decomposer.base.unwrap(),
                decomposer.args.into_iter().collect(),
            )
        }

        _ => (ty, vec![]),
    }
}
