//! This module contains the type definitions for the Fe type system.

use std::{collections::BTreeSet, fmt};

use hir::{
    hir_def::{
        kw,
        prim_ty::{IntTy as HirIntTy, PrimTy as HirPrimTy, UintTy as HirUintTy},
        scope_graph::ScopeId,
        Body, Contract, Enum, Func, FuncParam as HirFuncParam, IdentId, IngotId, IntegerId,
        ItemKind, Partial, Struct, TypeAlias as HirTypeAlias, TypeId as HirTyId, VariantKind,
    },
    span::DynLazySpan,
};
use rustc_hash::{FxHashMap, FxHashSet};

use super::{
    binder::Binder,
    const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
    constraint::{
        collect_adt_constraints, collect_func_def_constraints, AssumptionListId, ConstraintListId,
    },
    diagnostics::{TraitConstraintDiag, TyDiagCollection, TyLowerDiag},
    ty_lower::{lower_hir_ty, GenericParamOwnerId, GenericParamTypeSet},
    unify::{InferenceKey, UnificationTable},
    visitor::{TypeVisitable, TypeVisitor},
};
use crate::{
    ty::{
        constraint_solver::{check_ty_wf, GoalSatisfiability},
        ty_lower::collect_generic_params,
    },
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

    /// Returns the current arguments of the type.
    /// ## Example
    /// Calling this method for `TyApp<TyApp<Adt, T>, U>` returns `[T, U]`.
    pub fn generic_args(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        let (_, args) = self.decompose_ty_app(db);
        args
    }

    /// Returns teh base type of this type.
    /// ## Example
    /// `TyApp<Adt, i32>` returns `Adt`.
    /// `TyApp<TyParam<T>, i32>` returns `TyParam<T>`.
    pub fn base_ty(self, db: &dyn HirAnalysisDb) -> TyId {
        self.decompose_ty_app(db).0
    }

    /// Returns the type of const type if the type is a const type.
    pub fn const_ty_ty(self, db: &dyn HirAnalysisDb) -> Option<TyId> {
        match self.data(db) {
            TyData::ConstTy(const_ty) => Some(const_ty.ty(db)),
            _ => None,
        }
    }

    /// Returns `true` if the type is invalid, see [`contains_invalid`] if you
    /// want to check if the type contains any invalid types as a part of the
    /// type.
    pub fn is_invalid(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.data(db), TyData::Invalid(_))
    }

    /// Returns `true` is the type has `*` kind.
    pub fn is_star_kind(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.kind(db), Kind::Star | Kind::Any)
    }

    /// Returns `true` if the type is an integral type(like `u32`, `i32` etc.)
    pub fn is_integral(self, db: &dyn HirAnalysisDb) -> bool {
        match self.data(db) {
            TyData::TyBase(ty_base) => ty_base.is_integral(),
            TyData::TyVar(var) => {
                matches!(var.sort, TyVarSort::Integral)
            }
            _ => false,
        }
    }

    /// Returns `true` if the type is a bool type.
    pub fn is_bool(self, db: &dyn HirAnalysisDb) -> bool {
        match self.data(db) {
            TyData::TyBase(ty_base) => ty_base.is_bool(),
            _ => false,
        }
    }

    /// Returns `true` if the type is a bottom type.
    pub fn is_bot(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.data(db), TyData::Bot)
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
            TyData::ConstTy(const_ty) => const_ty.ty(db).contains_invalid(db),
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
    pub(super) fn decompose_ty_app(self, db: &dyn HirAnalysisDb) -> (TyId, &[TyId]) {
        let (base, args) = decompose_ty_app(db, self);
        (*base, args)
    }

    pub(super) fn ptr(db: &dyn HirAnalysisDb) -> TyId {
        Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Ptr)))
    }

    pub(super) fn tuple(db: &dyn HirAnalysisDb, n: usize) -> Self {
        Self::new(db, TyData::TyBase(TyBase::tuple(n)))
    }

    pub(super) fn tuple_with_elems(db: &dyn HirAnalysisDb, elems: &[TyId]) -> Self {
        let base = TyBase::tuple(elems.len());
        let mut ty = Self::new(db, TyData::TyBase(base));
        for &elem in elems {
            ty = Self::app(db, ty, elem);
        }
        ty
    }

    pub(super) fn bool(db: &dyn HirAnalysisDb) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Bool)))
    }

    pub(super) fn array(db: &dyn HirAnalysisDb) -> Self {
        let base = TyBase::Prim(PrimTy::Array);
        Self::new(db, TyData::TyBase(base))
    }

    pub(super) fn array_with_elem(db: &dyn HirAnalysisDb, elem: TyId, len: usize) -> Self {
        let base = TyBase::Prim(PrimTy::Array);
        let base = Self::new(db, TyData::TyBase(base));
        let array = TyId::app(db, base, elem);

        let len = EvaluatedConstTy::LitInt(IntegerId::new(db.as_hir_db(), len.into()));
        let len = ConstTyData::Evaluated(len, array.applicable_ty(db).unwrap().const_ty.unwrap());
        let len = TyId::const_ty(db, ConstTyId::new(db, len));

        TyId::app(db, array, len)
    }

    pub(super) fn unit(db: &dyn HirAnalysisDb) -> Self {
        Self::tuple(db, 0)
    }

    pub(super) fn bot(db: &dyn HirAnalysisDb) -> Self {
        Self::new(db, TyData::Bot)
    }

    pub(super) fn const_ty(db: &dyn HirAnalysisDb, const_ty: ConstTyId) -> Self {
        Self::new(db, TyData::ConstTy(const_ty))
    }

    pub(super) fn adt(db: &dyn HirAnalysisDb, adt: AdtDef) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Adt(adt)))
    }

    pub(super) fn func(db: &dyn HirAnalysisDb, func: FuncDef) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Func(func)))
    }

    pub(super) fn is_func(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::TyBase(TyBase::Func(_)))
    }

    pub(super) fn is_trait_self(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::TyParam(ty_param) if ty_param.is_trait_self)
    }

    pub(super) fn is_ty_var(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::TyVar(_))
    }

    pub(super) fn is_const_ty(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::ConstTy(_))
    }

    pub(super) fn is_tuple(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(
            self.base_ty(db).data(db),
            TyData::TyBase(TyBase::Prim(PrimTy::Tuple(_)))
        )
    }

    pub(super) fn is_array(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(
            self.base_ty(db).data(db),
            TyData::TyBase(TyBase::Prim(PrimTy::Array))
        )
    }

    pub(super) fn is_string(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(
            self.base_ty(db).data(db),
            TyData::TyBase(TyBase::Prim(PrimTy::String))
        )
    }

    pub(super) fn contains_ty_param(self, db: &dyn HirAnalysisDb) -> bool {
        !collect_type_params(db, self).is_empty()
    }

    pub(super) fn contains_trait_self(self, db: &dyn HirAnalysisDb) -> bool {
        match self.data(db) {
            TyData::TyParam(ty_param) => ty_param.is_trait_self,
            TyData::TyApp(lhs, rhs) => lhs.contains_trait_self(db) || rhs.contains_trait_self(db),
            _ => false,
        }
    }

    /// Emit diagnostics for the type if the type contains invalid types.
    pub(super) fn emit_diag(
        self,
        db: &dyn HirAnalysisDb,
        span: DynLazySpan,
    ) -> Option<TyDiagCollection> {
        struct EmitDiagVisitor<'db> {
            db: &'db dyn HirAnalysisDb,
            diag: Option<TyDiagCollection>,
            span: DynLazySpan,
        }

        impl<'db> TypeVisitor<'db> for EmitDiagVisitor<'db> {
            fn db(&self) -> &'db dyn HirAnalysisDb {
                self.db
            }

            fn visit_invalid(&mut self, cause: &InvalidCause) {
                let db = self.db;

                let span = self.span.clone();
                let diag = match cause {
                    InvalidCause::NotFullyApplied => {
                        TyLowerDiag::expected_star_kind_ty(span).into()
                    }

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

                    InvalidCause::ConstTyMismatch { expected, given } => {
                        TyLowerDiag::const_ty_mismatch(db, span, *expected, *given).into()
                    }

                    InvalidCause::ConstTyExpected { expected } => {
                        TyLowerDiag::const_ty_expected(db, span, *expected).into()
                    }

                    InvalidCause::NormalTypeExpected { given } => {
                        TyLowerDiag::normal_type_expected(db, span, *given).into()
                    }

                    InvalidCause::UnboundTypeAliasParam {
                        alias,
                        n_given_args: n_given_arg,
                    } => TyLowerDiag::unbound_type_alias_param(span, *alias, *n_given_arg).into(),

                    InvalidCause::AssocTy => TyLowerDiag::assoc_ty(span).into(),

                    InvalidCause::InvalidConstTyExpr { body } => {
                        TyLowerDiag::InvalidConstTyExpr(body.lazy_span().into()).into()
                    }

                    InvalidCause::Other => return,
                };

                self.diag.get_or_insert(diag);
            }
        }

        let mut visitor = EmitDiagVisitor {
            db,
            diag: None,
            span,
        };

        visitor.visit_ty(self);
        visitor.diag
    }

    pub(super) fn emit_sat_diag(
        self,
        db: &dyn HirAnalysisDb,
        assumptions: AssumptionListId,
        span: DynLazySpan,
    ) -> Option<TyDiagCollection> {
        match check_ty_wf(db, self, assumptions) {
            GoalSatisfiability::Satisfied => None,
            GoalSatisfiability::NotSatisfied(goal) => {
                Some(TraitConstraintDiag::trait_bound_not_satisfied(db, span, goal).into())
            }
            GoalSatisfiability::InfiniteRecursion(goal) => {
                Some(TraitConstraintDiag::infinite_bound_recursion(db, span, goal).into())
            }
        }
    }

    pub(super) fn ty_var(
        db: &dyn HirAnalysisDb,
        sort: TyVarSort,
        kind: Kind,
        key: InferenceKey,
    ) -> Self {
        Self::new(db, TyData::TyVar(TyVar { sort, kind, key }))
    }

    pub(super) fn const_ty_var(db: &dyn HirAnalysisDb, ty: TyId, key: InferenceKey) -> Self {
        let ty_var = TyVar {
            sort: TyVarSort::General,
            kind: ty.kind(db).clone(),
            key,
        };

        let data = ConstTyData::TyVar(ty_var, ty);
        Self::new(db, TyData::ConstTy(ConstTyId::new(db, data)))
    }

    /// Perform type level application.
    pub(super) fn app(db: &dyn HirAnalysisDb, lhs: Self, rhs: Self) -> TyId {
        let Some(applicable_ty) = lhs.applicable_ty(db) else {
            return Self::invalid(db, InvalidCause::kind_mismatch(None, rhs));
        };

        let rhs = rhs
            .evaluate_const_ty(db, applicable_ty.const_ty)
            .unwrap_or_else(|cause| Self::invalid(db, cause));

        let applicable_kind = applicable_ty.kind;
        if !applicable_kind.does_match(rhs.kind(db)) {
            return Self::invalid(db, InvalidCause::kind_mismatch(Some(&applicable_kind), rhs));
        };

        Self::new(db, TyData::TyApp(lhs, rhs))
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
        Self::new(db, TyData::TyBase(hir_prim.into()))
    }

    pub(super) fn const_ty_param(self, db: &dyn HirAnalysisDb) -> Option<TyId> {
        if let TyData::ConstTy(const_ty) = self.data(db) {
            Some(const_ty.ty(db))
        } else {
            None
        }
    }

    pub(super) fn evaluate_const_ty(
        self,
        db: &dyn HirAnalysisDb,
        expected_ty: Option<TyId>,
    ) -> Result<TyId, InvalidCause> {
        match (expected_ty, self.data(db)) {
            (Some(expected_const_ty), TyData::ConstTy(const_ty)) => {
                if expected_const_ty.is_invalid(db) {
                    Err(InvalidCause::Other)
                } else {
                    let evaluated_const_ty = const_ty.evaluate(db, expected_const_ty.into());
                    let evaluated_const_ty_ty = evaluated_const_ty.ty(db);
                    if let Some(cause) = evaluated_const_ty_ty.invalid_cause(db) {
                        Err(cause)
                    } else {
                        Ok(TyId::const_ty(db, evaluated_const_ty))
                    }
                }
            }

            (Some(expected_const_ty), _) => {
                if expected_const_ty.is_invalid(db) {
                    Err(InvalidCause::Other)
                } else {
                    Err(InvalidCause::ConstTyExpected {
                        expected: expected_const_ty,
                    })
                }
            }

            (None, TyData::ConstTy(const_ty)) => {
                let evaluated_const_ty = const_ty.evaluate(db, None);
                Err(InvalidCause::NormalTypeExpected {
                    given: TyId::const_ty(db, evaluated_const_ty),
                })
            }

            (None, _) => Ok(self),
        }
    }

    /// Returns the property of the type that can be applied to the `self`.
    pub fn applicable_ty(self, db: &dyn HirAnalysisDb) -> Option<ApplicableTyProp> {
        let applicable_kind = match self.kind(db) {
            Kind::Star => return None,
            Kind::Abs(arg, _) => *arg.clone(),
            Kind::Any => Kind::Any,
        };

        let (base, args) = self.decompose_ty_app(db);
        let TyData::TyBase(base) = base.data(db) else {
            return Some(ApplicableTyProp {
                kind: applicable_kind.clone(),
                const_ty: None,
            });
        };

        let const_ty = match base {
            TyBase::Adt(adt_def) => {
                let params = adt_def.params(db);
                let param = params.get(args.len()).copied();
                param.and_then(|ty| ty.const_ty_ty(db))
            }

            TyBase::Func(func_def) => {
                let params = func_def.params(db);
                let param = params.get(args.len()).copied();
                param.and_then(|ty| ty.const_ty_ty(db))
            }

            TyBase::Prim(PrimTy::Array) => {
                if args.len() == 1 {
                    Some(TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256))))
                } else {
                    None
                }
            }

            TyBase::Prim(PrimTy::String) => {
                if args.is_empty() {
                    Some(TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256))))
                } else {
                    None
                }
            }

            _ => None,
        };

        Some(ApplicableTyProp {
            kind: applicable_kind.clone(),
            const_ty,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ApplicableTyProp {
    /// A kind of the applicable type.
    pub kind: Kind,
    /// An expected type of const type if the applicable type is a const type.
    pub const_ty: Option<TyId>,
}

/// Represents a ADT type definition.
#[salsa::tracked]
pub struct AdtDef {
    pub adt_ref: AdtRefId,

    /// Type parameters of the ADT.
    #[return_ref]
    param_set: GenericParamTypeSet,

    /// Fields of the ADT, if the ADT is an enum, this represents variants.
    /// Otherwise, `fields[0]` represents all fields of the struct.
    #[return_ref]
    pub fields: Vec<AdtFieldList>,
}

impl AdtDef {
    pub(crate) fn name(self, db: &dyn HirAnalysisDb) -> IdentId {
        self.adt_ref(db).name(db)
    }

    pub(crate) fn params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        self.param_set(db).params(db)
    }

    pub(crate) fn original_params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        self.param_set(db).explicit_params(db)
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
}

#[salsa::tracked]
pub struct FuncDef {
    pub hir_func: Func,

    pub name: IdentId,

    pub params_set: GenericParamTypeSet,

    /// Argument types of the function.
    #[return_ref]
    pub arg_tys: Vec<Binder<TyId>>,

    /// Return types of the function.
    pub ret_ty: Binder<TyId>,
}

impl FuncDef {
    pub fn ingot(self, db: &dyn HirAnalysisDb) -> IngotId {
        self.hir_func(db)
            .top_mod(db.as_hir_db())
            .ingot(db.as_hir_db())
    }

    pub fn name_span(self, db: &dyn HirAnalysisDb) -> DynLazySpan {
        self.hir_func(db).lazy_span().name_moved().into()
    }

    pub fn params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        self.params_set(db).params(db)
    }

    pub fn explicit_params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        self.params_set(db).explicit_params(db)
    }

    pub fn receiver_ty(self, db: &dyn HirAnalysisDb) -> Option<Binder<TyId>> {
        if self.hir_func(db).is_method(db.as_hir_db()) {
            self.arg_tys(db).first().copied()
        } else {
            None
        }
    }

    pub fn is_method(self, db: &dyn HirAnalysisDb) -> bool {
        self.hir_func(db).is_method(db.as_hir_db())
    }

    pub fn offset_to_explicit_params_position(self, db: &dyn HirAnalysisDb) -> usize {
        self.params_set(db).offset_to_explicit_params_position(db)
    }

    pub(super) fn hir_params(self, db: &dyn HirAnalysisDb) -> &[HirFuncParam] {
        const EMPTY: &[HirFuncParam] = &[];
        self.hir_func(db)
            .params(db.as_hir_db())
            .to_opt()
            .map(|list| list.data(db.as_hir_db()).as_ref())
            .unwrap_or(EMPTY)
    }
}

/// This struct represents a field of an ADT. If the ADT is an enum, this
/// represents a variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AdtFieldList {
    /// Fields of the variant.
    /// If the adt is an struct or contract,
    /// the length of the vector is always 1.
    ///
    /// To allow recursive types, the type of the field is represented as a HIR
    /// type and.
    tys: Vec<Partial<HirTyId>>,

    scope: ScopeId,
}
impl AdtFieldList {
    pub fn ty(&self, db: &dyn HirAnalysisDb, i: usize) -> Binder<TyId> {
        let ty = if let Some(ty) = self.tys[i].to_opt() {
            lower_hir_ty(db, ty, self.scope)
        } else {
            TyId::invalid(db, InvalidCause::Other)
        };

        Binder::bind(ty)
    }

    /// Iterates all fields types of the `field`.
    pub fn iter_types<'a>(
        &'a self,
        db: &'a dyn HirAnalysisDb,
    ) -> impl Iterator<Item = Binder<TyId>> + 'a {
        (0..self.num_types()).map(|i| self.ty(db, i))
    }

    pub fn num_types(&self) -> usize {
        self.tys.len()
    }

    pub(super) fn new(tys: Vec<Partial<HirTyId>>, scope: ScopeId) -> Self {
        Self { tys, scope }
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

    ConstTy(ConstTyId),

    /// A bottom type.
    Bot,

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

    /// The given type doesn't match the expected const type.
    ConstTyMismatch {
        expected: TyId,
        given: TyId,
    },

    /// The given type is not a const type where it is required.
    ConstTyExpected {
        expected: TyId,
    },

    /// The given type is const type where it is *NOT* required.
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

    // The given expression is not supported yet in the const type context.
    // TODO: Remove this error kind and introduce a new error kind for more specific cause when
    // type inference is implemented.
    InvalidConstTyExpr {
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
    /// Represents star kind, i.e., `*` kind.
    Star,

    /// Represents higher kinded types.
    /// e.g.,
    /// `* -> *`, `(* -> *) -> *` or `* -> (* -> *) -> *`
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
    pub sort: TyVarSort,
    pub kind: Kind,
    pub(super) key: InferenceKey,
}

/// Represents the sort of a type variable that indicates what type domain
/// can be unified with the type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyVarSort {
    /// Type variable that can be unified with any other types.
    General,

    /// Type variable that can be unified with only string types that has at
    /// least the given length.
    String(usize),

    /// Type variable that can be unified with only integral types.
    Integral,
}

impl PartialOrd for TyVarSort {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::General, Self::General) => Some(std::cmp::Ordering::Equal),
            (Self::General, _) => Some(std::cmp::Ordering::Less),
            (_, Self::General) => Some(std::cmp::Ordering::Greater),
            (Self::String(n1), Self::String(n2)) => n1.partial_cmp(n2),
            (Self::String(_), _) | (_, Self::String(_)) => None,
            (Self::Integral, Self::Integral) => Some(std::cmp::Ordering::Equal),
        }
    }
}

impl TyVar {
    pub(super) fn pretty_print(&self) -> String {
        match self.sort {
            TyVarSort::General => ("_").to_string(),
            TyVarSort::Integral => "<integer>".to_string(),
            TyVarSort::String(n) => format!("String<{}>", n).to_string(),
        }
    }
}

/// Type generics parameter. We also treat `Self` type in a trait definition as
/// a special type parameter.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyParam {
    pub name: IdentId,
    // The index points to the lowered type parameter list, which means that the idx doesn't
    // correspond to the index of the type parameter in the original source code.
    // E.g.,
    // ```fe
    // impl Foo<T, U> {
    //     fn foo<V>(v: V) {}
    // ```
    // The `foo`'s type parameter list is lowered to [`T`, `U`, `V`], so the index of `V` is 2.
    pub idx: usize,
    pub kind: Kind,
    pub is_trait_self: bool,
}

impl TyParam {
    pub(super) fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        self.name.data(db.as_hir_db()).to_string()
    }

    pub(super) fn normal_param(name: IdentId, idx: usize, kind: Kind) -> Self {
        Self {
            name,
            idx,
            kind,
            is_trait_self: false,
        }
    }

    pub(super) fn const_param(name: IdentId, idx: usize) -> Self {
        Self {
            name,
            idx,
            kind: Kind::Star,
            is_trait_self: false,
        }
    }

    pub(super) fn trait_self(kind: Kind) -> Self {
        Self {
            name: kw::SELF_TY,
            idx: 0,
            kind,
            is_trait_self: true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
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

            Self::Func(func) => format!(
                "fn {}",
                func.hir_func(db)
                    .name(db.as_hir_db())
                    .to_opt()
                    .map(|name| name.data(db.as_hir_db()).as_str())
                    .unwrap_or_else(|| "<invalid>")
            ),
        }
    }

    pub(super) fn adt(self) -> Option<AdtDef> {
        match self {
            Self::Adt(adt) => Some(adt),
            _ => None,
        }
    }
}

impl From<HirPrimTy> for TyBase {
    fn from(hir_prim: HirPrimTy) -> Self {
        match hir_prim {
            HirPrimTy::Bool => Self::Prim(PrimTy::Bool),

            HirPrimTy::Int(int_ty) => match int_ty {
                HirIntTy::I8 => Self::Prim(PrimTy::I8),
                HirIntTy::I16 => Self::Prim(PrimTy::I16),
                HirIntTy::I32 => Self::Prim(PrimTy::I32),
                HirIntTy::I64 => Self::Prim(PrimTy::I64),
                HirIntTy::I128 => Self::Prim(PrimTy::I128),
                HirIntTy::I256 => Self::Prim(PrimTy::I256),
            },

            HirPrimTy::Uint(uint_ty) => match uint_ty {
                HirUintTy::U8 => Self::Prim(PrimTy::U8),
                HirUintTy::U16 => Self::Prim(PrimTy::U16),
                HirUintTy::U32 => Self::Prim(PrimTy::U32),
                HirUintTy::U64 => Self::Prim(PrimTy::U64),
                HirUintTy::U128 => Self::Prim(PrimTy::U128),
                HirUintTy::U256 => Self::Prim(PrimTy::U256),
            },

            HirPrimTy::String => Self::Prim(PrimTy::String),
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

    pub fn kind_name(self, db: &dyn HirAnalysisDb) -> &'static str {
        self.as_item(db).kind_name()
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

            TyData::ConstTy(const_ty) => const_ty.ty(db).kind(db).clone(),

            TyData::Bot => Kind::Any,

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
            Self::String => Kind::abs(Kind::Star, Kind::Star),
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

pub(crate) fn free_inference_keys<'db, V>(
    db: &'db dyn HirAnalysisDb,
    visitable: V,
) -> BTreeSet<InferenceKey>
where
    V: TypeVisitable<'db>,
{
    struct FreeInferenceKeyCollector<'db> {
        db: &'db dyn HirAnalysisDb,
        keys: BTreeSet<InferenceKey>,
    }

    impl<'db> TypeVisitor<'db> for FreeInferenceKeyCollector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_var(&mut self, var: &TyVar) {
            self.keys.insert(var.key);
        }
    }

    let mut collector = FreeInferenceKeyCollector {
        db,
        keys: BTreeSet::new(),
    };

    visitable.visit_with(&mut collector);
    collector.keys
}

pub(crate) fn collect_type_params<'db, V>(
    db: &'db dyn HirAnalysisDb,
    visitable: V,
) -> FxHashSet<TyId>
where
    V: TypeVisitable<'db>,
{
    struct TypeParamCollector<'db> {
        db: &'db dyn HirAnalysisDb,
        params: FxHashSet<TyId>,
    }

    impl<'db> TypeVisitor<'db> for TypeParamCollector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_param(&mut self, param: &TyParam) {
            let param_ty = TyId::new(self.db, TyData::TyParam(param.clone()));
            self.params.insert(param_ty);
        }

        fn visit_const_param(&mut self, param: &TyParam, const_ty_ty: TyId) {
            let db = self.db;
            let const_ty = ConstTyId::new(db, ConstTyData::TyParam(param.clone(), const_ty_ty));
            let const_param_ty = TyId::new(db, TyData::ConstTy(const_ty));
            self.params.insert(const_param_ty);
        }
    }

    let mut collector = TypeParamCollector {
        db,
        params: FxHashSet::default(),
    };

    visitable.visit_with(&mut collector);
    collector.params
}

#[salsa::tracked(return_ref)]
pub(crate) fn pretty_print_ty(db: &dyn HirAnalysisDb, ty: TyId) -> String {
    match ty.data(db) {
        TyData::TyVar(var) => var.pretty_print(),
        TyData::TyParam(param) => param.pretty_print(db),
        TyData::TyApp(_, _) => pretty_print_ty_app(db, ty),
        TyData::TyBase(ty_con) => ty_con.pretty_print(db),
        TyData::ConstTy(const_ty) => const_ty.pretty_print(db),
        TyData::Bot => "bot".to_string(),
        TyData::Invalid(..) => "<invalid>".to_string(),
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
            let mut args = args.iter();
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
            let mut args = args.iter();
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
#[salsa::tracked(return_ref)]
pub(crate) fn decompose_ty_app(db: &dyn HirAnalysisDb, ty: TyId) -> (TyId, Vec<TyId>) {
    struct TyAppDecomposer<'db> {
        db: &'db dyn HirAnalysisDb,
        base: Option<TyId>,
        args: Vec<TyId>,
    }

    impl<'db> TypeVisitor<'db> for TyAppDecomposer<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId) {
            let db = self.db;

            match ty.data(db) {
                TyData::TyApp(lhs, rhs) => {
                    self.visit_ty(*lhs);
                    self.args.push(*rhs);
                }
                _ => self.base = Some(ty),
            }
        }
    }

    let mut decomposer = TyAppDecomposer {
        db,
        base: None,
        args: Vec::new(),
    };

    ty.visit_with(&mut decomposer);
    (decomposer.base.unwrap(), decomposer.args)
}
