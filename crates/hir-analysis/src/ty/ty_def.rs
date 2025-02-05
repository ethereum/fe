//! This module contains the type definitions for the Fe type system.

use std::fmt;

use bitflags::bitflags;
use common::{indexmap::IndexSet, input::IngotKind};
use hir::{
    hir_def::{
        prim_ty::{IntTy as HirIntTy, PrimTy as HirPrimTy, UintTy as HirUintTy},
        scope_graph::ScopeId,
        Body, Enum, IdentId, IngotId, IntegerId, Struct, TypeAlias as HirTypeAlias,
    },
    span::DynLazySpan,
};
use if_chain::if_chain;
use rustc_hash::FxHashSet;

use super::{
    adt_def::AdtDef,
    const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
    diagnostics::{TraitConstraintDiag, TyDiagCollection, TyLowerDiag},
    func_def::FuncDef,
    trait_resolution::{PredicateListId, WellFormedness},
    ty_lower::{collect_generic_params, GenericParamOwnerId},
    unify::InferenceKey,
    visitor::{TyVisitable, TyVisitor},
};
use crate::{
    ty::{adt_def::AdtRef, trait_resolution::check_ty_wf},
    HirAnalysisDb,
};

#[salsa::interned]
pub struct TyId<'db> {
    #[return_ref]
    pub data: TyData<'db>,
}

#[salsa::tracked]
impl<'db> TyId<'db> {
    /// Returns the kind of the type.
    #[salsa::tracked(return_ref)]
    pub fn kind(self, db: &'db dyn HirAnalysisDb) -> Kind {
        self.data(db).kind(db)
    }

    /// Returns the current arguments of the type.
    /// ## Example
    /// Calling this method for `TyApp<TyApp<Adt, T>, U>` returns `[T, U]`.
    pub fn generic_args(self, db: &'db dyn HirAnalysisDb) -> &'db [Self] {
        let (_, args) = self.decompose_ty_app(db);
        args
    }

    /// Returns teh base type of this type.
    /// ## Example
    /// `TyApp<Adt, i32>` returns `Adt`.
    /// `TyApp<TyParam<T>, i32>` returns `TyParam<T>`.
    pub fn base_ty(self, db: &'db dyn HirAnalysisDb) -> Self {
        self.decompose_ty_app(db).0
    }

    /// Returns the type of const type if the type is a const type.
    pub fn const_ty_ty(self, db: &'db dyn HirAnalysisDb) -> Option<Self> {
        match self.data(db) {
            TyData::ConstTy(const_ty) => Some(const_ty.ty(db)),
            _ => None,
        }
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

    /// Returns `true` if the type is a never type.
    pub fn is_never(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.data(db), TyData::Never)
    }

    /// Returns `IngotId` that declares the type.
    pub fn ingot(self, db: &'db dyn HirAnalysisDb) -> Option<IngotId<'db>> {
        match self.data(db) {
            TyData::TyBase(TyBase::Adt(adt)) => adt.ingot(db).into(),
            TyData::TyBase(TyBase::Func(def)) => def.ingot(db).into(),
            TyData::TyApp(lhs, _) => lhs.ingot(db),
            _ => None,
        }
    }

    pub fn invalid_cause(self, db: &'db dyn HirAnalysisDb) -> Option<InvalidCause<'db>> {
        match self.data(db) {
            TyData::Invalid(cause) => Some(cause.clone()),
            _ => None,
        }
    }

    pub fn flags(self, db: &dyn HirAnalysisDb) -> TyFlags {
        ty_flags(db, self)
    }

    pub fn has_invalid(self, db: &dyn HirAnalysisDb) -> bool {
        self.flags(db).contains(TyFlags::HAS_INVALID)
    }

    pub fn has_param(self, db: &dyn HirAnalysisDb) -> bool {
        self.flags(db).contains(TyFlags::HAS_PARAM)
    }

    pub fn has_var(self, db: &dyn HirAnalysisDb) -> bool {
        self.flags(db).contains(TyFlags::HAS_VAR)
    }

    /// Returns `true` if the type has a `*` kind.
    pub fn has_star_kind(self, db: &dyn HirAnalysisDb) -> bool {
        !matches!(self.kind(db), Kind::Abs(_, _))
    }

    #[salsa::tracked(return_ref)]
    pub fn pretty_print(self, db: &'db dyn HirAnalysisDb) -> String {
        match self.data(db) {
            TyData::TyVar(var) => var.pretty_print(),
            TyData::TyParam(param) => param.pretty_print(db),
            TyData::TyApp(_, _) => pretty_print_ty_app(db, self),
            TyData::TyBase(ty_con) => ty_con.pretty_print(db),
            TyData::ConstTy(const_ty) => const_ty.pretty_print(db),
            TyData::Never => "!".to_string(),
            TyData::Invalid(..) => "<invalid>".to_string(),
        }
    }

    pub fn is_inherent_impl_allowed(self, db: &dyn HirAnalysisDb, ingot: IngotId) -> bool {
        if self.is_param(db) {
            return false;
        };

        let ty_ingot = self.ingot(db);
        match ingot.kind(db.as_hir_db()) {
            IngotKind::Core => ty_ingot.is_none() || ty_ingot == Some(ingot),
            _ => ty_ingot == Some(ingot),
        }
    }

    /// Decompose type application into the base type and type arguments, this
    /// doesn't perform deconstruction recursively. e.g.,
    /// `App(App(T, U), App(V, W))` -> `(T, [U, App(V, W)])`
    pub(super) fn decompose_ty_app(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> (TyId<'db>, &'db [TyId<'db>]) {
        let (base, args) = decompose_ty_app(db, self);
        (*base, args)
    }

    pub(super) fn ptr(db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Ptr)))
    }

    pub(super) fn tuple(db: &'db dyn HirAnalysisDb, n: usize) -> Self {
        Self::new(db, TyData::TyBase(TyBase::tuple(n)))
    }

    pub(super) fn tuple_with_elems(db: &'db dyn HirAnalysisDb, elems: &[TyId<'db>]) -> Self {
        let base = TyBase::tuple(elems.len());
        let mut ty = Self::new(db, TyData::TyBase(base));
        for &elem in elems {
            ty = Self::app(db, ty, elem);
        }
        ty
    }

    pub(super) fn bool(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Bool)))
    }

    pub(super) fn array(db: &'db dyn HirAnalysisDb, elem: TyId<'db>) -> Self {
        let base = TyBase::Prim(PrimTy::Array);
        let array = Self::new(db, TyData::TyBase(base));
        Self::app(db, array, elem)
    }

    pub(super) fn array_with_len(db: &'db dyn HirAnalysisDb, elem: TyId<'db>, len: usize) -> Self {
        let array = Self::array(db, elem);

        let len = EvaluatedConstTy::LitInt(IntegerId::new(db.as_hir_db(), len.into()));
        let len = ConstTyData::Evaluated(len, array.applicable_ty(db).unwrap().const_ty.unwrap());
        let len = TyId::const_ty(db, ConstTyId::new(db, len));

        TyId::app(db, array, len)
    }

    pub(super) fn unit(db: &'db dyn HirAnalysisDb) -> Self {
        Self::tuple(db, 0)
    }

    pub(super) fn never(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, TyData::Never)
    }

    pub(super) fn const_ty(db: &'db dyn HirAnalysisDb, const_ty: ConstTyId<'db>) -> Self {
        Self::new(db, TyData::ConstTy(const_ty))
    }

    pub(crate) fn adt(db: &'db dyn HirAnalysisDb, adt: AdtDef<'db>) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Adt(adt)))
    }

    pub(crate) fn func(db: &'db dyn HirAnalysisDb, func: FuncDef<'db>) -> Self {
        Self::new(db, TyData::TyBase(TyBase::Func(func)))
    }

    pub(crate) fn is_func(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::TyBase(TyBase::Func(_)))
    }

    pub(crate) fn is_trait_self(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::TyParam(ty_param) if ty_param.is_trait_self)
    }

    pub(crate) fn is_ty_var(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::TyVar(_))
    }

    pub(crate) fn is_const_ty(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::ConstTy(_))
    }

    pub(crate) fn is_tuple(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(
            self.base_ty(db).data(db),
            TyData::TyBase(TyBase::Prim(PrimTy::Tuple(_)))
        )
    }

    pub(crate) fn is_array(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(
            self.base_ty(db).data(db),
            TyData::TyBase(TyBase::Prim(PrimTy::Array))
        )
    }

    pub(crate) fn is_string(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(
            self.base_ty(db).data(db),
            TyData::TyBase(TyBase::Prim(PrimTy::String))
        )
    }

    pub(crate) fn is_param(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::TyParam(_))
    }

    /// Returns `true` if the base type is a user defined `struct` type.
    pub(crate) fn is_struct(self, db: &dyn HirAnalysisDb) -> bool {
        self.as_struct(db).is_some()
    }

    pub(crate) fn as_struct(self, db: &'db dyn HirAnalysisDb) -> Option<Struct<'db>> {
        let base_ty = self.base_ty(db);
        let AdtRef::Struct(s) = base_ty.adt_ref(db)?.data(db) else {
            return None;
        };
        Some(s)
    }

    pub fn is_prim(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.base_ty(db).data(db), TyData::TyBase(TyBase::Prim(_)))
    }

    /// Returns `true` if the base type is a user defined `enum` type.
    pub(crate) fn as_enum(self, db: &'db dyn HirAnalysisDb) -> Option<Enum<'db>> {
        let base_ty = self.base_ty(db);
        if_chain! {
            if let Some(adt_ref) = base_ty.adt_ref(db);
            if let AdtRef::Enum(enum_) = adt_ref.data(db);
            then {
                Some(enum_)
            } else {
                None
            }
        }
    }

    pub(crate) fn as_scope(self, db: &'db dyn HirAnalysisDb) -> Option<ScopeId<'db>> {
        match self.base_ty(db).data(db) {
            TyData::TyParam(param) => Some(param.scope(db)),
            TyData::TyBase(TyBase::Adt(adt)) => Some(adt.scope(db)),
            TyData::TyBase(TyBase::Func(func)) => Some(func.scope(db)),
            TyData::TyBase(TyBase::Prim(..)) => None,
            TyData::ConstTy(const_ty) => match const_ty.data(db) {
                ConstTyData::TyVar(..) => None,
                ConstTyData::TyParam(ty_param, _) => Some(ty_param.scope(db)),
                ConstTyData::Evaluated(..) => None,
                ConstTyData::UnEvaluated(body) => Some(body.scope()),
            },

            TyData::Never | TyData::Invalid(_) | TyData::TyVar(_) => None,
            TyData::TyApp(..) => unreachable!(),
        }
    }

    /// Returns the span of the name of the type, at its definition site
    pub fn name_span(self, db: &'db dyn HirAnalysisDb) -> Option<DynLazySpan<'db>> {
        match self.base_ty(db).data(db) {
            TyData::TyVar(_) => None,
            TyData::TyParam(param) => param.scope(db).name_span(db.as_hir_db()),

            TyData::TyBase(TyBase::Adt(adt)) => Some(adt.name_span(db)),
            TyData::TyBase(TyBase::Func(func)) => Some(func.name_span(db)),
            TyData::TyBase(TyBase::Prim(_)) => None,

            TyData::ConstTy(ty) => match ty.data(db) {
                ConstTyData::TyParam(param, _) => param.scope(db).name_span(db.as_hir_db()),
                _ => None,
            },

            TyData::Never | TyData::Invalid(_) => None,
            TyData::TyApp(..) => unreachable!(),
        }
    }

    /// Emit diagnostics for the type if the type contains invalid types.
    pub(super) fn emit_diag(
        self,
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
    ) -> Option<TyDiagCollection<'db>> {
        struct EmitDiagVisitor<'db> {
            db: &'db dyn HirAnalysisDb,
            diag: Option<TyDiagCollection<'db>>,
            span: DynLazySpan<'db>,
        }

        impl<'db> TyVisitor<'db> for EmitDiagVisitor<'db> {
            fn db(&self) -> &'db dyn HirAnalysisDb {
                self.db
            }

            fn visit_invalid(&mut self, cause: &InvalidCause<'db>) {
                let span = self.span.clone();
                let diag = match cause.clone() {
                    InvalidCause::NotFullyApplied => TyLowerDiag::ExpectedStarKind(span).into(),

                    InvalidCause::KindMismatch { expected, given } => {
                        TyLowerDiag::InvalidTypeArgKind {
                            span,
                            expected,
                            given,
                        }
                        .into()
                    }

                    InvalidCause::TooManyGenericArgs { expected, given } => {
                        TyLowerDiag::TooManyGenericArgs {
                            span,
                            expected,
                            given,
                        }
                        .into()
                    }

                    InvalidCause::InvalidConstParamTy => {
                        TyLowerDiag::InvalidConstParamTy(span).into()
                    }

                    InvalidCause::RecursiveConstParamTy => {
                        TyLowerDiag::RecursiveConstParamTy(span).into()
                    }

                    InvalidCause::ConstTyMismatch { expected, given } => {
                        TyLowerDiag::ConstTyMismatch {
                            span,
                            expected,
                            given,
                        }
                        .into()
                    }

                    InvalidCause::ConstTyExpected { expected } => {
                        TyLowerDiag::ConstTyExpected { span, expected }.into()
                    }

                    InvalidCause::NormalTypeExpected { given } => {
                        TyLowerDiag::NormalTypeExpected { span, given }.into()
                    }

                    InvalidCause::UnboundTypeAliasParam {
                        alias,
                        n_given_args,
                    } => TyLowerDiag::UnboundTypeAliasParam {
                        span,
                        alias,
                        n_given_args,
                    }
                    .into(),

                    InvalidCause::AssocTy => TyLowerDiag::AssocTy(span).into(),

                    InvalidCause::InvalidConstTyExpr { body } => {
                        TyLowerDiag::InvalidConstTyExpr(body.lazy_span().into()).into()
                    }

                    InvalidCause::Other => return,
                };

                self.diag.get_or_insert(diag);
            }
        }

        if !self.has_invalid(db) {
            return None;
        }

        let mut visitor = EmitDiagVisitor {
            db,
            diag: None,
            span,
        };

        visitor.visit_ty(self);
        visitor.diag
    }

    pub(super) fn emit_wf_diag(
        self,
        db: &'db dyn HirAnalysisDb,
        ingot: IngotId<'db>,
        assumptions: PredicateListId<'db>,
        span: DynLazySpan<'db>,
    ) -> Option<TyDiagCollection<'db>> {
        if let WellFormedness::IllFormed { goal, subgoal } =
            check_ty_wf(db, ingot, self, assumptions)
        {
            Some(
                TraitConstraintDiag::TraitBoundNotSat {
                    span,
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                }
                .into(),
            )
        } else {
            None
        }
    }

    pub(super) fn ty_var(
        db: &'db dyn HirAnalysisDb,
        sort: TyVarSort,
        kind: Kind,
        key: InferenceKey<'db>,
    ) -> Self {
        Self::new(db, TyData::TyVar(TyVar { sort, kind, key }))
    }

    pub(super) fn const_ty_var(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        key: InferenceKey<'db>,
    ) -> Self {
        let ty_var = TyVar {
            sort: TyVarSort::General,
            kind: ty.kind(db).clone(),
            key,
        };

        let data = ConstTyData::TyVar(ty_var, ty);
        Self::new(db, TyData::ConstTy(ConstTyId::new(db, data)))
    }

    /// Perform type level application.
    pub(crate) fn app(db: &'db dyn HirAnalysisDb, lhs: Self, rhs: Self) -> TyId<'db> {
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

    /// Folds over a series of type applications from left to right.
    ///
    /// For example, given base type B and arg types [A1, A2, A3],
    /// foldl would produce ((B A1) A2) A3).
    pub fn foldl(db: &'db dyn HirAnalysisDb, mut base: Self, args: &[Self]) -> Self {
        for (i, arg) in args.iter().enumerate() {
            if base.applicable_ty(db).is_some() {
                base = Self::app(db, base, *arg);
            } else {
                return Self::invalid(
                    db,
                    InvalidCause::TooManyGenericArgs {
                        expected: i,
                        given: args.len(),
                    },
                );
            }
        }
        base
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

    pub fn invalid(db: &'db dyn HirAnalysisDb, cause: InvalidCause<'db>) -> Self {
        Self::new(db, TyData::Invalid(cause))
    }

    pub(crate) fn from_hir_prim_ty(db: &'db dyn HirAnalysisDb, hir_prim: HirPrimTy) -> Self {
        Self::new(db, TyData::TyBase(hir_prim.into()))
    }

    pub(super) fn const_ty_param(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        if let TyData::ConstTy(const_ty) = self.data(db) {
            Some(const_ty.ty(db))
        } else {
            None
        }
    }

    pub(super) fn evaluate_const_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        expected_ty: Option<TyId<'db>>,
    ) -> Result<TyId<'db>, InvalidCause<'db>> {
        match (expected_ty, self.data(db)) {
            (Some(expected_const_ty), TyData::ConstTy(const_ty)) => {
                if expected_const_ty.has_invalid(db) {
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
                if expected_const_ty.has_invalid(db) {
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
    pub fn applicable_ty(self, db: &'db dyn HirAnalysisDb) -> Option<ApplicableTyProp<'db>> {
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
pub struct ApplicableTyProp<'db> {
    /// A kind of the applicable type.
    pub kind: Kind,
    /// An expected type of const type if the applicable type is a const type.
    pub const_ty: Option<TyId<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyData<'db> {
    /// Type variable.
    TyVar(TyVar<'db>),

    /// Type Parameter.
    TyParam(TyParam<'db>),

    // Type application,
    // e.g., `Option<i32>` is represented as `TApp(TyConst(Option), TyConst(i32))`.
    TyApp(TyId<'db>, TyId<'db>),

    /// A concrete type, e.g., `i32`, `u32`, `bool`, `String`, `Result` etc.
    TyBase(TyBase<'db>),

    ConstTy(ConstTyId<'db>),

    /// A never(bottom) type.
    Never,

    // Invalid type which means the type is ill-formed.
    // This type can be unified with any other types.
    // NOTE: For type soundness check in this level, we don't consider trait satisfiability.
    Invalid(InvalidCause<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InvalidCause<'db> {
    /// Type is not fully applied where it is required.
    NotFullyApplied,

    /// Kind mismatch between two types.
    KindMismatch {
        expected: Option<Kind>,
        given: TyId<'db>,
    },

    TooManyGenericArgs {
        expected: usize,
        given: usize,
    },

    InvalidConstParamTy,

    RecursiveConstParamTy,

    /// The given type doesn't match the expected const type.
    ConstTyMismatch {
        expected: TyId<'db>,
        given: TyId<'db>,
    },

    /// The given type is not a const type where it is required.
    ConstTyExpected {
        expected: TyId<'db>,
    },

    /// The given type is const type where it is *NOT* required.
    NormalTypeExpected {
        given: TyId<'db>,
    },

    /// Type alias parameter is not bound.
    /// NOTE: In our type system, type alias is a macro, so we can't perform
    /// partial application to type alias.
    UnboundTypeAliasParam {
        alias: HirTypeAlias<'db>,
        n_given_args: usize,
    },

    /// Associated Type is not allowed at the moment.
    AssocTy,

    // The given expression is not supported yet in the const type context.
    // TODO: Remove this error kind and introduce a new error kind for more specific cause when
    // type inference is implemented.
    InvalidConstTyExpr {
        body: Body<'db>,
    },

    // TraitConstraintNotSat(PredicateId),
    /// `Other` indicates the cause is already reported in other analysis
    /// passes, e.g., parser or name resolution.
    Other,
}

impl<'db> InvalidCause<'db> {
    pub(super) fn kind_mismatch(expected: Option<&Kind>, ty: TyId<'db>) -> Self {
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

    pub fn does_match(&self, other: &Self) -> bool {
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
pub struct TyVar<'db> {
    pub sort: TyVarSort,
    pub kind: Kind,
    pub(super) key: InferenceKey<'db>,
}

impl std::cmp::PartialOrd for TyVar<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl std::cmp::Ord for TyVar<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self == other {
            return std::cmp::Ordering::Equal;
        }
        self.key.cmp(&other.key)
    }
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

impl TyVar<'_> {
    pub(super) fn pretty_print(&self) -> String {
        match self.sort {
            TyVarSort::General => ("_").to_string(),
            TyVarSort::Integral => "{integer}".to_string(),
            TyVarSort::String(n) => format!("String<{}>", n).to_string(),
        }
    }
}

/// Type generics parameter. We also treat `Self` type in a trait definition as
/// a special type parameter.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyParam<'db> {
    pub name: IdentId<'db>,
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
    pub owner: ScopeId<'db>,
}

impl<'db> TyParam<'db> {
    pub(super) fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        self.name.data(db.as_hir_db()).to_string()
    }

    pub(super) fn normal_param(
        name: IdentId<'db>,
        idx: usize,
        kind: Kind,
        scope: ScopeId<'db>,
    ) -> Self {
        Self {
            name,
            idx,
            kind,
            is_trait_self: false,
            owner: scope,
        }
    }

    pub(super) fn trait_self(db: &'db dyn HirAnalysisDb, kind: Kind, scope: ScopeId<'db>) -> Self {
        Self {
            name: IdentId::make_self_ty(db.as_hir_db()),
            idx: 0,
            kind,
            is_trait_self: true,
            owner: scope,
        }
    }

    pub fn original_idx(&self, db: &'db dyn HirAnalysisDb) -> usize {
        let owner = GenericParamOwnerId::from_item_opt(db, self.owner.item()).unwrap();
        let param_set = collect_generic_params(db, owner);
        let offset = param_set.offset_to_explicit_params_position(db);

        // TyParam.idx includes implicit params, subtract offset to get original idx
        self.idx - offset
    }

    pub fn scope(&self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        if self.is_trait_self {
            self.owner
        } else {
            ScopeId::GenericParam(self.owner.item(), self.original_idx(db))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub enum TyBase<'db> {
    Prim(PrimTy),
    Adt(AdtDef<'db>),
    Func(FuncDef<'db>),
}

impl<'db> TyBase<'db> {
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
                PrimTy::Usize => "usize",
                PrimTy::I8 => "i8",
                PrimTy::I16 => "i16",
                PrimTy::I32 => "i32",
                PrimTy::I64 => "i64",
                PrimTy::I128 => "i128",
                PrimTy::I256 => "i256",
                PrimTy::Isize => "isize",
                PrimTy::String => "String",
                PrimTy::Array => "[]",
                PrimTy::Tuple(_) => "()",
                PrimTy::Ptr => "*",
            }
            .to_string(),

            Self::Adt(adt) => adt.name(db).data(db.as_hir_db()).to_string(),

            Self::Func(func) => format!("fn {}", func.name(db).data(db.as_hir_db())),
        }
    }

    pub(super) fn adt(self) -> Option<AdtDef<'db>> {
        match self {
            Self::Adt(adt) => Some(adt),
            _ => None,
        }
    }
}

impl From<HirPrimTy> for TyBase<'_> {
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
                HirIntTy::Isize => Self::Prim(PrimTy::Isize),
            },

            HirPrimTy::Uint(uint_ty) => match uint_ty {
                HirUintTy::U8 => Self::Prim(PrimTy::U8),
                HirUintTy::U16 => Self::Prim(PrimTy::U16),
                HirUintTy::U32 => Self::Prim(PrimTy::U32),
                HirUintTy::U64 => Self::Prim(PrimTy::U64),
                HirUintTy::U128 => Self::Prim(PrimTy::U128),
                HirUintTy::U256 => Self::Prim(PrimTy::U256),
                HirUintTy::Usize => Self::Prim(PrimTy::Usize),
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
    Usize,
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
    Isize,
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
                | Self::Usize
                | Self::I8
                | Self::I16
                | Self::I32
                | Self::I64
                | Self::I128
                | Self::I256
                | Self::Isize
        )
    }

    pub fn is_bool(self) -> bool {
        matches!(self, Self::Bool)
    }
}

pub(super) trait HasKind {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind;
}

impl HasKind for TyData<'_> {
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

            TyData::Never => Kind::Any,

            TyData::Invalid(_) => Kind::Any,
        }
    }
}

impl HasKind for TyVar<'_> {
    fn kind(&self, _db: &dyn HirAnalysisDb) -> Kind {
        self.kind.clone()
    }
}

impl HasKind for TyBase<'_> {
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

impl HasKind for AdtDef<'_> {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        let mut kind = Kind::Star;
        for param in self.params(db).iter().rev() {
            kind = Kind::abs(param.kind(db).clone(), kind);
        }

        kind
    }
}

impl HasKind for FuncDef<'_> {
    fn kind(&self, db: &dyn HirAnalysisDb) -> Kind {
        let mut kind = Kind::Star;
        for param in self.params(db).iter().rev() {
            kind = Kind::abs(param.kind(db).clone(), kind);
        }

        kind
    }
}

pub(crate) fn collect_variables<'db, V>(
    db: &'db dyn HirAnalysisDb,
    visitable: &V,
) -> IndexSet<TyVar<'db>>
where
    V: TyVisitable<'db>,
{
    struct TyVarCollector<'db> {
        db: &'db dyn HirAnalysisDb,
        vars: IndexSet<TyVar<'db>>,
    }

    impl<'db> TyVisitor<'db> for TyVarCollector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_var(&mut self, var: &TyVar<'db>) {
            self.vars.insert(var.clone());
        }
    }
    let mut collector = TyVarCollector {
        db,
        vars: IndexSet::default(),
    };

    visitable.visit_with(&mut collector);

    collector.vars
}

pub(crate) fn inference_keys<'db, V>(
    db: &'db dyn HirAnalysisDb,
    visitable: &V,
) -> FxHashSet<InferenceKey<'db>>
where
    V: TyVisitable<'db>,
{
    struct FreeInferenceKeyCollector<'db> {
        db: &'db dyn HirAnalysisDb,
        keys: FxHashSet<InferenceKey<'db>>,
    }

    impl<'db> TyVisitor<'db> for FreeInferenceKeyCollector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_var(&mut self, var: &TyVar<'db>) {
            self.keys.insert(var.key);
        }
    }

    let mut collector = FreeInferenceKeyCollector {
        db,
        keys: FxHashSet::default(),
    };

    visitable.visit_with(&mut collector);
    collector.keys
}

fn pretty_print_ty_app<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> String {
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
pub(crate) fn decompose_ty_app<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> (TyId<'db>, Vec<TyId<'db>>) {
    struct TyAppDecomposer<'db> {
        db: &'db dyn HirAnalysisDb,
        base: Option<TyId<'db>>,
        args: Vec<TyId<'db>>,
    }

    impl<'db> TyVisitor<'db> for TyAppDecomposer<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
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

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct TyFlags: u32 {
        const HAS_INVALID =  0b0000_0001;
        const HAS_VAR = 0b0000_0010;
        const HAS_PARAM = 0b0000_0100;
    }
}

#[salsa::tracked]
pub(crate) fn ty_flags<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyFlags {
    struct Collector<'db> {
        db: &'db dyn HirAnalysisDb,
        flags: TyFlags,
    }

    impl<'db> TyVisitor<'db> for Collector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_var(&mut self, _: &TyVar) {
            self.flags.insert(TyFlags::HAS_VAR);
        }

        fn visit_param(&mut self, _: &TyParam) {
            self.flags.insert(TyFlags::HAS_PARAM)
        }

        fn visit_invalid(&mut self, _: &InvalidCause) {
            self.flags.insert(TyFlags::HAS_INVALID)
        }
    }

    let mut collector = Collector {
        db,
        flags: TyFlags::empty(),
    };

    ty.visit_with(&mut collector);
    collector.flags
}
