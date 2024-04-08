//! This module contains the unification table for type inference and trait
//! satisfiability checking.

use ena::unify::{InPlace, InPlaceUnificationTable, UnifyKey, UnifyValue};
use num_bigint::BigUint;

use super::{
    trait_def::{Implementor, TraitInstId},
    ty_def::{Kind, Subst, TyData, TyId, TyVar, TyVarSort},
};
use crate::{
    ty::const_ty::{ConstTyData, EvaluatedConstTy},
    HirAnalysisDb,
};

pub(crate) struct UnificationTable<'db> {
    db: &'db dyn HirAnalysisDb,
    table: InPlaceUnificationTable<InferenceKey>,
}

pub type Snapshot = ena::unify::Snapshot<InPlace<InferenceKey>>;
pub type UnificationResult = Result<(), UnificationError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnificationError {
    OccursCheckFailed,
    TypeMismatch,
}

impl<'db> UnificationTable<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            db,
            table: InPlaceUnificationTable::new(),
        }
    }

    pub fn rollback_to(&mut self, snapshot: Snapshot) {
        self.table.rollback_to(snapshot);
    }

    pub fn snapshot(&mut self) -> Snapshot {
        self.table.snapshot()
    }

    pub fn unify<T>(&mut self, lhs: T, rhs: T) -> UnificationResult
    where
        T: Unifiable,
    {
        let snapshot = self.snapshot();
        match lhs.unify(self, rhs) {
            Ok(()) => {
                self.table.commit(snapshot);
                Ok(())
            }
            Err(err) => {
                self.rollback_to(snapshot);
                Err(err)
            }
        }
    }

    /// Returns `Ok()` if the two types were unified, otherwise returns an
    /// error. This method doesn't roll back the unification table. Please
    /// refer to `unify`[Self::unify] if you need to roll back the table
    /// automatically when unification fails.
    fn unify_ty(&mut self, ty1: TyId, ty2: TyId) -> UnificationResult {
        if !ty1.kind(self.db).does_match(ty2.kind(self.db)) {
            return Err(UnificationError::TypeMismatch);
        }

        let ty1 = self.apply(self.db, ty1);
        let ty2 = self.apply(self.db, ty2);

        match (ty1.data(self.db), ty2.data(self.db)) {
            (TyData::TyVar(_), TyData::TyVar(_)) => self.unify_var_var(ty1, ty2),

            (TyData::TyVar(var), _) => self.unify_var_value(var, ty2),

            (_, TyData::TyVar(var)) => self.unify_var_value(var, ty1),

            (TyData::TyApp(ty1_1, ty1_2), TyData::TyApp(ty2_1, ty2_2)) => {
                self.unify_ty(*ty1_1, *ty2_1)?;
                let ty1_2 = self.apply(self.db, *ty1_2);
                let ty2_2 = self.apply(self.db, *ty2_2);
                self.unify_ty(ty1_2, ty2_2)
            }

            (TyData::TyParam(_), TyData::TyParam(_)) | (TyData::TyBase(_), TyData::TyBase(_)) => {
                if ty1 == ty2 {
                    Ok(())
                } else {
                    Err(UnificationError::TypeMismatch)
                }
            }

            (TyData::Invalid(_), _)
            | (_, TyData::Invalid(_))
            | (TyData::Bot, _)
            | (_, TyData::Bot) => Ok(()),

            (TyData::ConstTy(const_ty1), TyData::ConstTy(const_ty2)) => {
                self.unify_ty(const_ty1.ty(self.db), const_ty2.ty(self.db))?;

                match (const_ty1.data(self.db), const_ty2.data(self.db)) {
                    (ConstTyData::TyVar(..), ConstTyData::TyVar(..)) => {
                        self.unify_var_var(ty1, ty2)
                    }

                    (ConstTyData::TyVar(var, _), _) => self.unify_var_value(var, ty2),

                    (_, ConstTyData::TyVar(var, _)) => self.unify_var_value(var, ty1),

                    (ConstTyData::Evaluated(val1, _), ConstTyData::Evaluated(val2, _)) => {
                        if val1 == val2 {
                            Ok(())
                        } else {
                            Err(UnificationError::TypeMismatch)
                        }
                    }

                    _ => Err(UnificationError::TypeMismatch),
                }
            }

            _ => Err(UnificationError::TypeMismatch),
        }
    }

    pub fn new_var(&mut self, sort: TyVarSort, kind: &Kind) -> TyId {
        let key = self.new_key(kind);
        TyId::ty_var(self.db, sort, kind.clone(), key)
    }

    pub(super) fn new_var_from_param(&mut self, ty: TyId) -> TyId {
        match ty.data(self.db) {
            TyData::TyParam(param) => {
                let key = self.new_key(&param.kind);
                let sort = TyVarSort::General;
                TyId::ty_var(self.db, sort, param.kind.clone(), key)
            }

            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyParam(_, ty) = const_ty.data(self.db) {
                    let key = self.new_key(ty.kind(self.db));
                    TyId::const_ty_var(self.db, *ty, key)
                } else {
                    panic!()
                }
            }
            _ => panic!(),
        }
    }

    pub fn new_key(&mut self, kind: &Kind) -> InferenceKey {
        self.table.new_key(InferenceValue::Unbound(kind.clone()))
    }

    fn probe(&mut self, key: InferenceKey) -> Option<TyId> {
        match self.table.probe_value(key) {
            InferenceValue::Bound(ty) => Some(ty),
            InferenceValue::Unbound(_) => None,
        }
    }

    /// Try to unify two type variables.
    ///
    /// When the two variables are in the same sort, we can just unify them.
    ///
    /// When the two variables are *NOT* in the same sort, a type variable
    /// that has a broader sort are narrowed down to the narrower one.
    ///
    /// NOTE: This assumes that we have only two sorts: General and Int.
    fn unify_var_var(&mut self, ty_var1: TyId, ty_var2: TyId) -> UnificationResult {
        let (var1, var2) = match (ty_var1.data(self.db), ty_var2.data(self.db)) {
            (TyData::TyVar(var1), TyData::TyVar(var2)) => (var1, var2),
            (TyData::ConstTy(const_ty1), TyData::ConstTy(const_ty2)) => {
                match (const_ty1.data(self.db), const_ty2.data(self.db)) {
                    (ConstTyData::TyVar(var1, _), ConstTyData::TyVar(var2, _)) => (var1, var2),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        };

        match (var1.sort, var2.sort) {
            (sort1, sort2) if sort1 == sort2 => self.table.unify_var_var(var1.key, var2.key),

            (TyVarSort::General, _) => self
                .table
                .unify_var_value(var1.key, InferenceValue::Bound(ty_var2)),

            (_, TyVarSort::General) => self
                .table
                .unify_var_value(var2.key, InferenceValue::Bound(ty_var1)),

            (TyVarSort::String(n1), TyVarSort::String(n2)) => {
                if n1 > n2 {
                    self.table
                        .unify_var_value(var2.key, InferenceValue::Bound(ty_var1))
                } else {
                    self.table
                        .unify_var_value(var1.key, InferenceValue::Bound(ty_var2))
                }
            }

            (_, _) => Err(UnificationError::TypeMismatch),
        }
    }

    /// Try to unify a type variable to a type.
    /// We perform the following checks:
    /// 1. Occurrence check: The same type variable must not occur in the type.
    /// 2. Universe check: The sort of the type variable must match the sort of
    ///    the type.
    fn unify_var_value(&mut self, var: &TyVar, value: TyId) -> UnificationResult {
        if value.free_inference_keys(self.db).contains(&var.key) {
            return Err(UnificationError::OccursCheckFailed);
        }

        if value.contains_invalid(self.db) {
            return self
                .table
                .unify_var_value(var.key, InferenceValue::Bound(value));
        }

        match var.sort {
            TyVarSort::General => self
                .table
                .unify_var_value(var.key, InferenceValue::Bound(value)),

            TyVarSort::Integral => {
                if value.is_integral(self.db) {
                    self.table
                        .unify_var_value(var.key, InferenceValue::Bound(value))
                } else if value.is_bot(self.db) {
                    Ok(())
                } else {
                    Err(UnificationError::TypeMismatch)
                }
            }

            TyVarSort::String(n_var) => {
                let (base, args) = value.decompose_ty_app(self.db);

                if base.is_bot(self.db) {
                    return Ok(());
                }

                if !base.is_string(self.db) || args.len() != 1 {
                    return Err(UnificationError::TypeMismatch);
                }

                let TyData::ConstTy(const_ty) = args[0].data(self.db) else {
                    return Ok(());
                };

                let ConstTyData::Evaluated(EvaluatedConstTy::LitInt(n_value), _) =
                    const_ty.data(self.db)
                else {
                    return Ok(());
                };

                if &BigUint::from(n_var) <= n_value.data(self.db.as_hir_db()) {
                    self.table
                        .unify_var_value(var.key, InferenceValue::Bound(value))
                } else {
                    Err(UnificationError::TypeMismatch)
                }
            }
        }
    }
}

impl<'db> Subst for UnificationTable<'db> {
    fn get(&mut self, ty: TyId) -> Option<TyId> {
        match ty.data(self.db) {
            TyData::TyApp(lhs, rhs) => {
                let lhs = self.get(*lhs).unwrap_or(*lhs);
                let rhs = self.get(*rhs).unwrap_or(*rhs);
                Some(TyId::app(self.db, lhs, rhs))
            }
            TyData::TyVar(var) => {
                let ty = self.probe(var.key)?;
                Some(self.get(ty).unwrap_or(ty))
            }

            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyVar(var, _) = const_ty.data(self.db) {
                    self.probe(var.key)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InferenceKey(pub(super) u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InferenceValue {
    Bound(TyId),
    Unbound(Kind),
}

impl UnifyKey for InferenceKey {
    type Value = InferenceValue;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(idx: u32) -> Self {
        Self(idx)
    }

    fn tag() -> &'static str {
        "InferenceKey"
    }
}

impl UnifyValue for InferenceValue {
    type Error = UnificationError;

    fn unify_values(v1: &Self, v2: &Self) -> Result<Self, Self::Error> {
        match (v1, v2) {
            (InferenceValue::Unbound(k1), InferenceValue::Unbound(k2)) => {
                assert!(k1.does_match(k2));
                Ok(InferenceValue::Unbound(k1.clone()))
            }

            (InferenceValue::Unbound(_), InferenceValue::Bound(ty))
            | (InferenceValue::Bound(ty), InferenceValue::Unbound(_)) => {
                Ok(InferenceValue::Bound(*ty))
            }

            (InferenceValue::Bound(ty1), InferenceValue::Bound(ty2)) => {
                if ty1 != ty2 {
                    Err(UnificationError::TypeMismatch)
                } else {
                    Ok(InferenceValue::Bound(*ty1))
                }
            }
        }
    }
}

pub(crate) trait Unifiable {
    fn unify(self, table: &mut UnificationTable, other: Self) -> UnificationResult;
}

impl Unifiable for TyId {
    fn unify(self, table: &mut UnificationTable, other: Self) -> UnificationResult {
        table.unify_ty(self, other)
    }
}

impl Unifiable for TraitInstId {
    fn unify(self, table: &mut UnificationTable, other: Self) -> UnificationResult {
        let db = table.db;
        if self.def(db) != other.def(db) {
            return Err(UnificationError::TypeMismatch);
        }

        for (&self_arg, &other_arg) in self.substs(db).iter().zip(other.substs(db)) {
            table.unify_ty(self_arg, other_arg)?;
        }

        Ok(())
    }
}

impl Unifiable for Implementor {
    fn unify(self, table: &mut UnificationTable, other: Self) -> UnificationResult {
        let db = table.db;
        table.unify(self.trait_(db), other.trait_(db))?;
        table.unify(self.ty(db), other.ty(db))
    }
}
