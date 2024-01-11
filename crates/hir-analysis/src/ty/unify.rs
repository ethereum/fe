//! This module contains the unification table for type inference and trait
//! satisfiability checking.

use ena::unify::{InPlace, InPlaceUnificationTable, UnifyKey, UnifyValue};

use super::{
    trait_def::{Implementor, TraitInstId},
    ty_def::{Kind, Subst, TyData, TyId, TyVar, TyVarUniverse},
};
use crate::{ty::dependent_ty::DependentTyData, HirAnalysisDb};

pub(crate) struct UnificationTable<'db> {
    db: &'db dyn HirAnalysisDb,
    table: InPlaceUnificationTable<InferenceKey>,
}

pub type Snapshot = ena::unify::Snapshot<InPlace<InferenceKey>>;

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

    pub fn unify<T>(&mut self, lhs: T, rhs: T) -> bool
    where
        T: Unifiable,
    {
        let snapshot = self.snapshot();
        if lhs.unify(self, rhs) {
            self.table.commit(snapshot);
            true
        } else {
            self.rollback_to(snapshot);
            false
        }
    }

    /// Returns true if the two types were unified.
    /// This method doesn't roll back the unification table. Please refer to
    /// `unify`[Self::unify] if you need to roll back the table automatically
    /// when unification fails.
    fn unify_ty(&mut self, ty1: TyId, ty2: TyId) -> bool {
        if !ty1.kind(self.db).does_match(ty2.kind(self.db)) {
            return false;
        }
        let ty1 = self.apply(self.db, ty1);
        let ty2 = self.apply(self.db, ty2);

        match (ty1.data(self.db), ty2.data(self.db)) {
            (TyData::TyVar(_), TyData::TyVar(_)) => self.unify_var_var(ty1, ty2),

            (TyData::TyVar(var), _) => self.unify_var_value(var, ty2),

            (_, TyData::TyVar(var)) => self.unify_var_value(var, ty1),

            (TyData::TyApp(ty1_1, ty1_2), TyData::TyApp(ty2_1, ty2_2)) => {
                let ok = self.unify_ty(*ty1_1, *ty2_1);
                if ok {
                    let ty1_2 = self.apply(self.db, *ty1_2);
                    let ty2_2 = self.apply(self.db, *ty2_2);
                    self.unify_ty(ty1_2, ty2_2)
                } else {
                    false
                }
            }

            (TyData::TyParam(_), TyData::TyParam(_)) | (TyData::TyBase(_), TyData::TyBase(_)) => {
                ty1 == ty2
            }

            (TyData::Invalid(_), _) | (_, TyData::Invalid(_)) => true,

            (TyData::DependentTy(dep_ty1), TyData::DependentTy(dep_ty2)) => {
                if !self.unify_ty(dep_ty1.ty(self.db), dep_ty2.ty(self.db)) {
                    return false;
                }

                match (dep_ty1.data(self.db), dep_ty2.data(self.db)) {
                    (DependentTyData::TyVar(..), DependentTyData::TyVar(..)) => {
                        self.unify_var_var(ty1, ty2)
                    }

                    (DependentTyData::TyVar(var, _), _) => self.unify_var_value(var, ty2),

                    (_, DependentTyData::TyVar(var, _)) => self.unify_var_value(var, ty1),

                    (DependentTyData::Evaluated(val1, _), DependentTyData::Evaluated(val2, _)) => {
                        val1 == val2
                    }

                    _ => false,
                }
            }

            _ => false,
        }
    }

    pub fn new_var(&mut self, universe: TyVarUniverse, kind: &Kind) -> TyId {
        let key = self.new_key(kind);
        TyId::ty_var(self.db, universe, kind.clone(), key)
    }

    pub(super) fn new_var_from_param(&mut self, db: &dyn HirAnalysisDb, ty: TyId) -> TyId {
        match ty.data(db) {
            TyData::TyParam(param) => {
                let key = self.new_key(&param.kind);
                let universe = TyVarUniverse::General;
                TyId::ty_var(db, universe, param.kind.clone(), key)
            }

            TyData::DependentTy(dep_ty) => {
                if let DependentTyData::TyParam(_, ty) = dep_ty.data(db) {
                    let key = self.new_key(ty.kind(db));
                    TyId::dependent_ty_var(db, *ty, key)
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
    /// Returns `true` if the two variables are unifiable.
    ///
    /// When the two variables are in the same universe, we can just unify them.
    ///
    /// When the two variables are *NOT* in the same universe, a type variable
    /// that has a broader universe are narrowed down to the narrower one.
    ///
    /// NOTE: This assumes that we have only two universes: General and Int.
    fn unify_var_var(&mut self, ty_var1: TyId, ty_var2: TyId) -> bool {
        let (var1, var2) = match (ty_var1.data(self.db), ty_var2.data(self.db)) {
            (TyData::TyVar(var1), TyData::TyVar(var2)) => (var1, var2),
            (TyData::DependentTy(dep_ty1), TyData::DependentTy(dep_ty2)) => {
                match (dep_ty1.data(self.db), dep_ty2.data(self.db)) {
                    (DependentTyData::TyVar(var1, _), DependentTyData::TyVar(var2, _)) => {
                        (var1, var2)
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(),
        };

        if var1.universe == var2.universe {
            self.table.unify_var_var(var1.key, var2.key).is_ok()
        } else if matches!(var1.universe, TyVarUniverse::General) {
            // Narrow down the universe of var1 to Int.
            self.table
                .unify_var_value(var1.key, InferenceValue::Bound(ty_var2))
                .is_ok()
        } else {
            // Narrow down the universe of var2 to Int.
            self.table
                .unify_var_value(var2.key, InferenceValue::Bound(ty_var1))
                .is_ok()
        }
    }

    /// Try to unify a type variable to a type.
    /// We perform the following checks:
    /// 1. Occurrence check: The same type variable must not occur in the type.
    /// 2. Universe check: The universe of the type variable must match the
    ///    universe of the type.
    fn unify_var_value(&mut self, var: &TyVar, value: TyId) -> bool {
        if value.free_inference_keys(self.db).contains(&var.key) {
            return false;
        }

        match (var.universe, value.is_integral(self.db)) {
            (TyVarUniverse::General, _) => self
                .table
                .unify_var_value(var.key, InferenceValue::Bound(value))
                .is_ok(),

            (TyVarUniverse::Integral, true) => self
                .table
                .unify_var_value(var.key, InferenceValue::Bound(value))
                .is_ok(),

            _ => false,
        }
    }
}

impl<'db> Subst for UnificationTable<'db> {
    fn get(&mut self, ty: TyId) -> Option<TyId> {
        match ty.data(self.db) {
            TyData::TyVar(var) => self.probe(var.key),
            TyData::DependentTy(dep_ty) => {
                if let DependentTyData::TyVar(var, _) = dep_ty.data(self.db) {
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
    type Error = ();

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
                    Err(())
                } else {
                    Ok(InferenceValue::Bound(*ty1))
                }
            }
        }
    }
}

pub(crate) trait Unifiable {
    fn unify(self, table: &mut UnificationTable, other: Self) -> bool;
}

impl Unifiable for TyId {
    fn unify(self, table: &mut UnificationTable, other: Self) -> bool {
        table.unify_ty(self, other)
    }
}

impl Unifiable for TraitInstId {
    fn unify(self, table: &mut UnificationTable, other: Self) -> bool {
        let db = table.db;
        if self.def(db) != other.def(db) {
            return false;
        }

        for (&self_arg, &other_arg) in self.substs(db).iter().zip(other.substs(db)) {
            if !table.unify_ty(self_arg, other_arg) {
                return false;
            }
        }

        true
    }
}

impl Unifiable for Implementor {
    fn unify(self, table: &mut UnificationTable, other: Self) -> bool {
        let db = table.db;
        if !table.unify(self.trait_(db), other.trait_(db)) {
            return false;
        }

        table.unify(self.ty(db), other.ty(db))
    }
}

pub trait Foo<const N: usize> {}
