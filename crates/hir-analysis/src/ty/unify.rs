use ena::unify::{InPlaceUnificationTable, NoError, UnifyKey, UnifyValue};

use crate::HirAnalysisDb;

use super::{
    trait_::{Implementor, TraitInstId},
    ty_def::{Kind, Subst, TyData, TyId, TyVar},
};

pub(crate) struct UnificationTable<'db> {
    db: &'db dyn HirAnalysisDb,
    table: InPlaceUnificationTable<InferenceKey>,
}

impl<'db> UnificationTable<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            db,
            table: InPlaceUnificationTable::new(),
        }
    }

    pub fn unify<T>(&mut self, lhs: T, rhs: T) -> bool
    where
        T: Unifiable,
    {
        let snapshot = self.table.snapshot();
        if lhs.unify(self, rhs) {
            self.table.commit(snapshot);
            true
        } else {
            self.table.rollback_to(snapshot);
            false
        }
    }

    /// Returns true if the two types were unified.
    /// This method doesn't roll back the unification table. Please refer to
    /// `unify`[Self::unify] if you need to roll back the table automatically
    /// when unification fails.
    fn unify_ty(&mut self, ty1: TyId, ty2: TyId) -> bool {
        if !ty1.kind(self.db).can_unify(ty2.kind(self.db)) {
            return false;
        }
        let ty1 = self.apply(self.db, ty1);
        let ty2 = self.apply(self.db, ty2);

        match (ty1.data(self.db), ty2.data(self.db)) {
            (TyData::TyVar(var1), TyData::TyVar(var2)) => {
                self.table.unify_var_var(var1.key, var2.key).is_ok()
            }

            (TyData::TyVar(var), _) if !ty2.free_inference_keys(self.db).contains(&var.key) => self
                .table
                .unify_var_value(var.key, InferenceValue::Bounded(ty2))
                .is_ok(),

            (_, TyData::TyVar(var)) if !ty1.free_inference_keys(self.db).contains(&var.key) => self
                .table
                .unify_var_value(var.key, InferenceValue::Bounded(ty1))
                .is_ok(),

            (TyData::TyApp(ty1_1, ty1_2), TyData::TyApp(ty2_1, ty2_2)) => {
                let ok = self.unify_ty(ty1_1, ty2_1);
                if ok {
                    let ty1_2 = self.apply(self.db, ty1_2);
                    let ty2_2 = self.apply(self.db, ty2_2);
                    self.unify_ty(ty1_2, ty2_2)
                } else {
                    false
                }
            }

            (TyData::TyParam(_), TyData::TyParam(_)) | (TyData::TyCon(_), TyData::TyCon(_)) => {
                ty1 == ty2
            }

            (TyData::Invalid(_), _) | (_, TyData::Invalid(_)) => true,

            _ => false,
        }
    }

    pub fn new_var(&mut self, kind: &Kind) -> TyId {
        let key = self.new_key(kind);
        let ty_var = TyVar {
            kind: kind.clone(),
            key,
        };

        TyId::new(self.db, TyData::TyVar(ty_var))
    }

    pub fn new_key(&mut self, kind: &Kind) -> InferenceKey {
        self.table.new_key(InferenceValue::Unbounded(kind.clone()))
    }

    pub fn probe(&mut self, key: InferenceKey) -> Option<TyId> {
        match self.table.probe_value(key) {
            InferenceValue::Bounded(ty) => Some(ty),
            InferenceValue::Unbounded(_) => None,
        }
    }
}

impl<'db> Subst for UnificationTable<'db> {
    fn get(&mut self, ty: TyId) -> Option<TyId> {
        match ty.data(self.db) {
            TyData::TyVar(var) => self.probe(var.key),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InferenceKey(pub(super) u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InferenceValue {
    Bounded(TyId),
    Unbounded(Kind),
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
            (InferenceValue::Unbounded(k1), InferenceValue::Unbounded(k2)) => {
                assert!(k1.can_unify(k2));
                Ok(InferenceValue::Unbounded(k1.clone()))
            }

            (InferenceValue::Unbounded(_), InferenceValue::Bounded(ty))
            | (InferenceValue::Bounded(ty), InferenceValue::Unbounded(_)) => {
                Ok(InferenceValue::Bounded(*ty))
            }

            (InferenceValue::Bounded(ty1), InferenceValue::Bounded(ty2)) => {
                if ty1 != ty2 {
                    Err(())
                } else {
                    Ok(InferenceValue::Bounded(*ty1))
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
