use ena::unify::{InPlaceUnificationTable, NoError, UnifyKey, UnifyValue};

use crate::HirAnalysisDb;

use super::ty::{Kind, Subst, TyData, TyId, TyVar};

pub struct UnificationTable<'db> {
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

    /// Returns true if the two types were unified.
    pub fn unify(&mut self, ty1: TyId, ty2: TyId) -> bool {
        let snapshot = self.table.snapshot();

        if self.unify_impl(ty1, ty2) {
            self.table.commit(snapshot);
            true
        } else {
            self.table.rollback_to(snapshot);
            false
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

    fn unify_impl(&mut self, ty1: TyId, ty2: TyId) -> bool {
        if ty1.kind(self.db) != ty2.kind(self.db) {
            return false;
        }
        let ty1 = self.apply(self.db, ty1);
        let ty2 = self.apply(self.db, ty2);

        match (ty1.data(self.db), ty2.data(self.db)) {
            (TyData::TyVar(var), _) if !ty2.free_inference_keys(self.db).contains(&var.key) => {
                self.table
                    .union_value(var.key, InferenceValue::Bounded(ty2));
                true
            }

            (_, TyData::TyVar(var)) if !ty1.free_inference_keys(self.db).contains(&var.key) => {
                self.table
                    .union_value(var.key, InferenceValue::Bounded(ty2));
                true
            }
            (TyData::TyVar(var1), TyData::TyVar(var2)) => {
                self.table.union(var1.key, var2.key);
                true
            }

            (TyData::TyApp(ty1_1, ty1_2), TyData::TyApp(ty2_1, ty2_2)) => {
                let ok = self.unify_impl(ty1_1, ty2_1);
                if ok {
                    let ty1_2 = self.apply(self.db, ty1_2);
                    let ty2_2 = self.apply(self.db, ty2_2);
                    self.unify_impl(ty1_2, ty2_2)
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
pub struct InferenceKey(u32);

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
    type Error = NoError;

    fn unify_values(v1: &Self, v2: &Self) -> Result<Self, Self::Error> {
        match (v1, v2) {
            (InferenceValue::Unbounded(k1), InferenceValue::Unbounded(k2)) => {
                assert!(k1 == k2);
                Ok(InferenceValue::Unbounded(k1.clone()))
            }

            (InferenceValue::Unbounded(_), InferenceValue::Bounded(ty))
            | (InferenceValue::Bounded(ty), InferenceValue::Unbounded(_)) => {
                Ok(InferenceValue::Bounded(*ty))
            }

            (InferenceValue::Bounded(ty1), InferenceValue::Bounded(ty2)) => {
                panic!("trying to unify two bounded types {:?} and {:?}", ty1, ty2)
            }
        }
    }
}
