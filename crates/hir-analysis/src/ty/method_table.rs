use common::input::IngotKind;
use hir::hir_def::{IdentId, Impl, IngotId};
use rustc_hash::FxHashMap;

use super::{
    ty_def::{FuncDef, InvalidCause, TyBase, TyId},
    ty_lower::{lower_func, lower_hir_ty},
    unify::UnificationTable,
};
use crate::HirAnalysisDb;

#[salsa::tracked(return_ref)]
pub(crate) fn collect_methods(db: &dyn HirAnalysisDb, ingot: IngotId) -> MethodTable {
    let impls = ingot.all_impls(db.as_hir_db());
    MethodCollector::new(db, ingot).collect(impls)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodTable {
    buckets: FxHashMap<TyBase, MethodBucket>,
    mode: TableMode,
}

impl MethodTable {
    pub fn prove(&self, db: &dyn HirAnalysisDb, ty: TyId, name: IdentId) -> Option<FuncDef> {
        let base = ty.base_ty(db)?;
        if let Some(bucket) = self.buckets.get(&base) {
            return bucket.prove(db, self.mode, ty, name);
        }

        None
    }

    pub(super) fn prove_eager(
        &self,
        db: &dyn HirAnalysisDb,
        ty: TyId,
        name: IdentId,
    ) -> Option<FuncDef> {
        let base = ty.base_ty(db)?;
        if let Some(bucket) = self.buckets.get(&base) {
            return bucket.prove(db, TableMode::Creation, ty, name);
        }

        None
    }

    pub(super) fn new() -> Self {
        Self {
            buckets: FxHashMap::default(),
            mode: TableMode::Creation,
        }
    }

    pub(super) fn finalize(mut self) -> Self {
        self.mode = TableMode::Lookup;
        self
    }

    pub(super) fn insert(&mut self, db: &dyn HirAnalysisDb, ty: TyId, func: FuncDef) {
        let Some(base) = ty.base_ty(db) else {
            return;
        };

        let name = func.name(db);
        let bucket = self.buckets.entry(base).or_insert_with(MethodBucket::new);
        let methods = bucket.methods.entry(ty).or_default();
        methods.insert(name, func);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MethodBucket {
    methods: FxHashMap<TyId, FxHashMap<IdentId, FuncDef>>,
}

impl MethodBucket {
    fn new() -> Self {
        Self {
            methods: FxHashMap::default(),
        }
    }

    fn prove(
        &self,
        db: &dyn HirAnalysisDb,
        mode: TableMode,
        ty: TyId,
        name: IdentId,
    ) -> Option<FuncDef> {
        for (cand_ty, funcs) in self.methods.iter() {
            let mut table = UnificationTable::new(db);
            let cand_ty = cand_ty.generalize(db, &mut table);
            let ty = if mode == TableMode::Creation {
                ty.generalize(db, &mut table)
            } else {
                ty
            };

            if table.unify(cand_ty, ty) {
                if let Some(func) = funcs.get(&name) {
                    return Some(*func);
                }
            }
        }

        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum TableMode {
    Creation,
    Lookup,
}

struct MethodCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    ingot: IngotId,
    method_table: MethodTable,
}

impl<'db> MethodCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, ingot: IngotId) -> Self {
        Self {
            db,
            ingot,
            method_table: MethodTable::new(),
        }
    }

    fn collect(mut self, impls: &[Impl]) -> MethodTable {
        for impl_ in impls {
            let ty = match impl_.ty(self.db.as_hir_db()).to_opt() {
                Some(ty) => lower_hir_ty(self.db, ty, impl_.scope()),
                None => TyId::invalid(self.db, InvalidCause::Other),
            };

            if ty.contains_invalid(self.db) {
                continue;
            }

            if !is_ty_implementable_in(self.db, ty, self.ingot) {
                continue;
            }

            for func in impl_.funcs(self.db.as_hir_db()) {
                let Some(func) = lower_func(self.db, func) else {
                    continue;
                };

                self.insert(ty, func)
            }
        }

        self.method_table.finalize()
    }

    fn insert(&mut self, ty: TyId, func: FuncDef) {
        let ty = match func.receiver_ty(self.db) {
            Some(ty) => ty,
            None => ty,
        };

        if self
            .method_table
            .prove(self.db, ty, func.name(self.db))
            .is_none()
        {
            self.method_table.insert(self.db, ty, func)
        }
    }
}

fn is_ty_implementable_in(db: &dyn HirAnalysisDb, ty: TyId, ingot: IngotId) -> bool {
    let ty_ingot = ty.ingot(db);
    match ingot.kind(db.as_hir_db()) {
        IngotKind::Std => ty_ingot.is_none() || ty_ingot == Some(ingot),
        _ => ty_ingot == Some(ingot),
    }
}
