use common::input::IngotKind;
use hir::hir_def::{Enum, IdentId, Impl, IngotId, VariantKind};
use rustc_hash::FxHashMap;

use super::{
    binder::Binder,
    canonical::Canonical,
    ty_def::{AdtRefId, FuncDef, InvalidCause, TyBase, TyId},
    ty_lower::{lower_func, lower_hir_ty},
    unify::UnificationTable,
};
use crate::{
    ty::{
        ty_def::{HirFuncDefKind, TyData},
        ty_lower::{lower_adt, GenericParamTypeSet},
    },
    HirAnalysisDb,
};

#[salsa::tracked(return_ref)]
pub(crate) fn collect_methods(db: &dyn HirAnalysisDb, ingot: IngotId) -> MethodTable {
    let mut collector = MethodCollector::new(db, ingot);

    let enums = ingot.all_enums(db.as_hir_db());
    collector.collect_variant_ctors(enums);

    let impls = ingot.all_impls(db.as_hir_db());

    collector.collect_impls(impls);
    collector.finalize()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodTable {
    buckets: FxHashMap<TyBase, MethodBucket>,
    mode: TableMode,
}

impl MethodTable {
    pub fn probe(
        &self,
        db: &dyn HirAnalysisDb,
        ty: Canonical<TyId>,
        name: IdentId,
    ) -> Option<FuncDef> {
        let mut table = UnificationTable::new(db);
        let ty = ty.decanonicalize(&mut table);
        let base = Self::extract_ty_base(ty, db)?;
        if let Some(bucket) = self.buckets.get(base) {
            return bucket.probe(&mut table, self.mode, ty, name);
        }

        None
    }

    pub(super) fn probe_eager(
        &self,
        db: &dyn HirAnalysisDb,
        ty: Canonical<TyId>,
        name: IdentId,
    ) -> Option<FuncDef> {
        let mut table = UnificationTable::new(db);
        let ty = ty.decanonicalize(&mut table);
        let base = Self::extract_ty_base(ty, db)?;
        if let Some(bucket) = self.buckets.get(base) {
            return bucket.probe(&mut table, TableMode::Creation, ty, name);
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
        let Some(base) = Self::extract_ty_base(ty, db) else {
            return;
        };

        let name = func.name(db);
        let bucket = self.buckets.entry(*base).or_insert_with(MethodBucket::new);
        let methods = bucket.methods.entry(Binder::bind(ty)).or_default();
        methods.insert(name, func);
    }

    fn extract_ty_base(ty: TyId, db: &dyn HirAnalysisDb) -> Option<&TyBase> {
        let base = ty.base_ty(db);
        match base.data(db) {
            TyData::TyBase(base) => Some(base),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MethodBucket {
    methods: FxHashMap<Binder<TyId>, FxHashMap<IdentId, FuncDef>>,
}

impl MethodBucket {
    fn new() -> Self {
        Self {
            methods: FxHashMap::default(),
        }
    }

    fn probe(
        &self,
        table: &mut UnificationTable,
        mode: TableMode,
        ty: TyId,
        name: IdentId,
    ) -> Option<FuncDef> {
        for (&cand_ty, funcs) in self.methods.iter() {
            let snapshot = table.snapshot();
            let cand_ty = table.instantiate_with_fresh_vars(cand_ty);
            let ty = if mode == TableMode::Creation {
                table.instantiate_with_fresh_vars(Binder::bind(ty))
            } else {
                ty
            };

            if table.unify(cand_ty, ty).is_ok() {
                if let Some(func) = funcs.get(&name) {
                    return Some(*func);
                }
            }
            table.rollback_to(snapshot);
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

    fn collect_variant_ctors(&mut self, enums: &[Enum]) {
        let hir_db = self.db.as_hir_db();
        for &enum_ in enums {
            let adt_ref = AdtRefId::new(self.db, enum_.into());
            let adt = lower_adt(self.db, adt_ref);
            for (i, variant) in enum_.variants(hir_db).data(hir_db).iter().enumerate() {
                if !matches!(variant.kind, VariantKind::Tuple(_)) {
                    continue;
                };
                let Some(name) = variant.name.to_opt() else {
                    continue;
                };
                let Some(variant) = adt.fields(self.db).get(i) else {
                    continue;
                };

                let arg_tys = variant.iter_types(self.db).collect();

                let mut ret_ty = TyId::adt(self.db, adt);
                let adt_param_set = adt.param_set(self.db);

                for &generic_param in adt_param_set.params(self.db) {
                    ret_ty = TyId::app(self.db, ret_ty, generic_param);
                }

                let param_set = GenericParamTypeSet::new(
                    self.db,
                    adt_param_set.params_precursor(self.db).to_vec(),
                    adt_param_set.scope(self.db),
                    adt_param_set.len(self.db),
                );

                let func = FuncDef::new(
                    self.db,
                    HirFuncDefKind::VariantCtor(enum_, i),
                    name,
                    param_set,
                    arg_tys,
                    Binder::bind(ret_ty),
                );

                self.insert(ret_ty, func)
            }
        }
    }

    fn collect_impls(&mut self, impls: &[Impl]) {
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
    }

    fn finalize(self) -> MethodTable {
        self.method_table.finalize()
    }

    fn insert(&mut self, ty: TyId, func: FuncDef) {
        let ty = match func.receiver_ty(self.db) {
            Some(ty) => ty.instantiate_identity(),
            None => ty,
        };

        if self
            .method_table
            .probe(
                self.db,
                Canonical::canonicalize(self.db, ty),
                func.name(self.db),
            )
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
