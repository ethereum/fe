use hir::hir_def::{Enum, IdentId, Impl, IngotId, VariantKind};
use rustc_hash::FxHashMap;

use super::{
    adt_def::{lower_adt, AdtRefId},
    binder::Binder,
    canonical::Canonical,
    func_def::{lower_func, FuncDef, HirFuncDefKind},
    ty_def::{InvalidCause, TyBase, TyId},
    ty_lower::lower_hir_ty,
    unify::UnificationTable,
};
use crate::{
    ty::{ty_def::TyData, ty_lower::GenericParamTypeSet},
    HirAnalysisDb,
};

#[salsa::tracked(return_ref)]
pub(crate) fn collect_methods<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: IngotId<'db>,
) -> MethodTable<'db> {
    let mut collector = MethodCollector::new(db, ingot);

    let enums = ingot.all_enums(db.as_hir_db());
    collector.collect_variant_ctors(enums);

    let impls = ingot.all_impls(db.as_hir_db());

    collector.collect_impls(impls);
    collector.finalize()
}

#[salsa::tracked(return_ref)]
pub(crate) fn probe_method<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: IngotId<'db>,
    ty: Canonical<TyId<'db>>,
    name: IdentId<'db>,
) -> Vec<FuncDef<'db>> {
    let table = collect_methods(db, ingot);
    table.probe(db, ty, name)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodTable<'db> {
    buckets: FxHashMap<TyBase<'db>, MethodBucket<'db>>,
}

impl<'db> MethodTable<'db> {
    fn probe(
        &self,
        db: &'db dyn HirAnalysisDb,
        ty: Canonical<TyId<'db>>,
        name: IdentId<'db>,
    ) -> Vec<FuncDef<'db>> {
        let mut table = UnificationTable::new(db);
        let ty = ty.extract_identity(&mut table);
        let Some(base) = Self::extract_ty_base(ty, db) else {
            return vec![];
        };

        if let Some(bucket) = self.buckets.get(base) {
            bucket.probe(&mut table, ty, name)
        } else {
            vec![]
        }
    }

    fn new() -> Self {
        Self {
            buckets: FxHashMap::default(),
        }
    }

    fn finalize(self) -> Self {
        self
    }

    fn insert(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>, func: FuncDef<'db>) {
        let Some(base) = Self::extract_ty_base(ty, db) else {
            return;
        };

        let name = func.name(db);
        let bucket = self.buckets.entry(*base).or_insert_with(MethodBucket::new);
        let methods = bucket.methods.entry(Binder::bind(ty)).or_default();
        methods.insert(name, func);
    }

    fn extract_ty_base(ty: TyId<'db>, db: &'db dyn HirAnalysisDb) -> Option<&'db TyBase<'db>> {
        let base = ty.base_ty(db);
        match base.data(db) {
            TyData::TyBase(base) => Some(base),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MethodBucket<'db> {
    methods: FxHashMap<Binder<TyId<'db>>, FxHashMap<IdentId<'db>, FuncDef<'db>>>,
}

impl<'db> MethodBucket<'db> {
    fn new() -> Self {
        Self {
            methods: FxHashMap::default(),
        }
    }

    fn probe(
        &self,
        table: &mut UnificationTable<'db>,
        ty: TyId<'db>,
        name: IdentId<'db>,
    ) -> Vec<FuncDef<'db>> {
        let mut methods = vec![];
        for (&cand_ty, funcs) in self.methods.iter() {
            let snapshot = table.snapshot();

            let ty = table.instantiate_to_term(ty);
            let cand_ty = table.instantiate_with_fresh_vars(cand_ty);
            let cand_ty = table.instantiate_to_term(cand_ty);

            if table.unify(cand_ty, ty).is_ok() {
                if let Some(func) = funcs.get(&name) {
                    methods.push(*func)
                }
            }
            table.rollback_to(snapshot);
        }

        methods
    }
}

struct MethodCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    ingot: IngotId<'db>,
    method_table: MethodTable<'db>,
}

impl<'db> MethodCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, ingot: IngotId<'db>) -> Self {
        Self {
            db,
            ingot,
            method_table: MethodTable::new(),
        }
    }

    fn collect_variant_ctors(&mut self, enums: &[Enum<'db>]) {
        let hir_db = self.db.as_hir_db();
        for &enum_ in enums {
            let adt_ref = AdtRefId::new(self.db, enum_.into());
            let adt = lower_adt(self.db, adt_ref);
            for (i, variant) in enum_.variants(hir_db).data(hir_db).iter().enumerate() {
                if !matches!(variant.kind, VariantKind::Tuple(_)) {
                    continue;
                };
                let (Some(name), Some(variant)) =
                    (variant.name.to_opt(), adt.fields(self.db).get(i))
                else {
                    continue;
                };

                let arg_tys = variant.iter_types(self.db).collect();
                let mut ret_ty = TyId::adt(self.db, adt);
                let adt_param_set = adt.param_set(self.db);
                ret_ty = TyId::foldl(self.db, ret_ty, adt.param_set(self.db).params(self.db));

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

    fn collect_impls(&mut self, impls: &[Impl<'db>]) {
        for impl_ in impls {
            let ty = match impl_.ty(self.db.as_hir_db()).to_opt() {
                Some(ty) => lower_hir_ty(self.db, ty, impl_.scope()),
                None => TyId::invalid(self.db, InvalidCause::Other),
            };

            if ty.has_invalid(self.db) | !ty.is_inherent_impl_allowed(self.db, self.ingot) {
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

    fn finalize(self) -> MethodTable<'db> {
        self.method_table.finalize()
    }

    fn insert(&mut self, ty: TyId<'db>, func: FuncDef<'db>) {
        let ty = match func.receiver_ty(self.db) {
            Some(ty) => ty.instantiate_identity(),
            None => ty,
        };

        if self
            .method_table
            .probe(self.db, Canonical::new(self.db, ty), func.name(self.db))
            .is_empty()
        {
            self.method_table.insert(self.db, ty, func)
        }
    }
}
