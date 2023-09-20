use hir::{
    hir_def::{scope_graph::ScopeId, FieldDef, TypeId as HirTyId, VariantKind},
    visitor::prelude::*,
};
use rustc_hash::FxHashSet;
use salsa::function::Configuration;

use crate::{ty::diagnostics::AdtDefDiagAccumulator, HirAnalysisDb};

use super::{
    diagnostics::TyLowerDiag,
    ty::{AdtDef, AdtRefId, TyId},
    ty_lower::{lower_adt, lower_hir_ty, lower_hir_ty_with_diag},
    visitor::{walk_ty, TyDiagCollector, TyVisitor},
};

#[salsa::tracked]
pub fn analyze_adt(db: &dyn HirAnalysisDb, adt: AdtRefId) {
    let mut analyzer = AdtDefAnalysisVisitor {
        db,
        accumulated: Vec::new(),
        scope: adt.scope(db),
    };
    let item = adt.as_item(db);

    let mut ctxt = VisitorCtxt::with_item(db.as_hir_db(), item);
    analyzer.visit_item(&mut ctxt, item);

    for diag in analyzer.accumulated {
        AdtDefDiagAccumulator::push(db, diag);
    }

    if let Some(diag) = check_recursive_adt(db, adt) {
        AdtDefDiagAccumulator::push(db, diag);
    }
}

#[salsa::tracked(recovery_fn = check_recursive_adt_impl)]
pub(crate) fn check_recursive_adt(db: &dyn HirAnalysisDb, adt: AdtRefId) -> Option<TyLowerDiag> {
    let adt_def = lower_adt(db, adt);
    for field in adt_def.fields(db) {
        for ty in field.iter_types(db) {
            for adt_ref in ty.collect_direct_adts(db) {
                check_recursive_adt(db, adt_ref);
            }
        }
    }

    None
}

struct AdtDefAnalysisVisitor<'db> {
    db: &'db dyn HirAnalysisDb,
    accumulated: Vec<TyLowerDiag>,
    scope: ScopeId,
}

impl<'db> AdtDefAnalysisVisitor<'db> {
    // This method ensures that field/variant types are fully applied.
    fn verify_fully_applied(&mut self, ty: HirTyId, span: DynLazySpan) {
        let ty = lower_hir_ty(self.db, ty, self.scope);
        if !ty.is_mono_type(self.db) {
            self.accumulated
                .push(TyLowerDiag::not_fully_applied_type(span));
        }
    }
}

impl<'db> Visitor for AdtDefAnalysisVisitor<'db> {
    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'_, LazyTySpan>, hir_ty: HirTyId) {
        self.accumulated
            .extend(lower_hir_ty_with_diag(self.db, hir_ty, ctxt.span().unwrap(), self.scope).1);
    }

    fn visit_field_def(&mut self, ctxt: &mut VisitorCtxt<'_, LazyFieldDefSpan>, field: &FieldDef) {
        if let Some(ty) = field.ty.to_opt() {
            self.verify_fully_applied(ty, ctxt.span().unwrap().ty().into());
        }

        walk_field_def(self, ctxt, field);
    }

    fn visit_variant_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyVariantDefSpan>,
        variant: &hir::hir_def::VariantDef,
    ) {
        if let VariantKind::Tuple(tuple_id) = variant.kind {
            let ty = tuple_id.to_ty(self.db.as_hir_db());
            self.verify_fully_applied(ty, ctxt.span().unwrap().tuple_type().into());
        }

        walk_variant_def(self, ctxt, variant);
    }
}

pub(super) fn collect_ty_lower_diags(
    db: &dyn HirAnalysisDb,
    hir_ty: HirTyId,
    span: LazyTySpan,
    scope: ScopeId,
) -> Vec<TyLowerDiag> {
    let collector = TyDiagCollector::new(db, scope);
    collector.collect(hir_ty, span)
}

fn check_recursive_adt_impl(
    db: &dyn HirAnalysisDb,
    cycle: &salsa::Cycle,
    adt: AdtRefId,
) -> Option<TyLowerDiag> {
    let participants: FxHashSet<_> = cycle
        .participant_keys()
        .map(|key| check_recursive_adt::key_from_id(key.key_index()))
        .collect();

    let adt_def = lower_adt(db, adt);
    for (field_idx, field) in adt_def.fields(db).iter().enumerate() {
        for (ty_idx, ty) in field.iter_types(db).enumerate() {
            for field_adt_ref in ty.collect_direct_adts(db) {
                if participants.contains(&field_adt_ref) && participants.contains(&adt) {
                    let diag = TyLowerDiag::recursive_type(
                        adt.name_span(db),
                        adt_def.variant_ty_span(db, field_idx, ty_idx),
                    );
                    return Some(diag);
                }
            }
        }
    }

    None
}

impl TyId {
    /// Collect all adts inside types which are not wrapped by indirect type
    /// wrapper like pointer or reference.
    fn collect_direct_adts(self, db: &dyn HirAnalysisDb) -> FxHashSet<AdtRefId> {
        let mut collector = AdtCollector {
            adts: FxHashSet::default(),
        };

        walk_ty(&mut collector, db, self);
        collector.adts
    }
}

struct AdtCollector {
    adts: FxHashSet<AdtRefId>,
}

impl TyVisitor for AdtCollector {
    fn visit_app(&mut self, db: &dyn HirAnalysisDb, abs: TyId, arg: TyId) {
        if !abs.is_indirect(db) {
            walk_ty(self, db, arg)
        }
    }

    fn visit_adt(&mut self, db: &dyn HirAnalysisDb, adt: AdtDef) {
        self.adts.insert(adt.adt_ref(db));
    }
}