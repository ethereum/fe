use hir::{
    hir_def::{
        scope_graph::ScopeId, FieldDef, TraitRefId, TypeAlias, TypeId as HirTyId, VariantKind,
    },
    visitor::prelude::*,
};
use rustc_hash::FxHashSet;
use salsa::function::Configuration;

use crate::{
    ty::diagnostics::{AdtDefDiagAccumulator, TypeAliasDefDiagAccumulator},
    HirAnalysisDb,
};

use super::{
    constraint::AssumptionListId,
    diagnostics::{TraitConstraintDiag, TyDiagCollection, TyLowerDiag},
    trait_lower::{lower_trait_ref, TraitRefLowerError},
    ty_def::{AdtDef, AdtRefId, InvalidCause, TyData, TyId},
    ty_lower::{lower_adt, lower_hir_ty},
    visitor::{walk_ty, TyVisitor},
};

#[salsa::tracked]
pub fn analyze_adt(db: &dyn HirAnalysisDb, adt_ref: AdtRefId) {
    let mut analyzer = AdtDefAnalysisVisitor::new(db, adt_ref);
    let item = adt_ref.as_item(db);

    let mut ctxt = VisitorCtxt::with_item(db.as_hir_db(), item);
    analyzer.visit_item(&mut ctxt, item);

    for diag in analyzer.diags {
        AdtDefDiagAccumulator::push(db, diag);
    }

    if let Some(diag) = check_recursive_adt(db, adt_ref) {
        AdtDefDiagAccumulator::push(db, diag);
    }
}

#[salsa::tracked]
pub fn analyze_type_alias(db: &dyn HirAnalysisDb, alias: TypeAlias) {
    let Some(hir_ty) = alias.ty(db.as_hir_db()).to_opt() else {
        return;
    };

    let ty = lower_hir_ty(db, hir_ty, alias.scope());

    if matches!(ty.data(db), TyData::Invalid(InvalidCause::AliasCycle)) {
        TypeAliasDefDiagAccumulator::push(
            db,
            TyLowerDiag::TypeAliasCycle(alias.lazy_span().ty().into()).into(),
        );
    }

    // We don't need to check for bound satisfiability here because type alias
    // doesn't have trait bound, it will be checked where the type alias is used.
    if let Some(diag) = ty.emit_diag(db, alias.lazy_span().ty().into()) {
        TypeAliasDefDiagAccumulator::push(db, diag.into());
    }
}

#[salsa::tracked(recovery_fn = check_recursive_adt_impl)]
pub(crate) fn check_recursive_adt(
    db: &dyn HirAnalysisDb,
    adt: AdtRefId,
) -> Option<TyDiagCollection> {
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

fn check_recursive_adt_impl(
    db: &dyn HirAnalysisDb,
    cycle: &salsa::Cycle,
    adt: AdtRefId,
) -> Option<TyDiagCollection> {
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
                    return Some(diag.into());
                }
            }
        }
    }

    None
}

struct AdtDefAnalysisVisitor<'db> {
    db: &'db dyn HirAnalysisDb,
    diags: Vec<TyDiagCollection>,
    scope: ScopeId,
    assumptions: AssumptionListId,
}

impl<'db> AdtDefAnalysisVisitor<'db> {
    fn new(db: &'db dyn HirAnalysisDb, adt: AdtRefId) -> Self {
        let adt = lower_adt(db, adt);
        Self {
            db,
            diags: Vec::new(),
            scope: adt.scope(db),
            assumptions: adt.constraints(db),
        }
    }

    // This method ensures that field/variant types are fully applied.
    fn verify_fully_applied(&mut self, ty: HirTyId, span: DynLazySpan) -> bool {
        let ty = lower_hir_ty(self.db, ty, self.scope);
        if !ty.is_mono_type(self.db) {
            self.diags
                .push(TyLowerDiag::not_fully_applied_type(span).into());
            false
        } else {
            true
        }
    }
}

impl<'db> Visitor for AdtDefAnalysisVisitor<'db> {
    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'_, LazyTySpan>, hir_ty: HirTyId) {
        let ty = lower_hir_ty(self.db, hir_ty, self.scope);
        if let Some(diag) = ty.emit_diag(self.db, ctxt.span().unwrap().into()) {
            self.diags.push(diag.into());
        } else if let Some(diag) =
            ty.emit_sat_diag(self.db, self.assumptions, ctxt.span().unwrap().into())
        {
            self.diags.push(diag.into())
        }
    }

    fn visit_field_def(&mut self, ctxt: &mut VisitorCtxt<'_, LazyFieldDefSpan>, field: &FieldDef) {
        if let Some(ty) = field.ty.to_opt() {
            if self.verify_fully_applied(ty, ctxt.span().unwrap().ty().into()) {
                walk_field_def(self, ctxt, field);
            }
        }
    }

    fn visit_variant_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyVariantDefSpan>,
        variant: &hir::hir_def::VariantDef,
    ) {
        if let VariantKind::Tuple(tuple_id) = variant.kind {
            let span = ctxt.span().unwrap().tuple_type_moved();
            for (i, elem_ty) in tuple_id.data(self.db.as_hir_db()).iter().enumerate() {
                let Some(elem_ty) = elem_ty.to_opt() else {
                    continue;
                };

                if self.verify_fully_applied(elem_ty, span.elem_ty(i).into()) {
                    walk_variant_def(self, ctxt, variant);
                }
            }
        }
    }

    fn visit_where_predicate(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyWherePredicateSpan>,
        pred: &hir::hir_def::WherePredicate,
    ) {
        let Some(hir_ty) = pred.ty.to_opt() else {
            return;
        };

        let ty = lower_hir_ty(self.db, hir_ty, self.scope);
        if ty.contains_invalid(self.db) {
            walk_where_predicate(self, ctxt, pred);
            return;
        }

        if !ty.contains_ty_param(self.db) {
            self.diags.push(
                TraitConstraintDiag::concrete_type_bound(
                    self.db,
                    ctxt.span().unwrap().ty().into(),
                    ty,
                )
                .into(),
            );
            return;
        }

        walk_where_predicate(self, ctxt, pred);
    }

    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyTraitRefSpan>,
        trait_ref: TraitRefId,
    ) {
        let trait_inst = match lower_trait_ref(self.db, trait_ref, self.scope) {
            Ok(trait_ref) => trait_ref,
            Err(TraitRefLowerError::TraitNotFound) => {
                return;
            }

            Err(TraitRefLowerError::ArgNumMismatch { expected, given }) => {
                self.diags.push(
                    TraitConstraintDiag::trait_arg_num_mismatch(
                        self.db,
                        ctxt.span().unwrap().into(),
                        expected,
                        given,
                    )
                    .into(),
                );
                return;
            }

            Err(TraitRefLowerError::ArgumentKindMisMatch { expected, given }) => {
                self.diags.push(
                    TraitConstraintDiag::trait_arg_kind_mismatch(
                        self.db,
                        ctxt.span().unwrap().into(),
                        &expected,
                        given,
                    )
                    .into(),
                );
                return;
            }

            Err(TraitRefLowerError::AssocTy(_)) => {
                self.diags
                    .push(TyLowerDiag::assoc_ty(ctxt.span().unwrap().into()).into());
                return;
            }
        };

        if let Some(diag) =
            trait_inst.emit_sat_diag(self.db, self.assumptions, ctxt.span().unwrap().into())
        {
            self.diags.push(diag.into());
            return;
        }

        walk_trait_ref(self, ctxt, trait_ref);
    }
}

impl TyId {
    /// Collect all adts inside types which are not wrapped by indirect type
    /// wrapper like pointer or reference.
    fn collect_direct_adts(self, db: &dyn HirAnalysisDb) -> FxHashSet<AdtRefId> {
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

        let mut collector = AdtCollector {
            adts: FxHashSet::default(),
        };

        walk_ty(&mut collector, db, self);
        collector.adts
    }
}
