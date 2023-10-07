use hir::{
    hir_def::{
        scope_graph::ScopeId, FieldDef, Trait, TraitRefId, TypeAlias, TypeId as HirTyId,
        VariantKind,
    },
    visitor::prelude::*,
};
use rustc_hash::FxHashSet;
use salsa::function::Configuration;

use crate::{
    ty::diagnostics::{
        AdtDefDiagAccumulator, TraitDefDiagAccumulator, TypeAliasDefDiagAccumulator,
    },
    HirAnalysisDb,
};

use super::{
    constraint::{collect_super_traits, AssumptionListId, SuperTraitCycle},
    diagnostics::{TraitConstraintDiag, TraitLowerDiag, TyDiagCollection, TyLowerDiag},
    trait_::TraitDef,
    trait_lower::{lower_trait, lower_trait_ref, TraitRefLowerError},
    ty_def::{AdtDef, AdtRefId, InvalidCause, TyData, TyId},
    ty_lower::{lower_adt, lower_hir_ty, lower_kind},
    visitor::{walk_ty, TyVisitor},
};

#[salsa::tracked]
pub fn analyze_adt(db: &dyn HirAnalysisDb, adt_ref: AdtRefId) {
    let analyzer = DefAnalyzer::for_adt(db, adt_ref);
    let diags = analyzer.analyze();

    for diag in diags {
        AdtDefDiagAccumulator::push(db, diag);
    }

    if let Some(diag) = check_recursive_adt(db, adt_ref) {
        AdtDefDiagAccumulator::push(db, diag);
    }
}

#[salsa::tracked]
pub fn analyze_trait(db: &dyn HirAnalysisDb, trait_: Trait) {
    let analyzer = DefAnalyzer::for_trait(db, trait_);
    let diags = analyzer.analyze();

    for diag in diags {
        TraitDefDiagAccumulator::push(db, diag);
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

pub struct DefAnalyzer<'db> {
    db: &'db dyn HirAnalysisDb,
    def: DefKind,
    diags: Vec<TyDiagCollection>,
    scope: ScopeId,
    assumptions: AssumptionListId,
    current_ty: Option<TyId>,
}

impl<'db> DefAnalyzer<'db> {
    fn for_adt(db: &'db dyn HirAnalysisDb, adt: AdtRefId) -> Self {
        let def = lower_adt(db, adt);
        let scope = adt.scope(db);
        let assumptions = def.constraints(db);
        Self {
            db,
            def: def.into(),
            diags: vec![],
            scope,
            assumptions,
            current_ty: None,
        }
    }

    fn for_trait(db: &'db dyn HirAnalysisDb, trait_: Trait) -> Self {
        let def = lower_trait(db, trait_);
        let scope = trait_.scope();
        let assumptions = def.constraints(db);
        Self {
            db,
            def: def.into(),
            diags: vec![],
            scope,
            assumptions,
            current_ty: None,
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

    fn analyze(mut self) -> Vec<TyDiagCollection> {
        match self.def {
            DefKind::Adt(def) => {
                let item = def.adt_ref(self.db).as_item(self.db);
                let mut ctxt = VisitorCtxt::with_item(self.db.as_hir_db(), item);
                self.visit_item(&mut ctxt, item);
            }

            DefKind::Trait(trait_) => {
                let trait_ = trait_.trait_(self.db);
                let mut ctxt = VisitorCtxt::with_trait(self.db.as_hir_db(), trait_);
                self.visit_trait(&mut ctxt, trait_);
            }
        }

        self.diags
    }
}

impl<'db> Visitor for DefAnalyzer<'db> {
    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'_, LazyTySpan>, hir_ty: HirTyId) {
        let ty = lower_hir_ty(self.db, hir_ty, self.scope);
        let span = ctxt.span().unwrap();
        if let Some(diag) = ty.emit_diag(self.db, span.clone().into()) {
            self.diags.push(diag)
        } else if let Some(diag) = ty.emit_sat_diag(self.db, self.assumptions, span.into()) {
            self.diags.push(diag)
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

        if !ty.contains_ty_param(self.db) {
            let diag = TraitConstraintDiag::concrete_type_bound(
                self.db,
                ctxt.span().unwrap().ty().into(),
                ty,
            )
            .into();
            self.diags.push(diag);
            return;
        }

        self.current_ty = Some(ty);
        walk_where_predicate(self, ctxt, pred);
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

    fn visit_generic_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyGenericParamSpan>,
        param: &hir::hir_def::GenericParam,
    ) {
        let ScopeId::GenericParam(_, idx) = ctxt.scope() else {
            unreachable!()
        };
        self.current_ty = Some(self.def.params(self.db)[idx]);

        walk_generic_param(self, ctxt, param)
    }

    fn visit_kind_bound(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyKindBoundSpan>,
        bound: &hir::hir_def::KindBound,
    ) {
        let Some(ty) = self.current_ty else {
            return;
        };

        let kind = lower_kind(bound);
        let former_kind = ty.kind(self.db);
        if !former_kind.does_match(&kind) {
            self.diags.push(
                TyLowerDiag::kind_bound_mismatch(
                    self.db,
                    ctxt.span().unwrap().into(),
                    ty,
                    former_kind,
                    &kind,
                )
                .into(),
            );
        }
    }

    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyTraitRefSpan>,
        trait_ref: TraitRefId,
    ) {
        if self
            .current_ty
            .map(|ty| ty.is_trait_self(self.db))
            .unwrap_or_default()
        {
            if let Some(cycle) = self.def.collect_super_trait_cycle(self.db) {
                if let Ok(trait_inst) = lower_trait_ref(self.db, trait_ref, self.scope) {
                    if cycle.contains(trait_inst.def(self.db)) {
                        self.diags.push(
                            TraitLowerDiag::CyclicSuperTraits(ctxt.span().unwrap().path().into())
                                .into(),
                        );
                        return;
                    }
                }
            }
        }

        if let Some(diag) = analyze_trait_ref(
            self.db,
            trait_ref,
            self.scope,
            self.assumptions,
            ctxt.span().unwrap().into(),
        ) {
            self.diags.push(diag);
            return;
        } else {
            walk_trait_ref(self, ctxt, trait_ref);
        }
    }

    fn visit_super_trait_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, hir::span::item::LazySuperTraitListSpan>,
        super_traits: &[TraitRefId],
    ) {
        self.current_ty = Some(self.def.trait_self_param(self.db));
        walk_super_trait_list(self, ctxt, super_traits);
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

fn analyze_trait_ref(
    db: &dyn HirAnalysisDb,
    trait_ref: TraitRefId,
    scope: ScopeId,
    assumptions: AssumptionListId,
    span: DynLazySpan,
) -> Option<TyDiagCollection> {
    let trait_inst = match lower_trait_ref(db, trait_ref, scope) {
        Ok(trait_ref) => trait_ref,
        Err(TraitRefLowerError::TraitNotFound) => {
            return None;
        }

        Err(TraitRefLowerError::ArgNumMismatch { expected, given }) => {
            return Some(
                TraitConstraintDiag::trait_arg_num_mismatch(db, span, expected, given).into(),
            );
        }

        Err(TraitRefLowerError::ArgumentKindMisMatch { expected, given }) => {
            return Some(
                TraitConstraintDiag::trait_arg_kind_mismatch(db, span, &expected, given).into(),
            );
        }

        Err(TraitRefLowerError::AssocTy(_)) => {
            return Some(TyLowerDiag::assoc_ty(span).into());
        }
    };

    if let Some(diag) = trait_inst.emit_sat_diag(db, assumptions, span) {
        Some(diag.into())
    } else {
        None
    }
}

#[derive(Clone, Copy, Debug, derive_more::From)]
enum DefKind {
    Adt(AdtDef),
    Trait(TraitDef),
}

impl DefKind {
    fn params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        match self {
            Self::Adt(def) => def.params(db),
            Self::Trait(def) => def.params(db),
        }
    }

    fn trait_self_param(self, db: &dyn HirAnalysisDb) -> TyId {
        if let Self::Trait(def) = self {
            def.self_param(db)
        } else {
            panic!()
        }
    }

    fn collect_super_trait_cycle(self, db: &dyn HirAnalysisDb) -> Option<&SuperTraitCycle> {
        if let Self::Trait(def) = self {
            collect_super_traits(db, def).as_ref().err()
        } else {
            None
        }
    }
}
