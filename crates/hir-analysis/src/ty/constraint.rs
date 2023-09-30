use hir::{
    hir_def::{scope_graph::ScopeId, Trait, TraitRef, TypeId, TypeKind, WherePredicate},
    visitor::prelude::*,
};
use rustc_hash::FxHashSet;
use salsa::function::Configuration;

use crate::HirAnalysisDb;

use super::{
    diagnostics::TraitLowerDiag,
    trait_::{TraitDef, TraitInstId},
    trait_lower::{lower_trait_ref, LowerDiagCollection},
    ty_def::{Subst, TyId},
};

#[salsa::interned]
pub(crate) struct ConstraintId {
    ty: TyId,
    trait_: TraitInstId,
}

impl ConstraintId {
    pub fn apply_subst<S: Subst>(self, db: &dyn HirAnalysisDb, subst: &mut S) -> Self {
        let ty = self.ty(db).apply_subst(db, subst);
        let trait_ = self.trait_(db).apply_subst(db, subst);
        Self::new(db, ty, trait_)
    }
}

/// Collects super traits, and verify there are no cyclic in
/// the super traits relationship.
///
/// This method is implemented independently from [`collect_constraints`] method
/// because
/// 1. the cycle check should be performed before collecting other constraints
///    to make sure the constraint simplification terminates.
/// 2. `collect_constraints` function needs to care about another cycle which is
///    caused by constraints simplification.
/// 3. we want to emit better error messages for cyclic super traits.
///
/// NOTE: This methods returns all super traits without any simplification.
#[salsa::tracked(return_ref, recovery_fn = recover_collect_super_traits)]
pub(crate) fn collect_super_traits(
    db: &dyn HirAnalysisDb,
    trait_: TraitDef,
) -> (Vec<TraitInstId>, Vec<LowerDiagCollection>) {
    let mut collector = SuperTraitCollector::new(db, trait_, FxHashSet::default());
    let (insts, diags) = collector.finalize();

    // Check for cycles.
    for inst in &insts {
        collect_super_traits(db, inst.def(db));
    }

    (insts, diags)
}

pub(crate) fn recover_collect_super_traits(
    db: &dyn HirAnalysisDb,
    cycle: &salsa::Cycle,
    trait_: TraitDef,
) -> (Vec<TraitInstId>, Vec<LowerDiagCollection>) {
    let participants: FxHashSet<_> = cycle
        .participant_keys()
        .map(|key| {
            let trait_ = collect_super_traits::key_from_id(key.key_index());
            trait_.trait_(db)
        })
        .collect();

    let mut collector = SuperTraitCollector::new(db, trait_, participants);
    collector.finalize()
}

struct SuperTraitCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    trait_: TraitDef,
    super_traits: Vec<TraitInstId>,
    diags: Vec<LowerDiagCollection>,
    cycle: FxHashSet<Trait>,
    scope: ScopeId,
}

impl<'db> SuperTraitCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, trait_: TraitDef, cycle: FxHashSet<Trait>) -> Self {
        Self {
            db,
            trait_,
            super_traits: vec![],
            diags: vec![],
            cycle,
            scope: trait_.trait_(db).scope(),
        }
    }

    fn finalize(mut self) -> (Vec<TraitInstId>, Vec<LowerDiagCollection>) {
        let hir_trait = self.trait_.trait_(self.db);
        let mut visitor_ctxt = VisitorCtxt::with_trait(self.db.as_hir_db(), hir_trait);
        self.visit_trait(&mut visitor_ctxt, hir_trait);

        (self.super_traits, self.diags)
    }
}

impl<'db> Visitor for SuperTraitCollector<'db> {
    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyTraitRefSpan>,
        trait_ref: TraitRef,
    ) {
        let span = ctxt.span().unwrap();
        let (trait_inst, diags) = lower_trait_ref(self.db, trait_ref, span, self.scope);
        if !diags.is_empty() {
            self.diags.extend(diags);
            return;
        }

        let Some(trait_inst) = trait_inst else {
            return;
        };

        if self
            .cycle
            .contains(&trait_inst.def(self.db).trait_(self.db))
        {
            let span = ctxt.span().unwrap().into();
            self.diags
                .push(TraitLowerDiag::CyclicSuperTraits(span).into());
            return;
        }

        self.super_traits.push(trait_inst);
    }

    fn visit_where_predicate(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyWherePredicateSpan>,
        pred: &WherePredicate,
    ) {
        match pred.ty.to_opt().map(|ty| ty.data(self.db.as_hir_db())) {
            // We just want to check super traits, so we don't care about other type constraints.
            Some(TypeKind::SelfType(args)) if args.is_empty(self.db.as_hir_db()) => {
                walk_where_predicate(self, ctxt, pred);
            }
            _ => return,
        }
    }

    fn visit_item(&mut self, _: &mut VisitorCtxt<'_, LazyItemSpan>, _: hir::hir_def::ItemKind) {
        // We don't want to visit nested items in the trait.
    }
}
