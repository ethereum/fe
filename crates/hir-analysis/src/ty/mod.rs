use adt_def::{lower_adt, AdtDef, AdtRef};
use def_analysis::check_recursive_adt;
use diagnostics::{TraitLowerDiag, TyLowerDiag};
use hir::hir_def::{TopLevelMod, TypeAlias};
use rustc_hash::FxHashSet;
use trait_def::TraitDef;
use trait_lower::lower_trait;
use trait_resolution::constraint::super_trait_cycle;
use ty_def::{InvalidCause, TyData};
use ty_lower::{lower_hir_ty, lower_type_alias};

use self::def_analysis::{
    analyze_adt, analyze_func, analyze_impl, analyze_impl_trait, analyze_trait,
};
use crate::{analysis_pass::ModuleAnalysisPass, diagnostics::DiagnosticVoucher, HirAnalysisDb};

pub mod adt_def;
pub mod binder;
pub mod canonical;
pub mod const_ty;
pub mod def_analysis;
pub mod diagnostics;
pub mod fold;
pub mod func_def;
mod method_cmp;
pub mod method_table;
pub mod trait_def;
pub mod trait_lower;
pub mod trait_resolution;
pub mod ty_check;
pub mod ty_def;
pub mod ty_lower;
pub mod unify;
pub mod visitor;

/// An analysis pass for type definitions.
pub struct AdtDefAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}
impl<'db> AdtDefAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass<'db> for AdtDefAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        let hir_db = self.db;
        let adts = top_mod
            .all_structs(hir_db)
            .iter()
            .copied()
            .map(AdtRef::from)
            .chain(top_mod.all_enums(hir_db).iter().copied().map(AdtRef::from))
            .chain(
                top_mod
                    .all_contracts(hir_db)
                    .iter()
                    .copied()
                    .map(AdtRef::from),
            );

        let mut diags = vec![];
        let mut cycle_participants = FxHashSet::<AdtDef<'db>>::default();

        for adt in adts {
            diags.extend(analyze_adt(self.db, adt).iter().map(|d| d.to_voucher()));
            let adt = lower_adt(self.db, adt);
            if !cycle_participants.contains(&adt) {
                if let Some(cycle) = check_recursive_adt(self.db, adt) {
                    diags.push(Box::new(TyLowerDiag::RecursiveType(cycle.clone())) as _);
                    cycle_participants.extend(cycle.iter().map(|m| m.adt));
                }
            }
        }
        diags
    }
}

pub struct BodyAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}
impl<'db> BodyAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}
impl<'db> ModuleAnalysisPass<'db> for BodyAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        top_mod
            .all_funcs(self.db)
            .iter()
            .flat_map(|func| &ty_check::check_func_body(self.db, *func).0)
            .map(|diag| diag.to_voucher())
            .collect()
    }
}

/// An analysis pass for trait definitions.
pub struct TraitAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}
impl<'db> TraitAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass<'db> for TraitAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        let mut diags = vec![];
        let mut cycle_participants = FxHashSet::<TraitDef<'db>>::default();

        for hir_trait in top_mod.all_traits(self.db) {
            let trait_ = lower_trait(self.db, *hir_trait);
            if !cycle_participants.contains(&trait_) {
                if let Some(cycle) = super_trait_cycle(self.db, trait_) {
                    diags.push(Box::new(TraitLowerDiag::CyclicSuperTraits(cycle.clone())) as _);
                    cycle_participants.extend(cycle.iter());
                }
            }
            diags.extend(
                analyze_trait(self.db, *hir_trait)
                    .iter()
                    .map(|d| d.to_voucher()),
            )
        }
        diags
    }
}

pub struct ImplAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> ImplAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass<'db> for ImplAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        top_mod
            .all_impls(self.db)
            .iter()
            .flat_map(|impl_| analyze_impl(self.db, *impl_))
            .map(|diag| diag.to_voucher())
            .collect()
    }
}

/// An analysis pass for `ImplTrait'.
pub struct ImplTraitAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> ImplTraitAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass<'db> for ImplTraitAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        top_mod
            .all_impl_traits(self.db)
            .iter()
            .flat_map(|trait_| analyze_impl_trait(self.db, *trait_))
            .map(|diag| diag.to_voucher())
            .collect()
    }
}

/// An analysis pass for `ImplTrait'.
pub struct FuncAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> FuncAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass<'db> for FuncAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        top_mod
            .all_funcs(self.db)
            .iter()
            .flat_map(|func| analyze_func(self.db, *func))
            .map(|diag| diag.to_voucher())
            .collect()
    }
}

/// An analysis pass for type aliases.
pub struct TypeAliasAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> TypeAliasAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

/// This function implements analysis for the type alias definition.
/// The analysis includes the following:
/// - Check if the type alias is not recursive.
/// - Check if the type in the type alias is well-formed.
///
/// NOTE: This function doesn't check the satisfiability of the type since our
/// type system treats the alias as kind of macro, meaning type alias isn't
/// included in the type system. Satisfiability is checked where the type alias
/// is used.
impl<'db> ModuleAnalysisPass<'db> for TypeAliasAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher<'db> + 'db>> {
        let mut diags = vec![];
        let mut cycle_participants = FxHashSet::<TypeAlias>::default();

        for alias in top_mod.all_type_aliases(self.db) {
            if cycle_participants.contains(alias) {
                continue;
            }
            let ta = lower_type_alias(self.db, *alias);
            let ty = ta.alias_to.skip_binder();
            if let TyData::Invalid(InvalidCause::AliasCycle(cycle)) = ty.data(self.db) {
                if let Some(diag) = ty.emit_diag(self.db, alias.lazy_span().ty().into()) {
                    diags.push(diag.to_voucher());
                }
                cycle_participants.extend(cycle.iter());
            } else if ty.has_invalid(self.db) {
                if let Some(hir_ty) = alias.ty(self.db).to_opt() {
                    let ty = lower_hir_ty(self.db, hir_ty, alias.scope());
                    if let Some(diag) = ty.emit_diag(self.db, alias.lazy_span().ty().into()) {
                        diags.push(diag.to_voucher());
                    }
                }
            }
        }
        diags
    }
}
