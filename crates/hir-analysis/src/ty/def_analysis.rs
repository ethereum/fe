//! This module contains analysis for the definition of the type/trait.
//! This module is the only module in `ty` module which is allowed to emit
//! diagnostics.

use std::collections::hash_map::Entry;

use common::indexmap::IndexSet;
use hir::{
    hir_def::{
        scope_graph::ScopeId, FieldDef, Func, FuncParamListId, GenericParam, GenericParamListId,
        IdentId, Impl as HirImpl, ImplTrait, ItemKind, PathId, Trait, TraitRefId,
        TypeId as HirTyId, VariantKind,
    },
    visitor::prelude::*,
};
use rustc_hash::{FxHashMap, FxHashSet};

use super::{
    adt_def::{lower_adt, AdtRef},
    canonical::Canonical,
    const_ty::ConstTyId,
    diagnostics::{ImplDiag, TraitConstraintDiag, TraitLowerDiag, TyDiagCollection, TyLowerDiag},
    func_def::FuncDef,
    method_cmp::compare_impl_method,
    method_table::probe_method,
    trait_def::{ingot_trait_env, Implementor, TraitDef},
    trait_lower::{lower_trait, lower_trait_ref, TraitRefLowerError},
    trait_resolution::{
        constraint::{
            collect_adt_constraints, collect_func_def_constraints, collect_impl_block_constraints,
        },
        PredicateListId,
    },
    ty_def::{InvalidCause, TyData, TyId},
    ty_lower::{collect_generic_params, lower_kind},
    visitor::{walk_ty, TyVisitor},
};
use crate::{
    name_resolution::{resolve_path, PathRes},
    ty::{
        adt_def::AdtDef,
        binder::Binder,
        canonical::Canonicalized,
        func_def::lower_func,
        trait_def::{does_impl_trait_conflict, TraitInstId},
        trait_lower::lower_impl_trait,
        trait_resolution::{
            constraint::{collect_trait_constraints, super_trait_cycle},
            is_goal_satisfiable, GoalSatisfiability,
        },
        ty_lower::lower_hir_ty,
        visitor::TyVisitable,
    },
    HirAnalysisDb,
};

/// This function implements analysis for the ADT definition.
/// The analysis includes the following:
/// - Check if the types in the ADT is well-formed.
/// - Check if the trait instantiation appears in the ADT is well-formed.
/// - Check if the field types are fully applied(i.e., these types should have
///   `*` kind).
/// - Check if the types in the ADT satisfies the constraints which is required
///   in type application.
/// - Check if the trait instantiations in the ADT satisfies the constraints.
/// - Check if the recursive types has indirect type wrapper like pointer.
#[salsa::tracked(return_ref)]
pub fn analyze_adt<'db>(
    db: &'db dyn HirAnalysisDb,
    adt_ref: AdtRef<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let analyzer = DefAnalyzer::for_adt(db, adt_ref);
    analyzer.analyze()
}

/// This function implements analysis for the trait definition.
/// The analysis includes the following:
/// - Check if the types appear in the trait is well-formed.
/// - Check if the trait instantiation appears in the trait is well-formed.
/// - Check if the types in the trait satisfy the constraints which is required
///   in type application.
/// - Check if the trait instantiations in the trait satisfies the constraints.
#[salsa::tracked(return_ref)]
pub fn analyze_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let analyzer = DefAnalyzer::for_trait(db, trait_);
    analyzer.analyze()
}

/// This function implements analysis for the trait implementation definition.
/// The analysis include the following:
/// - Check if the types appear in the trait impl is well-formed.
/// - Check if the trait instantiation appears in the trait impl is well-formed.
/// - Check if the types in the trait impl satisfy the constraints which is
///   required in type application.
/// - Check if the trait instantiations in the trait impl satisfies the
///   constraints.
/// - Check if the conflict doesn't occur.
/// - Check if the trait or type is included in the ingot which contains the
///   impl trait.
#[salsa::tracked(return_ref)]
pub fn analyze_impl_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let implementor = match analyze_impl_trait_specific_error(db, impl_trait) {
        Ok(implementor) => implementor,
        Err(diags) => {
            return diags;
        }
    };

    let mut diags = ImplTraitMethodAnalyzer::new(db, implementor.instantiate_identity()).analyze();

    let analyzer = DefAnalyzer::for_trait_impl(db, implementor.instantiate_identity());
    let def_diags = analyzer.analyze();

    diags.extend(def_diags);
    diags
}

#[salsa::tracked(return_ref)]
pub fn analyze_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: HirImpl<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let Some(hir_ty) = impl_.ty(db).to_opt() else {
        return Vec::new();
    };
    let ty = lower_hir_ty(db, hir_ty, impl_.scope());

    let analyzer = DefAnalyzer::for_impl(db, impl_, ty);
    analyzer.analyze()
}

#[salsa::tracked(return_ref)]
pub fn analyze_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let Some(func_def) = lower_func(db, func) else {
        return Vec::new();
    };

    let analyzer = DefAnalyzer::for_func(db, func_def);
    analyzer.analyze()
}

pub struct DefAnalyzer<'db> {
    db: &'db dyn HirAnalysisDb,
    def: DefKind<'db>,
    self_ty: Option<TyId<'db>>,
    diags: Vec<TyDiagCollection<'db>>,
    assumptions: PredicateListId<'db>,
    current_ty: Option<(TyId<'db>, DynLazySpan<'db>)>,
}

impl<'db> DefAnalyzer<'db> {
    fn for_adt(db: &'db dyn HirAnalysisDb, adt: AdtRef<'db>) -> Self {
        let def = lower_adt(db, adt);
        let assumptions = collect_adt_constraints(db, def).instantiate_identity();
        Self {
            db,
            def: def.into(),
            self_ty: None,
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    fn for_trait(db: &'db dyn HirAnalysisDb, trait_: Trait<'db>) -> Self {
        let def = lower_trait(db, trait_);
        let assumptions = collect_trait_constraints(db, def).instantiate_identity();
        Self {
            db,
            def: def.into(),
            self_ty: def.self_param(db).into(),
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    fn for_impl(db: &'db dyn HirAnalysisDb, impl_: HirImpl<'db>, ty: TyId<'db>) -> Self {
        let assumptions = collect_impl_block_constraints(db, impl_).instantiate_identity();
        let def = DefKind::Impl(impl_);
        Self {
            db,
            def,
            self_ty: ty.into(),
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    fn for_trait_impl(db: &'db dyn HirAnalysisDb, implementor: Implementor<'db>) -> Self {
        let assumptions = implementor.constraints(db);
        Self {
            db,
            def: implementor.into(),
            self_ty: implementor.self_ty(db).into(),
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    fn for_func(db: &'db dyn HirAnalysisDb, func: FuncDef<'db>) -> Self {
        let hir_db = db;
        let assumptions = collect_func_def_constraints(db, func, true).instantiate_identity();
        let self_ty = match func
            .hir_func_def(db)
            .unwrap()
            .scope()
            .parent(hir_db)
            .unwrap()
        {
            ScopeId::Item(ItemKind::Trait(trait_)) => lower_trait(db, trait_).self_param(db).into(),
            ScopeId::Item(ItemKind::ImplTrait(impl_trait)) => {
                match impl_trait.ty(hir_db).to_opt() {
                    Some(hir_ty) => lower_hir_ty(db, hir_ty, impl_trait.scope()).into(),
                    _ => TyId::invalid(db, InvalidCause::Other).into(),
                }
            }
            ScopeId::Item(ItemKind::Impl(impl_)) => match impl_.ty(hir_db).to_opt() {
                Some(hir_ty) => lower_hir_ty(db, hir_ty, impl_.scope()).into(),
                None => TyId::invalid(db, InvalidCause::Other).into(),
            },
            _ => None,
        };

        Self {
            db,
            def: func.into(),
            self_ty,
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    /// This method verifies if
    /// 1. the given `ty` has `*` kind.
    /// 2. the given `ty` is not const type
    ///
    /// TODO: This method is a stop-gap implementation until we design a true
    /// const type system.
    fn verify_term_type_kind(&mut self, ty: HirTyId<'db>, span: DynLazySpan<'db>) -> bool {
        let ty = lower_hir_ty(self.db, ty, self.scope());
        if !ty.has_star_kind(self.db) {
            self.diags.push(TyLowerDiag::ExpectedStarKind(span).into());
            false
        } else if ty.is_const_ty(self.db) {
            self.diags
                .push(TyLowerDiag::NormalTypeExpected { span, given: ty }.into());
            false
        } else {
            true
        }
    }

    // Check if the same generic parameter is already defined in the parent item.
    // Other name conflict check is done in the name resolution.
    //
    // This check is necessary because the conflict rule
    // for the generic parameter is the exceptional case where shadowing shouldn't
    // occur.
    fn verify_method_generic_param_conflict(
        &mut self,
        params: GenericParamListId<'db>,
        span: LazyGenericParamListSpan<'db>,
    ) -> bool {
        let mut is_conflict = false;
        for (i, param) in params.data(self.db).iter().enumerate() {
            if let Some(name) = param.name().to_opt() {
                let scope = self.scope();
                let parent_scope = scope.parent_item(self.db).unwrap().scope();
                let path = PathId::from_ident(self.db, name);

                match resolve_path(self.db, path, parent_scope, None, false) {
                    Ok(r @ PathRes::Ty(ty)) if ty.is_param(self.db) => {
                        self.diags.push(
                            TyLowerDiag::GenericParamAlreadyDefinedInParent {
                                span: span.param(i).into(),
                                conflict_with: r.name_span(self.db).unwrap(),
                                name,
                            }
                            .into(),
                        );
                        is_conflict = true;
                    }
                    _ => {}
                }
            }
        }

        !is_conflict
    }

    fn verify_self_type(&mut self, self_ty: HirTyId<'db>, span: DynLazySpan<'db>) -> bool {
        let Some(expected_ty) = self.self_ty else {
            return false;
        };

        let param_ty = lower_hir_ty(self.db, self_ty, self.def.scope(self.db));
        if !param_ty.has_invalid(self.db) && !expected_ty.has_invalid(self.db) {
            let (expected_base_ty, expected_param_ty_args) = expected_ty.decompose_ty_app(self.db);
            let (param_base_ty, param_ty_args) = param_ty.decompose_ty_app(self.db);

            if param_base_ty != expected_base_ty {
                self.diags.push(
                    ImplDiag::InvalidSelfType {
                        span: span.clone(),
                        expected: expected_ty,
                        given: param_ty,
                    }
                    .into(),
                );
                return false;
            }

            for (expected_arg, param_arg) in expected_param_ty_args.iter().zip(param_ty_args.iter())
            {
                if expected_arg != param_arg {
                    self.diags.push(
                        ImplDiag::InvalidSelfType {
                            span,
                            expected: expected_ty,
                            given: param_ty,
                        }
                        .into(),
                    );
                    return false;
                }
            }
        }

        true
    }

    fn check_method_conflict(&mut self, func: FuncDef<'db>) -> bool {
        let self_ty = func
            .receiver_ty(self.db)
            .map_or_else(|| self.self_ty.unwrap(), |ty| ty.instantiate_identity());

        if self_ty.has_invalid(self.db) {
            return true;
        }

        for &cand in probe_method(
            self.db,
            self.scope().ingot(self.db),
            Canonical::new(self.db, self_ty),
            func.name(self.db),
        ) {
            if cand != func {
                self.diags.push(
                    ImplDiag::ConflictMethodImpl {
                        primary: func,
                        conflict_with: cand,
                    }
                    .into(),
                );
                return false;
            }
        }

        true
    }

    fn scope(&self) -> ScopeId<'db> {
        self.def.scope(self.db)
    }

    fn analyze(mut self) -> Vec<TyDiagCollection<'db>> {
        match self.def {
            DefKind::Adt(def) => match def.adt_ref(self.db) {
                AdtRef::Struct(struct_) => {
                    let mut ctxt = VisitorCtxt::with_struct(self.db, struct_);
                    self.visit_struct(&mut ctxt, struct_);
                }

                AdtRef::Enum(enum_) => {
                    let mut ctxt = VisitorCtxt::with_enum(self.db, enum_);
                    self.visit_enum(&mut ctxt, enum_);
                }

                AdtRef::Contract(contract) => {
                    let mut ctxt = VisitorCtxt::with_contract(self.db, contract);
                    self.visit_contract(&mut ctxt, contract);
                }
            },

            DefKind::Trait(trait_) => {
                let trait_ = trait_.trait_(self.db);
                let mut ctxt = VisitorCtxt::with_trait(self.db, trait_);
                self.visit_trait(&mut ctxt, trait_);
            }

            DefKind::ImplTrait(implementor) => {
                let impl_trait = implementor.hir_impl_trait(self.db);
                let mut ctxt = VisitorCtxt::with_impl_trait(self.db, impl_trait);
                self.visit_impl_trait(&mut ctxt, impl_trait);
            }

            DefKind::Impl(hir_impl) => {
                let mut ctxt = VisitorCtxt::with_impl(self.db, hir_impl);
                self.visit_impl(&mut ctxt, hir_impl)
            }

            DefKind::Func(func) => {
                let hir_func = func.hir_func_def(self.db).unwrap();
                let mut ctxt = VisitorCtxt::with_func(self.db, hir_func);
                self.visit_func(&mut ctxt, hir_func);
            }
        }

        self.diags
    }
}

impl<'db> Visitor<'db> for DefAnalyzer<'db> {
    // We don't need to traverse the nested item, each item kinds are explicitly
    // handled(e.g, `visit_trait` or `visit_enum`).
    fn visit_item(&mut self, _ctxt: &mut VisitorCtxt<'db, LazyItemSpan>, _item: ItemKind<'db>) {}

    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'db, LazyTySpan<'db>>, hir_ty: HirTyId<'db>) {
        let ty = lower_hir_ty(self.db, hir_ty, self.scope());
        let span = ctxt.span().unwrap();
        if let Some(diag) = ty.emit_diag(self.db, span.clone().into()) {
            self.diags.push(diag)
        } else if let Some(diag) =
            ty.emit_wf_diag(self.db, ctxt.ingot(), self.assumptions, span.into())
        {
            self.diags.push(diag)
        }
    }

    fn visit_where_predicate(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyWherePredicateSpan<'db>>,
        pred: &hir::hir_def::WherePredicate<'db>,
    ) {
        let Some(hir_ty) = pred.ty.to_opt() else {
            return;
        };

        let ty = lower_hir_ty(self.db, hir_ty, self.scope());

        if ty.is_const_ty(self.db) {
            let diag =
                TraitConstraintDiag::ConstTyBound(ctxt.span().unwrap().ty().into(), ty).into();
            self.diags.push(diag);
            return;
        }

        if !ty.has_invalid(self.db) && !ty.has_param(self.db) {
            let diag =
                TraitConstraintDiag::ConcreteTypeBound(ctxt.span().unwrap().ty().into(), ty).into();
            self.diags.push(diag);
            return;
        }

        self.current_ty = Some((ty, ctxt.span().unwrap().ty().into()));
        walk_where_predicate(self, ctxt, pred);
    }

    fn visit_field_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFieldDefSpan<'db>>,
        field: &FieldDef<'db>,
    ) {
        let Some(ty) = field.ty.to_opt() else {
            return;
        };

        if !self.verify_term_type_kind(ty, ctxt.span().unwrap().ty().into()) {
            return;
        }

        let Some(name) = field.name.to_opt() else {
            return;
        };

        // Checks if the field type is the same as the type of const type parameter.
        if let Some(const_ty) = find_const_ty_param(self.db, name, ctxt.scope()) {
            let const_ty_ty = const_ty.ty(self.db);
            let field_ty = lower_hir_ty(self.db, ty, ctxt.scope());
            if !const_ty_ty.has_invalid(self.db)
                && !field_ty.has_invalid(self.db)
                && field_ty != const_ty_ty
            {
                self.diags.push(
                    TyLowerDiag::ConstTyMismatch {
                        span: ctxt.span().unwrap().ty().into(),
                        expected: const_ty_ty,
                        given: field_ty,
                    }
                    .into(),
                );
                return;
            }
        }

        walk_field_def(self, ctxt, field);
    }

    fn visit_variant_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyVariantDefSpan<'db>>,
        variant: &hir::hir_def::VariantDef<'db>,
    ) {
        if let VariantKind::Tuple(tuple_id) = variant.kind {
            let span = ctxt.span().unwrap().tuple_type_moved();
            for (i, elem_ty) in tuple_id.data(self.db).iter().enumerate() {
                let Some(elem_ty) = elem_ty.to_opt() else {
                    continue;
                };

                self.verify_term_type_kind(elem_ty, span.elem_ty(i).into());
            }
        }
        walk_variant_def(self, ctxt, variant);
    }

    fn visit_generic_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyGenericParamSpan<'db>>,
        param: &hir::hir_def::GenericParam<'db>,
    ) {
        let ScopeId::GenericParam(_, idx) = ctxt.scope() else {
            unreachable!()
        };

        if let Some(name) = param.name().to_opt() {
            let scope = self.scope();
            let parent_scope = scope.parent_item(self.db).unwrap().scope();
            let path = PathId::from_ident(self.db, name);
            match resolve_path(self.db, path, parent_scope, None, false) {
                Ok(r @ PathRes::Ty(ty)) if ty.is_param(self.db) => {
                    self.diags.push(
                        TyLowerDiag::GenericParamAlreadyDefinedInParent {
                            span: ctxt.span().unwrap().into(),
                            conflict_with: r.name_span(self.db).unwrap(),
                            name,
                        }
                        .into(),
                    );
                    return;
                }
                _ => {}
            }
        }

        match param {
            GenericParam::Type(_) => {
                self.current_ty = Some((
                    self.def.original_params(self.db)[idx],
                    ctxt.span().unwrap().into_type_param().name().into(),
                ));
                walk_generic_param(self, ctxt, param)
            }
            GenericParam::Const(_) => {
                let ty = self.def.original_params(self.db)[idx];
                let Some(const_ty_param) = ty.const_ty_param(self.db) else {
                    return;
                };

                if let Some(diag) = const_ty_param
                    .emit_diag(self.db, ctxt.span().unwrap().into_const_param().ty().into())
                {
                    self.diags.push(diag)
                }
            }
        }
    }

    fn visit_kind_bound(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyKindBoundSpan<'db>>,
        bound: &hir::hir_def::KindBound,
    ) {
        let Some((ty, _)) = self.current_ty else {
            return;
        };

        let kind = lower_kind(bound);
        let former_kind = ty.kind(self.db);
        if !former_kind.does_match(&kind) {
            self.diags.push(
                TyLowerDiag::InconsistentKindBound {
                    span: ctxt.span().unwrap().into(),
                    ty,
                    old: former_kind.clone(),
                    new: kind,
                }
                .into(),
            );
        }
    }

    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTraitRefSpan<'db>>,
        trait_ref: TraitRefId<'db>,
    ) {
        let current_ty = self
            .current_ty
            .as_ref()
            .map(|(ty, _)| *ty)
            .unwrap_or(TyId::invalid(self.db, InvalidCause::Other));

        if current_ty.is_trait_self(self.db) && self.def.super_trait_cycle(self.db).is_some() {
            // Skip analysis of traits involved in cycles.
            return;
        }

        if let (Some((ty, span)), Ok(trait_inst)) = (
            &self.current_ty,
            lower_trait_ref(self.db, current_ty, trait_ref, self.scope()),
        ) {
            let expected_kind = trait_inst.def(self.db).expected_implementor_kind(self.db);
            if !expected_kind.does_match(ty.kind(self.db)) {
                self.diags.push(
                    TraitConstraintDiag::TraitArgKindMismatch {
                        span: span.clone(),
                        expected: expected_kind.clone(),
                        actual: *ty,
                    }
                    .into(),
                );
            }
        }

        if let Some(diag) = analyze_trait_ref(
            self.db,
            current_ty,
            trait_ref,
            self.scope(),
            Some(self.assumptions),
            ctxt.span().unwrap().into(),
        ) {
            self.diags.push(diag);
        } else {
            walk_trait_ref(self, ctxt, trait_ref);
        }
    }

    fn visit_super_trait_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, hir::span::item::LazySuperTraitListSpan<'db>>,
        super_traits: &[TraitRefId<'db>],
    ) {
        let DefKind::Trait(def) = self.def else {
            unreachable!()
        };
        let name_span = def.trait_(self.db).lazy_span().name().into();
        self.current_ty = Some((self.def.trait_self_param(self.db), name_span));
        walk_super_trait_list(self, ctxt, super_traits);
    }

    fn visit_impl(&mut self, ctxt: &mut VisitorCtxt<'db, LazyImplSpan<'db>>, impl_: HirImpl<'db>) {
        let Some(impl_ty) = impl_.ty(self.db).to_opt() else {
            return;
        };

        let impl_ty = lower_hir_ty(self.db, impl_ty, impl_.scope());
        if !impl_ty.is_inherent_impl_allowed(self.db, self.scope().ingot(self.db)) {
            let base = impl_ty.base_ty(self.db);
            let diag = ImplDiag::InherentImplIsNotAllowed {
                primary: ctxt.span().unwrap().target_ty().into(),
                ty: base.pretty_print(self.db).to_string(),
                is_nominal: !base.is_param(self.db),
            };

            self.diags.push(diag.into());
        }

        if let Some(ty) = impl_ty.emit_diag(self.db, ctxt.span().unwrap().target_ty().into()) {
            self.diags.push(ty);
        } else {
            walk_impl(self, ctxt, impl_);
        }
    }

    fn visit_func(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFuncSpan<'db>>,
        hir_func: hir::hir_def::Func<'db>,
    ) {
        let Some(func) = lower_func(self.db, hir_func) else {
            return;
        };

        // We need to check the conflict only when the function is defined in the `impl`
        // block since this check requires the ingot-wide method table(i.e., which is
        // not performed in name resolution phase).
        if matches!(
            ctxt.scope().parent_item(self.db).unwrap(),
            ItemKind::Impl(_)
        ) && !self.check_method_conflict(func)
        {
            return;
        }

        if !self.verify_method_generic_param_conflict(
            hir_func.generic_params(self.db),
            hir_func.lazy_span().generic_params_moved(),
        ) {
            return;
        }

        let def = std::mem::replace(&mut self.def, func.into());
        let constraints = std::mem::replace(
            &mut self.assumptions,
            collect_func_def_constraints(self.db, func, true).instantiate_identity(),
        );

        walk_func(self, ctxt, hir_func);

        if let Some(ret_ty) = hir_func.ret_ty(self.db) {
            self.verify_term_type_kind(ret_ty, hir_func.lazy_span().ret_ty().into());
        }

        self.assumptions = constraints;
        self.def = def;
    }

    fn visit_body(&mut self, _ctxt: &mut VisitorCtxt<'_, LazyBodySpan>, _body: hir::hir_def::Body) {
    }

    fn visit_func_param_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFuncParamListSpan<'db>>,
        params: FuncParamListId<'db>,
    ) {
        // Checks if the argument names are not duplicated.
        let mut already_seen: FxHashMap<IdentId, usize> = FxHashMap::default();

        for (i, param) in params.data(self.db).iter().enumerate() {
            let Some(name) = param.name.to_opt().and_then(|name| name.ident()) else {
                continue;
            };

            match already_seen.entry(name) {
                Entry::Occupied(entry) => {
                    let diag = TyLowerDiag::DuplicatedArgName {
                        primary: ctxt.span().unwrap().param(i).name().into(),
                        conflict_with: ctxt.span().unwrap().param(*entry.get()).name().into(),
                        name,
                    }
                    .into();
                    self.diags.push(diag);
                }

                Entry::Vacant(entry) => {
                    entry.insert(i);
                }
            }
        }

        walk_func_param_list(self, ctxt, params)
    }

    fn visit_func_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFuncParamSpan<'db>>,
        param: &hir::hir_def::FuncParam<'db>,
    ) {
        let Some(hir_ty) = param.ty.to_opt() else {
            return;
        };

        let ty_span: DynLazySpan = if param.is_self_param(ctxt.db()) && param.self_ty_fallback {
            ctxt.span().unwrap().name().into()
        } else {
            ctxt.span().unwrap().ty().into()
        };

        if param.is_self_param(ctxt.db()) {
            self.verify_self_type(hir_ty, ty_span.clone());
        }

        if !self.verify_term_type_kind(hir_ty, ty_span) {
            return;
        }

        walk_func_param(self, ctxt, param);
    }
}

#[salsa::tracked(return_ref)]
pub(crate) fn check_recursive_adt<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
) -> Option<Vec<AdtCycleMember<'db>>> {
    check_recursive_adt_impl(db, adt, &[])
}

pub(crate) fn check_recursive_adt_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
    chain: &[AdtCycleMember<'db>],
) -> Option<Vec<AdtCycleMember<'db>>> {
    if chain.iter().any(|m| m.adt == adt) {
        return Some(chain.to_vec());
    } else if adt.fields(db).is_empty() {
        return None;
    }

    let mut chain = chain.to_vec();
    for (field_idx, field) in adt.fields(db).iter().enumerate() {
        for (ty_idx, ty) in field.iter_types(db).enumerate() {
            for field_adt_ref in ty.instantiate_identity().collect_direct_adts(db) {
                chain.push(AdtCycleMember {
                    adt,
                    field_idx: field_idx as u16,
                    ty_idx: ty_idx as u16,
                });

                if let Some(cycle) =
                    check_recursive_adt_impl(db, lower_adt(db, field_adt_ref), &chain)
                {
                    if cycle.iter().any(|m| m.adt == adt) {
                        return Some(cycle);
                    }
                }
                chain.pop();
            }
        }
    }
    None
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AdtCycleMember<'db> {
    pub adt: AdtDef<'db>,
    pub field_idx: u16,
    pub ty_idx: u16,
}

impl<'db> TyId<'db> {
    /// Collect all adts inside types which are not wrapped by indirect type
    /// wrapper like pointer or reference.
    fn collect_direct_adts(self, db: &'db dyn HirAnalysisDb) -> FxHashSet<AdtRef<'db>> {
        struct AdtCollector<'db> {
            db: &'db dyn HirAnalysisDb,
            adts: FxHashSet<AdtRef<'db>>,
        }

        impl<'db> TyVisitor<'db> for AdtCollector<'db> {
            fn db(&self) -> &'db dyn HirAnalysisDb {
                self.db
            }

            fn visit_app(&mut self, abs: TyId<'db>, arg: TyId<'db>) {
                if !abs.is_indirect(self.db) {
                    walk_ty(self, arg)
                }
            }

            fn visit_adt(&mut self, adt: AdtDef<'db>) {
                self.adts.insert(adt.adt_ref(self.db));
            }
        }

        let mut collector = AdtCollector {
            db,
            adts: FxHashSet::default(),
        };

        self.visit_with(&mut collector);
        collector.adts
    }
}

fn analyze_trait_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    self_ty: TyId<'db>,
    trait_ref: TraitRefId<'db>,
    scope: ScopeId<'db>,
    assumptions: Option<PredicateListId<'db>>,
    span: DynLazySpan<'db>,
) -> Option<TyDiagCollection<'db>> {
    let trait_inst = match lower_trait_ref(db, self_ty, trait_ref, scope) {
        Ok(trait_ref) => trait_ref,

        Err(TraitRefLowerError::ArgNumMismatch { expected, given }) => {
            return Some(
                TraitConstraintDiag::TraitArgNumMismatch {
                    span,
                    expected,
                    given,
                }
                .into(),
            );
        }

        Err(TraitRefLowerError::ArgKindMisMatch { expected, given }) => {
            return Some(
                TraitConstraintDiag::TraitArgKindMismatch {
                    span,
                    expected,
                    actual: given,
                }
                .into(),
            );
        }

        Err(TraitRefLowerError::ArgTypeMismatch { expected, given }) => match (expected, given) {
            (Some(expected), Some(given)) => {
                return Some(
                    TyLowerDiag::ConstTyMismatch {
                        span,
                        expected,
                        given,
                    }
                    .into(),
                )
            }

            (Some(expected), None) => {
                return Some(TyLowerDiag::ConstTyExpected { span, expected }.into())
            }

            (None, Some(given)) => {
                return Some(TyLowerDiag::NormalTypeExpected { span, given }.into())
            }

            (None, None) => unreachable!(),
        },

        Err(TraitRefLowerError::Other) => {
            return None;
        }
    };

    if let Some(assumptions) = assumptions {
        trait_inst.emit_sat_diag(db, scope.ingot(db), assumptions, span)
    } else {
        None
    }
}

#[derive(Clone, Copy, Debug, derive_more::From)]
enum DefKind<'db> {
    Adt(AdtDef<'db>),
    Trait(TraitDef<'db>),
    ImplTrait(Implementor<'db>),
    Impl(HirImpl<'db>),
    Func(FuncDef<'db>),
}

impl<'db> DefKind<'db> {
    fn original_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        match self {
            Self::Adt(def) => def.original_params(db),
            Self::Trait(def) => def.original_params(db),
            Self::ImplTrait(def) => def.original_params(db),
            Self::Impl(hir_impl) => collect_generic_params(db, hir_impl.into()).params(db),
            Self::Func(def) => def.explicit_params(db),
        }
    }

    fn trait_self_param(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        if let Self::Trait(def) = self {
            def.self_param(db)
        } else {
            panic!()
        }
    }

    fn super_trait_cycle(self, db: &'db dyn HirAnalysisDb) -> Option<&'db Vec<TraitDef<'db>>> {
        if let Self::Trait(def) = self {
            super_trait_cycle(db, def).as_ref()
        } else {
            None
        }
    }

    fn scope(self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        match self {
            Self::Adt(def) => def.adt_ref(db).scope(),
            Self::Trait(def) => def.trait_(db).scope(),
            Self::ImplTrait(def) => def.hir_impl_trait(db).scope(),
            Self::Impl(hir_impl) => hir_impl.scope(),
            Self::Func(def) => def.scope(db),
        }
    }
}

/// This function analyzes the trait impl specific error.
/// 1. If the trait ref is well-formed except for the satisfiability.
/// 2. If implementor type is well-formed except for the satisfiability.
/// 3. If the ingot contains impl trait is the same as the ingot which contains
///    either the type or trait.
/// 4. If conflict occurs.
/// 5. If implementor type satisfies the required kind bound.
/// 6. If implementor type satisfies the required trait bound.
fn analyze_impl_trait_specific_error<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Result<Binder<Implementor<'db>>, Vec<TyDiagCollection<'db>>> {
    let mut diags = vec![];
    let hir_db = db;
    // We don't need to report error because it should be reported from the parser.
    let (Some(trait_ref), Some(ty)) = (
        impl_trait.trait_ref(hir_db).to_opt(),
        impl_trait.ty(hir_db).to_opt(),
    ) else {
        return Err(diags);
    };

    // 1. Checks if implementor type is well-formed except for the satisfiability.
    let ty = lower_hir_ty(db, ty, impl_trait.scope());
    if let Some(diag) = ty.emit_diag(db, impl_trait.lazy_span().ty().into()) {
        diags.push(diag);
    }

    // 2. Checks if the trait ref is well-formed except for the satisfiability.
    if let Some(diag) = analyze_trait_ref(
        db,
        ty,
        trait_ref,
        impl_trait.scope(),
        None,
        impl_trait.lazy_span().trait_ref().into(),
    ) {
        diags.push(diag);
    }

    // If there is any error at the point, it means that `Implementor` is not
    // well-formed and no more analysis is needed to reduce the amount of error
    // messages.
    if !diags.is_empty() || ty.has_invalid(db) {
        return Err(diags);
    }

    let trait_inst = match lower_trait_ref(db, ty, trait_ref, impl_trait.scope()) {
        Ok(trait_inst) => trait_inst,
        Err(_) => return Err(vec![]),
    };

    // 3. Check if the ingot containing impl trait is the same as the ingot which
    //    contains either the type or trait.
    let impl_trait_ingot = impl_trait.top_mod(hir_db).ingot(hir_db);
    if Some(impl_trait_ingot) != ty.ingot(db) && impl_trait_ingot != trait_inst.def(db).ingot(db) {
        diags.push(TraitLowerDiag::ExternalTraitForExternalType(impl_trait).into());
        return Err(diags);
    }

    let trait_env = ingot_trait_env(db, impl_trait_ingot);
    let Some(implementor) = trait_env.map_impl_trait(impl_trait) else {
        // Lower impl trait never fails if the trait ref and implementor type is
        // well-formed.
        let current_impl = lower_impl_trait(db, impl_trait).unwrap();

        // 4. Checks if conflict occurs.
        // If there is no implementor type even if the trait ref and implementor type is
        // well-formed, it means that the conflict does occur.
        analyze_conflict_impl(db, current_impl, &mut diags);
        return Err(diags);
    };

    fn analyze_conflict_impl<'db>(
        db: &'db dyn HirAnalysisDb,
        implementor: Binder<Implementor<'db>>,
        diags: &mut Vec<TyDiagCollection<'db>>,
    ) {
        let trait_ = implementor.skip_binder().trait_(db);
        let env = ingot_trait_env(db, trait_.ingot(db));
        let Some(impls) = env.impls.get(&trait_.def(db)) else {
            return;
        };

        for cand in impls {
            if does_impl_trait_conflict(db, *cand, implementor) {
                diags.push(
                    TraitLowerDiag::ConflictTraitImpl {
                        primary: cand.skip_binder().hir_impl_trait(db),
                        conflict_with: implementor.skip_binder().hir_impl_trait(db),
                    }
                    .into(),
                );

                return;
            }
        }
    }

    // 5. Checks if implementor type satisfies the kind bound which is required by
    //    the trait.
    let expected_kind = implementor
        .instantiate_identity()
        .trait_def(db)
        .expected_implementor_kind(db);
    if ty.kind(db) != expected_kind {
        diags.push(
            TraitConstraintDiag::TraitArgKindMismatch {
                span: impl_trait.lazy_span().ty().into(),
                expected: expected_kind.clone(),
                actual: implementor.instantiate_identity().self_ty(db),
            }
            .into(),
        );
        return Err(diags);
    }

    let trait_def = trait_inst.def(db);
    let trait_constraints =
        collect_trait_constraints(db, trait_def).instantiate(db, trait_inst.args(db));
    let assumptions = implementor.instantiate_identity().constraints(db);

    let mut is_satisfied = |goal: TraitInstId<'db>, span: DynLazySpan<'db>| {
        let canonical_goal = Canonicalized::new(db, goal);
        match is_goal_satisfiable(db, impl_trait_ingot, canonical_goal.value, assumptions) {
            GoalSatisfiability::Satisfied(_) | GoalSatisfiability::ContainsInvalid => {}
            GoalSatisfiability::NeedsConfirmation(_) => unreachable!(),
            GoalSatisfiability::UnSat(subgoal) => {
                diags.push(
                    TraitConstraintDiag::TraitBoundNotSat {
                        span,
                        primary_goal: goal,
                        unsat_subgoal: subgoal.map(|subgoal| subgoal.value),
                    }
                    .into(),
                );
            }
        }
    };

    // 6. Checks if the trait inst is WF.
    let trait_ref_span: DynLazySpan = impl_trait.lazy_span().trait_ref_moved().into();
    for &goal in trait_constraints.list(db) {
        is_satisfied(goal, trait_ref_span.clone());
    }

    // 7. Checks if the implementor ty satisfies the super trait constraints.
    let target_ty_span: DynLazySpan = impl_trait.lazy_span().ty().into();
    for &super_trait in trait_def.super_traits(db) {
        let super_trait = super_trait.instantiate(db, trait_inst.args(db));
        is_satisfied(super_trait, target_ty_span.clone())
    }

    if diags.is_empty() {
        Ok(implementor)
    } else {
        Err(diags)
    }
}

struct ImplTraitMethodAnalyzer<'db> {
    db: &'db dyn HirAnalysisDb,
    diags: Vec<TyDiagCollection<'db>>,
    implementor: Implementor<'db>,
}

impl<'db> ImplTraitMethodAnalyzer<'db> {
    fn new(db: &'db dyn HirAnalysisDb, implementor: Implementor<'db>) -> Self {
        Self {
            db,
            diags: vec![],
            implementor,
        }
    }

    fn analyze(mut self) -> Vec<TyDiagCollection<'db>> {
        let impl_methods = self.implementor.methods(self.db);
        let hir_trait = self.implementor.trait_def(self.db).trait_(self.db);
        let trait_methods = self.implementor.trait_def(self.db).methods(self.db);
        let mut required_methods: IndexSet<_> = trait_methods
            .iter()
            .filter_map(|(name, &trait_method)| {
                if !trait_method.has_default_impl(self.db) {
                    Some(*name)
                } else {
                    None
                }
            })
            .collect();

        for (name, impl_m) in impl_methods {
            let Some(trait_m) = trait_methods.get(name) else {
                self.diags.push(
                    ImplDiag::MethodNotDefinedInTrait {
                        primary: self
                            .implementor
                            .hir_impl_trait(self.db)
                            .lazy_span()
                            .trait_ref()
                            .into(),
                        method_name: *name,
                        trait_: hir_trait,
                    }
                    .into(),
                );
                continue;
            };

            compare_impl_method(
                self.db,
                *impl_m,
                *trait_m,
                self.implementor.trait_(self.db),
                &mut self.diags,
            );

            required_methods.remove(name);
        }

        if !required_methods.is_empty() {
            self.diags.push(
                ImplDiag::NotAllTraitItemsImplemented {
                    primary: self
                        .implementor
                        .hir_impl_trait(self.db)
                        .lazy_span()
                        .ty_moved()
                        .into(),
                    not_implemented: required_methods.into_iter().collect(),
                }
                .into(),
            );
        }

        self.diags
    }
}

fn find_const_ty_param<'db>(
    db: &'db dyn HirAnalysisDb,
    ident: IdentId<'db>,
    scope: ScopeId<'db>,
) -> Option<ConstTyId<'db>> {
    let path = PathId::from_ident(db, ident);
    let Ok(PathRes::Ty(ty)) = resolve_path(db, path, scope, None, true) else {
        return None;
    };
    match ty.data(db) {
        TyData::ConstTy(const_ty) => Some(*const_ty),
        _ => None,
    }
}
