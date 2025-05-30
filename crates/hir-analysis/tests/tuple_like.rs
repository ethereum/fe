mod test_db;

use dir_test::{dir_test, Fixture};
use fe_hir_analysis::name_resolution::ResolvedVariant;

use hir::{
    hir_def::{EnumVariant, Func, ItemKind, VariantKind},
    visitor::{walk_func, walk_item, Visitor, VisitorCtxt},
};
use std::path::Path;
use test_db::{HirAnalysisTestDb, HirPropertyFormatter};
use test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/tuple_like",
    glob: "*.fe"
)]
fn tuple_like_analysis(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, mut prop_formatter) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let mut ctxt = VisitorCtxt::with_top_mod(&db, top_mod);
    TupleLikeAnalyzer {
        db: &db,
        top_mod,
        prop_formatter: &mut prop_formatter,
    }
    .visit_top_mod(&mut ctxt, top_mod);

    let res = prop_formatter.finish(&db);
    snap_test!(res, fixture.path());
}

struct TupleLikeAnalyzer<'db, 'a> {
    db: &'db HirAnalysisTestDb,
    top_mod: hir::hir_def::TopLevelMod<'db>,
    prop_formatter: &'a mut HirPropertyFormatter<'db>,
}

impl<'db> Visitor<'db> for TupleLikeAnalyzer<'db, '_> {
    fn visit_func(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, hir::span::item::LazyFuncSpan<'db>>,
        func: Func<'db>,
    ) {
        // Analyze function return type if it's specified
        if let Some(ret_ty_id) = func.ret_ty(self.db) {
            let ret_ty =
                fe_hir_analysis::ty::ty_lower::lower_hir_ty(self.db, ret_ty_id, func.scope());

            let arity = ret_ty.field_count(self.db);
            let field_types = ret_ty.field_types(self.db);

            // Only annotate if it's actually tuple-like or interesting
            if arity > 0 || self.is_likely_tuple_type(ret_ty) {
                let func_name = func
                    .name(self.db)
                    .to_opt()
                    .map(|n| n.data(self.db).to_string())
                    .unwrap_or_else(|| "anonymous".to_string());

                let annotation = match arity {
                    0 => format!("Tuple return type in '{}': unit type (arity: 0)", func_name),
                    1 => format!(
                        "Tuple return type in '{}': single element (arity: 1, {} field types)",
                        func_name,
                        field_types.len()
                    ),
                    2 => format!(
                        "Tuple return type in '{}': pair (arity: 2, {} field types)",
                        func_name,
                        field_types.len()
                    ),
                    3 => format!(
                        "Tuple return type in '{}': triple (arity: 3, {} field types)",
                        func_name,
                        field_types.len()
                    ),
                    n => format!(
                        "Tuple return type in '{}': {}-tuple (arity: {}, {} field types)",
                        func_name,
                        n,
                        n,
                        field_types.len()
                    ),
                };

                if let Some(span) = ctxt.span() {
                    self.prop_formatter
                        .push_prop(self.top_mod, span.into(), annotation);
                }

                // Validate consistency
                if field_types.len() != arity {
                    let error_msg = format!(
                        "INCONSISTENCY in '{}': field_types.len() ({}) != arity ({})",
                        func_name,
                        field_types.len(),
                        arity
                    );
                    if let Some(span) = ctxt.span() {
                        self.prop_formatter
                            .push_prop(self.top_mod, span.into(), error_msg);
                    }
                }
            }
        }

        walk_func(self, ctxt, func);
    }

    fn visit_item(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, hir::span::item::LazyItemSpan<'db>>,
        item: ItemKind<'db>,
    ) {
        if let ItemKind::Enum(enum_def) = item {
            let enum_name = enum_def
                .name(self.db)
                .to_opt()
                .map(|n| n.data(self.db).to_string())
                .unwrap_or_else(|| "unknown".to_string());

            let variants = enum_def.variants(self.db);

            for (idx, variant_data) in variants.data(self.db).iter().enumerate() {
                let variant = EnumVariant::new(enum_def, idx);
                let variant_name = variant_data
                    .name
                    .to_opt()
                    .map(|n| n.data(self.db).to_string())
                    .unwrap_or_else(|| format!("variant_{}", idx));

                // Get the enum type for the resolved variant
                // We need to create a proper resolved variant but this requires complex setup
                // For now, let's analyze what we can determine about the variant structure

                match variant.kind(self.db) {
                    VariantKind::Unit => {
                        // Unit variants are tuple-like with arity 0
                        let annotation = format!(
                            "Tuple enum variant '{}::{}': unit variant (arity: 0)",
                            enum_name, variant_name
                        );
                        if let Some(span) = ctxt.span() {
                            self.prop_formatter
                                .push_prop(self.top_mod, span.into(), annotation);
                        }
                    }
                    VariantKind::Tuple(fields) => {
                        // Tuple variants are tuple-like with arity = number of fields
                        let field_count = fields.data(self.db).len();
                        let annotation = format!(
                            "Tuple enum variant '{}::{}': tuple variant (arity: {})",
                            enum_name, variant_name, field_count
                        );
                        if let Some(span) = ctxt.span() {
                            self.prop_formatter
                                .push_prop(self.top_mod, span.into(), annotation);
                        }

                        // Test actual tuple field access if we can create a resolved variant
                        if let Ok(resolved_variant) =
                            self.try_create_resolved_variant(enum_def, variant)
                        {
                            let actual_arity = resolved_variant.field_count(self.db);
                            let actual_field_types = resolved_variant.field_types(self.db);

                            if actual_arity != field_count
                                || actual_field_types.len() != field_count
                            {
                                let consistency_msg = format!("Tuple validation for '{}::{}': expected arity {}, got arity {}, field_types.len() {}", 
                                                             enum_name, variant_name, field_count, actual_arity, actual_field_types.len());
                                if let Some(span) = ctxt.span() {
                                    self.prop_formatter.push_prop(
                                        self.top_mod,
                                        span.into(),
                                        consistency_msg,
                                    );
                                }
                            }
                        }
                    }
                    VariantKind::Record(_) => {
                        // Record variants are not tuple-like
                        let annotation = format!(
                            "Enum variant '{}::{}': record variant (not tuple-like)",
                            enum_name, variant_name
                        );
                        if let Some(span) = ctxt.span() {
                            self.prop_formatter
                                .push_prop(self.top_mod, span.into(), annotation);
                        }
                    }
                }
            }
        }

        walk_item(self, ctxt, item);
    }
}

impl<'db> TupleLikeAnalyzer<'db, '_> {
    fn is_likely_tuple_type(&self, _ty: fe_hir_analysis::ty::ty_def::TyId<'db>) -> bool {
        // For now, we'll be conservative and only annotate when we have positive arity
        // since we can't access private decompose_ty_app method
        false
    }

    fn try_create_resolved_variant(
        &self,
        _enum_def: hir::hir_def::Enum<'db>,
        _variant: EnumVariant<'db>,
    ) -> Result<ResolvedVariant<'db>, ()> {
        // Try to create a resolved variant for TupleLike analysis
        // This is complex because we need proper type resolution
        // For now, return an error to skip the advanced analysis
        Err(())
    }
}

#[cfg(test)]
mod unit_tests {
    use super::*;
    use fe_hir_analysis::ty::ty_def::{InvalidCause, TyId};

    #[test]
    fn test_tuple_field_access() {
        let db = HirAnalysisTestDb::default();

        // Test with a basic invalid type (safest to use)
        let dummy_ty = TyId::invalid(&db, InvalidCause::Other);

        // Test field access methods
        let arity = dummy_ty.field_count(&db);
        let field_types = dummy_ty.field_types(&db);

        // Basic assertions
        assert_eq!(arity, 0); // Invalid types have no fields
        assert_eq!(field_types.len(), 0);
    }

    #[test]
    fn test_tuple_basic_api() {
        let db = HirAnalysisTestDb::default();

        // Test with a basic type
        let dummy_ty = TyId::invalid(&db, InvalidCause::Other);

        // Basic API should work without errors
        let arity = dummy_ty.field_count(&db);
        let field_types = dummy_ty.field_types(&db);

        // Expect 0 arity and empty field types for invalid type
        assert_eq!(arity, 0);
        assert_eq!(field_types.len(), 0);
        assert!(field_types.is_empty());
    }
}
