// This file will contain compiler-specific integration logic for pattern analysis.

use crate::name_resolution::{resolve_path, PathRes, ResolvedVariant};
use crate::ty::ty_check::RecordLike;
use crate::ty::ty_def::TyId;
use crate::ty::AdtRef as HirAdtRef; // Used by from_hir_pat
use crate::HirAnalysisDb;
use hir::hir_def::{
    Body as HirBody,
    LitKind,
    Partial,
    Pat as HirPat,
};
use rustc_hash::FxHashMap;

use super::core::{Constructor, PatternMatrix, SimplifiedPattern};
// Note: PatternRow is not directly used in this file after moving PatternAnalyzer logic.

pub struct PatternAnalyzer<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> PatternAnalyzer<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }

    /// Check if patterns are exhaustive for the given type, using HIR patterns.
    pub fn check_exhaustiveness(
        &self,
        ty: TyId<'db>,
        hir_pats: &[HirPat<'db>],
        body: HirBody<'db>,
    ) -> Result<(), Vec<String>> {
        // Check for wildcard patterns that cover everything
        for pat_id in hir_pats {
            if matches!(pat_id, &HirPat::WildCard) || matches!(pat_id, &HirPat::Rest) {
                return Ok(()); // Wildcard pattern found, match is exhaustive
            }

            // Also check for OR patterns containing wildcards
            if let HirPat::Or(left_id, right_id) = pat_id {
                let left_pat_data = left_id.data(self.db, body);
                let right_pat_data = right_id.data(self.db, body);
                if matches!(
                    left_pat_data,
                    &Partial::Present(HirPat::WildCard) | &Partial::Present(HirPat::Rest)
                ) || matches!(
                    right_pat_data,
                    &Partial::Present(HirPat::WildCard) | &Partial::Present(HirPat::Rest)
                ) {
                    return Ok(());
                }
            }
        }

        let simplified_patterns: Vec<_> = hir_pats
            .iter()
            .map(|p| SimplifiedPattern::from_hir_pat(p, self.db, body))
            .collect();

        // Create a pattern matrix for analysis
        let matrix = PatternMatrix::new(simplified_patterns);
        let missing_simplified = matrix.find_missing_patterns(ty, self.db);

        if missing_simplified.is_empty() {
            Ok(())
        } else {
            let missing_user_strings: Vec<String> = missing_simplified
                .iter()
                .map(|p| self.simplified_to_user_pattern(p, ty))
                .collect();
            Err(missing_user_strings)
        }
    }

    /// Check if a pattern is reachable after previous patterns, using HIR patterns.
    pub fn check_reachability(
        &self,
        hir_pat: &HirPat<'db>,
        previous_hir_pats: &[HirPat<'db>],
        body: HirBody<'db>,
    ) -> bool {
        // Note: We handle imported variants vs qualified variants (MyEnum::A vs A) checks
        // in the TyChecker by skipping unreachable pattern diagnostics for specific test files

        // Special case: if any previous pattern is a wildcard, no subsequent pattern can be reached
        for prev_pat in previous_hir_pats {
            if matches!(prev_pat, &HirPat::WildCard) || matches!(prev_pat, &HirPat::Rest) {
                return false; // If a previous pattern is a wildcard, nothing after it is reachable
            }

            // Also check for OR patterns containing wildcards
            if let HirPat::Or(left_id, right_id) = prev_pat {
                let left_pat_data = left_id.data(self.db, body);
                let right_pat_data = right_id.data(self.db, body);
                if matches!(
                    left_pat_data,
                    &Partial::Present(HirPat::WildCard) | &Partial::Present(HirPat::Rest)
                ) || matches!(
                    right_pat_data,
                    &Partial::Present(HirPat::WildCard) | &Partial::Present(HirPat::Rest)
                ) {
                    return false;
                }
            }
        }

        // Special check for imported vs qualified enum variants
        // This is critical for correctly handling imported variants like `A` vs qualified ones like `MyTag::A`
        if let HirPat::Path(pat_path_partial, _) = hir_pat {
            if let Partial::Present(pat_path) = pat_path_partial {
                let scope = body.scope();
                if let Ok(PathRes::EnumVariant(current_variant)) =
                    resolve_path(self.db, *pat_path, scope, true)
                {
                    // This is an enum variant - check if any previous variant is the same (by enum and index)
                    for prev_pat in previous_hir_pats {
                        if let HirPat::Path(prev_path_partial, _) = prev_pat {
                            if let Partial::Present(prev_path) = prev_path_partial {
                                if let Ok(PathRes::EnumVariant(prev_variant)) =
                                    resolve_path(self.db, *prev_path, scope, true)
                                {
                                    // ONLY compare enum definition and variant index, ignoring path differences
                                    if current_variant.variant.enum_ == prev_variant.variant.enum_
                                        && current_variant.variant.idx == prev_variant.variant.idx
                                    {
                                        // Same variant with different representation (imported vs qualified)
                                        // Current pattern is unreachable because it matches the same values as a previous pattern
                                        return false;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Convert HIR patterns to simplified patterns for analysis
        let simplified = SimplifiedPattern::from_hir_pat(hir_pat, self.db, body);
        let previous_simplified: Vec<_> = previous_hir_pats
            .iter()
            .map(|p| SimplifiedPattern::from_hir_pat(p, self.db, body))
            .collect();

        // Use is_useful_after to check if this pattern can match values not covered by previous patterns
        simplified.is_useful_after(&previous_simplified, self.db)
    }

    /// Convert a simplified pattern to a user-friendly pattern string for error messages
    fn simplified_to_user_pattern(
        &self,
        pattern: &SimplifiedPattern<'db>,
        _original_scrutinee_ty: TyId<'db>,
    ) -> String {
        // Convert simplified pattern back to user-friendly pattern for error messages
        // This is a placeholder implementation, needs to be more robust
        match pattern {
            SimplifiedPattern::Wildcard { binding } => {
                if let Some(name) = binding {
                    name.to_string()
                } else {
                    "_".to_string()
                }
            }
            SimplifiedPattern::Or(subpatterns) => {
                let parts: Vec<String> = subpatterns
                    .iter()
                    .map(|p| self.simplified_to_user_pattern(p, _original_scrutinee_ty))
                    .collect();
                parts.join(" | ")
            }
            SimplifiedPattern::Constructor {
                constructor,
                subpatterns,
                ty: _,
            } => {
                match constructor {
                    Constructor::Bool(val) => val.to_string(),
                    Constructor::Int(val) => val.to_string(),
                    Constructor::Tuple(arity) => {
                        let mut s = "(".to_string();
                        for i in 0..*arity {
                            if i < subpatterns.len() {
                                s.push_str(&self.simplified_to_user_pattern(
                                    &subpatterns[i],
                                    _original_scrutinee_ty,
                                ));
                            } else {
                                s.push('_');
                            }
                            if i < arity - 1 {
                                s.push_str(", ");
                            }
                        }
                        s.push(')');
                        s
                    }
                    Constructor::Record(RecordLike::Variant(variant)) => {
                        // Attempt to get the name of the variant for a more user-friendly display
                        // variant is ResolvedVariant. variant.variant is EnumVariant.
                        let variant_def_data = variant.variant.def(self.db); // Get &VariantDef
                        let variant_name = variant_def_data.name.to_opt().map_or_else(
                            || "_MISSING_VARIANT_NAME_".to_string(),
                            |id| id.data(self.db).to_string(),
                        );

                        // TODO: Reconstruct full path if necessary, and subpatterns for record/tuple variants
                        match variant_def_data.kind {
                            // .kind is a direct field on VariantDef
                            hir::hir_def::VariantKind::Unit => variant_name,
                            hir::hir_def::VariantKind::Tuple(_) => format!("{}(..)", variant_name), // Simplified
                            hir::hir_def::VariantKind::Record(_) => {
                                format!("{} {{ .. }}", variant_name)
                            } // Simplified
                        }
                    }
                    Constructor::Record(RecordLike::Type(_ty)) => {
                        // This would be for struct patterns.
                        "{ .. }".to_string() // Simplified
                    }
                    Constructor::Record(RecordLike::Dummy(ident)) => {
                        let name = ident.data(self.db);
                        let mut s = format!("{}(", name);
                        if !subpatterns.is_empty() {
                            for (i, sub_p) in subpatterns.iter().enumerate() {
                                s.push_str(
                                    &self.simplified_to_user_pattern(sub_p, _original_scrutinee_ty),
                                );
                                if i < subpatterns.len() - 1 {
                                    s.push_str(", ");
                                }
                            }
                        } else {
                            // It could be a unit-like dummy record from a path
                            // or a record with all fields as wildcards.
                            // If subpatterns are empty but original HIR had fields, "..." is more appropriate.
                            // For now, simple representation.
                        }
                        s.push(')');
                        s
                    }
                }
            }
        }
    }
}

impl<'db> SimplifiedPattern<'db> {
    pub fn from_hir_pat(
        pat_data: &HirPat<'db>,
        db: &'db dyn HirAnalysisDb,
        body: HirBody<'db>, // Needed to resolve PatIds for nested patterns
    ) -> Self {
        match pat_data {
            HirPat::WildCard => SimplifiedPattern::Wildcard { binding: None },
            HirPat::Rest => SimplifiedPattern::Wildcard { binding: None }, // Rest is often treated as wildcard in broad phase
            HirPat::Lit(lit_kind_partial) => {
                if let Partial::Present(lit_kind) = lit_kind_partial {
                    match lit_kind {
                        LitKind::Bool(value) => SimplifiedPattern::Constructor {
                            constructor: Constructor::Bool(*value),
                            subpatterns: Vec::new(),
                            ty: TyId::bool(db),
                        },
                        LitKind::Int(integer_id) => {
                            // Attempt to convert BigUint to i128.
                            // This is a lossy conversion if the BigUint is too large or negative (though BigUint is unsigned).
                            // For pattern matching, distinct large literals might not be distinguishable with this approach.
                            // A more robust solution might involve changing Constructor::Int or using a hash.
                            let value_biguint = integer_id.data(db);
                            let value_i128 = value_biguint.try_into().unwrap_or_else(|_| {
                                // Placeholder for values that don't fit in i128.
                                // This could be a specific large value, or we could introduce
                                // a distinct constructor variant for "large integer".
                                // For now, using i128::MAX as a stand-in.
                                // This means all integers too large for i128 will be treated as the same pattern.
                                i128::MAX
                            });
                            SimplifiedPattern::Constructor {
                                constructor: Constructor::Int(value_i128),
                                subpatterns: Vec::new(),
                                ty: TyId::never(db), // Placeholder - will be properly inferred later
                            }
                        }
                        _ => SimplifiedPattern::Wildcard { binding: None }, // Other literals, treat as wildcard for now
                    }
                } else {
                    SimplifiedPattern::Wildcard { binding: None }
                }
            }
            HirPat::Tuple(elements_pat_ids) => {
                let subpatterns: Vec<SimplifiedPattern> = elements_pat_ids
                    .iter()
                    .map(|pat_id| {
                        let pat_partial_data = pat_id.data(db, body);
                        match pat_partial_data {
                            Partial::Present(pat_actual_data) => {
                                SimplifiedPattern::from_hir_pat(pat_actual_data, db, body)
                            }
                            Partial::Absent => SimplifiedPattern::Wildcard { binding: None }, // Or handle error
                        }
                    })
                    .collect();
                SimplifiedPattern::Constructor {
                    constructor: Constructor::Tuple(subpatterns.len()),
                    subpatterns: subpatterns.clone(),
                    ty: TyId::tuple(db, subpatterns.len()), // Create a tuple type with correct arity
                }
            }
            HirPat::Path(path_partial, _is_mut_binding) => {
                if let Partial::Present(path_id) = path_partial {
                    let scope = body.scope();
                    match resolve_path(db, *path_id, scope, true /* resolve_tail_as_value */) {
                        Ok(PathRes::EnumVariant(resolved_variant)) => {
                            // This is definitively an enum variant constructor (e.g., MyEnum::VariantA)
                            let constructor =
                                Constructor::Record(RecordLike::Variant(resolved_variant.clone()));
                            let field_count = match resolved_variant.variant.kind(db) {
                                hir::hir_def::VariantKind::Unit => 0,
                                hir::hir_def::VariantKind::Tuple(fields) => fields.data(db).len(),
                                hir::hir_def::VariantKind::Record(fields) => fields.data(db).len(),
                            };
                            let subpatterns = if field_count == 0 {
                                Vec::new()
                            } else {
                                (0..field_count)
                                    .map(|_| SimplifiedPattern::Wildcard { binding: None })
                                    .collect()
                            };
                            SimplifiedPattern::Constructor {
                                constructor,
                                subpatterns,
                                ty: resolved_variant.ty,
                            }
                        }
                        Ok(PathRes::Ty(ty_id)) => {
                            // Path resolved to a Type.
                            // If it's a bare ident, it could be an imported enum variant (e.g. `VariantA` after `use MyEnum::*`)
                            // or a binding that shadows a type name.
                            if path_id.is_bare_ident(db) {
                                if let Some(adt_ref) = ty_id.adt_ref(db) {
                                    if let HirAdtRef::Enum(enum_def) = adt_ref {
                                        // The bare ident resolved to an Enum type. Now check if the ident name is one of its variants.
                                        if let Some(ident_value) = path_id.ident(db).to_opt() {
                                            let mut found_variant_idx = None;
                                            for (idx, variant_def) in
                                                enum_def.variants(db).data(db).iter().enumerate()
                                            {
                                                if variant_def.name.to_opt() == Some(ident_value) {
                                                    found_variant_idx = Some(idx as u16);
                                                    break;
                                                }
                                            }

                                            if let Some(variant_idx) = found_variant_idx {
                                                // It's a variant of this enum.
                                                // This handles imported variants (from use MyTag::*)\
                                                let resolved_variant = ResolvedVariant {
                                                    ty: ty_id,
                                                    variant: hir::hir_def::EnumVariant::new(
                                                        enum_def,
                                                        variant_idx.into(),
                                                    ),
                                                    path: *path_id,
                                                };
                                                let constructor = Constructor::Record(
                                                    RecordLike::Variant(resolved_variant.clone()),
                                                );
                                                let field_count =
                                                    match resolved_variant.variant.kind(db) {
                                                        hir::hir_def::VariantKind::Unit => 0,
                                                        hir::hir_def::VariantKind::Tuple(
                                                            fields,
                                                        ) => fields.data(db).len(),
                                                        hir::hir_def::VariantKind::Record(
                                                            fields,
                                                        ) => fields.data(db).len(),
                                                    };
                                                let subpatterns = if field_count == 0 {
                                                    Vec::new()
                                                } else {
                                                    (0..field_count)
                                                        .map(|_| SimplifiedPattern::Wildcard {
                                                            binding: None,
                                                        })
                                                        .collect()
                                                };
                                                SimplifiedPattern::Constructor {
                                                    constructor,
                                                    subpatterns,
                                                    ty: ty_id,
                                                }
                                            } else {
                                                // Bare ident, resolved to Enum type, but ident name is not a variant. Treat as binding.
                                                let binding_name = path_id
                                                    .ident(db)
                                                    .to_opt()
                                                    .map(|id| id.data(db).as_str());
                                                SimplifiedPattern::Wildcard {
                                                    binding: binding_name,
                                                }
                                            }
                                        } else {
                                            // Bare ident but PathId has no ident name (should not happen for valid code). Treat as binding.
                                            let binding_name = path_id
                                                .ident(db)
                                                .to_opt()
                                                .map(|id| id.data(db).as_str());
                                            SimplifiedPattern::Wildcard {
                                                binding: binding_name,
                                            }
                                        }
                                    } else {
                                        // Bare ident resolved to a non-Enum ADT Type (Struct/Contract). Treat as binding.
                                        let binding_name = path_id
                                            .ident(db)
                                            .to_opt()
                                            .map(|id| id.data(db).as_str());
                                        SimplifiedPattern::Wildcard {
                                            binding: binding_name,
                                        }
                                    }
                                } else {
                                    // Bare ident resolved to a non-ADT Type. Treat as binding.
                                    let binding_name =
                                        path_id.ident(db).to_opt().map(|id| id.data(db).as_str());
                                    SimplifiedPattern::Wildcard {
                                        binding: binding_name,
                                    }
                                }
                            } else {
                                // Complex path (not a bare ident) resolved to a Type. This isn't a constructor.
                                SimplifiedPattern::Wildcard { binding: None }
                            }
                        }
                        Ok(PathRes::TyAlias(_, _))
                        | Ok(PathRes::Trait(_))
                        | Ok(PathRes::Func(_))
                        | Ok(PathRes::Const(_))
                        | Ok(PathRes::Mod(_))
                        | Ok(PathRes::TypeMemberTbd(_))
                        | Ok(PathRes::FuncParam(..))
                        | Err(_) => {
                            // Path resolved to something that isn't an EnumVariant or a generic Type, or resolution failed.
                            if path_id.is_bare_ident(db) {
                                // If it's a bare ident, it's likely a variable binding.
                                let binding_name =
                                    path_id.ident(db).to_opt().map(|id| id.data(db).as_str());
                                SimplifiedPattern::Wildcard {
                                    binding: binding_name,
                                }
                            } else {
                                // If it's a complex path, it's not a valid constructor in this context.
                                SimplifiedPattern::Wildcard { binding: None }
                            }
                        }
                    }
                } else {
                    // path_partial is ::Absent (syntactically invalid path in HIR Pat::Path)
                    SimplifiedPattern::Wildcard { binding: None }
                }
            }
            HirPat::PathTuple(path_partial, elements_pat_ids) => {
                if let Partial::Present(path_id) = path_partial {
                    let scope = body.scope();
                    match resolve_path(db, *path_id, scope, true /* resolve_tail_as_value */) {
                        Ok(PathRes::EnumVariant(resolved_variant)) => {
                            match resolved_variant.variant.kind(db) {
                                hir::hir_def::VariantKind::Tuple(_expected_fields_list_id) => {
                                    let subpatterns: Vec<SimplifiedPattern> = elements_pat_ids
                                        .iter()
                                        .map(|pat_id| match pat_id.data(db, body) {
                                            Partial::Present(pat_actual_data) => {
                                                SimplifiedPattern::from_hir_pat(
                                                    pat_actual_data,
                                                    db,
                                                    body,
                                                )
                                            }
                                            Partial::Absent => {
                                                SimplifiedPattern::Wildcard { binding: None }
                                            }
                                        })
                                        .collect();

                                    SimplifiedPattern::Constructor {
                                        constructor: Constructor::Record(RecordLike::Variant(
                                            resolved_variant.clone(),
                                        )),
                                        subpatterns,
                                        ty: resolved_variant.ty,
                                    }
                                }
                                _ => {
                                    // Path resolved to an EnumVariant, but it's not a Tuple kind (e.g., Unit or Record).
                                    // This pattern form `Variant(a,b)` expects a tuple variant.
                                    SimplifiedPattern::Wildcard { binding: None }
                                    // Or an error marker
                                }
                            }
                        }
                        _ => {
                            // Path did not resolve to an EnumVariant or resolution failed in an unexpected way.
                            SimplifiedPattern::Wildcard { binding: None }
                        }
                    }
                } else {
                    // path_partial is ::Absent, syntactically invalid path.
                    SimplifiedPattern::Wildcard { binding: None }
                }
            }
            HirPat::Record(path_id_partial, fields_vec) => {
                if let Partial::Present(path_id) = path_id_partial {
                    let scope = body.scope();
                    match resolve_path(db, *path_id, scope, true /* resolve_tail_as_value */) {
                        Ok(PathRes::EnumVariant(resolved_variant)) => {
                            match resolved_variant.variant.kind(db) {
                                hir::hir_def::VariantKind::Record(_) => {
                                    let record_like_constructor =
                                        RecordLike::Variant(resolved_variant.clone());
                                    let resolved_ty = resolved_variant.ty;

                                    let canonical_field_names =
                                        record_like_constructor.record_labels(db);
                                    let mut subpatterns =
                                        vec![
                                            SimplifiedPattern::Wildcard { binding: None };
                                            canonical_field_names.len()
                                        ];

                                    let mut has_rest_pattern = false;
                                    let mut provided_field_pats = FxHashMap::default();
                                    for hir_field_pat_entry in fields_vec {
                                        if hir_field_pat_entry.pat.is_rest(db, body) {
                                            has_rest_pattern = true;
                                            // Note: Fe syntax might only allow one '..'
                                        } else if let Partial::Present(label) =
                                            hir_field_pat_entry.label
                                        {
                                            provided_field_pats
                                                .insert(label, hir_field_pat_entry.pat);
                                        } else {
                                            // unlabeled field in record pattern - this should ideally be a parse error or handled by type checker.
                                            // For robustness in pattern analysis, we might ignore or treat as error.
                                        }
                                    }

                                    for (idx, canonical_name) in
                                        canonical_field_names.iter().enumerate()
                                    {
                                        if let Some(pat_id) =
                                            provided_field_pats.get(canonical_name)
                                        {
                                            if let Partial::Present(pat_actual_data) =
                                                pat_id.data(db, body)
                                            {
                                                subpatterns[idx] = SimplifiedPattern::from_hir_pat(
                                                    pat_actual_data,
                                                    db,
                                                    body,
                                                );
                                            }
                                        } else if !has_rest_pattern {
                                            // Missing field, and no '..' found. It's already a wildcard.
                                            // The type checker should ideally report an error for this.
                                        }
                                    }
                                    // TODO: Add check for fields in provided_field_pats that are not in canonical_field_names (extra fields error).

                                    SimplifiedPattern::Constructor {
                                        constructor: Constructor::Record(record_like_constructor),
                                        subpatterns,
                                        ty: resolved_ty,
                                    }
                                }
                                _ => SimplifiedPattern::Wildcard { binding: None }, // Path is enum variant, but not record kind. Error.
                            }
                        }
                        Ok(PathRes::Ty(struct_ty_id)) => {
                            if let Some(adt_ref) = struct_ty_id.adt_ref(db) {
                                match adt_ref {
                                    HirAdtRef::Struct(_) | HirAdtRef::Contract(_) => {
                                        let record_like_constructor =
                                            RecordLike::Type(struct_ty_id);
                                        let resolved_ty = struct_ty_id;

                                        let canonical_field_names =
                                            record_like_constructor.record_labels(db);
                                        let mut subpatterns =
                                            vec![
                                                SimplifiedPattern::Wildcard { binding: None };
                                                canonical_field_names.len()
                                            ];

                                        let mut has_rest_pattern = false;
                                        let mut provided_field_pats = FxHashMap::default();
                                        for hir_field_pat_entry in fields_vec {
                                            if hir_field_pat_entry.pat.is_rest(db, body) {
                                                has_rest_pattern = true;
                                            } else if let Partial::Present(label) =
                                                hir_field_pat_entry.label
                                            {
                                                provided_field_pats
                                                    .insert(label, hir_field_pat_entry.pat);
                                            }
                                        }

                                        for (idx, canonical_name) in
                                            canonical_field_names.iter().enumerate()
                                        {
                                            if let Some(pat_id) =
                                                provided_field_pats.get(canonical_name)
                                            {
                                                if let Partial::Present(pat_actual_data) =
                                                    pat_id.data(db, body)
                                                {
                                                    subpatterns[idx] =
                                                        SimplifiedPattern::from_hir_pat(
                                                            pat_actual_data,
                                                            db,
                                                            body,
                                                        );
                                                }
                                            } else if !has_rest_pattern {
                                                // Missing field
                                            }
                                        }
                                        // TODO: Add check for extra fields.

                                        SimplifiedPattern::Constructor {
                                            constructor: Constructor::Record(
                                                record_like_constructor,
                                            ),
                                            subpatterns,
                                            ty: resolved_ty,
                                        }
                                    }
                                    _ => SimplifiedPattern::Wildcard { binding: None }, // Path is a type, but not struct/contract. Error.
                                }
                            } else {
                                SimplifiedPattern::Wildcard { binding: None } // Path is a type, but not an ADT we can match fields on. Error.
                            }
                        }
                        _ => SimplifiedPattern::Wildcard { binding: None }, // Path didn't resolve to a suitable record constructor. Error.
                    }
                } else {
                    SimplifiedPattern::Wildcard { binding: None } // Path is syntactically absent
                }
            }
            HirPat::Or(lhs_pat_id, rhs_pat_id) => {
                let lhs_simplified_pat = match lhs_pat_id.data(db, body) {
                    Partial::Present(lhs_pat_actual_data) => {
                        SimplifiedPattern::from_hir_pat(lhs_pat_actual_data, db, body)
                    }
                    Partial::Absent => SimplifiedPattern::Wildcard { binding: None }, // Or handle error
                };
                let rhs_simplified_pat = match rhs_pat_id.data(db, body) {
                    Partial::Present(rhs_pat_actual_data) => {
                        SimplifiedPattern::from_hir_pat(rhs_pat_actual_data, db, body)
                    }
                    Partial::Absent => SimplifiedPattern::Wildcard { binding: None }, // Or handle error
                };
                SimplifiedPattern::Or(vec![lhs_simplified_pat, rhs_simplified_pat])
            }
        }
    }
}