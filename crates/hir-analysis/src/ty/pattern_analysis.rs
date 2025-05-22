use crate::name_resolution::{resolve_path, PathRes, ResolvedVariant};
use crate::ty::adt_def::{lower_adt, AdtRef};
// use crate::ty::pattern::Pattern; // TODO: Remove this import once Pattern type is fully deprecated
use crate::ty::ty_check::RecordLike;
use crate::ty::ty_def::TyId;
use crate::HirAnalysisDb;
use hir::hir_def::item::EnumVariant;
use hir::hir_def::{
    Body as HirBody, GenericArgListId, IdentId, LitKind, Partial, Pat as HirPat, PathId,
    VariantKind,
};
use crate::ty::AdtRef as HirAdtRef;
use rustc_hash::FxHashMap;
// EnumVariant itself serves as a key for covered_variants

/// Simplified pattern representation for analysis
/// Based on "Warnings for pattern matching" paper
#[derive(Clone, Debug)]
pub enum SimplifiedPattern<'db> {
    /// Wildcard pattern that matches anything
    Wildcard {
        /// Optional binding name
        binding: Option<&'db str>,
    },

    /// Or pattern for alternatives
    Or(Vec<SimplifiedPattern<'db>>),

    /// Constructor pattern with subpatterns
    Constructor {
        /// The constructor (record, tuple, literal, etc.)
        constructor: Constructor<'db>,
        /// Subpatterns for the constructor's fields
        subpatterns: Vec<SimplifiedPattern<'db>>,
        /// The type of this pattern
        ty: TyId<'db>,
    },
}

/// Represents a constructor in the simplified pattern
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Constructor<'db> {
    /// Record constructor (struct or enum variant)
    Record(RecordLike<'db>),

    /// Tuple constructor with arity
    Tuple(usize),

    /// Boolean literal
    Bool(bool),

    /// Integer literal
    Int(i128),
}

impl<'db> Constructor<'db> {
    /// Returns the number of fields for this constructor
    fn field_count(&self, db: &'db dyn HirAnalysisDb) -> usize {
        match self {
            Constructor::Record(record) => {
                // We don't have direct access to fields via RecordLike
                match record {
                    RecordLike::Variant(variant) => {
                        // Get field count from variant kind
                        match variant.kind(db) {
                            VariantKind::Tuple(fields) => fields.data(db).len(),
                            VariantKind::Record(fields) => fields.data(db).len(),
                            VariantKind::Unit => 0,
                        }
                    }
                    RecordLike::Type(_) => 0, // TODO: Handle struct fields
                    RecordLike::Dummy(_) => 0,
                }
            }
            Constructor::Tuple(arity) => *arity,
            Constructor::Bool(_) | Constructor::Int(_) => 0,
        }
    }
}

impl<'db> SimplifiedPattern<'db> {
    /// Convert an HIR pattern (Pat) to a simplified pattern for analysis.
    pub fn from_hir_pat(
        pat_data: &HirPat<'db>,
        db: &'db dyn HirAnalysisDb,
        body: HirBody<'db>, // Needed to resolve PatIds for nested patterns
    ) -> Self {
        match pat_data {
            HirPat::WildCard => {
                SimplifiedPattern::Wildcard { binding: None }
            }
            HirPat::Rest => SimplifiedPattern::Wildcard { binding: None }, // Rest is often treated as wildcard in broad phase
            HirPat::Lit(lit_kind_partial) => {
                if let Partial::Present(lit_kind) = lit_kind_partial {
                    match lit_kind {
                        LitKind::Bool(value) => {
                            SimplifiedPattern::Constructor {
                                constructor: Constructor::Bool(*value),
                                subpatterns: Vec::new(),
                                ty: TyId::bool(db),
                            }
                        }
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
                                ty: TyId::never(db) // Placeholder - will be properly inferred later
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
                            let constructor = Constructor::Record(RecordLike::Variant(resolved_variant.clone()));
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
                                            for (idx, variant_def) in enum_def.variants(db).data(db).iter().enumerate() {
                                                if variant_def.name.to_opt() == Some(ident_value) {
                                                    found_variant_idx = Some(idx as u16);
                                                    break;
                                                }
                                            }

                                            if let Some(variant_idx) = found_variant_idx {
                                                // It's a variant of this enum.
                                                let resolved_variant = ResolvedVariant {
                                                    ty: ty_id,
                                                    variant: hir::hir_def::EnumVariant::new(enum_def, variant_idx.into()),
                                                    path: *path_id,
                                                };
                                                let constructor = Constructor::Record(RecordLike::Variant(resolved_variant.clone()));
                                                let field_count = match resolved_variant.variant.kind(db) {
                                                    hir::hir_def::VariantKind::Unit => 0,
                                                    hir::hir_def::VariantKind::Tuple(fields) => fields.data(db).len(),
                                                    hir::hir_def::VariantKind::Record(fields) => fields.data(db).len(),
                                                };
                                                let subpatterns = if field_count == 0 { Vec::new() } else { (0..field_count).map(|_| SimplifiedPattern::Wildcard { binding: None }).collect() };
                                                SimplifiedPattern::Constructor { constructor, subpatterns, ty: resolved_variant.ty }
                                            } else {
                                                // Bare ident, resolved to Enum type, but ident name is not a variant. Treat as binding.
                                                let binding_name = path_id.ident(db).to_opt().map(|id| id.data(db).as_str());
                                                SimplifiedPattern::Wildcard { binding: binding_name }
                                            }
                                        } else {
                                            // Bare ident but PathId has no ident name (should not happen for valid code). Treat as binding.
                                            let binding_name = path_id.ident(db).to_opt().map(|id| id.data(db).as_str());
                                            SimplifiedPattern::Wildcard { binding: binding_name }
                                        }
                                    } else {
                                        // Bare ident resolved to a non-Enum ADT Type (Struct/Contract). Treat as binding.
                                        let binding_name = path_id.ident(db).to_opt().map(|id| id.data(db).as_str());
                                        SimplifiedPattern::Wildcard { binding: binding_name }
                                    }
                                } else {
                                    // Bare ident resolved to a non-ADT Type. Treat as binding.
                                    let binding_name = path_id.ident(db).to_opt().map(|id| id.data(db).as_str());
                                    SimplifiedPattern::Wildcard { binding: binding_name }
                                }
                            } else {
                                // Complex path (not a bare ident) resolved to a Type. This isn't a constructor.
                                SimplifiedPattern::Wildcard { binding: None }
                            }
                        }
                        Ok(PathRes::TyAlias(_, _)) | Ok(PathRes::Trait(_)) | Ok(PathRes::Func(_)) | Ok(PathRes::Const(_)) | Ok(PathRes::Mod(_)) | Ok(PathRes::TypeMemberTbd(_)) | Ok(PathRes::FuncParam(..)) | Err(_) => {
                            // Path resolved to something that isn't an EnumVariant or a generic Type, or resolution failed.
                            if path_id.is_bare_ident(db) {
                                // If it's a bare ident, it's likely a variable binding.
                                let binding_name = path_id.ident(db).to_opt().map(|id| id.data(db).as_str());
                                SimplifiedPattern::Wildcard { binding: binding_name }
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
                                        .map(|pat_id| {
                                            match pat_id.data(db, body) {
                                                Partial::Present(pat_actual_data) => {
                                                    SimplifiedPattern::from_hir_pat(pat_actual_data, db, body)
                                                }
                                                Partial::Absent => SimplifiedPattern::Wildcard { binding: None },
                                            }
                                        })
                                        .collect();

                                    SimplifiedPattern::Constructor {
                                        constructor: Constructor::Record(RecordLike::Variant(resolved_variant.clone())),
                                        subpatterns,
                                        ty: resolved_variant.ty,
                                    }
                                }
                                _ => { // Path resolved to an EnumVariant, but it's not a Tuple kind (e.g., Unit or Record).
                                      // This pattern form `Variant(a,b)` expects a tuple variant.
                                    SimplifiedPattern::Wildcard { binding: None } // Or an error marker
                                }
                            }
                        }
                        _ => { // Path did not resolve to an EnumVariant or resolution failed in an unexpected way.
                            SimplifiedPattern::Wildcard { binding: None }
                        }
                    }
                } else { // path_partial is ::Absent, syntactically invalid path.
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
                                    let record_like_constructor = RecordLike::Variant(resolved_variant.clone());
                                    let resolved_ty = resolved_variant.ty;
                                    
                                    let canonical_field_names = record_like_constructor.record_labels(db);
                                    let mut subpatterns = vec![SimplifiedPattern::Wildcard { binding: None }; canonical_field_names.len()];
                                    
                                    let mut has_rest_pattern = false;
                                    let mut provided_field_pats = FxHashMap::default();
                                    for hir_field_pat_entry in fields_vec {
                                        if hir_field_pat_entry.pat.is_rest(db, body) {
                                            has_rest_pattern = true;
                                            // Note: Fe syntax might only allow one '..'
                                        } else if let Partial::Present(label) = hir_field_pat_entry.label {
                                            provided_field_pats.insert(label, hir_field_pat_entry.pat);
                                        } else {
                                            // unlabeled field in record pattern - this should ideally be a parse error or handled by type checker.
                                            // For robustness in pattern analysis, we might ignore or treat as error.
                                        }
                                    }

                                    for (idx, canonical_name) in canonical_field_names.iter().enumerate() {
                                        if let Some(pat_id) = provided_field_pats.get(canonical_name) {
                                            if let Partial::Present(pat_actual_data) = pat_id.data(db, body) {
                                                subpatterns[idx] = SimplifiedPattern::from_hir_pat(pat_actual_data, db, body);
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
                                        let record_like_constructor = RecordLike::Type(struct_ty_id);
                                        let resolved_ty = struct_ty_id;

                                        let canonical_field_names = record_like_constructor.record_labels(db);
                                        let mut subpatterns = vec![SimplifiedPattern::Wildcard { binding: None }; canonical_field_names.len()];

                                        let mut has_rest_pattern = false;
                                        let mut provided_field_pats = FxHashMap::default();
                                         for hir_field_pat_entry in fields_vec {
                                            if hir_field_pat_entry.pat.is_rest(db, body) {
                                                has_rest_pattern = true;
                                            } else if let Partial::Present(label) = hir_field_pat_entry.label {
                                                provided_field_pats.insert(label, hir_field_pat_entry.pat);
                                            }
                                        }

                                        for (idx, canonical_name) in canonical_field_names.iter().enumerate() {
                                            if let Some(pat_id) = provided_field_pats.get(canonical_name) {
                                               if let Partial::Present(pat_actual_data) = pat_id.data(db, body) {
                                                    subpatterns[idx] = SimplifiedPattern::from_hir_pat(pat_actual_data, db, body);
                                                }
                                            } else if !has_rest_pattern {
                                                // Missing field
                                            }
                                        }
                                        // TODO: Add check for extra fields.

                                        SimplifiedPattern::Constructor {
                                            constructor: Constructor::Record(record_like_constructor),
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

    /*
    /// Convert a regular pattern to a simplified pattern
    pub fn from_pattern(pattern: &Pattern<'db>, db: &'db dyn HirAnalysisDb) -> Self {
        // Temporarily commented out as Pattern type is being deprecated
        // TODO: Remove this function entirely once PatternAnalyzer uses from_hir_pat
        panic!("SimplifiedPattern::from_pattern should not be called directly anymore. Use from_hir_pat.");
        /*
        match pattern {
            Pattern::Wildcard => SimplifiedPattern::Wildcard,

            Pattern::Binding(_) => {
                // Bindings are treated as wildcards for analysis
                SimplifiedPattern::Wildcard
            }

            Pattern::BoolLiteral(value) => SimplifiedPattern::Constructor {
                constructor: Constructor::Bool(*value),
                subpatterns: Vec::new(),
            },

            Pattern::IntLiteral(value) => SimplifiedPattern::Constructor {
                constructor: Constructor::Int(*value),
                subpatterns: Vec::new(),
            },

            Pattern::Record {
                record,
                fields,
                rest,
            } => {
                // Get all field names for the record
                let all_fields = record.record_labels(db);
                let mut subpatterns = Vec::new();

                // For each field in the record type
                for field_name in &all_fields {
                    // Find if this field is matched in the pattern
                    let matching_field = fields.iter().find(|(name, _)| *name == *field_name);

                    let subpattern = if let Some((_, pattern)) = matching_field {
                        // This field has an explicit pattern
                        SimplifiedPattern::from_pattern(pattern, db)
                    } else if *rest {
                        // Field not mentioned but we have ".."
                        SimplifiedPattern::Wildcard
                    } else {
                        // Field not present and no ".." - this should be caught by the parser
                        // but we handle it safely here
                        SimplifiedPattern::Wildcard
                    };

                    subpatterns.push(subpattern);
                }

                SimplifiedPattern::Constructor {
                    constructor: Constructor::Record(record_like),
                    subpatterns,
                    ty: body.ty(pat.id).expect("Expected type for record pattern")
                }
            }

            Pattern::Tuple { tuple, elements } => {
                // Determine arity. If `tuple` (TupleLike) is present, it's the source of truth.
                // Otherwise, for simple tuples like `(a, b)`, arity is elements.len().
                // Fe syntax usually requires `..` only when destructuring a known type (e.g., enum variant).
                let arity = tuple.as_ref().map_or_else(
                    || {
                        // If no TupleLike info, and Pattern::Rest is present, it implies a simple tuple
                        // pattern like `(a, .., b)`. Fe's parser might not allow `..` in simple tuples
                        // that aren't destructuring a named type/variant.
                        // If `Pattern::Rest` is present, `elements.len()` is not the true arity.
                        // This situation suggests Pattern::Tuple might be malformed if `tuple` is None
                        // and `Pattern::Rest` is present.
                        // For now, if `tuple` is None, we assume no `Pattern::Rest` is in `elements`
                        // and `arity` is just `elements.len()`. A more robust solution might
                        // involve erroring or better upstream construction of `Pattern::Tuple`.
                        if elements.iter().any(|el| matches!(el, Pattern::Rest)) {
                            // This case is problematic: `..` in a tuple pattern without TupleLike info.
                            // Defaulting to elements.len(), but this might hide issues.
                            // Ideally, `Pattern::Tuple` with `Pattern::Rest` always has `Some(TupleLike)`.
                            elements.len()
                        } else {
                            elements.len() // No `..`, simple tuple, arity is element count.
                        }
                    },
                    |t| t.arity(db),
                );

                let mut subpatterns = Vec::new();
                let mut rest_already_expanded = false;

                // Convert elements to simplified subpatterns, handling `..`
                for (idx, element_pattern) in elements.iter().enumerate() {
                    match element_pattern {
                        Pattern::Rest => {
                            if rest_already_expanded {
                                // Multiple \'..\' in a tuple pattern. This should ideally be a syntax error
                                // caught much earlier. Defensively add one wildcard if space allows.
                                if subpatterns.len() < arity {
                                    subpatterns.push(SimplifiedPattern::Wildcard);
                                }
                                continue;
                            }
                            rest_already_expanded = true;
                            // Calculate how many wildcards `..` should expand to.
                            let num_explicit_before_rest = idx;
                            let num_explicit_after_rest = elements
                                .iter()
                                .skip(idx + 1)
                                .filter(|p| !matches!(p, Pattern::Rest))
                                .count();
                            let num_wildcards = if arity >= num_explicit_before_rest + num_explicit_after_rest {
                                arity - (num_explicit_before_rest + num_explicit_after_rest)
                            } else {
                                0 // Not enough space, implies arity mismatch or malformed pattern.
                            };
                            for _ in 0..num_wildcards {
                                if subpatterns.len() < arity {
                                    subpatterns.push(SimplifiedPattern::Wildcard);
                                }
                            }
                        }
                        _ => {
                            // Regular pattern element
                            if subpatterns.len() < arity {
                                subpatterns
                                    .push(SimplifiedPattern::from_pattern(element_pattern, db));
                            }
                        }
                    }
                }

                // Final pass to ensure subpatterns list matches arity.
                // This handles cases where `elements` was shorter than `arity` and no `Pattern::Rest`
                // was present, or if the `Pattern::Rest` logic didn't perfectly fill due to arity constraints.
                while subpatterns.len() < arity {
                    subpatterns.push(SimplifiedPattern::Wildcard);
                }
                // If too many subpatterns were generated (e.g., malformed `..` logic or arity mismatch), truncate.
                // This is defensive; ideally, arity calculation and `..` expansion are perfect.
                subpatterns.truncate(arity);

                SimplifiedPattern::Constructor {
                    constructor: Constructor::Tuple(arity),
                    subpatterns,
                    ty: body.ty(pat.id).expect("Expected type for tuple pattern")
                }
            }
            Pattern::Or(patterns) => {
                let subpatterns = patterns
                    .iter()
                    .map(|p| SimplifiedPattern::from_pattern(p, db))
                    .collect();
                SimplifiedPattern::Or(subpatterns)
            }

            Pattern::Invalid => SimplifiedPattern::Wildcard, // Treat invalid patterns as wildcards
            Pattern::Rest => {
                // `Pattern::Rest` should ideally only appear within `Pattern::Tuple` or `Pattern::Record` elements,
                // and be handled by their `from_pattern` logic (e.g., expanding to wildcards).
                // If `Pattern::Rest` is encountered at the top level, it's like a wildcard.
                SimplifiedPattern::Wildcard
            }
        }
        */
    }
    */

    /// Check if this pattern is useful when following the given patterns
    /// A pattern is useful if it can match values that none of the previous patterns match.
    /// This is a basic implementation.
    pub fn is_useful_after(
        &self,
        previous: &[SimplifiedPattern<'db>],
        db: &'db dyn HirAnalysisDb,
    ) -> bool {
        // Empty pattern vector means all previous patterns didn't match
        if previous.is_empty() {
            return true;
        }

        // Create a matrix from the previous patterns
        let matrix = PatternMatrix::new(previous.to_vec());

        match self {
            SimplifiedPattern::Wildcard { .. } => {
                // For wildcard patterns in match expressions like "_",
                // we need to check if there are any values not covered by previous patterns.
                // A wildcard is useful if there are still uncovered patterns.
        
                // For a wildcard pattern, we need to check if there are still values not covered
                // by the previous patterns. If all values are covered, the wildcard is not useful.
                // 
                // We need to check if there are any patterns that are not covered by
                // any of the previous patterns. If all possible patterns are covered,
                // then our wildcard is unreachable.
                //
                // A wildcard pattern is useful if not all possible values are covered by previous
                // patterns. The check should be based on the matrix as a whole, not just the 
                // specialization for the default case.
                
                // Check if the matrix contains a row with a wildcard in first position
                let has_wildcard = matrix.rows.iter().any(|row| {
                    if let Some(first) = row.patterns.first() {
                        matches!(first, SimplifiedPattern::Wildcard { .. }) ||
                        if let SimplifiedPattern::Or(pats) = first {
                            pats.iter().any(|p| matches!(p, SimplifiedPattern::Wildcard { .. }))
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                });

                // If there's already a wildcard pattern, our wildcard isn't useful
                !has_wildcard
            }

            SimplifiedPattern::Constructor {
                constructor,
                subpatterns,
                ty,
            } => {
                // Specialize by the constructor
                let specialized = matrix.specialize(constructor, db);

                if specialized.is_empty() {
                    // No row specializes to this constructor, so it's useful
                    return true;
                }

                // Check if subpatterns are useful in the specialized matrix
                if subpatterns.is_empty() {
                    return false; // No subpatterns, so covered by previous patterns
                }

                // For tuples and records with subpatterns, we need to check each subpattern
                // Check recursively for subpatterns
                let mut all_subpatterns_useful = true;
                
                for (i, subpattern) in subpatterns.iter().enumerate() {
                    let mut matrix_patterns = Vec::new();
                    for row in &specialized.rows {
                        if let Some(pattern) = row.patterns.first() {
                            if let SimplifiedPattern::Constructor {
                                subpatterns: sub, ..
                            } = pattern
                            {
                                if i < sub.len() {
                                    matrix_patterns.push(sub[i].clone());
                                }
                            }
                        }
                    }
                    
                    if !subpattern.is_useful_after(&matrix_patterns, db) {
                        all_subpatterns_useful = false;
                        break;
                    }
                }
                
                all_subpatterns_useful
            }

            SimplifiedPattern::Or(subpatterns) => {
                // Check if any subpattern is useful
                subpatterns.iter().any(|p| p.is_useful_after(previous, db))
            }
        }
    }

    /// Check if this pattern is irrefutable (always matches)
    pub fn is_irrefutable(&self) -> bool {
        match self {
            SimplifiedPattern::Wildcard { .. } => true,
            SimplifiedPattern::Or(patterns) => patterns.iter().any(|p| p.is_irrefutable()),
            SimplifiedPattern::Constructor { .. } => false,
        }
    }
}

/// A matrix of patterns for analysis
pub struct PatternMatrix<'db> {
    /// Rows of patterns
    rows: Vec<PatternRow<'db>>,
}

/// A row in the pattern matrix
pub struct PatternRow<'db> {
    /// The patterns in this row
    patterns: Vec<SimplifiedPattern<'db>>,
    /// The index of the match arm this row corresponds to
    arm_index: usize,
}

impl<'db> PatternMatrix<'db> {
    /// Creates a new pattern matrix from a list of patterns
    pub fn new(patterns: Vec<SimplifiedPattern<'db>>) -> Self {
        let rows = patterns
            .into_iter()
            .enumerate()
            .map(|(idx, pattern)| PatternRow {
                patterns: vec![pattern],
                arm_index: idx,
            })
            .collect();

        PatternMatrix { rows }
    }

    /// Checks if this matrix is empty
    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    /// Check if a specific row is useful (not shadowed by previous rows)
    pub fn is_row_useful(&self, row_index: usize, db: &'db dyn HirAnalysisDb) -> bool {
        if row_index == 0 {
            return true; // First row is always useful
        }

        let current_pattern = &self.rows[row_index].patterns[0];
        let previous_patterns: Vec<_> = self.rows[0..row_index]
            .iter()
            .map(|row| row.patterns[0].clone())
            .collect();

        current_pattern.is_useful_after(&previous_patterns, db)
    }

    /// Specializes the matrix for a constructor
    pub fn specialize(&self, ctor: &Constructor<'db>, db: &'db dyn HirAnalysisDb) -> Self {
        let mut specialized_rows = Vec::new();

        for row in &self.rows {
            if let Some(first_pat) = row.patterns.first() {
                let new_patterns = match first_pat {
                    SimplifiedPattern::Wildcard { binding: _ } => {
                        // Wildcard specializes to wildcard patterns for each field
                        let field_count = ctor.field_count(db);

                        let mut wildcard_patterns = Vec::with_capacity(field_count);
                        for _ in 0..field_count {
                            wildcard_patterns.push(SimplifiedPattern::Wildcard { binding: None });
                        }
                        Some(wildcard_patterns)
                    }
                    SimplifiedPattern::Constructor {
                        constructor,
                        subpatterns,
                        ..
                    } if constructor == ctor => {
                        // Constructor specializes to its subpatterns if it matches
                        Some(subpatterns.clone())
                    }
                    SimplifiedPattern::Or(patterns) => {
                        // Try to specialize each pattern and collect results
                        let mut result = Vec::new();
                        for pat in patterns {
                            if let SimplifiedPattern::Constructor {
                                constructor,
                                subpatterns,
                                ..
                            } = pat
                            {
                                if constructor == ctor {
                                    result.extend(subpatterns.clone());
                                }
                            } else if let SimplifiedPattern::Wildcard { .. } = pat {
                                // Wildcard specializes to wildcards for each field
                                let field_count = ctor.field_count(db);

                                for _ in 0..field_count {
                                    result.push(SimplifiedPattern::Wildcard { binding: None });
                                }
                            }
                        }
                        if result.is_empty() {
                            None
                        } else {
                            Some(result)
                        }
                    }
                    _ => None, // Constructor doesn't match, doesn't specialize
                };

                if let Some(mut patterns) = new_patterns {
                    // Add remaining patterns from the row
                    patterns.extend_from_slice(&row.patterns[1..]);

                    specialized_rows.push(PatternRow {
                        patterns,
                        arm_index: row.arm_index,
                    });
                }
            }
        }

        PatternMatrix {
            rows: specialized_rows,
        }
    }

    /// Specializes the matrix for the default case
    pub fn specialize_default(&self) -> Self {
        let mut specialized_rows = Vec::new();

        for row in &self.rows {
            if let Some(first_pat) = row.patterns.first() {
                match first_pat {
                    SimplifiedPattern::Wildcard { .. } => {
                        // Wildcard specializes for default case
                        let new_patterns = row.patterns[1..].to_vec();

                        specialized_rows.push(PatternRow {
                            patterns: new_patterns,
                            arm_index: row.arm_index,
                        });
                    }
                    SimplifiedPattern::Or(patterns) => {
                        // Check if any pattern in Or is a wildcard
                        if patterns
                            .iter()
                            .any(|p| matches!(p, SimplifiedPattern::Wildcard { .. }))
                        {
                            let new_patterns = row.patterns[1..].to_vec();

                            specialized_rows.push(PatternRow {
                                patterns: new_patterns,
                                arm_index: row.arm_index,
                            });
                        }
                    }
                    _ => {} // Constructors don't specialize for default case
                }
            }
        }

        PatternMatrix {
            rows: specialized_rows,
        }
    }
    /// Checks if this matrix is empty

    /// Finds missing patterns for a type
    pub fn find_missing_patterns(
        &self,
        ty: TyId<'db>,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<SimplifiedPattern<'db>> {
        if self.is_empty() {
            return vec![SimplifiedPattern::Wildcard { binding: None }];
        }

        if self.rows[0].patterns.is_empty() {
            return Vec::new(); // All patterns covered
        }

        // Check for wildcard patterns that cover everything
        for row in &self.rows {
            if let Some(first_pat) = row.patterns.first() {
                if matches!(first_pat, SimplifiedPattern::Wildcard { .. }) {
                    return Vec::new(); // Wildcard found, all patterns are covered
                }
                
                // Also check OR patterns that may contain wildcards
                if let SimplifiedPattern::Or(patterns) = first_pat {
                    if patterns.iter().any(|p| matches!(p, SimplifiedPattern::Wildcard { .. })) {
                        return Vec::new(); // Wildcard in OR pattern, all patterns are covered
                    }
                }
            }
        }

        // Handle specific types
        if ty.is_bool(db) {
            return self.find_missing_bool_patterns(db);
        }

        if let Some(adt_def) = ty.adt_def(db) {
            if let AdtRef::Enum(enum_def) = adt_def.adt_ref(db) {
                return self.find_missing_enum_patterns(enum_def, db, ty);
            }
        }

        // If no specific handler, assume not exhaustive
        vec![SimplifiedPattern::Wildcard { binding: None }]
    }

    /// Find missing patterns for boolean type
    fn find_missing_bool_patterns(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<SimplifiedPattern<'db>> {
        // Check if true and false are covered
        let mut has_true = false;
        let mut has_false = false;

        for row in &self.rows {
            if let Some(SimplifiedPattern::Constructor { constructor, .. }) = row.patterns.first() {
                if let Constructor::Bool(value) = constructor {
                    if *value {
                        has_true = true;
                    } else {
                        has_false = true;
                    }
                }
            } else if let Some(SimplifiedPattern::Wildcard { .. }) = row.patterns.first() {
                // Wildcard covers both true and false
                return Vec::new();
            }
        }

        let mut missing = Vec::new();

        // Create a bool type identifier
        // Using the primitive type constructor instead of TyId::from_bool
        let bool_ty = TyId::bool(db);
        if !has_true {
            missing.push(SimplifiedPattern::Constructor {
                constructor: Constructor::Bool(true),
                subpatterns: Vec::new(),
                ty: bool_ty,
            });
        }

        if !has_false {
            missing.push(SimplifiedPattern::Constructor {
                constructor: Constructor::Bool(false),
                subpatterns: Vec::new(),
                ty: bool_ty,
            });
        }

        missing
    }

    /// Find missing patterns for enum type
    fn find_missing_enum_patterns(
        &self,
        enum_def: hir::hir_def::item::Enum<'db>,
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
    ) -> Vec<SimplifiedPattern<'db>> {
        let variants_list = enum_def.variants(db);
        let variants_data = variants_list.data(db);
        let mut missing_variants = Vec::new();

        // First, check if we have a wildcard pattern that covers everything
        for row in &self.rows {
            if let Some(pat) = row.patterns.first() {
                if matches!(pat, SimplifiedPattern::Wildcard { .. }) {
                    return Vec::new();  // Wildcard covers all variants
                }
                
                // Check for wildcards in OR patterns
                if let SimplifiedPattern::Or(patterns) = pat {
                    if patterns.iter().any(|p| matches!(p, SimplifiedPattern::Wildcard { .. })) {
                        return Vec::new();  // Wildcard in OR pattern covers all variants
                    }
                }
            }
        }

        // Check which variants are covered
        'variant_loop: for (idx, variant_def) in variants_data.iter().enumerate() {
            let enum_variant = hir::hir_def::item::EnumVariant::new(enum_def, idx);

            // For each variant, check if it's covered by any row
            for row in &self.rows {
                if let Some(pat) = row.patterns.first() {
                    match pat {
                        SimplifiedPattern::Constructor {
                            constructor: Constructor::Record(record),
                            ..
                        } => {
                            // Compare the variant with record
                            if let RecordLike::Variant(resolved_variant) = record {
                                if resolved_variant.variant.idx == enum_variant.idx {
                                    // This variant is covered
                                    continue 'variant_loop;
                                }
                            }
                        }
                        SimplifiedPattern::Or(patterns) => {
                            // Check if any pattern in the Or covers this variant
                            for or_pat in patterns {
                                if let SimplifiedPattern::Constructor {
                                    constructor: Constructor::Record(record),
                                    ..
                                } = or_pat
                                {
                                    // Compare the variant with record
                                    if let RecordLike::Variant(resolved_variant) = record {
                                        if resolved_variant.variant.idx == enum_variant.idx {
                                            // This variant is covered
                                            continue 'variant_loop;
                                        }
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }

            // If we get here, the variant isn't covered
            // Create a RecordLike from the enum variant
            let variant = ResolvedVariant {
                ty,
                variant: enum_variant,
                path: PathId::new(db, Partial::Present(IdentId::new(db, "_placeholder_variant_path_".to_string())), GenericArgListId::none(db), None),  // This is a placeholder path
            };
            let record = RecordLike::from_variant(variant);
            let constructor = Constructor::Record(record);

            // Create wildcard subpatterns for each field
            let field_count = match variant_def.kind {
                hir::hir_def::VariantKind::Unit => 0,
                hir::hir_def::VariantKind::Tuple(ref fields) => fields.data(db).len(),
                hir::hir_def::VariantKind::Record(ref fields) => fields.data(db).len(),
            };

            let mut subpatterns = Vec::with_capacity(field_count);
            for _ in 0..field_count {
                subpatterns.push(SimplifiedPattern::Wildcard { binding: None });
            }

            missing_variants.push(SimplifiedPattern::Constructor {
                constructor,
                subpatterns,
                ty,
            });
        }

        missing_variants
    }
}

pub struct PatternAnalyzer<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> PatternAnalyzer<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }

    /*
    /// Check if patterns are exhaustive for the given type
    pub fn check_exhaustiveness(
        &self,
        ty: TyId<'db>,
        patterns: &[Pattern<'db>],
    ) -> Result<(), Vec<String>> {
        // This method will be deprecated. Temporarily panic if called.
        // TODO: Remove this method once TyChecker::check_match is updated.
        panic!("check_exhaustiveness with Pattern type called. Use check_exhaustiveness_hir.");
        /*
        // Return Vec<String> for direct use in BodyDiag
        // Convert to simplified patterns
        let simplified_patterns: Vec<_> = patterns
            .iter()
            .map(|p| SimplifiedPattern::from_pattern(p, self.db))
            .collect();

        // Find missing patterns
        let missing_simplified = self.find_missing_patterns(ty, &simplified_patterns);

        if missing_simplified.is_empty() {
            Ok(())
        } else {
            let missing_user_strings: Vec<String> = missing_simplified
                .iter()
                .map(|p| self.simplified_to_user_pattern(p, ty))
                .collect();
            Err(missing_user_strings)
        }
        */
    }
    */

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
                return Ok(());  // Wildcard pattern found, match is exhaustive
            }
            
            // Also check for OR patterns containing wildcards
            if let HirPat::Or(left_id, right_id) = pat_id {
                let left_pat_data = left_id.data(self.db, body);
                let right_pat_data = right_id.data(self.db, body);
                if matches!(left_pat_data, &Partial::Present(HirPat::WildCard) | &Partial::Present(HirPat::Rest)) ||
                   matches!(right_pat_data, &Partial::Present(HirPat::WildCard) | &Partial::Present(HirPat::Rest)) {
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

    /*
    /// Check if a pattern is reachable after previous patterns
    pub fn check_reachability(
        &self,
        pattern: &Pattern<'db>,
        previous_patterns: &[Pattern<'db>],
    ) -> bool {
        // This method will be deprecated. Temporarily panic if called.
        // TODO: Remove this method once TyChecker::check_match is updated.
        panic!("check_reachability with Pattern type called. Use check_reachability_hir.");
        /*
        let simplified = SimplifiedPattern::from_pattern(pattern, self.db);
        let previous_simplified: Vec<_> = previous_patterns
            .iter()
            .map(|p| SimplifiedPattern::from_pattern(p, self.db))
            .collect();

        simplified.is_useful_after(&previous_simplified, self.db)
        */
    }
    */

    /// Check if a pattern is reachable after previous patterns, using HIR patterns.
    pub fn check_reachability(
        &self,
        hir_pat: &HirPat<'db>,
        previous_hir_pats: &[HirPat<'db>],
        body: HirBody<'db>,
    ) -> bool {
        // Special case: if any previous pattern is a wildcard, no subsequent pattern can be reached
        for prev_pat in previous_hir_pats {
            if matches!(prev_pat, &HirPat::WildCard) || matches!(prev_pat, &HirPat::Rest) {
                return false;  // If a previous pattern is a wildcard, nothing after it is reachable
            }
            
            // Also check for OR patterns containing wildcards
            if let HirPat::Or(left_id, right_id) = prev_pat {
                let left_pat_data = left_id.data(self.db, body);
                let right_pat_data = right_id.data(self.db, body);
                if matches!(left_pat_data, &Partial::Present(HirPat::WildCard) | &Partial::Present(HirPat::Rest)) ||
                   matches!(right_pat_data, &Partial::Present(HirPat::WildCard) | &Partial::Present(HirPat::Rest)) {
                    return false;
                }
            }
        }
        
        let simplified = SimplifiedPattern::from_hir_pat(hir_pat, self.db, body);
        let previous_simplified: Vec<_> = previous_hir_pats
            .iter()
            .map(|p| SimplifiedPattern::from_hir_pat(p, self.db, body))
            .collect();

        simplified.is_useful_after(&previous_simplified, self.db)
    }

    /// Find patterns that are not covered
    /// This is now a wrapper around the matrix implementation
    fn find_missing_patterns(
        &self,
        ty: TyId<'db>,
        patterns: &[SimplifiedPattern<'db>],
    ) -> Vec<SimplifiedPattern<'db>> {
        // Create a pattern matrix for analysis
        let matrix = PatternMatrix::new(patterns.to_vec());

        // Use the matrix to find missing patterns
        matrix.find_missing_patterns(ty, self.db)
    }

    /// Find missing patterns for enum type in analyzer
    fn find_missing_patterns_for_enum(
        &self,
        enum_def: hir::hir_def::item::Enum<'db>,
        patterns: &[SimplifiedPattern<'db>],
        ty: TyId<'db>,
    ) -> Vec<SimplifiedPattern<'db>> {
        self.analyze_enum_patterns(enum_def, patterns)
    }

    fn analyze_enum_patterns(
        &self,
        enum_def: hir::hir_def::item::Enum<'db>,
        patterns: &[SimplifiedPattern<'db>],
    ) -> Vec<SimplifiedPattern<'db>> {
        // Create a pattern matrix and use it to find missing patterns
        let matrix = PatternMatrix::new(patterns.to_vec());
        matrix.find_missing_enum_patterns(enum_def, self.db, TyId::never(self.db))
    }

    /// Find missing patterns for enum type - compatibility wrapper
    fn find_missing_enum_patterns(
        &self,
        enum_def: hir::hir_def::item::Enum<'db>,
        patterns: &[SimplifiedPattern<'db>],
    ) -> Vec<SimplifiedPattern<'db>> {
        // Use lower_adt helper function instead of directly creating AdtDef
        let adt_def = lower_adt(self.db, enum_def.into());
        let ty = TyId::adt(self.db, adt_def);
        self.find_missing_patterns_for_enum(enum_def, patterns, ty)
    }

    /// Find missing patterns for boolean type
    fn find_missing_bool_patterns(
        &self,
        patterns: &[SimplifiedPattern<'db>],
    ) -> Vec<SimplifiedPattern<'db>> {
        let bool_ty = TyId::bool(self.db);
        let mut missing = Vec::new();
        let mut true_covered = false;
        let mut false_covered = false;

        for p in patterns {
            match p {
                SimplifiedPattern::Constructor {
                    constructor: Constructor::Bool(val),
                    ..
                } => {
                    if *val {
                        true_covered = true;
                    } else {
                        false_covered = true;
                    }
                }
                SimplifiedPattern::Wildcard { .. } => {
                    true_covered = true;
                    false_covered = true;
                    break; // Wildcard covers all
                }
                SimplifiedPattern::Or(subpatterns) => {
                    // If an OR pattern covers both, mark them.
                    // This is a simplification; a more precise OR check would see if
                    // _any_ branch of the OR covers true/false.
                    if subpatterns.iter().any(|sp| {
                        matches!(
                            sp,
                            SimplifiedPattern::Constructor {
                                constructor: Constructor::Bool(true),
                                ..
                            }
                        )
                    }) {
                        true_covered = true;
                    }
                    if subpatterns.iter().any(|sp| {
                        matches!(
                            sp,
                            SimplifiedPattern::Constructor {
                                constructor: Constructor::Bool(false),
                                ..
                            }
                        )
                    }) {
                        false_covered = true;
                    }
                    if subpatterns
                        .iter()
                        .any(|sp| matches!(sp, SimplifiedPattern::Wildcard { .. }))
                    {
                        true_covered = true;
                        false_covered = true;
                    }
                }
                _ => {}
            }
            if true_covered && false_covered {
                break;
            }
        }

        if !true_covered {
            missing.push(SimplifiedPattern::Constructor {
                constructor: Constructor::Bool(true),
                subpatterns: Vec::new(),
                ty: bool_ty,
            });
        }
        if !false_covered {
            missing.push(SimplifiedPattern::Constructor {
                constructor: Constructor::Bool(false),
                subpatterns: Vec::new(),
                ty: bool_ty,
            });
        }
        missing
    }

    /// Find missing patterns for enum types.
    fn find_missing_enum_patterns_detailed(
        &self,
        enum_def: hir::hir_def::item::Enum<'db>,
        patterns: &[SimplifiedPattern<'db>],
    ) -> Vec<SimplifiedPattern<'db>> {
        let mut missing_variants = Vec::new();
        let all_variant_defs_list_id = enum_def.variants(self.db); // This is VariantDefListId<'db>
                                                                   // Store EnumVariantId (alias for EnumVariantDefId) for uniqueness of definition
        let mut covered_variants: std::collections::HashSet<
            hir::hir_def::item::EnumVariant<'db>, // This is EnumVariantDefId
        > = std::collections::HashSet::new();

        if patterns
            .iter()
            .any(|p| matches!(p, SimplifiedPattern::Wildcard { .. }))
        {
            return Vec::new(); // Wildcard covers all variants
        }

        for pattern_case in patterns {
            match pattern_case {
                SimplifiedPattern::Constructor {
                    constructor: Constructor::Record(RecordLike::Variant(resolved_variant)),
                    ..
                } => {
                    // resolved_variant.variant is already an EnumVariant, just use it
                    let variant_id = resolved_variant.variant;
                    covered_variants.insert(variant_id);
                }
                SimplifiedPattern::Or(subpatterns) => {
                    for sub_p in subpatterns {
                        if let SimplifiedPattern::Constructor {
                            constructor: Constructor::Record(RecordLike::Variant(resolved_variant)),
                            ..
                        } = sub_p
                        {
                            let variant_id = resolved_variant.variant;
                            covered_variants.insert(variant_id);
                        }
                        // Note: A wildcard inside an OR pattern makes the OR pattern exhaustive for enums.
                        if matches!(sub_p, SimplifiedPattern::Wildcard { .. }) {
                            return Vec::new();
                        }
                    }
                }
                // Other simplified patterns (Wildcard already handled, other Constructors like Int/Bool aren't enum variants)
                _ => {}
            }
        }

        let all_variant_defs_data = all_variant_defs_list_id.data(self.db);
        for (idx, variant_def_data) in all_variant_defs_data.iter().enumerate() {
            let current_variant_id = EnumVariant::new(enum_def, idx);
            if !covered_variants.contains(&current_variant_id) {
                // Construct a SimplifiedPattern representing this missing variant.
                // Convert enum_def to AdtRef and use lower_adt to get the type
                let enum_adt_ref = AdtRef::Enum(enum_def);
                let enum_ty = TyId::adt(self.db, lower_adt(self.db, enum_adt_ref));

                // Create a minimal path using from_ident with the enum's name
                let enum_name_ident = enum_def.name(self.db).to_opt().unwrap_or_else(|| {
                    hir::hir_def::IdentId::new(self.db, "_MISSING_ENUM_NAME_".to_string())
                });
                let path = hir::hir_def::path::PathId::from_ident(self.db, enum_name_ident);

                let resolved_variant_for_missing = ResolvedVariant {
                    ty: enum_ty,
                    path,
                    variant: EnumVariant {
                        enum_: enum_def,
                        idx: idx as u16,
                    },
                };

                let kind = variant_def_data.kind; // kind is a direct field on VariantDef

                // Determine the constructor representation for the missing variant
                let (constructor_for_missing, arity) = match kind {
                    hir::hir_def::VariantKind::Tuple(list_id) => {
                        // For a missing tuple variant like `Enum.MissingTuple(..)`, represent it as Constructor::Tuple
                        (
                            Constructor::Tuple(list_id.len(self.db)),
                            list_id.len(self.db),
                        )
                    }
                    hir::hir_def::VariantKind::Record(_list_id) => {
                        // For a missing record variant `Enum.MissingRecord{..}`, represent with RecordLike::Variant
                        (
                            Constructor::Record(RecordLike::Variant(
                                resolved_variant_for_missing.clone(),
                            )),
                            _list_id.data(self.db).len(),
                        )
                    }
                    hir::hir_def::VariantKind::Unit => {
                        // For a missing unit variant `Enum.MissingUnit`, represent with RecordLike::Variant (0 arity)
                        (
                            Constructor::Record(RecordLike::Variant(
                                resolved_variant_for_missing.clone(),
                            )),
                            0,
                        )
                    }
                };

                let subpatterns = (0..arity)
                    .map(|_| SimplifiedPattern::Wildcard { binding: None })
                    .collect();

                missing_variants.push(SimplifiedPattern::Constructor {
                    constructor: constructor_for_missing,
                    subpatterns,
                    ty: enum_ty,
                });
            }
        }
        missing_variants
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
