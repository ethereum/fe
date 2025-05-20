use crate::name_resolution::ResolvedVariant;
use crate::ty::adt_def::{lower_adt, AdtRef};
// use crate::ty::pattern::Pattern; // TODO: Remove this import once Pattern type is fully deprecated
use crate::ty::ty_check::RecordLike;
use crate::ty::ty_def::TyId;
use crate::HirAnalysisDb;
use hir::hir_def::item::EnumVariant;
use hir::hir_def::{Body as HirBody, LitKind, Partial, Pat as HirPat};
// EnumVariant itself serves as a key for covered_variants

/// Simplified pattern representation for analysis
/// Based on "Warnings for pattern matching" paper
#[derive(Clone, Debug)]
pub enum SimplifiedPattern<'db> {
    /// Wildcard pattern that matches anything
    Wildcard,

    /// Or pattern for alternatives
    Or(Vec<SimplifiedPattern<'db>>),

    /// Constructor pattern with subpatterns
    Constructor {
        /// The constructor (record, tuple, literal, etc.)
        constructor: Constructor<'db>,
        /// Subpatterns for the constructor's fields
        subpatterns: Vec<SimplifiedPattern<'db>>,
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

impl<'db> SimplifiedPattern<'db> {
    /// Convert an HIR pattern (Pat) to a simplified pattern for analysis.
    pub fn from_hir_pat(
        pat_data: &HirPat<'db>,
        db: &'db dyn HirAnalysisDb,
        body: HirBody<'db>, // Needed to resolve PatIds for nested patterns
    ) -> Self {
        match pat_data {
            HirPat::WildCard => SimplifiedPattern::Wildcard,
            HirPat::Rest => SimplifiedPattern::Wildcard, // Rest is often treated as wildcard in broad phase
            HirPat::Lit(lit_kind_partial) => {
                if let Partial::Present(lit_kind) = lit_kind_partial {
                    match lit_kind {
                        LitKind::Bool(value) => SimplifiedPattern::Constructor {
                            constructor: Constructor::Bool(*value),
                            subpatterns: Vec::new(),
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
                            }
                        }
                        _ => SimplifiedPattern::Wildcard, // Other literals, treat as wildcard for now
                    }
                } else {
                    SimplifiedPattern::Wildcard // Absent literal
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
                            Partial::Absent => SimplifiedPattern::Wildcard, // Or handle error
                        }
                    })
                    .collect();
                SimplifiedPattern::Constructor {
                    constructor: Constructor::Tuple(subpatterns.len()),
                    subpatterns,
                }
            }
            HirPat::Path(_path_id_partial, _is_mut_binding) => {
                // For now, treat all paths as wildcards.
                // A more complete implementation would resolve the path to determine
                // if it's an enum variant (Constructor) or a binding (Wildcard).
                SimplifiedPattern::Wildcard
            }
            HirPat::PathTuple(_path_id_partial, elements_pat_ids) => {
                // For now, ignore _path_id_partial and treat as a regular tuple.
                // A more complete implementation would use _path_id_partial to create
                // a specific constructor for the enum variant.
                let subpatterns: Vec<SimplifiedPattern> = elements_pat_ids
                    .iter()
                    .map(|pat_id| {
                        let pat_partial_data = pat_id.data(db, body);
                        match pat_partial_data {
                            Partial::Present(pat_actual_data) => {
                                SimplifiedPattern::from_hir_pat(pat_actual_data, db, body)
                            }
                            Partial::Absent => SimplifiedPattern::Wildcard, // Or handle error
                        }
                    })
                    .collect();
                SimplifiedPattern::Constructor {
                    constructor: Constructor::Tuple(subpatterns.len()),
                    subpatterns,
                }
            }
            HirPat::Record(path_id_partial, fields_vec) => {
                // For now, use a dummy RecordLike based on the path's first identifier if available.
                // A more complete implementation would resolve the path to a struct/enum variant.
                let record_like = if let Partial::Present(path_id) = path_id_partial {
                    if let Partial::Present(ident) = path_id.ident(db) {
                        RecordLike::Dummy(ident)
                    } else {
                        // Fallback if ident is not present in path
                        RecordLike::Dummy(hir::hir_def::IdentId::new(
                            db,
                            "_AnonymousRecord".to_string(),
                        ))
                    }
                } else {
                    // Fallback if path is not present
                    RecordLike::Dummy(hir::hir_def::IdentId::new(
                        db,
                        "_AnonymousRecord".to_string(),
                    ))
                };

                let subpatterns: Vec<SimplifiedPattern> = fields_vec
                    .iter()
                    .map(|field_pat_entry| {
                        // field_pat_entry is RecordPatField { label, pat }
                        // We primarily care about the pattern for the field.
                        let pat_partial_data = field_pat_entry.pat.data(db, body);
                        match pat_partial_data {
                            Partial::Present(pat_actual_data) => {
                                SimplifiedPattern::from_hir_pat(pat_actual_data, db, body)
                            }
                            Partial::Absent => SimplifiedPattern::Wildcard, // Or handle error
                        }
                    })
                    .collect();

                SimplifiedPattern::Constructor {
                    constructor: Constructor::Record(record_like),
                    subpatterns,
                }
            }
            HirPat::Or(lhs_pat_id, rhs_pat_id) => {
                let lhs_simplified_pat = match lhs_pat_id.data(db, body) {
                    Partial::Present(lhs_pat_actual_data) => {
                        SimplifiedPattern::from_hir_pat(lhs_pat_actual_data, db, body)
                    }
                    Partial::Absent => SimplifiedPattern::Wildcard, // Or handle error
                };
                let rhs_simplified_pat = match rhs_pat_id.data(db, body) {
                    Partial::Present(rhs_pat_actual_data) => {
                        SimplifiedPattern::from_hir_pat(rhs_pat_actual_data, db, body)
                    }
                    Partial::Absent => SimplifiedPattern::Wildcard, // Or handle error
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
                    constructor: Constructor::Record(record.clone()),
                    subpatterns,
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
        // If any preceding pattern is a wildcard, this pattern cannot be useful.
        if previous
            .iter()
            .any(|p| matches!(p, SimplifiedPattern::Wildcard))
        {
            return false;
        }

        // If this pattern is an Or pattern, it's useful if any of its subpatterns are useful.
        if let SimplifiedPattern::Or(subpatterns) = self {
            return subpatterns.iter().any(|p| p.is_useful_after(previous, db));
        }

        // For Wildcard and Constructor patterns:
        // At this point, we know no previous pattern was a wildcard.
        // A Wildcard pattern is always useful if no preceding pattern was a wildcard.
        // A Constructor pattern is also considered useful here.
        // A more sophisticated check for constructors would involve specializing `previous`
        // patterns against the current constructor and recursing, as per Maranget's paper.
        true
    }

    /// Check if this pattern is irrefutable (always matches)
    pub fn is_irrefutable(&self) -> bool {
        match self {
            SimplifiedPattern::Wildcard => true,
            SimplifiedPattern::Or(patterns) => patterns.iter().any(|p| p.is_irrefutable()),
            SimplifiedPattern::Constructor { .. } => false,
        }
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
        let simplified_patterns: Vec<_> = hir_pats
            .iter()
            .map(|p| SimplifiedPattern::from_hir_pat(p, self.db, body))
            .collect();

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
        let simplified = SimplifiedPattern::from_hir_pat(hir_pat, self.db, body);
        let previous_simplified: Vec<_> = previous_hir_pats
            .iter()
            .map(|p| SimplifiedPattern::from_hir_pat(p, self.db, body))
            .collect();

        simplified.is_useful_after(&previous_simplified, self.db)
    }

    /// Find patterns that are not covered
    fn find_missing_patterns(
        &self,
        ty: TyId<'db>,
        patterns: &[SimplifiedPattern<'db>],
    ) -> Vec<SimplifiedPattern<'db>> {
        // Dispatch to specific handlers based on type
        if ty.is_bool(self.db) {
            return self.find_missing_bool_patterns(patterns);
        }

        if let Some(adt_def) = ty.adt_def(self.db) {
            if let AdtRef::Enum(enum_def) = adt_def.adt_ref(self.db) {
                return self.find_missing_enum_patterns(enum_def, patterns);
            }
            // TODO: Add exhaustiveness for structs if applicable (e.g. if all fields must be specified)
            // For now, structs are considered "covered" if a pattern of that struct type exists.
        }

        // TODO: Add exhaustiveness for other types like tuples, integers (ranges), etc.

        // Default: if no specific handler, assume covered or analysis not implemented yet.
        // A truly exhaustive check for arbitrary types is complex.
        // For now, if any pattern is a wildcard, consider it exhaustive.
        if patterns
            .iter()
            .any(|p| matches!(p, SimplifiedPattern::Wildcard))
        {
            return Vec::new();
        }
        // Otherwise, if no wildcard and no specific handler, we can't determine missing patterns yet.
        // Returning an empty vec means "no missing patterns found by *this basic analysis*".
        // A more advanced analysis might find missing patterns here.
        Vec::new()
    }

    /// Find missing patterns for boolean type.
    fn find_missing_bool_patterns(
        &self,
        patterns: &[SimplifiedPattern<'db>],
    ) -> Vec<SimplifiedPattern<'db>> {
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
                SimplifiedPattern::Wildcard => {
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
                        .any(|sp| matches!(sp, SimplifiedPattern::Wildcard))
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
            });
        }
        if !false_covered {
            missing.push(SimplifiedPattern::Constructor {
                constructor: Constructor::Bool(false),
                subpatterns: Vec::new(),
            });
        }
        missing
    }

    /// Find missing patterns for enum types.
    fn find_missing_enum_patterns(
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
            .any(|p| matches!(p, SimplifiedPattern::Wildcard))
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
                        if matches!(sub_p, SimplifiedPattern::Wildcard) {
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

                let subpatterns = vec![SimplifiedPattern::Wildcard; arity];

                missing_variants.push(SimplifiedPattern::Constructor {
                    constructor: constructor_for_missing,
                    subpatterns,
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
            SimplifiedPattern::Wildcard => "_".to_string(),
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
                                s.push_str(&self.simplified_to_user_pattern(
                                    sub_p,
                                    _original_scrutinee_ty,
                                ));
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
