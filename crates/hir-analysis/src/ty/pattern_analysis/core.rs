use crate::name_resolution::{resolve_path, PathRes, ResolvedVariant};
use crate::ty::adt_def::AdtRef;
// use crate::ty::pattern::Pattern; // TODO: Remove this import once Pattern type is fully deprecated
use crate::ty::ty_check::RecordLike;
use crate::ty::ty_def::TyId;
use crate::ty::AdtRef as HirAdtRef;
use crate::HirAnalysisDb;
use hir::hir_def::{
    Body as HirBody, GenericArgListId, IdentId, LitKind, Partial, Pat as HirPat, PathId,
    VariantKind,
};
use rustc_hash::FxHashMap;

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

    /// Checks if this constructor represents the same enum variant as another
    /// This is critical for correctly handling imported enum variants vs qualified ones
    /// Checks if this constructor represents the same enum variant as another
    /// Critical for correctly handling imported enum variants vs qualified ones
    pub fn is_same_variant(&self, other: &Self, _db: &'db dyn HirAnalysisDb) -> bool {
        match (self, other) {
            // Simple constructors match if they're equal
            (Constructor::Bool(a), Constructor::Bool(b)) => a == b,
            (Constructor::Int(a), Constructor::Int(b)) => a == b,
            (Constructor::Tuple(a), Constructor::Tuple(b)) => a == b,

            // For record-like constructors, compare the actual enum variants
            (
                Constructor::Record(RecordLike::Variant(var_a)),
                Constructor::Record(RecordLike::Variant(var_b)),
            ) => {
                // ONLY compare the enum definition and variant index, ignoring path representation
                // This ensures that MyTag::A and A (imported) are considered the same variant
                var_a.variant.enum_ == var_b.variant.enum_ && var_a.variant.idx == var_b.variant.idx
            }

            // Different constructor kinds never match
            _ => false,
        }
    }
}

impl<'db> SimplifiedPattern<'db> {
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
                        matches!(first, SimplifiedPattern::Wildcard { .. })
                            || if let SimplifiedPattern::Or(pats) = first {
                                pats.iter()
                                    .any(|p| matches!(p, SimplifiedPattern::Wildcard { .. }))
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
                ty: _,
            } => {
                // For enum variants, check if this variant is already covered by previous patterns
                if let Constructor::Record(RecordLike::Variant(current_variant)) = constructor {
                    // Check each previous pattern to see if it covers this variant
                    let is_covered = previous.iter().any(|prev| {
                        match prev {
                            SimplifiedPattern::Constructor {
                                constructor: Constructor::Record(RecordLike::Variant(prev_variant)),
                                ..
                            } => {
                                // ONLY compare enum definition and variant index - this is the key for
                                // correctly handling imported variants (A) vs qualified ones (MyTag::A)
                                // The paths don't matter, just the underlying enum and variant index
                                current_variant.variant.enum_ == prev_variant.variant.enum_
                                    && current_variant.variant.idx == prev_variant.variant.idx
                            }
                            SimplifiedPattern::Wildcard { .. } => true, // Wildcard covers everything
                            SimplifiedPattern::Or(or_patterns) => {
                                or_patterns.iter().any(|p| {
                                    if let SimplifiedPattern::Constructor {
                                        constructor:
                                            Constructor::Record(RecordLike::Variant(prev_variant)),
                                        ..
                                    } = p
                                    {
                                        // ONLY compare enum definition and variant index - ignore path representation
                                        // This ensures imported variants (A) match qualified ones (MyTag::A)
                                        current_variant.variant.enum_ == prev_variant.variant.enum_
                                            && current_variant.variant.idx
                                                == prev_variant.variant.idx
                                    } else if let SimplifiedPattern::Wildcard { .. } = p {
                                        true
                                    } else {
                                        false
                                    }
                                })
                            }
                            _ => false,
                        }
                    });

                    if is_covered {
                        // If this variant is already covered by a previous pattern, it's not useful
                        return false;
                    }
                }

                // Specialize by the constructor
                let specialized = matrix.specialize(constructor, db);

                if specialized.is_empty() {
                    // No row specializes to this constructor, so it's useful
                    return true;
                }

                // Check if subpatterns are useful in the specialized matrix
                if subpatterns.is_empty() {
                    // No subpatterns to check further
                    return false; // No subpatterns, so covered by previous patterns
                }

                // For tuples and records with subpatterns, we need to check each subpattern
                let mut all_subpatterns_useful = true;

                for (i, subpattern) in subpatterns.iter().enumerate() {
                    let mut matrix_patterns = Vec::new();
                    for row in &specialized.rows {
                        if let Some(pattern) = row.patterns.first() {
                            match pattern {
                                SimplifiedPattern::Constructor {
                                    subpatterns: sub, ..
                                } => {
                                    if i < sub.len() {
                                        matrix_patterns.push(sub[i].clone());
                                    }
                                }
                                SimplifiedPattern::Wildcard { .. } => {
                                    // Wildcard can match anything at this position
                                    matrix_patterns.push(SimplifiedPattern::Wildcard { binding: None });
                                }
                                SimplifiedPattern::Or(patterns) => {
                                    for or_pat in patterns {
                                        if let SimplifiedPattern::Constructor {
                                            subpatterns: sub, ..
                                        } = or_pat
                                        {
                                            if i < sub.len() {
                                                matrix_patterns.push(sub[i].clone());
                                            }
                                        } else if let SimplifiedPattern::Wildcard { .. } = or_pat {
                                            matrix_patterns.push(SimplifiedPattern::Wildcard { binding: None });
                                        }
                                    }
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
            .map(|(i, pat)| PatternRow {
                patterns: vec![pat],
                arm_index: i,
            })
            .collect();

        Self { rows }
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
    /// This is a key operation for pattern matching analysis
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
                    } => {
                        // For constructor patterns, check if this constructor matches the one we're specializing on
                        // This needs to handle both direct equality and "same variant" equality for imported variants
                        if constructor == ctor || constructor.is_same_variant(ctor, db) {
                            // This pattern matches the constructor we're specializing on
                            // is_same_variant ensures that imported variants (A) and qualified ones (MyTag::A) match
                            Some(subpatterns.clone())
                        } else {
                            None
                        }
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
                                // Check if this constructor matches the one we're specializing on
                                // Need to handle both direct equality and "same variant" equality
                                if constructor == ctor || constructor.is_same_variant(ctor, db) {
                                    // Handle both imported variants (A) and qualified ones (MyTag::A)
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
                    } // All cases handled above - no default needed
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
                    if patterns
                        .iter()
                        .any(|p| matches!(p, SimplifiedPattern::Wildcard { .. }))
                    {
                        return Vec::new(); // Wildcard in OR pattern, all patterns are covered
                    }
                }
            }
        }

        // Handle specific types
        if ty.is_bool(db) {
            return self.find_missing_bool_patterns(db);
        }

        if ty.is_tuple(db) {
            return self.find_missing_tuple_patterns(ty, db);
        }

        if let Some(adt_def) = ty.adt_def(db) {
            if let AdtRef::Enum(enum_def) = adt_def.adt_ref(db) {
                return self.find_missing_enum_patterns(enum_def, db, ty);
            }
        }

        // If no specific handler, assume not exhaustive
        vec![SimplifiedPattern::Wildcard { binding: None }]
    }

    /// Find missing patterns for tuple types
    fn find_missing_tuple_patterns(
        &self,
        ty: TyId<'db>,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<SimplifiedPattern<'db>> {
        // Get tuple elements
        let (_, tuple_elems) = ty.decompose_ty_app(db);
        
        // Check if we have any completely covered cases from wildcards
        for row in &self.rows {
            if let Some(SimplifiedPattern::Wildcard { .. }) = row.patterns.first() {
                return Vec::new(); // All patterns covered by a wildcard
            }
            
            // Also check Or patterns that might contain wildcards
            if let Some(SimplifiedPattern::Or(patterns)) = row.patterns.first() {
                if patterns.iter().any(|p| matches!(p, SimplifiedPattern::Wildcard { .. })) {
                    return Vec::new(); // All patterns covered by a wildcard
                }
            }
            
            // Also check Or patterns that might contain wildcards
            if let Some(SimplifiedPattern::Or(patterns)) = row.patterns.first() {
                if patterns.iter().any(|p| matches!(p, SimplifiedPattern::Wildcard { .. })) {
                    return Vec::new(); // All patterns covered by a wildcard
                }
            }
        }
        
        // Check if we have any tuple constructors with the right arity
        let mut has_tuple_patterns = false;
        for row in &self.rows {
            if let Some(SimplifiedPattern::Constructor {
                constructor: Constructor::Tuple(n),
                ..
            }) = row.patterns.first()
            {
                if *n == tuple_elems.len() {
                    has_tuple_patterns = true;
                    break;
                }
            } else if let Some(SimplifiedPattern::Or(patterns)) = row.patterns.first() {
                if patterns.iter().any(|p| {
                    if let SimplifiedPattern::Constructor {
                        constructor: Constructor::Tuple(n),
                        ..
                    } = p
                    {
                        *n == tuple_elems.len()
                    } else {
                        false
                    }
                }) {
                    has_tuple_patterns = true;
                    break;
                }
            }
        }
        
        // If no tuple patterns of the right arity, the entire space is uncovered
        if !has_tuple_patterns {
            return vec![SimplifiedPattern::Wildcard { binding: None }];
        }
        
        // Check each element for missing patterns
        let mut missing_patterns = Vec::new();
        
        for i in 0..tuple_elems.len() {
            let specialized = self.specialize_tuple_element(i, tuple_elems.len());
            if specialized.rows.is_empty() {
                // This element is completely uncovered
                let mut subpatterns = vec![SimplifiedPattern::Wildcard { binding: None }; tuple_elems.len()];
                
                // For the uncovered element, use the missing patterns
                let element_missing = vec![SimplifiedPattern::Wildcard { binding: None }];
                for missing_elem in element_missing {
                    let mut specific_subpatterns = subpatterns.clone();
                    specific_subpatterns[i] = missing_elem;
                    
                    missing_patterns.push(SimplifiedPattern::Constructor {
                        constructor: Constructor::Tuple(tuple_elems.len()),
                        subpatterns: specific_subpatterns,
                        ty,
                    });
                }
            } else {
                // Check if this element has missing patterns
                let element_missing = specialized.find_missing_patterns(tuple_elems[i], db);
                if !element_missing.is_empty() {
                    // Build tuple patterns with the missing element patterns
                    for missing_elem in element_missing {
                        let mut subpatterns = vec![SimplifiedPattern::Wildcard { binding: None }; tuple_elems.len()];
                        subpatterns[i] = missing_elem;
                        
                        missing_patterns.push(SimplifiedPattern::Constructor {
                            constructor: Constructor::Tuple(tuple_elems.len()),
                            subpatterns,
                            ty,
                        });
                    }
                }
            }
        }
        
        if missing_patterns.is_empty() {
            // All elements are covered, so the tuple pattern is exhaustive
            Vec::new()
        } else {
            // Return the specific missing patterns
            missing_patterns
        }
    }
    
    /// Create a specialized matrix for a specific tuple element
    fn specialize_tuple_element(
        &self,
        element_idx: usize,
        arity: usize,
    ) -> PatternMatrix<'db> {
        let mut specialized_rows = Vec::new();
        
        for row in &self.rows {
            if let Some(SimplifiedPattern::Constructor {
                constructor: Constructor::Tuple(n),
                subpatterns,
                ..
            }) = row.patterns.first() {
                if *n == arity && element_idx < subpatterns.len() {
                    // Take the pattern at the specified element index
                    let element_pattern = &subpatterns[element_idx];
                    
                    // Create a new row with just this element's pattern
                    let mut new_patterns = vec![element_pattern.clone()];
                    new_patterns.extend_from_slice(&row.patterns[1..]);
                    
                    specialized_rows.push(PatternRow {
                        patterns: new_patterns,
                        arm_index: row.arm_index,
                    });
                }
            } else if let Some(SimplifiedPattern::Wildcard { .. }) = row.patterns.first() {
                // For wildcard patterns, add a wildcard for this element
                let mut new_patterns = vec![SimplifiedPattern::Wildcard { binding: None }];
                new_patterns.extend_from_slice(&row.patterns[1..]);
                
                specialized_rows.push(PatternRow {
                    patterns: new_patterns,
                    arm_index: row.arm_index,
                });
            } else if let Some(SimplifiedPattern::Or(patterns)) = row.patterns.first() {
                // Handle Or patterns by expanding each subpattern
                for pattern in patterns {
                    if let SimplifiedPattern::Constructor {
                        constructor: Constructor::Tuple(n),
                        subpatterns,
                        ..
                    } = pattern
                    {
                        if *n == arity && element_idx < subpatterns.len() {
                            let element_pattern = &subpatterns[element_idx];
                            let mut new_patterns = vec![element_pattern.clone()];
                            new_patterns.extend_from_slice(&row.patterns[1..]);
                            
                            specialized_rows.push(PatternRow {
                                patterns: new_patterns,
                                arm_index: row.arm_index,
                            });
                        }
                    } else if let SimplifiedPattern::Wildcard { .. } = pattern {
                        let mut new_patterns = vec![SimplifiedPattern::Wildcard { binding: None }];
                        new_patterns.extend_from_slice(&row.patterns[1..]);
                        
                        specialized_rows.push(PatternRow {
                            patterns: new_patterns,
                            arm_index: row.arm_index,
                        });
                    }
                }
            }
        }
        
        PatternMatrix { rows: specialized_rows }
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
                    return Vec::new(); // Wildcard covers all variants
                }

                // Check for wildcards in OR patterns
                if let SimplifiedPattern::Or(patterns) = pat {
                    if patterns
                        .iter()
                        .any(|p| matches!(p, SimplifiedPattern::Wildcard { .. }))
                    {
                        return Vec::new(); // Wildcard in OR pattern covers all variants
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
                path: PathId::new(
                    db,
                    Partial::Present(IdentId::new(db, "_placeholder_variant_path_".to_string())),
                    GenericArgListId::none(db),
                    None,
                ), // This is a placeholder path
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


