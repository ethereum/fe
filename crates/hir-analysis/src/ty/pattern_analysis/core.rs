use crate::name_resolution::ResolvedVariant;
use crate::ty::adt_def::AdtRef;
// use crate::ty::pattern::Pattern; // TODO: Remove this import once Pattern type is fully deprecated
use crate::ty::ty_check::{RecordLike, TupleLike};
use crate::ty::ty_def::TyId;
use crate::HirAnalysisDb;
use hir::hir_def::{GenericArgListId, IdentId, Partial, PathId, VariantKind};

// Type aliases for better readability
/// A collection of pattern rows forming the complete pattern matrix

/// The index of a match arm in the original source code
pub type MatchArmIndex = usize;

/// Simplified pattern representation for analysis
///
/// This is the core abstraction that converts Fe's HIR patterns into a simpler
/// representation suitable for the pattern matching algorithm. Each pattern is
/// either a wildcard (matches anything), an OR of alternatives, or a constructor
/// with sub-patterns.
///
/// Based on "Warnings for pattern matching" paper by Luc Maranget.
#[derive(Clone, Debug)]
pub enum SimplifiedPattern<'db> {
    /// Wildcard pattern that matches any value
    ///
    /// Examples: `_`, `x`, `mut y`
    /// In the algorithm, wildcards are handled by the δ (delta) specialization operation.
    Wildcard {
        /// Optional binding name for the matched value
        binding: Option<&'db str>,
    },

    /// OR pattern representing multiple alternatives
    ///
    /// Example: `A | B | C` becomes `Or([A, B, C])`
    /// The algorithm checks each alternative separately for usefulness.
    Or(Vec<SimplifiedPattern<'db>>),

    /// Constructor pattern with sub-patterns
    ///
    /// Examples:
    /// - `Some(x)` → Constructor { constructor: Variant(Some), subpatterns: [Wildcard] }
    /// - `(a, b)` → Constructor { constructor: Tuple(2), subpatterns: [a, b] }
    /// - `true` → Constructor { constructor: Bool(true), subpatterns: [] }
    ///
    /// In the algorithm, constructors are handled by the φ (phi) specialization operation.
    Constructor {
        /// The constructor that creates this pattern (tuple, record, literal, etc.)
        constructor: Constructor<'db>,
        /// Sub-patterns for the constructor's fields/elements
        subpatterns: Vec<SimplifiedPattern<'db>>,
        /// The type of values this pattern matches
        ty: TyId<'db>,
    },
}

/// Represents a constructor in pattern matching
///
/// Constructors are the "shape" of data that patterns match against. Each constructor
/// defines how many sub-patterns it expects and what values it can match.
///
/// This is a key concept in the pattern matching algorithm - patterns with the same
/// constructor are grouped together during specialization.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Constructor<'db> {
    /// Record constructor (struct or enum variant)
    ///
    /// Examples:
    /// - `Point { x, y }` (struct)
    /// - `Some(value)` (enum variant)
    /// - `Color::Red` (unit variant)
    Record(RecordLike<'db>),

    /// Tuple-like constructor (unified tuple types and tuple variants)
    ///
    /// Examples:
    /// - `(a, b)` → TupleLike(TupleLike::Type(tuple_ty))
    /// - `Some(x)` → TupleLike(TupleLike::Variant(some_variant))
    /// - `Point(x, y)` → TupleLike(TupleLike::Type(point_ty))
    TupleLike(TupleLike<'db>),

    /// Tuple constructor with specific arity (number of elements)
    ///
    /// Examples:
    /// - `(a, b)` → Tuple(2)
    /// - `(x, y, z)` → Tuple(3)
    /// - `()` → Tuple(0)
    Tuple(usize),

    /// Boolean literal constructor
    ///
    /// Examples:
    /// - `true` → Bool(true)
    /// - `false` → Bool(false)
    Bool(bool),

    /// Integer literal constructor
    ///
    /// Examples:
    /// - `42` → Int(42)
    /// - `-1` → Int(-1)
    /// - `0` → Int(0)
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
            Constructor::TupleLike(tuple_like) => tuple_like.arity(db),
            Constructor::Tuple(arity) => *arity,
            Constructor::Bool(_) | Constructor::Int(_) => 0,
        }
    }

    /// Checks if this constructor represents the same enum variant as another
    /// This is critical for correctly handling imported enum variants vs qualified ones
    /// Checks if this constructor represents the same enum variant as another
    /// Critical for correctly handling imported enum variants vs qualified ones
    pub fn is_same_variant(&self, other: &Self, db: &'db dyn HirAnalysisDb) -> bool {
        match (self, other) {
            // Simple constructors match if they're equal
            (Constructor::Bool(a), Constructor::Bool(b)) => a == b,
            (Constructor::Int(a), Constructor::Int(b)) => a == b,
            (Constructor::Tuple(a), Constructor::Tuple(b)) => a == b,

            // TupleLike constructors use unified compatibility check
            (Constructor::TupleLike(a), Constructor::TupleLike(b)) => a.is_compatible_with(b, db),

            // Cross-compatibility: TupleLike with old Tuple constructor
            (Constructor::TupleLike(tuple_like), Constructor::Tuple(arity)) |
            (Constructor::Tuple(arity), Constructor::TupleLike(tuple_like)) => {
                tuple_like.arity(db) == *arity
            }

            // For record-like constructors, compare the actual enum variants
            (
                Constructor::Record(RecordLike::Variant(var_a)),
                Constructor::Record(RecordLike::Variant(var_b)),
            ) => {
                // Check if they refer to the same enum variant
                var_a.variant.idx == var_b.variant.idx
            }

            // Everything else is not the same
            _ => false,
        }
    }

    /// Create a TupleLike constructor from a tuple type
    pub fn tuple_like_from_type(ty: TyId<'db>) -> Self {
        Constructor::TupleLike(TupleLike::Type(ty))
    }

    /// Create a TupleLike constructor from a variant
    pub fn tuple_like_from_variant(variant: ResolvedVariant<'db>) -> Self {
        Constructor::TupleLike(TupleLike::Variant(variant))
    }

    /// Convert a legacy Tuple constructor to TupleLike (for migration)
    pub fn upgrade_tuple_to_tuple_like(&self, ty: TyId<'db>) -> Option<Self> {
        match self {
            Constructor::Tuple(_arity) => Some(Constructor::TupleLike(TupleLike::Type(ty))),
            _ => None,
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

        // Create a matrix from the previous patterns (what might shadow our pattern)
        let matrix = PatternMatrix::new(previous.to_vec());

        match self {
            SimplifiedPattern::Wildcard { .. } => {
                // For wildcard patterns, use the default case specialization.
                // A wildcard is useful if there are values that are not covered by
                // any constructor patterns in the matrix.

                // Use default case specialization to check if there are existing wildcards
                let default_matrix = matrix.d_specialize();

                // If the default matrix is empty, there are no wildcards in the previous patterns,
                // so our wildcard is useful. If it's non-empty, there are existing wildcards
                // that might already cover the cases our wildcard would cover.
                default_matrix.is_empty()
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
                let specialized = matrix.phi_specialize(constructor, db); // P_c

                if specialized.is_empty() {
                    // No row specializes to this constructor, so it's useful
                    return true;
                }

                // Check if subpatterns are useful in the specialized matrix
                if subpatterns.is_empty() {
                    // No subpatterns to check further
                    return false; // No subpatterns, so covered by previous patterns
                }

                // For tuples and records with subpatterns, check usefulness recursively.
                return is_subpattern_vector_useful(subpatterns, &specialized, db);
            }

            SimplifiedPattern::Or(subpatterns) => {
                // An Or pattern is useful if any of its constituent patterns are useful.
                subpatterns.iter().any(|p| p.is_useful_after(previous, db))
            }
        }
    }

    /// Check if this pattern is irrefutable (always matches)
    pub fn is_irrefutable(&self) -> bool {
        match self {
            SimplifiedPattern::Wildcard { .. } => true,
            SimplifiedPattern::Or(patterns) => {
                // An Or pattern is irrefutable if any of its subpatterns are irrefutable
                // AND those irrefutable subpatterns cover all possibilities among themselves.
                // This is complex. A simpler, common definition is if *any* subpattern is irrefutable.
                // However, for correctness in `is_subpattern_vector_useful`, if an Or pattern `q_head`
                // is irrefutable, it means it covers all values of its type.
                // E.g. `Some(_) | None` is irrefutable for Option. `true | false` for bool.
                // For now, let's use: an Or pattern is irrefutable if any of its branches is.
                // This might not be fully correct for all `is_irrefutable` uses but sufficient for `is_subpattern_vector_useful`'s branching.
                // If one branch `p_i` of an Or pattern `p1 | ... | pn` is irrefutable,
                // then the Or pattern itself is irrefutable.
                patterns.iter().any(|p| p.is_irrefutable())
            }
            SimplifiedPattern::Constructor {
                constructor,
                subpatterns,
                ty: _,
            } => {
                // A constructor pattern is irrefutable if it's the *only* constructor for its type
                // (e.g., a tuple, or a struct, or an enum with a single variant) AND all its subpatterns are irrefutable.
                // This requires DB access to check type information (e.g. enum variants).
                // Simplified: Tuples are irrefutable if their subpatterns are. Records/Structs too.
                // Single-variant enums too. Multi-variant enums are not.
                // This is a placeholder. True irrefutability check needs more type info.
                // For the purpose of the algorithm, if it's a tuple, it's irrefutable if subpatterns are.
                match constructor {
                    Constructor::Tuple(_) => subpatterns.iter().all(|sp| sp.is_irrefutable()),
                    // Other constructors (Record, Bool, Int) are typically not irrefutable unless they are the sole variant of an enum
                    // or the only possible value of a type, which is hard to determine without full type info here.
                    // For the algorithm, we mostly care about `_` and tuples.
                    _ => false,
                }
            }
        }
    }
}

/// Helper function for `SimplifiedPattern::is_useful_after` to handle constructor subpatterns.
/// Implements the `U( (q1,...,qk), P )` logic from the paper.
/// `subpatterns` is `(q1,...,qk)`
/// `matrix` is `P_c` (matrix P specialized by the constructor c)
fn is_subpattern_vector_useful<'db>(
    subpatterns: &[SimplifiedPattern<'db>],
    matrix: &PatternMatrix<'db>,
    db: &'db dyn HirAnalysisDb,
) -> bool {
    if subpatterns.is_empty() {
        // No more subpatterns in the candidate pattern `q`.
        // If `matrix` (P_c) is also empty, it means `q = c()` was useful.
        // If `matrix` is not empty, it means `q = c()` was covered by a `c()` in P.
        // This case is typically handled by the caller:
        // `if subpatterns.is_empty() { return false; } // Executed if specialized (matrix) was NOT empty.`
        // So, if we reach here, it means the arity of `c` was 0, and `matrix` (P_c) must have been empty
        // for `is_useful_after` to not return `true` via `specialized.is_empty()`.
        // Thus, if `subpatterns` is empty, it means `c()` was useful if `P_c` was empty.
        // If `P_c` was not empty, the caller returns `false`.
        // This function should only be called if `subpatterns` is not empty initially by `is_useful_after`.
        // If recursion leads to empty subpatterns, it means all prior subpatterns were irrefutable and covered.
        return false; // All subpatterns processed and found to be covered by irrefutability.
    }

    let q_head = &subpatterns[0];
    let q_tail = &subpatterns[1..];

    // 1. Check if q_head is useful w.r.t. the first column of `matrix`.
    let first_column_of_matrix: Vec<SimplifiedPattern<'db>> = matrix
        .rows
        .iter()
        .filter_map(|row| row.patterns.first().cloned())
        .collect();

    if q_head.is_useful_after(&first_column_of_matrix, db) {
        return true;
    }

    // 2. If not, and q_head is irrefutable, then compute D(q_head, matrix) and recurse.
    if q_head.is_irrefutable() {
        let mut next_matrix_rows: Vec<PatternRowWithMetadata<'db>> = Vec::new();
        for prev_row in &matrix.rows {
            if prev_row.patterns.is_empty() {
                continue;
            } // Should not happen

            let p_head = &prev_row.patterns[0];
            let p_tail = &prev_row.patterns[1..];

            match q_head {
                SimplifiedPattern::Wildcard { .. } => {
                    // D(_, prev_row) = p_tail
                    next_matrix_rows.push(PatternRowWithMetadata {
                        patterns: p_tail.to_vec(),
                        arm_index: prev_row.arm_index,
                    });
                }
                SimplifiedPattern::Constructor {
                    constructor: c_q,
                    subpatterns: _s_q_sub,
                    ..
                } => {
                    match p_head {
                        SimplifiedPattern::Constructor {
                            constructor: c_p,
                            subpatterns: s_p_sub,
                            ..
                        } if c_q.is_same_variant(c_p, db) || c_q == c_p => {
                            // D(C(qs), row C(ps)|pt) = ps|pt
                            let mut new_patterns = s_p_sub.clone();
                            new_patterns.extend_from_slice(p_tail);
                            next_matrix_rows.push(PatternRowWithMetadata {
                                patterns: new_patterns,
                                arm_index: prev_row.arm_index,
                            });
                        }
                        SimplifiedPattern::Wildcard { .. } => {
                            // D(C(qs), row _|pt)
                            // If p_head is wildcard, it is specialized by c_q.
                            // The "fields" of this specialized wildcard are then taken.
                            // These are effectively wildcards for each field of c_q.
                            let field_count = c_q.field_count(db);
                            let mut new_head_patterns = Vec::with_capacity(field_count);
                            for _ in 0..field_count {
                                new_head_patterns
                                    .push(SimplifiedPattern::Wildcard { binding: None });
                            }

                            let mut new_patterns = new_head_patterns;
                            new_patterns.extend_from_slice(p_tail);
                            next_matrix_rows.push(PatternRowWithMetadata {
                                patterns: new_patterns,
                                arm_index: prev_row.arm_index,
                            });
                        }
                        SimplifiedPattern::Or(p_head_ors) => {
                            // If p_head is Or, and q_head (irrefutable constructor) covers it,
                            // then p_head must effectively be irrefutable too (e.g. contain wildcard or all variants).
                            // We need to see which branch of p_head_ors is compatible.
                            // This part is complex because one row in `matrix` might lead to multiple effective rows for `next_matrix`.
                            // For now, if *any* branch of p_head_ors is compatible as a wildcard or matching constructor,
                            // we form the next row based on that. This simplification might not be fully correct if multiple branches are compatible.
                            // A more robust approach might involve expanding Or patterns in the matrix rows earlier.
                            let mut _row_added = false;
                            for p_or_alt in p_head_ors {
                                if matches!(p_or_alt, SimplifiedPattern::Wildcard { .. }) {
                                    let field_count = c_q.field_count(db);
                                    let mut new_head_patterns = Vec::with_capacity(field_count);
                                    for _ in 0..field_count {
                                        new_head_patterns
                                            .push(SimplifiedPattern::Wildcard { binding: None });
                                    }
                                    let mut new_patterns = new_head_patterns;
                                    new_patterns.extend_from_slice(p_tail);
                                    next_matrix_rows.push(PatternRowWithMetadata {
                                        patterns: new_patterns,
                                        arm_index: prev_row.arm_index,
                                    });
                                    _row_added = true;
                                    break; // Assume wildcard branch is dominant for deriving next matrix row
                                } else if let SimplifiedPattern::Constructor {
                                    constructor: c_p_alt,
                                    subpatterns: s_p_alt_sub,
                                    ..
                                } = p_or_alt
                                {
                                    if c_q.is_same_variant(c_p_alt, db) || c_q == c_p_alt {
                                        let mut new_patterns = s_p_alt_sub.clone();
                                        new_patterns.extend_from_slice(p_tail);
                                        next_matrix_rows.push(PatternRowWithMetadata {
                                            patterns: new_patterns,
                                            arm_index: prev_row.arm_index,
                                        });
                                        _row_added = true;
                                        // If multiple Or branches match, this could add multiple rows to next_matrix_rows for prev_row.
                                        // The paper's D operator is defined on a matrix P, not row-by-row with expansion.
                                    }
                                }
                            }
                            // If no compatible branch found, this row from `matrix` is incompatible.
                        }
                        _ => { /* p_head is a non-matching constructor; row is incompatible, skip */
                        }
                    }
                }
                SimplifiedPattern::Or(_q_head_ors) => {
                    // q_head should not be an Or pattern if `is_irrefutable` is true for it,
                    // unless the Or itself is irrefutable (e.g. `true | false`).
                    // The usefulness check for Or(q_subs) in `is_useful_after` already iterates `q_subs`.
                    // So `q_head` reaching here as Or and irrefutable implies a complex scenario.
                    // For now, assume this case is handled by `is_useful_after` for `Or`.
                    return false;
                }
            }
        }
        let next_matrix = PatternMatrix {
            rows: next_matrix_rows,
        };
        return is_subpattern_vector_useful(q_tail, &next_matrix, db);
    }

    // 3. q_head is refutable but not useful against the first column.
    // This means q_head's constructor might still be useful in combination with its subpatterns.
    // Specialize the matrix by q_head's constructor and check the full subpattern vector.
    if let SimplifiedPattern::Constructor {
        constructor: q_constructor,
        ..
    } = q_head
    {
        let specialized_matrix = matrix.phi_specialize(q_constructor, db);
        return is_subpattern_vector_useful(subpatterns, &specialized_matrix, db);
    }

    // q_head is neither useful, irrefutable, nor a constructor
    false
}

/// Pattern matrix for exhaustiveness and reachability analysis
///
/// The pattern matrix is the core data structure of the pattern matching algorithm.
/// It represents a 2D matrix where:
/// - Each row corresponds to one match arm in the source code
/// - Each column corresponds to one "position" in the pattern (e.g., tuple elements)
///
/// The algorithm works by systematically "specializing" this matrix for different
/// constructors, reducing the problem size until a definitive answer is reached.
///
/// Example for `match (x, y) { (0, _) => ..., (_, 1) => ..., (2, 2) => ... }`:
/// ```text
/// Row 0: [Int(0), Wildcard]
/// Row 1: [Wildcard, Int(1)]
/// Row 2: [Int(2), Int(2)]
/// ```
pub struct PatternMatrix<'db> {
    /// The rows of the pattern matrix, each representing one match arm
    rows: Vec<PatternRowWithMetadata<'db>>,
}

/// A single row in the pattern matrix with metadata
///
/// Each row represents one match arm from the original source code, along with
/// metadata needed for error reporting and analysis.
#[derive(Clone)]
pub struct PatternRowWithMetadata<'db> {
    /// The patterns in this row (one pattern per "column" in the matrix)
    patterns: Vec<SimplifiedPattern<'db>>,
    /// The index of the match arm this row corresponds to in the original source
    arm_index: MatchArmIndex,
}

impl<'db> PatternMatrix<'db> {
    /// Creates a new pattern matrix from a list of patterns
    ///
    /// This is typically called with the patterns from all match arms in a single
    /// match expression. Each pattern becomes one row in the matrix.
    ///
    /// # Arguments
    /// * `patterns` - List of simplified patterns, one per match arm
    ///
    /// # Returns
    /// A new pattern matrix ready for analysis
    pub fn new(patterns: Vec<SimplifiedPattern<'db>>) -> Self {
        let rows = patterns
            .into_iter()
            .enumerate()
            .map(|(i, pat)| PatternRowWithMetadata {
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

    /// Check if a specific row is useful (reachable/not shadowed by previous rows)
    ///
    /// A row is "useful" if there exists at least one input value that:
    /// 1. Matches the pattern in this row
    /// 2. Does NOT match any of the patterns in previous rows
    ///
    /// This is the core operation for reachability analysis.
    ///
    /// # Arguments
    /// * `row_index` - Index of the row to check (0-based)
    /// * `db` - Database for type information
    ///
    /// # Returns
    /// `true` if the row is useful (reachable), `false` if it's unreachable
    pub fn is_row_useful(&self, row_index: usize, db: &'db dyn HirAnalysisDb) -> bool {
        if row_index == 0 {
            return true; // First row is always useful (nothing can shadow it)
        }

        // Create a matrix containing only the previous rows
        let previous_matrix = PatternMatrix {
            rows: self.rows[0..row_index].to_vec(),
        };

        // Check if the current row is useful against all previous rows
        // This implements: U(P_previous, q_current) where P is previous patterns, q is current pattern
        Self::is_useful_recursive(&previous_matrix, &self.rows[row_index].patterns, db)
    }

    /// Core recursive usefulness algorithm
    ///
    /// This implements the heart of the pattern matching algorithm from Maranget's paper.
    /// The algorithm answers: "Is pattern Q useful with respect to matrix P?"
    ///
    /// # Algorithm Overview
    ///
    /// The usefulness check U(P, Q) works as follows:
    /// 1. **Base cases:**
    ///    - If P is empty, then Q is useful (no previous patterns to shadow it)
    ///    - If Q is empty, then Q is not useful (empty pattern can't match anything new)
    ///
    /// 2. **Recursive cases:** Based on the first pattern in Q:
    ///    - **Wildcard:** Use δ-specialization (handle all constructors)
    ///    - **Constructor:** Use φ-specialization (specialize for this constructor)
    ///    - **OR pattern:** Check if any alternative is useful
    ///
    /// # Arguments
    /// * `p_prev` - Matrix P of previous patterns (what might shadow Q)
    /// * `q_current_row_patterns` - Pattern row Q to check for usefulness
    /// * `db` - Database for type and constructor information
    ///
    /// # Returns
    /// `true` if Q is useful (can match values not matched by P), `false` otherwise
    fn is_useful_recursive(
        p_prev: &PatternMatrix<'db>, // Matrix P (previous patterns that might shadow)
        q_current_row_patterns: &[SimplifiedPattern<'db>], // Pattern row Q (candidate pattern)
        db: &'db dyn HirAnalysisDb,
    ) -> bool {
        // Base case 1: No previous patterns means Q is definitely useful
        if p_prev.is_empty() {
            return true; // Rule 1: U(∅, Q) = true
        }

        // Base case 2: Empty pattern row cannot be useful against non-empty matrix
        // This happens when we've specialized down to 0-arity constructors
        if q_current_row_patterns.is_empty() {
            return false; // Rule 2: U(P, ∅) = false when P ≠ ∅
        }

        // Recursive case: examine the first pattern in Q
        let q_head = &q_current_row_patterns[0];
        let q_tail = &q_current_row_patterns[1..];

        match q_head {
            // Case 1: Q starts with a wildcard → use δ-specialization
            SimplifiedPattern::Wildcard { .. } => {
                // Rule Wildcard: U( (_ | Q_t), P ) = U( Q_t, D(P) )
                let p_next = p_prev.d_specialize(); // D(P)
                                                    // Q_t is just q_tail for wildcard head
                Self::is_useful_recursive(&p_next, q_tail, db)
            }
            SimplifiedPattern::Constructor {
                constructor: c_q,
                subpatterns: s_q,
                ..
            } => {
                // Rule Constructor: U( (c(s_q) | Q_t), P ) = U( (s_q | Q_t), S(c,P) )
                let p_next = p_prev.phi_specialize(c_q, db); // S(c,P)

                let mut q_next_patterns = s_q.clone();
                q_next_patterns.extend_from_slice(q_tail);

                Self::is_useful_recursive(&p_next, &q_next_patterns, db)
            }
            SimplifiedPattern::Or(q_subs) => {
                // Rule Or: U( (q_a | q_b | Q_t), P ) = U( (q_a | Q_t), P ) || U( (q_b | Q_t), P )
                // The reference implementation's `is_pattern_useful` for Or is:
                // `pats.iter().any(|pat_prime| self.is_pattern_useful(db, &PatternRowVec::new(vec![pat_prime.clone()])))`
                // This seems to check if any q_sub alone is useful against P.
                // This is equivalent to: U(q_a, P) || U(q_b, P) if Q_t is empty.
                // If Q_t is not empty, it should be: U([q_sub | q_tail], P_prev)
                q_subs.iter().any(|q_sub| {
                    let mut next_q_row_patterns = vec![q_sub.clone()];
                    next_q_row_patterns.extend_from_slice(q_tail);
                    Self::is_useful_recursive(p_prev, &next_q_row_patterns, db)
                })
            }
        }
    }

    /// Specializes the matrix for a constructor (φ-specialization)
    ///
    /// This is the φ (phi) operation from Maranget's algorithm. It transforms the pattern
    /// matrix by keeping only rows that can match the given constructor and "unpeeling"
    /// one layer of pattern structure.
    ///
    /// # How φ-specialization works:
    ///
    /// For a constructor C and matrix P, φ(C, P) produces a new matrix where:
    /// 1. Rows starting with constructor C get their sub-patterns extracted
    /// 2. Rows starting with wildcards get expanded with C's arity
    /// 3. Rows starting with incompatible constructors are removed
    /// 4. OR patterns are expanded to check each alternative
    ///
    /// # Example:
    ///
    /// Original matrix for `match (x, y)`:
    /// ```text
    /// Row 0: [Tuple(2) [Int(0), Wildcard], ...]  // (0, _)
    /// Row 1: [Wildcard, ...]                     // _
    /// Row 2: [Tuple(2) [Int(1), Int(2)], ...]    // (1, 2)
    /// ```
    ///
    /// After φ(Tuple(2), P):
    /// ```text
    /// Row 0: [Int(0), Wildcard, ...]             // From (0, _) → 0, _
    /// Row 1: [Wildcard, Wildcard, ...]           // From _ → _, _
    /// Row 2: [Int(1), Int(2), ...]               // From (1, 2) → 1, 2
    /// ```
    ///
    /// # Arguments
    /// * `ctor_to_specialize_by` - The constructor to specialize for
    /// * `db` - Database for type information
    ///
    /// # Returns
    /// A new specialized matrix
    pub fn phi_specialize(
        &self,
        ctor_to_specialize_by: &Constructor<'db>,
        db: &'db dyn HirAnalysisDb,
    ) -> Self {
        let mut result_rows = Vec::new();

        // Process each row, keeping only those that start with wildcards or OR patterns

        // Process each row in the matrix
        for row in &self.rows {
            if row.patterns.is_empty() {
                continue;
            }
            let first_pat = &row.patterns[0];
            let remaining_pats = &row.patterns[1..];

            match first_pat {
                SimplifiedPattern::Wildcard { .. } => {
                    let field_count = ctor_to_specialize_by.field_count(db);
                    let mut new_head_patterns = Vec::with_capacity(field_count);
                    for _ in 0..field_count {
                        new_head_patterns.push(SimplifiedPattern::Wildcard { binding: None });
                    }
                    let mut final_patterns = new_head_patterns;
                    final_patterns.extend_from_slice(remaining_pats);
                    result_rows.push(PatternRowWithMetadata {
                        patterns: final_patterns,
                        arm_index: row.arm_index,
                    });
                }
                SimplifiedPattern::Constructor {
                    constructor: c_pat,
                    subpatterns: s_pat,
                    ..
                } => {
                    if c_pat == ctor_to_specialize_by
                        || c_pat.is_same_variant(ctor_to_specialize_by, db)
                    {
                        let mut final_patterns = s_pat.clone();
                        final_patterns.extend_from_slice(remaining_pats);
                        result_rows.push(PatternRowWithMetadata {
                            patterns: final_patterns,
                            arm_index: row.arm_index,
                        });
                    }
                    // If constructors don't match, this row does not contribute to the specialized matrix.
                }
                SimplifiedPattern::Or(or_subpatterns) => {
                    for or_pat_component in or_subpatterns {
                        // Treat each component of the Or as if it were the first_pat itself
                        // This means an Or pattern in one row can lead to multiple rows in the specialized matrix
                        match or_pat_component {
                            SimplifiedPattern::Wildcard { .. } => {
                                let field_count = ctor_to_specialize_by.field_count(db);
                                let mut new_head_patterns = Vec::with_capacity(field_count);
                                for _ in 0..field_count {
                                    new_head_patterns
                                        .push(SimplifiedPattern::Wildcard { binding: None });
                                }
                                let mut final_patterns = new_head_patterns;
                                final_patterns.extend_from_slice(remaining_pats);
                                result_rows.push(PatternRowWithMetadata {
                                    patterns: final_patterns,
                                    arm_index: row.arm_index, // Same arm_index because it's from the same original row
                                });
                            }
                            SimplifiedPattern::Constructor {
                                constructor: c_or_comp,
                                subpatterns: s_or_comp,
                                ..
                            } => {
                                if c_or_comp == ctor_to_specialize_by
                                    || c_or_comp.is_same_variant(ctor_to_specialize_by, db)
                                {
                                    let mut final_patterns = s_or_comp.clone();
                                    final_patterns.extend_from_slice(remaining_pats);
                                    result_rows.push(PatternRowWithMetadata {
                                        patterns: final_patterns,
                                        arm_index: row.arm_index,
                                    });
                                }
                            }
                            SimplifiedPattern::Or(_) => {
                                // Or patterns nested within Or patterns are typically expanded/flattened earlier
                                // or represent an invalid state. For robustness, treat as non-matching or log error.
                                // Here, we'll skip, as it shouldn't contribute if not a wildcard or matching constructor.
                            }
                        }
                    }
                }
            }
        }
        PatternMatrix { rows: result_rows }
    }

    /// Specializes the matrix for the default case (wildcard matching)
    pub fn d_specialize(&self) -> Self {
        let mut result_rows = Vec::new();
        for row in &self.rows {
            if row.patterns.is_empty() {
                continue;
            }
            let first_pat = &row.patterns[0];
            let remaining_pats = &row.patterns[1..];

            match first_pat {
                SimplifiedPattern::Wildcard { .. } => {
                    result_rows.push(PatternRowWithMetadata {
                        patterns: remaining_pats.to_vec(),
                        arm_index: row.arm_index,
                    });
                }
                SimplifiedPattern::Or(or_subpatterns) => {
                    // If any component of the Or is a wildcard, the whole Or pattern matches the default case
                    if or_subpatterns
                        .iter()
                        .any(|p| matches!(p, SimplifiedPattern::Wildcard { .. }))
                    {
                        result_rows.push(PatternRowWithMetadata {
                            patterns: remaining_pats.to_vec(),
                            arm_index: row.arm_index,
                        });
                    }
                }
                SimplifiedPattern::Constructor { .. } => {
                    // Constructors do not match the default case, so this row is dropped.
                }
            }
        }
        PatternMatrix { rows: result_rows }
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

            // Check for tuple patterns with all wildcard subpatterns (e.g., (_, _))
            if let Some(SimplifiedPattern::Constructor { constructor, subpatterns, .. }) = row.patterns.first() {
                let pattern_arity = match constructor {
                    Constructor::Tuple(n) => Some(*n),
                    Constructor::TupleLike(tuple_like) => Some(tuple_like.arity(db)),
                    _ => None,
                };
                if let Some(arity) = pattern_arity {
                    if arity == tuple_elems.len() && 
                       subpatterns.len() == arity &&
                       subpatterns.iter().all(|p| matches!(p, SimplifiedPattern::Wildcard { .. })) {
                        return Vec::new(); // All patterns covered by tuple wildcard pattern
                    }
                }
            }

            // Also check Or patterns that might contain wildcards
            if let Some(SimplifiedPattern::Or(patterns)) = row.patterns.first() {
                if patterns
                    .iter()
                    .any(|p| matches!(p, SimplifiedPattern::Wildcard { .. }))
                {
                    return Vec::new(); // All patterns covered by a wildcard
                }
                
                // Check Or patterns for tuple wildcards too
                if patterns.iter().any(|p| {
                    if let SimplifiedPattern::Constructor { constructor, subpatterns, .. } = p {
                        let pattern_arity = match constructor {
                            Constructor::Tuple(n) => Some(*n),
                            Constructor::TupleLike(tuple_like) => Some(tuple_like.arity(db)),
                            _ => None,
                        };
                        if let Some(arity) = pattern_arity {
                            arity == tuple_elems.len() && 
                            subpatterns.len() == arity &&
                            subpatterns.iter().all(|sp| matches!(sp, SimplifiedPattern::Wildcard { .. }))
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }) {
                    return Vec::new(); // All patterns covered by tuple wildcard in Or pattern
                }
            }
        }

        // Check if we have any tuple constructors with the right arity
        let mut has_tuple_patterns = false;
        for row in &self.rows {
            if let Some(SimplifiedPattern::Constructor {
                constructor,
                ..
            }) = row.patterns.first()
            {
                let arity = match constructor {
                    Constructor::Tuple(n) => Some(*n),
                    Constructor::TupleLike(tuple_like) => Some(tuple_like.arity(db)),
                    _ => None,
                };
                if let Some(n) = arity {
                    if n == tuple_elems.len() {
                        has_tuple_patterns = true;
                        break;
                    }
                }
            } else if let Some(SimplifiedPattern::Or(patterns)) = row.patterns.first() {
                if patterns.iter().any(|p| {
                    if let SimplifiedPattern::Constructor {
                        constructor,
                        ..
                    } = p
                    {
                        let arity = match constructor {
                            Constructor::Tuple(n) => Some(*n),
                            Constructor::TupleLike(tuple_like) => Some(tuple_like.arity(db)),
                            _ => None,
                        };
                        arity == Some(tuple_elems.len())
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
            let specialized = self.specialize_tuple_element(i, tuple_elems.len(), db);
            if specialized.rows.is_empty() {
                // This element is completely uncovered
                let subpatterns =
                    vec![SimplifiedPattern::Wildcard { binding: None }; tuple_elems.len()];

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
                        let mut subpatterns =
                            vec![SimplifiedPattern::Wildcard { binding: None }; tuple_elems.len()];
                        subpatterns[i] = missing_elem;

                        missing_patterns.push(SimplifiedPattern::Constructor {
                            constructor: Constructor::TupleLike(TupleLike::Type(ty)),
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
    fn specialize_tuple_element(&self, element_idx: usize, arity: usize, db: &'db dyn HirAnalysisDb) -> PatternMatrix<'db> {
        let mut specialized_rows = Vec::new();

        for row in &self.rows {
            if let Some(SimplifiedPattern::Constructor {
                constructor,
                subpatterns,
                ..
            }) = row.patterns.first()
            {
                let pattern_arity = match constructor {
                    Constructor::Tuple(n) => Some(*n),
                    Constructor::TupleLike(tuple_like) => Some(tuple_like.arity(db)),
                    _ => None,
                };
                if pattern_arity == Some(arity) && element_idx < subpatterns.len() {
                    // Take the pattern at the specified element index
                    let element_pattern = &subpatterns[element_idx];

                    // Create a new row with just this element's pattern
                    let mut new_patterns = vec![element_pattern.clone()];
                    new_patterns.extend_from_slice(&row.patterns[1..]);

                    specialized_rows.push(PatternRowWithMetadata {
                        patterns: new_patterns,
                        arm_index: row.arm_index,
                    });
                }
            } else if let Some(SimplifiedPattern::Wildcard { .. }) = row.patterns.first() {
                // For wildcard patterns, add a wildcard for this element
                let mut new_patterns = vec![SimplifiedPattern::Wildcard { binding: None }];
                new_patterns.extend_from_slice(&row.patterns[1..]);

                specialized_rows.push(PatternRowWithMetadata {
                    patterns: new_patterns,
                    arm_index: row.arm_index,
                });
            } else if let Some(SimplifiedPattern::Or(patterns)) = row.patterns.first() {
                // Handle Or patterns by expanding each subpattern
                for pattern in patterns {
                    if let SimplifiedPattern::Constructor {
                        constructor,
                        subpatterns,
                        ..
                    } = pattern
                    {
                        let pattern_arity = match constructor {
                            Constructor::Tuple(n) => Some(*n),
                            Constructor::TupleLike(tuple_like) => Some(tuple_like.arity(db)),
                            _ => None,
                        };
                        if pattern_arity == Some(arity) && element_idx < subpatterns.len() {
                            let element_pattern = &subpatterns[element_idx];
                            let mut new_patterns = vec![element_pattern.clone()];
                            new_patterns.extend_from_slice(&row.patterns[1..]);

                            specialized_rows.push(PatternRowWithMetadata {
                                patterns: new_patterns,
                                arm_index: row.arm_index,
                            });
                        }
                    } else if let SimplifiedPattern::Wildcard { .. } = pattern {
                        let mut new_patterns = vec![SimplifiedPattern::Wildcard { binding: None }];
                        new_patterns.extend_from_slice(&row.patterns[1..]);

                        specialized_rows.push(PatternRowWithMetadata {
                            patterns: new_patterns,
                            arm_index: row.arm_index,
                        });
                    }
                }
            }
        }

        PatternMatrix {
            rows: specialized_rows,
        }
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
                        SimplifiedPattern::Constructor {
                            constructor: Constructor::TupleLike(tuple_like),
                            ..
                        } => {
                            // Compare the variant with TupleLike variant
                            if let TupleLike::Variant(resolved_variant) = tuple_like {
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
                                } else if let SimplifiedPattern::Constructor {
                                    constructor: Constructor::TupleLike(tuple_like),
                                    ..
                                } = or_pat
                                {
                                    // Compare the variant with TupleLike variant
                                    if let TupleLike::Variant(resolved_variant) = tuple_like {
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
            let constructor = match variant_def.kind {
                hir::hir_def::VariantKind::Tuple(_) => {
                    // Use TupleLike for tuple variants
                    Constructor::TupleLike(TupleLike::Variant(variant.clone()))
                }
                _ => {
                    // Use RecordLike for record and unit variants
                    let record = RecordLike::from_variant(variant.clone());
                    Constructor::Record(record)
                }
            };

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
