//! Decision tree builder implementation

use rustc_hash::FxHashMap;

use super::types::{Case, DecisionTree, LeafNode, Occurrence, SwitchNode};
use crate::{
    ty::pattern_analysis::{Constructor, PatternMatrix, PatternRowWithMetadata, SimplifiedPattern},
    HirAnalysisDb,
};

/// Main entry point for building decision trees from pattern matrices
///
/// This function takes a pattern matrix (from exhaustiveness/reachability analysis)
/// and compiles it into an optimized decision tree for efficient execution.
///
/// # Arguments
/// * `db` - Database for type information and constructor details
/// * `matrix` - The pattern matrix to compile
/// * `policy` - Column selection policy for optimization
///
/// # Returns
/// An optimized decision tree ready for code generation
pub fn build_decision_tree<'db>(
    db: &'db dyn HirAnalysisDb,
    matrix: &PatternMatrix<'db>,
    policy: ColumnSelectionPolicy,
) -> DecisionTree<'db> {
    let builder = DecisionTreeBuilder::new(policy);
    builder.build(db, matrix.clone(), Occurrence::new())
}

/// Column selection policy for decision tree optimization
///
/// The order in which we examine pattern columns significantly affects the efficiency
/// of the resulting decision tree. This struct implements various heuristics from
/// the literature to choose good column orderings.
#[derive(Debug, Clone, Default)]
pub struct ColumnSelectionPolicy {
    heuristics: Vec<ColumnHeuristic>,
}

impl ColumnSelectionPolicy {
    /// Creates a new empty policy
    pub fn new() -> Self {
        Self::default()
    }

    /// Add the arity heuristic
    ///
    /// Prefers columns where constructors have smaller arities (fewer sub-patterns).
    /// This tends to create more balanced trees.
    pub fn arity(mut self) -> Self {
        self.heuristics.push(ColumnHeuristic::Arity);
        self
    }

    /// Add the small branching heuristic
    ///
    /// Prefers columns with fewer distinct constructors.
    /// This reduces the branching factor of switch nodes.
    pub fn small_branching(mut self) -> Self {
        self.heuristics.push(ColumnHeuristic::SmallBranching);
        self
    }

    /// Add the needed prefix heuristic
    ///
    /// Prefers columns that are "needed" for more rows from the beginning.
    /// This tends to handle common cases early.
    pub fn needed_prefix(mut self) -> Self {
        self.heuristics.push(ColumnHeuristic::NeededPrefix);
        self
    }

    fn select_column<'db>(&self, db: &'db dyn HirAnalysisDb, matrix: &WorkingMatrix<'db>) -> usize {
        if matrix.ncols() <= 1 {
            return 0;
        }

        let mut candidates: Vec<usize> = (0..matrix.ncols()).collect();

        // Apply each heuristic in order, filtering down candidates
        for heuristic in &self.heuristics {
            if candidates.len() <= 1 {
                break;
            }

            let mut best_score = i32::MIN;
            let mut next_candidates = Vec::new();

            for &col in &candidates {
                let score = heuristic.score(db, matrix, col);
                match score.cmp(&best_score) {
                    std::cmp::Ordering::Greater => {
                        best_score = score;
                        next_candidates = vec![col];
                    }
                    std::cmp::Ordering::Equal => {
                        next_candidates.push(col);
                    }
                    std::cmp::Ordering::Less => {}
                }
            }

            candidates = next_candidates;
        }

        // If multiple candidates remain, pick the rightmost (arbitrary tiebreaker)
        candidates.into_iter().max().unwrap_or(0)
    }
}

#[derive(Debug, Clone, Copy)]
enum ColumnHeuristic {
    Arity,
    SmallBranching,
    NeededPrefix,
}

impl ColumnHeuristic {
    fn score<'db>(
        self,
        db: &'db dyn HirAnalysisDb,
        matrix: &WorkingMatrix<'db>,
        col: usize,
    ) -> i32 {
        match self {
            ColumnHeuristic::Arity => {
                // Score = sum of negative arities of constructors in this column
                let constructors = matrix.constructors_in_column(col);
                constructors
                    .iter()
                    .map(|ctor| -(ctor.field_count(db) as i32))
                    .sum()
            }
            ColumnHeuristic::SmallBranching => {
                // Score = negative number of constructors
                let constructors = matrix.constructors_in_column(col);
                let count = constructors.len() as i32;
                let is_complete = matrix.is_constructor_set_complete(col);
                if is_complete {
                    -count
                } else {
                    -count - 1 // Penalty for incomplete sets (need default case)
                }
            }
            ColumnHeuristic::NeededPrefix => {
                // Score = number of rows from start that "need" this column
                matrix.needed_prefix_score(col)
            }
        }
    }
}

/// Internal builder for constructing decision trees
struct DecisionTreeBuilder {
    policy: ColumnSelectionPolicy,
}

impl DecisionTreeBuilder {
    fn new(policy: ColumnSelectionPolicy) -> Self {
        Self { policy }
    }

    fn build<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        matrix: PatternMatrix<'db>,
        occurrence: Occurrence,
    ) -> DecisionTree<'db> {
        let working_matrix = WorkingMatrix::new(matrix.clone(), vec![occurrence]);

        if working_matrix.is_empty() {
            panic!("Cannot build decision tree from empty matrix");
        }

        self.build_recursive(db, working_matrix)
    }

    fn build_recursive<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        mut matrix: WorkingMatrix<'db>,
    ) -> DecisionTree<'db> {
        // Base case: if first row is all wildcards, we have a leaf
        if matrix.first_row_all_wildcards() {
            let arm_idx = matrix
                .matrix
                .get_arm_index(0)
                .expect("First row should exist");
            let bindings = matrix.extract_bindings_for_first_row();
            return DecisionTree::Leaf(LeafNode::with_bindings(arm_idx, bindings));
        }

        // Choose column to split on
        let col = self.policy.select_column(db, &matrix);
        matrix.move_column_to_front(col);

        let occurrence = matrix.occurrences[0].clone();
        let constructors = matrix.constructors_in_column(0);

        let mut switch_arms = Vec::new();

        // Create branches for each constructor
        for constructor in constructors {
            let specialized_matrix = matrix.specialize_for_constructor(db, &constructor);
            if !specialized_matrix.is_empty() {
                let subtree = self.build_recursive(db, specialized_matrix);
                switch_arms.push((Case::Constructor(constructor), subtree));
            }
        }

        // Add default branch if constructor set is incomplete
        if !matrix.is_constructor_set_complete(0) {
            let default_matrix = matrix.specialize_for_default(db);
            if !default_matrix.is_empty() {
                let subtree = self.build_recursive(db, default_matrix);
                switch_arms.push((Case::Default, subtree));
            }
        }

        DecisionTree::Switch(SwitchNode::with_arms(occurrence, switch_arms))
    }
}

/// Working matrix that includes occurrence tracking for decision tree building
///
/// This extends the basic PatternMatrix with occurrence information needed
/// for generating the decision tree.
#[derive(Debug, Clone)]
struct WorkingMatrix<'db> {
    /// The underlying pattern matrix
    matrix: PatternMatrix<'db>,
    /// Occurrences for each column (tracks where to find values)
    occurrences: Vec<Occurrence>,
    /// Variable bindings accumulated for each row
    bindings: Vec<FxHashMap<(String, usize), Occurrence>>,
}

impl<'db> WorkingMatrix<'db> {
    fn new(matrix: PatternMatrix<'db>, occurrences: Vec<Occurrence>) -> Self {
        // Initialize empty bindings for each row
        let bindings = vec![FxHashMap::default(); matrix.row_count()];

        Self {
            matrix,
            occurrences,
            bindings,
        }
    }

    fn is_empty(&self) -> bool {
        self.matrix.is_empty()
    }

    fn ncols(&self) -> usize {
        self.occurrences.len()
    }

    fn first_row_all_wildcards(&self) -> bool {
        self.matrix.first_row_all_wildcards()
    }

    fn constructors_in_column(&self, col: usize) -> Vec<Constructor<'db>> {
        let mut constructors = Vec::new();

        for row_idx in 0..self.matrix.row_count() {
            if let Some(pattern) = self.matrix.get_pattern(row_idx, col) {
                match pattern {
                    SimplifiedPattern::Constructor { constructor, .. } => {
                        if !constructors.iter().any(|c| c == constructor) {
                            constructors.push(constructor.clone());
                        }
                    }
                    SimplifiedPattern::Or(or_patterns) => {
                        for or_pat in or_patterns {
                            if let SimplifiedPattern::Constructor { constructor, .. } = or_pat {
                                if !constructors.iter().any(|c| c == constructor) {
                                    constructors.push(constructor.clone());
                                }
                            }
                        }
                    }
                    SimplifiedPattern::Wildcard { .. } => {
                        // Wildcards don't contribute constructors
                    }
                }
            }
        }

        constructors
    }

    fn is_constructor_set_complete(&self, col: usize) -> bool {
        // This is a simplified check - in a full implementation, you'd need
        // to get the complete set of constructors for the type and compare
        let constructors = self.constructors_in_column(col);

        if constructors.is_empty() {
            return false;
        }

        // For now, assume it's complete if we have constructors
        // TODO: Implement proper completeness checking based on type
        true
    }

    fn needed_prefix_score(&self, col: usize) -> i32 {
        let mut score = 0;
        for row_idx in 0..self.matrix.row_count() {
            if let Some(pattern) = self.matrix.get_pattern(row_idx, col) {
                if !matches!(pattern, SimplifiedPattern::Wildcard { .. }) {
                    score += 1;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        score
    }

    fn move_column_to_front(&mut self, col: usize) {
        if col == 0 {
            return;
        }

        // Swap occurrences
        self.occurrences.swap(0, col);

        // Create new matrix with swapped columns
        let mut new_rows = Vec::new();
        for row in self.matrix.rows() {
            let mut new_patterns = row.patterns.clone();
            if col < new_patterns.len() {
                new_patterns.swap(0, col);
            }
            new_rows.push(PatternRowWithMetadata {
                patterns: new_patterns,
                arm_index: row.arm_index,
            });
        }
        self.matrix = PatternMatrix::from_rows(new_rows);
    }

    fn specialize_for_constructor(
        &self,
        db: &'db dyn HirAnalysisDb,
        constructor: &Constructor<'db>,
    ) -> Self {
        let arity = constructor.field_count(db);
        let mut new_occurrences = self.occurrences[0].specialize_for_arity(arity);
        new_occurrences.extend_from_slice(&self.occurrences[1..]);

        let mut new_matrix_rows = Vec::new();
        let mut new_bindings = Vec::new();

        for (row_idx, row) in self.matrix.rows().iter().enumerate() {
            if row.patterns.is_empty() {
                continue;
            }

            let first_pat = &row.patterns[0];
            let rest_patterns = &row.patterns[1..];

            match first_pat {
                SimplifiedPattern::Wildcard { binding } => {
                    // Wildcard matches any constructor
                    let mut new_patterns = Vec::new();
                    for _ in 0..arity {
                        new_patterns.push(SimplifiedPattern::Wildcard { binding: None });
                    }
                    new_patterns.extend_from_slice(rest_patterns);

                    let mut row_bindings = self.bindings[row_idx].clone();
                    if let Some(name) = binding {
                        row_bindings.insert(
                            (name.to_string(), row.arm_index),
                            self.occurrences[0].clone(),
                        );
                    }

                    new_matrix_rows.push(PatternRowWithMetadata {
                        patterns: new_patterns,
                        arm_index: row.arm_index,
                    });
                    new_bindings.push(row_bindings);
                }
                SimplifiedPattern::Constructor {
                    constructor: pat_ctor,
                    subpatterns,
                    ..
                } => {
                    // Only include if constructors match
                    if pat_ctor == constructor || pat_ctor.is_same_variant(constructor, db) {
                        let mut new_patterns = subpatterns.clone();
                        new_patterns.extend_from_slice(rest_patterns);

                        new_matrix_rows.push(PatternRowWithMetadata {
                            patterns: new_patterns,
                            arm_index: row.arm_index,
                        });
                        new_bindings.push(self.bindings[row_idx].clone());
                    }
                }
                SimplifiedPattern::Or(or_patterns) => {
                    // Handle each alternative in the OR pattern
                    for or_pat in or_patterns {
                        match or_pat {
                            SimplifiedPattern::Wildcard { binding } => {
                                let mut new_patterns = Vec::new();
                                for _ in 0..arity {
                                    new_patterns
                                        .push(SimplifiedPattern::Wildcard { binding: None });
                                }
                                new_patterns.extend_from_slice(rest_patterns);

                                let mut row_bindings = self.bindings[row_idx].clone();
                                if let Some(name) = binding {
                                    row_bindings.insert(
                                        (name.to_string(), row.arm_index),
                                        self.occurrences[0].clone(),
                                    );
                                }

                                new_matrix_rows.push(PatternRowWithMetadata {
                                    patterns: new_patterns,
                                    arm_index: row.arm_index,
                                });
                                new_bindings.push(row_bindings);
                            }
                            SimplifiedPattern::Constructor {
                                constructor: or_ctor,
                                subpatterns: or_subpatterns,
                                ..
                            } => {
                                if or_ctor == constructor
                                    || or_ctor.is_same_variant(constructor, db)
                                {
                                    let mut new_patterns = or_subpatterns.clone();
                                    new_patterns.extend_from_slice(rest_patterns);

                                    new_matrix_rows.push(PatternRowWithMetadata {
                                        patterns: new_patterns,
                                        arm_index: row.arm_index,
                                    });
                                    new_bindings.push(self.bindings[row_idx].clone());
                                }
                            }
                            SimplifiedPattern::Or(_) => {
                                // Nested OR patterns should be flattened elsewhere
                            }
                        }
                    }
                }
            }
        }

        Self {
            matrix: PatternMatrix::from_rows(new_matrix_rows),
            occurrences: new_occurrences,
            bindings: new_bindings,
        }
    }

    fn specialize_for_default(&self, _db: &'db dyn HirAnalysisDb) -> Self {
        let new_occurrences = self.occurrences[1..].to_vec();
        let mut new_matrix_rows = Vec::new();
        let mut new_bindings = Vec::new();

        for (row_idx, row) in self.matrix.rows().iter().enumerate() {
            if row.patterns.is_empty() {
                continue;
            }

            let first_pat = &row.patterns[0];
            let rest_patterns = &row.patterns[1..];

            match first_pat {
                SimplifiedPattern::Wildcard { binding } => {
                    let mut row_bindings = self.bindings[row_idx].clone();
                    if let Some(name) = binding {
                        row_bindings.insert(
                            (name.to_string(), row.arm_index),
                            self.occurrences[0].clone(),
                        );
                    }

                    new_matrix_rows.push(PatternRowWithMetadata {
                        patterns: rest_patterns.to_vec(),
                        arm_index: row.arm_index,
                    });
                    new_bindings.push(row_bindings);
                }
                SimplifiedPattern::Or(or_patterns) => {
                    // Include if any alternative is a wildcard
                    if or_patterns
                        .iter()
                        .any(|p| matches!(p, SimplifiedPattern::Wildcard { .. }))
                    {
                        new_matrix_rows.push(PatternRowWithMetadata {
                            patterns: rest_patterns.to_vec(),
                            arm_index: row.arm_index,
                        });
                        new_bindings.push(self.bindings[row_idx].clone());
                    }
                }
                SimplifiedPattern::Constructor { .. } => {
                    // Constructors don't contribute to default case
                }
            }
        }

        Self {
            matrix: PatternMatrix::from_rows(new_matrix_rows),
            occurrences: new_occurrences,
            bindings: new_bindings,
        }
    }

    fn extract_bindings_for_first_row(&self) -> FxHashMap<(String, usize), Occurrence> {
        if self.matrix.is_empty() {
            return FxHashMap::default();
        }

        let mut bindings = if !self.bindings.is_empty() {
            self.bindings[0].clone()
        } else {
            FxHashMap::default()
        };

        // Add bindings from remaining wildcard patterns in first row
        for col in 0..self.matrix.column_count() {
            if let Some(SimplifiedPattern::Wildcard {
                binding: Some(name),
            }) = self.matrix.get_pattern(0, col)
            {
                if let Some(arm_index) = self.matrix.get_arm_index(0) {
                    bindings.insert((name.to_string(), arm_index), self.occurrences[col].clone());
                }
            }
        }

        bindings
    }
}
