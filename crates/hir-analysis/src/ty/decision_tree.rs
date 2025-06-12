//! Decision tree generation for efficient pattern matching compilation
//! Based on "Compiling pattern matching to good decision trees"

use super::pattern_analysis::{PatternMatrix, PatternRowVec, SigmaSet};
use super::simplified_pattern::{ConstructorKind, SimplifiedPattern, SimplifiedPatternKind};
use crate::ty::ty_def::TyId;
use crate::HirAnalysisDb;
use hir::hir_def::IdentId;
use indexmap::IndexMap;

/// A decision tree for pattern matching compilation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DecisionTree<'db> {
    /// Leaf node - execute this match arm
    Leaf(LeafNode<'db>),
    /// Switch node - test a value and branch
    Switch(SwitchNode<'db>),
}

/// A leaf node in the decision tree
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LeafNode<'db> {
    pub arm_index: usize,
    pub bindings: IndexMap<(IdentId<'db>, usize), Occurrence>,
}

impl<'db> LeafNode<'db> {
    fn new(arm: SimplifiedArm<'db>, occurrences: &[Occurrence]) -> Self {
        let arm_index = arm.body;
        let bindings = arm.finalize_binds(occurrences);
        Self {
            arm_index,
            bindings,
        }
    }
}

/// A switch node in the decision tree
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwitchNode<'db> {
    pub occurrence: Occurrence,
    pub arms: Vec<(Case<'db>, DecisionTree<'db>)>,
}

/// A case in a switch node
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Case<'db> {
    Constructor(ConstructorKind<'db>),
    Default,
}

/// Represents a path to a value in the matched expression
/// e.g., expr.0.1 would be Occurrence(vec![0, 1])
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Occurrence(pub Vec<usize>);

impl Default for Occurrence {
    fn default() -> Self {
        Self::new()
    }
}

impl Occurrence {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn child(&self, index: usize) -> Self {
        let mut path = self.0.clone();
        path.push(index);
        Self(path)
    }

    pub fn iter(&self) -> impl Iterator<Item = &usize> {
        self.0.iter()
    }

    pub fn parent(&self) -> Option<Occurrence> {
        let mut inner = self.0.clone();
        inner.pop().map(|_| Occurrence(inner))
    }

    fn phi_specialize(&self, db: &dyn HirAnalysisDb, ctor: ConstructorKind) -> Vec<Self> {
        let arity = ctor.arity(db);
        (0..arity)
            .map(|i| {
                let mut inner = self.0.clone();
                inner.push(i);
                Self(inner)
            })
            .collect()
    }

    pub fn last_index(&self) -> Option<usize> {
        self.0.last().copied()
    }
}

/// Column selection policy for decision tree optimization
#[derive(Debug, Clone, Default)]
pub struct ColumnSelectionPolicy(Vec<ColumnScoringFunction>);

impl ColumnSelectionPolicy {
    /// The score of column i is the sum of the negation of the arities of
    /// constructors in sigma(i).
    pub fn arity(&mut self) -> &mut Self {
        self.add_heuristic(ColumnScoringFunction::Arity)
    }

    /// The score is the negation of the cardinal of sigma(i), C(Sigma(i)).
    /// If sigma(i) is NOT complete, the resulting score is C(Sigma(i)) - 1.
    pub fn small_branching(&mut self) -> &mut Self {
        self.add_heuristic(ColumnScoringFunction::SmallBranching)
    }

    /// The score is the number of needed rows of column i in the necessity
    /// matrix.
    pub fn needed_column(&mut self) -> &mut Self {
        self.add_heuristic(ColumnScoringFunction::NeededColumn)
    }

    /// The score is the larger row index j such that column i is needed for all
    /// rows j′; 1 ≤ j′ ≤ j.
    pub fn needed_prefix(&mut self) -> &mut Self {
        self.add_heuristic(ColumnScoringFunction::NeededPrefix)
    }

    fn add_heuristic(&mut self, heuristic: ColumnScoringFunction) -> &mut Self {
        self.0.push(heuristic);
        self
    }

    fn select_column<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        matrix: &SimplifiedArmMatrix<'db>,
    ) -> usize {
        let mut candidates: Vec<_> = (0..matrix.ncols()).collect();

        for scoring_fn in &self.0 {
            let mut max_score = i32::MIN;
            for col in std::mem::take(&mut candidates) {
                let score = scoring_fn.score(db, matrix, col);
                match score.cmp(&max_score) {
                    std::cmp::Ordering::Less => {}
                    std::cmp::Ordering::Equal => {
                        candidates.push(col);
                    }
                    std::cmp::Ordering::Greater => {
                        candidates = vec![col];
                        max_score = score;
                    }
                }
            }

            if candidates.len() == 1 {
                return candidates.pop().unwrap();
            }
        }

        // If there are more than one candidates remained, filter the columns with the
        // shortest occurrences among the candidates, then select the rightmost one.
        // This heuristics corresponds to the R pseudo heuristic in the paper.
        let mut shortest_occurrences = usize::MAX;
        for col in std::mem::take(&mut candidates) {
            let occurrences = matrix.occurrences[col].0.len();
            match occurrences.cmp(&shortest_occurrences) {
                std::cmp::Ordering::Less => {
                    candidates = vec![col];
                    shortest_occurrences = occurrences;
                }
                std::cmp::Ordering::Equal => {
                    candidates.push(col);
                }
                std::cmp::Ordering::Greater => {}
            }
        }

        candidates.pop().unwrap()
    }
}

/// Column scoring functions for decision tree optimization
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ColumnScoringFunction {
    /// The score of column i is the sum of the negation of the arities of
    /// constructors in sigma(i).
    Arity,
    /// The score is the negation of the cardinal of sigma(i), C(Sigma(i)).
    /// If sigma(i) is NOT complete, the resulting score is C(Sigma(i)) - 1.
    SmallBranching,
    /// The score is the number of needed rows of column i in the necessity
    /// matrix.
    NeededColumn,
    /// The score is the larger row index j such that column i is needed for all
    /// rows j′; 1 ≤ j′ ≤ j.
    NeededPrefix,
}

impl ColumnScoringFunction {
    fn score<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        matrix: &SimplifiedArmMatrix<'db>,
        col: usize,
    ) -> i32 {
        match self {
            ColumnScoringFunction::Arity => {
                let sigma_set = matrix.sigma_set(col);
                -sigma_set
                    .into_iter()
                    .map(|ctor| ctor.arity(db) as i32)
                    .sum::<i32>()
            }
            ColumnScoringFunction::SmallBranching => {
                let sigma_set = matrix.sigma_set(col);
                let ty = matrix.first_column_ty();
                let is_complete = sigma_set.is_complete(db, ty);
                let cardinal = sigma_set.into_iter().count() as i32;
                if is_complete {
                    -cardinal
                } else {
                    -(cardinal + 1)
                }
            }
            ColumnScoringFunction::NeededColumn => {
                matrix.necessity_matrix(db).compute_needed_column_score(col)
            }
            ColumnScoringFunction::NeededPrefix => {
                matrix.necessity_matrix(db).compute_needed_prefix_score(col)
            }
        }
    }
}

/// Build a decision tree from a pattern matrix with default policy
pub fn build_decision_tree<'db>(
    db: &'db dyn HirAnalysisDb,
    matrix: &PatternMatrix<'db>,
) -> DecisionTree<'db> {
    let policy = ColumnSelectionPolicy::default();
    build_decision_tree_with_policy(db, matrix, policy)
}

/// Build a decision tree from a pattern matrix with a specific column selection policy
pub fn build_decision_tree_with_policy<'db>(
    db: &'db dyn HirAnalysisDb,
    matrix: &PatternMatrix<'db>,
    policy: ColumnSelectionPolicy,
) -> DecisionTree<'db> {
    let simplified_matrix = SimplifiedArmMatrix::new(matrix);
    DecisionTreeBuilder::new(policy).build(db, simplified_matrix)
}

/// Create a default optimized column selection policy
pub fn default_optimized_policy() -> ColumnSelectionPolicy {
    let mut policy = ColumnSelectionPolicy::default();
    policy.arity().small_branching();
    policy
}

/// Build a decision tree from a pattern matrix with optimized column selection
pub fn build_optimized_decision_tree<'db>(
    db: &'db dyn HirAnalysisDb,
    matrix: &PatternMatrix<'db>,
) -> DecisionTree<'db> {
    let policy = default_optimized_policy();
    build_decision_tree_with_policy(db, matrix, policy)
}

/// Decision tree builder with configurable policy
struct DecisionTreeBuilder {
    policy: ColumnSelectionPolicy,
}

impl DecisionTreeBuilder {
    fn new(policy: ColumnSelectionPolicy) -> Self {
        DecisionTreeBuilder { policy }
    }

    fn build<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        mut matrix: SimplifiedArmMatrix<'db>,
    ) -> DecisionTree<'db> {
        debug_assert!(matrix.nrows() > 0, "unexhausted pattern matrix");

        if matrix.is_first_arm_satisfied() {
            matrix.arms.truncate(1);
            return DecisionTree::Leaf(LeafNode::new(
                matrix.arms.pop().unwrap(),
                &matrix.occurrences,
            ));
        }

        // Handle case where matrix has no columns (fully specialized)
        if matrix.ncols() == 0 {
            return DecisionTree::Leaf(LeafNode::new(
                matrix.arms.pop().unwrap(),
                &matrix.occurrences,
            ));
        }

        let col = self.policy.select_column(db, &matrix);
        matrix.swap(col);

        let mut switch_arms = vec![];

        // Handle case where occurrences vector is empty due to pattern analysis changes
        if matrix.occurrences.is_empty() {
            // If no occurrences but we have a matrix, create a default leaf
            if let Some(first_arm) = matrix.arms.first() {
                return DecisionTree::Leaf(LeafNode::new(first_arm.clone(), &[]));
            } else {
                panic!("Empty matrix with no arms in decision tree build");
            }
        }

        let occurrence = &matrix.occurrences[0];
        let sigma_set = matrix.sigma_set(0);
        let ty = matrix.first_column_ty();
        let is_complete = sigma_set.is_complete(db, ty);
        let constructors: Vec<_> = sigma_set.into_iter().collect();

        for ctor in constructors {
            let specialized_matrix = matrix.phi_specialize(db, ctor.clone(), occurrence);
            let subtree = self.build(db, specialized_matrix);
            switch_arms.push((Case::Constructor(ctor), subtree));
        }

        if !is_complete {
            let specialized_matrix = matrix.d_specialize(db, occurrence);
            let subtree = self.build(db, specialized_matrix);
            switch_arms.push((Case::Default, subtree));
        }

        DecisionTree::Switch(SwitchNode {
            occurrence: occurrence.clone(),
            arms: switch_arms,
        })
    }
}

/// Simplified arm representation for efficient processing
#[derive(Clone, Debug)]
struct SimplifiedArm<'db> {
    pat_vec: PatternRowVec<'db>,
    body: usize,
    binds: IndexMap<(IdentId<'db>, usize), Occurrence>,
}

impl<'db> SimplifiedArm<'db> {
    fn new(pat_vec: &PatternRowVec<'db>, body: usize) -> Self {
        let generalized_patterns = pat_vec.inner.iter().map(generalize_pattern).collect();
        let pat_vec = PatternRowVec::new(generalized_patterns);
        Self {
            pat_vec,
            body,
            binds: IndexMap::new(),
        }
    }

    fn new_binds(&self, occurrence: &Occurrence) -> IndexMap<(IdentId<'db>, usize), Occurrence> {
        let mut binds = self.binds.clone();
        if let Some(SimplifiedPatternKind::WildCard(Some(bind))) =
            self.pat_vec.head().map(|pat| &pat.kind)
        {
            binds.entry(bind.clone()).or_insert(occurrence.clone());
        }
        binds
    }

    fn finalize_binds(
        &self,
        occurrences: &[Occurrence],
    ) -> IndexMap<(IdentId<'db>, usize), Occurrence> {
        use super::simplified_pattern::SimplifiedPatternKind;

        let mut binds = self.binds.clone();

        // Extract bindings from current patterns
        for (pat, occurrence) in self.pat_vec.inner.iter().zip(occurrences.iter()) {
            if let SimplifiedPatternKind::WildCard(Some(bind)) = &pat.kind {
                binds
                    .entry(bind.clone())
                    .or_insert_with(|| occurrence.clone());
            }
        }

        binds
    }
}

/// Simplified arm matrix for efficient decision tree construction
#[derive(Clone, Debug)]
struct SimplifiedArmMatrix<'db> {
    arms: Vec<SimplifiedArm<'db>>,
    occurrences: Vec<Occurrence>,
}

impl<'db> SimplifiedArmMatrix<'db> {
    fn new(matrix: &PatternMatrix<'db>) -> Self {
        let cols = matrix.ncols();
        let arms: Vec<_> = matrix
            .rows
            .iter()
            .enumerate()
            .map(|(body, pat)| SimplifiedArm::new(pat, body))
            .collect();
        let occurrences = vec![Occurrence::new(); cols];

        SimplifiedArmMatrix { arms, occurrences }
    }

    fn nrows(&self) -> usize {
        self.arms.len()
    }

    fn ncols(&self) -> usize {
        if self.arms.is_empty() {
            0
        } else {
            self.arms[0].pat_vec.inner.len()
        }
    }

    fn sigma_set(&self, col: usize) -> SigmaSet<'db> {
        SigmaSet::from_rows(self.arms.iter().map(|arm| &arm.pat_vec), col)
    }

    fn first_column_ty(&self) -> TyId<'db> {
        if self.arms.is_empty() {
            panic!("Cannot get first column type from empty matrix");
        }
        self.arms[0].pat_vec.inner[0].ty
    }

    fn is_first_arm_satisfied(&self) -> bool {
        if self.arms.is_empty() {
            return false;
        }
        self.arms[0]
            .pat_vec
            .inner
            .iter()
            .all(SimplifiedPattern::is_wildcard)
    }

    fn swap(&mut self, col: usize) {
        if col == 0 {
            return;
        }

        // Swap column col with column 0 in all arms
        for arm in &mut self.arms {
            arm.pat_vec.inner.swap(0, col);
        }

        // Swap occurrences
        self.occurrences.swap(0, col);
    }

    fn phi_specialize(
        &self,
        db: &'db dyn HirAnalysisDb,
        ctor: ConstructorKind<'db>,
        occurrence: &Occurrence,
    ) -> Self {
        let mut new_arms = Vec::new();

        for arm in &self.arms {
            let binds = arm.new_binds(occurrence);
            new_arms.extend(
                arm.pat_vec
                    .phi_specialize(db, ctor)
                    .into_iter()
                    .map(|pat_vec| SimplifiedArm {
                        pat_vec,
                        body: arm.body,
                        binds: binds.clone(),
                    }),
            );
        }

        let mut new_occurrences = self.occurrences[0].phi_specialize(db, ctor);
        new_occurrences.extend_from_slice(&self.occurrences.as_slice()[1..]);

        Self {
            arms: new_arms,
            occurrences: new_occurrences,
        }
    }

    pub fn d_specialize(&self, db: &'db dyn HirAnalysisDb, _occurrence: &Occurrence) -> Self {
        let mut new_arms = Vec::new();

        for arm in &self.arms {
            new_arms.extend(arm.pat_vec.d_specialize(db).into_iter().map(|pat_vec| {
                SimplifiedArm {
                    pat_vec,
                    body: arm.body,
                    binds: arm.binds.clone(),
                }
            }));
        }

        SimplifiedArmMatrix {
            arms: new_arms,
            occurrences: self.occurrences[1..].to_vec(),
        }
    }

    fn necessity_matrix(&self, db: &'db dyn HirAnalysisDb) -> NecessityMatrix {
        NecessityMatrix::from_matrix(db, self)
    }
}

/// Necessity matrix for pattern analysis optimization
struct NecessityMatrix {
    data: Vec<bool>,
    ncol: usize,
    nrow: usize,
}

impl NecessityMatrix {
    fn new(ncol: usize, nrow: usize) -> Self {
        let data = vec![false; ncol * nrow];
        Self { data, ncol, nrow }
    }

    fn from_matrix<'db>(db: &'db dyn HirAnalysisDb, matrix: &SimplifiedArmMatrix<'db>) -> Self {
        let ncol = matrix.ncols();
        let nrow = matrix.nrows();
        let mut necessity_mat = Self::new(ncol, nrow);
        necessity_mat.compute(db, matrix);
        necessity_mat
    }

    fn compute<'db>(&mut self, db: &'db dyn HirAnalysisDb, matrix: &SimplifiedArmMatrix<'db>) {
        for i in 0..self.nrow {
            for j in 0..self.ncol {
                let pos = self.pos(i, j);
                let is_needed = self.is_needed(db, matrix, i, j);
                self.data[pos] = is_needed;
            }
        }
    }

    fn is_needed<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        matrix: &SimplifiedArmMatrix<'db>,
        row: usize,
        col: usize,
    ) -> bool {
        if row >= matrix.nrows() || col >= matrix.ncols() {
            return false;
        }

        let current_pattern = &matrix.arms[row].pat_vec.inner[col];

        // Check if this column is needed to distinguish this row from previous rows
        for prev_row in 0..row {
            let prev_pattern = &matrix.arms[prev_row].pat_vec.inner[col];

            if !patterns_compatible(db, current_pattern, prev_pattern) {
                return true;
            }
        }

        false
    }

    fn compute_needed_column_score(&self, col: usize) -> i32 {
        let mut num = 0;
        for i in 0..self.nrow {
            if self.data[self.pos(i, col)] {
                num += 1;
            }
        }
        num
    }

    fn compute_needed_prefix_score(&self, col: usize) -> i32 {
        let mut current_row = 0;
        for i in 0..self.nrow {
            if self.data[self.pos(i, col)] {
                current_row += 1;
            } else {
                return current_row;
            }
        }
        current_row
    }

    fn pos(&self, row: usize, col: usize) -> usize {
        self.ncol * row + col
    }
}

/// Generalize a pattern by removing bindings from constructors
fn generalize_pattern<'db>(pat: &SimplifiedPattern<'db>) -> SimplifiedPattern<'db> {
    match &pat.kind {
        SimplifiedPatternKind::WildCard(_) => pat.clone(),

        SimplifiedPatternKind::Constructor { kind, fields } => {
            let fields = fields.iter().map(generalize_pattern).collect();
            let kind = SimplifiedPatternKind::Constructor {
                kind: kind.clone(),
                fields,
            };
            SimplifiedPattern::new(kind, pat.ty)
        }

        SimplifiedPatternKind::Or(pats) => {
            let mut gen_pats = vec![];
            for pat in pats {
                let gen_pat = generalize_pattern(pat);
                if gen_pat.is_wildcard() {
                    gen_pats.push(gen_pat);
                    break;
                } else {
                    gen_pats.push(gen_pat);
                }
            }

            if gen_pats.len() == 1 {
                gen_pats.pop().unwrap()
            } else {
                SimplifiedPattern::new(SimplifiedPatternKind::Or(gen_pats), pat.ty)
            }
        }
    }
}

/// Check if two patterns are compatible (could match the same value)
fn patterns_compatible<'db>(
    _db: &'db dyn HirAnalysisDb,
    pat1: &SimplifiedPattern<'db>,
    pat2: &SimplifiedPattern<'db>,
) -> bool {
    match (&pat1.kind, &pat2.kind) {
        // Wildcards are compatible with everything
        (SimplifiedPatternKind::WildCard(_), _) | (_, SimplifiedPatternKind::WildCard(_)) => true,

        // Constructors are compatible if they're the same constructor
        (
            SimplifiedPatternKind::Constructor { kind: k1, .. },
            SimplifiedPatternKind::Constructor { kind: k2, .. },
        ) => k1 == k2,

        // Or patterns need more complex analysis, for now assume compatible
        (SimplifiedPatternKind::Or(_), _) | (_, SimplifiedPatternKind::Or(_)) => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_occurrence_nested_access() {
        let root = Occurrence::new();
        let tuple_first = root.child(0);
        let tuple_second = root.child(1);
        let nested = tuple_first.child(0).child(1);

        assert_eq!(root.0, vec![]);
        assert_eq!(tuple_first.0, vec![0]);
        assert_eq!(tuple_second.0, vec![1]);
        assert_eq!(nested.0, vec![0, 0, 1]);
    }

    #[test]
    fn test_occurrence_path_building() {
        // Test building complex occurrence paths
        let root = Occurrence::new();
        assert_eq!(root.0, vec![]);

        // Simulate accessing tuple.0.field.1
        let tuple_field = root.child(0);
        let nested_field = tuple_field.child(2);
        let final_access = nested_field.child(1);

        assert_eq!(tuple_field.0, vec![0]);
        assert_eq!(nested_field.0, vec![0, 2]);
        assert_eq!(final_access.0, vec![0, 2, 1]);

        // Test that different paths are independent
        let other_path = root.child(1).child(0);
        assert_eq!(other_path.0, vec![1, 0]);
        assert_ne!(final_access.0, other_path.0);
    }

    #[test]
    fn test_necessity_matrix_api() {
        // Test necessity matrix creation and basic operations
        let matrix = NecessityMatrix::new(3, 2);

        // Test position calculation
        assert_eq!(matrix.pos(0, 0), 0);
        assert_eq!(matrix.pos(0, 1), 1);
        assert_eq!(matrix.pos(0, 2), 2);
        assert_eq!(matrix.pos(1, 0), 3);
        assert_eq!(matrix.pos(1, 1), 4);
        assert_eq!(matrix.pos(1, 2), 5);
    }
}
