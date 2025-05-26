#[cfg(test)]
use crate::ty::decision_tree::{ColumnSelectionPolicy, DecisionTree, LeafNode, Occurrence};
#[cfg(test)]
use crate::ty::pattern_analysis::{PatternMatrix, SimplifiedPattern};

#[test]
fn test_occurrence_creation() {
    let root = Occurrence::new();
    assert_eq!(root.depth(), 0);
    assert_eq!(root.path(), &[]);
    assert_eq!(root.parent(), None);
    assert_eq!(root.last_index(), None);
}

#[test]
fn test_occurrence_extension() {
    let root = Occurrence::new();
    let field_0 = root.extend(0);
    assert_eq!(field_0.depth(), 1);
    assert_eq!(field_0.path(), &[0]);
    assert_eq!(field_0.last_index(), Some(0));

    let field_0_1 = field_0.clone().extend(1);
    assert_eq!(field_0_1.depth(), 2);
    assert_eq!(field_0_1.path(), &[0, 1]);
    assert_eq!(field_0_1.last_index(), Some(1));

    // Test parent relationship
    assert_eq!(field_0_1.parent(), Some(field_0.clone()));
    assert_eq!(field_0.parent(), Some(root));
}

#[test]
fn test_occurrence_specialization() {
    let root = Occurrence::new();
    let specialized = root.specialize_for_arity(3);

    assert_eq!(specialized.len(), 3);
    assert_eq!(specialized[0], root.extend(0));
    assert_eq!(specialized[1], root.extend(1));
    assert_eq!(specialized[2], root.extend(2));
}

#[test]
fn test_leaf_node_creation() {
    let leaf = LeafNode::new(42);
    assert_eq!(leaf.arm_idx, 42);
    assert!(leaf.bindings.is_empty());
}

#[test]
fn test_column_selection_policy_builder() {
    let _policy = ColumnSelectionPolicy::new()
        .arity()
        .small_branching()
        .needed_prefix();

    // Just test that the policy builds without error
    // Can't test heuristics field since it's private
}

#[test]
fn test_decision_tree_construction() {
    // Test basic decision tree structure creation
    let leaf = LeafNode::new(0);
    let tree = DecisionTree::Leaf(leaf);

    match tree {
        DecisionTree::Leaf(node) => {
            assert_eq!(node.arm_idx, 0);
        }
        DecisionTree::Switch(_) => {
            panic!("Expected leaf node");
        }
    }
}

#[test]
fn test_pattern_matrix_basic_operations() {
    let patterns = vec![SimplifiedPattern::Wildcard { binding: None }];
    let matrix = PatternMatrix::new(patterns);

    assert!(!matrix.is_empty());
    assert_eq!(matrix.row_count(), 1);
    assert_eq!(matrix.column_count(), 1);
    assert_eq!(matrix.get_arm_index(0), Some(0));
    assert!(matrix.first_row_all_wildcards());
}
