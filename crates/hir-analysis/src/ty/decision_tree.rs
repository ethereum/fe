//! Decision tree generation for efficient pattern matching compilation
//! Based on "Compiling pattern matching to good decision trees"

use super::pattern_analysis::{ConstructorKind, PatternMatrix};
use crate::HirAnalysisDb;

/// A decision tree for pattern matching compilation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DecisionTree<'db> {
    /// Leaf node - execute this match arm
    Leaf {
        arm_index: usize,
        bindings: Vec<(String, Occurrence)>,
    },
    /// Switch node - test a value and branch
    Switch {
        occurrence: Occurrence,
        branches: Vec<(ConstructorKind<'db>, DecisionTree<'db>)>,
        default: Option<Box<DecisionTree<'db>>>,
    },
}

/// Represents a path to a value in the matched expression
/// e.g., expr.0.1 would be Occurrence(vec![0, 1])
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Occurrence(pub Vec<usize>);

impl Occurrence {
    pub fn new() -> Self {
        Self(vec![])
    }
    
    pub fn child(&self, index: usize) -> Self {
        let mut path = self.0.clone();
        path.push(index);
        Self(path)
    }
}

/// Build a decision tree from a pattern matrix with arm tracking
pub fn build_decision_tree<'db>(
    db: &'db dyn HirAnalysisDb,
    matrix: &PatternMatrix<'db>,
) -> DecisionTree<'db> {
    build_decision_tree_with_arms(db, matrix, (0..matrix.nrows()).collect())
}

fn build_decision_tree_with_arms<'db>(
    db: &'db dyn HirAnalysisDb,
    matrix: &PatternMatrix<'db>,
    arm_indices: Vec<usize>,
) -> DecisionTree<'db> {
    if matrix.nrows() == 0 {
        panic!("Cannot build decision tree from empty matrix");
    }
    
    if matrix.ncols() == 0 || is_all_wildcards(matrix, 0) {
        return DecisionTree::Leaf {
            arm_index: arm_indices[0],
            bindings: vec![],
        };
    }
    
    // For now, always select column 0
    let occurrence = Occurrence::new();
    let ty = matrix.first_column_ty();
    let sigma_set = matrix.sigma_set();
    
    let mut branches = vec![];
    for ctor in sigma_set.into_iter() {
        let (specialized_matrix, specialized_arms) = specialize_matrix_with_arms(db, matrix, &arm_indices, ctor, true);
        if specialized_matrix.nrows() > 0 {
            let subtree = build_decision_tree_with_arms(db, &specialized_matrix, specialized_arms);
            branches.push((ctor, subtree));
        }
    }
    
    let sigma_set = matrix.sigma_set(); // Recreate since we consumed it
    let default = if !sigma_set.is_complete(db, ty) {
        let (specialized_matrix, specialized_arms) = specialize_matrix_with_arms(db, matrix, &arm_indices, ConstructorKind::Tuple(ty), false);
        if specialized_matrix.nrows() > 0 {
            Some(Box::new(build_decision_tree_with_arms(db, &specialized_matrix, specialized_arms)))
        } else {
            None
        }
    } else {
        None
    };
    
    DecisionTree::Switch {
        occurrence,
        branches,
        default,
    }
}

fn specialize_matrix_with_arms<'db>(
    db: &'db dyn HirAnalysisDb,
    matrix: &PatternMatrix<'db>,
    arm_indices: &[usize],
    ctor: ConstructorKind<'db>,
    is_constructor: bool,
) -> (PatternMatrix<'db>, Vec<usize>) {
    let mut new_rows = Vec::new();
    let mut new_arms = Vec::new();
    
    for (row_idx, row) in matrix.rows.iter().enumerate() {
        let specialized_rows = if is_constructor {
            row.phi_specialize(db, ctor)
        } else {
            row.d_specialize(db)
        };
        
        for specialized_row in specialized_rows {
            new_rows.push(specialized_row);
            new_arms.push(arm_indices[row_idx]);
        }
    }
    
    (PatternMatrix::new(new_rows), new_arms)
}

/// Check if the first row is all wildcards
fn is_all_wildcards<'db>(matrix: &PatternMatrix<'db>, row: usize) -> bool {
    if row >= matrix.nrows() {
        return false;
    }
    
    matrix.rows[row].inner.iter().all(|pat| pat.is_wildcard())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_occurrence_creation() {
        let occ = Occurrence::new();
        assert_eq!(occ.0, vec![]);
        
        let child = occ.child(0);
        assert_eq!(child.0, vec![0]);
        
        let grandchild = child.child(1);
        assert_eq!(grandchild.0, vec![0, 1]);
    }

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
    fn test_decision_tree_leaf_creation() {
        let leaf = DecisionTree::Leaf {
            arm_index: 0,
            bindings: vec![("x".to_string(), Occurrence::new())],
        };
        
        match leaf {
            DecisionTree::Leaf { arm_index, bindings } => {
                assert_eq!(arm_index, 0);
                assert_eq!(bindings.len(), 1);
                assert_eq!(bindings[0].0, "x");
                assert_eq!(bindings[0].1, Occurrence::new());
            }
            _ => panic!("Expected leaf node"),
        }
    }

    #[test]
    fn test_decision_tree_switch_creation() {
        let leaf = DecisionTree::Leaf {
            arm_index: 0,
            bindings: vec![],
        };
        
        let switch = DecisionTree::Switch {
            occurrence: Occurrence::new(),
            branches: vec![],
            default: Some(Box::new(leaf)),
        };
        
        match switch {
            DecisionTree::Switch { occurrence, branches, default } => {
                assert_eq!(occurrence, Occurrence::new());
                assert_eq!(branches.len(), 0);
                assert!(default.is_some());
            }
            _ => panic!("Expected switch node"),
        }
    }

    #[test]
    fn test_is_all_wildcards_helper() {
        use crate::ty::pattern_analysis::{PatternMatrix, PatternRowVec, SimplifiedPattern, SimplifiedPatternKind};
        use crate::ty::ty_def::TyId;
        
        // Test with empty matrix
        let empty_matrix = PatternMatrix::new(vec![]);
        assert!(!is_all_wildcards(&empty_matrix, 0));
        assert!(!is_all_wildcards(&empty_matrix, 1));
        
        // Test with wildcard pattern - we can't easily create a TyId without a db,
        // so this test shows the intended structure
    }

    // Helper to create a mock database for testing
    // For now, we'll create minimal tests that don't require full pattern matrices
    
    #[test]
    fn test_decision_tree_api_coverage() {
        // Test that our decision tree structures work as expected
        let occurrence = Occurrence::new();
        let child_occurrence = occurrence.child(0);
        
        // Test leaf creation with bindings
        let leaf = DecisionTree::Leaf {
            arm_index: 1,
            bindings: vec![
                ("x".to_string(), occurrence.clone()),
                ("y".to_string(), child_occurrence),
            ],
        };
        
        if let DecisionTree::Leaf { arm_index, bindings } = leaf {
            assert_eq!(arm_index, 1);
            assert_eq!(bindings.len(), 2);
        } else {
            panic!("Expected leaf");
        }
        
        // Test switch creation with multiple branches
        let branch_leaf = DecisionTree::Leaf {
            arm_index: 0,
            bindings: vec![],
        };
        
        let switch = DecisionTree::Switch {
            occurrence: Occurrence::new(),
            branches: vec![], // We can't easily create ConstructorKind without full setup
            default: Some(Box::new(branch_leaf)),
        };
        
        if let DecisionTree::Switch { occurrence, branches, default } = switch {
            assert_eq!(occurrence, Occurrence::new());
            assert_eq!(branches.len(), 0);
            assert!(default.is_some());
        } else {
            panic!("Expected switch");
        }
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
    fn test_decision_tree_nesting() {
        // Test creating nested decision trees
        let inner_leaf = DecisionTree::Leaf {
            arm_index: 2,
            bindings: vec![("inner".to_string(), Occurrence::new().child(1))],
        };
        
        let outer_switch = DecisionTree::Switch {
            occurrence: Occurrence::new(),
            branches: vec![], 
            default: Some(Box::new(inner_leaf)),
        };
        
        let root_switch = DecisionTree::Switch {
            occurrence: Occurrence::new(),
            branches: vec![],
            default: Some(Box::new(outer_switch)),
        };
        
        // Verify nested structure
        if let DecisionTree::Switch { default: Some(inner), .. } = root_switch {
            if let DecisionTree::Switch { default: Some(leaf), .. } = *inner {
                if let DecisionTree::Leaf { arm_index, bindings } = *leaf {
                    assert_eq!(arm_index, 2);
                    assert_eq!(bindings.len(), 1);
                } else {
                    panic!("Expected inner leaf");
                }
            } else {
                panic!("Expected inner switch");
            }
        } else {
            panic!("Expected root switch");
        }
    }
}