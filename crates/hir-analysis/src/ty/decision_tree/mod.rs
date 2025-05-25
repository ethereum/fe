//! # Decision Tree Generation for Pattern Matching
//!
//! This module implements decision tree generation for efficient pattern matching compilation.
//! The algorithm is based on Luc Maranget's "Compiling Pattern Matching to Good Decision Trees".
//!
//! ## Overview
//!
//! The decision tree compiler takes a pattern matrix (from pattern analysis) and generates
//! an optimized decision tree that can be efficiently lowered to executable code.
//!
//! ### Key Concepts
//!
//! 1. **Decision Tree**: A tree structure where:
//!    - **Leaf nodes** represent match arm execution
//!    - **Switch nodes** represent branching on constructor/value discrimination
//!
//! 2. **Occurrence**: Tracks the "path" through nested data structures (e.g., `expr.0.1`)
//!
//! 3. **Column Selection**: Heuristics for choosing which pattern column to branch on
//!    to generate efficient decision trees
//!
//! ## Usage
//!
//! ```rust,ignore
//! use crate::ty::decision_tree::{build_decision_tree, ColumnSelectionPolicy};
//! use crate::ty::pattern_analysis::PatternMatrix;
//!
//! // First, create a pattern matrix from your match arms
//! let pattern_matrix = PatternMatrix::new(simplified_patterns);
//!
//! // Configure the column selection policy for optimization
//! let policy = ColumnSelectionPolicy::new()
//!     .needed_prefix()    // Handle common patterns first
//!     .small_branching()  // Prefer fewer branches
//!     .arity();          // Consider constructor complexity
//!
//! // Build the optimized decision tree
//! let tree = build_decision_tree(db, &pattern_matrix, policy);
//!
//! // The tree can then be lowered to executable code
//! match tree {
//!     DecisionTree::Leaf(leaf) => {
//!         // Execute match arm leaf.arm_idx with bindings leaf.bindings
//!     }
//!     DecisionTree::Switch(switch) => {
//!         // Branch on switch.occurrence and handle each case
//!         for (case, subtree) in switch.arms {
//!             // Handle specific constructor or default case
//!         }
//!     }
//! }
//! ```

mod builder;
mod types;
mod tests;

pub use self::builder::{build_decision_tree, ColumnSelectionPolicy};
pub use self::types::{Case, DecisionTree, LeafNode, Occurrence, SwitchNode};