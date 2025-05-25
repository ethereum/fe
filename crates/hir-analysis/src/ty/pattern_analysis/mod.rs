//! # Pattern Matching Analysis for Fe Compiler
//!
//! This module implements comprehensive pattern matching analysis including:
//! - **Exhaustiveness checking**: Ensures all possible values are covered by match patterns
//! - **Reachability analysis**: Detects unreachable (dead) patterns that can never be matched
//!
//! ## Algorithm Overview
//!
//! The implementation is based on the classical pattern matching algorithm described in:
//! - "Warnings for pattern matching" by Luc Maranget
//! - "Compiling Pattern Matching to Good Decision Trees" by Luc Maranget
//!
//! ### Key Concepts
//!
//! 1. **Pattern Matrix**: A 2D matrix where each row represents a match arm and each column
//!    represents a pattern position. The algorithm operates by specializing this matrix.
//!
//! 2. **Specialization**: The core operation that splits patterns based on constructors:
//!    - `φ(C, P)` (phi_specialize): Specializes matrix P for constructor C
//!    - `δ(P)` (d_specialize): Specializes matrix P for wildcard patterns
//!
//! 3. **Usefulness**: A pattern is "useful" if there exists at least one value that:
//!    - Matches the pattern
//!    - Doesn't match any previous pattern in the matrix
//!
//! ### How It Works
//!
//! ```text
//! Exhaustiveness: U(∅, [p₁, p₂, ..., pₙ]) = false
//! - If we can construct a value that matches no patterns, the match is non-exhaustive
//!
//! Reachability: U([p₁, ..., pᵢ₋₁], pᵢ) = true
//! - If pattern pᵢ is useful against previous patterns, it's reachable
//! ```
//!
//! ## Usage Example
//!
//! ```rust,ignore
//! use crate::ty::pattern_analysis::PatternAnalyzer;
//!
//! let analyzer = PatternAnalyzer::new(db);
//!
//! // Check if patterns are exhaustive
//! match analyzer.check_exhaustiveness(scrutinee_type, &patterns, body) {
//!     Ok(()) => println!("Match is exhaustive"),
//!     Err(missing) => println!("Missing patterns: {:?}", missing),
//! }
//!
//! // Check if a pattern is reachable
//! let is_reachable = analyzer.check_reachability(&pattern, &previous_patterns, body);
//! ```
//!
//! ## Module Structure
//!
//! - [`core`]: Core algorithm implementation with [`PatternMatrix`] and [`SimplifiedPattern`]
//! - [`hir_integration`]: HIR integration layer with [`PatternAnalyzer`] API
//!
//! ## Pattern Types Supported
//!
//! - **Literals**: Boolean, integer, string literals
//! - **Wildcards**: `_` patterns that match anything
//! - **Tuples**: `(a, b, c)` with arbitrary nesting
//! - **Records**: Struct and enum variant patterns
//! - **Or patterns**: `A | B` alternative patterns
//!
//! ## Design Principles
//!
//! 1. **Separation of Concerns**: HIR-specific logic is separate from core algorithm
//! 2. **Mathematical Soundness**: Implementation follows proven algorithms exactly
//! 3. **Performance**: Matrix operations are optimized for common cases
//! 4. **Extensibility**: Easy to add new pattern types and constructors

mod core;
mod hir_integration;

pub use self::core::{Constructor, MatchArmIndex, PatternMatrix, PatternRowWithMetadata, SimplifiedPattern};
pub use self::hir_integration::PatternAnalyzer;
