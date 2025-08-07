//! AST spatial index for efficient position-based lookups
//!
//! This module provides utilities for finding AST nodes at specific text positions
//! within source code. It uses rowan's syntax tree traversal to efficiently locate
//! the deepest node that contains a given cursor position.

use crate::{ast::prelude::AstNode, SyntaxNode, TextSize};
use rangemap::RangeMap;
use std::ops::Range;

/// Find the deepest syntax node that contains the given offset
///
/// This function recursively traverses the syntax tree to find the most specific
/// (deepest) node that contains the cursor position. This is based on the
/// rust-analyzer pattern for position-based AST lookups.
///
/// # Arguments
/// * `node` - The root syntax node to search from
/// * `offset` - The text position to find
///
/// # Returns
/// The deepest syntax node containing the offset, or the input node if no
/// children contain the offset.
pub fn find_node_at_offset(node: &SyntaxNode, offset: TextSize) -> SyntaxNode {
    // Ensure the node contains the offset
    let node_range = node.text_range();
    if !node_range.contains(offset) {
        return node.clone();
    }

    // Try to find a child that contains the offset
    for child in node.children() {
        let child_range = child.text_range();
        if child_range.contains(offset) {
            return find_node_at_offset(&child, offset);
        }
    }

    // No child contains the offset, so this node is the deepest
    node.clone()
}

/// Spatial index for AST nodes using range-based lookups
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstSpatialIndex {
    /// Map from text ranges to expression nodes
    expressions: RangeMap<TextSize, crate::ast::AstPtr<crate::ast::Expr>>,
    /// Map from text ranges to statement nodes
    statements: RangeMap<TextSize, crate::ast::AstPtr<crate::ast::Stmt>>,
    /// Map from text ranges to pattern nodes
    patterns: RangeMap<TextSize, crate::ast::AstPtr<crate::ast::Pat>>,
}

impl AstSpatialIndex {
    /// Create a new empty spatial index
    pub fn new() -> Self {
        Self {
            expressions: RangeMap::new(),
            statements: RangeMap::new(),
            patterns: RangeMap::new(),
        }
    }

    /// Build a spatial index from a syntax tree
    pub fn build_from_syntax(root: &SyntaxNode) -> Self {
        let mut index = Self::new();
        index.index_node(root);
        index
    }

    /// Find the most specific AST node at the given position
    pub fn find_at_position(&self, offset: TextSize) -> AstNodeAtPosition {
        // Try expressions first (most specific)
        if let Some(_expr_ptr) = self.expressions.get(&offset) {
            // Note: We'd need to resolve the AstPtr to get the actual node
            // For now, return a placeholder
            return AstNodeAtPosition::None; // TODO: resolve AstPtr
        }

        // Try statements
        if let Some(_stmt_ptr) = self.statements.get(&offset) {
            return AstNodeAtPosition::None; // TODO: resolve AstPtr
        }

        // Try patterns
        if let Some(_pat_ptr) = self.patterns.get(&offset) {
            return AstNodeAtPosition::None; // TODO: resolve AstPtr
        }

        AstNodeAtPosition::None
    }

    /// Find all nodes overlapping with the given range
    pub fn find_overlapping(&self, range: Range<TextSize>) -> Vec<AstNodeAtPosition> {
        let results = Vec::new();

        // Collect overlapping expressions
        for (_range, _expr_ptr) in self.expressions.overlapping(&range) {
            // TODO: resolve AstPtr and add to results
        }

        // Collect overlapping statements
        for (_range, _stmt_ptr) in self.statements.overlapping(&range) {
            // TODO: resolve AstPtr and add to results
        }

        // Collect overlapping patterns
        for (_range, _pat_ptr) in self.patterns.overlapping(&range) {
            // TODO: resolve AstPtr and add to results
        }

        results
    }

    /// Recursively index all nodes in the syntax tree
    fn index_node(&mut self, node: &SyntaxNode) {
        use crate::ast;

        let range = node.text_range();
        let range_start = range.start();
        let range_end = range.end();

        // Try to cast to specific AST node types and index them
        if let Some(expr) = ast::Expr::cast(node.clone()) {
            let ptr = ast::AstPtr::new(&expr);
            self.expressions.insert(range_start..range_end, ptr);
        } else if let Some(stmt) = ast::Stmt::cast(node.clone()) {
            let ptr = ast::AstPtr::new(&stmt);
            self.statements.insert(range_start..range_end, ptr);
        } else if let Some(pat) = ast::Pat::cast(node.clone()) {
            let ptr = ast::AstPtr::new(&pat);
            self.patterns.insert(range_start..range_end, ptr);
        }

        // Recursively index children
        for child in node.children() {
            self.index_node(&child);
        }
    }
}

impl Default for AstSpatialIndex {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of finding an AST node at a specific position
#[derive(Debug, Clone)]
pub enum AstNodeAtPosition {
    /// Found an expression node
    Expr(crate::ast::Expr),
    /// Found a statement node
    Stmt(crate::ast::Stmt),
    /// Found a pattern node
    Pat(crate::ast::Pat),
    /// No relevant AST node found at this position
    None,
}

/// Find the most specific AST node at the given position
///
/// This function takes a syntax node and attempts to find the most specific
/// AST node (expression, statement, or pattern) at the given cursor position.
/// It prefers expressions over statements over patterns when multiple nodes
/// are at the same position.
///
/// # Arguments
/// * `root` - The root syntax node to search from
/// * `offset` - The text position to find
///
/// # Returns
/// The most specific AST node at the position, or `None` if no relevant
/// node is found.
pub fn find_ast_node_at_position(root: &SyntaxNode, offset: TextSize) -> AstNodeAtPosition {
    use crate::ast;

    // Find the deepest node containing the cursor
    let deepest_node = find_node_at_offset(root, offset);

    // Try to cast to specific AST node types, preferring expressions > statements > patterns
    if let Some(expr) = ast::Expr::cast(deepest_node.clone()) {
        return AstNodeAtPosition::Expr(expr);
    }

    if let Some(stmt) = ast::Stmt::cast(deepest_node.clone()) {
        return AstNodeAtPosition::Stmt(stmt);
    }

    if let Some(pat) = ast::Pat::cast(deepest_node.clone()) {
        return AstNodeAtPosition::Pat(pat);
    }

    AstNodeAtPosition::None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_source_file;

    #[test]
    fn test_find_node_at_offset_basic() {
        let source = "fn test() { 42 }";
        let (green_node, _) = parse_source_file(source);
        let root = SyntaxNode::new_root(green_node);

        // Test at the beginning
        let node = find_node_at_offset(&root, TextSize::from(0));
        assert!(node.text_range().contains(TextSize::from(0)));

        // Test at position of "42"
        let node = find_node_at_offset(&root, TextSize::from(12));
        assert!(node.text_range().contains(TextSize::from(12)));
    }

    #[test]
    fn test_find_ast_node_at_position() {
        let source = r#"
fn calculate(x: i32) -> i32 {
    let y = x + 1;
    let z = y * 2;
    z
}
"#;
        let (green_node, _) = parse_source_file(source);
        let root = SyntaxNode::new_root(green_node);

        // Test at various positions
        let positions = [
            TextSize::from(35), // Around "let y"
            TextSize::from(50), // Around "x + 1"
            TextSize::from(70), // Around "let z"
        ];

        for pos in positions {
            let result = find_ast_node_at_position(&root, pos);
            // Should find some AST node at these positions
            assert!(!matches!(result, AstNodeAtPosition::None));
        }
    }

    #[test]
    fn test_spatial_index_creation() {
        let source = "fn test() { let x = 42; }";
        let (green_node, _) = parse_source_file(source);
        let root = SyntaxNode::new_root(green_node);

        // Build spatial index
        let index = AstSpatialIndex::build_from_syntax(&root);

        // Should have indexed some nodes
        // Note: Actual verification would require resolving AstPtrs
        // For now, just ensure creation doesn't panic
        let _result = index.find_at_position(TextSize::from(20));
    }

    #[test]
    fn test_spatial_index_overlapping() {
        let source = "fn test() { let x = 42; let y = 24; }";
        let (green_node, _) = parse_source_file(source);
        let root = SyntaxNode::new_root(green_node);

        let index = AstSpatialIndex::build_from_syntax(&root);

        // Test overlapping range query
        let range = TextSize::from(10)..TextSize::from(30);
        let results = index.find_overlapping(range);

        // Should work without panicking
        // Actual verification would require resolving AstPtrs
        println!("Found {} overlapping nodes", results.len());
    }
}
