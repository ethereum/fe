//! AST spatial index for efficient position-based lookups
//!
//! This module provides utilities for finding AST nodes at specific text positions
//! within source code. It uses rowan's syntax tree traversal to efficiently locate
//! the deepest node that contains a given cursor position.

use crate::{ast::prelude::AstNode, SyntaxNode, TextSize};

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
    use crate::{lexer::Lexer, parse_source_file, parser::Parser};

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
    fn test_ast_node_type_preference() {
        // Test that expressions are preferred over statements when both are possible
        let source = "fn test() { let x = 42; }";
        let (green_node, _) = parse_source_file(source);
        let root = SyntaxNode::new_root(green_node);

        // Position at "42" should find an expression
        let result = find_ast_node_at_position(&root, TextSize::from(20));
        assert!(matches!(result, AstNodeAtPosition::Expr(_)));
    }
}
