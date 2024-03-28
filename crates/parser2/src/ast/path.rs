use rowan::ast::{support, AstNode};

use super::{ast_node, AstChildren};
use crate::{syntax_node::SyntaxToken, SyntaxKind as SK};

ast_node! {
    /// A path.
    /// `foo::bar::baz`
    pub struct Path,
    SK::Path,
    IntoIterator<Item=PathSegment>,
}
impl Path {
    /// Returns the segments of the path.
    pub fn segments(&self) -> AstChildren<PathSegment> {
        support::children(self.syntax())
    }
}

ast_node! {
    /// A path segment.
    pub struct PathSegment,
    SK::PathSegment
}
impl PathSegment {
    pub fn kind(&self) -> Option<PathSegmentKind> {
        match self.syntax().first_child_or_token() {
            Some(node) => match node.kind() {
                SK::IngotKw => Some(PathSegmentKind::Ingot(node.into_token().unwrap())),
                SK::SuperKw => Some(PathSegmentKind::Super(node.into_token().unwrap())),
                SK::SelfTypeKw => Some(PathSegmentKind::SelfTy(node.into_token().unwrap())),
                SK::SelfKw => Some(PathSegmentKind::Self_(node.into_token().unwrap())),
                SK::Ident => Some(PathSegmentKind::Ident(node.into_token().unwrap())),
                _ => None,
            },
            _ => None,
        }
    }
    /// Returns the identifier of the segment.
    pub fn ident(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns `true` if the segment  is a `self` keyword.
    pub fn is_self(&self) -> bool {
        support::token(self.syntax(), SK::SelfKw).is_some()
    }

    /// Returns `true` if the segment is a `Self` keyword.
    pub fn is_self_ty(&self) -> bool {
        support::token(self.syntax(), SK::SelfTypeKw).is_some()
    }
}

/// A path segment kind.
pub enum PathSegmentKind {
    /// `ingot`
    Ingot(SyntaxToken),
    /// `super`
    Super(SyntaxToken),
    /// `Self`
    SelfTy(SyntaxToken),
    /// `self`
    Self_(SyntaxToken),
    /// `foo`
    Ident(SyntaxToken),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lexer::Lexer,
        parser::{path::PathScope, Parser},
    };

    use wasm_bindgen_test::wasm_bindgen_test;

    fn parse_path(source: &str) -> Path {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        parser.parse(PathScope::default()).unwrap();
        Path::cast(parser.finish_to_node().0).unwrap()
    }

    #[test]
    #[wasm_bindgen_test]
    fn path_ast() {
        let source = r#"self::Foo"#;
        let path = parse_path(source);
        let mut segments = path.segments();

        assert!(segments.next().unwrap().is_self());
        assert_eq!(segments.next().unwrap().ident().unwrap().text(), "Foo");
        assert!(segments.next().is_none());
    }

    #[test]
    #[wasm_bindgen_test]
    fn path_ast2() {
        let source = r#"Self::Dep"#;
        let path = parse_path(source);
        let mut segments = path.segments();

        assert!(segments.next().unwrap().is_self_ty());
        assert_eq!(segments.next().unwrap().ident().unwrap().text(), "Dep");
        assert!(segments.next().is_none());
    }
}
