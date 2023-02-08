pub mod item;
pub mod param;
pub mod path;
pub mod type_;

pub use item::*;
pub use param::*;
pub use path::*;
pub use type_::*;

pub type AstChildren<T> = rowan::ast::AstChildren<T>;
pub type SyntaxText = rowan::SyntaxText;

macro_rules! ast_node {
    (
        $(#[$attrs: meta])*
        $visibility: vis struct $name: ident $({
             $($field_vis: vis $field: ident: $ty: ty),*
        })?,
        $kind: pat
    ) => {
        $(#[$attrs])*
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        $visibility struct $name {
            __syntax: crate::SyntaxNode,
            $($($field: $ty),*)?
        }

        impl rowan::ast::AstNode for $name {
            type Language = $crate::FeLang;
            fn can_cast(node: crate::SyntaxKind) -> bool {
                matches!(node, $kind)
            }
            fn cast(node: crate::SyntaxNode) -> Option<Self> {
                Self::can_cast(node.kind()).then(|| Self{
                    __syntax: node.into(),
                    $($($field: Default::default(),)*)?
                })
            }
            fn syntax(&self) -> &crate::SyntaxNode {
                &self.__syntax
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(rowan::ast::AstNode::syntax(self), f)
            }
        }
    };
}

use ast_node;
