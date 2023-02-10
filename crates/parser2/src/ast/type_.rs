use rowan::ast::{support, AstNode};

use super::{ast_node, AstChildren};
use crate::{SyntaxKind as SK, SyntaxToken};

ast_node! {
    /// A type node.
    /// If you want to match a specific kind of type, use `[Type::kind]`.
    pub struct Type,
    SK::PtrType
    | SK::PathType
    | SK::SelfType
    | SK::TupleType
    | SK::ArrayType
}
impl Type {
    pub fn kind(&self) -> TypeKind {
        match self.syntax().kind() {
            SK::PtrType => TypeKind::Ptr(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::PathType => TypeKind::Path(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::SelfType => TypeKind::SelfType(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::TupleType => TypeKind::Tuple(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::ArrayType => TypeKind::Array(AstNode::cast(self.syntax().clone()).unwrap()),
            _ => unreachable!(),
        }
    }
}

ast_node! {
    /// A pointer type.
    /// `*i32`
    pub struct PtrType,
    SK::PtrType,
}
impl PtrType {
    /// Returns the `*` token.
    pub fn star(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Star)
    }

    /// Returns the type pointed to.
    pub fn inner(&self) -> Option<Type> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// A path type.
    /// `foo::Type<T, U + 2>`
    pub struct PathType,
    SK::PathType
}
impl PathType {
    /// Returns the path of the type.
    pub fn path(&self) -> Option<super::Path> {
        support::child(self.syntax())
    }
}
impl super::GenericArgsOwner for PathType {}

ast_node! {
    /// A self type.
    /// `Self`
    pub struct SelfType,
    SK::SelfType,
}
impl SelfType {
    /// Returns the `Self` keyword.
    pub fn self_kw(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::SelfTypeKw)
    }
}

ast_node! {
    /// A tuple type.
    /// `(i32, foo::Bar)`
    pub struct TupleType,
    SK::TupleType,
    IntoIterator<Item=Type>,
}
impl TupleType {
    /// Returns the types in the tuple.
    pub fn elem_tys(&self) -> AstChildren<Type> {
        support::children(self.syntax())
    }
}

ast_node! {
    /// An array type.
    /// `[i32; 4]`
    pub struct ArrayType,
    SK::ArrayType,
}
impl ArrayType {
    /// Returns the type of the array elements.
    pub fn elem_ty(&self) -> Option<Type> {
        support::child(self.syntax())
    }

    /// Returns the length of the array.
    pub fn len(&self) -> Option<super::Expr> {
        support::child(self.syntax())
    }
}

/// A specific kind of type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Ptr(PtrType),
    Path(PathType),
    SelfType(SelfType),
    Tuple(TupleType),
    Array(ArrayType),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::prelude::*;
    use crate::{lexer::Lexer, parser};

    fn parse_type(source: &str) -> Type {
        let lexer = Lexer::new(source);
        let mut parser = parser::Parser::new(lexer);
        parser::type_::parse_type(&mut parser, None, true);
        Type::cast(parser.finish().0).unwrap()
    }

    #[test]
    fn ptr_type() {
        let ty = parse_type("*i32");
        let ptr_ty = match ty.kind() {
            TypeKind::Ptr(ptr_ty) => ptr_ty,
            _ => panic!(),
        };

        assert_eq!(ptr_ty.star().unwrap().text(), "*");
        assert!(matches!(ptr_ty.inner().unwrap().kind(), TypeKind::Path(_)));
    }

    #[test]
    fn path_type() {
        let ty = parse_type("Foo::Bar<T, {U + 2}>");
        let path_type = match ty.kind() {
            TypeKind::Path(ptr_ty) => ptr_ty,
            _ => panic!(),
        };

        for (i, segment) in path_type.path().unwrap().segments().enumerate() {
            match i {
                0 => assert_eq!(segment.ident().unwrap().text(), "Foo"),
                1 => assert_eq!(segment.ident().unwrap().text(), "Bar"),
                _ => panic!(),
            }
        }

        let generic_args = path_type.generic_args().unwrap();
        for (i, arg) in generic_args.iter().enumerate() {
            match i {
                0 => assert!(matches!(arg.kind(), crate::ast::GenericArgKind::Type(_))),
                1 => assert!(matches!(arg.kind(), crate::ast::GenericArgKind::Const(_))),
                _ => panic!(),
            }
        }
    }

    #[test]
    fn self_type() {
        let ty = parse_type("Self");
        assert!(matches!(ty.kind(), TypeKind::SelfType(_)));
    }

    #[test]
    fn tuple_type() {
        let ty = parse_type("((i32, u32), foo::Bar, *usize");
        let tuple_type = match ty.kind() {
            TypeKind::Tuple(tuple_type) => tuple_type,
            _ => panic!(),
        };

        for (i, ty) in tuple_type.elem_tys().enumerate() {
            match i {
                0 => assert!(matches!(ty.kind(), TypeKind::Tuple(_))),
                1 => assert!(matches!(ty.kind(), TypeKind::Path(_))),
                2 => assert!(matches!(ty.kind(), TypeKind::Ptr(_))),
                _ => panic!(),
            }
        }
    }

    #[test]
    fn array_type() {
        let ty = parse_type("[(i32, u32); 1]");
        let array_type = match ty.kind() {
            TypeKind::Array(array_type) => array_type,
            _ => panic!(),
        };

        assert!(matches!(
            array_type.elem_ty().unwrap().kind(),
            TypeKind::Tuple(_)
        ));
        assert!(array_type.len().is_some());
    }
}
