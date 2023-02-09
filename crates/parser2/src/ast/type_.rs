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

    pub fn generic_args(&self) -> Option<super::GenericArgList> {
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
pub enum TypeKind {
    Ptr(PtrType),
    Path(PathType),
    SelfType(SelfType),
    Tuple(TupleType),
    Array(ArrayType),
}
