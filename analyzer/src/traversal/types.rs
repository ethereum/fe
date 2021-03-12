use crate::errors::SemanticError;
use crate::namespace::scopes::Scope;
use crate::namespace::types;
use crate::namespace::types::{
    FixedSize,
    Type,
};
use fe_parser::ast as fe;
use fe_parser::node::Node;

/// Maps a type description node to an enum type.
pub fn type_desc(scope: Scope, typ: &Node<fe::TypeDesc>) -> Result<Type, SemanticError> {
    types::type_desc(&scope.module_scope().borrow().type_defs, &typ.kind)
}

/// Maps a type description node to a fixed size enum type.
pub fn type_desc_fixed_size(
    scope: Scope,
    typ: &Node<fe::TypeDesc>,
) -> Result<FixedSize, SemanticError> {
    types::type_desc_fixed_size(&scope.module_scope().borrow().type_defs, &typ.kind)
}
