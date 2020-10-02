use crate::errors::SemanticError;
use crate::namespace::scopes::Scope;
use crate::namespace::types;
use crate::namespace::types::{
    FixedSize,
    Type,
};
use fe_parser::ast as fe;
use fe_parser::span::Spanned;

/// Maps a type description node to an enum type.
pub fn type_desc(scope: Scope, typ: &Spanned<fe::TypeDesc>) -> Result<Type, SemanticError> {
    types::type_desc(&scope.module_scope().borrow().defs, &typ.node)
}

/// Maps a type description node to a fixed size enum type.
pub fn type_desc_fixed_size(
    scope: Scope,
    typ: &Spanned<fe::TypeDesc>,
) -> Result<FixedSize, SemanticError> {
    types::type_desc_fixed_size(&scope.module_scope().borrow().defs, &typ.node)
}
