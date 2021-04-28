use crate::errors::SemanticError;
use crate::namespace::scopes::{Scope, Shared};
use crate::namespace::types;
use crate::namespace::types::{FixedSize, Type};
use crate::Context;
use fe_parser::ast as fe;
use fe_parser::node::Node;

/// Maps a type description node to an enum type.
pub fn type_desc(
    scope: Scope,
    context: Shared<Context>,
    desc: &Node<fe::TypeDesc>,
) -> Result<Type, SemanticError> {
    let typ = types::type_desc(&scope.module_scope().borrow().type_defs, &desc.kind)?;
    context.borrow_mut().add_type_desc(desc, typ.clone());

    if let Type::Tuple(tuple) = &typ {
        scope
            .module_scope()
            .borrow_mut()
            .tuples_used
            .insert(tuple.to_owned());
    }

    Ok(typ)
}

/// Maps a type description node to a fixed size enum type.
pub fn type_desc_fixed_size(
    scope: Scope,
    context: Shared<Context>,
    desc: &Node<fe::TypeDesc>,
) -> Result<FixedSize, SemanticError> {
    let typ = types::type_desc_fixed_size(&scope.module_scope().borrow().type_defs, &desc.kind)?;
    context
        .borrow_mut()
        .add_type_desc(desc, Type::from(typ.clone()));
    if let FixedSize::Tuple(tuple) = &typ {
        scope
            .module_scope()
            .borrow_mut()
            .tuples_used
            .insert(tuple.to_owned());
    }

    Ok(typ)
}
