use crate::context::{Context, Location};
use crate::errors::SemanticError;
use crate::namespace::scopes::{BlockScope, Shared};
use crate::operations;
use crate::traversal::expressions;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::rc::Rc;

/// Gather context information for assignments and check for type errors.
///
/// e.g. `foo[42] = "bar"`, `self.foo[42] = "bar"`, `foo = 42`
pub fn assign(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Assign { target, value } = &stmt.kind {
        let target_attributes = expressions::expr(Rc::clone(&scope), Rc::clone(&context), target)?;
        let value_attributes = expressions::expr(Rc::clone(&scope), Rc::clone(&context), value)?;

        if target_attributes.typ != value_attributes.typ {
            return Err(SemanticError::type_error());
        }

        if matches!(
            (
                value_attributes.final_location(),
                target_attributes.location
            ),
            (Location::Storage { .. }, Location::Memory)
        ) {
            return Err(SemanticError::cannot_move());
        }

        return Ok(());
    }

    unreachable!()
}

/// Gather context information for assignments and check for type errors.
pub fn aug_assign(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::AugAssign { target, op, value } = &stmt.kind {
        let target_attributes = expressions::expr(Rc::clone(&scope), Rc::clone(&context), target)?;
        let value_attributes = expressions::expr(scope, context, value)?;

        operations::bin(&target_attributes.typ, &op.kind, &value_attributes.typ)?;
        return Ok(());
    }

    unreachable!()
}
