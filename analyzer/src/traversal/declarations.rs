use crate::errors::SemanticError;
use crate::namespace::scopes::{BlockScope, Scope, Shared};
use crate::traversal::{expressions, types};
use crate::Context;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::rc::Rc;

/// Gather context information for var declarations and check for type errors.
pub fn var_decl(
    scope: Shared<BlockScope>,
    context: &mut Context,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::VarDecl { target, typ, value } = &stmt.kind {
        let name = match &target.kind {
            fe::VarDeclTarget::Name(name) => name,
            fe::VarDeclTarget::Tuple(_) => todo!("tuple destructuring variable declaration"),
        };
        let declared_type =
            types::type_desc_fixed_size(&Scope::Block(Rc::clone(&scope)), context, &typ)?;

        if let Some(value) = value {
            let value_attributes = expressions::assignable_expr(
                Rc::clone(&scope),
                context,
                value,
                Some(&declared_type.clone().into()),
            )?;

            if declared_type != value_attributes.typ {
                context.type_error(
                    "type mismatch",
                    value.span,
                    &declared_type,
                    value_attributes.typ,
                );
            }
        }

        scope.borrow_mut().add_var(&name, declared_type.clone())?;
        context.add_declaration(stmt, declared_type);

        return Ok(());
    }

    unreachable!()
}
