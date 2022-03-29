use crate::context::{AnalyzerContext, BindingMutability};
use crate::errors::FatalError;
use crate::namespace::scopes::BlockScope;
use crate::namespace::types::Type;
use crate::traversal::{const_expr, expressions, types};
use fe_common::{diagnostics::Label, utils::humanize::pluralize_conditionally};
use fe_parser::ast as fe;
use fe_parser::node::Node;

/// Gather context information for var declarations and check for type errors.
pub fn var_decl(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    let (mut_, target, typ, value) = match &stmt.kind {
        fe::FuncStmt::VarDecl {
            mut_,
            target,
            typ,
            value,
        } => (mut_, target, typ, value),
        _ => unreachable!(),
    };
    let declared_type = types::type_desc(scope, typ)?;
    if let Type::Map(_) = declared_type {
        return Err(FatalError::new(scope.error(
            "invalid variable type",
            typ.span,
            "`Map` type can only be used as a contract field",
        )));
    }

    if let Some(value) = value {
        let value_attr = expressions::assignable_expr(scope, value, Some(&declared_type))?;

        if declared_type != value_attr.typ {
            scope.type_error("type mismatch", value.span, &declared_type, &value_attr.typ);
        } else if mut_.is_some() && !value_attr.mutable && !value_attr.typ.is_base() {
            scope.fancy_error(
                "mutability mismatch",
                vec![
                    Label::primary(target.span, "this is defined to be mutable"),
                    Label::secondary(value.span, "this value is not mutable"),
                ],
                vec![],
            );
        }
    }
    let mutability = match mut_ {
        Some(_) => BindingMutability::Mutable,
        None => BindingMutability::Immutable,
    };

    add_var(scope, target, declared_type, mutability)?;
    Ok(())
}

pub fn const_decl(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::ConstantDecl { name, typ, value } = &stmt.kind {
        let declared_type = match types::type_desc(scope, typ) {
            Ok(typ) if typ.has_fixed_size() => typ,
            _ => {
                // If this conversion fails, the type must be a map (for now at least)
                return Err(FatalError::new(scope.error(
                    "invalid constant type",
                    typ.span,
                    "`Map` type can only be used as a contract field",
                )));
            }
        };

        // Perform semantic analysis before const evaluation.
        let value_attr = expressions::assignable_expr(scope, value, Some(&declared_type))?;

        if declared_type != value_attr.typ {
            scope.type_error("type mismatch", value.span, &declared_type, &value_attr.typ);
        }

        // Perform constant evaluation.
        let const_value = const_expr::eval_expr(scope, value)?;

        scope.root.map_variable_type(name, declared_type.clone());
        // this logs a message on err, so it's safe to ignore here.
        let _ = scope.add_var(
            name.kind.as_str(),
            declared_type,
            BindingMutability::Const,
            name.span,
        );
        scope.add_constant(name, value, const_value);
        return Ok(());
    }

    unreachable!()
}

/// Add declared variables to the scope.
fn add_var(
    scope: &mut BlockScope,
    target: &Node<fe::VarDeclTarget>,
    typ: Type,
    mutability: BindingMutability,
) -> Result<(), FatalError> {
    match &target.kind {
        fe::VarDeclTarget::Name(name) => {
            scope.root.map_variable_type(target, typ.clone());
            // this logs a message on err, so it's safe to ignore here.
            let _ = scope.add_var(name, typ, mutability, target.span);
            Ok(())
        }
        fe::VarDeclTarget::Tuple(items) => {
            match typ {
                Type::Tuple(items_ty) => {
                    let items_ty = items_ty.items;
                    let items_ty_len = items_ty.as_vec().len();
                    if items.len() != items_ty_len {
                        return Err(FatalError::new(scope.fancy_error(
                            "invalid declaration",
                            vec![Label::primary(target.span, "")],
                            vec![format!(
                                "Tuple declaration has {} {} but the specified tuple type has {} {}",
                                items.len(),
                                pluralize_conditionally("item", items.len()),
                                items_ty_len,
                                pluralize_conditionally("item", items_ty_len),
                            )],
                        )));
                    }
                    for (item, item_ty) in items.iter().zip(items_ty.into_iter()) {
                        add_var(scope, item, item_ty, mutability)?;
                    }
                    Ok(())
                }
                _ => Err(FatalError::new(scope.fancy_error("invalid declaration",
                                                           vec![Label::primary(target.span, "")],
                                                           vec![format!("Tuple declaration targets need to be declared with the tuple type but here the type is {}", typ)])))
            }
        }
    }
}
