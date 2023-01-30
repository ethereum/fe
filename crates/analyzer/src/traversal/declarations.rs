use crate::context::AnalyzerContext;
use crate::display::Displayable;
use crate::errors::{self, FatalError, TypeCoercionError};
use crate::namespace::scopes::BlockScope;
use crate::namespace::types::{Type, TypeId};
use crate::traversal::{const_expr, expressions, types};
use fe_common::{diagnostics::Label, utils::humanize::pluralize_conditionally};
use fe_parser::ast as fe;
use fe_parser::node::Node;

/// Gather context information for var declarations and check for type errors.
pub fn var_decl(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    let (target, typ, value, mut_) = match &stmt.kind {
        fe::FuncStmt::VarDecl {
            target,
            typ,
            value,
            mut_,
        } => (target, typ, value, mut_),
        _ => unreachable!(),
    };

    let self_ty = scope
        .parent_function()
        .clone()
        .self_type(scope.db())
        .map(|val| val.as_trait_or_type());
    let declared_type = types::type_desc(scope, typ, self_ty)?;
    if let Type::Map(_) = declared_type.typ(scope.db()) {
        return Err(FatalError::new(scope.error(
            "invalid variable type",
            typ.span,
            "`Map` type can only be used as a contract field",
        )));
    }

    if let Some(value) = value {
        let rhs = expressions::expr(scope, value, Some(declared_type))?;
        let should_copy = mut_.is_some() || rhs.typ.is_mut(scope.db());
        match types::try_coerce_type(scope, Some(value), rhs.typ, declared_type, should_copy) {
            Err(TypeCoercionError::RequiresToMem) => {
                scope.add_diagnostic(errors::to_mem_error(value.span));
            }
            Err(TypeCoercionError::Incompatible) => {
                scope.type_error(
                    "type mismatch",
                    value.span,
                    declared_type,
                    rhs.typ.deref(scope.db()),
                );
            }
            Err(TypeCoercionError::SelfContractType) => {
                scope.add_diagnostic(errors::self_contract_type_error(
                    value.span,
                    &rhs.typ.display(scope.db()),
                ));
            }
            Ok(_) => {}
        }
    } else if matches!(
        declared_type.typ(scope.db()),
        Type::Array(_) | Type::Struct(_) | Type::Tuple(_)
    ) {
        scope.error(
            "uninitialized variable",
            target.span,
            &format!(
                "{} types must be initialized at declaration site",
                declared_type.kind_display_name(scope.db())
            ),
        );
    }

    if mut_.is_some() {
        add_var(scope, target, Type::Mut(declared_type).id(scope.db()))?;
    } else {
        add_var(scope, target, declared_type)?;
    }
    Ok(())
}

pub fn const_decl(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::ConstantDecl { name, typ, value } = &stmt.kind {
        let self_ty = scope
            .parent_function()
            .clone()
            .self_type(scope.db())
            .map(|val| val.as_trait_or_type());

        let declared_type = match types::type_desc(scope, typ, self_ty) {
            Ok(typ) if typ.has_fixed_size(scope.db()) => typ,
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
        let value_attributes = expressions::expr(scope, value, Some(declared_type))?;

        if declared_type != value_attributes.typ {
            scope.type_error(
                "type mismatch",
                value.span,
                declared_type,
                value_attributes.typ,
            );
        }

        // Perform constant evaluation.
        let const_value = const_expr::eval_expr(scope, value)?;

        scope.root.map_variable_type(name, declared_type);
        // this logs a message on err, so it's safe to ignore here.
        let _ = scope.add_var(name.kind.as_str(), declared_type, true, name.span);
        scope.add_constant(name, value, const_value);
        return Ok(());
    }

    unreachable!()
}

/// Add declared variables to the scope.
fn add_var(
    scope: &mut BlockScope,
    target: &Node<fe::VarDeclTarget>,
    typ: TypeId,
) -> Result<(), FatalError> {
    match &target.kind {
        fe::VarDeclTarget::Name(name) => {
            scope.root.map_variable_type(target, typ);
            // this logs a message on err, so it's safe to ignore here.
            let _ = scope.add_var(name, typ, false, target.span);
            Ok(())
        }
        fe::VarDeclTarget::Tuple(items) => {
            match typ.typ(scope.db()) {
                Type::Tuple(items_ty) => {
                    let items_ty = items_ty.items;
                    let items_ty_len = items_ty.len();
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
                    for (item, item_ty) in items.iter().zip(items_ty.iter()) {
                        add_var(scope, item, *item_ty)?;
                    }
                    Ok(())
                }
                _ => Err(FatalError::new(scope.fancy_error(
                    "invalid declaration",
                    vec![Label::primary(target.span, "")],
                    vec![format!(
                        "Tuple declaration targets need to be declared with the tuple type but here the type is {}",
                        typ.display(scope.db())
                    )]
                )))
            }
        }
    }
}
