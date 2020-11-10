use crate::errors::SemanticError;
use crate::namespace::scopes::{
    BlockScope,
    ContractDef,
    ContractScope,
    Scope,
    Shared,
};
use crate::namespace::types::{
    Base,
    FixedSize,
    Tuple,
    Type,
};
use crate::traversal::_utils::spanned_expression;
use crate::traversal::{
    assignments,
    declarations,
    expressions,
    types,
};
use crate::{
    Context,
    FunctionAttributes,
};
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use std::rc::Rc;

/// Gather context information for a function definition and check for type
/// errors.
pub fn func_def(
    contract_scope: Shared<ContractScope>,
    context: Shared<Context>,
    def: &Spanned<fe::ContractStmt>,
) -> Result<(), SemanticError> {
    if let fe::ContractStmt::FuncDef {
        qual,
        name,
        args,
        return_type,
        body,
    } = &def.node
    {
        let function_scope = BlockScope::from_contract_scope(def.span, Rc::clone(&contract_scope));

        let name = name.node.to_string();
        let param_types = args
            .iter()
            .map(|arg| func_def_arg(Rc::clone(&function_scope), arg))
            .collect::<Result<Vec<_>, _>>()?;

        let return_type = return_type
            .as_ref()
            .map(|typ| types::type_desc_fixed_size(Scope::Block(Rc::clone(&function_scope)), &typ))
            .transpose()?
            .unwrap_or_else(|| Tuple::empty().to_fixed_size());

        // If the return type is an empty tuple we do not have to validate any further
        // at this point because both returning (explicit) or not returning (implicit
        // return) are valid syntax.
        // If the return type is anything else, we do need to ensure that all code paths
        // return or revert.
        if !return_type.is_empty_tuple() {
            validate_all_paths_return_or_revert(&body)?
        }

        let is_public = qual.is_some();
        contract_scope.borrow_mut().add_function(
            name.clone(),
            is_public,
            param_types.clone(),
            return_type.clone(),
        );

        let attributes = FunctionAttributes {
            name,
            param_types,
            return_type,
        };

        context.borrow_mut().add_function(def, attributes);

        traverse_statements(function_scope, context, body)?;

        return Ok(());
    }

    unreachable!()
}

fn traverse_statements(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    body: &[Spanned<fe::FuncStmt>],
) -> Result<(), SemanticError> {
    for stmt in body.iter() {
        func_stmt(Rc::clone(&scope), Rc::clone(&context), stmt)?
    }
    Ok(())
}

fn validate_all_paths_return_or_revert(
    block: &[Spanned<fe::FuncStmt>],
) -> Result<(), SemanticError> {
    for statement in block.iter().rev() {
        if let fe::FuncStmt::Return { .. } = &statement.node {
            return Ok(());
        }

        if let fe::FuncStmt::Revert { .. } = &statement.node {
            return Ok(());
        }

        if let fe::FuncStmt::If {
            test: _,
            body,
            or_else,
        } = &statement.node
        {
            let body_returns = validate_all_paths_return_or_revert(body).is_ok();
            let or_else_returns =
                or_else.is_empty() || validate_all_paths_return_or_revert(or_else).is_ok();
            if body_returns && or_else_returns {
                return Ok(());
            }
        }
    }

    Err(SemanticError::MissingReturn)
}

fn func_def_arg(
    scope: Shared<BlockScope>,
    arg: &Spanned<fe::FuncDefArg>,
) -> Result<FixedSize, SemanticError> {
    let name = arg.node.name.node.to_string();
    let typ = types::type_desc_fixed_size(Scope::Block(Rc::clone(&scope)), &arg.node.typ)?;

    scope.borrow_mut().add_var(name, typ.clone().into_type());

    Ok(typ)
}

fn func_stmt(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    match &stmt.node {
        fe::FuncStmt::Return { .. } => func_return(scope, context, stmt),
        fe::FuncStmt::VarDecl { .. } => declarations::var_decl(scope, context, stmt),
        fe::FuncStmt::Assign { .. } => assignments::assign(scope, context, stmt),
        fe::FuncStmt::Emit { .. } => emit(scope, context, stmt),
        fe::FuncStmt::AugAssign { .. } => unimplemented!(),
        fe::FuncStmt::For { .. } => unimplemented!(),
        fe::FuncStmt::While { .. } => while_loop(scope, context, stmt),
        fe::FuncStmt::If { .. } => if_statement(scope, context, stmt),
        fe::FuncStmt::Assert { .. } => assert(scope, context, stmt),
        fe::FuncStmt::Expr { .. } => expr(scope, context, stmt),
        fe::FuncStmt::Pass => unimplemented!(),
        fe::FuncStmt::Break => unimplemented!(),
        fe::FuncStmt::Continue => unimplemented!(),
        fe::FuncStmt::Revert => Ok(()),
    }
}

fn verify_is_boolean(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    expr: &Spanned<fe::Expr>,
) -> Result<(), SemanticError> {
    let attributes = expressions::expr(scope, context, expr)?;
    if let Type::Base(Base::Bool) = attributes.typ {
        return Ok(());
    }

    Err(SemanticError::TypeError)
}

fn if_statement(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    match &stmt.node {
        fe::FuncStmt::If {
            test,
            body,
            or_else,
        } => {
            let body_scope = BlockScope::from_block_scope(stmt.span, scope.clone());
            traverse_statements(body_scope, context.clone(), body)?;
            let or_else_scope = BlockScope::from_block_scope(stmt.span, scope.clone());
            traverse_statements(or_else_scope, context.clone(), or_else)?;
            verify_is_boolean(scope, context, test)
        }
        _ => unreachable!(),
    }
}

fn while_loop(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    match &stmt.node {
        fe::FuncStmt::While {
            test,
            body,
            or_else,
        } => {
            if !or_else.is_empty() {
                unimplemented!();
            }
            let body_scope = BlockScope::from_block_scope(stmt.span, scope.clone());
            traverse_statements(body_scope, context.clone(), body)?;
            verify_is_boolean(scope, context, test)
        }
        _ => unreachable!(),
    }
}

fn expr(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Expr { value } = &stmt.node {
        let spanned = spanned_expression(&stmt.span, value);
        let _attributes = expressions::expr(scope, context, &spanned)?;
    }

    Ok(())
}

fn emit(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Emit {
        value: Spanned {
            node: fe::Expr::Call { func, args },
            ..
        },
    } = &stmt.node
    {
        let event_name = expressions::expr_name_string(func)?;

        if let Some(ContractDef::Event(event)) = scope.borrow().contract_def(event_name) {
            context.borrow_mut().add_emit(stmt, event);
        }

        for arg in args.node.iter() {
            call_arg(Rc::clone(&scope), Rc::clone(&context), arg)?;
        }

        return Ok(());
    }

    unreachable!()
}

fn assert(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Assert { test, msg } = &stmt.node {
        verify_is_boolean(scope.clone(), context.clone(), test)?;
        if let Some(msg) = msg {
            // TODO: type check for a string once strings are supported
            let _msg_attributes = expressions::expr(scope, context, msg)?;
        }

        return Ok(());
    }

    unreachable!()
}

fn call_arg(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    arg: &Spanned<fe::CallArg>,
) -> Result<(), SemanticError> {
    match &arg.node {
        fe::CallArg::Arg(value) => {
            let spanned = spanned_expression(&arg.span, value);
            let _attributes = expressions::expr(scope, context, &spanned)?;
            // TODO: Perform type checking
        }
        fe::CallArg::Kwarg(fe::Kwarg { name: _, value }) => {
            let _attributes = expressions::expr(scope, context, value)?;
            // TODO: Perform type checking
        }
    };

    Ok(())
}

fn func_return(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Return { value: Some(value) } = &stmt.node {
        let attributes = expressions::expr(scope.clone(), context.clone(), value)?;

        match context
            .borrow()
            .get_function(scope.borrow().function_scope().borrow().span)
        {
            Some(fn_attr) => {
                if fn_attr.return_type.clone().into_type() != attributes.typ {
                    return Err(SemanticError::TypeError);
                }
            }
            None => unreachable!(),
        }

        return Ok(());
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use crate::namespace::scopes::{
        ContractDef,
        ContractScope,
        ModuleScope,
        Shared,
    };
    use crate::namespace::types::{
        Base,
        FixedSize,
    };
    use crate::traversal::functions::func_def;
    use crate::Context;
    use fe_parser as parser;
    use std::rc::Rc;

    fn scope() -> Shared<ContractScope> {
        let module_scope = ModuleScope::new();
        ContractScope::new(module_scope)
    }

    fn analyze(scope: Shared<ContractScope>, src: &str) -> Context {
        let context = Context::new_shared();
        let tokens = parser::get_parse_tokens(src).expect("Couldn't parse expression");
        let def = &parser::parsers::func_def(&tokens[..])
            .expect("Couldn't build func def AST")
            .1;

        func_def(scope, Rc::clone(&context), def).expect("Couldn't map func def AST");
        Rc::try_unwrap(context)
            .map_err(|_| "")
            .unwrap()
            .into_inner()
    }

    #[test]
    fn simple_func_def() {
        let scope = scope();
        let func_def = "\
        def foo(x: u256) -> u256:\
            return x + x\
        ";
        let context = analyze(Rc::clone(&scope), func_def);
        assert_eq!(context.expressions.len(), 3);
        assert_eq!(
            scope.borrow().def("foo".to_string()),
            Some(ContractDef::Function {
                is_public: false,
                params: vec![FixedSize::Base(Base::U256)],
                returns: FixedSize::Base(Base::U256)
            })
        );
    }
}
