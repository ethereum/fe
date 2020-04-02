use crate::errors::CompileError;
use crate::yul::mappers::{expressions};
use crate::yul::namespace::scopes::{ContractDef, FunctionDef, FunctionScope, Shared};
use crate::yul::namespace::types::FixedSize;
use std::rc::Rc;
use vyper_parser::ast as vyp;
use vyper_parser::span::Spanned;
use yultsur::*;

/// Builds a Yul statement from a Vyper assignment.
pub fn assign(
    scope: Shared<FunctionScope>,
    targets: &Vec<Spanned<vyp::Expr>>,
    value: &vyp::Expr,
) -> Result<yul::Statement, CompileError> {
    let targets = targets.iter().map(|t| &t.node).collect::<Vec<&vyp::Expr>>();

    match targets.first() {
        Some(vyp::Expr::Name(name)) => assign_name(scope, name.to_string(), value),
        Some(vyp::Expr::Subscript {
            value: target,
            slices,
        }) => assign_subscript(scope, &target.node, &slices.node, value),
        _ => Err(CompileError::static_str("Targets not supported")),
    }
}

fn assign_subscript(
    scope: Shared<FunctionScope>,
    target: &vyp::Expr,
    slices: &Vec<Spanned<vyp::Slice>>,
    value: &vyp::Expr,
) -> Result<yul::Statement, CompileError> {
    match target {
        vyp::Expr::Name(name) => assign_subscript_name(scope, name.to_string(), slices, value),
        vyp::Expr::Attribute {
            value: target_value,
            attr,
        } => assign_subscript_attribute(
            scope,
            &target_value.node,
            attr.node.to_string(),
            slices,
            value,
        ),
        _ => Err(CompileError::static_str("Invalid subscript target")),
    }
}

fn assign_subscript_name(
    scope: Shared<FunctionScope>,
    name: String,
    slices: &Vec<Spanned<vyp::Slice>>,
    value: &vyp::Expr,
) -> Result<yul::Statement, CompileError> {
    let (key, _) = expressions::slices_index(Rc::clone(&scope), slices)?;
    let (value, _) = expressions::expr(Rc::clone(&scope), value)?;

    match scope.borrow().def(name.clone()) {
        Some(FunctionDef::Array(array)) => array.mstore_elem(name, key, value),
        None => Err(CompileError::static_str("No definition found")),
        _ => Err(CompileError::static_str("Invalid definition")),
    }
}

fn assign_subscript_attribute(
    scope: Shared<FunctionScope>,
    target_value: &vyp::Expr,
    name: String,
    slices: &Vec<Spanned<vyp::Slice>>,
    value: &vyp::Expr,
) -> Result<yul::Statement, CompileError> {
    match expressions::expr_name_str(target_value)? {
        "self" => assign_subscript_self(scope, name, slices, value),
        _ => Err(CompileError::static_str("Invalid attribute value")),
    }
}

fn assign_subscript_self(
    scope: Shared<FunctionScope>,
    name: String,
    slices: &Vec<Spanned<vyp::Slice>>,
    value: &vyp::Expr,
) -> Result<yul::Statement, CompileError> {
    let (key, _) = expressions::slices_index(Rc::clone(&scope), slices)?;
    let (value, _) = expressions::expr(Rc::clone(&scope), value)?;

    match scope.borrow().contract_def(name) {
        Some(ContractDef::Map { index, map }) => map.sstore(index, key, value),
        None => Err(CompileError::static_str("No definition found")),
        _ => Err(CompileError::static_str("Invalid definition"))
    }
}

fn assign_name(
    scope: Shared<FunctionScope>,
    name: String,
    value: &vyp::Expr,
) -> Result<yul::Statement, CompileError> {
    let identifier = identifier! {(name)};
    let (value, value_type) = expressions::expr(Rc::clone(&scope), value)?;

    match value_type {
        FixedSize::Array(array) => scope.borrow_mut().add_array(name, array),
        FixedSize::Base(base) => scope.borrow_mut().add_base(name, base),
    };

    Ok(statement! { [identifier] := [value] })
}

#[cfg(test)]
mod tests {
    use crate::yul::mappers::assignments::assign;
    use crate::yul::namespace::scopes::{ContractScope, FunctionScope, ModuleScope, Shared};
    use crate::yul::namespace::types::{Array, Base};
    
    use vyper_parser as parser;
    use vyper_parser::ast as vyp;

    fn scope() -> Shared<FunctionScope> {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        FunctionScope::new(contract_scope)
    }

    fn map(scope: Shared<FunctionScope>, src: &str) -> String {
        let tokens = parser::get_parse_tokens(src).expect("Couldn't parse assignment");
        let stmt = &parser::parsers::assign_stmt(&tokens[..])
            .expect("Couldn't build assignment AST")
            .1
            .node;

        if let vyp::FuncStmt::Assign { targets, value } = stmt {
            let assign = assign(scope, targets, &value.node).expect("Couldn't map assignment AST");

            assign.to_string()
        } else {
            panic!("Didn't get an assignment")
        }
    }

    #[test]
    fn assign_u256() {
        let scope = scope();
        scope.borrow_mut().add_base("bar".to_string(), Base::U256);

        assert_eq!(map(scope, "foo = bar"), "foo := bar")
    }

    #[test]
    fn assign_subscript_u256() {
        let scope = scope();
        scope.borrow_mut().add_array(
            "foo".to_string(),
            Array {
                dimension: 10,
                inner: Base::U256,
            },
        );

        assert_eq!(
            map(scope, "foo[4] = 2"),
            "mstoren(add(foo, mul(4, 32)), 2, 32)"
        )
    }
}
