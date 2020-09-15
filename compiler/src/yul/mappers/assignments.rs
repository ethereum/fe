use crate::errors::CompileError;
use crate::yul::mappers::expressions;
use crate::yul::namespace::scopes::{
    ContractDef,
    FunctionDef,
    FunctionScope,
    Shared,
};
use std::rc::Rc;
use vyper_parser::ast as vyp;
use vyper_parser::span::Spanned;
use yultsur::*;

/// Builds a Yul statement from a Vyper assignment.
pub fn assign(
    scope: Shared<FunctionScope>,
    stmt: &Spanned<vyp::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::FuncStmt::Assign { targets, value } = &stmt.node {
        if targets.len() > 1 {
            unimplemented!("multiple assignment targets")
        }

        if let Some(first_target) = targets.first() {
            return match &first_target.node {
                vyp::Expr::Name(_) => assign_name(scope, first_target, value),
                vyp::Expr::Subscript { .. } => assign_subscript(scope, first_target, value),
                _ => unreachable!(),
            };
        }
    }

    unreachable!()
}

fn assign_subscript(
    scope: Shared<FunctionScope>,
    target: &Spanned<vyp::Expr>,
    value: &Spanned<vyp::Expr>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::Expr::Subscript {
        value: target_value,
        slices,
    } = &target.node
    {
        return match &target_value.node {
            vyp::Expr::Name(_) => assign_subscript_name(scope, target_value, slices, value),
            vyp::Expr::Attribute { .. } => {
                assign_subscript_attribute(scope, target_value, slices, value)
            }
            _ => Err(CompileError::static_str("invalid subscript target")),
        };
    }

    unreachable!()
}

fn assign_subscript_name(
    scope: Shared<FunctionScope>,
    target_value: &Spanned<vyp::Expr>,
    slices: &Spanned<Vec<Spanned<vyp::Slice>>>,
    value: &Spanned<vyp::Expr>,
) -> Result<yul::Statement, CompileError> {
    let name = expressions::expr_name_string(target_value)?;

    let array_ptr = identifier_expression! {(name)};
    let index = expressions::slices_index(Rc::clone(&scope), slices)?.expression;
    let value = expressions::expr(Rc::clone(&scope), value)?.expression;

    match scope.borrow().def(name) {
        Some(FunctionDef::Array(array_type)) => array_type.mstore_elem(array_ptr, index, value),
        Some(FunctionDef::Base(_)) => Err(CompileError::static_str(
            "can't assign subscript value to base type",
        )),
        None => Err(CompileError::static_str("definition not found")),
    }
}

fn assign_subscript_attribute(
    scope: Shared<FunctionScope>,
    target_value: &Spanned<vyp::Expr>,
    slices: &Spanned<Vec<Spanned<vyp::Slice>>>,
    value: &Spanned<vyp::Expr>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::Expr::Attribute {
        value: target_value,
        attr,
    } = &target_value.node
    {
        return match expressions::expr_name_str(target_value)? {
            "self" => assign_subscript_self(scope, attr, slices, value),
            _ => Err(CompileError::static_str("unrecognized attribute value")),
        };
    }

    unreachable!()
}

fn assign_subscript_self(
    scope: Shared<FunctionScope>,
    attr: &Spanned<&str>,
    slices: &Spanned<Vec<Spanned<vyp::Slice>>>,
    value: &Spanned<vyp::Expr>,
) -> Result<yul::Statement, CompileError> {
    let name = attr.node.to_string();

    let key = expressions::slices_index(Rc::clone(&scope), slices)?.expression;
    let value = expressions::expr(Rc::clone(&scope), value)?.expression;

    match scope.borrow().contract_def(name) {
        Some(ContractDef::Map { index, map }) => map.sstore(index, key, value),
        None => Err(CompileError::static_str("definition not found")),
        _ => Err(CompileError::static_str("invalid definition")),
    }
}

fn assign_name(
    scope: Shared<FunctionScope>,
    target: &Spanned<vyp::Expr>,
    value: &Spanned<vyp::Expr>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::Expr::Name(name) = target.node {
        let identifier = identifier! {(name)};
        let value = expressions::expr(scope, value)?.expression;

        return Ok(statement! { [identifier] := [value] });
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use crate::yul::mappers::assignments::assign;
    use crate::yul::namespace::scopes::{
        ContractScope,
        FunctionScope,
        ModuleScope,
        Shared,
    };
    use crate::yul::namespace::types::{
        Array,
        Base,
    };

    use vyper_parser as parser;

    fn scope() -> Shared<FunctionScope> {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        FunctionScope::new(contract_scope)
    }

    fn map(scope: Shared<FunctionScope>, src: &str) -> String {
        let tokens = parser::get_parse_tokens(src).expect("couldn't parse assignment");
        let stmt = parser::parsers::assign_stmt(&tokens[..])
            .expect("couldn't build assignment AST")
            .1;

        let assign = assign(scope, &stmt).expect("couldn't map assignment AST");
        assign.to_string()
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
