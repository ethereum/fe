use crate::errors::CompileError;
use crate::yul::namespace::scopes::{ContractDef, FunctionDef, FunctionScope, Scope, Shared};
use crate::yul::namespace::types::{Base, FixedSize};
use std::rc::Rc;
use vyper_parser::ast as vyp;
use vyper_parser::span::Spanned;
use yultsur::*;

/// Builds a Yul expression from a Vyper expression.
pub fn expr(
    scope: Shared<FunctionScope>,
    expr: &vyp::Expr,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    match expr {
        vyp::Expr::Name(name) => expr_name(scope, name.to_string()),
        vyp::Expr::Num(num) => Ok((literal_expression! {(num)}, FixedSize::Base(Base::U256))),
        vyp::Expr::Subscript { value, slices } => expr_subscript(scope, &value.node, &slices.node),
        _ => Err(CompileError::static_str("Expression not supported")),
    }
}

/// Retrieves the &str value of a name expression.
pub fn expr_name_str<'a>(expr: &'a vyp::Expr<'a>) -> Result<&'a str, CompileError> {
    if let vyp::Expr::Name(name) = expr {
        return Ok(*name);
    }

    Err(CompileError::static_str("Not a name expression."))
}

/// Retrieves the &str value of a name expression and converts it to a String.
pub fn expr_name_string(expr: &vyp::Expr) -> Result<String, CompileError> {
    expr_name_str(expr).map(|name| name.to_string())
}

/// Builds a Yul expression from the first slice, if it is an index.
pub fn slices_index(
    scope: Shared<FunctionScope>,
    slices: &Vec<Spanned<vyp::Slice>>,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    if let Some(first_slice) = slices.first() {
        if let vyp::Slice::Index(index) = &first_slice.node {
            return Ok(expr(scope, index)?);
        }

        return Err(CompileError::static_str("First slice is not an index"));
    }

    return Err(CompileError::static_str("No slices in vector"));
}

fn expr_name(
    scope: Shared<FunctionScope>,
    name: String,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    match scope.borrow().def(name.clone()) {
        Some(FunctionDef::Base(base)) => {
            Ok((identifier_expression! {(name)}, FixedSize::Base(base)))
        }
        Some(FunctionDef::Array(array)) => {
            Ok((identifier_expression! {(name)}, FixedSize::Array(array)))
        }
        None => Err(CompileError::static_str("Function definition not found")),
    }
}

fn expr_subscript(
    scope: Shared<FunctionScope>,
    value: &vyp::Expr,
    slices: &Vec<Spanned<vyp::Slice>>,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    match value {
        vyp::Expr::Attribute { value, attr } => {
            expr_subscript_attribute(scope, &value.node, attr.node.to_string(), slices)
        }
        vyp::Expr::Name(name) => expr_subscript_name(scope, name.to_string(), slices),
        _ => Err(CompileError::static_str(
            "Subscript expression not supported",
        )),
    }
}

fn expr_subscript_name(
    scope: Shared<FunctionScope>,
    name: String,
    slices: &Vec<Spanned<vyp::Slice>>,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    let (key, _) = slices_index(Rc::clone(&scope), slices)?;

    match scope.borrow().def(name.clone()) {
        Some(FunctionDef::Array(array)) => {
            Ok((array.mload_elem(name, key)?, FixedSize::Base(array.inner)))
        }
        None => Err(CompileError::static_str("Function definition not found")),
        _ => Err(CompileError::static_str(
            "Function definition not supported",
        )),
    }
}

fn expr_subscript_attribute(
    scope: Shared<FunctionScope>,
    value: &vyp::Expr,
    name: String,
    slices: &Vec<Spanned<vyp::Slice>>,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    match expr_name_str(value)? {
        "self" => expr_subscript_self(scope, name, slices),
        _ => Err(CompileError::static_str("Unknown attribute value")),
    }
}

fn expr_subscript_self(
    scope: Shared<FunctionScope>,
    name: String,
    slices: &Vec<Spanned<vyp::Slice>>,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    let (key, _) = slices_index(Rc::clone(&scope), slices)?;

    match scope.borrow().contract_def(name) {
        Some(ContractDef::Map { index, map }) => Ok((map.sload(index, key)?, map.value)),
        _ => Err(CompileError::static_str(
            "Contract definition not supported",
        )),
    }
}

#[cfg(test)]
mod tests {
    use crate::yul::mappers::expressions::expr;
    use crate::yul::namespace::scopes::{ContractScope, FunctionScope, ModuleScope, Shared};
    use crate::yul::namespace::types::{Array, Base, FixedSize, Map};
    use std::rc::Rc;
    use vyper_parser as parser;

    fn scope() -> Shared<FunctionScope> {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        FunctionScope::new(contract_scope)
    }

    fn map(scope: Shared<FunctionScope>, src: &str) -> (String, FixedSize) {
        let tokens = parser::get_parse_tokens(src).expect("Couldn't parse expression");
        let (expr, typ) = expr(
            scope,
            &parser::parsers::expr(&tokens[..])
                .expect("Couldn't build expression AST")
                .1
                .node,
        )
        .expect("Couldn't map expression AST");
        (expr.to_string(), typ)
    }

    #[test]
    fn map_sload_u256() {
        let scope = scope();
        scope.borrow_mut().contract_scope().borrow_mut().add_map(
            "foo".to_string(),
            Map {
                key: FixedSize::Base(Base::Address),
                value: FixedSize::Base(Base::U256),
            },
        );

        assert_eq!(
            map(scope, "self.foo[3]"),
            (
                "sloadn(dualkeccak256(0, 3), 32)".to_string(),
                FixedSize::Base(Base::U256)
            )
        )
    }

    #[test]
    fn map_sload_array_and_address() {
        let scope = scope();
        scope.borrow_mut().contract_scope().borrow_mut().add_map(
            "foo".to_string(),
            Map {
                key: FixedSize::Base(Base::Address),
                value: FixedSize::Array(Array {
                    dimension: 5,
                    inner: Base::Address,
                }),
            },
        );

        scope.borrow_mut().contract_scope().borrow_mut().add_map(
            "bar".to_string(),
            Map {
                key: FixedSize::Base(Base::U256),
                value: FixedSize::Base(Base::Address),
            },
        );

        assert_eq!(
            map(Rc::clone(&scope), "self.foo[42]"),
            (
                "scopy(dualkeccak256(0, 42), 100)".to_string(),
                FixedSize::Array(Array {
                    dimension: 5,
                    inner: Base::Address
                })
            )
        );

        assert_eq!(
            map(scope, "self.bar[2]"),
            (
                "sloadn(dualkeccak256(1, 2), 20)".to_string(),
                FixedSize::Base(Base::Address)
            )
        )
    }

    #[test]
    fn map_sload_w_array_elem() {
        let scope = scope();
        scope.borrow_mut().contract_scope().borrow_mut().add_map(
            "foo_map".to_string(),
            Map {
                key: FixedSize::Base(Base::Byte),
                value: FixedSize::Array(Array {
                    dimension: 8,
                    inner: Base::Address,
                }),
            },
        );

        scope.borrow_mut().add_array(
            "bar_array".to_string(),
            Array {
                dimension: 100,
                inner: Base::Byte,
            },
        );

        scope.borrow_mut().add_base("index".to_string(), Base::U256);

        assert_eq!(
            map(Rc::clone(&scope), "self.foo_map[bar_array[index]]"),
            (
                "scopy(dualkeccak256(0, mloadn(add(bar_array, mul(index, 1)), 1)), 160)"
                    .to_string(),
                FixedSize::Array(Array {
                    dimension: 8,
                    inner: Base::Address
                })
            )
        );
    }
}
