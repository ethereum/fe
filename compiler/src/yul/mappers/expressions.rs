use crate::errors::CompileError;
use crate::yul::namespace::scopes::{
    ContractDef,
    FunctionDef,
    FunctionScope,
    Shared,
};
use crate::yul::namespace::types::{
    Base,
    FixedSize,
    Map,
    Type,
};
use std::rc::Rc;
use vyper_parser::ast as vyp;
use vyper_parser::span::{
    Span,
    Spanned,
};
use yultsur::*;

/// The location of an evaluated expression.
#[derive(Clone, Debug, PartialEq)]
pub enum Location {
    /// The expression's own value.
    Value,
    /// The expression points to some region in memory.
    Memory,
    /// The expression points to some region in storage.
    Storage,
}

/// A Yul expression extended to include location and type.
#[derive(Clone, Debug, PartialEq)]
pub struct ExtExpression {
    pub expression: yul::Expression,
    pub location: Location,
    pub typ: Type,
}

/// Builds a Yul expression from a Vyper expression.
pub fn expr(
    scope: Shared<FunctionScope>,
    exp: &Spanned<vyp::Expr>,
) -> Result<ExtExpression, CompileError> {
    match &exp.node {
        vyp::Expr::Name(_) => expr_name(scope, exp),
        vyp::Expr::Num(_) => expr_num(exp),
        vyp::Expr::Subscript { .. } => expr_subscript(scope, exp),
        vyp::Expr::Attribute { .. } => expr_attribute(scope, exp),
        vyp::Expr::Ternary { .. } => unimplemented!(),
        vyp::Expr::BoolOperation { .. } => unimplemented!(),
        vyp::Expr::BinOperation { .. } => unimplemented!(),
        vyp::Expr::UnaryOperation { .. } => unimplemented!(),
        vyp::Expr::CompOperation { .. } => unimplemented!(),
        vyp::Expr::Call { .. } => unimplemented!(),
        vyp::Expr::List { .. } => unimplemented!(),
        vyp::Expr::ListComp { .. } => unimplemented!(),
        vyp::Expr::Tuple { .. } => unimplemented!(),
        vyp::Expr::Str(_) => unimplemented!(),
        vyp::Expr::Ellipsis => unimplemented!(),
    }
}

/// Retrieves the &str value of a name expression.
pub fn expr_name_str<'a>(exp: &Spanned<vyp::Expr<'a>>) -> Result<&'a str, CompileError> {
    if let vyp::Expr::Name(name) = exp.node {
        return Ok(name);
    }

    unreachable!()
}

/// Retrieves the &str value of a name expression and converts it to a String.
pub fn expr_name_string(exp: &Spanned<vyp::Expr>) -> Result<String, CompileError> {
    expr_name_str(exp).map(|name| name.to_string())
}

/// Builds a Yul expression from the first slice, if it is an index.
pub fn slices_index(
    scope: Shared<FunctionScope>,
    slices: &Spanned<Vec<Spanned<vyp::Slice>>>,
) -> Result<ExtExpression, CompileError> {
    if let Some(first_slice) = slices.node.first() {
        return slice_index(scope, first_slice);
    }

    unreachable!()
}

/// Creates a new spanned expression. Useful in cases where an `Expr` is nested
/// within the node of a `Spanned` object.
pub fn spanned_expression<'a>(span: &Span, exp: &vyp::Expr<'a>) -> Spanned<vyp::Expr<'a>> {
    Spanned {
        node: (*exp).clone(),
        span: (*span).to_owned(),
    }
}

pub fn slice_index(
    scope: Shared<FunctionScope>,
    slice: &Spanned<vyp::Slice>,
) -> Result<ExtExpression, CompileError> {
    if let vyp::Slice::Index(index) = &slice.node {
        let spanned = spanned_expression(&slice.span, index.as_ref());
        return expr(scope, &spanned);
    }

    unreachable!()
}

fn expr_name(
    scope: Shared<FunctionScope>,
    exp: &Spanned<vyp::Expr>,
) -> Result<ExtExpression, CompileError> {
    if let vyp::Expr::Name(name) = exp.node {
        let identifier = identifier_expression! {(name)};

        return match scope.borrow().def(name.to_string()) {
            Some(FunctionDef::Base(base)) => Ok(ExtExpression {
                expression: identifier,
                location: Location::Value,
                typ: Type::Base(base),
            }),
            Some(FunctionDef::Array(array)) => Ok(ExtExpression {
                expression: identifier,
                location: Location::Memory,
                typ: Type::Array(array),
            }),
            None => Err(CompileError::static_str("no definition found")),
        };
    }

    unreachable!()
}

fn expr_num(exp: &Spanned<vyp::Expr>) -> Result<ExtExpression, CompileError> {
    if let vyp::Expr::Num(num) = &exp.node {
        return Ok(ExtExpression {
            expression: literal_expression! {(num)},
            location: Location::Value,
            typ: Type::Base(Base::U256),
        });
    }

    unreachable!()
}

fn expr_subscript(
    scope: Shared<FunctionScope>,
    exp: &Spanned<vyp::Expr>,
) -> Result<ExtExpression, CompileError> {
    if let vyp::Expr::Subscript { value, slices } = &exp.node {
        let value = expr(Rc::clone(&scope), value)?;
        let index = slices_index(scope, slices)?;

        return match (&value.typ, &value.location) {
            (Type::Map(_), Location::Storage) => keyed_storage_map(value, index),
            (Type::Array(_), Location::Storage) => unimplemented!(),
            (Type::Array(_), Location::Memory) => indexed_memory_array(value, index),
            (_, _) => unreachable!(),
        };
    }

    unreachable!()
}

fn keyed_storage_map(
    map: ExtExpression,
    key: ExtExpression,
) -> Result<ExtExpression, CompileError> {
    if let Type::Map(Map {
        key: _,
        value: value_type,
    }) = map.typ
    {
        let sptr = expression! { dualkeccak256([map.expression], [key.expression]) };

        let (expression, location) = match value_type.clone() {
            FixedSize::Array(array) => (array.scopy(sptr)?, Location::Memory),
            FixedSize::Base(base) => (base.sload(sptr)?, Location::Value),
        };

        return Ok(ExtExpression {
            expression,
            location,
            typ: value_type.into_type(),
        });
    }

    unreachable!()
}

fn indexed_memory_array(
    array: ExtExpression,
    index: ExtExpression,
) -> Result<ExtExpression, CompileError> {
    if let Type::Array(array_type) = array.typ {
        return Ok(ExtExpression {
            expression: array_type.mload_elem(array.expression, index.expression)?,
            location: Location::Value,
            typ: Type::Base(array_type.inner),
        });
    }

    unreachable!()
}

fn expr_attribute(
    scope: Shared<FunctionScope>,
    exp: &Spanned<vyp::Expr>,
) -> Result<ExtExpression, CompileError> {
    if let vyp::Expr::Attribute { value, attr } = &exp.node {
        return match expr_name_str(value)? {
            "msg" => expr_attribute_msg(attr),
            "self" => expr_attribute_self(scope, attr),
            _ => Err(CompileError::static_str("invalid attribute value")),
        };
    }

    unreachable!()
}

fn expr_attribute_msg(attr: &Spanned<&str>) -> Result<ExtExpression, CompileError> {
    match attr.node {
        "sender" => Ok(ExtExpression {
            expression: expression! { caller() },
            location: Location::Value,
            typ: Type::Base(Base::Address),
        }),
        _ => Err(CompileError::static_str("invalid msg attribute name")),
    }
}

fn expr_attribute_self(
    scope: Shared<FunctionScope>,
    attr: &Spanned<&str>,
) -> Result<ExtExpression, CompileError> {
    match scope.borrow().contract_def(attr.node.to_string()) {
        Some(ContractDef::Map { index, map }) => Ok(ExtExpression {
            expression: literal_expression! {(index)},
            location: Location::Storage,
            typ: Type::Map(map),
        }),
        Some(ContractDef::Function { .. }) => unimplemented!(),
        Some(ContractDef::Event(_)) => {
            Err(CompileError::static_str("invalid use of event definition"))
        }
        None => Err(CompileError::static_str("unknown contract definition")),
    }
}

#[cfg(test)]
mod tests {
    use crate::yul::mappers::expressions::{
        expr,
        ExtExpression,
        Location,
    };
    use crate::yul::namespace::scopes::{
        ContractScope,
        FunctionScope,
        ModuleScope,
        Shared,
    };
    use crate::yul::namespace::types::{
        Array,
        Base,
        FixedSize,
        Map,
        Type,
    };
    use std::rc::Rc;
    use vyper_parser as parser;

    fn scope() -> Shared<FunctionScope> {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        FunctionScope::new(contract_scope)
    }

    fn map(scope: Shared<FunctionScope>, src: &str) -> ExtExpression {
        let tokens = parser::get_parse_tokens(src).expect("Couldn't parse expression");
        let expression = &parser::parsers::expr(&tokens[..])
            .expect("Couldn't build expression AST")
            .1;

        expr(scope, expression).expect("Couldn't map expression AST")
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

        let result = map(scope, "self.foo[3]");

        assert_eq!(
            result.expression.to_string(),
            "sloadn(dualkeccak256(0, 3), 32)"
        );
        assert_eq!(result.location, Location::Value);
        assert_eq!(result.typ, Type::Base(Base::U256));
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

        let foo_result = map(Rc::clone(&scope), "self.foo[42]");
        let bar_result = map(scope, "self.bar[2]");

        assert_eq!(
            foo_result.expression.to_string(),
            "scopy(dualkeccak256(0, 42), 100)"
        );
        assert_eq!(foo_result.location, Location::Memory);
        assert_eq!(
            foo_result.typ,
            Type::Array(Array {
                dimension: 5,
                inner: Base::Address
            })
        );

        assert_eq!(
            bar_result.expression.to_string(),
            "sloadn(dualkeccak256(1, 2), 20)"
        );
        assert_eq!(bar_result.location, Location::Value);
        assert_eq!(bar_result.typ, Type::Base(Base::Address));
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
        let result = map(Rc::clone(&scope), "self.foo_map[bar_array[index]]");

        assert_eq!(
            result.expression.to_string(),
            "scopy(dualkeccak256(0, mloadn(add(bar_array, mul(index, 1)), 1)), 160)"
        );
        assert_eq!(result.location, Location::Memory);
        assert_eq!(
            result.typ,
            Type::Array(Array {
                dimension: 8,
                inner: Base::Address
            })
        );
    }

    #[test]
    fn msg_sender() {
        let result = map(scope(), "msg.sender");

        assert_eq!(result.expression.to_string(), "caller()");
        assert_eq!(result.location, Location::Value);
        assert_eq!(result.typ, Type::Base(Base::Address));
    }
}
