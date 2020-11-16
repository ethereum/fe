use crate::errors::SemanticError;
use crate::namespace::scopes::{
    BlockDef,
    BlockScope,
    ContractDef,
    Shared,
};

use crate::namespace::operations;
use crate::namespace::types::{
    Base,
    Type,
};
use crate::traversal::_utils::{
    expression_attributes_to_types,
    fixed_sizes_to_types,
    spanned_expression,
};
use crate::{
    Context,
    ExpressionAttributes,
    Location,
};
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use std::rc::Rc;

/// Gather context information for expressions and check for type errors.
pub fn expr(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    let attributes = match &exp.node {
        fe::Expr::Name(_) => expr_name(scope, exp)?,
        fe::Expr::Num(_) => expr_num(exp)?,
        fe::Expr::Bool(_) => expr_bool(exp)?,
        fe::Expr::Subscript { .. } => expr_subscript(scope, Rc::clone(&context), exp)?,
        fe::Expr::Attribute { .. } => expr_attribute(scope, exp)?,
        fe::Expr::Ternary { .. } => unimplemented!(),
        fe::Expr::BoolOperation { .. } => unimplemented!(),
        fe::Expr::BinOperation { .. } => expr_bin_operation(scope, Rc::clone(&context), exp)?,
        fe::Expr::UnaryOperation { .. } => unimplemented!(),
        fe::Expr::CompOperation { .. } => expr_comp_operation(scope, Rc::clone(&context), exp)?,
        fe::Expr::Call { .. } => expr_call(scope, Rc::clone(&context), exp)?,
        fe::Expr::List { .. } => unimplemented!(),
        fe::Expr::ListComp { .. } => unimplemented!(),
        fe::Expr::Tuple { .. } => unimplemented!(),
        fe::Expr::Str(_) => unimplemented!(),
        fe::Expr::Ellipsis => unimplemented!(),
    };

    context.borrow_mut().add_expression(exp, attributes.clone());

    Ok(attributes)
}

/// Retrieves the &str value of a name expression.
pub fn expr_name_str<'a>(exp: &Spanned<fe::Expr<'a>>) -> Result<&'a str, SemanticError> {
    if let fe::Expr::Name(name) = exp.node {
        return Ok(name);
    }

    unreachable!()
}

/// Retrieves the &str value of a name expression and converts it to a String.
pub fn expr_name_string(exp: &Spanned<fe::Expr>) -> Result<String, SemanticError> {
    expr_name_str(exp).map(|name| name.to_string())
}

/// Gather context information for an index and check for type errors.
pub fn slices_index(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    slices: &Spanned<Vec<Spanned<fe::Slice>>>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let Some(first_slice) = slices.node.first() {
        return slice_index(scope, context, first_slice);
    }

    unreachable!()
}

/// Gather context information for an index and check for type errors.
pub fn slice_index(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    slice: &Spanned<fe::Slice>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Slice::Index(index) = &slice.node {
        let spanned = spanned_expression(&slice.span, index.as_ref());
        return expr(scope, context, &spanned);
    }

    unreachable!()
}

fn expr_name(
    scope: Shared<BlockScope>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Name(name) = exp.node {
        return match scope.borrow().def(name.to_string()) {
            Some(BlockDef::Base(base)) => Ok(ExpressionAttributes {
                location: Location::Value,
                typ: Type::Base(base),
            }),
            Some(BlockDef::Array(array)) => Ok(ExpressionAttributes {
                location: Location::Memory,
                typ: Type::Array(array),
            }),
            None => Err(SemanticError::UndefinedValue {
                value: name.to_string(),
            }),
        };
    }

    unreachable!()
}

fn expr_bool(exp: &Spanned<fe::Expr>) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Bool(_) = &exp.node {
        return Ok(ExpressionAttributes {
            location: Location::Value,
            typ: Type::Base(Base::Bool),
        });
    }

    unreachable!()
}

fn expr_num(exp: &Spanned<fe::Expr>) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Num(_) = &exp.node {
        return Ok(ExpressionAttributes {
            location: Location::Value,
            typ: Type::Base(Base::U256),
        });
    }

    unreachable!()
}

fn expr_subscript(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Subscript { value, slices } = &exp.node {
        let value_attributes = expr(Rc::clone(&scope), Rc::clone(&context), value)?;
        let index_attributes = slices_index(scope, context, slices)?;

        let typ = operations::index(value_attributes.typ.clone(), index_attributes.typ)?;
        let location = match value_attributes.typ {
            Type::Map(map) => {
                match *map.value {
                    Type::Base(_) => Location::Value,
                    Type::Array(_) => Location::Memory,
                    // Index value is ignored. We may want to introduce a new location
                    // variant named StorageRuntime or something to suit this case.
                    Type::Map(_) => Location::Storage { index: 0 },
                    Type::Tuple(_) => Location::Memory,
                }
            }
            Type::Array(_) => Location::Value,
            Type::Base(_) => return Err(SemanticError::NotSubscriptable),
            Type::Tuple(_) => return Err(SemanticError::NotSubscriptable),
        };

        return Ok(ExpressionAttributes { typ, location });
    }

    unreachable!()
}

fn expr_attribute(
    scope: Shared<BlockScope>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Attribute { value, attr } = &exp.node {
        return match expr_name_str(value)? {
            "msg" => expr_attribute_msg(attr),
            "self" => expr_attribute_self(scope, attr),
            value => Err(SemanticError::UndefinedValue {
                value: value.to_string(),
            }),
        };
    }

    unreachable!()
}

fn expr_attribute_msg(attr: &Spanned<&str>) -> Result<ExpressionAttributes, SemanticError> {
    match attr.node {
        "sender" => Ok(ExpressionAttributes {
            location: Location::Value,
            typ: Type::Base(Base::Address),
        }),
        value => Err(SemanticError::UndefinedValue {
            value: value.to_string(),
        }),
    }
}

fn expr_attribute_self(
    scope: Shared<BlockScope>,
    attr: &Spanned<&str>,
) -> Result<ExpressionAttributes, SemanticError> {
    match scope.borrow().contract_def(attr.node.to_string()) {
        Some(ContractDef::Map { index, map }) => Ok(ExpressionAttributes {
            location: Location::Storage { index },
            typ: Type::Map(map),
        }),
        Some(ContractDef::Function { .. }) => unimplemented!(),
        Some(ContractDef::Event(_)) => unimplemented!(),
        None => unimplemented!(),
    }
}

fn expr_bin_operation(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::BinOperation { left, op: _, right } = &exp.node {
        let _left_attributes = expr(Rc::clone(&scope), Rc::clone(&context), left);
        let _right_attributes = expr(Rc::clone(&scope), Rc::clone(&context), right);

        // TODO: Perform type checking

        // for now we assume these are the only possible attributes
        return Ok(ExpressionAttributes {
            typ: Type::Base(Base::U256),
            location: Location::Value,
        });
    }

    unreachable!()
}

pub fn call_arg(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    arg: &Spanned<fe::CallArg>,
) -> Result<ExpressionAttributes, SemanticError> {
    match &arg.node {
        fe::CallArg::Arg(value) => {
            let spanned = spanned_expression(&arg.span, value);
            expr(scope, context, &spanned)
        }
        fe::CallArg::Kwarg(fe::Kwarg { name: _, value }) => expr(scope, context, value),
    }
}

fn expr_call(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Call { args, func } = &exp.node {
        let argument_attributes: Vec<ExpressionAttributes> = args
            .node
            .iter()
            // Side effect: Performs semantic analysis on each call arg and adds its attributes to
            // the context
            .map(|argument| call_arg(scope.clone(), context.clone(), argument))
            .collect::<Result<_, _>>()?;

        if let fe::Expr::Attribute { value: _, attr } = &func.node {
            let contract_scope = &scope.borrow().contract_scope();
            let called_func = contract_scope.borrow().def(attr.node.to_string());
            match called_func {
                Some(ContractDef::Function {
                    is_public: _,
                    params,
                    returns: return_type,
                }) => {
                    if fixed_sizes_to_types(params)
                        != expression_attributes_to_types(argument_attributes)
                    {
                        return Err(SemanticError::TypeError);
                    }

                    return Ok(ExpressionAttributes {
                        typ: return_type.into_type(),
                        location: Location::Value,
                    });
                }
                _ => unreachable!(),
            };
        }
    }

    unreachable!()
}

fn expr_comp_operation(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::CompOperation { left, op: _, right } = &exp.node {
        let _left_attributes = expr(Rc::clone(&scope), Rc::clone(&context), left);
        let _right_attributes = expr(Rc::clone(&scope), Rc::clone(&context), right);

        // TODO: Perform type checking

        // for now we assume these are the only possible attributes
        return Ok(ExpressionAttributes {
            typ: Type::Base(Base::Bool),
            location: Location::Value,
        });
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use crate::namespace::scopes::{
        BlockScope,
        ContractScope,
        ModuleScope,
        Shared,
    };
    use crate::namespace::types::{
        Array,
        Base,
        Map,
        Type,
    };
    use crate::traversal::expressions::expr;
    use crate::{
        Context,
        ExpressionAttributes,
        Location,
    };
    use fe_parser as parser;
    use fe_parser::span::Span;
    use rstest::rstest;
    use std::rc::Rc;

    static U256_VAL: ExpressionAttributes = ExpressionAttributes {
        typ: Type::Base(Base::U256),
        location: Location::Value,
    };

    static ADDR_VAL: ExpressionAttributes = ExpressionAttributes {
        typ: Type::Base(Base::Address),
        location: Location::Value,
    };

    static ADDR_ARRAY_MEM: ExpressionAttributes = ExpressionAttributes {
        typ: Type::Array(Array {
            dimension: 100,
            inner: Base::Address,
        }),
        location: Location::Memory,
    };

    fn addr_u256_map_sto() -> ExpressionAttributes {
        ExpressionAttributes {
            typ: Type::Map(Map {
                key: Base::Address,
                value: Box::new(Type::Base(Base::U256)),
            }),
            location: Location::Storage { index: 0 },
        }
    }

    fn scope() -> Shared<BlockScope> {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        BlockScope::from_contract_scope(Span::new(0, 0), contract_scope)
    }

    fn analyze(scope: Shared<BlockScope>, src: &str) -> Context {
        let context = Context::new_shared();
        let tokens = parser::get_parse_tokens(src).expect("Couldn't parse expression");
        let expression = &parser::parsers::expr(&tokens[..])
            .expect("Couldn't build expression AST")
            .1;

        expr(scope, Rc::clone(&context), expression).expect("Couldn't map expression AST");
        Rc::try_unwrap(context)
            .map_err(|_| "")
            .unwrap()
            .into_inner()
    }

    #[rstest(
        expression,
        expected_span_attrs,
        case("42", &[(0,2, &U256_VAL)]),
        case(
            "42 + 42",
            &[(0, 2, &U256_VAL), (5, 7, &U256_VAL), (0, 7, &U256_VAL)]
        ),
        case(
            "my_addr_array",
            &[(0, 13, &ADDR_ARRAY_MEM)]
        ),
        case(
            "my_addr_array[42]",
            &[(0, 13, &ADDR_ARRAY_MEM), (14, 16, &U256_VAL), (0, 17, &ADDR_VAL)]
        ),
        case(
            "self.my_addr_u256_map[my_addr_array[42]] + 26",
            &[
                (0, 21, &addr_u256_map_sto()), (22, 35, &ADDR_ARRAY_MEM), (36, 38, &U256_VAL),
                (43, 45, &U256_VAL), (22, 39, &ADDR_VAL), (0, 40, &U256_VAL), (0, 45, &U256_VAL)
            ]
        ),
    )]
    fn exprs(expression: &str, expected_span_attrs: &[(usize, usize, &ExpressionAttributes)]) {
        let scope = scope();
        scope
            .borrow_mut()
            .add_base("my_addr".to_string(), Base::Address);
        scope.borrow_mut().add_array(
            "my_addr_array".to_string(),
            Array {
                dimension: 100,
                inner: Base::Address,
            },
        );
        scope.borrow_mut().contract_scope().borrow_mut().add_map(
            "my_addr_u256_map".to_string(),
            Map {
                key: Base::Address,
                value: Box::new(Type::Base(Base::U256)),
            },
        );

        let context = analyze(scope, expression);
        for span_attr in expected_span_attrs {
            let attributes = context.expressions.get(&Span {
                start: span_attr.0,
                end: span_attr.1,
            });

            if attributes.is_none() {
                panic!(
                    "no attributes found at span ({}, {})",
                    span_attr.0, span_attr.1
                )
            }

            assert_eq!(attributes.unwrap(), span_attr.2)
        }
    }
}
