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
    FixedSize,
    Integer,
    Type,
    U256,
};
use crate::traversal::_utils::{
    expression_attributes_to_types,
    fixed_sizes_to_types,
    spanned_expression,
};
use crate::{
    CallType,
    Context,
    ExpressionAttributes,
    Location,
};
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use std::rc::Rc;

const SELF: &str = "self";

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
        fe::Expr::Ternary { .. } => expr_ternary(scope, Rc::clone(&context), exp)?,
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

/// Gather context information for expressions and check for type errors.
///
/// Attributes a value move to the expression.
pub fn expr_with_value_move(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    let attributes = expr(Rc::clone(&scope), Rc::clone(&context), exp)?.with_value_move()?;

    context.borrow_mut().add_expression(exp, attributes.clone());

    Ok(attributes)
}

/// Gather context information for expressions and check for type errors.
///
/// Attributes the default move to the expression.
pub fn expr_with_default_move(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    let attributes = expr(Rc::clone(&scope), Rc::clone(&context), exp)?.with_default_move()?;

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
        let attributes = expr_with_value_move(scope, Rc::clone(&context), &spanned)?;

        return Ok(attributes);
    }

    unreachable!()
}

fn expr_name(
    scope: Shared<BlockScope>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Name(name) = exp.node {
        return match scope.borrow().def(name.to_string()) {
            Some(BlockDef::Variable(FixedSize::Base(base))) => {
                Ok(ExpressionAttributes::new(Type::Base(base), Location::Value))
            }
            Some(BlockDef::Variable(FixedSize::Array(array))) => Ok(ExpressionAttributes::new(
                Type::Array(array),
                Location::Memory,
            )),
            Some(BlockDef::Variable(FixedSize::String(string))) => Ok(ExpressionAttributes::new(
                Type::String(string),
                Location::Memory,
            )),
            Some(BlockDef::Variable(FixedSize::Tuple(_))) => unimplemented!(),
            None => Err(SemanticError::UndefinedValue {
                value: name.to_string(),
            }),
        };
    }

    unreachable!()
}

fn expr_bool(exp: &Spanned<fe::Expr>) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Bool(_) = &exp.node {
        return Ok(ExpressionAttributes::new(
            Type::Base(Base::Bool),
            Location::Value,
        ));
    }

    unreachable!()
}

fn expr_num(exp: &Spanned<fe::Expr>) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Num(_) = &exp.node {
        return Ok(ExpressionAttributes::new(Type::Base(U256), Location::Value));
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

        // performs type checking
        let typ = operations::index(value_attributes.typ.clone(), index_attributes.typ)?;
        let location = match value_attributes.location {
            Location::Storage { .. } => Location::Storage { nonce: None },
            Location::Memory => Location::Memory,
            // neither maps or arrays can be stored as values, so this is unreachable
            Location::Value => unreachable!(),
        };

        return Ok(ExpressionAttributes::new(typ, location));
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
        "sender" => Ok(ExpressionAttributes::new(
            Type::Base(Base::Address),
            Location::Value,
        )),
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
        Some(ContractDef::Field { nonce, typ }) => Ok(ExpressionAttributes::new(
            typ,
            Location::Storage { nonce: Some(nonce) },
        )),
        None => Err(SemanticError::UndefinedValue {
            value: attr.node.to_string(),
        }),
        _ => Err(SemanticError::TypeError),
    }
}

fn expr_bin_operation(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::BinOperation { left, op: _, right } = &exp.node {
        let left_attributes = expr_with_value_move(Rc::clone(&scope), Rc::clone(&context), left)?;
        let right_attributes = expr_with_value_move(Rc::clone(&scope), Rc::clone(&context), right)?;

        validate_types_equal(&left_attributes, &right_attributes)?;

        // for now we assume these are the only possible attributes
        return Ok(ExpressionAttributes::new(
            right_attributes.typ,
            Location::Value,
        ));
    }

    unreachable!()
}

fn validate_types_equal(
    expression_a: &ExpressionAttributes,
    expression_b: &ExpressionAttributes,
) -> Result<(), SemanticError> {
    if expression_a.typ == expression_b.typ {
        Ok(())
    } else {
        Err(SemanticError::TypeError)
    }
}

pub fn call_arg(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    arg: &Spanned<fe::CallArg>,
) -> Result<ExpressionAttributes, SemanticError> {
    match &arg.node {
        fe::CallArg::Arg(value) => {
            let spanned = spanned_expression(&arg.span, value);
            let attributes = expr_with_default_move(scope, Rc::clone(&context), &spanned)?;

            Ok(attributes)
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
            .map(|argument| call_arg(Rc::clone(&scope), Rc::clone(&context), argument))
            .collect::<Result<_, _>>()?;

        return match &func.node {
            fe::Expr::Attribute { value, attr } => {
                let value = expr_name_string(value)?;

                if value == SELF {
                    context.borrow_mut().add_call(
                        exp,
                        CallType::SelfFunction {
                            name: attr.node.to_string(),
                        },
                    );
                    expr_call_self(scope, context, attr, argument_attributes)
                } else {
                    Err(SemanticError::UndefinedValue { value })
                }
            }
            fe::Expr::Name(name) => {
                if argument_attributes.len() != 1 {
                    return Err(SemanticError::TypeError);
                }

                context
                    .borrow_mut()
                    .add_call(exp, CallType::TypeConstructor);
                expr_type_constructor(scope, context, *name)
            }
            _ => Err(SemanticError::NotCallable),
        };
    }

    unreachable!()
}

fn expr_call_self(
    scope: Shared<BlockScope>,
    _context: Shared<Context>,
    func_name: &Spanned<&str>,
    argument_attributes: Vec<ExpressionAttributes>,
) -> Result<ExpressionAttributes, SemanticError> {
    let contract_scope = &scope.borrow().contract_scope();
    let called_func = contract_scope.borrow().def(func_name.node.to_string());

    match called_func {
        Some(ContractDef::Function {
            is_public: _,
            param_types,
            return_type,
            scope: _,
        }) => {
            if fixed_sizes_to_types(param_types)
                != expression_attributes_to_types(argument_attributes)
            {
                return Err(SemanticError::TypeError);
            }

            Ok(ExpressionAttributes::new(
                return_type.into(),
                Location::Value,
            ))
        }
        Some(_) => Err(SemanticError::NotCallable),
        None => Err(SemanticError::UndefinedValue {
            value: func_name.node.to_string(),
        }),
    }
}

fn expr_type_constructor(
    _scope: Shared<BlockScope>,
    _context: Shared<Context>,
    func_name: &str,
) -> Result<ExpressionAttributes, SemanticError> {
    let typ = match func_name {
        "address" => Type::Base(Base::Address),
        "u256" => Type::Base(Base::Numeric(Integer::U256)),
        "u128" => Type::Base(Base::Numeric(Integer::U128)),
        "u64" => Type::Base(Base::Numeric(Integer::U64)),
        "u32" => Type::Base(Base::Numeric(Integer::U32)),
        "u16" => Type::Base(Base::Numeric(Integer::U16)),
        "u8" => Type::Base(Base::Numeric(Integer::U8)),
        _ => {
            return Err(SemanticError::UndefinedValue {
                value: func_name.to_string(),
            })
        }
    };

    Ok(ExpressionAttributes::new(typ, Location::Value))
}

fn expr_comp_operation(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::CompOperation { left, op: _, right } = &exp.node {
        // comparison operands should be moved to the stack
        let left_attributes = expr_with_value_move(Rc::clone(&scope), Rc::clone(&context), left)?;
        let right_attributes = expr_with_value_move(Rc::clone(&scope), Rc::clone(&context), right)?;

        validate_types_equal(&left_attributes, &right_attributes)?;

        // for now we assume these are the only possible attributes
        return Ok(ExpressionAttributes::new(
            Type::Base(Base::Bool),
            Location::Value,
        ));
    }

    unreachable!()
}

fn expr_ternary(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Ternary {
        if_expr,
        test,
        else_expr,
    } = &exp.node
    {
        // test attributes should be stored as a value
        let test_attributes = expr_with_value_move(Rc::clone(&scope), Rc::clone(&context), test)?;
        // the return expressions should be stored in their default locations
        //
        // If, for example, one of the expressions is stored in memory and the other is
        // stored in storage, it's necessary that we move them to the same location.
        // This could be memory or the stack, depending on the type.
        let if_expr_attributes =
            expr_with_default_move(Rc::clone(&scope), Rc::clone(&context), if_expr)?;
        let else_expr_attributes =
            expr_with_default_move(Rc::clone(&scope), Rc::clone(&context), else_expr)?;

        // Make sure the `test_attributes` is a boolean type.
        if Type::Base(Base::Bool) == test_attributes.typ {
            // Should have the same return Type
            if if_expr_attributes.typ == else_expr_attributes.typ {
                // can return else_expr_attributes as well.
                return Ok(ExpressionAttributes::new(
                    if_expr_attributes.typ.clone(),
                    if_expr_attributes.final_location(),
                ));
            }
        }
        return Err(SemanticError::TypeError);
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
        FixedSize,
        Integer,
        Map,
        Type,
        U256,
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

    fn u256_val() -> ExpressionAttributes {
        ExpressionAttributes::new(Type::Base(U256), Location::Value)
    }

    fn u128_val() -> ExpressionAttributes {
        ExpressionAttributes::new(Type::Base(Base::Numeric(Integer::U128)), Location::Value)
    }

    fn u256_sto_with_move() -> ExpressionAttributes {
        let mut attributes =
            ExpressionAttributes::new(Type::Base(U256), Location::Storage { nonce: None });
        attributes.move_location = Some(Location::Value);
        attributes
    }

    fn addr_val() -> ExpressionAttributes {
        ExpressionAttributes::new(Type::Base(Base::Address), Location::Value)
    }

    fn addr_mem() -> ExpressionAttributes {
        ExpressionAttributes::new(Type::Base(Base::Address), Location::Memory)
    }

    fn addr_mem_with_move() -> ExpressionAttributes {
        let mut attributes = ExpressionAttributes::new(Type::Base(Base::Address), Location::Memory);
        attributes.move_location = Some(Location::Value);
        attributes
    }

    fn addr_array_mem() -> ExpressionAttributes {
        ExpressionAttributes::new(
            Type::Array(Array {
                dimension: 100,
                inner: Base::Address,
            }),
            Location::Memory,
        )
    }

    fn addr_u256_map_sto() -> ExpressionAttributes {
        ExpressionAttributes::new(
            Type::Map(Map {
                key: Base::Address,
                value: Box::new(Type::Base(U256)),
            }),
            Location::Storage { nonce: Some(0) },
        )
    }

    fn scope() -> Shared<BlockScope> {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        BlockScope::from_contract_scope("".to_string(), contract_scope)
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
        expected_attributes,
        case("42", &[("42", &u256_val())]),
        case(
            "42 + 26",
            &[
                ("42", &u256_val()),
                ("26", &u256_val()),
                ("42 + 26", &u256_val())
            ]
        ),
        case(
            "my_addr_array",
            &[
                ("my_addr_array", &addr_array_mem())
            ]
        ),
        case(
            "my_addr_array[42]",
            &[
                ("my_addr_array", &addr_array_mem()),
                ("42", &u256_val()),
                ("my_addr_array[42]", &addr_mem())
            ]
        ),
        case(
            "self.my_addr_u256_map[my_addr_array[42]] + 26",
            &[
                ("self.my_addr_u256_map", &addr_u256_map_sto()),
                ("my_addr_array", &addr_array_mem()),
                ("42", &u256_val()),
                ("26", &u256_val()),
                ("my_addr_array[42]", &addr_mem_with_move()),
                ("self.my_addr_u256_map[my_addr_array[42]]", &u256_sto_with_move()),
                ("self.my_addr_u256_map[my_addr_array[42]] + 26", &u256_val())
            ]
        ),
        case(
            "address(0)",
            &[
                ("0", &u256_val()),
                ("address(0)", &addr_val()),
            ]
        ),
        case(
            "u128(0)",
            &[
                ("0", &u256_val()),
                ("u128(0)", &u128_val()),
            ]
        )
    )]
    fn exprs(expression: &str, expected_attributes: &[(&str, &ExpressionAttributes)]) {
        let scope = scope();
        scope
            .borrow_mut()
            .add_var("my_addr".to_string(), FixedSize::Base(Base::Address));
        scope.borrow_mut().add_var(
            "my_addr_array".to_string(),
            FixedSize::Array(Array {
                dimension: 100,
                inner: Base::Address,
            }),
        );
        scope.borrow_mut().contract_scope().borrow_mut().add_field(
            "my_addr_u256_map".to_string(),
            Type::Map(Map {
                key: Base::Address,
                value: Box::new(Type::Base(U256)),
            }),
        );

        let context = analyze(scope, expression);

        for (sub_expression, expected_attribute) in expected_attributes.to_owned() {
            let start = expression
                .find(sub_expression)
                .expect(&format!("sub expression not found: {}", sub_expression));
            let end = start + sub_expression.len();

            let actual_attributes = context
                .expressions
                .get(&Span { start, end })
                .expect(&format!("attributes missing: {}", sub_expression));

            assert_eq!(expected_attribute, actual_attributes)
        }
    }
}
