use crate::errors::SemanticError;
use crate::namespace::scopes::{
    BlockScope,
    ContractFunctionDef,
    Shared,
};
use std::convert::TryFrom;

use crate::builtins;
use crate::namespace::operations;
use crate::namespace::types::{
    Array,
    Base,
    FeString,
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

/// Gather context information for expressions and check for type errors.
pub fn expr(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    let attributes = match &exp.node {
        fe::Expr::Name(_) => expr_name(scope, exp),
        fe::Expr::Num(_) => expr_num(exp),
        fe::Expr::Bool(_) => expr_bool(exp),
        fe::Expr::Subscript { .. } => expr_subscript(scope, Rc::clone(&context), exp),
        fe::Expr::Attribute { .. } => expr_attribute(scope, exp),
        fe::Expr::Ternary { .. } => expr_ternary(scope, Rc::clone(&context), exp),
        fe::Expr::BoolOperation { .. } => unimplemented!(),
        fe::Expr::BinOperation { .. } => expr_bin_operation(scope, Rc::clone(&context), exp),
        fe::Expr::UnaryOperation { .. } => expr_unary_operation(scope, Rc::clone(&context), exp),
        fe::Expr::CompOperation { .. } => expr_comp_operation(scope, Rc::clone(&context), exp),
        fe::Expr::Call { .. } => expr_call(scope, Rc::clone(&context), exp),
        fe::Expr::List { .. } => expr_list(scope, Rc::clone(&context), exp),
        fe::Expr::ListComp { .. } => unimplemented!(),
        fe::Expr::Tuple { .. } => unimplemented!(),
        fe::Expr::Str(_) => expr_str(scope, exp),
        fe::Expr::Ellipsis => unimplemented!(),
    }
    .map_err(|error| error.with_context(exp.span))?;

    context.borrow_mut().add_expression(exp, attributes.clone());

    Ok(attributes)
}

pub fn expr_list(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::List { elts } = &exp.node {
        // Assuming every element attribute should matches the attribute of 0th element
        // of list. Safe to unwrap as we already checked the length of the
        // list.
        if let Some(elt) = elts.first() {
            let attribute_to_be_matched = expr(Rc::clone(&scope), Rc::clone(&context), elt)?;

            // Rationale - A list contains only one type of element.
            // No need to iterate the whole list if the first attribute doesn't match next
            // one.
            for elt in elts.iter() {
                let next_attribute = expr(Rc::clone(&scope), Rc::clone(&context), elt)?;
                validate_types_equal(&next_attribute, &attribute_to_be_matched)?;
            }

            // TODO: Right now we are only supporting Base type arrays
            // Potential we can support the tuples as well.
            if let Type::Base(base) = attribute_to_be_matched.typ {
                return Ok(ExpressionAttributes {
                    typ: Type::Array(Array {
                        dimension: elts.len(),
                        inner: base,
                    }),
                    location: attribute_to_be_matched.location,
                    move_location: None,
                });
            }
            return Err(SemanticError::type_error());
        }
    }
    unreachable!()
}

/// Gather context information for expressions and check for type errors.
///
/// Also ensures that the expression is on the stack.
pub fn value_expr(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    let attributes = expr(Rc::clone(&scope), Rc::clone(&context), exp)?.into_loaded()?;

    context.borrow_mut().add_expression(exp, attributes.clone());

    Ok(attributes)
}

/// Gather context information for expressions and check for type errors.
///
/// Also ensures that the expression is in the type's assigment location.
pub fn assignable_expr(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    let attributes = expr(Rc::clone(&scope), Rc::clone(&context), exp)?.into_assignable()?;

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
        let attributes = value_expr(scope, Rc::clone(&context), &spanned)?;

        return Ok(attributes);
    }

    unreachable!()
}

fn expr_name(
    scope: Shared<BlockScope>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Name(name) = exp.node {
        return match scope.borrow().variable_def(name.to_string()) {
            Some(FixedSize::Base(base)) => {
                Ok(ExpressionAttributes::new(Type::Base(base), Location::Value))
            }
            Some(FixedSize::Array(array)) => Ok(ExpressionAttributes::new(
                Type::Array(array),
                Location::Memory,
            )),
            Some(FixedSize::String(string)) => Ok(ExpressionAttributes::new(
                Type::String(string),
                Location::Memory,
            )),
            Some(FixedSize::Tuple(_)) => unimplemented!(),
            None => Err(SemanticError::undefined_value()),
        };
    }

    unreachable!()
}

fn expr_str(
    scope: Shared<BlockScope>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Str(lines) = &exp.node {
        let string_val = lines.join("");
        let string_length = string_val.len();

        scope
            .borrow_mut()
            .contract_scope()
            .borrow_mut()
            .add_string(string_val);

        return Ok(ExpressionAttributes::new(
            Type::String(FeString {
                max_size: string_length,
            }),
            Location::Memory,
        ));
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
            builtins::MSG => expr_attribute_msg(attr),
            builtins::SELF => expr_attribute_self(scope, attr),
            _ => Err(SemanticError::undefined_value()),
        };
    }

    unreachable!()
}

fn expr_attribute_msg(attr: &Spanned<&str>) -> Result<ExpressionAttributes, SemanticError> {
    match attr.node {
        builtins::SENDER => Ok(ExpressionAttributes::new(
            Type::Base(Base::Address),
            Location::Value,
        )),
        _ => Err(SemanticError::undefined_value()),
    }
}

fn expr_attribute_self(
    scope: Shared<BlockScope>,
    attr: &Spanned<&str>,
) -> Result<ExpressionAttributes, SemanticError> {
    match scope.borrow().contract_field_def(attr.node.to_string()) {
        Some(field) => Ok(ExpressionAttributes::new(
            field.typ,
            Location::Storage {
                nonce: Some(field.nonce),
            },
        )),
        None => Err(SemanticError::undefined_value()),
    }
}

fn expr_bin_operation(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::BinOperation { left, op: _, right } = &exp.node {
        let left_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), left)?;
        let right_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), right)?;

        validate_types_equal(&left_attributes, &right_attributes)?;

        // for now we assume these are the only possible attributes
        return Ok(ExpressionAttributes::new(
            right_attributes.typ,
            Location::Value,
        ));
    }

    unreachable!()
}

fn expr_unary_operation(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::UnaryOperation { op, operand } = &exp.node {
        if let fe::UnaryOperator::USub = &op.node {
            let operand_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), operand)?;

            if !matches!(operand_attributes.typ, Type::Base(Base::Numeric(_))) {
                return Err(SemanticError::type_error());
            }
            // No matter what numeric type the operand was before, the minus symbol turns it
            // into an I256 just like all positive values default to U256.
            return Ok(ExpressionAttributes::new(
                Type::Base(Base::Numeric(Integer::I256)),
                Location::Value,
            ));
        }

        unimplemented!()
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
        Err(SemanticError::type_error())
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
            let attributes = assignable_expr(scope, Rc::clone(&context), &spanned)?;

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
        return match expr_call_type(Rc::clone(&scope), Rc::clone(&context), func)? {
            CallType::TypeConstructor { typ } => {
                expr_call_type_constructor(scope, context, typ, args)
            }
            CallType::SelfAttribute { func_name } => {
                expr_call_self_attribute(scope, context, func_name, args)
            }
            CallType::ValueAttribute => expr_call_value_attribute(scope, context, func, args),
        };
    }

    unreachable!()
}

fn expr_call_type_constructor(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    typ: Type,
    args: &Spanned<Vec<Spanned<fe::CallArg>>>,
) -> Result<ExpressionAttributes, SemanticError> {
    if args.node.len() != 1 {
        return Err(SemanticError::wrong_number_of_params());
    }

    call_arg(Rc::clone(&scope), Rc::clone(&context), &args.node[0])?;

    match typ {
        Type::String(ref fe_string) => {
            validate_str_literal_fits_type(&args.node[0].node, &fe_string)?;
            Ok(ExpressionAttributes::new(typ, Location::Memory))
        }
        _ => {
            let num = validate_is_numeric_literal(&args.node[0].node)?;

            if !matches!(typ, Type::Base(Base::Address)) {
                validate_numeric_literal_fits_type(&num, &typ)?;
            }

            Ok(ExpressionAttributes::new(typ, Location::Value))
        }
    }
}

fn validate_is_numeric_literal(call_arg: &fe::CallArg) -> Result<String, SemanticError> {
    if let fe::CallArg::Arg(fe::Expr::UnaryOperation { operand, op: _ }) = call_arg {
        if let fe::Expr::Num(num) = (*operand).node {
            return Ok(format!("-{}", num));
        }
    } else if let fe::CallArg::Arg(fe::Expr::Num(num)) = call_arg {
        return Ok(num.to_string());
    }

    Err(SemanticError::numeric_literal_expected())
}

fn validate_numeric_literal_fits_type(num: &str, typ: &Type) -> Result<(), SemanticError> {
    if let Type::Base(Base::Numeric(integer)) = typ {
        if integer.fits(num) {
            return Ok(());
        } else {
            return Err(SemanticError::numeric_capacity_mismatch());
        }
    }

    Err(SemanticError::type_error())
}

fn validate_str_literal_fits_type(
    call_arg: &fe::CallArg,
    typ: &FeString,
) -> Result<(), SemanticError> {
    if let fe::CallArg::Arg(fe::Expr::Str(lines)) = call_arg {
        let string_length: usize = lines.join("").len();
        if string_length > typ.max_size {
            return Err(SemanticError::string_capacity_mismatch());
        } else {
            return Ok(());
        }
    }

    Err(SemanticError::type_error())
}

fn expr_call_self_attribute(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    func_name: String,
    args: &Spanned<Vec<Spanned<fe::CallArg>>>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let Some(ContractFunctionDef {
        is_public: _,
        param_types,
        return_type,
        scope: _,
    }) = scope
        .borrow()
        .contract_scope()
        .borrow()
        .function_def(func_name)
    {
        let argument_attributes = args
            .node
            .iter()
            .map(|arg| call_arg(Rc::clone(&scope), Rc::clone(&context), arg))
            .collect::<Result<Vec<_>, _>>()?;

        if fixed_sizes_to_types(param_types) != expression_attributes_to_types(argument_attributes)
        {
            return Err(SemanticError::type_error());
        }

        let return_location = match &return_type {
            FixedSize::Base(_) => Location::Value,
            _ => Location::Memory,
        };
        Ok(ExpressionAttributes::new(
            return_type.into(),
            return_location,
        ))
    } else {
        Err(SemanticError::undefined_value())
    }
}

fn expr_call_value_attribute(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    func: &Spanned<fe::Expr>,
    args: &Spanned<Vec<Spanned<fe::CallArg>>>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Attribute { value, attr } = &func.node {
        let value_attributes = expr(scope, context, value)?;

        // for now all of these function expect 0 arguments
        if !args.node.is_empty() {
            return Err(SemanticError::wrong_number_of_params());
        }

        return match attr.node {
            builtins::CLONE => value_attributes.into_cloned(),
            builtins::TO_MEM => value_attributes.into_cloned_from_sto(),
            _ => Err(SemanticError::undefined_value()),
        };
    }

    unreachable!()
}

fn expr_call_type(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    func: &Spanned<fe::Expr>,
) -> Result<CallType, SemanticError> {
    let call_type = match &func.node {
        fe::Expr::Name(name) => expr_name_call_type(scope, Rc::clone(&context), name),
        fe::Expr::Attribute { .. } => expr_attribute_call_type(scope, Rc::clone(&context), func),
        _ => Err(SemanticError::not_callable()),
    }?;

    context.borrow_mut().add_call(func, call_type.clone());
    Ok(call_type)
}

fn expr_name_call_type(
    _scope: Shared<BlockScope>,
    _context: Shared<Context>,
    name: &str,
) -> Result<CallType, SemanticError> {
    match name {
        "address" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Address),
        }),
        "u256" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U256)),
        }),
        "u128" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U128)),
        }),
        "u64" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U64)),
        }),
        "u32" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U32)),
        }),
        "u16" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U16)),
        }),
        "u8" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U8)),
        }),
        "i256" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I256)),
        }),
        "i128" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I128)),
        }),
        "i64" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I64)),
        }),
        "i32" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I32)),
        }),
        "i16" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I16)),
        }),
        "i8" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I8)),
        }),
        value if value.starts_with("string") => Ok(CallType::TypeConstructor {
            typ: Type::String(
                TryFrom::try_from(value).map_err(|_| SemanticError::undefined_value())?,
            ),
        }),
        _ => Err(SemanticError::undefined_value()),
    }
}

fn expr_attribute_call_type(
    _scope: Shared<BlockScope>,
    _context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<CallType, SemanticError> {
    if let fe::Expr::Attribute { value, attr } = &exp.node {
        return match value.node {
            fe::Expr::Name(builtins::SELF) => Ok(CallType::SelfAttribute {
                func_name: attr.node.to_string(),
            }),
            _ => Ok(CallType::ValueAttribute),
        };
    }

    unreachable!()
}

fn expr_comp_operation(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Spanned<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::CompOperation { left, op: _, right } = &exp.node {
        // comparison operands should be moved to the stack
        let left_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), left)?;
        let right_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), right)?;

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
        let test_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), test)?;
        // the return expressions should be stored in their default locations
        //
        // If, for example, one of the expressions is stored in memory and the other is
        // stored in storage, it's necessary that we move them to the same location.
        // This could be memory or the stack, depending on the type.
        let if_expr_attributes = assignable_expr(Rc::clone(&scope), Rc::clone(&context), if_expr)?;
        let else_expr_attributes =
            assignable_expr(Rc::clone(&scope), Rc::clone(&context), else_expr)?;

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
        return Err(SemanticError::type_error());
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
