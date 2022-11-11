use crate::context::{
    Adjustment, AdjustmentKind, AnalyzerContext, Constant, ExpressionAttributes, NamedThing,
};
use crate::display::Displayable;
use crate::errors::{TypeCoercionError, TypeError};
use crate::namespace::items::{Item, TraitId};
use crate::namespace::types::{
    Base, FeString, GenericArg, GenericParamKind, GenericType, Integer, Tuple, Type, TypeId,
};
use crate::traversal::call_args::validate_arg_count;
use fe_common::diagnostics::Label;
use fe_common::utils::humanize::pluralize_conditionally;
use fe_common::Spanned;
use fe_parser::ast;
use fe_parser::node::{Node, Span};

pub fn try_cast_type(
    context: &mut dyn AnalyzerContext,
    from: TypeId,
    from_expr: &Node<ast::Expr>,
    into: TypeId,
    into_span: Span,
) {
    if into == from {
        return;
    }
    if from.is_sptr(context.db()) {
        let from = deref_type(context, from_expr, from);
        return try_cast_type(context, from, from_expr, into, into_span);
    }

    match (from.typ(context.db()), into.typ(context.db())) {
        (Type::String(from), Type::String(into)) => {
            if from.max_size > into.max_size {
                context.error(
                    "string capacity exceeded",
                    from_expr.span,
                    &format!(
                        "this string has length {}; expected length <= {}",
                        from.max_size, into.max_size
                    ),
                );
            }
        }

        (Type::Base(Base::Address), Type::Contract(_)) => {}
        (Type::Contract(_), Type::Base(Base::Address)) => {}

        (Type::Base(Base::Numeric(from_int)), Type::Base(Base::Numeric(into_int))) => {
            let sign_differs = from_int.is_signed() != into_int.is_signed();
            let size_differs = from_int.size() != into_int.size();

            if sign_differs && size_differs {
                context.error(
                        "Casting between numeric values can change the sign or size but not both at once",
                        from_expr.span,
                        &format!("can not cast from `{}` to `{}` in a single step",
                                 from.display(context.db()),
                                 into.display(context.db())));
            }
        }
        (Type::Base(Base::Numeric(_)), Type::Base(Base::Address)) => {}
        (Type::Base(Base::Address), Type::Base(Base::Numeric(into))) => {
            if into != Integer::U256 {
                context.error(
                    &format!("can't cast `address` to `{}`", into),
                    into_span,
                    "try `u256` here",
                );
            }
        }
        (Type::SelfContract(_), Type::Base(Base::Address)) => {
            context.error(
                "`self` address must be retrieved via `Context` object",
                into_span + from_expr.span,
                "use `ctx.self_address()` here",
            );
        }

        (_, Type::Base(Base::Unit)) => unreachable!(), // rejected in expr_call_type
        (_, Type::Base(Base::Bool)) => unreachable!(), // handled in expr_call_type_constructor
        (_, Type::Tuple(_)) => unreachable!(),         // rejected in expr_call_type
        (_, Type::Struct(_)) => unreachable!(),        // handled in expr_call_type_constructor
        (_, Type::Map(_)) => unreachable!(),           // handled in expr_call_type_constructor
        (_, Type::Array(_)) => unreachable!(),         // handled in expr_call_type_constructor
        (_, Type::Generic(_)) => unreachable!(),       // handled in expr_call_type_constructor
        (_, Type::SelfContract(_)) => unreachable!(),  // contract names become Contract

        _ => {
            context.error(
                &format!(
                    "incorrect type for argument to `{}`",
                    into.display(context.db())
                ),
                from_expr.span,
                &format!(
                    "cannot cast type `{}` to type `{}`",
                    from.display(context.db()),
                    into.display(context.db()),
                ),
            );
        }
    };
}

pub fn deref_type(context: &mut dyn AnalyzerContext, expr: &Node<ast::Expr>, ty: TypeId) -> TypeId {
    match ty.typ(context.db()) {
        Type::SPtr(inner) => adjust_type(context, expr, inner, AdjustmentKind::FromStorage),
        // Type::Mut(inner) => {
        //     adjust_type(context, expr, inner, AdjustmentKind::DeMut);
        //     deref_type(context, expr, inner)
        // }
        _ => ty,
    }
}

fn adjust_type(
    context: &mut dyn AnalyzerContext,
    expr: &Node<ast::Expr>,
    into: TypeId,
    kind: AdjustmentKind,
) -> TypeId {
    context.update_expression(expr, &|attr: &mut ExpressionAttributes| {
        attr.type_adjustments.push(Adjustment { into, kind });
    });
    into
}

pub fn try_coerce_type(
    context: &mut dyn AnalyzerContext,
    from_expr: Option<&Node<ast::Expr>>,
    from: TypeId,
    into: TypeId,
) -> Result<TypeId, TypeCoercionError> {
    let chain = coerce(context, from_expr, from, into, vec![])?;
    if let Some(expr) = from_expr {
        context.update_expression(expr, &|attr: &mut ExpressionAttributes| {
            attr.type_adjustments.extend(chain.iter())
        });
    }
    Ok(into)
}

fn add_adjustment(
    mut chain: Vec<Adjustment>,
    into: TypeId,
    kind: AdjustmentKind,
) -> Vec<Adjustment> {
    chain.push(Adjustment { into, kind });
    chain
}

fn coerce(
    context: &mut dyn AnalyzerContext,
    from_expr: Option<&Node<ast::Expr>>,
    from: TypeId,
    into: TypeId,
    chain: Vec<Adjustment>,
) -> Result<Vec<Adjustment>, TypeCoercionError> {
    if from == into {
        return Ok(chain);
    }

    match (from.typ(context.db()), into.typ(context.db())) {
        (Type::SPtr(from), Type::SPtr(into)) => coerce(context, from_expr, from, into, chain),

        // XXX LOAD_PRIMITIVE_FROM_STO
        // Primitive types can be moved from storage implicitly.
        // Contract type is also a primitive.
        (Type::SPtr(from_inner), Type::Base(_) | Type::Contract(_)) => coerce(
            context,
            from_expr,
            from_inner,
            into,
            add_adjustment(chain, into, AdjustmentKind::FromStorage),
        ),

        // XXX STO_TO_MEM?
        // complex types require .to_mem()
        (Type::SPtr(from), _) => {
            // If the inner types are incompatible, report that error instead
            try_coerce_type(context, from_expr, from, into)?;
            Err(TypeCoercionError::RequiresToMem)
        }

        // All types can be moved into storage implicitly.
        // Note that no `Adjustment` is added here.
        (_, Type::SPtr(into)) => coerce(context, from_expr, from, into, chain),

        (
            Type::String(FeString { max_size: from_sz }),
            Type::String(FeString { max_size: into_sz }),
        ) => {
            if into_sz >= from_sz {
                Ok(add_adjustment(
                    chain,
                    into,
                    AdjustmentKind::StringSizeIncrease,
                ))
            } else {
                Err(TypeCoercionError::Incompatible)
            }
        }
        (Type::SelfContract(from), Type::Contract(into)) => {
            if from == into {
                Err(TypeCoercionError::SelfContractType)
            } else {
                Err(TypeCoercionError::Incompatible)
            }
        }

        (Type::Tuple(ftup), Type::Tuple(itup)) => {
            // If the rhs is a tuple expr, each element gets its own coercion chain.
            // Else, we don't allow coercion (for now, at least).
            if let Some(Node {
                kind: ast::Expr::Tuple { elts },
                ..
            }) = &from_expr
            {
                if ftup.items.len() == itup.items.len()
                    && elts
                        .iter()
                        .zip(ftup.items.iter().zip(itup.items.iter()))
                        .map(|(elt, (from, into))| {
                            try_coerce_type(context, Some(elt), *from, *into).is_ok()
                        })
                        .all(|x| x)
                {
                    // Update the type of the rhs tuple, because its elements
                    // have been coerced into the lhs element types.
                    context.update_expression(from_expr.unwrap(), &|attr| attr.typ = into);
                    return Ok(chain);
                }
            }
            Err(TypeCoercionError::Incompatible)
        }

        // XXX
        (Type::Base(Base::Numeric(f)), Type::Base(Base::Numeric(i))) => {
            if f.is_signed() == i.is_signed() && i.size() > f.size() {
                Ok(add_adjustment(chain, into, AdjustmentKind::IntSizeIncrease))
            } else {
                Err(TypeCoercionError::Incompatible)
            }
        }
        (_, _) => Err(TypeCoercionError::Incompatible),
    }
}

pub fn apply_generic_type_args(
    context: &mut dyn AnalyzerContext,
    generic: GenericType,
    name_span: Span,
    args: Option<&Node<Vec<ast::GenericArg>>>,
) -> Result<TypeId, TypeError> {
    let params = generic.params();

    let args = args.ok_or_else(|| {
        TypeError::new(context.fancy_error(
            &format!(
                "missing generic {} for type `{}`",
                pluralize_conditionally("argument", params.len()),
                generic.name()
            ),
            vec![Label::primary(
                name_span,
                &format!(
                    "expected {} generic {}",
                    params.len(),
                    pluralize_conditionally("argument", params.len())
                ),
            )],
            vec![friendly_generic_arg_example_string(generic)],
        ))
    })?;

    if let Some(diag) = validate_arg_count(
        context,
        &generic.name(),
        name_span,
        args,
        params.len(),
        "generic argument",
    ) {
        return Err(TypeError::new(diag));
    }

    let concrete_args = params
        .iter()
        .zip(args.kind.iter())
        .map(|(param, arg)| match (param.kind, arg) {
            (GenericParamKind::Int, ast::GenericArg::Int(int_node)) => {
                Ok(GenericArg::Int(int_node.kind))
            }

            (GenericParamKind::Int, ast::GenericArg::TypeDesc(_)) => {
                Err(TypeError::new(context.fancy_error(
                    &format!("`{}` {} must be an integer", generic.name(), param.name),
                    vec![Label::primary(arg.span(), "expected an integer")],
                    vec![],
                )))
            }

            (GenericParamKind::Int, ast::GenericArg::ConstExpr(expr)) => {
                // Performs semantic analysis on `expr`.
                super::expressions::expr(context, expr, None)?;

                // Evaluates expression.
                let const_value = super::const_expr::eval_expr(context, expr)?;

                // TODO: Fix me when `GenericArg` can represent literals not only `Int`.
                match const_value {
                    Constant::Int(val) => Ok(GenericArg::Int(val.try_into().unwrap())),
                    Constant::Bool(_) | Constant::Str(_) => Err(TypeError::new(
                        context.not_yet_implemented("non numeric type const generics", expr.span),
                    )),
                }
            }

            (GenericParamKind::PrimitiveType, ast::GenericArg::TypeDesc(type_node)) => {
                let typ = type_desc(context, type_node)?;
                if typ.is_base(context.db()) {
                    Ok(GenericArg::Type(typ))
                } else {
                    Err(TypeError::new(context.error(
                        &format!(
                            "`{}` {} must be a primitive type",
                            generic.name(),
                            param.name
                        ),
                        type_node.span,
                        &format!(
                            "this has type `{}`; expected a primitive type",
                            typ.display(context.db())
                        ),
                    )))
                }
            }

            (GenericParamKind::AnyType, ast::GenericArg::TypeDesc(type_node)) => {
                Ok(GenericArg::Type(type_desc(context, type_node)?))
            }

            (
                GenericParamKind::PrimitiveType | GenericParamKind::AnyType,
                ast::GenericArg::Int(_) | ast::GenericArg::ConstExpr(_),
            ) => Err(TypeError::new(context.fancy_error(
                &format!("`{}` {} must be a type", generic.name(), param.name),
                vec![Label::primary(arg.span(), "expected a type name")],
                vec![],
            ))),
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(generic
        .apply(context.db(), &concrete_args)
        .expect("failed to construct generic type after checking args"))
}

fn friendly_generic_arg_example_string(generic: GenericType) -> String {
    let example_args = generic
        .params()
        .iter()
        .map(|param| match param.kind {
            GenericParamKind::Int => "32",
            GenericParamKind::PrimitiveType => "u64",
            GenericParamKind::AnyType => "String<32>",
        })
        .collect::<Vec<&'static str>>();

    format!("Example: `{}<{}>`", generic.name(), example_args.join(", "))
}

pub fn resolve_concrete_type_name<T: std::fmt::Display>(
    context: &mut dyn AnalyzerContext,
    name: &str,
    base_desc: &Node<T>,
    generic_args: Option<&Node<Vec<ast::GenericArg>>>,
) -> Result<TypeId, TypeError> {
    let named_thing = context.resolve_name(name, base_desc.span)?;
    resolve_concrete_type_named_thing(context, named_thing, base_desc, generic_args)
}

pub fn resolve_concrete_type_path<T: std::fmt::Display>(
    context: &mut dyn AnalyzerContext,
    path: &ast::Path,
    base_desc: &Node<T>,
    generic_args: Option<&Node<Vec<ast::GenericArg>>>,
) -> Result<TypeId, TypeError> {
    let named_thing = context.resolve_path(path, base_desc.span)?;
    resolve_concrete_type_named_thing(context, Some(named_thing), base_desc, generic_args)
}

pub fn resolve_concrete_type_named_thing<T: std::fmt::Display>(
    context: &mut dyn AnalyzerContext,
    named_thing: Option<NamedThing>,
    base_desc: &Node<T>,
    generic_args: Option<&Node<Vec<ast::GenericArg>>>,
) -> Result<TypeId, TypeError> {
    match named_thing {
        Some(NamedThing::Item(Item::Type(id))) => {
            if let Some(args) = generic_args {
                context.fancy_error(
                    &format!("`{}` type is not generic", base_desc.kind),
                    vec![Label::primary(
                        args.span,
                        "unexpected generic argument list",
                    )],
                    vec![],
                );
            }
            id.type_id(context.db())
        }
        Some(NamedThing::Item(Item::GenericType(generic))) => {
            apply_generic_type_args(context, generic, base_desc.span, generic_args)
        }
        Some(named_thing) => Err(TypeError::new(context.fancy_error(
            &format!("`{}` is not a type name", base_desc.kind),
            if let Some(def_span) = named_thing.name_span(context.db()) {
                vec![
                    Label::primary(
                        def_span,
                        format!(
                            "`{}` is defined here as a {}",
                            base_desc.kind,
                            named_thing.item_kind_display_name()
                        ),
                    ),
                    Label::primary(
                        base_desc.span,
                        format!("`{}` is used here as a type", base_desc.kind),
                    ),
                ]
            } else {
                vec![Label::primary(
                    base_desc.span,
                    format!(
                        "`{}` is a {}",
                        base_desc.kind,
                        named_thing.item_kind_display_name()
                    ),
                )]
            },
            vec![],
        ))),
        None => Err(TypeError::new(context.error(
            "undefined type",
            base_desc.span,
            &format!("`{}` has not been defined", base_desc.kind),
        ))),
    }
}

/// Maps a type description node to an enum type.
pub fn type_desc(
    context: &mut dyn AnalyzerContext,
    desc: &Node<ast::TypeDesc>,
) -> Result<TypeId, TypeError> {
    match &desc.kind {
        ast::TypeDesc::Base { base } => resolve_concrete_type_name(context, base, desc, None),
        ast::TypeDesc::Path(path) => resolve_concrete_type_path(context, path, desc, None),
        // generic will need to allow for paths too
        ast::TypeDesc::Generic { base, args } => {
            resolve_concrete_type_name(context, &base.kind, base, Some(args))
        }
        ast::TypeDesc::Tuple { items } => {
            let types = items
                .iter()
                .map(|typ| match type_desc(context, typ) {
                    Ok(typ) if typ.has_fixed_size(context.db()) => Ok(typ),
                    Err(e) => Err(e),
                    _ => Err(TypeError::new(context.error(
                        "tuple elements must have fixed size",
                        typ.span,
                        "this can't be stored in a tuple",
                    ))),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(context.db().intern_type(Type::Tuple(Tuple {
                items: types.into(),
            })))
        }
        ast::TypeDesc::Unit => Ok(TypeId::unit(context.db())),
    }
}

/// Maps a type description node to a `TraitId`.
pub fn type_desc_to_trait(
    context: &mut dyn AnalyzerContext,
    desc: &Node<ast::TypeDesc>,
) -> Result<TraitId, TypeError> {
    match &desc.kind {
        ast::TypeDesc::Base { base } => {
            let named_thing = context.resolve_name(base, desc.span)?;
            resolve_concrete_trait_named_thing(context, named_thing, desc)
        }
        ast::TypeDesc::Path(path) => {
            let named_thing = context.resolve_path(path, desc.span)?;
            resolve_concrete_trait_named_thing(context, Some(named_thing), desc)
        }
        // generic will need to allow for paths too
        ast::TypeDesc::Generic { base, .. } => {
            let named_thing = context.resolve_name(&base.kind, desc.span)?;
            resolve_concrete_trait_named_thing(context, named_thing, desc)
        }
        _ => panic!("Should be rejected by parser"),
    }
}

pub fn resolve_concrete_trait_named_thing<T: std::fmt::Display>(
    context: &mut dyn AnalyzerContext,
    val: Option<NamedThing>,
    base_desc: &Node<T>,
) -> Result<TraitId, TypeError> {
    match val {
        Some(NamedThing::Item(Item::Trait(treit))) => Ok(treit),
        Some(NamedThing::Item(Item::Type(ty))) => Err(TypeError::new(context.error(
            &format!("expected trait, found type `{}`", ty.name(context.db())),
            base_desc.span,
            "not a trait",
        ))),
        Some(named_thing) => Err(TypeError::new(context.fancy_error(
            &format!("`{}` is not a trait name", base_desc.kind),
            if let Some(def_span) = named_thing.name_span(context.db()) {
                vec![
                    Label::primary(
                        def_span,
                        format!(
                            "`{}` is defined here as a {}",
                            base_desc.kind,
                            named_thing.item_kind_display_name()
                        ),
                    ),
                    Label::primary(
                        base_desc.span,
                        format!("`{}` is used here as a trait", base_desc.kind),
                    ),
                ]
            } else {
                vec![Label::primary(
                    base_desc.span,
                    format!(
                        "`{}` is a {}",
                        base_desc.kind,
                        named_thing.item_kind_display_name()
                    ),
                )]
            },
            vec![],
        ))),
        None => Err(TypeError::new(context.error(
            "undefined trait",
            base_desc.span,
            &format!("`{}` has not been defined", base_desc.kind),
        ))),
    }
}
