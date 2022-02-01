use crate::errors::TypeError;
use crate::namespace::items::Item;
use crate::namespace::types::{FixedSize, GenericArg, GenericParamKind, GenericType, Tuple, Type};
use crate::traversal::call_args::validate_arg_count;
use crate::{
    context::{AnalyzerContext, NamedThing},
    traversal::const_expr::Constant,
};
use fe_common::diagnostics::Label;
use fe_common::utils::humanize::pluralize_conditionally;
use fe_common::Spanned;
use fe_parser::ast;
use fe_parser::node::{Node, Span};
use vec1::Vec1;

pub fn apply_generic_type_args(
    context: &mut dyn AnalyzerContext,
    generic: GenericType,
    name_span: Span,
    args: Option<&Node<Vec<ast::GenericArg>>>,
) -> Result<Type, TypeError> {
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
                super::expressions::expr(context, expr, None)
                    .map_err(|err| TypeError::new(err.0))?;

                // Evaluates expression.
                let const_value =
                    super::const_expr::eval_expr(context, expr).map_err(|err| TypeError(err.0))?;

                // TODO: Fix me when `GenericArg` can represent literals not only `Int`.
                match const_value {
                    Constant::Int(val) => Ok(GenericArg::Int(val.try_into().unwrap())),
                    Constant::Bool(_) | Constant::Str(_) => {
                        todo!()
                    }
                }
            }

            (GenericParamKind::PrimitiveType, ast::GenericArg::TypeDesc(type_node)) => {
                match type_desc(context, type_node)? {
                    Type::Base(base) => Ok(GenericArg::Type(Type::Base(base))),
                    typ => Err(TypeError::new(context.error(
                        &format!(
                            "`{}` {} must be a primitive type",
                            generic.name(),
                            param.name
                        ),
                        type_node.span,
                        &format!("this has type `{}`; expected a primitive type", typ),
                    ))),
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
        .apply(&concrete_args)
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
) -> Result<Type, TypeError> {
    let named_thing = context.resolve_name(name);
    resolve_concrete_type_named_thing(context, named_thing, base_desc, generic_args)
}

pub fn resolve_concrete_type_path<T: std::fmt::Display>(
    context: &mut dyn AnalyzerContext,
    path: &ast::Path,
    base_desc: &Node<T>,
    generic_args: Option<&Node<Vec<ast::GenericArg>>>,
) -> Result<Type, TypeError> {
    let named_thing = context.resolve_path(path);
    resolve_concrete_type_named_thing(context, named_thing, base_desc, generic_args)
}

pub fn resolve_concrete_type_named_thing<T: std::fmt::Display>(
    context: &mut dyn AnalyzerContext,
    named_thing: Option<NamedThing>,
    base_desc: &Node<T>,
    generic_args: Option<&Node<Vec<ast::GenericArg>>>,
) -> Result<Type, TypeError> {
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
            id.typ(context.db())
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
) -> Result<Type, TypeError> {
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
                .map(|typ| match FixedSize::try_from(type_desc(context, typ)?) {
                    Ok(typ) => Ok(typ),
                    Err(_) => Err(TypeError::new(context.error(
                        "tuple elements must have fixed size",
                        typ.span,
                        "this can't be stored in a tuple",
                    ))),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Type::Tuple(Tuple {
                items: Vec1::try_from_vec(types).expect("tuple is empty"),
            }))
        }
        ast::TypeDesc::Unit => Ok(Type::unit()),
    }
}
