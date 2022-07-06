use crate::context::{AnalyzerContext, Constant, NamedThing};
use crate::display::Displayable;
use crate::errors::TypeError;
use crate::namespace::items::{Item, TraitId};
use crate::namespace::types::{GenericArg, GenericParamKind, GenericType, Tuple, Type, TypeId};
use crate::traversal::call_args::validate_arg_count;
use fe_common::diagnostics::Label;
use fe_common::utils::humanize::pluralize_conditionally;
use fe_common::Spanned;
use fe_parser::ast;
use fe_parser::node::{Node, Span};

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
    let named_thing = context.resolve_path(path, base_desc.span);
    resolve_concrete_type_named_thing(context, named_thing, base_desc, generic_args)
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
            let named_thing = context.resolve_path(path, desc.span);
            resolve_concrete_trait_named_thing(context, named_thing, desc)
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
