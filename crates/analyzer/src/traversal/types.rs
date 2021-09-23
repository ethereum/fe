use crate::context::AnalyzerContext;
use crate::errors::TypeError;
use crate::namespace::types::{Array, Base, FeString, FixedSize, Map, Tuple, Type};
use crate::traversal::call_args::validate_arg_count;
use fe_common::diagnostics::Label;
use fe_parser::ast as fe;
use fe_parser::node::{Node, Span};
use std::convert::TryFrom;
use std::str::FromStr;
use vec1::Vec1;

pub fn resolve_type_name(
    context: &mut dyn AnalyzerContext,
    name: &str,
    name_span: Span,
    generic_args: Option<&Node<Vec<fe::GenericArg>>>,
) -> Option<Result<Type, TypeError>> {
    match (name, generic_args) {
        ("String", Some(args)) => match &args.kind[..] {
            [fe::GenericArg::Int(len)] => Some(Ok(Type::String(FeString { max_size: len.kind }))),
            _ => Some(Err(TypeError::new(context.fancy_error(
                "invalid `String` type argument",
                vec![Label::primary(args.span, "expected an integer")],
                vec!["Example: String<100>".into()],
            )))),
        },
        ("String", None) => Some(Err(TypeError::new(context.fancy_error(
            "`String` type requires a max size argument",
            vec![Label::primary(name_span, "")],
            vec!["Example: String<100>".into()],
        )))),
        ("Map", Some(args)) => {
            let diag_voucher = validate_arg_count(context, name, name_span, args, 2);

            let key = match args.kind.get(0) {
                Some(fe::GenericArg::TypeDesc(type_node)) => match type_desc(context, type_node) {
                    Ok(Type::Base(base)) => base,
                    Err(err) => return Some(Err(err)),
                    _ => {
                        return Some(Err(TypeError::new(context.error(
                            "`Map` key must be a primitive type",
                            type_node.span,
                            "this can't be used as a Map key",
                        ))));
                    }
                },
                Some(fe::GenericArg::Int(node)) => {
                    return Some(Err(TypeError::new(context.error(
                        "`Map` key must be a type",
                        node.span,
                        "this should be a type name",
                    ))));
                }
                None => return Some(Err(TypeError::new(diag_voucher.unwrap()))),
            };
            let value = match args.kind.get(1) {
                Some(fe::GenericArg::TypeDesc(type_node)) => match type_desc(context, type_node) {
                    Ok(typ) => typ,
                    Err(err) => return Some(Err(err)),
                },
                Some(fe::GenericArg::Int(node)) => {
                    return Some(Err(TypeError::new(context.error(
                        "`Map` value must be a type",
                        node.span,
                        "this should be a type name",
                    ))));
                }
                None => return Some(Err(TypeError::new(diag_voucher.unwrap()))),
            };
            Some(Ok(Type::Map(Map {
                key,
                value: Box::new(value),
            })))
        }
        ("Map", None) => Some(Err(TypeError::new(context.fancy_error(
            "`Map` type requires key and value type arguments",
            vec![Label::primary(name_span, "")],
            vec!["Example: Map<address, u256>".into()],
        )))),
        (_, _) => {
            let typ = if let Ok(base_type) = Base::from_str(name) {
                Type::Base(base_type)
            } else {
                match context.resolve_type(name) {
                    Some(Ok(typ)) => typ,
                    Some(Err(err)) => return Some(Err(err)),
                    None => return None,
                }
            };

            if let Some(args) = generic_args {
                // User-defined types can't be generic yet
                context.fancy_error(
                    &format!("`{}` type is not generic", typ),
                    vec![Label::primary(args.span, "unexpected type argument list")],
                    vec![format!("Hint: use `{}`", typ)],
                );
            }
            Some(Ok(typ))
        }
    }
}

fn resolve_type_name_or_err(
    context: &mut dyn AnalyzerContext,
    name: &str,
    name_span: Span,
    generic_args: Option<&Node<Vec<fe::GenericArg>>>,
) -> Result<Type, TypeError> {
    if let Some(typ) = resolve_type_name(context, name, name_span, generic_args) {
        typ
    } else {
        Err(TypeError::new(context.error(
            "undefined type",
            name_span,
            "this type name has not been defined",
        )))
    }
}

/// Maps a type description node to an enum type.
pub fn type_desc(
    context: &mut dyn AnalyzerContext,
    desc: &Node<fe::TypeDesc>,
) -> Result<Type, TypeError> {
    match &desc.kind {
        fe::TypeDesc::Base { base } => resolve_type_name_or_err(context, base, desc.span, None),
        fe::TypeDesc::Array { typ, dimension } => {
            if let Type::Base(base) = type_desc(context, typ)? {
                Ok(Type::Array(Array {
                    inner: base,
                    size: *dimension,
                }))
            } else {
                Err(TypeError::new(context.error(
                    "arrays can only hold primitive types",
                    typ.span,
                    "can't be stored in an array",
                )))
            }
        }
        fe::TypeDesc::Generic { base, args } => {
            resolve_type_name_or_err(context, &base.kind, base.span, Some(args))
        }
        fe::TypeDesc::Tuple { items } => {
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
        fe::TypeDesc::Unit => Ok(Type::unit()),
    }
}
