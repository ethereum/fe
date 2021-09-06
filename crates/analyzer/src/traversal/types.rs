use crate::context::AnalyzerContext;
use crate::errors::TypeError;
use crate::namespace::types::{Array, Base, FeString, FixedSize, Map, Tuple, Type};
use crate::traversal::call_args::validate_arg_count;
use fe_common::diagnostics::Label;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::convert::TryFrom;
use std::str::FromStr;
use vec1::Vec1;

/// Maps a type description node to an enum type.
pub fn type_desc(
    context: &mut dyn AnalyzerContext,
    desc: &Node<fe::TypeDesc>,
) -> Result<Type, TypeError> {
    let typ = match &desc.kind {
        fe::TypeDesc::Base { base } => {
            if let Ok(base_type) = Base::from_str(base) {
                Type::Base(base_type)
            } else {
                match base.as_str() {
                    "String" => {
                        return Err(TypeError::new(context.fancy_error(
                            "`String` type requires a max size argument",
                            vec![Label::primary(desc.span, "")],
                            vec!["Example: String<100>".into()],
                        )))
                    }
                    "Map" => {
                        return Err(TypeError::new(context.fancy_error(
                            "`Map` type requires key and value type arguments",
                            vec![Label::primary(desc.span, "")],
                            vec!["Example: Map<address, u256>".into()],
                        )))
                    }
                    _ => {
                        if let Some(typ) = context.resolve_type(base) {
                            typ?
                        } else {
                            return Err(TypeError::new(context.error(
                                "undefined type",
                                desc.span,
                                "this type name has not been defined",
                            )));
                        }
                    }
                }
            }
        }
        fe::TypeDesc::Array { typ, dimension } => {
            if let Type::Base(base) = type_desc(context, typ)? {
                Type::Array(Array {
                    inner: base,
                    size: *dimension,
                })
            } else {
                return Err(TypeError::new(context.error(
                    "arrays can only hold primitive types",
                    typ.span,
                    "can't be stored in an array",
                )));
            }
        }
        fe::TypeDesc::Generic { base, args } => match base.kind.as_str() {
            "Map" => {
                let diag_voucher = validate_arg_count(context, &base.kind, base.span, args, 2);

                let key = match args.kind.get(0) {
                    Some(fe::GenericArg::TypeDesc(type_node)) => {
                        match type_desc(context, type_node)? {
                            Type::Base(base) => base,
                            _ => {
                                return Err(TypeError::new(context.error(
                                    "`Map` key must be a primitive type",
                                    type_node.span,
                                    "this can't be used as a Map key",
                                )));
                            }
                        }
                    }
                    Some(fe::GenericArg::Int(node)) => {
                        return Err(TypeError::new(context.error(
                            "`Map` key must be a type",
                            node.span,
                            "this should be a type name",
                        )));
                    }
                    None => return Err(TypeError::new(diag_voucher.unwrap())),
                };
                let value = match args.kind.get(1) {
                    Some(fe::GenericArg::TypeDesc(type_node)) => type_desc(context, type_node)?,
                    Some(fe::GenericArg::Int(node)) => {
                        return Err(TypeError::new(context.error(
                            "`Map` value must be a type",
                            node.span,
                            "this should be a type name",
                        )));
                    }
                    None => return Err(TypeError::new(diag_voucher.unwrap())),
                };

                Type::Map(Map {
                    key,
                    value: Box::new(value),
                })
            }
            "String" => match &args.kind[..] {
                [fe::GenericArg::Int(len)] => Type::String(FeString { max_size: len.kind }),
                _ => {
                    return Err(TypeError::new(context.fancy_error(
                        "invalid `String` type argument",
                        vec![Label::primary(args.span, "expected an integer")],
                        vec!["Example: String<100>".into()],
                    )));
                }
            },
            other => {
                if let Ok(base_type) = Base::from_str(other) {
                    return Err(TypeError::new(context.fancy_error(
                        &format!("`{}` type is not generic", base_type),
                        vec![Label::primary(args.span, "unexpected type argument list")],
                        vec![format!("Hint: use `{}`", base_type)],
                    )));
                }
                return Err(TypeError::new(context.error(
                    "undefined generic type",
                    base.span,
                    "this type name has not been defined",
                )));
            }
        },
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
            Type::Tuple(Tuple {
                items: Vec1::try_from_vec(types).expect("tuple is empty"),
            })
        }
        fe::TypeDesc::Unit => Type::unit(),
    };

    Ok(typ)
}
