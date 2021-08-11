use crate::context::AnalyzerContext;
use crate::errors::TypeError;
use crate::namespace::types::{Array, Base, FeString, FixedSize, Integer, Map, Tuple, Type};
use crate::traversal::call_args::validate_arg_count;
use fe_common::diagnostics::Label;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::convert::TryFrom;
use vec1::Vec1;

/// Maps a type description node to an enum type.
pub fn type_desc(
    context: &mut dyn AnalyzerContext,
    desc: &Node<fe::TypeDesc>,
) -> Result<Type, TypeError> {
    use Base::*;
    use Integer::*;

    let typ = match &desc.kind {
        fe::TypeDesc::Base { base } => match base.as_str() {
            "u256" => Type::Base(Numeric(U256)),
            "u128" => Type::Base(Numeric(U128)),
            "u64" => Type::Base(Numeric(U64)),
            "u32" => Type::Base(Numeric(U32)),
            "u16" => Type::Base(Numeric(U16)),
            "u8" => Type::Base(Numeric(U8)),
            "i256" => Type::Base(Numeric(I256)),
            "i128" => Type::Base(Numeric(I128)),
            "i64" => Type::Base(Numeric(I64)),
            "i32" => Type::Base(Numeric(I32)),
            "i16" => Type::Base(Numeric(I16)),
            "i8" => Type::Base(Numeric(I8)),
            "bool" => Type::Base(Bool),
            "address" => Type::Base(Address),
            base => {
                if let Some(typ) = context.resolve_type(base) {
                    typ?
                } else {
                    context.error(
                        "undefined type",
                        desc.span,
                        "this type name has not been defined",
                    );
                    return Err(TypeError);
                }
            }
        },
        fe::TypeDesc::Array { typ, dimension } => {
            if let Type::Base(base) = type_desc(context, typ)? {
                Type::Array(Array {
                    inner: base,
                    size: *dimension,
                })
            } else {
                context.error(
                    "arrays can only hold primitive types",
                    typ.span,
                    "can't be stored in an array",
                );
                return Err(TypeError);
            }
        }
        fe::TypeDesc::Generic { base, args } => match base.kind.as_str() {
            "Map" => {
                validate_arg_count(context, &base.kind, base.span, args, 2);

                let key = match args.kind.get(0) {
                    Some(fe::GenericArg::TypeDesc(type_node)) => {
                        match type_desc(context, type_node)? {
                            Type::Base(base) => base,
                            _ => {
                                context.error(
                                    "`Map` key must be a primitive type",
                                    type_node.span,
                                    "this can't be used as a Map key",
                                );
                                return Err(TypeError);
                            }
                        }
                    }
                    Some(fe::GenericArg::Int(node)) => {
                        context.error(
                            "`Map` key must be a type",
                            node.span,
                            "this should be a type name",
                        );
                        return Err(TypeError);
                    }
                    None => return Err(TypeError),
                };
                let value = match args.kind.get(1) {
                    Some(fe::GenericArg::TypeDesc(type_node)) => type_desc(context, type_node)?,
                    Some(fe::GenericArg::Int(node)) => {
                        context.error(
                            "`Map` value must be a type",
                            node.span,
                            "this should be a type name",
                        );
                        return Err(TypeError);
                    }
                    None => return Err(TypeError),
                };

                Type::Map(Map {
                    key,
                    value: Box::new(value),
                })
            }
            "String" => match &args.kind[..] {
                [fe::GenericArg::Int(len)] => Type::String(FeString { max_size: len.kind }),
                _ => {
                    context.fancy_error(
                        "Numeric generic type parameter expected",
                        vec![Label::primary(args.span, "invalid type parameter")],
                        vec!["Example: String<100>".into()],
                    );
                    return Err(TypeError);
                }
            },
            _ => {
                context.error(
                    "undefined generic type",
                    base.span,
                    "this type name has not been defined",
                );
                return Err(TypeError);
            }
        },
        fe::TypeDesc::Tuple { items } => {
            let types = items
                .iter()
                .map(|typ| match FixedSize::try_from(type_desc(context, typ)?) {
                    Ok(typ) => Ok(typ),
                    Err(_) => {
                        context.error(
                            "tuple elements must have fixed size",
                            typ.span,
                            "this can't be stored in a tuple",
                        );
                        Err(TypeError)
                    }
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
