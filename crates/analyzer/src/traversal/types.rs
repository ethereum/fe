use crate::context::Context;
use crate::errors::{FatalError, TypeError};
use crate::namespace::scopes::Scope;
use crate::namespace::types::{Array, Base, FeString, FixedSize, Integer, Map, Tuple, Type};
use crate::traversal::expressions::validate_arg_count;
use fe_common::diagnostics::Label;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::convert::TryFrom;
use vec1::Vec1;

/// Maps a type description node to an enum type.
pub fn type_desc(
    scope: &Scope,
    context: &mut Context,
    desc: &Node<fe::TypeDesc>,
) -> Result<Type, FatalError> {
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
            "bytes" => Type::Base(Byte),
            "address" => Type::Base(Address),
            base => {
                if let Some(typ) = scope.module_scope().borrow().type_defs.get(base) {
                    typ.clone()
                } else {
                    context.error(
                        "undefined type",
                        desc.span,
                        "this type name has not been defined",
                    );
                    return Err(FatalError);
                }
            }
        },
        fe::TypeDesc::Array { typ, dimension } => {
            if let Type::Base(base) = type_desc(scope, context, &typ)? {
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
                return Err(FatalError);
            }
        }
        fe::TypeDesc::Generic { base, args } => match base.kind.as_str() {
            "Map" => {
                validate_arg_count(context, &base.kind, base.span, &args, 2);
                if args.kind.len() < 2 {
                    return Err(FatalError);
                }
                match &args.kind[..2] {
                    [fe::GenericArg::TypeDesc(from), fe::GenericArg::TypeDesc(to)] => {
                        let key_type = type_desc(scope, context, &from)?;
                        if let Type::Base(base) = key_type {
                            Type::Map(Map {
                                key: base,
                                value: Box::new(type_desc(scope, context, &to)?),
                            })
                        } else {
                            context.error(
                                "`map` key must be a primitive type",
                                from.span,
                                "this can't be used as a map key",
                            );
                            return Err(FatalError);
                        }
                    }
                    _ => {
                        for arg in &args.kind[..2] {
                            if let fe::GenericArg::Int(node) = arg {
                                context.error(
                                    "`map` key and value must be types",
                                    node.span,
                                    "this should be a type name",
                                );
                            }
                        }
                        return Err(FatalError);
                    }
                }
            }
            "String" => match &args.kind[..] {
                [fe::GenericArg::Int(len)] => Type::String(FeString { max_size: len.kind }),
                _ => {
                    context.fancy_error(
                        "Numeric generic type parameter expected",
                        vec![Label::primary(args.span, "invalid type parameter")],
                        vec!["Example: String<100>".into()],
                    );
                    return Err(FatalError);
                }
            },
            _ => {
                context.error(
                    "undefined generic type",
                    base.span,
                    "this type name has not been defined",
                );
                return Err(FatalError);
            }
        },
        fe::TypeDesc::Tuple { items } => Type::Tuple(Tuple {
            items: Vec1::try_from_vec(
                items
                    .iter()
                    .map(|typ| type_desc_fixed_size(scope, context, &typ))
                    .collect::<Result<_, _>>()?,
            )
            .expect("tuple is empty"),
        }),
        fe::TypeDesc::Unit => Type::unit(),
    };

    context.add_type_desc(desc, typ.clone());
    Ok(typ)
}

/// Maps a type description node to a fixed size enum type.
pub fn type_desc_fixed_size(
    scope: &Scope,
    context: &mut Context,
    desc: &Node<fe::TypeDesc>,
) -> Result<FixedSize, FatalError> {
    match FixedSize::try_from(type_desc(scope, context, desc)?) {
        Err(TypeError) => {
            context.error("Expected a value with a fixed size", desc.span, "");
            Err(FatalError)
        }
        Ok(val) => Ok(val),
    }
}
