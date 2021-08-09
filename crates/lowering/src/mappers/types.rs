use crate::context::ModuleContext;
use crate::names;
use crate::utils::ZeroSpanNode;
use fe_analyzer::namespace::types::{Base, Map, Type, TypeDowncast};
use fe_parser::ast::{GenericArg, TypeDesc};
use fe_parser::node::Node;

pub fn type_desc(module: &mut ModuleContext, typ: &Type) -> Node<TypeDesc> {
    match typ {
        Type::Base(Base::Unit) => TypeDesc::Unit,
        Type::Base(base) => TypeDesc::Base {
            base: names::base_type_name(base),
        },
        Type::Array(array) => TypeDesc::Array {
            dimension: array.size,
            typ: Box::new(type_desc(module, &array.inner.into())),
        },
        Type::Tuple(tuple) => {
            let typ = TypeDesc::Base {
                base: names::tuple_struct_name(tuple),
            };
            module.add_tuple(tuple.clone());
            typ
        }
        Type::String(string) => TypeDesc::Generic {
            base: "String".to_string().into_node(),
            args: vec![GenericArg::Int(string.max_size.into_node())].into_node(),
        },
        Type::Contract(contract) => TypeDesc::Base {
            base: contract.name.clone(),
        },
        Type::Struct(strukt) => TypeDesc::Base {
            base: strukt.name.clone(),
        },
        Type::Map(Map { key, value }) => TypeDesc::Generic {
            base: "Map".to_string().into_node(),
            args: vec![
                GenericArg::TypeDesc(type_desc(module, &Type::Base(*key))),
                GenericArg::TypeDesc(type_desc(module, value)),
            ]
            .into_node(),
        },
    }
    .into_node()
}

pub fn type_desc1(context: &mut ModuleContext, desc: Node<TypeDesc>, typ: &Type) -> Node<TypeDesc> {
    match desc.kind {
        TypeDesc::Unit | TypeDesc::Base { .. } => desc,

        TypeDesc::Tuple { items } => {
            let typ = typ.as_tuple().expect("expected tuple type");

            for (item_desc, item_type) in items.into_iter().zip(typ.items.iter()) {
                type_desc1(context, item_desc, &item_type.clone().into());
            }
            context.add_tuple(typ.clone());
            Node::new(
                TypeDesc::Base {
                    base: names::tuple_struct_name(typ),
                },
                desc.span,
            )
        }

        TypeDesc::Array {
            typ: desc,
            dimension,
        } => {
            let inner_type = typ.as_array().expect("expected array type").inner;
            let span = desc.span;
            Node::new(
                TypeDesc::Array {
                    typ: Box::new(type_desc1(context, *desc, &Type::Base(inner_type))),
                    dimension,
                },
                span,
            )
        }

        TypeDesc::Generic { base, args } => Node::new(
            TypeDesc::Generic {
                base,
                args: Node::new(
                    args.kind
                        .into_iter()
                        .enumerate()
                        .map(|(idx, arg)| match arg {
                            GenericArg::Int(_) => arg,
                            GenericArg::TypeDesc(node) => GenericArg::TypeDesc(type_desc1(
                                context,
                                node,
                                &typ.generic_arg_type(idx)
                                    .expect("expected generic type arg"),
                            )),
                        })
                        .collect(),
                    args.span,
                ),
            },
            desc.span,
        ),
    }
}
