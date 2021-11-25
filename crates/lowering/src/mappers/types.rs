use crate::context::ModuleContext;
use crate::names;
use fe_analyzer::namespace::types::{Type, TypeDowncast};
use fe_parser::ast::{GenericArg, TypeDesc};
use fe_parser::node::Node;

pub fn type_desc(context: &mut ModuleContext, desc: Node<TypeDesc>, typ: &Type) -> Node<TypeDesc> {
    match desc.kind {
        TypeDesc::Unit | TypeDesc::Base { .. } | TypeDesc::Path(_) => desc,

        TypeDesc::Tuple { items } => {
            let typ = typ.as_tuple().expect("expected tuple type");

            for (item_desc, item_type) in items.into_iter().zip(typ.items.iter()) {
                type_desc(context, item_desc, &item_type.clone().into());
            }
            context.tuples.insert(typ.clone());
            Node::new(
                TypeDesc::Base {
                    base: names::tuple_struct_name(typ),
                },
                desc.span,
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
                            GenericArg::TypeDesc(node) => GenericArg::TypeDesc(type_desc(
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
