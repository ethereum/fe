use crate::lowering::context::Context;
use crate::lowering::names;
use fe_analyzer::namespace::types::TypeDowncast;
use fe_parser::ast::{GenericArg, TypeDesc};
use fe_parser::node::Node;

pub fn type_desc(context: &mut Context, desc: Node<TypeDesc>) -> Node<TypeDesc> {
    match desc.kind {
        TypeDesc::Unit | TypeDesc::Base { .. } => desc,

        TypeDesc::Tuple { items } => {
            let typ = context
                .module
                .analysis
                .get_type_desc(desc.id)
                .expect("missing type desc type")
                .as_tuple()
                .expect("expected tuple type");

            for item in items.into_iter() {
                type_desc(context, item);
            }
            context.module.tuples.insert(typ.clone());
            Node::new(
                TypeDesc::Base {
                    base: names::tuple_struct_name(typ),
                },
                desc.span,
            )
        }

        TypeDesc::Array { typ, dimension } => Node::new(
            TypeDesc::Array {
                typ: Box::new(type_desc(context, *typ)),
                dimension,
            },
            desc.span,
        ),

        TypeDesc::Generic { base, args } => Node::new(
            TypeDesc::Generic {
                base,
                args: Node::new(
                    args.kind
                        .into_iter()
                        .map(|arg| match arg {
                            GenericArg::Int(_) => arg,
                            GenericArg::TypeDesc(node) => {
                                GenericArg::TypeDesc(type_desc(context, node))
                            }
                        })
                        .collect(),
                    args.span,
                ),
            },
            desc.span,
        ),
    }
}
