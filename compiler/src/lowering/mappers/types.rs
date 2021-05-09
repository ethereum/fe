use crate::lowering::names;
use fe_analyzer::context::Context;
use fe_analyzer::namespace::types::Type;
use fe_common::Spanned;
use fe_parser::ast as fe;
use fe_parser::node::Node;

pub fn type_desc(context: &Context, desc: Node<fe::TypeDesc>) -> Node<fe::TypeDesc> {
    let typ = context.get_type_desc(&desc).expect("missing attributes");

    match typ {
        Type::Tuple(tuple) => Node::new(names::tuple_struct_type_desc(tuple), desc.span),
        Type::Map(map) => match &*map.value {
            Type::Tuple(tuple) => {
                if let fe::TypeDesc::Generic { base, args } = desc.kind {
                    let new_args = vec![
                        args.kind[0].clone(),
                        fe::GenericArg::TypeDesc(Node::new(
                            names::tuple_struct_type_desc(&tuple),
                            args.kind[1].span(),
                        )),
                    ];
                    Node::new(
                        fe::TypeDesc::Generic {
                            base,
                            args: Node::new(new_args, args.span),
                        },
                        desc.span,
                    )
                } else {
                    unreachable!()
                }
            }
            _ => desc,
        },
        _ => desc,
    }
}
