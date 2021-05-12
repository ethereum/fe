use crate::lowering::names;
use fe_analyzer::context::Context;
use fe_analyzer::namespace::types::Type;
use fe_parser::ast as fe;
use fe_parser::node::Node;

pub fn type_desc(context: &Context, desc: Node<fe::TypeDesc>) -> Node<fe::TypeDesc> {
    let typ = context.get_type_desc(&desc).expect("missing attributes");

    match typ {
        Type::Tuple(tuple) => Node::new(names::tuple_struct_type_desc(tuple), desc.span),
        _ => desc,
    }
}
