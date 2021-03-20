use fe_analyzer::namespace::types::Type;
use fe_analyzer::Context;

use fe_parser::ast as fe;
use fe_parser::node::{
    Node,
    Span,
};

pub fn module(context: &Context, module: &fe::Module) -> fe::Module {
    let attributes = context.get_module()?;

    let tuples_used = attributes
        .type_defs
        .values()
        .into_iter()
        .filter_map(|typ| match typ {
            Type::Tuple(tuple) => Some(tuple),
            _ => None,
        });
}

fn struct_def_from_tuple() -> fe::ModuleStmt {
    fe::ModuleStmt::StructDef {
        name: Node::new("my_tuple", Span::new(0, 0)),
        body: vec![],
    }
}
