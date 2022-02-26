use crate::context::ModuleContext;
use crate::mappers::{functions, types};
use fe_analyzer::namespace::items::{StructFieldId, StructId};
use fe_parser::ast;
use fe_parser::node::Node;

pub fn struct_def(context: &mut ModuleContext, struct_: StructId) -> Node<ast::Struct> {
    let db = context.db;

    let fields = struct_
        .fields(db)
        .values()
        .map(|field| struct_field(context, *field))
        .collect();

    let functions = struct_
        .functions(db)
        .values()
        .map(|function| functions::func_def(context, *function))
        .collect();

    let node = &struct_.data(context.db).ast;
    Node::new(
        ast::Struct {
            name: node.kind.name.clone(),
            fields,
            functions,
            pub_qual: node.kind.pub_qual,
        },
        node.span,
    )
}

fn struct_field(context: &mut ModuleContext, field: StructFieldId) -> Node<ast::Field> {
    let node = &field.data(context.db).ast;
    let typ = field.typ(context.db).expect("struct field type error");
    Node::new(
        ast::Field {
            is_pub: node.kind.is_pub,
            is_const: node.kind.is_const,
            name: node.kind.name.clone(),
            typ: types::type_desc(context, node.kind.typ.clone(), &typ.into()),
            value: node.kind.value.clone(),
        },
        node.span,
    )
}
