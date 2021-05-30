use fe_analyzer::context::Context;
use fe_analyzer::namespace::types::Tuple;

use crate::lowering::mappers::contracts;
use crate::lowering::names;
use fe_parser::ast as fe;
use fe_parser::node::{Node, Span};

/// Lowers a module.
pub fn module(context: &mut Context, module: fe::Module) -> fe::Module {
    let lowered_body = module
        .body
        .into_iter()
        .map(|stmt| match &stmt.kind {
            fe::ModuleStmt::Pragma { .. } => stmt,
            fe::ModuleStmt::TypeDef { .. } => stmt,
            fe::ModuleStmt::StructDef { .. } => stmt,
            fe::ModuleStmt::FromImport { .. } => stmt,
            fe::ModuleStmt::SimpleImport { .. } => stmt,
            fe::ModuleStmt::ContractDef { .. } => contracts::contract_def(context, stmt),
        })
        .collect::<Vec<_>>();

    let attributes = context.get_module().expect("missing attributes");

    let struct_defs_from_tuples = attributes
        .tuples_used
        .iter()
        .map(|tuple| Node::new(tuple_to_struct_def(tuple), Span::zero()))
        .collect::<Vec<Node<fe::ModuleStmt>>>();

    fe::Module {
        body: [struct_defs_from_tuples, lowered_body].concat(),
    }
}

fn tuple_to_struct_def(tuple: &Tuple) -> fe::ModuleStmt {
    let fields = tuple
        .items
        .iter()
        .enumerate()
        .map(|(index, typ)| {
            Node::new(
                build_struct_field(format!("item{}", index), names::fixed_size_type_desc(typ)),
                Span::zero(),
            )
        })
        .collect();

    fe::ModuleStmt::StructDef {
        name: Node::new(names::tuple_struct_string(tuple), Span::zero()),
        fields,
    }
}

fn build_struct_field(name: String, type_desc: fe::TypeDesc) -> fe::Field {
    fe::Field {
        is_pub: false,
        is_const: false,
        name: Node::new(name, Span::zero()),
        typ: Node::new(type_desc, Span::zero()),
        value: None,
    }
}
