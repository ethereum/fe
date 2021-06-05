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
        .map(|stmt| match stmt {
            fe::ModuleStmt::Pragma(_) => stmt,
            fe::ModuleStmt::TypeAlias(_) => stmt,
            fe::ModuleStmt::StructDef(_) => stmt,
            fe::ModuleStmt::Import(_) => stmt,
            fe::ModuleStmt::ContractDef(inner) => {
                fe::ModuleStmt::ContractDef(contracts::contract_def(context, inner))
            }
        })
        .collect::<Vec<_>>();

    let attributes = context.get_module().expect("missing attributes");

    let struct_defs_from_tuples = attributes
        .tuples_used
        .iter()
        .map(|tuple| fe::ModuleStmt::StructDef(Node::new(tuple_to_struct_def(tuple), Span::zero())))
        .collect::<Vec<fe::ModuleStmt>>();

    fe::Module {
        body: [struct_defs_from_tuples, lowered_body].concat(),
    }
}

fn tuple_to_struct_def(tuple: &Tuple) -> fe::StructDef {
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

    fe::StructDef {
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
