use fe_analyzer::Context;

use crate::lowering::mappers::functions;
use crate::lowering::mappers::types;
use fe_parser::ast as fe;
use fe_parser::ast::ContractStmt;
use fe_parser::node::Node;

/// Lowers a contract definition.
pub fn contract_def(context: &Context, stmt: Node<fe::ModuleStmt>) -> Node<fe::ModuleStmt> {
    if let fe::ModuleStmt::ContractDef { name, fields, body } = stmt.kind {
        let lowered_body = body
            .into_iter()
            .map(|stmt| match stmt.kind {
                ContractStmt::EventDef { .. } => event_def(context, stmt),
                ContractStmt::FuncDef { .. } => functions::func_def(context, stmt),
            })
            .collect();

        let lowered_fields = fields
            .into_iter()
            .map(|field| contract_field(context, field))
            .collect();

        return Node::new(
            fe::ModuleStmt::ContractDef {
                name,
                fields: lowered_fields,
                body: lowered_body,
            },
            stmt.span,
        );
    }

    unreachable!()
}

fn contract_field(context: &Context, field: Node<fe::Field>) -> Node<fe::Field> {
    Node::new(
        fe::Field {
            pub_qual: field.kind.pub_qual,
            const_qual: field.kind.const_qual,
            name: field.kind.name,
            typ: types::type_desc(context, field.kind.typ),
            value: field.kind.value,
        },
        field.span,
    )
}

fn event_def(context: &Context, stmt: Node<fe::ContractStmt>) -> Node<fe::ContractStmt> {
    if let fe::ContractStmt::EventDef { name, fields } = stmt.kind {
        let lowered_fields = fields
            .into_iter()
            .map(|field| {
                Node::new(
                    fe::EventField {
                        idx_qual: field.kind.idx_qual,
                        name: field.kind.name,
                        typ: types::type_desc(context, field.kind.typ),
                    },
                    field.span,
                )
            })
            .collect();

        return Node::new(
            fe::ContractStmt::EventDef {
                name,
                fields: lowered_fields,
            },
            stmt.span,
        );
    }

    unreachable!()
}
