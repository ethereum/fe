use fe_analyzer::Context;

use crate::lowering::mappers::functions;
use fe_parser::ast as fe;
use fe_parser::ast::ContractStmt;
use fe_parser::node::Node;

/// Lowers a contract definition.
pub fn contract_def(context: &Context, stmt: Node<fe::ModuleStmt>) -> Node<fe::ModuleStmt> {
    if let fe::ModuleStmt::ContractDef { name, fields, body } = stmt.kind {
        let lowered_body = body
            .into_iter()
            .map(|stmt| match stmt.kind {
                ContractStmt::EventDef { .. } => stmt,
                ContractStmt::FuncDef { .. } => functions::func_def(context, stmt),
            })
            .collect();

        return Node::new(
            fe::ModuleStmt::ContractDef {
                name,
                fields,
                body: lowered_body,
            },
            stmt.span,
        );
    }

    unreachable!()
}
