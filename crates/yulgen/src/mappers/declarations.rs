use crate::mappers::expressions;
use crate::types::EvmSized;
use crate::{names, Context};
use fe_analyzer::namespace::types::FixedSize;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use yultsur::*;

/// Builds a Yul statement from a Fe variable declaration
pub fn var_decl(context: &mut Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    let decl_type = context
        .analysis
        .get_declaration(stmt)
        .expect("missing attributes");

    if let fe::FuncStmt::VarDecl { target, value, .. } = &stmt.kind {
        let target = names::var_name(var_decl_name(&target.kind));

        return if let Some(value) = value {
            let value = expressions::expr(context, &value);
            statement! { let [target] := [value] }
        } else {
            match decl_type {
                FixedSize::Base(_) => statement! { let [target] := 0 },
                typ => {
                    let size = literal_expression! { (typ.size()) };
                    statement! { let [target] := alloc([size]) }
                }
            }
        };
    }

    unreachable!()
}

fn var_decl_name(target: &fe::VarDeclTarget) -> &str {
    if let fe::VarDeclTarget::Name(name) = target {
        name
    } else {
        panic!("complex VarDeclTargets should be lowered to VarDeclTarget::Name")
    }
}
