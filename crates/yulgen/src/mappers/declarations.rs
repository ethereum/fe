use crate::context::FnContext;
use crate::mappers::expressions;
use crate::names;
use crate::types::EvmSized;
use fe_analyzer::namespace::types::FixedSize;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use yultsur::*;

/// Builds a Yul statement from a Fe variable declaration
pub fn var_decl(context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::VarDecl { target, typ, value } = &stmt.kind {
        let decl_type = context.declaration_type(typ);

        let target = names::var_name(var_decl_name(&target.kind));

        return if let Some(value) = value {
            let value = expressions::expr(context, value);
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

/// Builds a Yul statement from a Fe const declaration.
/// TODO: We don't perform any optimization on constant and treat it in the same way as a local variable.
pub fn const_decl(context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::ConstantDecl { name, value, .. } = &stmt.kind {
        let target = names::var_name(name.kind.as_str());
        let value = expressions::expr(context, value);

        return statement! { let [target] := [value] };
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
