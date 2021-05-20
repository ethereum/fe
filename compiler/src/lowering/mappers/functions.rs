use crate::lowering::mappers::expressions;
use crate::lowering::mappers::types;
use crate::lowering::names;
use crate::lowering::utils::ZeroSpanNode;
use fe_analyzer::context::Context;
use fe_analyzer::namespace::types::FixedSize;
use fe_parser::ast as fe;
use fe_parser::node::Node;

/// Lowers a function definition.
pub fn func_def(context: &Context, def: Node<fe::ContractStmt>) -> Node<fe::ContractStmt> {
    if let fe::ContractStmt::FuncDef {
        is_pub,
        name,
        args,
        return_type,
        body,
    } = def.kind
    {
        let attributes = context.get_function(def.id).expect("missing attributes");

        // The return type is lowered if it exists. If there is no return type, we set it to the unit type.
        let lowered_return_type = return_type
            .map(|return_type| types::type_desc(context, return_type))
            .unwrap_or_else(|| names::fixed_size_type_desc(&FixedSize::Unit).into_node());

        let lowered_body = {
            let mut lowered_body = multiple_stmts(context, body);
            // append `return ()` to the body if there is no return
            if attributes.return_type.is_unit() && !is_last_statement_return(&lowered_body) {
                lowered_body.push(
                    fe::FuncStmt::Return {
                        value: Some(fe::Expr::Unit.into_node()),
                    }
                    .into_node(),
                );
            }
            lowered_body
        };

        let lowered_args = args
            .into_iter()
            .map(|arg| {
                Node::new(
                    fe::FuncDefArg {
                        name: arg.kind.name,
                        typ: types::type_desc(context, arg.kind.typ),
                    },
                    arg.span,
                )
            })
            .collect();

        let lowered_function = fe::ContractStmt::FuncDef {
            is_pub,
            name,
            args: lowered_args,
            return_type: Some(lowered_return_type),
            body: lowered_body,
        };

        Node::new(lowered_function, def.span)
    } else {
        unreachable!()
    }
}

fn func_stmt(context: &Context, stmt: Node<fe::FuncStmt>) -> Vec<Node<fe::FuncStmt>> {
    let lowered_kinds = match stmt.kind {
        fe::FuncStmt::Return { value } => stmt_return(context, value),
        fe::FuncStmt::VarDecl { target, typ, value } => match target.kind {
            fe::VarDeclTarget::Name(_) => vec![fe::FuncStmt::VarDecl {
                target,
                typ: types::type_desc(context, typ),
                value: expressions::optional_expr(context, value),
            }],
            fe::VarDeclTarget::Tuple(_) => todo!("tuple var decl lowering"),
        },
        fe::FuncStmt::Assign { target, value } => vec![fe::FuncStmt::Assign {
            target: expressions::expr(context, target),
            value: expressions::expr(context, value),
        }],
        fe::FuncStmt::Emit { name, args } => vec![fe::FuncStmt::Emit {
            name,
            args: expressions::call_args(context, args),
        }],
        fe::FuncStmt::AugAssign { target, op, value } => {
            stmt_aug_assign(context, target, op, value)
        }
        fe::FuncStmt::For { target, iter, body } => vec![fe::FuncStmt::For {
            target,
            iter: expressions::expr(context, iter),
            body: multiple_stmts(context, body),
        }],
        fe::FuncStmt::While { test, body } => vec![fe::FuncStmt::While {
            test: expressions::expr(context, test),
            body: multiple_stmts(context, body),
        }],
        fe::FuncStmt::If {
            test,
            body,
            or_else,
        } => vec![fe::FuncStmt::If {
            test: expressions::expr(context, test),
            body: multiple_stmts(context, body),
            or_else: multiple_stmts(context, or_else),
        }],
        fe::FuncStmt::Assert { test, msg } => vec![fe::FuncStmt::Assert {
            test: expressions::expr(context, test),
            msg: expressions::optional_expr(context, msg),
        }],
        fe::FuncStmt::Expr { value } => vec![fe::FuncStmt::Expr {
            value: expressions::expr(context, value),
        }],
        fe::FuncStmt::Pass => vec![stmt.kind],
        fe::FuncStmt::Break => vec![stmt.kind],
        fe::FuncStmt::Continue => vec![stmt.kind],
        fe::FuncStmt::Revert => vec![stmt.kind],
    };
    let span = stmt.span;

    lowered_kinds
        .into_iter()
        .map(|kind| Node::new(kind, span))
        .collect()
}

fn multiple_stmts(context: &Context, stmts: Vec<Node<fe::FuncStmt>>) -> Vec<Node<fe::FuncStmt>> {
    stmts
        .into_iter()
        .map(|stmt| func_stmt(context, stmt))
        .collect::<Vec<Vec<Node<fe::FuncStmt>>>>()
        .concat()
}

fn stmt_aug_assign(
    context: &Context,
    target: Node<fe::Expr>,
    op: Node<fe::BinOperator>,
    value: Node<fe::Expr>,
) -> Vec<fe::FuncStmt> {
    let original_value_span = value.span;
    let lowered_target = expressions::expr(context, target.clone());
    let lowered_lh_value = expressions::expr(context, target);
    let lowered_rh_value = expressions::expr(context, value);

    let new_value_kind = fe::Expr::BinOperation {
        left: Box::new(lowered_lh_value),
        op,
        right: Box::new(lowered_rh_value),
    };

    let new_value = Node::new(new_value_kind, original_value_span);

    // the new statement is: `target = target <op> value`.
    vec![fe::FuncStmt::Assign {
        target: lowered_target,
        value: new_value,
    }]
}

fn stmt_return(context: &Context, value: Option<Node<fe::Expr>>) -> Vec<fe::FuncStmt> {
    if let Some(value) = value {
        // lower a return statement that contains a value (e.g. `return true` or `return ()`)
        vec![fe::FuncStmt::Return {
            value: Some(expressions::expr(context, value)),
        }]
    } else {
        // lower a return statement with no value to `return empty_tuple()`
        vec![fe::FuncStmt::Return {
            value: Some(fe::Expr::Unit.into_node()),
        }]
    }
}

fn is_last_statement_return(stmts: &[Node<fe::FuncStmt>]) -> bool {
    if let Some(stmt) = stmts.last() {
        matches!(stmt.kind, fe::FuncStmt::Return { .. })
    } else {
        false
    }
}
