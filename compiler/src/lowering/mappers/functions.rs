use crate::lowering::mappers::expressions;
use crate::lowering::mappers::types;
use fe_analyzer::context::Context;
use fe_parser::ast as fe;
use fe_parser::node::Node;

/// Lowers a function definition.
pub fn func_def(context: &Context, def: Node<fe::ContractStmt>) -> Node<fe::ContractStmt> {
    if let fe::ContractStmt::FuncDef {
        pub_qual,
        name,
        args,
        return_type,
        body,
    } = def.kind
    {
        let lowered_return_type =
            return_type.map(|return_type| types::type_desc(context, return_type));
        let lowered_body = multiple_stmts(context, body);
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

        let lowered_kind = fe::ContractStmt::FuncDef {
            pub_qual,
            name,
            args: lowered_args,
            return_type: lowered_return_type,
            body: lowered_body,
        };

        return Node::new(lowered_kind, def.span);
    }

    unreachable!()
}

fn func_stmt(context: &Context, stmt: Node<fe::FuncStmt>) -> Vec<Node<fe::FuncStmt>> {
    let lowered_kinds = match stmt.kind {
        fe::FuncStmt::Return { value } => vec![fe::FuncStmt::Return {
            value: expressions::optional_expr(context, value),
        }],
        fe::FuncStmt::VarDecl { target, typ, value } => match target.kind {
            fe::VarDeclTarget::Name(_) => vec![fe::FuncStmt::VarDecl {
                target,
                typ: types::type_desc(context, typ),
                value: expressions::optional_expr(context, value),
            }],
            fe::VarDeclTarget::Tuple(_) => todo!("tuple var decl lowering"),
        },
        fe::FuncStmt::Assign { targets, value } => vec![fe::FuncStmt::Assign {
            targets: expressions::multiple_exprs(context, targets),
            value: expressions::expr(context, value),
        }],
        fe::FuncStmt::Emit { name, args } => vec![fe::FuncStmt::Emit {
            name,
            args: expressions::call_args(context, args),
        }],
        fe::FuncStmt::AugAssign { target, op, value } => aug_assign(context, target, op, value),
        fe::FuncStmt::For {
            target,
            iter,
            body,
            or_else,
        } => vec![fe::FuncStmt::For {
            target: expressions::expr(context, target),
            iter: expressions::expr(context, iter),
            body: multiple_stmts(context, body),
            or_else: multiple_stmts(context, or_else),
        }],
        fe::FuncStmt::While {
            test,
            body,
            or_else,
        } => vec![fe::FuncStmt::While {
            test: expressions::expr(context, test),
            body: multiple_stmts(context, body),
            or_else: multiple_stmts(context, or_else),
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

fn aug_assign(
    context: &Context,
    target: Node<fe::Expr>,
    op: Node<fe::BinOperator>,
    value: Node<fe::Expr>,
) -> Vec<fe::FuncStmt> {
    let lowered_target = expressions::expr(context, target);
    let original_value_span = value.span;
    let lowered_value = expressions::expr(context, value);

    let new_value_kind = fe::Expr::BinOperation {
        left: Box::new(lowered_target.clone().new_id()),
        op,
        right: Box::new(lowered_value),
    };

    let new_value = Node::new(new_value_kind, original_value_span);

    // the new statement is: `target = target <op> value`.
    vec![fe::FuncStmt::Assign {
        targets: vec![lowered_target],
        value: new_value,
    }]
}
