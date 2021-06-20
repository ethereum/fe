use crate::lowering::context::Context;
use crate::lowering::mappers::expressions;
use crate::lowering::mappers::types;
use crate::lowering::utils::ZeroSpanNode;
use fe_parser::ast as fe;
use fe_parser::node::Node;

/// Lowers a function definition.
pub fn func_def(context: &mut Context, def: Node<fe::Function>) -> Node<fe::Function> {
    let fe::Function {
        is_pub,
        name,
        args,
        return_type,
        body,
    } = def.kind;
    // The return type is lowered if it exists. If there is no return type, we set it to the unit type.
    let lowered_return_type = return_type
        .map(|return_type| types::type_desc(context, return_type))
        .unwrap_or_else(|| fe::TypeDesc::Unit.into_node());

    let lowered_body = {
        let mut lowered_body = multiple_stmts(context, body);
        // append `return ()` to the body if there is no return
        if lowered_return_type.kind == fe::TypeDesc::Unit
            && !is_last_statement_return(&lowered_body)
        {
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
                fe::FunctionArg {
                    name: arg.kind.name,
                    typ: types::type_desc(context, arg.kind.typ),
                },
                arg.span,
            )
        })
        .collect();

    let lowered_function = fe::Function {
        is_pub,
        name,
        args: lowered_args,
        return_type: Some(lowered_return_type),
        body: lowered_body,
    };

    Node::new(lowered_function, def.span)
}

fn func_stmt(context: &mut Context, stmt: Node<fe::FuncStmt>) -> Vec<Node<fe::FuncStmt>> {
    let lowered_kinds = match stmt.kind {
        fe::FuncStmt::Return { value } => stmt_return(context, value),
        fe::FuncStmt::VarDecl { target, typ, value } => match target.kind {
            fe::VarDeclTarget::Name(_) => vec![fe::FuncStmt::VarDecl {
                target,
                typ: types::type_desc(context, typ),
                value: expressions::optional_expr(context, value),
            }],
            fe::VarDeclTarget::Tuple(_) => {
                lower_tuple_destructuring(context, target, typ, value, stmt.span)
            }
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

fn multiple_stmts(
    context: &mut Context,
    stmts: Vec<Node<fe::FuncStmt>>,
) -> Vec<Node<fe::FuncStmt>> {
    stmts
        .into_iter()
        .map(|stmt| func_stmt(context, stmt))
        .collect::<Vec<Vec<Node<fe::FuncStmt>>>>()
        .concat()
}

fn stmt_aug_assign(
    context: &mut Context,
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

fn stmt_return(context: &mut Context, value: Option<Node<fe::Expr>>) -> Vec<fe::FuncStmt> {
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

/// Lowers tuple desctructuring.
///
/// e.g.
/// ```fe
/// (x, y): (uint256, bool) = (1, true)
/// ```
/// will be lowered to
/// ```fe
/// $tmp_tuple_0: (uint256, bool) = (1, true)
/// x: uint256 = $tmp_tuple_0.item0
/// y: bool = $tmp_tuple_0.item1
/// ```
fn lower_tuple_destructuring(
    context: &mut Context,
    target: Node<fe::VarDeclTarget>,
    typ: Node<fe::TypeDesc>,
    value: Option<Node<fe::Expr>>,
    span: fe_common::Span,
) -> Vec<fe::FuncStmt> {
    let mut stmts = vec![];
    let tmp_tuple = context.module.make_unique_name("tmp_tuple");
    stmts.push(fe::FuncStmt::VarDecl {
        target: Node::new(fe::VarDeclTarget::Name(tmp_tuple.clone()), span),
        typ: types::type_desc(context, typ.clone()),
        value: expressions::optional_expr(context, value),
    });

    declare_tuple_items(
        context,
        target,
        typ,
        &tmp_tuple,
        &mut vec![],
        span,
        &mut stmts,
    );
    stmts
}

fn declare_tuple_items(
    context: &mut Context,
    target: Node<fe::VarDeclTarget>,
    typ: Node<fe::TypeDesc>,
    tmp_tuple: &str,
    indices: &mut Vec<usize>,
    span: fe_common::Span,
    stmts: &mut Vec<fe::FuncStmt>,
) {
    match target.kind {
        fe::VarDeclTarget::Name(_) => {
            let mut value = Node::new(fe::Expr::Name(tmp_tuple.to_string()), span);
            for index in indices.iter() {
                value = Node::new(
                    fe::Expr::Attribute {
                        value: value.into(),
                        attr: Node::new(format!("item{}", index), span),
                    },
                    span,
                );
            }

            stmts.push(fe::FuncStmt::VarDecl {
                target,
                typ,
                value: expressions::optional_expr(context, Some(value)),
            });
        }

        fe::VarDeclTarget::Tuple(items) => {
            let items_typ = match typ.kind {
                fe::TypeDesc::Tuple { items } => items,
                _ => unreachable!(),
            };
            for (index, (target, typ)) in items.into_iter().zip(items_typ.into_iter()).enumerate() {
                indices.push(index);
                declare_tuple_items(context, target, typ, tmp_tuple, indices, span, stmts);
                indices.pop().unwrap();
            }
        }
    }
}
