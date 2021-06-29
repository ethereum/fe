use crate::lowering::context::Context;
use crate::lowering::names::{list_expr_generator_fn_name, tuple_struct_name};
use crate::lowering::utils::ZeroSpanNode;
use fe_analyzer::builtins::Object;
use fe_analyzer::namespace::types::{Type, TypeDowncast};
use fe_parser::ast as fe;
use fe_parser::node::Node;

/// Lowers an expression and all sub expressions.
pub fn expr(context: &mut Context, exp: Node<fe::Expr>) -> Node<fe::Expr> {
    let span = exp.span;

    let lowered_kind = match exp.kind {
        fe::Expr::Name(_) => exp.kind,
        fe::Expr::Num(_) => exp.kind,
        fe::Expr::Bool(_) => exp.kind,
        fe::Expr::Subscript { value, index } => fe::Expr::Subscript {
            value: boxed_expr(context, value),
            index: boxed_expr(context, index),
        },
        fe::Expr::Attribute { value, attr } => fe::Expr::Attribute {
            value: boxed_expr(context, value),
            attr,
        },
        fe::Expr::Ternary {
            if_expr,
            test,
            else_expr,
        } => fe::Expr::Ternary {
            if_expr: boxed_expr(context, if_expr),
            test: boxed_expr(context, test),
            else_expr: boxed_expr(context, else_expr),
        },
        fe::Expr::BoolOperation { left, op, right } => fe::Expr::BoolOperation {
            left: boxed_expr(context, left),
            op,
            right: boxed_expr(context, right),
        },
        fe::Expr::BinOperation { left, op, right } => fe::Expr::BinOperation {
            left: boxed_expr(context, left),
            op,
            right: boxed_expr(context, right),
        },
        fe::Expr::UnaryOperation { op, operand } => fe::Expr::UnaryOperation {
            op,
            operand: boxed_expr(context, operand),
        },
        fe::Expr::CompOperation { left, op, right } => fe::Expr::CompOperation {
            left: boxed_expr(context, left),
            op,
            right: boxed_expr(context, right),
        },
        fe::Expr::Call {
            func,
            generic_args,
            args,
        } => fe::Expr::Call {
            func: boxed_expr(context, func),
            generic_args,
            args: call_args(context, args),
        },
        fe::Expr::List { .. } => expr_list(context, exp),
        fe::Expr::Tuple { .. } => expr_tuple(context, exp),
        fe::Expr::Str(_) => exp.kind,
        fe::Expr::Unit => exp.kind,
    };

    Node::new(lowered_kind, span)
}

/// Lowers and optional expression.
pub fn optional_expr(context: &mut Context, exp: Option<Node<fe::Expr>>) -> Option<Node<fe::Expr>> {
    exp.map(|exp| expr(context, exp))
}

/// Lowers a boxed expression.
#[allow(clippy::boxed_local)]
pub fn boxed_expr(context: &mut Context, exp: Box<Node<fe::Expr>>) -> Box<Node<fe::Expr>> {
    Box::new(expr(context, *exp))
}

/// Lowers call arguments
pub fn call_args(
    context: &mut Context,
    args: Node<Vec<Node<fe::CallArg>>>,
) -> Node<Vec<Node<fe::CallArg>>> {
    let lowered_args = args
        .kind
        .into_iter()
        .map(|arg| {
            Node::new(
                fe::CallArg {
                    label: arg.kind.label,
                    value: expr(context, arg.kind.value),
                },
                arg.span,
            )
        })
        .collect();

    Node::new(lowered_args, args.span)
}

fn expr_tuple(context: &mut Context, exp: Node<fe::Expr>) -> fe::Expr {
    let typ = context
        .module
        .analysis
        .get_expression(&exp)
        .expect("missing attributes")
        .typ
        .as_tuple()
        .expect("expected tuple type");

    context.module.tuples.insert(typ.clone());
    let elts = match exp.kind {
        fe::Expr::Tuple { elts } => elts,
        _ => unreachable!(),
    };

    // map the tuple args to named args
    let args = Node::new(
        elts.into_iter()
            .enumerate()
            .map(|(index, elt)| {
                let span = elt.span;
                Node::new(
                    fe::CallArg {
                        label: Some(Node::new(format!("item{}", index), span)),
                        value: expr(context, elt),
                    },
                    span,
                )
            })
            .collect(),
        exp.span,
    );

    // create type constructor call for the lowered tuple
    fe::Expr::Call {
        func: Box::new(Node::new(fe::Expr::Name(tuple_struct_name(typ)), exp.span)),
        generic_args: None,
        args,
    }
}

fn expr_list(context: &mut Context, exp: Node<fe::Expr>) -> fe::Expr {
    let attributes = context
        .module
        .analysis
        .get_expression(&exp)
        .expect("missing attributes");

    if let Type::Array(array) = &attributes.typ {
        context.list_expressions.insert(array.clone());
        let fn_name = list_expr_generator_fn_name(array);

        if let fe::Expr::List { elts } = exp.kind {
            let args = elts
                .into_iter()
                .map(|list_val| {
                    fe::CallArg {
                        label: None,
                        value: list_val,
                    }
                    .into_node()
                })
                .collect::<Vec<_>>()
                .into_node();

            // Turn List Expression into a function call
            return fe::Expr::Call {
                func: fe::Expr::Attribute {
                    value: fe::Expr::Name(Object::Self_.to_string()).into_boxed_node(),
                    attr: fn_name.into_node(),
                }
                .into_boxed_node(),
                generic_args: None,
                args,
            };
        }
    }

    unreachable!()
}
