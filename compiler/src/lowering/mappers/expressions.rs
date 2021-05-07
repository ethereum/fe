use crate::lowering::names::{list_expr_generator_fn_name, tuple_struct_name};
use crate::lowering::utils::ZeroSpanNode;
use fe_analyzer::builtins::Object;
use fe_analyzer::context::Context;
use fe_analyzer::namespace::types::Type;
use fe_parser::ast as fe;
use fe_parser::ast::Kwarg;
use fe_parser::node::Node;

/// Lowers an expression and all sub expressions.
pub fn expr(context: &Context, exp: Node<fe::Expr>) -> Node<fe::Expr> {
    let span = exp.span;

    let lowered_kind = match exp.kind {
        fe::Expr::Name(_) => exp.kind,
        fe::Expr::Num(_) => exp.kind,
        fe::Expr::Bool(_) => exp.kind,
        fe::Expr::Subscript { value, slices } => fe::Expr::Subscript {
            value: boxed_expr(context, value),
            slices: slices_index_expr(context, slices),
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
        fe::Expr::Call { func, args } => fe::Expr::Call {
            func: boxed_expr(context, func),
            args: call_args(context, args),
        },
        fe::Expr::List { .. } => expr_list(context, exp),
        fe::Expr::Tuple { .. } => expr_tuple(context, exp),
        fe::Expr::Str(_) => exp.kind,
    };

    Node::new(lowered_kind, span)
}

fn slices_index_expr(
    context: &Context,
    slices: Node<Vec<Node<fe::Slice>>>,
) -> Node<Vec<Node<fe::Slice>>> {
    let first_slice = &slices.kind[0];

    if let fe::Slice::Index(exp) = &first_slice.kind {
        return Node::new(
            vec![Node::new(
                fe::Slice::Index(Box::new(expr(context, *exp.to_owned()))),
                first_slice.span,
            )],
            slices.span,
        );
    }

    unreachable!()
}

/// Lowers and optional expression.
pub fn optional_expr(context: &Context, exp: Option<Node<fe::Expr>>) -> Option<Node<fe::Expr>> {
    exp.map(|exp| expr(context, exp))
}

/// Lowers a boxed expression.
#[allow(clippy::boxed_local)]
pub fn boxed_expr(context: &Context, exp: Box<Node<fe::Expr>>) -> Box<Node<fe::Expr>> {
    Box::new(expr(context, *exp))
}

/// Lowers a list of expression.
pub fn multiple_exprs(context: &Context, exp: Vec<Node<fe::Expr>>) -> Vec<Node<fe::Expr>> {
    exp.into_iter().map(|exp| expr(context, exp)).collect()
}

/// Lowers call arguments
pub fn call_args(
    context: &Context,
    args: Node<Vec<Node<fe::CallArg>>>,
) -> Node<Vec<Node<fe::CallArg>>> {
    let lowered_args = args
        .kind
        .into_iter()
        .map(|arg| match arg.kind {
            fe::CallArg::Arg(inner_arg) => {
                Node::new(fe::CallArg::Arg(expr(context, inner_arg)), arg.span)
            }
            fe::CallArg::Kwarg(inner_arg) => {
                Node::new(fe::CallArg::Kwarg(kwarg(context, inner_arg)), arg.span)
            }
        })
        .collect();

    Node::new(lowered_args, args.span)
}

fn kwarg(context: &Context, kwarg: fe::Kwarg) -> fe::Kwarg {
    fe::Kwarg {
        name: kwarg.name,
        value: boxed_expr(context, kwarg.value),
    }
}

fn expr_tuple(context: &Context, exp: Node<fe::Expr>) -> fe::Expr {
    let attributes = context.get_expression(&exp).expect("missing attributes");

    if let Type::Tuple(tuple) = &attributes.typ {
        if tuple.is_empty() {
            return exp.kind;
        }

        let name = tuple_struct_name(tuple);

        if let fe::Expr::Tuple { elts } = exp.kind {
            // map the tuple args to kwargs
            let args = Node::new(
                elts.into_iter()
                    .enumerate()
                    .map(|(index, elt)| {
                        Node::new(
                            fe::CallArg::Kwarg(Kwarg {
                                name: Node::new(format!("item{}", index), elt.span),
                                value: Box::new(elt.clone()),
                            }),
                            elt.span,
                        )
                    })
                    .collect(),
                exp.span,
            );

            // create type constructor call for the lowered tuple
            return fe::Expr::Call {
                func: Box::new(Node::new(name, exp.span)),
                args,
            };
        }
    }

    unreachable!()
}

fn expr_list(context: &Context, exp: Node<fe::Expr>) -> fe::Expr {
    let attributes = context.get_expression(&exp).expect("missing attributes");

    if let Type::Array(array) = &attributes.typ {
        let fn_name = list_expr_generator_fn_name(array);

        if let fe::Expr::List { elts } = exp.kind {
            let args = elts
                .into_iter()
                .map(|list_val| fe::CallArg::Arg(list_val).into_node())
                .collect::<Vec<_>>()
                .into_node();

            // Turn List Expression into a function call
            return fe::Expr::Call {
                func: fe::Expr::Attribute {
                    value: fe::Expr::Name(Object::Self_.to_string()).into_boxed_node(),
                    attr: fn_name.into_node(),
                }
                .into_boxed_node(),
                args,
            };
        }
    }

    unreachable!()
}
