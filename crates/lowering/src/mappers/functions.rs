use crate::ast_utils::{
    boolean_expr_to_if, get_first_boolean_expressions, get_first_ternary_expressions,
};
use crate::ast_utils::{
    inject_before_expression, replace_node_with_name_expression, ternary_to_if,
};
use crate::context::{FnContext, ModuleContext};
use crate::mappers::expressions;
use crate::mappers::types;
use crate::utils::ZeroSpanNode;
use fe_analyzer::namespace::items::FunctionId;
use fe_analyzer::namespace::types::{Base, Type};
use fe_analyzer::namespace::types::{FixedSize, TypeDowncast};
use fe_parser::ast::{self as fe, Expr, FuncStmt, RegularFunctionArg, SmolStr};
use fe_parser::node::Node;

/// Lowers a function definition.
pub fn func_def(context: &mut ModuleContext, function: FunctionId) -> Node<fe::Function> {
    let node = &function.data(context.db).ast;
    let fe::Function {
        pub_,
        unsafe_,
        name,
        args,
        return_type: return_type_node,
        body,
    } = &node.kind;

    let signature = function.signature(context.db);

    let return_type = signature
        .return_type
        .as_ref()
        .expect("fn return type error");

    let mut fn_ctx = FnContext::new(context, function, function.body(context.db));

    let lowered_body = {
        let mut lowered_body = multiple_stmts(&mut fn_ctx, body.clone());
        // append `return ()` to the body if there is no return
        if return_type.is_unit() && !is_last_statement_return(&lowered_body) {
            lowered_body.push(
                fe::FuncStmt::Return {
                    value: Some(fe::Expr::Unit.into_node()),
                }
                .into_node(),
            );
        }
        lowered_body
    };

    let lowered_body = lower_iteratively(
        &mut fn_ctx,
        lowered_body,
        "ternary_result",
        &get_first_ternary_expressions,
        &ternary_to_if,
    );
    let lowered_body = lower_iteratively(
        &mut fn_ctx,
        lowered_body,
        "boolean_expr_result",
        &get_first_boolean_expressions,
        &boolean_expr_to_if,
    );

    let param_types = {
        let params = &signature.params;
        let mut types = vec![];
        if matches!(
            args.first(),
            Some(Node {
                kind: fe::FunctionArg::Zelf,
                ..
            })
        ) {
            // The type of self is excluded from the parameter type list, so we add
            // a unit type placeholder to accommodate the following zip.
            types.push(Type::Base(Base::Unit));
        }
        for param in params {
            types.push(param.typ.clone().expect("fn param type error").into())
        }
        types
    };

    let args = args
        .iter()
        .zip(param_types)
        .map(|(pnode, ptype)| {
            if let fe::FunctionArg::Regular(regular) = &pnode.kind {
                fe::FunctionArg::Regular(RegularFunctionArg {
                    name: regular.name.clone(),
                    typ: types::type_desc(fn_ctx.module, regular.typ.clone(), &ptype),
                })
                .into_node()
            } else {
                // the self arg remains the same
                pnode.to_owned()
            }
        })
        .collect();

    // The return type is lowered if it exists. If there is no return type, we set it to the unit type.
    let lowered_return_type = return_type_node
        .clone()
        .map(|type_desc| types::type_desc(fn_ctx.module, type_desc, &return_type.clone().into()))
        .unwrap_or_else(|| fe::TypeDesc::Unit.into_node());

    let lowered_function = fe::Function {
        pub_: *pub_,
        unsafe_: *unsafe_,
        name: name.clone(),
        args,
        return_type: Some(lowered_return_type),
        body: lowered_body,
    };

    Node::new(lowered_function, node.span)
}

fn lower_iteratively(
    context: &mut FnContext,
    statements: Vec<Node<FuncStmt>>,
    result_name: &str,
    getter_fn: &dyn Fn(&[Node<FuncStmt>]) -> Vec<Node<Expr>>,
    mapper_fn: &dyn Fn(FixedSize, &Node<Expr>, &str) -> Vec<Node<FuncStmt>>,
) -> Vec<Node<FuncStmt>> {
    let mut current_statements = statements;

    loop {
        let expressions = getter_fn(&current_statements);

        if let Some(current_expression) = expressions.last() {
            let expr_attr = context
                .expression_attributes(current_expression.original_id)
                .expect("missing attributes");

            let expression_type =
                FixedSize::try_from(expr_attr.typ.clone()).expect("Not a fixed size");

            let unique_name = context.make_unique_name(result_name);
            let generated_statements =
                mapper_fn(expression_type.clone(), current_expression, &unique_name);
            current_statements = inject_before_expression(
                &current_statements,
                current_expression.original_id,
                &generated_statements,
            );
            current_statements = replace_node_with_name_expression(
                &current_statements,
                current_expression.original_id,
                &unique_name,
            );
        } else {
            break;
        }
    }

    current_statements
}

fn func_stmt(context: &mut FnContext, stmt: Node<fe::FuncStmt>) -> Vec<Node<fe::FuncStmt>> {
    let lowered_kinds = match stmt.kind {
        fe::FuncStmt::Return { value } => stmt_return(context, value),
        fe::FuncStmt::VarDecl { target, typ, value } => {
            let var_type = context
                .var_decl_type(typ.id)
                .expect("missing var decl type")
                .clone()
                .into();

            match target.kind {
                fe::VarDeclTarget::Name(_) => vec![fe::FuncStmt::VarDecl {
                    target,
                    typ: types::type_desc(context.module, typ, &var_type),
                    value: expressions::optional_expr(context, value),
                }],
                fe::VarDeclTarget::Tuple(_) => {
                    lower_tuple_destructuring(context, target, typ, &var_type, value, stmt.span)
                }
            }
        }
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
        fe::FuncStmt::Unsafe(body) => vec![fe::FuncStmt::Unsafe(multiple_stmts(context, body))],
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
        fe::FuncStmt::Revert { error } => vec![fe::FuncStmt::Revert {
            error: error.map(|expr| expressions::expr(context, expr)),
        }],
    };
    let span = stmt.span;

    lowered_kinds
        .into_iter()
        .map(|kind| Node::new(kind, span))
        .collect()
}

fn multiple_stmts(
    context: &mut FnContext,
    stmts: Vec<Node<fe::FuncStmt>>,
) -> Vec<Node<fe::FuncStmt>> {
    stmts
        .into_iter()
        .map(|stmt| func_stmt(context, stmt))
        .collect::<Vec<Vec<Node<fe::FuncStmt>>>>()
        .concat()
}

fn stmt_aug_assign(
    context: &mut FnContext,
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

fn stmt_return(context: &mut FnContext, value: Option<Node<fe::Expr>>) -> Vec<fe::FuncStmt> {
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
    context: &mut FnContext,
    target: Node<fe::VarDeclTarget>,
    type_desc: Node<fe::TypeDesc>,
    typ: &Type,
    value: Option<Node<fe::Expr>>,
    span: fe_common::Span,
) -> Vec<fe::FuncStmt> {
    let mut stmts = vec![];
    let tmp_tuple: SmolStr = context.make_unique_name("tmp_tuple").into();
    stmts.push(fe::FuncStmt::VarDecl {
        target: Node::new(fe::VarDeclTarget::Name(tmp_tuple.clone()), span),
        typ: types::type_desc(context.module, type_desc.clone(), typ),
        value: expressions::optional_expr(context, value),
    });

    declare_tuple_items(
        context,
        target,
        type_desc,
        typ,
        &tmp_tuple,
        &mut vec![],
        &mut stmts,
    );
    stmts
}

fn declare_tuple_items(
    context: &mut FnContext,
    target: Node<fe::VarDeclTarget>,
    type_desc: Node<fe::TypeDesc>,
    typ: &Type,
    tmp_tuple: &str,
    indices: &mut Vec<usize>,
    stmts: &mut Vec<fe::FuncStmt>,
) {
    match target.kind {
        fe::VarDeclTarget::Name(_) => {
            let mut value = fe::Expr::Name(tmp_tuple.into()).into_node();
            for index in indices.iter() {
                value = fe::Expr::Attribute {
                    value: value.into(),
                    attr: SmolStr::new(format!("item{}", index)).into_node(),
                }
                .into_node();
            }

            stmts.push(fe::FuncStmt::VarDecl {
                target,
                typ: types::type_desc(context.module, type_desc, typ),
                value: expressions::optional_expr(context, Some(value)),
            });
        }

        fe::VarDeclTarget::Tuple(targets) => {
            let item_type_descs = match type_desc.kind {
                fe::TypeDesc::Tuple { items } => items,
                _ => unreachable!(),
            };
            let item_types = &typ
                .as_tuple()
                .expect("tuple declaration type mismatch")
                .items;
            for (index, ((target, type_desc), typ)) in targets
                .into_iter()
                .zip(item_type_descs.into_iter())
                .zip(item_types.into_iter())
                .enumerate()
            {
                indices.push(index);
                declare_tuple_items(
                    context,
                    target,
                    type_desc,
                    &typ.clone().into(),
                    tmp_tuple,
                    indices,
                    stmts,
                );
                indices.pop().unwrap();
            }
        }
    }
}
