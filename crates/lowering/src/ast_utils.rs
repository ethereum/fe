use fe_analyzer::namespace::types::FixedSize;
use fe_parser::ast::{BoolOperator, CallArg, Expr, FuncStmt, UnaryOperator, VarDeclTarget};
use fe_parser::node::{Node, NodeId};

use crate::names;
use crate::utils::ZeroSpanNode;
use std::ops::Deref;

#[derive(Debug)]
pub enum StmtOrExpr {
    Stmt(Box<Node<FuncStmt>>),
    Expr(Node<Expr>),
}

impl From<Node<FuncStmt>> for StmtOrExpr {
    fn from(stmt: Node<FuncStmt>) -> Self {
        StmtOrExpr::Stmt(Box::new(stmt))
    }
}

impl From<Node<Expr>> for StmtOrExpr {
    fn from(expr: Node<Expr>) -> Self {
        StmtOrExpr::Expr(expr)
    }
}

impl StmtOrExpr {
    pub fn as_stmt(&self) -> Node<FuncStmt> {
        match self {
            StmtOrExpr::Stmt(stmt) => stmt.deref().clone(),
            _ => panic!("not a statement"),
        }
    }

    pub fn as_expr(&self) -> Node<Expr> {
        match self {
            StmtOrExpr::Expr(expr) => expr.clone(),
            _ => panic!("not an expression"),
        }
    }
}

/// Recursively map the given `node` by applying the given `map_fn`
pub fn map_ast_node<M>(node: StmtOrExpr, map_fn: &mut M) -> StmtOrExpr
where
    M: FnMut(StmtOrExpr) -> StmtOrExpr,
{
    match node {
        StmtOrExpr::Stmt(stmt) => {
            let node = match stmt.kind {
                FuncStmt::Assert { test, msg } => FuncStmt::Assert {
                    test: map_ast_node(test.into(), map_fn).as_expr(),
                    msg: msg.map(|val| map_ast_node(val.into(), map_fn).as_expr()),
                },
                FuncStmt::Assign { target, value } => FuncStmt::Assign {
                    target: map_ast_node(target.into(), map_fn).as_expr(),
                    value: map_ast_node(value.into(), map_fn).as_expr(),
                },
                FuncStmt::AugAssign { target, op, value } => FuncStmt::AugAssign {
                    target: map_ast_node(target.into(), map_fn).as_expr(),
                    value: map_ast_node(value.into(), map_fn).as_expr(),
                    op,
                },
                FuncStmt::Emit { name, args } => FuncStmt::Emit {
                    name,
                    args: map_call_args(args, map_fn),
                },
                FuncStmt::Expr { value } => FuncStmt::Expr {
                    value: map_ast_node(value.into(), map_fn).as_expr(),
                },
                FuncStmt::For { target, iter, body } => FuncStmt::For {
                    target,
                    iter: map_ast_node(iter.into(), map_fn).as_expr(),
                    body: map_body(body, map_fn),
                },
                FuncStmt::If {
                    body,
                    test,
                    or_else,
                } => FuncStmt::If {
                    body: map_body(body, map_fn),
                    or_else: map_body(or_else, map_fn),
                    test: map_ast_node(test.into(), map_fn).as_expr(),
                },

                FuncStmt::Return { value } => FuncStmt::Return {
                    value: value.map(|val| map_ast_node(val.into(), map_fn).as_expr()),
                },
                FuncStmt::Revert { error } => FuncStmt::Revert {
                    error: error.map(|val| map_ast_node(val.into(), map_fn).as_expr()),
                },
                FuncStmt::Unsafe(body) => FuncStmt::Unsafe(map_body(body, map_fn)),
                FuncStmt::VarDecl { target, typ, value } => FuncStmt::VarDecl {
                    target,
                    typ,
                    value: value.map(|val| map_ast_node(val.into(), map_fn).as_expr()),
                },
                FuncStmt::While { test, body } => FuncStmt::While {
                    test: map_ast_node(test.into(), map_fn).as_expr(),
                    body: map_body(body, map_fn),
                },
                // See comment below for why no catch all should be used here
                FuncStmt::Pass | FuncStmt::Break | FuncStmt::Continue => stmt.kind,
            }
            .into_traceable_node(stmt.original_id);

            map_fn(node.into())
        }
        StmtOrExpr::Expr(expr) => {
            let expr = match expr.kind {
                Expr::Attribute { value, attr } => Expr::Attribute {
                    value: Box::new(map_ast_node((*value).into(), map_fn).as_expr()),
                    attr,
                },
                Expr::BinOperation { left, op, right } => Expr::BinOperation {
                    left: Box::new(map_ast_node((*left).into(), map_fn).as_expr()),
                    right: Box::new(map_ast_node((*right).into(), map_fn).as_expr()),
                    op,
                },
                Expr::BoolOperation { left, op, right } => Expr::BoolOperation {
                    left: Box::new(map_ast_node((*left).into(), map_fn).as_expr()),
                    right: Box::new(map_ast_node((*right).into(), map_fn).as_expr()),
                    op,
                },
                Expr::Call {
                    args,
                    func,
                    generic_args,
                } => Expr::Call {
                    args: map_call_args(args, map_fn),
                    func: Box::new(map_ast_node((*func).into(), map_fn).as_expr()),
                    generic_args,
                },
                Expr::CompOperation { left, op, right } => Expr::CompOperation {
                    left: Box::new(map_ast_node((*left).into(), map_fn).as_expr()),
                    right: Box::new(map_ast_node((*right).into(), map_fn).as_expr()),
                    op,
                },
                Expr::List { elts } => Expr::List {
                    elts: elts
                        .into_iter()
                        .map(|val| map_ast_node(val.into(), map_fn).as_expr())
                        .collect(),
                },
                Expr::Subscript { value, index } => Expr::Subscript {
                    value: Box::new(map_ast_node((*value).into(), map_fn).as_expr()),
                    index: Box::new(map_ast_node((*index).into(), map_fn).as_expr()),
                },
                Expr::Ternary {
                    if_expr,
                    test,
                    else_expr,
                } => Expr::Ternary {
                    if_expr: Box::new(map_ast_node((*if_expr).into(), map_fn).as_expr()),
                    test: Box::new(map_ast_node((*test).into(), map_fn).as_expr()),
                    else_expr: Box::new(map_ast_node((*else_expr).into(), map_fn).as_expr()),
                },
                Expr::Tuple { elts } => Expr::Tuple {
                    elts: elts
                        .into_iter()
                        .map(|val| map_ast_node(val.into(), map_fn).as_expr())
                        .collect(),
                },
                Expr::UnaryOperation { op, operand } => Expr::UnaryOperation {
                    op,
                    operand: Box::new(map_ast_node((*operand).into(), map_fn).as_expr()),
                },
                // The following *could* be covered via catch all. However, that would turn into a footgun if we add
                // more expressions in the future that need to be walked. It's better to not use a catch all here.
                Expr::Bool(_)
                | Expr::Name(_)
                | Expr::Num(_)
                | Expr::Path(_)
                | Expr::Str(_)
                | Expr::Unit => expr.kind,
            }
            .into_traceable_node(expr.original_id);

            map_fn(expr.into())
        }
    }
}

fn map_call_args<M>(args: Node<Vec<Node<CallArg>>>, map_fn: &mut M) -> Node<Vec<Node<CallArg>>>
where
    M: FnMut(StmtOrExpr) -> StmtOrExpr,
{
    args.kind
        .into_iter()
        .map(|call_arg| {
            CallArg {
                label: call_arg.kind.label,
                value: map_ast_node(call_arg.kind.value.into(), map_fn).as_expr(),
            }
            .into_traceable_node(call_arg.original_id)
        })
        .collect::<Vec<_>>()
        .into_traceable_node(args.original_id)
}

fn map_body<M>(body: Vec<Node<FuncStmt>>, map_fn: &mut M) -> Vec<Node<FuncStmt>>
where
    M: FnMut(StmtOrExpr) -> StmtOrExpr,
{
    body.into_iter()
        .map(|val| map_ast_node(val.into(), map_fn).as_stmt())
        .collect()
}

/// Returns `true` if the `node_id` matches the `orignal_id` of `node` or any of its children.
pub fn contains_node(node: &Node<Expr>, node_id: NodeId) -> bool {
    let mut success = false;
    let wrapper = FuncStmt::Expr {
        value: node.clone(),
    }
    .into_node();
    map_ast_node(wrapper.into(), &mut |val| {
        match val {
            StmtOrExpr::Expr(ref expr) if expr.original_id == node_id => {
                success = true;
            }
            StmtOrExpr::Stmt(ref stmt) if stmt.original_id == node_id => {
                success = true;
            }
            _ => {}
        }

        val
    });

    success
}

/// Takes a mutable `body` of statements and inserts an `injection` of statements if any of the
/// `test_nodes` or their children contain the given `target_nodeid`.
pub fn inject_if_contains_target(
    body: &mut Vec<Node<FuncStmt>>,
    injection: &[Node<FuncStmt>],
    test_nodes: &[&Node<Expr>],
    target_nodeid: NodeId,
) -> bool {
    if test_nodes
        .iter()
        .any(|val| contains_node(val, target_nodeid))
    {
        for item in injection {
            body.push(item.clone())
        }
        return true;
    }
    false
}

/// Like `inject_if_contains_target` but appends a `fallback` statement to the body if otherwise
/// no injection would take place.
pub fn inject_or_add_current(
    body: &mut Vec<Node<FuncStmt>>,
    injection: &[Node<FuncStmt>],
    test: &[&Node<Expr>],
    target: NodeId,
    fallback: &Node<FuncStmt>,
) {
    if !inject_if_contains_target(body, injection, test, target) {
        body.push(fallback.clone());
    }
}

/// Inject a series of statements within a body of existing statements right before
/// a given `expression` occures but not any earlier.
pub fn inject_before_expression(
    body: &[Node<FuncStmt>],
    expression: NodeId,
    injection: &[Node<FuncStmt>],
) -> Vec<Node<FuncStmt>> {
    let mut transformed_body = vec![];

    for stmt in body {
        let injection_and_current_stmt = &[injection, &[stmt.clone()]].concat();
        match stmt.kind.clone() {
            FuncStmt::If {
                body,
                test,
                or_else,
            } => {
                if !inject_if_contains_target(
                    &mut transformed_body,
                    injection_and_current_stmt,
                    &[&test],
                    expression,
                ) {
                    transformed_body.push(
                        FuncStmt::If {
                            body: inject_before_expression(&body, expression, injection),
                            or_else: inject_before_expression(&or_else, expression, injection),
                            test,
                        }
                        .into_traceable_node(stmt.original_id),
                    );
                }
            }
            FuncStmt::For { target, iter, body } => {
                if !inject_if_contains_target(
                    &mut transformed_body,
                    injection_and_current_stmt,
                    &[&iter],
                    expression,
                ) {
                    transformed_body.push(
                        FuncStmt::For {
                            target,
                            iter,
                            body: inject_before_expression(&body, expression, injection),
                        }
                        .into_traceable_node(stmt.original_id),
                    );
                }
            }
            FuncStmt::While { test, body } => {
                if !inject_if_contains_target(
                    &mut transformed_body,
                    injection_and_current_stmt,
                    &[&test],
                    expression,
                ) {
                    transformed_body.push(
                        FuncStmt::While {
                            test,
                            body: inject_before_expression(&body, expression, injection),
                        }
                        .into_traceable_node(stmt.original_id),
                    )
                }
            }
            FuncStmt::Unsafe(body) => transformed_body.push(
                FuncStmt::Unsafe(inject_before_expression(&body, expression, injection))
                    .into_traceable_node(stmt.original_id),
            ),
            // The following statements contain no further sub statements, only expressions.
            // At this point it doesn't matter how deeply nested our expression is found because
            // expressions can not contain statements.
            // If we find it somewhere in the expression tree, we will inject the code right before it.
            FuncStmt::Expr { value } => {
                inject_or_add_current(
                    &mut transformed_body,
                    injection_and_current_stmt,
                    &[&value],
                    expression,
                    stmt,
                );
            }
            FuncStmt::Assign { target, value } => {
                inject_or_add_current(
                    &mut transformed_body,
                    injection_and_current_stmt,
                    &[&value, &target],
                    expression,
                    stmt,
                );
            }
            FuncStmt::Return { value } => {
                if let Some(val) = value {
                    inject_or_add_current(
                        &mut transformed_body,
                        injection_and_current_stmt,
                        &[&val],
                        expression,
                        stmt,
                    );
                } else {
                    transformed_body.push(stmt.clone())
                }
            }
            FuncStmt::VarDecl { value, .. } => {
                if let Some(val) = value {
                    inject_or_add_current(
                        &mut transformed_body,
                        injection_and_current_stmt,
                        &[&val],
                        expression,
                        stmt,
                    );
                } else {
                    transformed_body.push(stmt.clone())
                }
            }
            FuncStmt::Assert { test, msg } => {
                if let Some(msg) = msg {
                    inject_or_add_current(
                        &mut transformed_body,
                        injection_and_current_stmt,
                        &[&test, &msg],
                        expression,
                        stmt,
                    );
                } else {
                    inject_or_add_current(
                        &mut transformed_body,
                        injection_and_current_stmt,
                        &[&test],
                        expression,
                        stmt,
                    );
                }
            }
            FuncStmt::AugAssign { target, value, .. } => {
                inject_or_add_current(
                    &mut transformed_body,
                    injection_and_current_stmt,
                    &[&target, &value],
                    expression,
                    stmt,
                );
            }
            FuncStmt::Emit { args, .. } => {
                if args
                    .kind
                    .iter()
                    .any(|val| contains_node(&val.kind.value, expression))
                {
                    transformed_body = [&transformed_body, injection].concat();
                }
                transformed_body.push(stmt.clone());
            }
            FuncStmt::Revert { error } => {
                if let Some(error) = error {
                    inject_or_add_current(
                        &mut transformed_body,
                        injection_and_current_stmt,
                        &[&error],
                        expression,
                        stmt,
                    )
                } else {
                    transformed_body.push(stmt.clone())
                }
            }
            FuncStmt::Break | FuncStmt::Continue | FuncStmt::Pass => {
                transformed_body.push(stmt.clone())
            }
        }
    }
    transformed_body
}

/// Turns a ternary expression into a set of statements resembling an if/else block with equal
/// functionality. Expects the type and variable result name to be provided as parameters.
pub fn ternary_to_if(
    ternary_type: FixedSize,
    expr: &Node<Expr>,
    result_name: &str,
) -> Vec<Node<FuncStmt>> {
    if let Expr::Ternary {
        if_expr,
        test,
        else_expr,
    } = &expr.kind
    {
        let mut stmts = vec![FuncStmt::VarDecl {
            target: VarDeclTarget::Name(result_name.into()).into_node(),
            typ: names::fixed_size_type_desc(&ternary_type).into_node(),
            value: None,
        }
        .into_node()];

        let if_branch = FuncStmt::Assign {
            target: Expr::Name(result_name.into()).into_node(),
            value: if_expr
                .kind
                .clone()
                .into_traceable_node(if_expr.original_id),
        }
        .into_node();

        let else_branch = FuncStmt::Assign {
            target: Expr::Name(result_name.into()).into_node(),
            value: else_expr
                .kind
                .clone()
                .into_traceable_node(else_expr.original_id),
        }
        .into_node();

        stmts.push(
            FuncStmt::If {
                test: test.kind.clone().into_node(),
                body: vec![if_branch],
                or_else: vec![else_branch],
            }
            .into_node(),
        );

        return stmts;
    }

    unreachable!()
}

/// Turns a boolean expression into a set of statements resembling an if/else block with equal
/// functionality. Expects the type and variable result name to be provided as parameters.
pub fn boolean_expr_to_if(
    expr_type: FixedSize,
    expr: &Node<Expr>,
    result_name: &str,
) -> Vec<Node<FuncStmt>> {
    if let Expr::BoolOperation { left, op, right } = &expr.kind {
        return match op.kind {
            BoolOperator::And => {
                // from: left && right

                // into:
                // res: bool = false
                // if left:
                //     res = right

                let mut stmts = vec![FuncStmt::VarDecl {
                    target: VarDeclTarget::Name(result_name.into()).into_node(),
                    typ: names::fixed_size_type_desc(&expr_type).into_node(),
                    value: Some(Expr::Bool(false).into_node()),
                }
                .into_node()];
                let if_branch = FuncStmt::Assign {
                    target: Expr::Name(result_name.into()).into_node(),
                    value: right.kind.clone().into_traceable_node(right.original_id),
                }
                .into_node();

                stmts.push(
                    FuncStmt::If {
                        test: left.kind.clone().into_traceable_node(left.original_id),
                        body: vec![if_branch],
                        or_else: vec![],
                    }
                    .into_node(),
                );
                stmts
            }
            BoolOperator::Or => {
                // from: left || right
                // into:
                // res: bool = true
                // if not left:
                //     res = right
                let mut stmts = vec![FuncStmt::VarDecl {
                    target: VarDeclTarget::Name(result_name.into()).into_node(),
                    typ: names::fixed_size_type_desc(&expr_type).into_node(),
                    value: Some(Expr::Bool(true).into_node()),
                }
                .into_node()];
                let if_branch = FuncStmt::Assign {
                    target: Expr::Name(result_name.into()).into_node(),
                    value: right.kind.clone().into_traceable_node(right.original_id),
                }
                .into_node();

                stmts.push(
                    FuncStmt::If {
                        test: Expr::UnaryOperation {
                            op: UnaryOperator::Not.into_node(),
                            operand: Box::new(
                                left.kind.clone().into_traceable_node(left.original_id),
                            ),
                        }
                        .into_node(),
                        body: vec![if_branch],
                        or_else: vec![],
                    }
                    .into_node(),
                );
                stmts
            }
        };
    }

    unreachable!()
}

/// Returns a vector of expressions with all ternary expressions that are
/// contained within the given function statement. The last expression
/// in the list is the outermost ternary expression found in the statement.
pub fn get_all_ternary_expressions(node: &Node<FuncStmt>) -> Vec<Node<Expr>> {
    let mut terns = vec![];
    map_ast_node(node.clone().into(), &mut |exp| {
        if let StmtOrExpr::Expr(expr) = &exp {
            if let Expr::Ternary { .. } = expr.kind {
                terns.push(expr.clone())
            }
        }

        exp
    });

    terns
}

/// For a given set of nodes returns the first set of ternary expressions that can be found.
/// The last expression in the list is the outermost ternary expression found in the statement.
pub fn get_first_ternary_expressions(nodes: &[Node<FuncStmt>]) -> Vec<Node<Expr>> {
    for node in nodes {
        let result = get_all_ternary_expressions(node);
        if !result.is_empty() {
            return result;
        }
    }
    vec![]
}

/// Returns a vector of expressions with all boolean expressions that are
/// contained within the given function statement. The last expression
/// in the list is the outermost boolean expression found in the statement.
pub fn get_all_boolean_expressions(node: &Node<FuncStmt>) -> Vec<Node<Expr>> {
    let mut expressions = vec![];
    map_ast_node(node.clone().into(), &mut |exp| {
        if let StmtOrExpr::Expr(expr) = &exp {
            if let Expr::BoolOperation { .. } = expr.kind {
                expressions.push(expr.clone())
            }
        }

        exp
    });

    expressions
}

/// For a given set of nodes returns the first set of boolean expressions that can be found.
/// The last expression in the list is the outermost boolean expression found in the statement.
pub fn get_first_boolean_expressions(nodes: &[Node<FuncStmt>]) -> Vec<Node<Expr>> {
    for node in nodes {
        let result = get_all_boolean_expressions(node);
        if !result.is_empty() {
            return result;
        }
    }
    vec![]
}

/// In a given set of `nodes replaces a node that matches the `node_id` with a name expression node.
pub fn replace_node_with_name_expression(
    nodes: &[Node<FuncStmt>],
    node_id: NodeId,
    name: &str,
) -> Vec<Node<FuncStmt>> {
    nodes
        .iter()
        .map(|node| {
            map_ast_node(node.clone().into(), &mut |val| match val {
                StmtOrExpr::Expr(expr) if expr.original_id == node_id => {
                    Expr::Name(name.into()).into_node().into()
                }
                _ => val,
            })
            .as_stmt()
        })
        .collect()
}

#[cfg(test)]
mod tests {

    use crate::ast_utils::get_first_ternary_expressions;
    use crate::ast_utils::inject_before_expression;
    use crate::ast_utils::map_ast_node;
    use crate::ast_utils::replace_node_with_name_expression;
    use crate::ast_utils::ternary_to_if;
    use crate::ast_utils::StmtOrExpr;
    use crate::utils::ZeroSpanNode;
    use fe_analyzer::namespace::types::FixedSize;
    use fe_parser::ast::BinOperator;
    use fe_parser::ast::CallArg;
    use fe_parser::ast::Expr;
    use fe_parser::ast::FuncStmt;
    use fe_parser::node::Node;
    use std::vec;

    fn to_code(body: &[Node<FuncStmt>]) -> String {
        let mut source = String::new();
        for stmt in body {
            source.push_str(&stmt.kind.to_string());
            source.push('\n');
        }
        source.trim().into()
    }

    #[test]
    fn transform_statement() {
        let stmt = FuncStmt::Return { value: None }.into_node();
        assert_eq!(to_code(&[stmt.clone()]), "return");

        let transformed_stmt = map_ast_node(stmt.into(), &mut |stmt| {
            if let StmtOrExpr::Stmt(stmt) = stmt {
                let node = match stmt.kind {
                    FuncStmt::Return { .. } => FuncStmt::Return {
                        value: Some(Expr::Name("foo".into()).into_node()),
                    },
                    _ => stmt.kind,
                }
                .into_node();
                return node.into();
            }
            stmt
        })
        .as_stmt();

        assert_eq!(to_code(&[transformed_stmt]), "return foo");
    }

    #[test]
    fn transform_expr() {
        let stmt = FuncStmt::Return {
            value: Some(Expr::Name("foo".into()).into_node()),
        }
        .into_node();
        assert_eq!(to_code(&[stmt.clone()]), "return foo");

        let mut transform_expr = |expr| {
            if let StmtOrExpr::Expr(expr) = expr {
                let node = match expr.kind {
                    Expr::Name(_) => Expr::Name("bar".into()),
                    _ => expr.kind,
                }
                .into_node();
                return node.into();
            }
            expr
        };

        let transformed_stmt = map_ast_node(stmt.into(), &mut transform_expr).as_stmt();

        assert_eq!(to_code(&[transformed_stmt]), "return bar");
    }

    #[test]
    fn count_expr() {
        let add_expr = Expr::BinOperation {
            left: Expr::Num("1".into()).into_boxed_node(),
            right: Expr::Num("2".into()).into_boxed_node(),
            op: BinOperator::Add.into_node(),
        };
        let stmt = FuncStmt::Return {
            value: Some(add_expr.into_node()),
        }
        .into_node();

        let mut counter = 0;

        let mut transform_expr = |expr| {
            if let StmtOrExpr::Expr(expr) = expr {
                let node = match expr.kind.clone() {
                    Expr::Num(_) => {
                        counter += 1;
                        expr.kind
                    }
                    _ => expr.kind,
                }
                .into_node();
                return node.into();
            }

            expr
        };

        map_ast_node(stmt.into(), &mut transform_expr);

        assert_eq!(counter, 2)
    }

    #[test]
    fn inject_expression() {
        let nested_ternary = Expr::Ternary {
            test: Expr::Bool(true).into_boxed_node(),
            if_expr: Expr::Num("1".into()).into_boxed_node(),
            else_expr: Expr::Num("2".into()).into_boxed_node(),
        }
        .into_boxed_node();

        let ternary = Expr::Ternary {
            test: Expr::Bool(true).into_boxed_node(),
            if_expr: Expr::Num("1".into()).into_boxed_node(),
            else_expr: nested_ternary,
        }
        .into_node();

        let replacement = vec![FuncStmt::Return { value: None }.into_node()];

        let stmt = FuncStmt::Expr {
            value: ternary.clone(),
        }
        .into_node();

        let original_body = vec![stmt];
        assert_eq!(to_code(&original_body), "1 if true else 1 if true else 2");
        let body = inject_before_expression(&original_body, ternary.original_id, &replacement);

        assert_eq!(
            to_code(&body),
            "return
1 if true else 1 if true else 2"
        );
    }

    #[test]
    fn inject_expression_nested() {
        let nested_ternary = Expr::Ternary {
            test: Expr::Bool(true).into_boxed_node(),
            if_expr: Expr::Num("1".into()).into_boxed_node(),
            else_expr: Expr::Num("2".into()).into_boxed_node(),
        }
        .into_boxed_node();

        let ternary = Expr::Ternary {
            test: Expr::Bool(true).into_boxed_node(),
            if_expr: Expr::Num("1".into()).into_boxed_node(),
            else_expr: nested_ternary,
        }
        .into_node();

        let if_else = FuncStmt::If {
            test: Expr::Bool(true).into_node(),
            body: vec![],
            or_else: vec![FuncStmt::Expr {
                value: ternary.clone(),
            }
            .into_node()],
        }
        .into_node();

        let replacement = vec![FuncStmt::Return { value: None }.into_node()];

        let original_body = vec![if_else];
        assert_eq!(
            to_code(&original_body),
            "if true:

else:
    1 if true else 1 if true else 2"
        );

        let body = inject_before_expression(&original_body, ternary.original_id, &replacement);

        assert_eq!(
            to_code(&body),
            "if true:

else:
    return
    1 if true else 1 if true else 2"
        );
    }

    #[test]
    fn lower_nested_ternary() {
        let nested_ternary = Expr::Ternary {
            test: Expr::Bool(true).into_boxed_node(),
            if_expr: Expr::Num("1".into()).into_boxed_node(),
            else_expr: Expr::Num("2".into()).into_boxed_node(),
        }
        .into_boxed_node();

        let ternary = Expr::Ternary {
            test: Expr::Bool(true).into_boxed_node(),
            if_expr: Expr::Num("1".into()).into_boxed_node(),
            else_expr: nested_ternary.clone(),
        }
        .into_node();

        let call = Expr::Call {
            func: Expr::Name("foo".into()).into_boxed_node(),
            args: vec![CallArg {
                value: ternary.clone(),
                label: None,
            }
            .into_node()]
            .into_node(),
            generic_args: None,
        }
        .into_node();

        let stmt = FuncStmt::Expr { value: call }.into_node();

        let original_body = vec![stmt];

        assert_eq!(
            to_code(&original_body),
            "foo(1 if true else 1 if true else 2)"
        );

        let all_ternary = get_first_ternary_expressions(&original_body);

        assert_eq!(all_ternary.len(), 2);

        // They are collected inside-out which means the lowest level is at the end of the vec.
        let first_ternary = all_ternary.get(0).unwrap();
        assert_eq!(first_ternary.original_id, nested_ternary.original_id);
        let second_ternary = all_ternary.get(1).unwrap();
        assert_eq!(second_ternary.original_id, ternary.original_id);

        let ternary_type = FixedSize::u256();

        let transformed_outer = ternary_to_if(ternary_type.clone(), second_ternary, "outer");

        assert_eq!(
            to_code(&transformed_outer),
            "let outer: u256
if true:
    outer = 1
else:
    outer = 1 if true else 2"
        );

        let new_body = inject_before_expression(
            &original_body,
            second_ternary.original_id,
            &transformed_outer,
        );

        assert_eq!(
            to_code(&new_body),
            "let outer: u256
if true:
    outer = 1
else:
    outer = 1 if true else 2

foo(1 if true else 1 if true else 2)"
        );

        let new_body =
            replace_node_with_name_expression(&new_body, second_ternary.original_id, "outer");

        assert_eq!(
            to_code(&new_body),
            "let outer: u256
if true:
    outer = 1
else:
    outer = 1 if true else 2

foo(outer)"
        );

        let remaining_terns = get_first_ternary_expressions(&new_body);
        let last_ternary = remaining_terns.last().unwrap();
        let transformed_inner = ternary_to_if(ternary_type, last_ternary, "inner");

        let new_body =
            inject_before_expression(&new_body, last_ternary.original_id, &transformed_inner);

        let new_body =
            replace_node_with_name_expression(&new_body, last_ternary.original_id, "inner");

        assert_eq!(
            to_code(&new_body),
            r#"let outer: u256
if true:
    outer = 1
else:
    let inner: u256
    if true:
        inner = 1
    else:
        inner = 2

    outer = inner

foo(outer)"#
        )
    }
}
