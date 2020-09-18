use crate::errors::SemanticError;
use crate::namespace::scopes::{
    FunctionScope,
    Shared,
};
use crate::traversal::expressions;
use crate::Context;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use std::rc::Rc;

/// Gather context information for assignments and check for type errors.
///
/// e.g. `foo[42] = "bar"`, `self.foo[42] = "bar"`, `foo = 42`
pub fn assign(
    scope: Shared<FunctionScope>,
    context: Shared<Context>,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Assign { targets, value } = &stmt.node {
        if targets.len() > 1 {
            unimplemented!()
        }

        if let Some(target) = targets.first() {
            match &target.node {
                fe::Expr::Name(_) => assign_name(scope, context, target, value)?,
                fe::Expr::Subscript { .. } => assign_subscript(scope, context, target, value)?,
                _ => unimplemented!(),
            };
        }

        return Ok(());
    }

    unreachable!()
}

/// Gather context information for subscript assignments and check for type
/// errors.
///
/// e.g. `foo[42] = "bar"`, `self.foo[42] = "bar"`
fn assign_subscript(
    scope: Shared<FunctionScope>,
    context: Shared<Context>,
    target: &Spanned<fe::Expr>,
    value: &Spanned<fe::Expr>,
) -> Result<(), SemanticError> {
    if let fe::Expr::Subscript {
        value: target_value,
        slices,
    } = &target.node
    {
        match &target_value.node {
            fe::Expr::Name(_) => {
                assign_subscript_name(scope, context, target_value, slices, value)?;
            }
            fe::Expr::Attribute { .. } => {
                assign_subscript_attribute(scope, context, target_value, slices, value)?;
            }
            _ => return Err(SemanticError::UnassignableExpression),
        };

        return Ok(());
    }

    unreachable!()
}

/// Gather context information for named subscript assignments and check for
/// type errors.
///
/// e.g. `foo[42] = "bar"`
fn assign_subscript_name(
    scope: Shared<FunctionScope>,
    context: Shared<Context>,
    _target_value: &Spanned<fe::Expr>,
    slices: &Spanned<Vec<Spanned<fe::Slice>>>,
    value: &Spanned<fe::Expr>,
) -> Result<(), SemanticError> {
    expressions::expr(Rc::clone(&scope), Rc::clone(&context), value)?;
    expressions::slices_index(scope, context, slices)?;

    // TODO: perform type checking
    // let name = expressions::expr_name_string(target_value)?;
    // match scope.borrow().def(name) {
    //   ...
    // }

    Ok(())
}

/// Gather context information for attribute subscript assignments and check for
/// type errors.
///
/// e.g. `self.foo[42] = "bar"`
fn assign_subscript_attribute(
    scope: Shared<FunctionScope>,
    context: Shared<Context>,
    target_value: &Spanned<fe::Expr>,
    slices: &Spanned<Vec<Spanned<fe::Slice>>>,
    value: &Spanned<fe::Expr>,
) -> Result<(), SemanticError> {
    if let fe::Expr::Attribute {
        value: target_value,
        attr,
    } = &target_value.node
    {
        match expressions::expr_name_str(target_value)? {
            "self" => assign_subscript_self(scope, context, attr, slices, value)?,
            _ => return Err(SemanticError::UnrecognizedValue),
        };

        return Ok(());
    }

    unreachable!()
}

/// Gather context information for self attribute subscript assignments and
/// check for type errors.
///
/// e.g. `self.foo[42] = "bar"`
fn assign_subscript_self(
    scope: Shared<FunctionScope>,
    context: Shared<Context>,
    _attr: &Spanned<&str>,
    slices: &Spanned<Vec<Spanned<fe::Slice>>>,
    value: &Spanned<fe::Expr>,
) -> Result<(), SemanticError> {
    let _value_attributes = expressions::expr(Rc::clone(&scope), Rc::clone(&context), value)?;
    let _index_attributes = expressions::slices_index(scope, context, slices)?;

    // TODO:: Perform type checking
    // match scope.borrow().contract_def(name) {
    //   ...
    // };

    Ok(())
}

/// Gather context information for named assignments and check for type errors.
///
/// e.g. `foo = 42`
fn assign_name(
    scope: Shared<FunctionScope>,
    context: Shared<Context>,
    _target: &Spanned<fe::Expr>,
    value: &Spanned<fe::Expr>,
) -> Result<(), SemanticError> {
    let _attributes = expressions::expr(scope, context, value)?;

    // TODO:: Perform type checking

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::namespace::scopes::{
        ContractScope,
        FunctionScope,
        ModuleScope,
        Shared,
    };
    use crate::traversal::assignments::assign;
    use crate::Context;
    use fe_parser as parser;
    use rstest::rstest;
    use std::rc::Rc;

    fn scope() -> Shared<FunctionScope> {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        FunctionScope::new(contract_scope)
    }

    fn analyze(scope: Shared<FunctionScope>, src: &str) -> Context {
        let context = Context::new();
        let tokens = parser::get_parse_tokens(src).expect("Couldn't parse expression");
        let assignment = &parser::parsers::assign_stmt(&tokens[..])
            .expect("Couldn't build assigment AST")
            .1;

        assign(scope, Rc::clone(&context), assignment).expect("Couldn't map assignment AST");
        Rc::try_unwrap(context)
            .map_err(|_| "")
            .unwrap()
            .into_inner()
    }

    #[rstest(
        assignment,
        expected_num_expr_attrs,
        case("foo = 42", 1),
        case("foo[26] = 42", 2),
        case("self.foo[26 + 26] = 42", 4)
    )]
    fn assigns(assignment: &str, expected_num_expr_attrs: usize) {
        let context = analyze(scope(), assignment);
        assert_eq!(context.expressions.len(), expected_num_expr_attrs)
    }
}
