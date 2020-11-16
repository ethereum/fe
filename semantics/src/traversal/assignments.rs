use crate::errors::SemanticError;
use crate::namespace::operations;
use crate::namespace::scopes::{
    BlockScope,
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
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Assign { targets, value } = &stmt.node {
        if targets.len() > 1 {
            unimplemented!()
        }

        if let Some(target) = targets.first() {
            match &target.node {
                fe::Expr::Name(_) => assign_name(scope, Rc::clone(&context), target, value)?,
                fe::Expr::Subscript { .. } => {
                    assign_subscript(scope, Rc::clone(&context), target, value)?
                }
                _ => return Err(SemanticError::UnassignableExpression),
            }
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
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    target: &Spanned<fe::Expr>,
    value: &Spanned<fe::Expr>,
) -> Result<(), SemanticError> {
    if let fe::Expr::Subscript {
        value: target,
        slices,
    } = &target.node
    {
        let target_attributes = expressions::expr(Rc::clone(&scope), Rc::clone(&context), target)?;
        let value_attributes = expressions::expr(Rc::clone(&scope), Rc::clone(&context), value)?;
        let index_attributes = expressions::slices_index(scope, context, slices)?;

        let indexed_type = operations::index(target_attributes.typ, index_attributes.typ)?;

        if indexed_type != value_attributes.typ {
            return Err(SemanticError::TypeError);
        }

        return Ok(());
    }

    unreachable!()
}

/// Gather context information for named assignments and check for type errors.
///
/// e.g. `foo = 42`
fn assign_name(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    target: &Spanned<fe::Expr>,
    value: &Spanned<fe::Expr>,
) -> Result<(), SemanticError> {
    let target_attributes = expressions::expr(Rc::clone(&scope), Rc::clone(&context), target)?;
    let value_attributes = expressions::expr(scope, context, value)?;

    if target_attributes.typ != value_attributes.typ {
        return Err(SemanticError::TypeError);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::errors::SemanticError;
    use crate::namespace::scopes::{
        BlockScope,
        ContractScope,
        ModuleScope,
        Shared,
    };
    use crate::namespace::types::{
        Array,
        Base,
        Map,
        Type,
    };
    use crate::traversal::assignments::assign;
    use crate::Context;
    use fe_parser as parser;
    use fe_parser::span::Span;
    use rstest::rstest;
    use std::rc::Rc;

    // comes with a few values in scope:
    // - self.foobar: Map<u256, u256>
    // - foo: u256
    // - bar: u256[100]
    fn scope() -> Shared<BlockScope> {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        contract_scope.borrow_mut().add_map(
            "foobar".to_string(),
            Map {
                key: Base::U256,
                value: Box::new(Type::Base(Base::U256)),
            },
        );
        let function_scope = BlockScope::from_contract_scope(Span::new(0, 0), contract_scope);
        function_scope
            .borrow_mut()
            .add_base("foo".to_string(), Base::U256);
        function_scope.borrow_mut().add_array(
            "bar".to_string(),
            Array {
                inner: Base::U256,
                dimension: 100,
            },
        );
        function_scope
    }

    fn analyze(scope: Shared<BlockScope>, src: &str) -> Result<Context, SemanticError> {
        let context = Context::new_shared();
        let tokens = parser::get_parse_tokens(src).expect("Couldn't parse expression");
        let assignment = &parser::parsers::assign_stmt(&tokens[..])
            .expect("Couldn't build assigment AST")
            .1;

        assign(scope, Rc::clone(&context), assignment)?;
        Ok(Rc::try_unwrap(context)
            .map_err(|_| "")
            .unwrap()
            .into_inner())
    }

    #[rstest(
        assignment,
        expected_num_expr_attrs,
        case("foo = 42", 2),
        case("bar[26] = 42", 3),
        case("self.foobar[26 + 26] = 42", 5)
    )]
    fn basic_assigns(assignment: &str, expected_num_expr_attrs: usize) {
        let context = analyze(scope(), assignment).expect("failed to analyze the assignment");
        assert_eq!(context.expressions.len(), expected_num_expr_attrs)
    }

    #[rstest(assignment, case("bar = 42"), case("self.foobar[42] = bar"))]
    fn type_error_assigns(assignment: &str) {
        let result = analyze(scope(), assignment);
        assert_eq!(
            result.expect_err("semantic analysis did not return an error"),
            SemanticError::TypeError
        )
    }

    #[rstest(assignment, case("self.foobar = foo"))]
    fn unassignable_assigns(assignment: &str) {
        let result = analyze(scope(), assignment);
        assert_eq!(
            result.expect_err("semantic analysis did not return an error"),
            SemanticError::UnassignableExpression
        )
    }
}
