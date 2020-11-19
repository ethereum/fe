use crate::errors::SemanticError;
use crate::namespace::scopes::{
    BlockScope,
    Scope,
    Shared,
};
use crate::traversal::{
    expressions,
    types,
};
use crate::Context;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use std::rc::Rc;

/// Gather context information for var declarations and check for type errors.
pub fn var_decl(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::VarDecl { target, typ, value } = &stmt.node {
        let name = expressions::expr_name_string(target)?;
        let declared_type = types::type_desc_fixed_size(Scope::Block(Rc::clone(&scope)), typ)?;
        if let Some(value) = value {
            let value_attributes =
                expressions::expr(Rc::clone(&scope), Rc::clone(&context), value)?;
            if declared_type.clone().into_type() != value_attributes.typ {
                return Err(SemanticError::TypeError);
            }
        }

        scope
            .borrow_mut()
            .add_var(name, declared_type.clone().into_type());
        context.borrow_mut().add_declaration(stmt, declared_type);

        return Ok(());
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use crate::errors::SemanticError;
    use crate::namespace::scopes::{
        BlockDef,
        BlockScope,
        ContractScope,
        ModuleScope,
        Shared,
    };
    use crate::namespace::types::{
        Base,
        Type,
    };
    use crate::traversal::declarations::var_decl;
    use crate::Context;
    use fe_parser as parser;
    use fe_parser::span::Span;
    use std::rc::Rc;

    fn scope() -> Shared<BlockScope> {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        BlockScope::from_contract_scope(Span::new(0, 0), contract_scope)
    }

    fn analyze(scope: Shared<BlockScope>, src: &str) -> Result<Context, SemanticError> {
        let context = Context::new_shared();
        let tokens = parser::get_parse_tokens(src).expect("Couldn't parse expression");
        let statement = &parser::parsers::vardecl_stmt(&tokens[..])
            .expect("Couldn't build statement AST")
            .1;

        var_decl(scope, Rc::clone(&context), statement)?;
        Ok(Rc::try_unwrap(context)
            .map_err(|_| "")
            .unwrap()
            .into_inner())
    }

    #[test]
    fn simple_decl() {
        let statement = "foo: u256 = 26 + 42";
        let scope = scope();
        let context = analyze(Rc::clone(&scope), statement).expect("analysis failed");
        assert_eq!(context.expressions.len(), 3);
        assert_eq!(
            scope.borrow().def("foo".to_string()),
            Some(BlockDef::Variable(Type::Base(Base::U256)))
        );
    }

    #[test]
    fn type_error_decl() {
        let statement = "foo: u256[100] = 26";
        let result = analyze(scope(), statement);
        assert_eq!(
            result.expect_err("analysis didn't fail"),
            SemanticError::TypeError
        );
    }
}
