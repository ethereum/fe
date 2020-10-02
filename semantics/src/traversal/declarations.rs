use crate::errors::SemanticError;
use crate::namespace::scopes::{
    FunctionScope,
    Scope,
    Shared,
};
use crate::namespace::types::FixedSize;
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
    scope: Shared<FunctionScope>,
    context: Shared<Context>,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::VarDecl { target, typ, value } = &stmt.node {
        let name = expressions::expr_name_string(target)?;
        let declared_type = types::type_desc_fixed_size(Scope::Function(Rc::clone(&scope)), typ)?;
        if let Some(value) = value {
            let _value_type = expressions::expr(Rc::clone(&scope), Rc::clone(&context), value);
            // TODO: Perform type checking
        }

        match declared_type.clone() {
            FixedSize::Base(base) => scope.borrow_mut().add_base(name, base),
            FixedSize::Array(array) => scope.borrow_mut().add_array(name, array),
        };
        context.borrow_mut().add_declaration(stmt, declared_type);

        return Ok(());
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use crate::namespace::scopes::{
        ContractScope,
        FunctionDef,
        FunctionScope,
        ModuleScope,
        Shared,
    };
    use crate::namespace::types::Base;
    use crate::traversal::declarations::var_decl;
    use crate::Context;
    use fe_parser as parser;
    use std::rc::Rc;

    fn scope() -> Shared<FunctionScope> {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        FunctionScope::new(contract_scope)
    }

    fn analyze(scope: Shared<FunctionScope>, src: &str) -> Context {
        let context = Context::new_shared();
        let tokens = parser::get_parse_tokens(src).expect("Couldn't parse expression");
        let statement = &parser::parsers::vardecl_stmt(&tokens[..])
            .expect("Couldn't build statement AST")
            .1;

        var_decl(scope, Rc::clone(&context), statement).expect("Couldn't map assignment AST");
        Rc::try_unwrap(context)
            .map_err(|_| "")
            .unwrap()
            .into_inner()
    }

    #[test]
    fn simple_var_decl() {
        let statement = "foo: u256 = 26 + 42";
        let scope = scope();
        let context = analyze(Rc::clone(&scope), statement);
        assert_eq!(context.expressions.len(), 3);
        assert_eq!(
            scope.borrow().def("foo".to_string()),
            Some(FunctionDef::Base(Base::U256))
        );
    }
}
