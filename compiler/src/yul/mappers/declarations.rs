use crate::errors::CompileError;
use crate::yul::mappers::expressions::{expr, expr_name_string};
use crate::yul::mappers::types;
use crate::yul::namespace::scopes::{
    ContractDef, ContractScope, FunctionDef, FunctionScope, Scope, Shared,
};
use crate::yul::namespace::types::{FixedSize, Type, Base, Array};
use std::rc::Rc;
use vyper_parser::ast as vyp;
use vyper_parser::span::Spanned;
use yultsur::*;

/// Builds a Yul statement from a Vyper variable declaration
pub fn var_decl(
    scope: Shared<FunctionScope>,
    target: &vyp::Expr,
    typ: &vyp::TypeDesc,
    value: &Option<Spanned<vyp::Expr>>,
) -> Result<yul::Statement, CompileError> {
    match types::type_desc(Scope::Function(Rc::clone(&scope)), typ)? {
        Type::Base(base) => var_decl_base(scope, target, base, value),
        Type::Array(array) => var_decl_array(scope, target, array, value),
        Type::Map(_) => {
            Err(CompileError::static_str("Cannot declare map in function"))
        }
    }
}

fn var_decl_base(
    scope: Shared<FunctionScope>,
    target: &vyp::Expr,
    base: Base,
    value: &Option<Spanned<vyp::Expr>>,
) -> Result<yul::Statement, CompileError> {
    let name = expr_name_string(target)?;
    scope.borrow_mut().add_base(name.clone(), base);

    if let Some(value) = value {
        let (value, _) = expr(scope, &value.node)?;
        Ok(statement! { let [identifier! {(name)}] := [value] })
    } else {
        Ok(statement! { let [identifier! {(name)}] := 0 })
    }
}

fn var_decl_array(
    scope: Shared<FunctionScope>,
    target: &vyp::Expr,
    array: Array,
    value: &Option<Spanned<vyp::Expr>>,
) -> Result<yul::Statement, CompileError> {
    let name = expr_name_string(target)?;
    let size = literal_expression! {(array.size())};

    scope.borrow_mut().add_array(name.clone(), array);

    if let Some(value) = value {
        Err(CompileError::static_str("Array copying not supported yet"))
    } else {
        Ok(statement! { let [identifier! {(name)}] := alloc([size]) })
    }
}

#[cfg(test)]
mod tests {
    use crate::yul::namespace::scopes::{ContractScope, FunctionScope, ModuleScope, Shared, FunctionDef};
    use crate::yul::namespace::types::{Array, Base, FixedSize, Map, Type};
    use crate::yul::mappers::assignments::assign;
    use std::rc::Rc;
    use vyper_parser as parser;
    use vyper_parser::ast as vyp;
    use crate::yul::mappers::declarations::var_decl;

    fn scope() -> Shared<FunctionScope> {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        FunctionScope::new(contract_scope)
    }

    fn map(scope: Shared<FunctionScope>, src: &str) -> String {
        let tokens = parser::get_parse_tokens(src).expect("Couldn't parse declaration");
        let stmt = &parser::parsers::vardecl_stmt(&tokens[..])
            .expect("Couldn't build declaration AST")
            .1
            .node;

        if let vyp::FuncStmt::VarDecl { target, typ, value } = stmt {
            let decl = var_decl(
                scope,
                &target.node,
                &typ.node,
                value
            ).expect("Couldn't map declaration AST");

            decl.to_string()
        } else {
            panic!("Didn't get a declaration")
        }
    }

    #[test]
    fn decl_u256() {
        let scope = scope();
        scope.borrow_mut().add_base(
            "bar".to_string(),
            Base::U256
        );

        assert_eq!(
            map(Rc::clone(&scope), "foo: u256 = bar"),
            "let foo := bar"
        );

        let foo_def = scope.borrow().def("foo".to_string()).unwrap();
        assert_eq!(
            foo_def,
            FunctionDef::Base(Base::U256)
        )
    }

    #[test]
    fn decl_array() {
        let scope = scope();

        assert_eq!(
            map(Rc::clone(&scope), "foo: address[10]"),
            "let foo := alloc(200)"
        );

        let foo_def = scope.borrow().def("foo".to_string()).unwrap();
        assert_eq!(
            foo_def,
            FunctionDef::Array(Array { dimension: 10, inner: Base::Address })
        )
    }

    #[test]
    fn decl_type_def() {
        let scope = scope();
        scope.borrow_mut().module_scope().borrow_mut().add_type_def(
            "FourAddresses".to_string(),
            Type::Array(Array {
                dimension: 4,
                inner: Base::Address
            })
        );

        assert_eq!(
            map(Rc::clone(&scope), "foo: FourAddresses"),
            "let foo := alloc(80)"
        );

        let foo_def = scope.borrow().def("foo".to_string()).unwrap();
        assert_eq!(
            foo_def,
            FunctionDef::Array(Array { dimension: 4, inner: Base::Address })
        )
    }
}

