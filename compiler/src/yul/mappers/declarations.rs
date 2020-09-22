use crate::errors::CompileError;
use crate::yul::mappers::{
    expressions,
    types,
};
use crate::yul::namespace::scopes::{
    FunctionScope,
    Scope,
    Shared,
};
use crate::yul::namespace::types::{
    Array,
    Base,
    Type,
};
use fe_parser::ast as vyp;
use fe_parser::span::Spanned;
use std::rc::Rc;
use yultsur::*;

/// Builds a Yul statement from a Fe variable declaration
pub fn var_decl(
    scope: Shared<FunctionScope>,
    stmt: &Spanned<vyp::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::FuncStmt::VarDecl { target, typ, value } = &stmt.node {
        return match types::type_desc(Scope::Function(Rc::clone(&scope)), typ)? {
            Type::Base(base) => var_decl_base(scope, target, value, base),
            Type::Array(array) => var_decl_array(scope, target, value, array),
            Type::Map(_) => Err(CompileError::static_str("cannot declare map in function")),
        };
    }

    unreachable!()
}

fn var_decl_base(
    scope: Shared<FunctionScope>,
    target: &Spanned<vyp::Expr>,
    value: &Option<Spanned<vyp::Expr>>,
    base: Base,
) -> Result<yul::Statement, CompileError> {
    if let vyp::Expr::Name(name) = target.node {
        scope.borrow_mut().add_base(name.to_string(), base);

        let identifier = identifier! {(name)};

        return Ok(if let Some(value) = value {
            let value = expressions::expr(scope, &value)?.expression;
            statement! { let [identifier] := [value] }
        } else {
            statement! { let [identifier] := 0 }
        });
    }

    unreachable!()
}

fn var_decl_array(
    scope: Shared<FunctionScope>,
    target: &Spanned<vyp::Expr>,
    value: &Option<Spanned<vyp::Expr>>,
    array: Array,
) -> Result<yul::Statement, CompileError> {
    if let vyp::Expr::Name(name) = target.node {
        let identifier = identifier! {(name)};
        let size = literal_expression! {(array.size())};

        scope.borrow_mut().add_array(name.to_string(), array);

        return Ok(if value.is_some() {
            unimplemented!("array copying")
        } else {
            statement! { let [identifier] := alloc([size]) }
        });
    }

    unreachable!()
}

#[cfg(test)]
mod tests {

    use crate::yul::mappers::declarations::var_decl;
    use crate::yul::namespace::scopes::{
        ContractScope,
        FunctionDef,
        FunctionScope,
        ModuleScope,
        Shared,
    };
    use crate::yul::namespace::types::{
        Array,
        Base,
        Type,
    };
    use fe_parser as parser;
    use std::rc::Rc;

    fn scope() -> Shared<FunctionScope> {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        FunctionScope::new(contract_scope)
    }

    fn map(scope: Shared<FunctionScope>, src: &str) -> String {
        let tokens = parser::get_parse_tokens(src).expect("Couldn't parse declaration");
        let stmt = &parser::parsers::vardecl_stmt(&tokens[..])
            .expect("Couldn't build declaration AST")
            .1;

        let decl = var_decl(scope, &stmt).expect("Couldn't map declaration AST");

        decl.to_string()
    }

    #[test]
    fn decl_u256() {
        let scope = scope();
        scope.borrow_mut().add_base("bar".to_string(), Base::U256);

        assert_eq!(
            map(Rc::clone(&scope), "foo: uint256 = bar"),
            "let foo := bar"
        );

        let foo_def = scope.borrow().def("foo".to_string()).unwrap();
        assert_eq!(foo_def, FunctionDef::Base(Base::U256))
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
            FunctionDef::Array(Array {
                dimension: 10,
                inner: Base::Address
            })
        )
    }

    #[test]
    fn decl_type_def() {
        let scope = scope();
        scope.borrow_mut().module_scope().borrow_mut().add_type_def(
            "FourAddresses".to_string(),
            Type::Array(Array {
                dimension: 4,
                inner: Base::Address,
            }),
        );

        assert_eq!(
            map(Rc::clone(&scope), "foo: FourAddresses"),
            "let foo := alloc(80)"
        );

        let foo_def = scope.borrow().def("foo".to_string()).unwrap();
        assert_eq!(
            foo_def,
            FunctionDef::Array(Array {
                dimension: 4,
                inner: Base::Address
            })
        )
    }
}
