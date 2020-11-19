use crate::errors::CompileError;
use crate::yul::mappers::expressions;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use fe_semantics::namespace::types::{
    Array,
    FeSized,
    FixedSize,
};
use fe_semantics::Context;
use yultsur::*;

/// Builds a Yul statement from a Fe variable declaration
pub fn var_decl(
    context: &Context,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let Some(typ) = context.get_declaration(stmt) {
        return match typ {
            FixedSize::Base(_) => var_decl_base(context, stmt),
            FixedSize::Array(array) => var_decl_array(context, stmt, array.to_owned()),
            FixedSize::Tuple(_) => unimplemented!(),
            FixedSize::String(_) => unimplemented!(),
        };
    }

    unreachable!()
}

fn var_decl_base(
    context: &Context,
    decl: &Spanned<fe::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::VarDecl {
        target,
        typ: _,
        value,
    } = &decl.node
    {
        let target = identifier! { (expressions::expr_name_string(target)?) };

        return Ok(if let Some(value) = value {
            let value = expressions::expr(context, &value)?;
            statement! { let [target] := [value] }
        } else {
            statement! { let [target] := 0 }
        });
    }

    unreachable!()
}

fn var_decl_array(
    _context: &Context,
    decl: &Spanned<fe::FuncStmt>,
    array: Array,
) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::VarDecl {
        target,
        typ: _,
        value,
    } = &decl.node
    {
        let target = identifier! { (expressions::expr_name_string(target)?) };
        let size = literal_expression! { (array.size()) };

        return Ok(if value.is_some() {
            unimplemented!()
        } else {
            statement! { let [target] := alloc([size]) }
        });
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use crate::yul::mappers::declarations::var_decl;
    use fe_parser as parser;
    use fe_semantics::namespace::types::{
        Array,
        Base,
        FixedSize,
        Type,
        U256,
    };
    use fe_semantics::test_utils::ContextHarness;
    use fe_semantics::{
        Context,
        ExpressionAttributes,
        Location,
    };

    fn map(context: &Context, src: &str) -> String {
        let tokens = parser::get_parse_tokens(src).expect("Couldn't parse declaration");
        let stmt = &parser::parsers::vardecl_stmt(&tokens[..])
            .expect("Couldn't build declaration AST")
            .1;

        let decl = var_decl(context, &stmt).expect("Couldn't map declaration AST");

        decl.to_string()
    }

    #[test]
    fn decl_u256() {
        let mut harness = ContextHarness::new("foo: u256 = bar");
        harness.add_declaration("foo: u256 = bar", FixedSize::Base(U256));
        harness.add_expression(
            "bar",
            ExpressionAttributes::new(Type::Base(U256), Location::Value),
        );

        assert_eq!(map(&harness.context, &harness.src), "let foo := bar");
    }

    #[test]
    fn decl_array() {
        let mut harness = ContextHarness::new("foo: address[10]");
        harness.add_declaration(
            "foo: address[10]",
            FixedSize::Array(Array {
                dimension: 10,
                inner: Base::Address,
            }),
        );

        assert_eq!(map(&harness.context, &harness.src), "let foo := alloc(200)");
    }
}
