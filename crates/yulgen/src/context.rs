use crate::AnalyzerDb;
use fe_analyzer::context::{CallType, ExpressionAttributes, FunctionBody};
use fe_analyzer::namespace::types::{Event, FeString, FixedSize, Struct};
use fe_parser::ast;
use fe_parser::node::Node;
use indexmap::IndexSet;
use std::rc::Rc;

#[derive(Default)]
pub struct ContractContext {
    /// String literals used in the contract
    pub string_literals: IndexSet<String>,

    /// Names of contracts that have been created inside of this contract.
    pub created_contracts: IndexSet<String>,

    /// Strings that can be used as revert error in assertions
    pub assert_strings: IndexSet<FeString>,

    /// Structs that can be used as errors in revert statements
    pub revert_errors: IndexSet<Struct>,
}

pub struct FnContext<'a, 'b> {
    pub db: &'a dyn AnalyzerDb,
    pub contract: &'b mut ContractContext,
    fn_body: Rc<FunctionBody>,
}

impl<'a, 'b> FnContext<'a, 'b> {
    pub fn new(
        db: &'a dyn AnalyzerDb,
        contract: &'b mut ContractContext,
        fn_body: Rc<FunctionBody>,
    ) -> Self {
        Self {
            db,
            contract,
            fn_body,
        }
    }

    /// Get information that has been attributed to an expression node.
    pub fn expression_attributes(&self, expr: &Node<ast::Expr>) -> Option<&ExpressionAttributes> {
        self.fn_body.expressions.get(&expr.id)
    }

    /// Get the type of a variable declaration.
    pub fn declaration_type(&self, typ: &Node<ast::TypeDesc>) -> Option<&FixedSize> {
        self.fn_body.var_decl_types.get(&typ.id)
    }

    /// Get information that has been attributed to a call expression node.
    pub fn call_type(&self, expr: &Node<ast::Expr>) -> Option<CallType> {
        self.fn_body.calls.get(&expr.id).cloned()
    }

    /// Get information that has been attributed to an emit statement node.
    pub fn emitted_event(&self, emit_stmt: &Node<ast::FuncStmt>) -> Option<Rc<Event>> {
        self.fn_body
            .emits
            .get(&emit_stmt.id)
            .map(|event| event.typ(self.db))
    }
}
