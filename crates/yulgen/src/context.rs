use crate::{AnalyzerDb, YulgenDb};
use fe_analyzer::context::{CallType, ExpressionAttributes, FunctionBody};
use fe_analyzer::namespace::types::{Event, Type};
use fe_parser::ast;
use fe_parser::node::Node;
use std::rc::Rc;

pub struct FnContext<'a> {
    pub adb: &'a dyn AnalyzerDb,
    pub db: &'a dyn YulgenDb,
    fn_body: Rc<FunctionBody>,
}

impl<'a> FnContext<'a> {
    pub fn new(db: &'a dyn YulgenDb, fn_body: Rc<FunctionBody>) -> Self {
        Self {
            adb: db.upcast(),
            db,
            fn_body,
        }
    }

    /// Get information that has been attributed to an expression node.
    pub fn expression_attributes(&self, expr: &Node<ast::Expr>) -> &ExpressionAttributes {
        self.fn_body
            .expressions
            .get(&expr.id)
            .expect("missing expression attributes")
    }

    /// Get the type of a variable declaration.
    pub fn declaration_type(&self, typ: &Node<ast::TypeDesc>) -> &Type {
        self.fn_body
            .var_decl_types
            .get(&typ.id)
            .expect("missing declaration type")
    }

    /// Get information that has been attributed to a call expression node.
    pub fn call_type(&self, expr: &Node<ast::Expr>) -> CallType {
        self.fn_body
            .calls
            .get(&expr.id)
            .cloned()
            .expect("missing call type")
    }

    /// Get information that has been attributed to an emit statement node.
    pub fn emitted_event(&self, emit_stmt: &Node<ast::FuncStmt>) -> Rc<Event> {
        self.fn_body
            .emits
            .get(&emit_stmt.id)
            .expect("missing emit event type")
            .typ(self.adb)
    }
}
