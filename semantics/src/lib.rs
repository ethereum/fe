//! Fe semantic analysis.
//!
//! This library is used to analyze the semantics of a given Fe AST. It detects
//! any semantic errors within a given AST and produces a `Context` instance
//! that can be used to query contextual information attributed to AST nodes.

mod errors;
pub mod namespace;
mod traversal;

use crate::errors::SemanticError;
use crate::namespace::events::Event;
use crate::namespace::scopes::Shared;
use crate::namespace::types::Type;
use fe_parser::ast as fe;
use fe_parser::span::{
    Span,
    Spanned,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Indicates where an expression is stored.
#[derive(Clone, Debug, PartialEq)]
pub enum Location {
    Storage,
    Memory,
    Value,
}

/// Contains contextual information relating to an expression AST node.
#[derive(Clone, Debug, PartialEq)]
pub struct ExpressionAttributes {
    pub typ: Type,
    pub location: Location,
}

/// Contains contextual information about a Fe module and can be queried using
/// `Spanned` AST nodes.
#[derive(Clone, Debug, Default)]
pub struct Context {
    expressions: HashMap<Span, ExpressionAttributes>,
    emits: HashMap<Span, Event>,
}

impl Context {
    pub fn new() -> Shared<Self> {
        Rc::new(RefCell::new(Context {
            expressions: HashMap::new(),
            emits: HashMap::new(),
        }))
    }

    /// Attribute contextual information to an expression node.
    pub fn add_expression(
        &mut self,
        spanned: &Spanned<fe::Expr>,
        attributes: ExpressionAttributes,
    ) {
        self.expressions.insert(spanned.span, attributes);
    }

    /// Get information that has been attributed to an expression node.
    pub fn get_expression(&self, spanned: &Spanned<fe::Expr>) -> Option<&ExpressionAttributes> {
        self.expressions.get(&spanned.span)
    }

    /// Attribute contextual information to an emit statement node.
    pub fn add_emit(&mut self, spanned: &Spanned<fe::FuncStmt>, event: Event) {
        self.emits.insert(spanned.span, event);
    }

    /// Get information that has been attributed to an emit statement node.
    pub fn get_emit(&self, spanned: &Spanned<fe::FuncStmt>) -> Option<&Event> {
        self.emits.get(&spanned.span)
    }
}

/// Performs semantic analysis of the source program and returns a `Context`
/// instance.
pub fn analysis(module: &fe::Module) -> Result<Context, SemanticError> {
    let context = Context::new();
    traversal::module::module(Rc::clone(&context), module)?;
    Ok(Rc::try_unwrap(context)
        .map_err(|_| "more than one strong reference pointing to context")
        // This should never panic.
        .expect("failed to unwrap reference counter")
        .into_inner())
}
