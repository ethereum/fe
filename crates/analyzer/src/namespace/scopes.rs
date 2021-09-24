#![allow(unstable_name_collisions)] // expect_none, which ain't gonna be stabilized

use crate::builtins;
use crate::context::{AnalyzerContext, CallType, ExpressionAttributes, FunctionBody};
use crate::errors::{AlreadyDefined, TypeError};
use crate::namespace::items::{EventId, FunctionId, ModuleId};
use crate::namespace::types::{FixedSize, Type};
use crate::AnalyzerDb;
use fe_common::diagnostics::Diagnostic;
use fe_common::Span;
use fe_parser::ast;
use fe_parser::node::Node;
use std::cell::RefCell;
use std::collections::BTreeMap;

pub struct ItemScope<'a> {
    db: &'a dyn AnalyzerDb,
    module: ModuleId,
    pub diagnostics: Vec<Diagnostic>,
}
impl<'a> ItemScope<'a> {
    pub fn new(db: &'a dyn AnalyzerDb, module: ModuleId) -> Self {
        Self {
            db,
            module,
            diagnostics: vec![],
        }
    }
}

impl<'a> AnalyzerContext for ItemScope<'a> {
    fn resolve_type(&self, name: &str) -> Option<Result<Type, TypeError>> {
        self.module.resolve_type(self.db, name)
    }
    fn add_diagnostic(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag)
    }
}

pub struct FunctionScope<'a> {
    pub db: &'a dyn AnalyzerDb,
    pub function: FunctionId,
    pub body: RefCell<FunctionBody>,
    pub diagnostics: RefCell<Vec<Diagnostic>>,
}

impl<'a> FunctionScope<'a> {
    pub fn new(db: &'a dyn AnalyzerDb, function: FunctionId) -> Self {
        Self {
            db,
            function,
            body: RefCell::new(FunctionBody::default()),
            diagnostics: RefCell::new(vec![]),
        }
    }

    pub fn var_type(&self, name: &str) -> Option<Result<FixedSize, TypeError>> {
        self.function
            .signature(self.db)
            .params
            .iter()
            .find_map(|param| (param.name == name).then(|| param.typ.clone()))
    }

    pub fn var_def_span(&self, name: &str) -> Option<Span> {
        self.function
            .data(self.db)
            .ast
            .kind
            .args
            .iter()
            .find_map(|param| (param.name() == name).then(|| param.span))
    }

    pub fn contract_field(&self, name: &str) -> Option<(Result<Type, TypeError>, usize)> {
        self.function.parent(self.db).field_type(self.db, name)
    }

    pub fn contract_function(&self, name: &str) -> Option<FunctionId> {
        self.function.parent(self.db).function(self.db, name)
    }

    pub fn self_contract_function(&self, name: &str) -> Option<FunctionId> {
        self.function.parent(self.db).self_function(self.db, name)
    }

    pub fn pure_contract_function(&self, name: &str) -> Option<FunctionId> {
        self.function.parent(self.db).pure_function(self.db, name)
    }

    pub fn resolve_event(&self, name: &str) -> Option<EventId> {
        self.function.parent(self.db).event(self.db, name)
    }

    pub fn function_return_type(&self) -> Result<FixedSize, TypeError> {
        self.function.signature(self.db).return_type.clone()
    }
    /// Attribute contextual information to an expression node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_expression(&self, node: &Node<ast::Expr>, attributes: ExpressionAttributes) {
        self.add_node(node);
        self.body
            .borrow_mut()
            .expressions
            .insert(node.id, attributes)
            .expect_none("expression attributes already exist");
    }

    /// Update the expression attributes.
    ///
    /// # Panics
    ///
    /// Panics if an entry does not already exist for the node id.
    pub fn update_expression(&self, node: &Node<ast::Expr>, attributes: ExpressionAttributes) {
        self.body
            .borrow_mut()
            .expressions
            .insert(node.id, attributes)
            .expect("expression attributes do not exist");
    }
    /// Attribute contextual information to an emit statement node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_emit(&self, node: &Node<ast::FuncStmt>, event: EventId) {
        self.add_node(node);
        self.body
            .borrow_mut()
            .emits
            .insert(node.id, event)
            .expect_none("emit statement attributes already exist");
    }
    /// Attribute contextual information to a declaration node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_declaration(&self, node: &Node<ast::TypeDesc>, typ: FixedSize) {
        self.add_node(node);
        self.body
            .borrow_mut()
            .var_decl_types
            .insert(node.id, typ)
            .expect_none("declaration attributes already exist");
    }
    /// Attribute contextual information to a call expression node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_call(&self, node: &Node<ast::Expr>, call_type: CallType) {
        self.add_node(node);
        self.body
            .borrow_mut()
            .calls
            .insert(node.id, call_type)
            .expect_none("call attributes already exist");
    }

    fn add_node<T>(&self, node: &Node<T>) {
        self.body.borrow_mut().spans.insert(node.id, node.span);
    }
}

impl<'a> AnalyzerContext for FunctionScope<'a> {
    fn add_diagnostic(&mut self, diag: Diagnostic) {
        self.diagnostics.borrow_mut().push(diag)
    }

    fn resolve_type(&self, name: &str) -> Option<Result<Type, TypeError>> {
        self.function.module(self.db).resolve_type(self.db, name)
    }
}

pub struct BlockScope<'a, 'b> {
    pub root: &'a FunctionScope<'b>,
    pub parent: Option<&'a BlockScope<'a, 'b>>,
    pub variable_defs: BTreeMap<String, (FixedSize, Span)>,
    pub typ: BlockScopeType,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockScopeType {
    Function,
    IfElse,
    Loop,
}

impl AnalyzerContext for BlockScope<'_, '_> {
    fn resolve_type(&self, name: &str) -> Option<Result<Type, TypeError>> {
        self.root.resolve_type(name)
    }
    fn add_diagnostic(&mut self, diag: Diagnostic) {
        self.root.diagnostics.borrow_mut().push(diag)
    }
}

impl<'a, 'b> BlockScope<'a, 'b> {
    pub fn new(root: &'a FunctionScope<'b>, typ: BlockScopeType) -> Self {
        BlockScope {
            root,
            parent: None,
            variable_defs: BTreeMap::new(),
            typ,
        }
    }

    pub fn new_child(&'a self, typ: BlockScopeType) -> Self {
        BlockScope {
            root: self.root,
            parent: Some(self),
            variable_defs: BTreeMap::new(),
            typ,
        }
    }

    pub fn contract_name(&self) -> String {
        self.root.function.parent(self.root.db).name(self.root.db)
    }

    pub fn db(&self) -> &dyn AnalyzerDb {
        self.root.db
    }

    /// Lookup a definition in current or inherited block scope
    pub fn var_type(&self, name: &str) -> Option<Result<FixedSize, TypeError>> {
        self.variable_defs
            .get(name)
            .map(|(typ, _span)| Ok(typ.clone()))
            .or_else(|| self.parent?.var_type(name))
            .or_else(|| self.root.var_type(name))
    }

    pub fn var_def_span(&self, name: &str) -> Option<Span> {
        self.variable_defs
            .get(name)
            .map(|(_typ, span)| *span)
            .or_else(|| self.parent?.var_def_span(name))
            .or_else(|| self.root.var_def_span(name))
    }

    /// Add a variable to the block scope.
    pub fn add_var(
        &mut self,
        name: &str,
        typ: FixedSize,
        span: Span,
    ) -> Result<(), AlreadyDefined> {
        if let Some(reserved) = builtins::reserved_name(name) {
            self.error(
                &format!(
                    "variable name conflicts with built-in {}",
                    reserved.as_ref()
                ),
                span,
                &format!("`{}` is a built-in {}", name, reserved.as_ref()),
            );
            return Err(AlreadyDefined);
        }

        // It's (currently) an error to shadow a variable in a nested scope
        match self.var_def_span(name) {
            Some(prev_span) => {
                self.duplicate_name_error(
                    &format!("duplicate definition of variable `{}`", name),
                    name,
                    prev_span,
                    span,
                );
                Err(AlreadyDefined)
            }
            None => {
                self.variable_defs.insert(name.to_string(), (typ, span));
                Ok(())
            }
        }
    }

    /// Return true if the scope or any of its parents is of the given type
    pub fn inherits_type(&self, typ: BlockScopeType) -> bool {
        self.typ == typ || self.parent.map_or(false, |scope| scope.inherits_type(typ))
    }
}

/// temporary helper until `BTreeMap::try_insert` is stabilized
trait OptionExt {
    fn expect_none(self, msg: &str);
}

impl<T> OptionExt for Option<T> {
    fn expect_none(self, msg: &str) {
        if self.is_some() {
            panic!("{}", msg)
        }
    }
}
