#![allow(unstable_name_collisions)] // expect_none, which ain't gonna be stabilized

use crate::context::{AnalyzerContext, CallType, ExpressionAttributes, FunctionBody};
use crate::db::AnalyzerDb;
use crate::errors::AlreadyDefined2;
use crate::namespace::events::EventDef;
use crate::namespace::items::{EventId, FunctionId, ModuleId};
use crate::namespace::types::{self, Array, FixedSize, Tuple, Type};
use fe_common::diagnostics::Diagnostic;
use fe_common::Span;
use fe_parser::ast;
use fe_parser::node::{Node, NodeId};
use std::cell::RefCell;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::rc::Rc;

pub type Shared<T> = Rc<RefCell<T>>;

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
    fn resolve_type(&self, name: &str) -> Option<Rc<Type>> {
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

    pub fn add_diagnostic(&self, diag: Diagnostic) {
        self.diagnostics.borrow_mut().push(diag)
    }

    fn resolve_type(&self, name: &str) -> Option<Rc<Type>> {
        self.function.module(self.db).resolve_type(self.db, name)
    }

    pub fn var_type(&self, name: &str) -> Option<FixedSize> {
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
            .find_map(|param| (param.kind.name.kind == name).then(|| param.span))
    }

    pub fn contract_field(&self, name: &str) -> Option<(Rc<Type>, usize)> {
        self.function.contract(self.db).field(self.db, name)
    }

    pub fn contract_function(&self, name: &str) -> Option<Rc<types::ContractFunction>> {
        self.function
            .contract(self.db)
            .typ(self.db)
            .functions
            .get(name)
            .cloned()
    }

    pub fn resolve_event(&self, name: &str) -> Option<Rc<EventDef>> {
        Some(
            self.function
                .contract(self.db)
                .event(self.db, name)?
                .typ(self.db),
        )
    }

    pub fn function_return_type(&self) -> FixedSize {
        self.function.signature(self.db).return_type.clone()
    }
    /// Attribute contextual information to an expression node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_expression(&mut self, node: &Node<ast::Expr>, attributes: ExpressionAttributes) {
        // self.add_node(node);
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
    pub fn update_expression(&mut self, node: &Node<ast::Expr>, attributes: ExpressionAttributes) {
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
    pub fn add_emit(&mut self, node: &Node<ast::FuncStmt>, event: EventId) {
        // self.add_node(node);
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
    pub fn add_declaration(&mut self, node: &Node<ast::FuncStmt>, typ: FixedSize) {
        self.body
            .borrow_mut()
            .declarations
            .insert(node.id, typ)
            .expect_none("declaration attributes already exist");
    }
    /// Attribute contextual information to a call expression node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_call(&mut self, node: &Node<ast::Expr>, call_type: CallType) {
        // self.add_node(node);
        self.body
            .borrow_mut()
            .calls
            .insert(node.id, call_type)
            .expect_none("call attributes already exist");
    }
    /// Attribute contextual information to a type description node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_type_desc(&mut self, node: &Node<ast::TypeDesc>, typ: Type) {
        // self.add_node(node);
        self.body
            .borrow_mut()
            .type_descs
            .insert(node.id, typ)
            .expect_none("type desc attributes already exist");
    }
}

pub struct BlockScope<'a, 'b> {
    root: &'a FunctionScope<'b>,
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
    fn resolve_type(&self, name: &str) -> Option<Rc<Type>> {
        self.root.resolve_type(name)
    }
    fn add_diagnostic(&mut self, diag: Diagnostic) {
        self.root.add_diagnostic(diag)
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
        self.root.function.contract(self.root.db).name(self.root.db)
    }

    /// Lookup a field definition on the inherited contract scope
    pub fn contract_field(&self, name: &str) -> Option<(Rc<Type>, usize)> {
        self.root.contract_field(name)
    }

    /// Lookup a function definition on the inherited contract scope.
    pub fn contract_function(&self, name: &str) -> Option<Rc<types::ContractFunction>> {
        self.root.contract_function(name)
    }

    pub fn resolve_event(&self, name: &str) -> Option<Rc<EventDef>> {
        self.root.resolve_event(name)
    }

    pub fn function_return_type(&self) -> FixedSize {
        self.root.function_return_type()
    }

    /// Lookup a definition in current or inherited block scope
    pub fn var_type(&self, name: &str) -> Option<FixedSize> {
        self.variable_defs
            .get(name)
            .map(|(typ, _span)| typ.clone())
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
    ) -> Result<(), AlreadyDefined2<Span>> {
        // It's (currently) an error to shadow a variable in a nested scope
        match self.var_def_span(name) {
            Some(prev_span) => Err(AlreadyDefined2(prev_span)),
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

    // /// Filter module scope for type definitions that match the given predicate
    // pub fn get_module_type_defs<B, F: FnMut(&Type) -> Option<B>>(&self, predicate: F) -> Vec<B> {
    //     self.module_scope().borrow().get_type_defs(predicate)
    // }
}

// #[derive(Clone, Debug, PartialEq)]
// pub struct ContractScope {
//     pub name: String,
//     pub parent: Shared<ModuleScope>,
//     pub interface: Vec<String>,
//     pub event_defs: BTreeMap<String, EventDef>,
//     pub field_defs: BTreeMap<String, ContractFieldDef>,
//     pub function_defs: BTreeMap<String, ContractFunctionDef>,
//     pub list_expressions: BTreeSet<Array>,
//     pub string_defs: BTreeSet<String>,
//     pub created_contracts: BTreeSet<String>,
//     num_fields: usize,
// }

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
