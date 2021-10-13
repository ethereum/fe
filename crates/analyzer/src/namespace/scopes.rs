#![allow(unstable_name_collisions)] // expect_none, which ain't gonna be stabilized

use crate::context::{AnalyzerContext, CallType, ExpressionAttributes, FunctionBody, NamedThing};
use crate::errors::{AlreadyDefined, TypeError};
use crate::namespace::items::{EventId, FunctionId, ModuleId};
use crate::namespace::types::FixedSize;
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
    fn db(&self) -> &dyn AnalyzerDb {
        self.db
    }
    fn resolve_name(&self, name: &str) -> Option<NamedThing> {
        self.module
            .resolve_name(self.db, name)
            .map(NamedThing::Item)
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
    fn db(&self) -> &dyn AnalyzerDb {
        self.db
    }

    fn add_diagnostic(&mut self, diag: Diagnostic) {
        self.diagnostics.borrow_mut().push(diag)
    }

    fn resolve_name(&self, name: &str) -> Option<NamedThing> {
        // Getting param names and spans should be simpler
        self.function
            .signature(self.db)
            .params
            .iter()
            .find_map(|param| {
                (param.name == name).then(|| {
                    let span = self
                        .function
                        .data(self.db)
                        .ast
                        .kind
                        .args
                        .iter()
                        .find_map(|param| (param.name() == name).then(|| param.name_span()))
                        .expect("found param type but not span");

                    NamedThing::Variable {
                        name: name.to_string(),
                        typ: param.typ.clone(),
                        span,
                    }
                })
            })
            .or_else(|| {
                if let Some(contract) = self.function.contract(self.db) {
                    contract.resolve_name(self.db, name)
                } else {
                    self.function.module(self.db).resolve_name(self.db, name)
                }
                .map(NamedThing::Item)
            })
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
    Unsafe,
}

impl AnalyzerContext for BlockScope<'_, '_> {
    fn db(&self) -> &dyn AnalyzerDb {
        self.root.db
    }
    fn resolve_name(&self, name: &str) -> Option<NamedThing> {
        self.variable_defs
            .get(name)
            .map(|(typ, span)| NamedThing::Variable {
                name: name.to_string(),
                typ: Ok(typ.clone()),
                span: *span,
            })
            .or_else(|| {
                if let Some(parent) = self.parent {
                    parent.resolve_name(name)
                } else {
                    self.root.resolve_name(name)
                }
            })
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

    pub fn contract_name(&self) -> Option<String> {
        Some(
            self.root
                .function
                .contract(self.root.db)?
                .name(self.root.db),
        )
    }

    /// Add a variable to the block scope.
    pub fn add_var(
        &mut self,
        name: &str,
        typ: FixedSize,
        span: Span,
    ) -> Result<(), AlreadyDefined> {
        if let Some(named_item) = self.resolve_name(name) {
            if named_item.is_builtin() {
                self.error(
                    &format!(
                        "variable name conflicts with built-in {}",
                        named_item.item_kind_display_name(),
                    ),
                    span,
                    &format!(
                        "`{}` is a built-in {}",
                        name,
                        named_item.item_kind_display_name()
                    ),
                );
                return Err(AlreadyDefined);
            } else {
                // It's (currently) an error to shadow a variable in a nested scope
                self.duplicate_name_error(
                    &format!("duplicate definition of variable `{}`", name),
                    name,
                    named_item
                        .name_span(self.db())
                        .expect("missing name_span of non-builtin"),
                    span,
                );
            }
            Err(AlreadyDefined)
        } else {
            self.variable_defs.insert(name.to_string(), (typ, span));
            Ok(())
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
