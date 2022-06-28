#![allow(unstable_name_collisions)] // expect_none, which ain't gonna be stabilized

use crate::context::{
    AnalyzerContext, CallType, Constant, ExpressionAttributes, FunctionBody, NamedThing,
};
use crate::errors::{AlreadyDefined, IncompleteItem, TypeError};
use crate::namespace::items::{EventId, FunctionId, ModuleId};
use crate::namespace::items::{Item, TypeDef};
use crate::namespace::types::{Type, TypeId};
use crate::AnalyzerDb;
use fe_common::diagnostics::Diagnostic;
use fe_common::Span;
use fe_parser::{ast, node::NodeId, Label};
use fe_parser::{ast::Expr, node::Node};
use indexmap::IndexMap;
use std::cell::RefCell;
use std::collections::BTreeMap;

pub struct ItemScope<'a> {
    db: &'a dyn AnalyzerDb,
    module: ModuleId,
    expressions: RefCell<IndexMap<NodeId, ExpressionAttributes>>,
    pub diagnostics: RefCell<Vec<Diagnostic>>,
}
impl<'a> ItemScope<'a> {
    pub fn new(db: &'a dyn AnalyzerDb, module: ModuleId) -> Self {
        Self {
            db,
            module,
            expressions: RefCell::new(IndexMap::default()),
            diagnostics: RefCell::new(vec![]),
        }
    }
}

impl<'a> AnalyzerContext for ItemScope<'a> {
    fn db(&self) -> &dyn AnalyzerDb {
        self.db
    }

    fn add_expression(&self, node: &Node<ast::Expr>, attributes: ExpressionAttributes) {
        self.expressions
            .borrow_mut()
            .insert(node.id, attributes)
            .expect_none("expression attributes already exist");
    }

    fn update_expression(&self, node: &Node<ast::Expr>, attributes: ExpressionAttributes) {
        self.expressions
            .borrow_mut()
            .insert(node.id, attributes)
            .expect("expression attributes do not exist");
    }

    fn expr_typ(&self, expr: &Node<Expr>) -> Type {
        self.expressions
            .borrow()
            .get(&expr.id)
            .unwrap()
            .typ
            .typ(self.db())
    }

    fn add_constant(
        &self,
        _name: &Node<ast::SmolStr>,
        _expr: &Node<ast::Expr>,
        _value: crate::context::Constant,
    ) {
        // We use salsa query to get constant. So no need to add constant
        // explicitly.
    }

    fn constant_value_by_name(
        &self,
        name: &ast::SmolStr,
        _span: Span,
    ) -> Result<Option<Constant>, IncompleteItem> {
        if let Some(constant) = self.module.resolve_constant(self.db, name)? {
            // It's ok to ignore an error.
            // Diagnostics are already emitted when an error occurs.
            Ok(constant.constant_value(self.db).ok())
        } else {
            Ok(None)
        }
    }

    fn parent(&self) -> Item {
        Item::Module(self.module)
    }

    fn module(&self) -> ModuleId {
        self.module
    }

    fn parent_function(&self) -> FunctionId {
        panic!("ItemContext has no parent function")
    }

    fn add_call(&self, _node: &Node<ast::Expr>, _call_type: CallType) {
        unreachable!("Can't call function outside of function")
    }

    fn is_in_function(&self) -> bool {
        false
    }

    fn inherits_type(&self, _typ: BlockScopeType) -> bool {
        false
    }

    fn resolve_name(&self, name: &str, span: Span) -> Result<Option<NamedThing>, IncompleteItem> {
        let item = self.module.resolve_name(self.db, name)?;

        if let Some(item) = item {
            check_item_visibility(self, item, span);
        }

        Ok(item.map(NamedThing::Item))
    }

    fn resolve_path(&self, path: &ast::Path, span: Span) -> Option<NamedThing> {
        let item = self.module.resolve_path_internal(self.db(), path);

        for diagnostic in item.diagnostics.iter() {
            self.add_diagnostic(diagnostic.clone())
        }

        if let Some(item) = item.value {
            check_item_visibility(self, item, span);
        }

        item.value.map(NamedThing::Item)
    }

    fn add_diagnostic(&self, diag: Diagnostic) {
        self.diagnostics.borrow_mut().push(diag)
    }

    /// Gets `std::context::Context` if it exists
    fn get_context_type(&self) -> Option<TypeId> {
        if let Ok(Some(NamedThing::Item(Item::Type(TypeDef::Struct(id))))) =
            // `Context` is guaranteed to be public, so we can use `Span::dummy()` .
            self.resolve_name("Context", Span::dummy())
        {
            // we just assume that there is only one `Context` defined in `std`
            if id.module(self.db()).ingot(self.db()).name(self.db()) == "std" {
                return Some(self.db().intern_type(Type::Struct(id)));
            }
        }

        None
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

    pub fn function_return_type(&self) -> Result<TypeId, TypeError> {
        self.function.signature(self.db).return_type.clone()
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

    pub fn map_variable_type<T>(&self, node: &Node<T>, typ: TypeId) {
        self.add_node(node);
        self.body
            .borrow_mut()
            .var_types
            .insert(node.id, typ)
            .expect_none("variable has already registered")
    }

    fn add_node<T>(&self, node: &Node<T>) {
        self.body.borrow_mut().spans.insert(node.id, node.span);
    }
}

impl<'a> AnalyzerContext for FunctionScope<'a> {
    fn db(&self) -> &dyn AnalyzerDb {
        self.db
    }

    fn add_diagnostic(&self, diag: Diagnostic) {
        self.diagnostics.borrow_mut().push(diag)
    }

    fn add_expression(&self, node: &Node<ast::Expr>, attributes: ExpressionAttributes) {
        self.add_node(node);
        self.body
            .borrow_mut()
            .expressions
            .insert(node.id, attributes)
            .expect_none("expression attributes already exist");
    }

    fn update_expression(&self, node: &Node<ast::Expr>, attributes: ExpressionAttributes) {
        self.body
            .borrow_mut()
            .expressions
            .insert(node.id, attributes)
            .expect("expression attributes do not exist");
    }

    fn expr_typ(&self, expr: &Node<Expr>) -> Type {
        self.body
            .borrow()
            .expressions
            .get(&expr.id)
            .unwrap()
            .typ
            .typ(self.db())
    }

    fn add_constant(&self, _name: &Node<ast::SmolStr>, expr: &Node<ast::Expr>, value: Constant) {
        self.body
            .borrow_mut()
            .expressions
            .get_mut(&expr.id)
            .expect("expression attributes must exist before adding constant value")
            .const_value = Some(value);
    }

    fn constant_value_by_name(
        &self,
        _name: &ast::SmolStr,
        _span: Span,
    ) -> Result<Option<Constant>, IncompleteItem> {
        Ok(None)
    }

    fn parent(&self) -> Item {
        self.function.parent(self.db())
    }

    fn module(&self) -> ModuleId {
        self.function.module(self.db())
    }

    fn parent_function(&self) -> FunctionId {
        self.function
    }

    fn add_call(&self, node: &Node<ast::Expr>, call_type: CallType) {
        // TODO: should probably take the Expr::Call node, rather than the function node

        self.add_node(node);
        self.body
            .borrow_mut()
            .calls
            .insert(node.id, call_type)
            .expect_none("call attributes already exist");
    }

    fn is_in_function(&self) -> bool {
        true
    }

    fn inherits_type(&self, _typ: BlockScopeType) -> bool {
        false
    }

    fn resolve_name(&self, name: &str, span: Span) -> Result<Option<NamedThing>, IncompleteItem> {
        let sig = self.function.signature(self.db);

        if name == "self" {
            return Ok(Some(NamedThing::SelfValue {
                decl: sig.self_decl,
                parent: self.function.sig(self.db).self_item(self.db),
                span: self.function.self_span(self.db),
            }));
        }

        // Getting param names and spans should be simpler
        let param = sig.params.iter().find_map(|param| {
            (param.name == name).then(|| {
                let span = self
                    .function
                    .data(self.db)
                    .ast
                    .kind
                    .sig
                    .kind
                    .args
                    .iter()
                    .find_map(|param| (param.name() == name).then(|| param.name_span()))
                    .expect("found param type but not span");

                NamedThing::Variable {
                    name: name.to_string(),
                    typ: param.typ.clone(),
                    is_const: false,
                    span,
                }
            })
        });

        if let Some(param) = param {
            Ok(Some(param))
        } else {
            let item =
                if let Item::Type(TypeDef::Contract(contract)) = self.function.parent(self.db) {
                    contract.resolve_name(self.db, name)
                } else {
                    self.function.module(self.db).resolve_name(self.db, name)
                }?;

            if let Some(item) = item {
                check_item_visibility(self, item, span);
            }

            Ok(item.map(NamedThing::Item))
        }
    }

    fn resolve_path(&self, path: &ast::Path, span: Span) -> Option<NamedThing> {
        let item = self
            .function
            .module(self.db())
            .resolve_path_internal(self.db(), path);

        for diagnostic in item.diagnostics.iter() {
            self.add_diagnostic(diagnostic.clone())
        }

        if let Some(item) = item.value {
            check_item_visibility(self, item, span);
        }

        item.value.map(NamedThing::Item)
    }

    fn get_context_type(&self) -> Option<TypeId> {
        if let Ok(Some(NamedThing::Item(Item::Type(TypeDef::Struct(id))))) =
            self.resolve_name("Context", Span::dummy())
        {
            if id.module(self.db()).ingot(self.db()).name(self.db()) == "std" {
                return Some(self.db().intern_type(Type::Struct(id)));
            }
        }

        None
    }
}

pub struct BlockScope<'a, 'b> {
    pub root: &'a FunctionScope<'b>,
    pub parent: Option<&'a BlockScope<'a, 'b>>,
    /// Maps Name -> (Type, is_const, span)
    pub variable_defs: BTreeMap<String, (TypeId, bool, Span)>,
    pub constant_defs: RefCell<BTreeMap<String, Constant>>,
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

    fn resolve_name(&self, name: &str, span: Span) -> Result<Option<NamedThing>, IncompleteItem> {
        if let Some(var) =
            self.variable_defs
                .get(name)
                .map(|(typ, is_const, span)| NamedThing::Variable {
                    name: name.to_string(),
                    typ: Ok(*typ),
                    is_const: *is_const,
                    span: *span,
                })
        {
            Ok(Some(var))
        } else if let Some(parent) = self.parent {
            parent.resolve_name(name, span)
        } else {
            self.root.resolve_name(name, span)
        }
    }

    fn add_expression(&self, node: &Node<ast::Expr>, attributes: ExpressionAttributes) {
        self.root.add_expression(node, attributes)
    }

    fn update_expression(&self, node: &Node<ast::Expr>, attributes: ExpressionAttributes) {
        self.root.update_expression(node, attributes)
    }

    fn expr_typ(&self, expr: &Node<Expr>) -> Type {
        self.root.expr_typ(expr)
    }

    fn add_constant(&self, name: &Node<ast::SmolStr>, expr: &Node<ast::Expr>, value: Constant) {
        self.constant_defs
            .borrow_mut()
            .insert(name.kind.clone().to_string(), value.clone())
            .expect_none("expression attributes already exist");

        self.root.add_constant(name, expr, value)
    }

    fn constant_value_by_name(
        &self,
        name: &ast::SmolStr,
        span: Span,
    ) -> Result<Option<Constant>, IncompleteItem> {
        if let Some(constant) = self.constant_defs.borrow().get(name.as_str()) {
            Ok(Some(constant.clone()))
        } else if let Some(parent) = self.parent {
            parent.constant_value_by_name(name, span)
        } else {
            match self.resolve_name(name, span)? {
                Some(NamedThing::Item(Item::Constant(constant))) => {
                    Ok(constant.constant_value(self.db()).ok())
                }
                _ => Ok(None),
            }
        }
    }

    fn parent(&self) -> Item {
        Item::Function(self.root.function)
    }

    fn module(&self) -> ModuleId {
        self.root.module()
    }

    fn parent_function(&self) -> FunctionId {
        self.root.function
    }

    fn add_call(&self, node: &Node<ast::Expr>, call_type: CallType) {
        self.root.add_call(node, call_type)
    }

    fn is_in_function(&self) -> bool {
        true
    }

    fn inherits_type(&self, typ: BlockScopeType) -> bool {
        self.typ == typ || self.parent.map_or(false, |scope| scope.inherits_type(typ))
    }

    fn resolve_path(&self, path: &ast::Path, span: Span) -> Option<NamedThing> {
        self.root.resolve_path(path, span)
    }

    fn add_diagnostic(&self, diag: Diagnostic) {
        self.root.diagnostics.borrow_mut().push(diag)
    }

    fn get_context_type(&self) -> Option<TypeId> {
        self.root.get_context_type()
    }
}

impl<'a, 'b> BlockScope<'a, 'b> {
    pub fn new(root: &'a FunctionScope<'b>, typ: BlockScopeType) -> Self {
        BlockScope {
            root,
            parent: None,
            variable_defs: BTreeMap::new(),
            constant_defs: RefCell::new(BTreeMap::new()),
            typ,
        }
    }

    pub fn new_child(&'a self, typ: BlockScopeType) -> Self {
        BlockScope {
            root: self.root,
            parent: Some(self),
            variable_defs: BTreeMap::new(),
            constant_defs: RefCell::new(BTreeMap::new()),
            typ,
        }
    }

    /// Add a variable to the block scope.
    pub fn add_var(
        &mut self,
        name: &str,
        typ: TypeId,
        is_const: bool,
        span: Span,
    ) -> Result<(), AlreadyDefined> {
        match self.resolve_name(name, span) {
            Ok(Some(NamedThing::SelfValue { .. })) => {
                self.error(
                    "`self` can't be used as a variable name",
                    span,
                    "expected a name, found keyword `self`",
                );
                Err(AlreadyDefined)
            }

            Ok(Some(named_item)) => {
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
            }
            _ => {
                self.variable_defs
                    .insert(name.to_string(), (typ, is_const, span));
                Ok(())
            }
        }
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

/// Check an item visibility and sink diagnostics if an item is invisible from
/// the scope.
fn check_item_visibility(context: &dyn AnalyzerContext, item: Item, span: Span) {
    let item_module = item
        .module(context.db())
        .unwrap_or_else(|| context.module());
    if !item.is_public(context.db()) && item_module != context.module() {
        let module_name = item_module.name(context.db());
        let item_name = item.name(context.db());
        let item_span = item.name_span(context.db()).unwrap_or(span);
        let item_kind_name = item.item_kind_display_name();
        context.fancy_error(
            &format!("the {} `{}` is private", item_kind_name, item_name,),
            vec![
                Label::primary(span, format!("this {} is not `pub`", item_kind_name)),
                Label::secondary(item_span, format!("`{}` is defined here", item_name)),
            ],
            vec![
                format!("`{}` can only be used within `{}`", item_name, module_name),
                format!(
                    "Hint: use `pub` to make `{}` visible from outside of `{}`",
                    item_name, module_name,
                ),
            ],
        );
    }
}
