use crate::errors::{self, CannotMove, IncompleteItem, TypeError};
use crate::namespace::items::{Class, ContractId, DiagnosticSink, EventId, FunctionId, Item};
use crate::namespace::types::{FixedSize, SelfDecl, Type};
use crate::AnalyzerDb;
use crate::{
    builtins::{ContractTypeMethod, GlobalFunction, Intrinsic, ValueMethod},
    namespace::scopes::BlockScopeType,
};
use fe_common::diagnostics::Diagnostic;
pub use fe_common::diagnostics::Label;
use fe_common::Span;
use fe_parser::ast;
use fe_parser::node::{Node, NodeId};

use indexmap::{IndexMap, IndexSet};
use num_bigint::BigInt;
use smol_str::SmolStr;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Analysis<T> {
    pub value: T,
    pub diagnostics: Rc<[Diagnostic]>,
}
impl<T> Analysis<T> {
    pub fn new(value: T, diagnostics: Rc<[Diagnostic]>) -> Self {
        Self { value, diagnostics }
    }
    pub fn sink_diagnostics(&self, sink: &mut impl DiagnosticSink) {
        self.diagnostics.iter().for_each(|diag| sink.push(diag))
    }
}

pub trait AnalyzerContext {
    fn resolve_name(&self, name: &str) -> Result<Option<NamedThing>, IncompleteItem>;
    fn resolve_path(&mut self, path: &ast::Path) -> Option<NamedThing>;
    fn add_diagnostic(&mut self, diag: Diagnostic);
    fn db(&self) -> &dyn AnalyzerDb;

    fn error(&mut self, message: &str, label_span: Span, label: &str) -> DiagnosticVoucher {
        self.register_diag(errors::error(message, label_span, label))
    }

    /// Attribute contextual information to an expression node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    fn add_expression(&self, node: &Node<ast::Expr>, attributes: ExpressionAttributes);

    /// Update the expression attributes.
    ///
    /// # Panics
    ///
    /// Panics if an entry does not already exist for the node id.
    fn update_expression(&self, node: &Node<ast::Expr>, attributes: ExpressionAttributes);

    /// Returns a type of an expression.
    ///
    /// # Panics
    ///
    /// Panics if type analysis is not performed for an `expr`.
    fn expr_typ(&self, expr: &Node<ast::Expr>) -> Type;

    /// Add evaluated constant value in a constant declaration to the context.
    fn add_constant(&self, name: &Node<ast::SmolStr>, expr: &Node<ast::Expr>, value: Constant);

    /// Returns constant value from variable name.
    fn constant_value_by_name(
        &self,
        name: &ast::SmolStr,
    ) -> Result<Option<Constant>, IncompleteItem>;

    /// Returns an item enclosing current context.
    ///
    /// # Example
    ///
    /// ```fe
    /// contract Foo:
    ///     fn foo():
    ///        if ...:
    ///            ...
    ///        else:
    ///            ...
    /// ```
    /// If the context is in `then` block, then this function returns
    /// `Item::Function(..)`.
    fn parent(&self) -> Item;

    /// Returns a function id that encloses a context.
    ///
    /// # Panics
    ///
    /// Panics if a context is not in a function. Use [`Self::is_in_function`]
    /// to determine whether a context is in a function.
    fn parent_function(&self) -> FunctionId;

    /// Returns a non-function item that encloses a context.
    ///
    /// # Example
    ///
    /// ```fe
    /// contract Foo:
    ///     fn foo():
    ///        if ...:
    ///            ...
    ///        else:
    ///            ...
    /// ```
    /// If the context is in `then` block, then this function returns
    /// `Item::Type(TypeDef::Contract(..))`.
    fn root_item(&self) -> Item {
        let mut item = self.parent();
        while let Item::Function(func_id) = item {
            item = func_id.parent(self.db());
        }
        item
    }

    /// # Panics
    ///
    /// Panics if a context is not in a function. Use [`Self::is_in_function`]
    /// to determine whether a context is in a function.
    fn add_call(&self, node: &Node<ast::Expr>, call_type: CallType);

    /// Store string literal to the current context.
    ///
    /// # Panics
    ///
    /// Panics if a context is not in a function. Use [`Self::is_in_function`]
    /// to determine whether a context is in a function.
    fn add_string(&self, str_lit: SmolStr);

    /// Returns `true` if the context is in function scope.
    fn is_in_function(&self) -> bool;

    /// Returns `true` if the scope or any of its parents is of the given type.
    fn inherits_type(&self, typ: BlockScopeType) -> bool;

    fn type_error(
        &mut self,
        message: &str,
        span: Span,
        expected: &dyn Display,
        actual: &dyn Display,
    ) -> DiagnosticVoucher {
        self.register_diag(errors::type_error(message, span, expected, actual))
    }

    fn not_yet_implemented(&mut self, feature: &str, span: Span) -> DiagnosticVoucher {
        self.register_diag(errors::not_yet_implemented(feature, span))
    }

    fn fancy_error(
        &mut self,
        message: &str,
        labels: Vec<Label>,
        notes: Vec<String>,
    ) -> DiagnosticVoucher {
        self.register_diag(errors::fancy_error(message, labels, notes))
    }

    fn duplicate_name_error(
        &mut self,
        message: &str,
        name: &str,
        original: Span,
        duplicate: Span,
    ) -> DiagnosticVoucher {
        self.register_diag(errors::duplicate_name_error(
            message, name, original, duplicate,
        ))
    }

    fn name_conflict_error(
        &mut self,
        name_kind: &str, // Eg "function parameter" or "variable name"
        name: &str,
        original: &NamedThing,
        original_span: Option<Span>,
        duplicate_span: Span,
    ) -> DiagnosticVoucher {
        self.register_diag(errors::name_conflict_error(
            name_kind,
            name,
            original,
            original_span,
            duplicate_span,
        ))
    }

    fn register_diag(&mut self, diag: Diagnostic) -> DiagnosticVoucher {
        self.add_diagnostic(diag);
        DiagnosticVoucher(PhantomData::default())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NamedThing {
    Item(Item),
    SelfValue {
        /// Function `self` parameter.
        decl: Option<SelfDecl>,

        /// The function's parent, if any. If `None`, `self` has been
        /// used in a module-level function.
        class: Option<Class>,
        span: Option<Span>,
    },
    // SelfType // when/if we add a `Self` type keyword
    Variable {
        name: String,
        typ: Result<FixedSize, TypeError>,
        is_const: bool,
        span: Span,
    },
}

impl NamedThing {
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        match self {
            NamedThing::Item(item) => item.name_span(db),
            NamedThing::SelfValue { span, .. } => *span,
            NamedThing::Variable { span, .. } => Some(*span),
        }
    }

    pub fn is_builtin(&self) -> bool {
        match self {
            NamedThing::Item(item) => item.is_builtin(),
            NamedThing::Variable { .. } | NamedThing::SelfValue { .. } => false,
        }
    }

    pub fn item_kind_display_name(&self) -> &str {
        match self {
            NamedThing::Item(item) => item.item_kind_display_name(),
            NamedThing::Variable { .. } => "variable",
            NamedThing::SelfValue { .. } => "value",
        }
    }
}

/// This should only be created by [`AnalyzerContext`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DiagnosticVoucher(PhantomData<()>);

impl DiagnosticVoucher {
    pub fn assume_the_parser_handled_it() -> Self {
        Self(PhantomData::default())
    }
}

#[derive(Default)]
pub struct TempContext {
    pub diagnostics: Vec<Diagnostic>,
}
impl AnalyzerContext for TempContext {
    fn db(&self) -> &dyn AnalyzerDb {
        panic!("TempContext has no analyzer db")
    }

    fn resolve_name(&self, _name: &str) -> Result<Option<NamedThing>, IncompleteItem> {
        panic!("TempContext can't resolve names")
    }

    fn add_expression(&self, _node: &Node<ast::Expr>, _attributes: ExpressionAttributes) {
        panic!("TempContext can't store expression")
    }

    fn update_expression(&self, _node: &Node<ast::Expr>, _attributes: ExpressionAttributes) {
        panic!("TempContext can't update expression");
    }

    fn expr_typ(&self, _expr: &Node<ast::Expr>) -> Type {
        panic!("TempContext can't return expression type")
    }

    fn add_constant(&self, _name: &Node<ast::SmolStr>, _expr: &Node<ast::Expr>, _value: Constant) {
        panic!("TempContext can't store constant")
    }

    fn constant_value_by_name(
        &self,
        _name: &ast::SmolStr,
    ) -> Result<Option<Constant>, IncompleteItem> {
        Ok(None)
    }

    fn parent(&self) -> Item {
        panic!("TempContext has no root item")
    }

    fn parent_function(&self) -> FunctionId {
        panic!("TempContext has no parent function")
    }

    fn add_call(&self, _node: &Node<ast::Expr>, _call_type: CallType) {
        panic!("TempContext can't add call");
    }

    fn add_string(&self, _str_lit: SmolStr) {
        panic!("TempContext can't store string literal")
    }

    fn is_in_function(&self) -> bool {
        false
    }

    fn inherits_type(&self, _typ: BlockScopeType) -> bool {
        false
    }

    fn add_diagnostic(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag)
    }

    fn resolve_path(&mut self, _path: &ast::Path) -> Option<NamedThing> {
        panic!("TempContext can't resolve paths")
    }
}

/// Indicates where an expression is stored.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Location {
    /// A storage value may not have a nonce known at compile time, so it is
    /// optional.
    Storage {
        nonce: Option<usize>,
    },
    Memory,
    Value,
}

impl Location {
    /// The expected location of a value with the given type when being
    /// assigned, returned, or passed.
    pub fn assign_location(typ: &FixedSize) -> Self {
        match typ {
            FixedSize::Base(_) | FixedSize::Contract(_) => Location::Value,
            FixedSize::Array(_)
            | FixedSize::Tuple(_)
            | FixedSize::String(_)
            | FixedSize::Struct(_) => Location::Memory,
        }
    }
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct FunctionBody {
    pub expressions: IndexMap<NodeId, ExpressionAttributes>,
    pub emits: IndexMap<NodeId, EventId>,
    pub string_literals: IndexSet<SmolStr>, // for yulgen

    // This is the id of the VarDecl TypeDesc node
    pub var_decl_types: IndexMap<NodeId, FixedSize>,
    pub calls: IndexMap<NodeId, CallType>,
    pub spans: HashMap<NodeId, Span>,
}

/// Contains contextual information relating to an expression AST node.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExpressionAttributes {
    pub typ: Type,
    pub location: Location,
    pub move_location: Option<Location>,
    // Evaluated constant value of const local definition.
    pub const_value: Option<Constant>,
}

impl ExpressionAttributes {
    pub fn new(typ: Type, location: Location) -> Self {
        Self {
            typ,
            location,
            move_location: None,
            const_value: None,
        }
    }

    /// Adds a move to memory.
    pub fn into_cloned(mut self) -> Self {
        self.move_location = Some(Location::Memory);
        self
    }

    /// Adds a move to value, if it is in storage or memory.
    pub fn into_loaded(mut self) -> Result<Self, CannotMove> {
        match self.typ {
            Type::Base(_) | Type::Contract(_) => {
                if self.location != Location::Value {
                    self.move_location = Some(Location::Value);
                }

                Ok(self)
            }
            _ => Err(CannotMove),
        }
    }

    /// The final location of an expression after a possible move.
    pub fn final_location(&self) -> Location {
        if let Some(location) = self.move_location {
            return location;
        }
        self.location
    }
}

impl fmt::Display for ExpressionAttributes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        if let Some(move_to) = self.move_location {
            write!(f, "{}: {:?} => {:?}", self.typ, self.location, move_to)
        } else {
            write!(f, "{}: {:?}", self.typ, self.location)
        }
    }
}

/// The type of a function call.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CallType {
    BuiltinFunction(GlobalFunction),
    Intrinsic(Intrinsic),
    BuiltinValueMethod {
        method: ValueMethod,
        typ: Type,
    },

    // create, create2 (will be methods of the context struct soon)
    BuiltinAssociatedFunction {
        contract: ContractId,
        function: ContractTypeMethod,
    },

    // MyStruct.foo() (soon MyStruct::foo())
    AssociatedFunction {
        class: Class,
        function: FunctionId,
    },
    ValueMethod {
        is_self: bool,
        class: Class,
        method: FunctionId,
    },
    External {
        contract: ContractId,
        function: FunctionId,
    },
    Pure(FunctionId),
    TypeConstructor(Type),
}

impl CallType {
    pub fn function(&self) -> Option<FunctionId> {
        use CallType::*;
        match self {
            BuiltinFunction(_)
            | BuiltinValueMethod { .. }
            | TypeConstructor(_)
            | Intrinsic(_)
            | BuiltinAssociatedFunction { .. } => None,
            AssociatedFunction { function: id, .. }
            | ValueMethod { method: id, .. }
            | External { function: id, .. }
            | Pure(id) => Some(*id),
        }
    }

    pub fn function_name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        match self {
            CallType::BuiltinFunction(f) => f.as_ref().into(),
            CallType::Intrinsic(f) => f.as_ref().into(),
            CallType::BuiltinValueMethod { method, .. } => method.as_ref().into(),
            CallType::BuiltinAssociatedFunction { function, .. } => function.as_ref().into(),
            CallType::AssociatedFunction { function: id, .. }
            | CallType::ValueMethod { method: id, .. }
            | CallType::External { function: id, .. }
            | CallType::Pure(id) => id.name(db),
            CallType::TypeConstructor(typ) => typ.name(),
        }
    }

    pub fn is_unsafe(&self, db: &dyn AnalyzerDb) -> bool {
        if let CallType::Intrinsic(_) = self {
            true
        } else {
            self.function().map(|id| id.is_unsafe(db)).unwrap_or(false)
        }
    }
}

impl fmt::Display for CallType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

/// Represents constant value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constant {
    Int(BigInt),
    Bool(bool),
    Str(SmolStr),
}
