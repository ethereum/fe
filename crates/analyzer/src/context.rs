use crate::{
    display::Displayable,
    errors::FatalError,
    namespace::items::{EnumVariantId, TypeDef},
    pattern_analysis::PatternMatrix,
};

use crate::namespace::items::{
    ContractId, DiagnosticSink, FunctionId, FunctionSigId, Item, TraitId,
};
use crate::namespace::types::{Generic, SelfDecl, Type, TypeId};
use crate::AnalyzerDb;
use crate::{
    builtins::{ContractTypeMethod, GlobalFunction, Intrinsic, ValueMethod},
    namespace::scopes::BlockScopeType,
};
use crate::{
    errors::{self, IncompleteItem, TypeError},
    namespace::items::ModuleId,
};
use fe_common::diagnostics::Diagnostic;
pub use fe_common::diagnostics::Label;
use fe_common::Span;
use fe_parser::ast;
use fe_parser::node::{Node, NodeId};

use indexmap::IndexMap;
use num_bigint::BigInt;
use smol_str::SmolStr;
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::Rc;
use std::{cell::RefCell, collections::HashMap};

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
    pub fn has_diag(&self) -> bool {
        !self.diagnostics.is_empty()
    }
}

pub trait AnalyzerContext {
    fn resolve_name(&self, name: &str, span: Span) -> Result<Option<NamedThing>, IncompleteItem>;
    /// Resolves the given path and registers all errors
    fn resolve_path(&self, path: &ast::Path, span: Span) -> Result<NamedThing, FatalError>;
    /// Resolves the given path only if it is visible. Does not register any errors
    fn resolve_visible_path(&self, path: &ast::Path) -> Option<NamedThing>;
    /// Resolves the given path. Does not register any errors
    fn resolve_any_path(&self, path: &ast::Path) -> Option<NamedThing>;

    fn add_diagnostic(&self, diag: Diagnostic);
    fn db(&self) -> &dyn AnalyzerDb;

    fn error(&self, message: &str, label_span: Span, label: &str) -> DiagnosticVoucher {
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
    fn update_expression(&self, node: &Node<ast::Expr>, f: &dyn Fn(&mut ExpressionAttributes));

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
        span: Span,
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

    /// Returns the module enclosing current context.
    fn module(&self) -> ModuleId;

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
    fn get_call(&self, node: &Node<ast::Expr>) -> Option<CallType>;

    /// Returns `true` if the context is in function scope.
    fn is_in_function(&self) -> bool;

    /// Returns `true` if the scope or any of its parents is of the given type.
    fn inherits_type(&self, typ: BlockScopeType) -> bool;

    /// Returns the `Context` type, if it is defined.
    fn get_context_type(&self) -> Option<TypeId>;

    fn type_error(
        &self,
        message: &str,
        span: Span,
        expected: TypeId,
        actual: TypeId,
    ) -> DiagnosticVoucher {
        self.register_diag(errors::type_error(
            message,
            span,
            expected.display(self.db()),
            actual.display(self.db()),
        ))
    }

    fn not_yet_implemented(&self, feature: &str, span: Span) -> DiagnosticVoucher {
        self.register_diag(errors::not_yet_implemented(feature, span))
    }

    fn fancy_error(
        &self,
        message: &str,
        labels: Vec<Label>,
        notes: Vec<String>,
    ) -> DiagnosticVoucher {
        self.register_diag(errors::fancy_error(message, labels, notes))
    }

    fn duplicate_name_error(
        &self,
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
        &self,
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

    fn register_diag(&self, diag: Diagnostic) -> DiagnosticVoucher {
        self.add_diagnostic(diag);
        DiagnosticVoucher(PhantomData)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NamedThing {
    Item(Item),
    EnumVariant(EnumVariantId),
    SelfValue {
        /// Function `self` parameter.
        decl: Option<SelfDecl>,

        /// The function's parent, if any. If `None`, `self` has been
        /// used in a module-level function.
        parent: Option<Item>,
        span: Option<Span>,
    },
    // SelfType // when/if we add a `Self` type keyword
    Variable {
        name: SmolStr,
        typ: Result<TypeId, TypeError>,
        is_const: bool,
        span: Span,
    },
}

impl NamedThing {
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        match self {
            NamedThing::Item(item) => item.name(db),
            NamedThing::EnumVariant(variant) => variant.name(db),
            NamedThing::SelfValue { .. } => "self".into(),
            NamedThing::Variable { name, .. } => name.clone(),
        }
    }

    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        match self {
            NamedThing::Item(item) => item.name_span(db),
            NamedThing::EnumVariant(variant) => Some(variant.span(db)),
            NamedThing::SelfValue { span, .. } => *span,
            NamedThing::Variable { span, .. } => Some(*span),
        }
    }

    pub fn is_builtin(&self) -> bool {
        match self {
            NamedThing::Item(item) => item.is_builtin(),
            NamedThing::EnumVariant(_)
            | NamedThing::Variable { .. }
            | NamedThing::SelfValue { .. } => false,
        }
    }

    pub fn item_kind_display_name(&self) -> &str {
        match self {
            NamedThing::Item(item) => item.item_kind_display_name(),
            NamedThing::EnumVariant(_) => "enum variant",
            NamedThing::Variable { .. } => "variable",
            NamedThing::SelfValue { .. } => "value",
        }
    }

    pub fn resolve_path_segment(
        &self,
        db: &dyn AnalyzerDb,
        segment: &SmolStr,
    ) -> Option<NamedThing> {
        if let Self::Item(Item::Type(TypeDef::Enum(enum_))) = self {
            if let Some(variant) = enum_.variant(db, segment) {
                return Some(NamedThing::EnumVariant(variant));
            }
        }

        match self {
            Self::Item(item) => item
                .items(db)
                .get(segment)
                .map(|resolved| NamedThing::Item(*resolved)),

            _ => None,
        }
    }
}

/// This should only be created by [`AnalyzerContext`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DiagnosticVoucher(PhantomData<()>);

impl DiagnosticVoucher {
    pub fn assume_the_parser_handled_it() -> Self {
        Self(PhantomData)
    }
}

#[derive(Default)]
pub struct TempContext {
    pub diagnostics: RefCell<Vec<Diagnostic>>,
}
impl AnalyzerContext for TempContext {
    fn db(&self) -> &dyn AnalyzerDb {
        panic!("TempContext has no analyzer db")
    }

    fn resolve_name(&self, _name: &str, _span: Span) -> Result<Option<NamedThing>, IncompleteItem> {
        panic!("TempContext can't resolve names")
    }

    fn resolve_path(&self, _path: &ast::Path, _span: Span) -> Result<NamedThing, FatalError> {
        panic!("TempContext can't resolve paths")
    }

    fn resolve_visible_path(&self, _path: &ast::Path) -> Option<NamedThing> {
        panic!("TempContext can't resolve paths")
    }

    fn resolve_any_path(&self, _path: &ast::Path) -> Option<NamedThing> {
        panic!("TempContext can't resolve paths")
    }

    fn add_expression(&self, _node: &Node<ast::Expr>, _attributes: ExpressionAttributes) {
        panic!("TempContext can't store expression")
    }

    fn update_expression(&self, _node: &Node<ast::Expr>, _f: &dyn Fn(&mut ExpressionAttributes)) {
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
        _span: Span,
    ) -> Result<Option<Constant>, IncompleteItem> {
        Ok(None)
    }

    fn parent(&self) -> Item {
        panic!("TempContext has no root item")
    }

    fn module(&self) -> ModuleId {
        panic!("TempContext has no module")
    }

    fn parent_function(&self) -> FunctionId {
        panic!("TempContext has no parent function")
    }

    fn add_call(&self, _node: &Node<ast::Expr>, _call_type: CallType) {
        panic!("TempContext can't add call");
    }

    fn get_call(&self, _node: &Node<ast::Expr>) -> Option<CallType> {
        panic!("TempContext can't have calls");
    }

    fn is_in_function(&self) -> bool {
        false
    }

    fn inherits_type(&self, _typ: BlockScopeType) -> bool {
        false
    }

    fn add_diagnostic(&self, diag: Diagnostic) {
        self.diagnostics.borrow_mut().push(diag)
    }

    fn get_context_type(&self) -> Option<TypeId> {
        panic!("TempContext can't resolve Context")
    }
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct FunctionBody {
    pub expressions: IndexMap<NodeId, ExpressionAttributes>,
    // Map match statements to the corresponding [`PatternMatrix`]
    pub matches: IndexMap<NodeId, PatternMatrix>,
    // Map lhs of variable declaration to type.
    pub var_types: IndexMap<NodeId, TypeId>,
    pub calls: IndexMap<NodeId, CallType>,
    pub spans: HashMap<NodeId, Span>,
}

/// Contains contextual information relating to an expression AST node.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExpressionAttributes {
    pub typ: TypeId,
    // Evaluated constant value of const local definition.
    pub const_value: Option<Constant>,
    pub type_adjustments: Vec<Adjustment>,
}
impl ExpressionAttributes {
    pub fn original_type(&self) -> TypeId {
        self.typ
    }
    pub fn adjusted_type(&self) -> TypeId {
        if let Some(adj) = self.type_adjustments.last() {
            adj.into
        } else {
            self.typ
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Adjustment {
    pub into: TypeId,
    pub kind: AdjustmentKind,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AdjustmentKind {
    Copy,
    /// Load from storage ptr
    Load,
    IntSizeIncrease,
    StringSizeIncrease,
}

impl ExpressionAttributes {
    pub fn new(typ: TypeId) -> Self {
        Self {
            typ,
            const_value: None,
            type_adjustments: vec![],
        }
    }
}

impl crate::display::DisplayWithDb for ExpressionAttributes {
    fn format(&self, db: &dyn AnalyzerDb, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let ExpressionAttributes {
            typ,
            const_value,
            type_adjustments,
        } = self;
        write!(f, "{}", typ.display(db))?;
        if let Some(val) = &const_value {
            write!(f, " = {val:?}")?;
        }
        for adj in type_adjustments {
            write!(f, " -{:?}-> {}", adj.kind, adj.into.display(db))?;
        }
        Ok(())
    }
}

/// The type of a function call.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CallType {
    BuiltinFunction(GlobalFunction),
    Intrinsic(Intrinsic),
    BuiltinValueMethod {
        method: ValueMethod,
        typ: TypeId,
    },

    // create, create2 (will be methods of the context struct soon)
    BuiltinAssociatedFunction {
        contract: ContractId,
        function: ContractTypeMethod,
    },

    // MyStruct.foo() (soon MyStruct::foo())
    AssociatedFunction {
        typ: TypeId,
        function: FunctionId,
    },
    // some_struct_or_contract.foo()
    ValueMethod {
        typ: TypeId,
        method: FunctionId,
    },
    // some_trait.foo()
    // The reason this can not use `ValueMethod` is mainly because the trait might not have a
    // function implementation and even if it had it might not be the one that ends up getting
    // executed. An `impl` block will decide that.
    TraitValueMethod {
        trait_id: TraitId,
        method: FunctionSigId,
        // Traits can not directly be used as types but can act as bounds for generics. This is the
        // generic type that the method is called on.
        generic_type: Generic,
    },
    External {
        contract: ContractId,
        function: FunctionId,
    },
    Pure(FunctionId),
    TypeConstructor(TypeId),
    EnumConstructor(EnumVariantId),
}

impl CallType {
    pub fn function(&self) -> Option<FunctionId> {
        use CallType::*;
        match self {
            BuiltinFunction(_)
            | BuiltinValueMethod { .. }
            | TypeConstructor(_)
            | EnumConstructor(_)
            | Intrinsic(_)
            | TraitValueMethod { .. }
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
            CallType::TraitValueMethod { method: id, .. } => id.name(db),
            CallType::TypeConstructor(typ) => typ.display(db).to_string().into(),
            CallType::EnumConstructor(variant) => {
                let enum_name = variant.parent(db).name(db);
                let variant_name = variant.name(db);
                format!("{enum_name}::{variant_name}").into()
            }
        }
    }

    pub fn is_unsafe(&self, db: &dyn AnalyzerDb) -> bool {
        if let CallType::Intrinsic(_) = self {
            true
        } else if let CallType::TypeConstructor(type_id) = self {
            // check that this is the `Context` struct defined in `std`
            // this should be deleted once associated functions are supported and we can
            // define unsafe constructors in Fe
            if let Type::Struct(struct_) = type_id.typ(db) {
                struct_.name(db) == "Context" && struct_.module(db).ingot(db).name(db) == "std"
            } else {
                false
            }
        } else {
            self.function().map(|id| id.is_unsafe(db)).unwrap_or(false)
        }
    }
}

impl fmt::Display for CallType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{self:?}")
    }
}

/// Represents constant value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constant {
    Int(BigInt),
    Address(BigInt),
    Bool(bool),
    Str(SmolStr),
}
