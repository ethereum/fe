use crate::builtins::GlobalMethod;
use crate::errors::{self, CannotMove, TypeError};
use crate::namespace::items::{DiagnosticSink, EventId};
use crate::namespace::types::{FixedSize, Type};
use fe_common::diagnostics::Diagnostic;
pub use fe_common::diagnostics::Label;
use fe_common::Span;
use fe_parser::node::NodeId;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Analysis<T> {
    pub value: T,
    pub diagnostics: Rc<Vec<Diagnostic>>,
}
impl<T> Analysis<T> {
    pub fn sink_diagnostics(&self, sink: &mut impl DiagnosticSink) {
        self.diagnostics.iter().for_each(|diag| sink.push(diag))
    }
}

pub trait AnalyzerContext {
    fn resolve_type(&self, name: &str) -> Option<Result<Type, TypeError>>;
    fn add_diagnostic(&mut self, diag: Diagnostic);

    fn error(&mut self, message: &str, label_span: Span, label: &str) -> DiagnosticVoucher {
        self.register_diag(errors::error(message, label_span, label))
    }

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

    fn register_diag(&mut self, diag: Diagnostic) -> DiagnosticVoucher {
        self.add_diagnostic(diag);
        DiagnosticVoucher(PhantomData::default())
    }
}

/// This should only be created by [`AnalyzerContext`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DiagnosticVoucher(PhantomData<()>);

#[derive(Default)]
pub struct TempContext {
    pub diagnostics: Vec<Diagnostic>,
}
impl AnalyzerContext for TempContext {
    fn resolve_type(&self, _name: &str) -> Option<Result<Type, TypeError>> {
        panic!("TempContext can't resolve types")
    }
    fn add_diagnostic(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag)
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
            FixedSize::Base(_) => Location::Value,
            FixedSize::Contract(_) => Location::Value,
            FixedSize::Array(_) => Location::Memory,
            FixedSize::Tuple(_) => Location::Memory,
            FixedSize::String(_) => Location::Memory,
            FixedSize::Struct(_) => Location::Memory,
        }
    }
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct FunctionBody {
    pub expressions: IndexMap<NodeId, ExpressionAttributes>,
    pub emits: IndexMap<NodeId, EventId>,

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
}

impl ExpressionAttributes {
    pub fn new(typ: Type, location: Location) -> Self {
        Self {
            typ,
            location,
            move_location: None,
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
            Type::Base(_) => {}
            Type::Contract(_) => {}
            _ => return Err(CannotMove),
        }

        if self.location != Location::Value {
            self.move_location = Some(Location::Value);
        }

        Ok(self)
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
        write!(
            f,
            "{}: {:?} => {:?}",
            self.typ, self.location, self.move_location
        )
    }
}

/// The type of a function call.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CallType {
    BuiltinFunction { func: GlobalMethod },
    TypeConstructor { typ: Type },
    SelfAttribute { func_name: String, self_span: Span },
    Pure { func_name: String },
    ValueAttribute,
    TypeAttribute { typ: Type, func_name: String },
}
