use crate::builtins::GlobalMethod;
use crate::errors::CannotMove;
use crate::namespace::events::EventDef;
use crate::namespace::scopes::{ContractFunctionDef, ContractScope, ModuleScope, Shared};
use crate::namespace::types::{Contract, FixedSize, Struct, Type};
pub use fe_common::diagnostics::Label;
use fe_common::diagnostics::{Diagnostic, Severity};
use fe_common::files::SourceFileId;
use fe_common::Span;
use fe_parser::ast as fe;
use fe_parser::node::{Node, NodeId};

use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

/// Indicates where an expression is stored.
#[derive(Copy, Clone, Debug, PartialEq, Hash)]
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

/// Contains contextual information relating to a contract AST node.
#[derive(Clone, Debug, PartialEq, Hash)]
pub struct ContractAttributes {
    /// Public functions that have been defined by the user.
    pub public_functions: Vec<FunctionAttributes>,
    /// An init function that has been defined by the user.
    pub init_function: Option<FunctionAttributes>,
    /// Events that have been defined by the user.
    pub events: Vec<EventDef>,
    /// Static strings that the contract defines
    pub string_literals: BTreeSet<String>,
    /// Structs that have been defined by the user
    pub structs: Vec<Struct>,
    /// External contracts that may be called from within this contract.
    pub external_contracts: Vec<Contract>,
    /// Names of contracts that have been created inside of this contract.
    pub created_contracts: BTreeSet<String>,
}

impl From<Shared<ContractScope>> for ContractAttributes {
    fn from(scope: Shared<ContractScope>) -> Self {
        let mut public_functions = vec![];
        let mut init_function = None;

        for (name, def) in scope.borrow().function_defs.iter() {
            if !def.is_public {
                continue;
            }

            if name != "__init__" {
                public_functions.push(FunctionAttributes {
                    is_public: def.is_public,
                    name: name.clone(),
                    params: def.params.to_owned(),
                    return_type: def.return_type.to_owned(),
                });
            } else {
                init_function = Some(FunctionAttributes {
                    is_public: def.is_public,
                    name: name.clone(),
                    params: def.params.to_owned(),
                    return_type: FixedSize::unit(),
                })
            }
        }

        let external_contracts = scope.borrow().get_module_type_defs(|typ| {
            if let Type::Contract(contract) = typ {
                // Skip our own contract
                if contract.name == scope.borrow().name {
                    None
                } else {
                    Some(contract.to_owned())
                }
            } else {
                None
            }
        });

        let structs = scope.borrow().get_module_type_defs(|typ| {
            if let Type::Struct(val) = typ {
                Some(val.to_owned())
            } else {
                None
            }
        });

        ContractAttributes {
            public_functions,
            init_function,
            events: scope
                .borrow()
                .event_defs
                .values()
                .map(|event| event.to_owned())
                .collect::<Vec<EventDef>>(),
            string_literals: scope.borrow().string_defs.clone(),
            structs,
            external_contracts,
            created_contracts: scope.borrow().created_contracts.to_owned(),
        }
    }
}

/// Contains contextual information relating to an expression AST node.
#[derive(Clone, Debug, PartialEq, Hash)]
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

/// The type of a function call.
#[derive(Clone, Debug, PartialEq, Hash)]
pub enum CallType {
    BuiltinFunction { func: GlobalMethod },
    TypeConstructor { typ: Type },
    SelfAttribute { func_name: String },
    ValueAttribute,
    TypeAttribute { typ: Type, func_name: String },
}

/// Contains contextual information relating to a function definition AST node.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionAttributes {
    pub is_public: bool,
    pub name: String,
    pub params: Vec<(String, FixedSize)>,
    pub return_type: FixedSize,
}

impl FunctionAttributes {
    pub fn param_types(&self) -> Vec<FixedSize> {
        self.params.iter().map(|(_, typ)| typ.to_owned()).collect()
    }

    pub fn param_names(&self) -> Vec<String> {
        self.params
            .iter()
            .map(|(name, _)| name.to_owned())
            .collect()
    }
}

impl From<ContractFunctionDef> for FunctionAttributes {
    fn from(def: ContractFunctionDef) -> Self {
        Self {
            is_public: def.is_public,
            name: def.name,
            params: def.params,
            return_type: def.return_type,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModuleAttributes {
    /// Type definitions in a module.
    pub type_defs: BTreeMap<String, Type>,
}

impl From<Shared<ModuleScope>> for ModuleAttributes {
    fn from(scope: Shared<ModuleScope>) -> Self {
        Self {
            type_defs: scope.borrow().type_defs.clone(),
        }
    }
}

/// Contains contextual information about a Fe module and can be queried using
/// `Spanned` AST nodes.
#[derive(Clone, Debug)]
pub struct Context {
    /// Node ids in the order they were visited.
    pub node_ids: Vec<NodeId>,
    /// The span of a given node id.
    pub spans: BTreeMap<NodeId, Span>,
    pub expressions: BTreeMap<NodeId, ExpressionAttributes>,
    pub emits: BTreeMap<NodeId, EventDef>,
    pub functions: BTreeMap<NodeId, FunctionAttributes>,
    pub declarations: BTreeMap<NodeId, FixedSize>,
    pub contracts: BTreeMap<NodeId, ContractAttributes>,
    pub calls: BTreeMap<NodeId, CallType>,
    pub events: BTreeMap<NodeId, EventDef>,
    pub type_descs: BTreeMap<NodeId, Type>,
    pub module: Option<ModuleAttributes>,
    pub file_id: SourceFileId,
    pub diagnostics: Vec<Diagnostic>,
    /// Holds fresh id for [`Context::make_unique_name`]
    fresh_id: u64,
}

impl Context {
    pub fn new_shared(file_id: SourceFileId) -> Shared<Self> {
        Rc::new(RefCell::new(Self::new(file_id)))
    }

    pub fn new(file_id: SourceFileId) -> Self {
        Context {
            node_ids: Vec::new(),
            spans: BTreeMap::new(),
            expressions: BTreeMap::new(),
            emits: BTreeMap::new(),
            functions: BTreeMap::new(),
            declarations: BTreeMap::new(),
            contracts: BTreeMap::new(),
            calls: BTreeMap::new(),
            events: BTreeMap::new(),
            type_descs: BTreeMap::new(),
            module: None,
            file_id,
            diagnostics: Vec::new(),
            fresh_id: 0,
        }
    }

    fn add_node<T>(&mut self, node: &Node<T>) {
        self.node_ids.push(node.id);
        self.spans.insert(node.id, node.span);
    }

    /// Attribute contextual information to an expression node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_expression(&mut self, node: &Node<fe::Expr>, attributes: ExpressionAttributes) {
        self.add_node(node);
        expect_none(
            self.expressions.insert(node.id, attributes),
            "expression attributes already exist",
        );
    }

    /// Update the expression attributes.
    ///
    /// # Panics
    ///
    /// Panics if an entry does not already exist for the node id.
    pub fn update_expression(&mut self, node: &Node<fe::Expr>, attributes: ExpressionAttributes) {
        self.expressions
            .insert(node.id, attributes)
            .expect("expression attributes do not exist");
    }

    /// Get information that has been attributed to an expression node.
    pub fn get_expression<T: Into<NodeId>>(&self, node_id: T) -> Option<&ExpressionAttributes> {
        self.expressions.get(&node_id.into())
    }

    /// Attribute contextual information to an emit statement node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_emit(&mut self, node: &Node<fe::FuncStmt>, event: EventDef) {
        self.add_node(node);
        expect_none(
            self.emits.insert(node.id, event),
            "emit statement attributes already exist",
        );
    }

    /// Get information that has been attributed to an emit statement node.
    pub fn get_emit<T: Into<NodeId>>(&self, node_id: T) -> Option<&EventDef> {
        self.emits.get(&node_id.into())
    }

    /// Attribute contextual information to a function definition node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_function(&mut self, node: &Node<fe::Function>, attributes: FunctionAttributes) {
        self.add_node(node);
        expect_none(
            self.functions.insert(node.id, attributes),
            "function attributes already exist",
        );
    }

    /// Get information that has been attributed to a function definition node.
    pub fn get_function<T: Into<NodeId>>(&self, node_id: T) -> Option<&FunctionAttributes> {
        self.functions.get(&node_id.into())
    }

    /// Attribute contextual information to a declaration node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_declaration(&mut self, node: &Node<fe::FuncStmt>, typ: FixedSize) {
        self.add_node(node);
        expect_none(
            self.declarations.insert(node.id, typ),
            "declaration attributes already exist",
        );
    }

    /// Get information that has been attributed to a declaration node.
    pub fn get_declaration<T: Into<NodeId>>(&self, node_id: T) -> Option<&FixedSize> {
        self.declarations.get(&node_id.into())
    }

    /// Attribute contextual information to a contract definition node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_contract(&mut self, node: &Node<fe::Contract>, attributes: ContractAttributes) {
        self.add_node(node);
        expect_none(
            self.contracts.insert(node.id, attributes),
            "contract attributes already exist",
        );
    }

    /// Get information that has been attributed to a contract definition node.
    pub fn get_contract<T: Into<NodeId>>(&self, node_id: T) -> Option<&ContractAttributes> {
        self.contracts.get(&node_id.into())
    }

    /// Attribute contextual information to a call expression node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_call(&mut self, node: &Node<fe::Expr>, call_type: CallType) {
        self.add_node(node);
        expect_none(
            self.calls.insert(node.id, call_type),
            "call attributes already exist",
        );
    }

    /// Get information that has been attributed to a call expression node.
    pub fn get_call<T: Into<NodeId>>(&self, node_id: T) -> Option<&CallType> {
        self.calls.get(&node_id.into())
    }

    /// Attribute contextual information to an event definition node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_event(&mut self, node: &Node<fe::Event>, event: EventDef) {
        self.add_node(node);
        expect_none(
            self.events.insert(node.id, event),
            "event attributes already exist",
        );
    }

    /// Get information that has been attributed to an event definition node.
    pub fn get_event<T: Into<NodeId>>(&self, node_id: T) -> Option<&EventDef> {
        self.events.get(&node_id.into())
    }

    /// Attribute contextual information to a type description node.
    ///
    /// # Panics
    ///
    /// Panics if an entry already exists for the node id.
    pub fn add_type_desc(&mut self, node: &Node<fe::TypeDesc>, typ: Type) {
        self.add_node(node);
        expect_none(
            self.type_descs.insert(node.id, typ),
            "type desc attributes already exist",
        );
    }

    /// Get information that has been attributed to a type description node.
    pub fn get_type_desc<T: Into<NodeId>>(&self, node_id: T) -> Option<&Type> {
        self.type_descs.get(&node_id.into())
    }

    /// Attribute contextual information to the module.
    pub fn set_module(&mut self, attributes: ModuleAttributes) {
        self.module = Some(attributes);
    }

    /// Get information that has been attributed to the module.
    pub fn get_module(&self) -> Option<&ModuleAttributes> {
        self.module.as_ref()
    }

    /// Get the span and attributes of all expressions.
    pub fn get_spanned_expressions(&self) -> Vec<(Span, ExpressionAttributes)> {
        self.node_ids
            .iter()
            .filter_map(|node_id| {
                self.get_expression(*node_id)
                    .map(|attributes| (self.spans[node_id], attributes.to_owned()))
            })
            .collect::<Vec<_>>()
    }

    /// Get the span and attributes of all emits.
    pub fn get_spanned_emits(&self) -> Vec<(Span, EventDef)> {
        self.node_ids
            .iter()
            .filter_map(|node_id| {
                self.get_emit(*node_id)
                    .map(|attributes| (self.spans[node_id], attributes.to_owned()))
            })
            .collect::<Vec<_>>()
    }

    /// Get the span and attributes of all functions.
    pub fn get_spanned_functions(&self) -> Vec<(Span, FunctionAttributes)> {
        self.node_ids
            .iter()
            .filter_map(|node_id| {
                self.get_function(*node_id)
                    .map(|attributes| (self.spans[node_id], attributes.to_owned()))
            })
            .collect::<Vec<_>>()
    }

    /// Get the span and attributes of all declarations.
    pub fn get_spanned_declarations(&self) -> Vec<(Span, FixedSize)> {
        self.node_ids
            .iter()
            .filter_map(|node_id| {
                self.get_declaration(*node_id)
                    .map(|attributes| (self.spans[node_id], attributes.to_owned()))
            })
            .collect::<Vec<_>>()
    }

    /// Get the span and attributes of all contracts.
    pub fn get_spanned_contracts(&self) -> Vec<(Span, ContractAttributes)> {
        self.node_ids
            .iter()
            .filter_map(|node_id| {
                self.get_contract(*node_id)
                    .map(|attributes| (self.spans[node_id], attributes.to_owned()))
            })
            .collect::<Vec<_>>()
    }

    /// Get the span and attributes of all calls.
    pub fn get_spanned_calls(&self) -> Vec<(Span, CallType)> {
        self.node_ids
            .iter()
            .filter_map(|node_id| {
                self.get_call(*node_id)
                    .map(|attributes| (self.spans[node_id], attributes.to_owned()))
            })
            .collect::<Vec<_>>()
    }

    /// Get the span and attributes of all events.
    pub fn get_spanned_events(&self) -> Vec<(Span, EventDef)> {
        self.node_ids
            .iter()
            .filter_map(|node_id| {
                self.get_event(*node_id)
                    .map(|attributes| (self.spans[node_id], attributes.to_owned()))
            })
            .collect::<Vec<_>>()
    }

    /// Get the span and attributes of all type descs.
    pub fn get_spanned_type_descs(&self) -> Vec<(Span, Type)> {
        self.node_ids
            .iter()
            .filter_map(|node_id| {
                self.get_type_desc(*node_id)
                    .map(|attributes| (self.spans[node_id], attributes.to_owned()))
            })
            .collect::<Vec<_>>()
    }

    pub fn error(
        &mut self,
        message: impl Into<String>,
        label_span: Span,
        label: impl Into<String>,
    ) {
        self.fancy_error(message, vec![Label::primary(label_span, label)], vec![]);
    }

    pub fn type_error(
        &mut self,
        message: impl Into<String>,
        span: Span,
        expected: impl Display,
        actual: impl Display,
    ) {
        self.error(
            message,
            span,
            format!("this has type `{}`; expected type `{}`", actual, expected),
        )
    }

    pub fn not_yet_implemented(&mut self, feature: impl Display, span: Span) {
        self.error(
            "feature not yet implemented",
            span,
            format!("{} is not yet implemented", feature),
        )
    }

    pub fn fancy_error(
        &mut self,
        message: impl Into<String>,
        labels: Vec<Label>,
        notes: Vec<String>,
    ) {
        self.diagnostics.push(Diagnostic {
            severity: Severity::Error,
            code: None,
            message: message.into(),
            labels: labels
                .into_iter()
                .map(|lbl| lbl.into_cs_label(self.file_id))
                .collect(),
            notes,
        });
    }

    /// Makes a unique name from the given name, keeping it as readable as possible.
    pub fn make_unique_name(&mut self, name: &str) -> String {
        let id = self.fresh_id;
        self.fresh_id += 1;
        format!("${}_{}", name, id)
    }
}

/// Temporary helper until `expect_none` is stable.
fn expect_none<T>(item: Option<T>, msg: &str) {
    if item.is_some() {
        panic!("{}", msg)
    }
}
