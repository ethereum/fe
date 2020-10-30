use crate::namespace::events::Event;
use crate::namespace::types::{
    Array,
    Base,
    FixedSize,
    Map,
    Type,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use fe_parser::span::Span;

pub type Shared<T> = Rc<RefCell<T>>;

#[derive(Clone, Debug, PartialEq)]
pub enum ModuleDef {
    Type(Type),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ContractDef {
    Function {
        is_public: bool,
        params: Vec<FixedSize>,
        returns: FixedSize,
    },
    Map {
        index: usize,
        map: Map,
    },
    Event(Event),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionDef {
    Base(Base),
    Array(Array),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModuleScope {
    pub defs: HashMap<String, ModuleDef>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ContractScope {
    pub parent: Shared<ModuleScope>,
    pub defs: HashMap<String, ContractDef>,
    pub interface: Vec<String>,
    storage_count: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionScope {
    pub span: Span,
    pub parent: Shared<ContractScope>,
    pub defs: HashMap<String, FunctionDef>,
}

#[allow(dead_code)]
pub enum Scope {
    Module(Shared<ModuleScope>),
    Contract(Shared<ContractScope>),
    Function(Shared<FunctionScope>),
}

impl Scope {
    pub fn module_scope(&self) -> Shared<ModuleScope> {
        match self {
            Scope::Module(scope) => Rc::clone(scope),
            Scope::Contract(scope) => Rc::clone(&scope.borrow().parent),
            Scope::Function(scope) => Rc::clone(&scope.borrow().parent.borrow().parent),
        }
    }
}

impl ModuleScope {
    pub fn new() -> Shared<Self> {
        Rc::new(RefCell::new(ModuleScope {
            defs: HashMap::new(),
        }))
    }

    pub fn add_type_def(&mut self, name: String, typ: Type) {
        self.defs.insert(name, ModuleDef::Type(typ));
    }
}

impl ContractScope {
    pub fn new(parent: Shared<ModuleScope>) -> Shared<Self> {
        Rc::new(RefCell::new(ContractScope {
            parent,
            defs: HashMap::new(),
            interface: vec![],
            storage_count: 0,
        }))
    }

    pub fn def(&self, name: String) -> Option<ContractDef> {
        self.defs.get(&name).map(|def| (*def).clone())
    }

    pub fn add_map(&mut self, name: String, map: Map) {
        self.defs.insert(
            name,
            ContractDef::Map {
                index: self.storage_count,
                map,
            },
        );
        self.storage_count += 1;
    }

    pub fn add_function(
        &mut self,
        name: String,
        is_public: bool,
        params: Vec<FixedSize>,
        returns: FixedSize,
    ) {
        self.interface.push(name.clone());
        self.defs.insert(
            name,
            ContractDef::Function {
                is_public,
                params,
                returns,
            },
        );
    }

    pub fn add_event(&mut self, name: String, event: Event) {
        self.defs.insert(name, ContractDef::Event(event));
    }
}

impl FunctionScope {
    pub fn new(span: Span, parent: Shared<ContractScope>) -> Shared<Self> {
        Rc::new(RefCell::new(FunctionScope {
            span,
            parent,
            defs: HashMap::new(),
        }))
    }

    #[allow(dead_code)]
    pub fn module_scope(&self) -> Shared<ModuleScope> {
        Rc::clone(&self.parent.borrow().parent)
    }

    pub fn contract_scope(&self) -> Shared<ContractScope> {
        Rc::clone(&self.parent)
    }

    pub fn contract_def(&self, name: String) -> Option<ContractDef> {
        self.contract_scope().borrow().def(name)
    }

    pub fn def(&self, name: String) -> Option<FunctionDef> {
        self.defs.get(&name).map(|def| (*def).clone())
    }

    pub fn add_array(&mut self, name: String, array: Array) {
        self.defs.insert(name, FunctionDef::Array(array));
    }

    pub fn add_base(&mut self, name: String, base: Base) {
        self.defs.insert(name, FunctionDef::Base(base));
    }
}
