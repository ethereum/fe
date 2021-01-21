use crate::namespace::events::Event;
use crate::namespace::types::{
    FixedSize,
    Type,
};
use std::cell::RefCell;
use std::collections::{
    HashMap,
    HashSet,
};
use std::rc::Rc;

pub type Shared<T> = Rc<RefCell<T>>;

#[derive(Clone, Debug, PartialEq)]
pub struct ContractFunctionDef {
    pub is_public: bool,
    pub param_types: Vec<FixedSize>,
    pub return_type: FixedSize,
    pub scope: Shared<BlockScope>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ContractFieldDef {
    pub nonce: usize,
    pub typ: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModuleScope {
    pub type_defs: HashMap<String, Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ContractScope {
    pub parent: Shared<ModuleScope>,
    pub interface: Vec<String>,
    pub event_defs: HashMap<String, Event>,
    pub field_defs: HashMap<String, ContractFieldDef>,
    pub function_defs: HashMap<String, ContractFunctionDef>,
    pub string_defs: HashSet<String>,
    num_fields: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockScope {
    pub name: String,
    pub parent: BlockScopeParent,
    pub variable_defs: HashMap<String, FixedSize>,
    pub typ: BlockScopeType,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Scope {
    Module(Shared<ModuleScope>),
    Contract(Shared<ContractScope>),
    Block(Shared<BlockScope>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockScopeParent {
    Contract(Shared<ContractScope>),
    Block(Shared<BlockScope>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockScopeType {
    Function,
    IfElse,
    Loop,
}

impl Scope {
    pub fn module_scope(&self) -> Shared<ModuleScope> {
        match self {
            Scope::Module(scope) => Rc::clone(scope),
            Scope::Contract(scope) => Rc::clone(&scope.borrow().parent),
            Scope::Block(scope) => Rc::clone(&scope.borrow().contract_scope().borrow().parent),
        }
    }
}

impl ModuleScope {
    pub fn new() -> Shared<Self> {
        Rc::new(RefCell::new(ModuleScope {
            type_defs: HashMap::new(),
        }))
    }

    /// Add a type definiton to the scope
    pub fn add_type_def(&mut self, name: String, typ: Type) {
        self.type_defs.insert(name, typ);
    }
}

impl ContractScope {
    pub fn new(parent: Shared<ModuleScope>) -> Shared<Self> {
        Rc::new(RefCell::new(ContractScope {
            parent,
            function_defs: HashMap::new(),
            event_defs: HashMap::new(),
            field_defs: HashMap::new(),
            string_defs: HashSet::new(),
            interface: vec![],
            num_fields: 0,
        }))
    }

    /// Lookup contract event definition by its name.
    pub fn event_def(&self, name: String) -> Option<Event> {
        self.event_defs.get(&name).map(|def| (*def).clone())
    }

    /// Lookup contract field definition by its name.
    pub fn field_def(&self, name: String) -> Option<ContractFieldDef> {
        self.field_defs.get(&name).map(|def| (*def).clone())
    }

    /// Lookup contract function definition by its name.
    pub fn function_def(&self, name: String) -> Option<ContractFunctionDef> {
        self.function_defs.get(&name).map(|def| (*def).clone())
    }

    /// Add a contract field definition to the scope.
    pub fn add_field(&mut self, name: String, typ: Type) {
        self.field_defs.insert(
            name,
            ContractFieldDef {
                nonce: self.num_fields,
                typ,
            },
        );

        self.num_fields += 1;
    }

    /// Add a function definition to the scope.
    pub fn add_function(
        &mut self,
        name: String,
        is_public: bool,
        param_types: Vec<FixedSize>,
        return_type: FixedSize,
        scope: Shared<BlockScope>,
    ) {
        self.interface.push(name.clone());
        self.function_defs.insert(
            name,
            ContractFunctionDef {
                is_public,
                param_types,
                return_type,
                scope,
            },
        );
    }

    /// Add an event definition to the scope.
    pub fn add_event(&mut self, name: String, event: Event) {
        self.event_defs.insert(name, event);
    }

    /// Add a static string definition to the scope.
    pub fn add_string(&mut self, value: String) {
        self.string_defs.insert(value);
    }
}

impl BlockScope {
    pub fn new(name: String, typ: BlockScopeType, parent: BlockScopeParent) -> Shared<Self> {
        Rc::new(RefCell::new(BlockScope {
            name,
            parent,
            variable_defs: HashMap::new(),
            typ,
        }))
    }

    /// Create a block scope from a contract scope.
    pub fn from_contract_scope(name: String, parent: Shared<ContractScope>) -> Shared<Self> {
        BlockScope::new(
            name,
            BlockScopeType::Function,
            BlockScopeParent::Contract(parent),
        )
    }

    /// Create a block scope from another block scope.
    pub fn from_block_scope(typ: BlockScopeType, parent: Shared<BlockScope>) -> Shared<Self> {
        BlockScope::new(
            "BlockScope".to_string(),
            typ,
            BlockScopeParent::Block(parent),
        )
    }

    /// Return the contract scope and its immediate block scope child
    fn find_scope_boundary(&self) -> (Shared<ContractScope>, Shared<BlockScope>) {
        let mut parent = self.parent.clone();
        let mut last_block_scope = Rc::new(RefCell::new(self.clone()));
        loop {
            parent = match parent {
                BlockScopeParent::Block(ref scope) => {
                    last_block_scope = Rc::clone(&scope);
                    scope.borrow().parent.clone()
                }
                BlockScopeParent::Contract(ref scope) => {
                    return (Rc::clone(&scope), last_block_scope)
                }
            }
        }
    }

    /// Return the contract scope that the block scope inherits from
    pub fn contract_scope(&self) -> Shared<ContractScope> {
        let (contract_scope, _) = self.find_scope_boundary();
        contract_scope
    }

    /// Return the module scope that the block scope inherits from
    pub fn module_scope(&self) -> Shared<ModuleScope> {
        Rc::clone(&self.contract_scope().borrow().parent)
    }

    /// Return the block scope that is associated with the function block
    pub fn function_scope(&self) -> Shared<BlockScope> {
        let (_, function_scope) = self.find_scope_boundary();
        function_scope
    }

    /// Lookup an event definition on the inherited contract scope
    pub fn contract_event_def(&self, name: String) -> Option<Event> {
        self.contract_scope().borrow().event_def(name)
    }

    /// Lookup a field definition on the inherited contract scope
    pub fn contract_field_def(&self, name: String) -> Option<ContractFieldDef> {
        self.contract_scope().borrow().field_def(name)
    }

    /// Lookup a function definition on the inherited contract scope.
    pub fn contract_function_def(&self, name: String) -> Option<ContractFunctionDef> {
        self.contract_scope().borrow().function_def(name)
    }

    /// Lookup the function definition for the current block scope on the
    /// inherited contract scope.
    pub fn current_function_def(&self) -> Option<ContractFunctionDef> {
        let function_name = self.function_scope().borrow().name.clone();
        self.contract_function_def(function_name)
    }

    /// Lookup a definition in current or inherited block scope
    pub fn variable_def(&self, name: String) -> Option<FixedSize> {
        let block_def = self.variable_defs.get(&name).map(|def| (*def).clone());
        if block_def.is_none() {
            if let BlockScopeParent::Block(scope) = &self.parent {
                scope.borrow().variable_def(name)
            } else {
                None
            }
        } else {
            block_def
        }
    }

    /// Add a variable to the block scope.
    pub fn add_var(&mut self, name: String, typ: FixedSize) {
        self.variable_defs.insert(name, typ);
    }

    /// Return true if the scope or any of its parents is of the given type
    pub fn inherits_type(&self, typ: BlockScopeType) -> bool {
        if self.typ != typ {
            if let BlockScopeParent::Block(scope) = &self.parent {
                return scope.borrow().inherits_type(typ);
            } else {
                return false;
            }
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use crate::namespace::scopes::{
        BlockScope,
        BlockScopeType,
        ContractScope,
        ModuleScope,
    };
    use crate::namespace::types::{
        Base,
        FixedSize,
    };
    use std::rc::Rc;

    #[test]
    fn test_scope_resolution_on_first_level_block_scope() {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        let block_scope_1 =
            BlockScope::from_contract_scope("".to_string(), Rc::clone(&contract_scope));
        assert_eq!(block_scope_1, block_scope_1.borrow().function_scope());
        assert_eq!(contract_scope, block_scope_1.borrow().contract_scope());
    }

    #[test]
    fn test_scope_resolution_on_second_level_block_scope() {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        let block_scope_1 =
            BlockScope::from_contract_scope("".to_string(), Rc::clone(&contract_scope));
        let block_scope_2 =
            BlockScope::from_block_scope(BlockScopeType::IfElse, Rc::clone(&block_scope_1));
        assert_eq!(block_scope_1, block_scope_2.borrow().function_scope());
        assert_eq!(contract_scope, block_scope_2.borrow().contract_scope());
    }

    #[test]
    fn test_1st_level_def_lookup_on_1st_level_block_scope() {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        let block_scope_1 =
            BlockScope::from_contract_scope("".to_string(), Rc::clone(&contract_scope));
        block_scope_1
            .borrow_mut()
            .add_var("some_thing".to_string(), FixedSize::Base(Base::Bool));
        assert_eq!(
            Some(FixedSize::Base(Base::Bool)),
            block_scope_1
                .borrow()
                .variable_def("some_thing".to_string())
        );
    }

    #[test]
    fn test_1st_level_def_lookup_on_2nd_level_block_scope() {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        let block_scope_1 =
            BlockScope::from_contract_scope("".to_string(), Rc::clone(&contract_scope));
        let block_scope_2 =
            BlockScope::from_block_scope(BlockScopeType::IfElse, Rc::clone(&block_scope_1));
        block_scope_1
            .borrow_mut()
            .add_var("some_thing".to_string(), FixedSize::Base(Base::Bool));
        assert_eq!(
            Some(FixedSize::Base(Base::Bool)),
            block_scope_2
                .borrow()
                .variable_def("some_thing".to_string())
        );
    }

    #[test]
    fn test_2nd_level_def_lookup_on_1nd_level_block_scope_fails() {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        let block_scope_1 =
            BlockScope::from_contract_scope("".to_string(), Rc::clone(&contract_scope));
        let block_scope_2 =
            BlockScope::from_block_scope(BlockScopeType::IfElse, Rc::clone(&block_scope_1));
        block_scope_2
            .borrow_mut()
            .add_var("some_thing".to_string(), FixedSize::Base(Base::Bool));
        assert_eq!(
            None,
            block_scope_1
                .borrow()
                .variable_def("some_thing".to_string())
        );
    }

    #[test]
    fn test_inherits_type() {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new(module_scope);
        let block_scope_1 =
            BlockScope::from_contract_scope("".to_string(), Rc::clone(&contract_scope));
        assert_eq!(
            true,
            block_scope_1
                .borrow()
                .inherits_type(BlockScopeType::Function)
        );
        assert_eq!(
            false,
            block_scope_1.borrow().inherits_type(BlockScopeType::IfElse)
        );
        assert_eq!(
            false,
            block_scope_1.borrow().inherits_type(BlockScopeType::Loop)
        );

        let block_scope_2 =
            BlockScope::from_block_scope(BlockScopeType::IfElse, Rc::clone(&block_scope_1));
        assert_eq!(
            true,
            block_scope_2
                .borrow()
                .inherits_type(BlockScopeType::Function)
        );
        assert_eq!(
            true,
            block_scope_2.borrow().inherits_type(BlockScopeType::IfElse)
        );
        assert_eq!(
            false,
            block_scope_2.borrow().inherits_type(BlockScopeType::Loop)
        );

        let block_scope_3 =
            BlockScope::from_block_scope(BlockScopeType::Loop, Rc::clone(&block_scope_2));
        assert_eq!(
            true,
            block_scope_3
                .borrow()
                .inherits_type(BlockScopeType::Function)
        );
        assert_eq!(
            true,
            block_scope_3.borrow().inherits_type(BlockScopeType::IfElse)
        );
        assert_eq!(
            true,
            block_scope_3.borrow().inherits_type(BlockScopeType::Loop)
        );
    }
}
