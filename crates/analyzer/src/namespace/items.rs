use crate::db::AnalyzerDb;
use crate::errors;
use crate::impl_intern_key;
use crate::namespace::{events, types};
use fe_common::diagnostics::{Diagnostic, Label};
use fe_common::files::SourceFileId;
use fe_parser::ast;
use fe_parser::node::{Node, Span};
use indexmap::IndexMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Module {
    pub ast: ast::Module,
    pub file: SourceFileId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct ModuleId(pub(crate) u32);
impl_intern_key!(ModuleId);
impl ModuleId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Module> {
        db.lookup_intern_module(*self)
    }

    /// Maps defined type name to its [`TypeDefId`].
    /// Type defs include structs, type aliases, and contracts.
    pub fn type_def_map(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<String, TypeDefId>> {
        db.module_type_def_map(*self).value
    }

    /// Includes type defs with duplicate names
    pub fn all_type_defs(&self, db: &dyn AnalyzerDb) -> Rc<Vec<TypeDefId>> {
        db.module_all_type_defs(*self)
    }

    pub fn resolve_type(&self, db: &dyn AnalyzerDb, name: &str) -> Option<Rc<types::Type>> {
        db.module_resolve_type(*self, name.into())
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Rc<Vec<Diagnostic>> {
        db.module_diagnostics(*self)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TypeDefId {
    Alias(TypeAliasId),
    Struct(StructId),
    Contract(ContractId),
    // Event(EventDefId),
}
impl TypeDefId {
    pub fn name(&self, db: &dyn AnalyzerDb) -> String {
        match self {
            TypeDefId::Alias(id) => id.data(db).ast.kind.name.kind.clone(),
            TypeDefId::Struct(id) => id.data(db).ast.kind.name.kind.clone(),
            TypeDefId::Contract(id) => id.data(db).ast.kind.name.kind.clone(),
        }
    }

    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        match self {
            TypeDefId::Alias(id) => id.data(db).ast.span,
            TypeDefId::Struct(id) => id.data(db).ast.span,
            TypeDefId::Contract(id) => id.data(db).ast.span,
        }
    }

    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<types::Type> {
        match self {
            TypeDefId::Alias(id) => id.typ(db),
            TypeDefId::Struct(id) => Rc::new(types::Type::Struct(id.typ(db).as_ref().clone())),
            TypeDefId::Contract(id) => Rc::new(types::Type::Contract(id.typ(db).as_ref().clone())),
        }
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Rc<Vec<Diagnostic>> {
        match self {
            TypeDefId::Alias(id) => id.diagnostics(db),
            TypeDefId::Struct(id) => id.diagnostics(db),
            TypeDefId::Contract(id) => id.diagnostics(db),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypeAlias {
    pub ast: Node<ast::TypeAlias>,
    pub module: ModuleId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct TypeAliasId(pub(crate) u32);
impl_intern_key!(TypeAliasId);

impl TypeAliasId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<TypeAlias> {
        db.lookup_intern_type_alias(*self)
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<types::Type> {
        db.type_alias_type(*self).value
    }
    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Rc<Vec<Diagnostic>> {
        db.type_alias_type(*self).diagnostics
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Contract {
    pub ast: Node<ast::Contract>,
    pub module: ModuleId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct ContractId(pub(crate) u32);
impl_intern_key!(ContractId);
impl ContractId {
    pub fn name(&self, db: &dyn AnalyzerDb) -> String {
        self.data(db).ast.kind.name.kind.clone()
    }
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Contract> {
        db.lookup_intern_contract(*self)
    }
    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }

    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<types::Contract> {
        db.contract_type(*self).value
    }

    pub fn field(&self, db: &dyn AnalyzerDb, name: &str) -> Option<(Rc<types::Type>, usize)> {
        let fields = db.contract_fields(*self).value;
        let (index, _, typ) = fields.get_full(name)?;
        Some((Rc::clone(&typ), index))
    }

    /// All functions, including duplicates
    pub fn functions(&self, db: &dyn AnalyzerDb) -> Rc<Vec<FunctionId>> {
        db.contract_functions(*self)
    }

    /// All events, including duplicates
    pub fn events(&self, db: &dyn AnalyzerDb) -> Rc<Vec<EventId>> {
        db.contract_events(*self)
    }

    pub fn event(&self, db: &dyn AnalyzerDb, name: &str) -> Option<EventId> {
        db.contract_events(*self)
            .iter()
            .cloned()
            .find(|event| event.typ(db).name == name)
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Rc<Vec<Diagnostic>> {
        db.contract_diagnostics(*self)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Function {
    pub ast: Node<ast::Function>,
    pub contract: ContractId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct FunctionId(pub(crate) u32);
impl_intern_key!(FunctionId);
impl FunctionId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Function> {
        db.lookup_intern_function(*self)
    }

    pub fn contract(&self, db: &dyn AnalyzerDb) -> ContractId {
        self.data(db).contract
    }

    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.contract(db).module(db)
    }

    pub fn signature(&self, db: &dyn AnalyzerDb) -> Rc<types::FunctionSignature> {
        db.function_signature(*self).value
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
        let mut diags = vec![];
        diags.extend(db.function_signature(*self).diagnostics.iter().cloned());
        diags.extend(db.function_body(*self).diagnostics.iter().cloned());
        diags
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Struct {
    pub ast: Node<ast::Struct>,
    pub module: ModuleId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct StructId(pub(crate) u32);
impl_intern_key!(StructId);
impl StructId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Struct> {
        db.lookup_intern_struct(*self)
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<types::Struct> {
        db.struct_type(*self).value
    }
    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Rc<Vec<Diagnostic>> {
        db.struct_type(*self).diagnostics
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Event {
    pub ast: Node<ast::Event>,
    pub contract: ContractId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct EventId(pub(crate) u32);
impl_intern_key!(EventId);

impl EventId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Event> {
        db.lookup_intern_event(*self)
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<events::EventDef> {
        db.event_type(*self).value
    }
    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).contract.module(db)
    }
    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Rc<Vec<Diagnostic>> {
        db.event_type(*self).diagnostics
    }
    // namespace::events::EventDef
}
