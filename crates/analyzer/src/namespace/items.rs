use crate::builtins;
use crate::context;
use crate::errors::{self, TypeError};
use crate::impl_intern_key;
use crate::namespace::types::{self, GenericType, SelfDecl};
use crate::traversal::pragma::check_pragma_version;
use crate::AnalyzerDb;
use fe_common::diagnostics::Diagnostic;
use fe_parser::ast;
use fe_parser::ast::Expr;
use fe_parser::node::{Node, Span};
use indexmap::IndexMap;
use std::rc::Rc;

/// A named item. This does not include things inside of
/// a function body.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Item {
    // Module   // TODO: modules don't have names yet
    // Constant // TODO: when `const` is implemented
    Type(TypeDef),
    // GenericType probably shouldn't be a separate category.
    // Any of the items inside TypeDef (struct, alias, etc)
    // could be optionally generic.
    GenericType(GenericType),
    // Events aren't normal types; they *could* be moved into
    // TypeDef, but it would have consequences.
    Event(EventId),
    Function(FunctionId),
    Constant(ModuleConstantId),
    // Needed until we can represent keccak256 as a FunctionId.
    // We can't represent keccak256's arg type yet.
    BuiltinFunction(builtins::GlobalMethod),

    // This should go away soon. The globals (block, msg, etc) will be replaced
    // with a context struct that'll appear in the fn parameter list.
    // `self` should just be removed from here and handled as a special parameter.
    Object(builtins::Object),
}

impl Item {
    pub fn name(&self, db: &dyn AnalyzerDb) -> String {
        match self {
            Item::Type(id) => id.name(db),
            Item::GenericType(id) => id.name().to_string(),
            Item::Event(id) => id.name(db),
            Item::Function(id) => id.name(db),
            Item::BuiltinFunction(id) => id.as_ref().to_string(),
            Item::Object(id) => id.as_ref().to_string(),
            Item::Constant(id) => id.name(db),
        }
    }

    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        match self {
            Item::Type(id) => id.name_span(db),
            Item::GenericType(_) => None,
            Item::Event(id) => Some(id.name_span(db)),
            Item::Function(id) => Some(id.name_span(db)),
            Item::BuiltinFunction(_) => None,
            Item::Object(_) => None,
            Item::Constant(id) => Some(id.name_span(db)),
        }
    }

    pub fn is_builtin(&self) -> bool {
        match self {
            Item::Type(TypeDef::Primitive(_)) => true,
            Item::Type(_) => false,
            Item::GenericType(_) => true,
            Item::Event(_) => false,
            Item::Function(_) => false,
            Item::BuiltinFunction(_) => true,
            Item::Object(_) => true,
            Item::Constant(_) => false,
        }
    }

    pub fn item_kind_display_name(&self) -> &'static str {
        match self {
            Item::Type(_) => "type",
            Item::GenericType(_) => "type",
            Item::Event(_) => "event",
            Item::Function(_) => "function",
            Item::BuiltinFunction(_) => "function",
            Item::Object(_) => "object",
            Item::Constant(_) => "constant",
        }
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        match self {
            Item::Type(id) => id.sink_diagnostics(db, sink),
            Item::GenericType(_) => {}
            Item::Event(id) => id.sink_diagnostics(db, sink),
            Item::Function(id) => id.sink_diagnostics(db, sink),
            Item::BuiltinFunction(_) => {}
            Item::Object(_) => {}
            Item::Constant(id) => id.sink_diagnostics(db, sink),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Module {
    pub ast: ast::Module,
    // When we support multiple files, a module should know its file id,
    // but for now this isn't used.
    // pub file: SourceFileId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct ModuleId(pub(crate) u32);
impl_intern_key!(ModuleId);
impl ModuleId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Module> {
        db.lookup_intern_module(*self)
    }

    /// Returns a map of the named items in the module
    pub fn items(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<String, Item>> {
        db.module_item_map(*self).value
    }

    /// Includes duplicate names
    pub fn all_items(&self, db: &dyn AnalyzerDb) -> Rc<Vec<Item>> {
        db.module_all_items(*self)
    }

    pub fn imported_items(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<String, Item>> {
        db.module_imported_item_map(*self)
    }

    pub fn resolve_name(&self, db: &dyn AnalyzerDb, name: &str) -> Option<Item> {
        self.items(db)
            .get(name)
            .copied()
            .or_else(|| self.imported_items(db).get(name).copied())
    }

    /// All contracts, including duplicates
    pub fn all_contracts(&self, db: &dyn AnalyzerDb) -> Rc<Vec<ContractId>> {
        db.module_contracts(*self)
    }
    /// All structs, including duplicates
    pub fn all_structs(&self, db: &dyn AnalyzerDb) -> Rc<Vec<StructId>> {
        db.module_structs(*self)
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        self.sink_diagnostics(db, &mut diagnostics);
        diagnostics
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        let ast::Module { body } = &self.data(db).ast;
        for stmt in body {
            match stmt {
                ast::ModuleStmt::Pragma(inner) => {
                    if let Some(diag) = check_pragma_version(inner) {
                        sink.push(&diag)
                    }
                }
                ast::ModuleStmt::Use(inner) => {
                    sink.push(&errors::not_yet_implemented("use", inner.span));
                }
                _ => {} // everything else is a type def, handled below.
            }
        }

        // duplicate item name errors
        sink.push_all(db.module_item_map(*self).diagnostics.iter());

        // errors for each item
        self.all_items(db)
            .iter()
            .for_each(|id| id.sink_diagnostics(db, sink));
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ModuleConstant {
    pub ast: Node<ast::ConstantDecl>,
    pub module: ModuleId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct ModuleConstantId(pub(crate) u32);
impl_intern_key!(ModuleConstantId);

impl ModuleConstantId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<ModuleConstant> {
        db.lookup_intern_module_const(*self)
    }
    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.span
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<types::Type, TypeError> {
        db.module_constant_type(*self).value
    }

    pub fn is_base_type(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.typ(db), Ok(types::Type::Base(_)))
    }

    pub fn name(&self, db: &dyn AnalyzerDb) -> String {
        self.data(db).ast.kind.name.kind.clone()
    }

    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }

    pub fn value(&self, db: &dyn AnalyzerDb) -> fe_parser::ast::Expr {
        self.data(db).ast.kind.value.kind.clone()
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        db.module_constant_type(*self)
            .diagnostics
            .iter()
            .for_each(|d| sink.push(d));

        if !matches!(
            self.value(db),
            Expr::Bool(_) | Expr::Num(_) | Expr::Str(_) | Expr::Unit
        ) {
            sink.push(&errors::error(
                "non-literal expressions not yet supported for constants",
                self.data(db).ast.kind.value.span,
                "not a literal",
            ))
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TypeDef {
    Alias(TypeAliasId),
    Struct(StructId),
    Contract(ContractId),
    Primitive(types::Base),
}
impl TypeDef {
    pub fn name(&self, db: &dyn AnalyzerDb) -> String {
        match self {
            TypeDef::Alias(id) => id.name(db),
            TypeDef::Struct(id) => id.name(db),
            TypeDef::Contract(id) => id.name(db),
            TypeDef::Primitive(typ) => typ.to_string(),
        }
    }

    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        match self {
            TypeDef::Alias(id) => Some(id.name_span(db)),
            TypeDef::Struct(id) => Some(id.name_span(db)),
            TypeDef::Contract(id) => Some(id.name_span(db)),
            TypeDef::Primitive(_) => None,
        }
    }

    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<types::Type, TypeError> {
        match self {
            TypeDef::Alias(id) => id.typ(db),
            TypeDef::Struct(id) => Ok(types::Type::Struct(types::Struct {
                id: *id,
                name: id.name(db),
                field_count: id.fields(db).len(), // for the EvmSized trait
            })),
            TypeDef::Contract(id) => Ok(types::Type::Contract(types::Contract {
                id: *id,
                name: id.name(db),
            })),
            TypeDef::Primitive(base) => Ok(types::Type::Base(*base)),
        }
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        match self {
            TypeDef::Alias(id) => id.sink_diagnostics(db, sink),
            TypeDef::Struct(id) => id.sink_diagnostics(db, sink),
            TypeDef::Contract(id) => id.sink_diagnostics(db, sink),
            TypeDef::Primitive(_) => {}
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
    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.span
    }
    pub fn name(&self, db: &dyn AnalyzerDb) -> String {
        self.data(db).ast.name().to_string()
    }
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<types::Type, TypeError> {
        db.type_alias_type(*self).value
    }
    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        db.type_alias_type(*self)
            .diagnostics
            .iter()
            .for_each(|d| sink.push(d))
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Contract {
    pub name: String,
    pub ast: Node<ast::Contract>,
    pub module: ModuleId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct ContractId(pub(crate) u32);
impl_intern_key!(ContractId);
impl ContractId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Contract> {
        db.lookup_intern_contract(*self)
    }
    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.span
    }
    pub fn name(&self, db: &dyn AnalyzerDb) -> String {
        self.data(db).ast.name().to_string()
    }
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }

    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }

    pub fn fields(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<String, ContractFieldId>> {
        db.contract_field_map(*self).value
    }

    /// All field ids, including those with duplicate names
    pub fn all_fields(&self, db: &dyn AnalyzerDb) -> Rc<Vec<ContractFieldId>> {
        db.contract_all_fields(*self)
    }

    pub fn field_type(
        &self,
        db: &dyn AnalyzerDb,
        name: &str,
    ) -> Option<(Result<types::Type, TypeError>, usize)> {
        let fields = db.contract_field_map(*self).value;
        let (index, _, field) = fields.get_full(name)?;
        Some((field.typ(db), index))
    }

    pub fn resolve_name(&self, db: &dyn AnalyzerDb, name: &str) -> Option<Item> {
        self.pure_function(db, name)
            .map(Item::Function)
            .or_else(|| self.event(db, name).map(Item::Event))
    }

    pub fn init_function(&self, db: &dyn AnalyzerDb) -> Option<FunctionId> {
        db.contract_init_function(*self).value
    }

    /// User functions, public and not. Excludes `__init__`.
    pub fn functions(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<String, FunctionId>> {
        db.contract_function_map(*self).value
    }

    /// Lookup a function by name. Searches all user functions, private or not. Excludes init function.
    pub fn function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionId> {
        self.functions(db).get(name).copied()
    }

    /// Excludes `__init__`.
    pub fn public_functions(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<String, FunctionId>> {
        db.contract_public_function_map(*self)
    }

    /// Lookup a function by name. Matches on public and private functions, excludes init function.
    pub fn public_function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionId> {
        self.public_functions(db).get(name).copied()
    }

    /// Functions that do not have a self parameter.
    pub fn pure_functions(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<String, FunctionId>> {
        db.contract_pure_function_map(*self)
    }

    /// Get a pure function by its name.
    pub fn pure_function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionId> {
        self.pure_functions(db).get(name).copied()
    }

    /// Functions that have a self parameter.
    pub fn self_functions(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<String, FunctionId>> {
        db.contract_self_function_map(*self)
    }

    /// Get a function that takes self by its name.
    pub fn self_function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionId> {
        self.self_functions(db).get(name).copied()
    }

    /// A `Vec` of every function defined in the contract, including duplicates and the init function.
    pub fn all_functions(&self, db: &dyn AnalyzerDb) -> Rc<Vec<FunctionId>> {
        db.contract_all_functions(*self)
    }

    /// Lookup an event by name.
    pub fn event(&self, db: &dyn AnalyzerDb, name: &str) -> Option<EventId> {
        self.events(db).get(name).copied()
    }

    /// A map of events defined within the contract.
    pub fn events(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<String, EventId>> {
        db.contract_event_map(*self).value
    }

    /// A `Vec` of all events defined within the contract, including those with duplicate names.
    pub fn all_events(&self, db: &dyn AnalyzerDb) -> Rc<Vec<EventId>> {
        db.contract_all_events(*self)
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        // fields
        db.contract_field_map(*self).sink_diagnostics(sink);
        db.contract_all_fields(*self)
            .iter()
            .for_each(|field| field.sink_diagnostics(db, sink));

        // events
        db.contract_event_map(*self).sink_diagnostics(sink);
        db.contract_all_events(*self)
            .iter()
            .for_each(|event| event.sink_diagnostics(db, sink));

        // functions
        db.contract_init_function(*self).sink_diagnostics(sink);
        db.contract_function_map(*self).sink_diagnostics(sink);
        db.contract_all_functions(*self)
            .iter()
            .for_each(|id| id.sink_diagnostics(db, sink));
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ContractField {
    pub ast: Node<ast::Field>,
    pub parent: ContractId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct ContractFieldId(pub(crate) u32);
impl_intern_key!(ContractFieldId);
impl ContractFieldId {
    pub fn name(&self, db: &dyn AnalyzerDb) -> String {
        self.data(db).ast.name().to_string()
    }
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<ContractField> {
        db.lookup_intern_contract_field(*self)
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<types::Type, TypeError> {
        db.contract_field_type(*self).value
    }
    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        sink.push_all(db.contract_field_type(*self).diagnostics.iter())
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Function {
    pub ast: Node<ast::Function>,
    pub module: ModuleId,
    pub contract: ContractId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct FunctionId(pub(crate) u32);
impl_intern_key!(FunctionId);
impl FunctionId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Function> {
        db.lookup_intern_function(*self)
    }
    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.span
    }
    pub fn name(&self, db: &dyn AnalyzerDb) -> String {
        self.data(db).ast.name().to_string()
    }
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }
    pub fn is_public(&self, db: &dyn AnalyzerDb) -> bool {
        self.data(db).ast.kind.is_pub
    }
    pub fn is_pure(&self, db: &dyn AnalyzerDb) -> bool {
        self.signature(db).self_decl == SelfDecl::None
    }
    pub fn contract(&self, db: &dyn AnalyzerDb) -> ContractId {
        self.data(db).contract
    }
    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }
    pub fn signature(&self, db: &dyn AnalyzerDb) -> Rc<types::FunctionSignature> {
        db.function_signature(*self).value
    }
    pub fn body(&self, db: &dyn AnalyzerDb) -> Rc<context::FunctionBody> {
        db.function_body(*self).value
    }
    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        sink.push_all(db.function_signature(*self).diagnostics.iter());
        sink.push_all(db.function_body(*self).diagnostics.iter());
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Struct {
    pub ast: Node<ast::Struct>,
    pub module: ModuleId,
}

#[derive(Default, Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct StructId(pub(crate) u32);
impl_intern_key!(StructId);
impl StructId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Struct> {
        db.lookup_intern_struct(*self)
    }
    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.span
    }
    pub fn name(&self, db: &dyn AnalyzerDb) -> String {
        self.data(db).ast.name().to_string()
    }
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }
    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<types::Struct> {
        db.struct_type(*self)
    }

    pub fn field(&self, db: &dyn AnalyzerDb, name: &str) -> Option<StructFieldId> {
        self.fields(db).get(name).copied()
    }
    /// All fields, including duplicates
    pub fn all_fields(&self, db: &dyn AnalyzerDb) -> Rc<Vec<StructFieldId>> {
        db.struct_all_fields(*self)
    }
    pub fn fields(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<String, StructFieldId>> {
        db.struct_field_map(*self).value
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        sink.push_all(db.struct_field_map(*self).diagnostics.iter());
        db.struct_all_fields(*self)
            .iter()
            .for_each(|field| field.sink_diagnostics(db, sink));
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct StructField {
    pub ast: Node<ast::Field>,
    pub parent: StructId,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct StructFieldId(pub(crate) u32);
impl_intern_key!(StructFieldId);
impl StructFieldId {
    pub fn name(&self, db: &dyn AnalyzerDb) -> String {
        self.data(db).ast.name().to_string()
    }
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<StructField> {
        db.lookup_intern_struct_field(*self)
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<types::FixedSize, TypeError> {
        db.struct_field_type(*self).value
    }
    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        db.struct_field_type(*self).sink_diagnostics(sink)
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
    pub fn name(&self, db: &dyn AnalyzerDb) -> String {
        self.data(db).ast.name().to_string()
    }
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Event> {
        db.lookup_intern_event(*self)
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<types::Event> {
        db.event_type(*self).value
    }
    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).contract.module(db)
    }
    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        sink.push_all(db.event_type(*self).diagnostics.iter());
    }
}

pub trait DiagnosticSink {
    fn push(&mut self, diag: &Diagnostic);
    fn push_all<'a>(&mut self, iter: impl Iterator<Item = &'a Diagnostic>) {
        iter.for_each(|diag| self.push(diag))
    }
}

impl DiagnosticSink for Vec<Diagnostic> {
    fn push(&mut self, diag: &Diagnostic) {
        self.push(diag.clone())
    }
    fn push_all<'a>(&mut self, iter: impl Iterator<Item = &'a Diagnostic>) {
        self.extend(iter.cloned())
    }
}
