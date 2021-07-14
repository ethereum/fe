use crate::constants;
use crate::context::{Analysis, AnalyzerContext, Context, ContractAttributes};
use crate::db::AnalyzerDb;
use crate::errors::{self, AlreadyDefined, FatalError};
use crate::namespace::events::EventDef;
use crate::namespace::items::ContractId;
use crate::namespace::scopes::{ContractScope, ModuleScope, Scope, Shared};
use crate::namespace::types::{self, FixedSize, Type};
use crate::traversal::{functions, types::type_desc_fixed_size};
use fe_common::diagnostics::{Diagnostic, Label};
use fe_common::utils::humanize::pluralize_conditionally;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::rc::Rc;

// Contract info is split between 3 structs.
// Should be 2:
//  - basic type outline (name, fns)
//  - analysis (created from context object)
//      diagnostics

// types::Contract
//     pub name: String,
//     pub functions: Vec<FunctionAttributes>,

// scopes::ContractScope
//     pub name: String,
//     pub parent: Shared<ModuleScope>,
// UNUSED pub interface: Vec<String>,
//     pub event_defs: BTreeMap<String, EventDef>,
//     pub field_defs: BTreeMap<String, ContractFieldDef>,
//     pub function_defs: BTreeMap<String, ContractFunctionDef>,
//     pub list_expressions: BTreeSet<Array>,
//     pub string_defs: BTreeSet<String>,
//     pub created_contracts: BTreeSet<String>,
//     num_fields: usize,

// context::ContractAttributes
//     pub public_functions: Vec<FunctionAttributes>,
//     pub init_function: Option<FunctionAttributes>,
//     pub events: Vec<EventDef>,
//     pub list_expressions: BTreeSet<Array>,
//     pub string_literals: BTreeSet<String>,
//     pub structs: Vec<Struct>,
//     pub external_contracts: Vec<Contract>,
//     pub created_contracts: BTreeSet<String>,

/// Gather context information for contract definitions and check for type
/// errors.
pub fn contract_def(
    module_scope: Shared<ModuleScope>,
    context: &mut Context,
    stmt: &Node<fe::Contract>,
) -> Result<Shared<ContractScope>, FatalError> {
    let fe::Contract { name, body, .. } = self.data(db);
    let contract_scope = ContractScope::new(&name.kind, Rc::clone(&module_scope));

    // Contract fields are evaluated in the next pass together with function bodies
    // so that they can use other contract types that may only be defined after the
    // current contract.

    for stmt in body {
        match &stmt {
            fe::ContractStmt::Event(def) => event_def(Rc::clone(&contract_scope), context, def),
            fe::ContractStmt::Function(def) => {
                functions::func_def(Rc::clone(&contract_scope), context, def)
            }
        }?
    }

    let contract_attributes = ContractAttributes::from(Rc::clone(&contract_scope));

    if let Err(AlreadyDefined) = contract_scope
        .borrow()
        .module_scope()
        .borrow_mut()
        .add_type_def(
            &name.kind,
            Type::Contract(Contract {
                name: name.kind.to_string(),
                functions: contract_attributes.public_functions,
            }),
        )
    {
        context.fancy_error(
            "a contract with the same name already exists",
            // TODO: figure out how to include the previously defined contract
            vec![Label::primary(
                stmt.span,
                format!("Conflicting definition of contract `{}`", name.kind),
            )],
            vec![format!(
                "Note: Give one of the `{}` contracts a different name",
                name.kind
            )],
        )
    }

    Ok(contract_scope)
}

/// Gather context information for fields and function bodies of contracts.
/// Gathering this information is deferred to allow contracts to refer to other
/// contract types that are defined after it.
pub fn contract_body(
    contract_scope: Shared<ContractScope>,
    context: &mut Context,
    stmt: &Node<fe::Contract>,
) -> Result<(), FatalError> {
    let fe::Contract { fields, body, .. } = &stmt.kind;
    for field in fields {
        contract_field(Rc::clone(&contract_scope), context, field)?;
    }

    for stmt in body {
        if let fe::ContractStmt::Function(def) = &stmt {
            functions::func_body(Rc::clone(&contract_scope), context, def)?
        };
    }

    let contract_attributes = ContractAttributes::from(Rc::clone(&contract_scope));

    context.add_contract(stmt, contract_attributes);
    Ok(())
}

fn contract_field(
    scope: Shared<ContractScope>,
    context: &mut Context,
    stmt: &Node<fe::Field>,
) -> Result<(), FatalError> {
    let fe::Field { name, typ, .. } = &stmt.kind;
    let typ = types::type_desc(&Scope::Contract(Rc::clone(&scope)), context, &typ)?;

    if let Err(AlreadyDefined) = scope.borrow_mut().add_field(&name.kind, typ) {
        context.fancy_error(
            "a contract field with the same name already exists",
            // TODO: figure out how to include the previously defined field
            vec![Label::primary(
                stmt.span,
                format!("Conflicting definition of field `{}`", name.kind),
            )],
            vec![format!(
                "Note: Give one of the `{}` fields a different name",
                name.kind
            )],
        )
    }

    Ok(())
}

struct FunctionAnalysisContext {
    // Need type name map

    // parent: &'a mut ModuleContext,
    diagnostics: Vec<Diagnostic>,
    pub list_expressions: BTreeSet<types::Array>,
    pub tuples: BTreeSet<types::Tuple>,
    pub static_strings: BTreeSet<String>,
    pub created_contracts: BTreeSet<String>,
}

impl AnalyzerContext for ContractContext {
    fn resolve_type(&self, name: &str) -> Option<Type> {
        todo!()
    }
    fn add_diagnostic(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag)
    }
}

struct FieldAnalysisContext<'a> {
    db: &dyn AnalyzerDb,
    diags: Vec<Diagnostic>,
}
