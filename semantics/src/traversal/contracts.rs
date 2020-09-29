use crate::errors::SemanticError;
use crate::namespace::events::Event;
use crate::namespace::scopes::{
    ContractScope,
    ModuleScope,
    Scope,
    Shared,
};
use crate::namespace::types::{
    FixedSize,
    Type,
};
use crate::traversal::{
    functions,
    types,
};
use crate::Context;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use std::rc::Rc;

/// Gather context information for contract definitions and check for type
/// errors.
pub fn contract_def(
    module_scope: Shared<ModuleScope>,
    context: Shared<Context>,
    stmt: &Spanned<fe::ModuleStmt>,
) -> Result<(), SemanticError> {
    if let fe::ModuleStmt::ContractDef { name: _, body } = &stmt.node {
        let contract_scope = ContractScope::new(module_scope);
        let mut functions = vec![];

        for stmt in body.iter() {
            match &stmt.node {
                fe::ContractStmt::ContractField { .. } => {
                    contract_field(Rc::clone(&contract_scope), stmt)?
                }
                fe::ContractStmt::EventDef { .. } => event_def(Rc::clone(&contract_scope), stmt)?,
                fe::ContractStmt::FuncDef { .. } => {
                    let function =
                        functions::func_def(Rc::clone(&contract_scope), Rc::clone(&context), stmt)?;
                    functions.push(function)
                }
            };
        }

        context.borrow_mut().add_contract(stmt, functions);

        return Ok(());
    }

    unreachable!()
}

fn contract_field(
    scope: Shared<ContractScope>,
    stmt: &Spanned<fe::ContractStmt>,
) -> Result<(), SemanticError> {
    if let fe::ContractStmt::ContractField { qual: _, name, typ } = &stmt.node {
        match types::type_desc(Scope::Contract(Rc::clone(&scope)), typ)? {
            Type::Map(map) => scope.borrow_mut().add_map(name.node.to_string(), map),
            Type::Array { .. } => unimplemented!(),
            Type::Base(_) => unimplemented!(),
        };

        return Ok(());
    }

    unreachable!()
}

fn event_def(
    scope: Shared<ContractScope>,
    stmt: &Spanned<fe::ContractStmt>,
) -> Result<(), SemanticError> {
    if let fe::ContractStmt::EventDef { name, fields } = &stmt.node {
        let name = name.node.to_string();
        let fields = fields
            .iter()
            .map(|field| event_field(Rc::clone(&scope), field))
            .collect::<Result<Vec<FixedSize>, SemanticError>>()?;

        scope
            .borrow_mut()
            .add_event(name.clone(), Event::new(name, fields));

        return Ok(());
    }

    unreachable!()
}

fn event_field(
    scope: Shared<ContractScope>,
    field: &Spanned<fe::EventField>,
) -> Result<FixedSize, SemanticError> {
    types::type_desc_fixed_size(Scope::Contract(scope), &field.node.typ)
}
