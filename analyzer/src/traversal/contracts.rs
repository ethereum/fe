use crate::errors::SemanticError;
use crate::namespace::events::Event;
use crate::namespace::scopes::{
    ContractScope,
    ModuleScope,
    Scope,
    Shared,
};
use crate::namespace::types::{
    Contract,
    FixedSize,
    Type,
};
use crate::traversal::{
    functions,
    types,
};
use crate::{
    Context,
    ContractAttributes,
};
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
    if let fe::ModuleStmt::ContractDef { name, body } = &stmt.node {
        let contract_scope = ContractScope::new(Rc::clone(&module_scope));

        for stmt in body.iter() {
            match &stmt.node {
                fe::ContractStmt::ContractField { .. } => {
                    contract_field(Rc::clone(&contract_scope), stmt)
                }
                fe::ContractStmt::EventDef { .. } => event_def(Rc::clone(&contract_scope), stmt),
                fe::ContractStmt::FuncDef { .. } => {
                    functions::func_def(Rc::clone(&contract_scope), Rc::clone(&context), stmt)
                }
            }
            .map_err(|error| error.with_context(stmt.span))?;
        }

        for stmt in body.iter() {
            if let fe::ContractStmt::FuncDef { .. } = &stmt.node {
                functions::func_body(Rc::clone(&contract_scope), Rc::clone(&context), stmt)
                    .map_err(|error| error.with_context(stmt.span))?;
            };
        }

        let contract_attributes = ContractAttributes::from(Rc::clone(&contract_scope));

        contract_scope
            .borrow()
            .module_scope()
            .borrow_mut()
            .add_type_def(
                name.node.to_string(),
                Type::Contract(Contract {
                    name: name.node.to_string(),
                    functions: contract_attributes.public_functions.clone(),
                }),
            );

        context.borrow_mut().add_contract(stmt, contract_attributes);

        return Ok(());
    }

    unreachable!()
}

fn contract_field(
    scope: Shared<ContractScope>,
    stmt: &Spanned<fe::ContractStmt>,
) -> Result<(), SemanticError> {
    if let fe::ContractStmt::ContractField { qual: _, name, typ } = &stmt.node {
        let typ = types::type_desc(Scope::Contract(Rc::clone(&scope)), typ)?;
        return scope.borrow_mut().add_field(name.node.to_string(), typ);
    }

    unreachable!()
}

fn event_def(
    scope: Shared<ContractScope>,
    stmt: &Spanned<fe::ContractStmt>,
) -> Result<(), SemanticError> {
    if let fe::ContractStmt::EventDef { name, fields } = &stmt.node {
        let name = name.node.to_string();

        let (is_indexed_bools, fields): (Vec<bool>, Vec<FixedSize>) = fields
            .iter()
            .map(|field| event_field(Rc::clone(&scope), field))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .unzip();

        let indexed_fields = is_indexed_bools
            .into_iter()
            .enumerate()
            .filter(|(_, is_indexed)| *is_indexed)
            .map(|(index, _)| index)
            .collect::<Vec<_>>();

        if indexed_fields.len() > 3 {
            return Err(SemanticError::more_than_three_indexed_params());
        }

        // check if they are trying to index an array type
        for index in indexed_fields.clone() {
            match fields[index].to_owned() {
                FixedSize::Base(_) => {}
                _ => unimplemented!("non-base type indexed event params"),
            }
        }

        return scope
            .borrow_mut()
            .add_event(name.clone(), Event::new(name, fields, indexed_fields));
    }

    unreachable!()
}

fn event_field(
    scope: Shared<ContractScope>,
    field: &Spanned<fe::EventField>,
) -> Result<(bool, FixedSize), SemanticError> {
    Ok((
        field.node.qual.is_some(),
        types::type_desc_fixed_size(Scope::Contract(scope), &field.node.typ)?,
    ))
}
