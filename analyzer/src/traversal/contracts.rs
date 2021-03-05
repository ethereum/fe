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
) -> Result<Shared<ContractScope>, SemanticError> {
    if let fe::ModuleStmt::ContractDef { name, body } = &stmt.node {
        let contract_scope = ContractScope::new(name.node, Rc::clone(&module_scope));

        for stmt in body.iter() {
            match &stmt.node {
                fe::ContractStmt::ContractField { .. } => {
                    // Contract fields are evaluated in the next pass together with function bodies
                    // so that they can use other contract types that may only be defined after the
                    // current contract.
                    Ok(())
                }
                fe::ContractStmt::EventDef { .. } => event_def(Rc::clone(&contract_scope), stmt),
                fe::ContractStmt::FuncDef { .. } => {
                    functions::func_def(Rc::clone(&contract_scope), Rc::clone(&context), stmt)
                }
            }
            .map_err(|error| error.with_context(stmt.span))?;
        }

        let contract_attributes = ContractAttributes::from(Rc::clone(&contract_scope));

        contract_scope
            .borrow()
            .module_scope()
            .borrow_mut()
            .add_type_def(
                name.node,
                Type::Contract(Contract {
                    name: name.node.to_owned(),
                    functions: contract_attributes.public_functions,
                }),
            );

        return Ok(contract_scope);
    }

    unreachable!()
}

/// Gather context information for fields and function bodies of contracts.
/// Gathering this information is deferred to allow contracts to refer to other
/// contract types that are defined after it.
pub fn contract_body(
    contract_scope: Shared<ContractScope>,
    context: Shared<Context>,
    stmt: &Spanned<fe::ModuleStmt>,
) -> Result<(), SemanticError> {
    if let fe::ModuleStmt::ContractDef { body, .. } = &stmt.node {
        for stmt in body.iter() {
            if let fe::ContractStmt::ContractField { .. } = &stmt.node {
                contract_field(Rc::clone(&contract_scope), stmt)?;
            };

            if let fe::ContractStmt::FuncDef { .. } = &stmt.node {
                functions::func_body(Rc::clone(&contract_scope), Rc::clone(&context), stmt)
                    .map_err(|error| error.with_context(stmt.span))?;
            };
        }

        let contract_attributes = ContractAttributes::from(Rc::clone(&contract_scope));

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
        return scope.borrow_mut().add_field(name.node, typ);
    }

    unreachable!()
}

fn event_def(
    scope: Shared<ContractScope>,
    stmt: &Spanned<fe::ContractStmt>,
) -> Result<(), SemanticError> {
    if let fe::ContractStmt::EventDef { name, fields } = &stmt.node {
        let name = name.node;

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
            .add_event(name, Event::new(name, fields, indexed_fields));
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
