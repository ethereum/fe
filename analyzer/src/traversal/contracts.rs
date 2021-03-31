use crate::errors::SemanticError;
use crate::namespace::events::EventDef;
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
use fe_parser::node::Node;
use std::rc::Rc;

/// Gather context information for contract definitions and check for type
/// errors.
pub fn contract_def(
    module_scope: Shared<ModuleScope>,
    context: Shared<Context>,
    stmt: &Node<fe::ModuleStmt>,
) -> Result<Shared<ContractScope>, SemanticError> {
    if let fe::ModuleStmt::ContractDef {
        name,
        fields: _,
        body,
    } = &stmt.kind
    {
        let contract_scope = ContractScope::new(&name.kind, Rc::clone(&module_scope));

        // Contract fields are evaluated in the next pass together with function bodies
        // so that they can use other contract types that may only be defined after the
        // current contract.

        for stmt in body {
            match &stmt.kind {
                fe::ContractStmt::EventDef { .. } => {
                    event_def(Rc::clone(&contract_scope), Rc::clone(&context), stmt)
                }
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
                &name.kind,
                Type::Contract(Contract {
                    name: name.kind.to_string(),
                    functions: contract_attributes.public_functions,
                }),
            )?;

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
    stmt: &Node<fe::ModuleStmt>,
) -> Result<(), SemanticError> {
    if let fe::ModuleStmt::ContractDef { fields, body, .. } = &stmt.kind {
        for field in fields {
            contract_field(Rc::clone(&contract_scope), field)?;
        }

        for stmt in body {
            if let fe::ContractStmt::FuncDef { .. } = &stmt.kind {
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
    stmt: &Node<fe::Field>,
) -> Result<(), SemanticError> {
    let fe::Field { name, typ, .. } = &stmt.kind;
    let typ = types::type_desc(Scope::Contract(Rc::clone(&scope)), typ)?;
    scope.borrow_mut().add_field(&name.kind, typ)
}

fn event_def(
    scope: Shared<ContractScope>,
    context: Shared<Context>,
    stmt: &Node<fe::ContractStmt>,
) -> Result<(), SemanticError> {
    if let fe::ContractStmt::EventDef { name, fields } = &stmt.kind {
        let name = &name.kind;

        let (is_indexed_bools, fields): (Vec<bool>, Vec<(String, FixedSize)>) = fields
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
            match fields[index].1.to_owned() {
                FixedSize::Base(_) => {}
                _ => unimplemented!("non-base type indexed event params"),
            }
        }

        let event = EventDef::new(name, fields, indexed_fields);

        context.borrow_mut().add_event(stmt, event.clone());

        return scope.borrow_mut().add_event(name, event);
    }

    unreachable!()
}

fn event_field(
    scope: Shared<ContractScope>,
    field: &Node<fe::EventField>,
) -> Result<(bool, (String, FixedSize)), SemanticError> {
    Ok((
        field.kind.idx_qual.is_some(),
        (
            field.kind.name.kind.to_string(),
            types::type_desc_fixed_size(Scope::Contract(scope), &field.kind.typ)?,
        ),
    ))
}
