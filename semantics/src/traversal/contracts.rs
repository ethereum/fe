use crate::errors::SemanticError;
use crate::namespace::events::Event;
use crate::namespace::scopes::{
    ContractScope,
    ModuleScope,
    Scope,
    Shared,
};
use crate::namespace::types::{
    AbiDecodeLocation,
    FixedSize,
};
use crate::traversal::{
    functions,
    types,
};
use crate::{
    Context,
    ContractAttributes,
    FunctionAttributes,
    RuntimeOperations,
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
    if let fe::ModuleStmt::ContractDef { name: _, body } = &stmt.node {
        let contract_scope = ContractScope::new(module_scope);

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

        let mut runtime_operations = vec![];
        let mut public_functions = vec![];

        for (_, event) in contract_scope.borrow().event_defs.iter() {
            runtime_operations.push(RuntimeOperations::AbiEncode {
                params: event.field_types(),
            })
        }

        for (name, def) in contract_scope.borrow().function_defs.iter() {
            if !def.is_public {
                continue;
            }

            if name != "__init__" {
                public_functions.push(FunctionAttributes {
                    name: name.clone(),
                    param_types: def.param_types.clone(),
                    return_type: def.return_type.clone(),
                });
                for param in &def.param_types {
                    runtime_operations.push(RuntimeOperations::AbiDecode {
                        param: param.clone(),
                        location: AbiDecodeLocation::Calldata,
                    })
                }
                if !def.return_type.is_empty_tuple() {
                    runtime_operations.push(RuntimeOperations::AbiEncode {
                        params: vec![def.return_type.clone()],
                    })
                }
            } else {
                for param in &def.param_types {
                    runtime_operations.push(RuntimeOperations::AbiDecode {
                        param: param.clone(),
                        location: AbiDecodeLocation::Memory,
                    })
                }
            }
        }

        runtime_operations.sort();
        runtime_operations.dedup();

        let attributes = ContractAttributes {
            runtime_operations,
            public_functions,
        };

        context.borrow_mut().add_contract(stmt, attributes);

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
        scope.borrow_mut().add_field(name.node.to_string(), typ);

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

        scope
            .borrow_mut()
            .add_event(name.clone(), Event::new(name, fields, indexed_fields));

        return Ok(());
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
