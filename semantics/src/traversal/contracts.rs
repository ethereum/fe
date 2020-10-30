use crate::errors::SemanticError;
use crate::namespace::events::Event;
use crate::namespace::scopes::{
    ContractDef,
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
                    contract_field(Rc::clone(&contract_scope), stmt)?
                }
                fe::ContractStmt::EventDef { .. } => {
                    event_def(Rc::clone(&contract_scope), stmt)?;
                }
                fe::ContractStmt::FuncDef { .. } => {
                    functions::func_def(Rc::clone(&contract_scope), Rc::clone(&context), stmt)?;
                }
            };
        }

        let mut runtime_operations = vec![];
        let mut public_functions = vec![];

        for (name, def) in contract_scope.borrow().defs.iter() {
            match def {
                ContractDef::Event(event) => {
                    runtime_operations.push(RuntimeOperations::AbiEncode {
                        params: event.fields.clone(),
                    })
                }
                ContractDef::Function {
                    is_public,
                    params,
                    returns,
                } => {
                    if *is_public {
                        public_functions.push(FunctionAttributes {
                            name: name.clone(),
                            param_types: params.clone(),
                            return_type: returns.clone(),
                        });
                        if !returns.is_empty_tuple() {
                            runtime_operations.push(RuntimeOperations::AbiEncode {
                                params: vec![returns.clone()],
                            })
                        }
                    }
                }
                _ => {}
            }
        }

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
        match types::type_desc(Scope::Contract(Rc::clone(&scope)), typ)? {
            Type::Map(map) => scope.borrow_mut().add_map(name.node.to_string(), map),
            Type::Array { .. } => unimplemented!(),
            Type::Base(_) => unimplemented!(),
            Type::Tuple(_) => unimplemented!(),
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
