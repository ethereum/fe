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
                    is_public: true,
                    param_types,
                    return_type,
                } => {
                    if name != "__init__" {
                        public_functions.push(FunctionAttributes {
                            name: name.clone(),
                            param_types: param_types.clone(),
                            return_type: return_type.clone(),
                        });
                        for param in param_types {
                            runtime_operations.push(RuntimeOperations::AbiDecode {
                                param: param.clone(),
                                location: AbiDecodeLocation::Calldata,
                            })
                        }
                        if !return_type.is_empty_tuple() {
                            runtime_operations.push(RuntimeOperations::AbiEncode {
                                params: vec![return_type.clone()],
                            })
                        }
                    } else {
                        for param in param_types {
                            runtime_operations.push(RuntimeOperations::AbiDecode {
                                param: param.clone(),
                                location: AbiDecodeLocation::Memory,
                            })
                        }
                    }
                }
                _ => {}
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
