use crate::errors::CompileError;
use crate::yul::mappers::{constructor, functions, types};
use crate::yul::namespace::events::Event;
use crate::yul::namespace::scopes::{ContractScope, ModuleScope, Scope, Shared};
use crate::yul::namespace::types::{FixedSize, Type};
use crate::yul::runtime::abi as runtime_abi;
use crate::yul::runtime::functions as runtime_functions;
use std::rc::Rc;
use vyper_parser::ast as vyp;
use vyper_parser::span::Spanned;
use yultsur::*;

/// Builds a Yul object from a Vyper contract.
pub fn contract_def(
    module_scope: Shared<ModuleScope>,
    stmt: &Spanned<vyp::ModuleStmt>,
) -> Result<yul::Object, CompileError> {
    if let vyp::ModuleStmt::ContractDef { name: _, body } = &stmt.node {
        let contract_scope = ContractScope::new(module_scope);

        let mut statements = body.iter().try_fold::<_, _, Result<_, CompileError>>(
            vec![],
            |mut statements, stmt| {
                match &stmt.node {
                    vyp::ContractStmt::ContractField { .. } => {
                        contract_field(Rc::clone(&contract_scope), stmt)?
                    }
                    vyp::ContractStmt::EventDef { .. } => {
                        event_def(Rc::clone(&contract_scope), stmt)?
                    }
                    vyp::ContractStmt::FuncDef { .. } => {
                        statements.push(functions::func_def(Rc::clone(&contract_scope), stmt)?)
                    }
                };

                Ok(statements)
            },
        )?;

        statements.append(&mut runtime_functions::all());
        statements.push(runtime_abi::dispatcher(
            &contract_scope.borrow().interface,
            &contract_scope.borrow().defs,
        )?);

        return Ok(yul::Object {
            name: identifier! { Contract },
            code: constructor::runtime(),
            objects: vec![yul::Object {
                name: identifier! { runtime },
                code: yul::Code {
                    block: yul::Block { statements },
                },
                objects: vec![],
            }],
        });
    }

    unreachable!()
}

fn contract_field(
    scope: Shared<ContractScope>,
    stmt: &Spanned<vyp::ContractStmt>,
) -> Result<(), CompileError> {
    if let vyp::ContractStmt::ContractField { qual: _, name, typ } = &stmt.node {
        match types::type_desc(Scope::Contract(Rc::clone(&scope)), typ)? {
            Type::Map(map) => scope.borrow_mut().add_map(name.node.to_string(), map),
            Type::Array { .. } => unimplemented!("Array contract field"),
            Type::Base(_) => unimplemented!("Base contract field"),
        };

        return Ok(());
    }

    unreachable!()
}

fn event_def(
    scope: Shared<ContractScope>,
    stmt: &Spanned<vyp::ContractStmt>,
) -> Result<(), CompileError> {
    if let vyp::ContractStmt::EventDef { name, fields } = &stmt.node {
        let name = name.node.to_string();
        let fields = fields
            .iter()
            .map(|field| event_field(Rc::clone(&scope), field))
            .collect::<Result<Vec<FixedSize>, CompileError>>()?;

        scope
            .borrow_mut()
            .add_event(name.clone(), Event::new(name, fields));

        return Ok(());
    }

    unreachable!()
}

fn event_field(
    scope: Shared<ContractScope>,
    field: &Spanned<vyp::EventField>,
) -> Result<FixedSize, CompileError> {
    types::type_desc_fixed_size(Scope::Contract(scope), &field.node.typ)
}
