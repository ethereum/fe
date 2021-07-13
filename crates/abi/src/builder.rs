use crate::elements::{Component, Contract, Event, EventField, JsonAbi, ModuleAbis};
use crate::errors::AbiError;
use fe_analyzer::context::Context;
use fe_parser::ast as fe;

/// Parse a map of contract ABIs from the input `module`.
pub fn module(context: &Context, module: &fe::Module) -> Result<ModuleAbis, AbiError> {
    module
        .body
        .iter()
        .try_fold(ModuleAbis::new(), |mut abis, stmt| {
            if let fe::ModuleStmt::Contract(contract) = &stmt {
                let name = &contract.kind.name.kind;
                if abis
                    .insert(name.to_string(), contract_def(context, &contract.kind.body))
                    .is_some()
                {
                    return Err(AbiError::DuplicateContractDefinition(name.to_string()));
                }
            };

            Ok(abis)
        })
}

fn contract_def(context: &Context, body: &[fe::ContractStmt]) -> Contract {
    body.iter().fold(Contract::new(), |mut contract, stmt| {
        match &stmt {
            fe::ContractStmt::Function(def) => {
                let attributes = context
                    .get_function(def)
                    .expect("missing function attributes");

                if attributes.is_public {
                    contract.functions.push(attributes.to_owned().into())
                }
            }
            fe::ContractStmt::Event(def) => {
                let attributes = context.get_event(def).expect("missing function attributes");

                let event = Event {
                    name: attributes.name.to_owned(),
                    typ: "event".to_string(),
                    fields: attributes
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(index, (name, typ))| EventField {
                            name: name.to_owned(),
                            typ: typ.abi_json_name(),
                            indexed: attributes.indexed_fields.contains(&index),
                            components: typ
                                .abi_components()
                                .iter()
                                .map(|component| Component::from(component.to_owned()))
                                .collect(),
                        })
                        .collect(),
                    anonymous: false,
                };

                contract.events.push(event);
            }
        }

        contract
    })
}

#[cfg(test)]
mod tests {
    use crate::builder;
    use fe_common::files::SourceFileId;
    use fe_parser::{grammar::module::parse_module, parse_code_chunk};

    #[test]
    fn build_contract_abi() {
        let contract = "\
            \ncontract Foo:\
            \n  event Food:\
            \n    idx barge: u256
            \n  pub def __init__(x: address):\
            \n    pass\
            \n  def baz(x: address) -> u256:\
            \n    revert\
            \n  pub def bar(x: u256) -> u256[10]:\
            \n    revert";

        let module = parse_code_chunk(parse_module, contract)
            .expect("unable to build module AST")
            .kind;
        let context =
            fe_analyzer::analyze(&module, SourceFileId(0)).expect("failed to analyze source");
        let abis = builder::module(&context, &module).expect("unable to build ABI");

        if let Some(abi) = abis.get("Foo") {
            // event
            assert_eq!(abi.events[0].name, "Food");
            // function count
            assert_eq!(abi.functions.len(), 2);
            // __init__
            assert_eq!(abi.functions[0].name, "");
            assert_eq!(abi.functions[0].inputs[0].typ, "address",);
            // bar
            assert_eq!(abi.functions[1].name, "bar",);
            assert_eq!(abi.functions[1].inputs[0].typ, "uint256",);
            assert_eq!(abi.functions[1].outputs[0].typ, "uint256[10]",);
        } else {
            panic!("contract \"Foo\" not found in module")
        }
    }
}
