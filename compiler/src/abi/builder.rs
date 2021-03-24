use crate::abi::elements::{
    Contract,
    Event,
    EventField,
    ModuleAbis,
};
use crate::errors::CompileError;
use fe_analyzer::namespace::types::AbiEncoding;
use fe_analyzer::Context;
use fe_parser::ast as fe;
use fe_parser::node::Node;

/// Parse a map of contract ABIs from the input `module`.
pub fn module(context: &Context, module: &fe::Module) -> Result<ModuleAbis, CompileError> {
    module
        .body
        .iter()
        .try_fold(ModuleAbis::new(), |mut abis, stmt| {
            if let fe::ModuleStmt::ContractDef { name, body } = &stmt.kind {
                if abis
                    .insert(name.kind.to_string(), contract_def(context, body)?)
                    .is_some()
                {
                    return Err(CompileError::static_str("duplicate contract definition"));
                }
            };

            Ok(abis)
        })
}

fn contract_def(
    context: &Context,
    body: &[Node<fe::ContractStmt>],
) -> Result<Contract, CompileError> {
    body.iter().try_fold(Contract::new(), |mut contract, stmt| {
        match &stmt.kind {
            fe::ContractStmt::FuncDef { .. } => {
                let attributes = context
                    .get_function(stmt)
                    .expect("missing function attributes");

                if attributes.is_public {
                    contract.functions.push(attributes.to_owned().into())
                }
            }
            fe::ContractStmt::EventDef { .. } => {
                let attributes = context
                    .get_event(stmt)
                    .expect("missing function attributes");

                let event = Event {
                    name: attributes.name.to_owned(),
                    typ: "event".to_string(),
                    fields: attributes
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(index, (name, typ))| EventField {
                            name: name.to_owned(),
                            typ: typ.abi_type_name(),
                            indexed: attributes.indexed_fields.contains(&index),
                        })
                        .collect(),
                    anonymous: false,
                };

                contract.events.push(event);
            }
            fe::ContractStmt::ContractField { .. } => {}
        }

        Ok(contract)
    })
}

#[cfg(test)]
mod tests {
    use crate::abi::builder;
    use fe_analyzer;
    use fe_parser::parsers;

    #[test]
    fn build_contract_abi() {
        let tokens = fe_parser::get_parse_tokens(
            "\
            \ncontract Foo:\
            \n  event Food:\
            \n    idx barge: u256
            \n  pub def __init__(x: address):\
            \n    pass\
            \n  def baz(x: address) -> u256:\
            \n    revert\
            \n  pub def bar(x: u256) -> u256[10]:\
            \n    revert",
        )
        .expect("unable to parse contract");

        let module = parsers::file_input(&tokens[..])
            .expect("unable to build module AST")
            .1
            .kind;
        let context = fe_analyzer::analyze(&module).expect("failed to analyze source");
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
