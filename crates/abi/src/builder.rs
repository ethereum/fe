use crate::elements::{
    Component, Contract, Event, EventField, FuncInput, FuncOutput, FuncType, Function, JsonAbi,
    ModuleAbis,
};
use crate::AbiError;
use fe_analyzer::namespace::items::{ContractId, FunctionId, ModuleId};
use fe_analyzer::namespace::types;
use fe_analyzer::AnalyzerDb;

/// Parse a map of contract ABIs from the input `module`.
pub fn module(db: &dyn AnalyzerDb, module: ModuleId) -> Result<ModuleAbis, AbiError> {
    module
        .all_contracts(db)
        .iter()
        .try_fold(ModuleAbis::new(), |mut abis, contract| {
            if abis
                .insert(contract.name(db).into(), contract_def(db, *contract))
                .is_some()
            {
                return Err(AbiError::DuplicateContractDefinition(
                    contract.name(db).into(),
                ));
            }
            Ok(abis)
        })
}

fn contract_def(db: &dyn AnalyzerDb, contract: ContractId) -> Contract {
    let events = contract
        .events(db)
        .iter()
        .map(|(name, eventid)| {
            let attributes = eventid.typ(db);
            Event {
                name: name.to_string(),
                typ: "event".to_string(),
                fields: attributes
                    .fields
                    .iter()
                    .map(|field| {
                        let typ = field.typ.clone().expect("event field type error");
                        EventField {
                            name: field.name.to_string(),
                            typ: typ.abi_json_name(),
                            indexed: field.is_indexed,
                            components: components(db, &typ),
                        }
                    })
                    .collect(),
                anonymous: false,
            }
        })
        .collect();

    let mut functions = contract
        .public_functions(db)
        .iter()
        .map(|(name, func)| function_def(db, name, *func, FuncType::Function))
        .collect::<Vec<_>>();

    if let Some(init_fn) = contract.init_function(db) {
        functions.push(function_def(db, "", init_fn, FuncType::Constructor));
    }

    Contract { events, functions }
}

fn function_def(db: &dyn AnalyzerDb, name: &str, fn_id: FunctionId, typ: FuncType) -> Function {
    let sig = fn_id.signature(db);
    let inputs = sig
        .external_params()
        .iter()
        .map(|param| {
            let typ = param.typ.clone().expect("function parameter type error");

            FuncInput {
                name: param.name.to_string(),
                typ: typ.abi_json_name(),
                components: components(db, &typ),
            }
        })
        .collect();

    let return_type = sig.return_type.clone().expect("function return type error");
    let outputs = if return_type.is_unit() {
        vec![]
    } else {
        vec![FuncOutput {
            name: "".to_string(),
            typ: return_type.abi_json_name(),
            components: components(db, &return_type),
        }]
    };

    Function {
        name: name.to_string(),
        typ,
        inputs,
        outputs,
    }
}

fn components(db: &dyn AnalyzerDb, typ: &types::Type) -> Vec<Component> {
    match typ {
        types::Type::Struct(types::Struct { id, .. }) => id
            .fields(db)
            .iter()
            .map(|(name, field_id)| Component {
                name: name.to_string(),
                typ: field_id
                    .typ(db)
                    .expect("struct field type error")
                    .abi_json_name(),
            })
            .collect(),
        types::Type::Tuple(types::Tuple { items }) => items
            .iter()
            .enumerate()
            .map(|(index, item)| Component {
                name: format!("item{}", index),
                typ: item.abi_json_name(),
            })
            .collect(),
        _ => vec![],
    }
}

#[cfg(test)]
mod tests {
    use crate::builder;
    use fe_analyzer::namespace::items::ModuleId;
    use fe_analyzer::TestDb;
    use fe_common::diagnostics::print_diagnostics;

    #[test]
    fn build_contract_abi() {
        let contract = r#"
pub fn add(_ x: u256, _ y: u256) -> u256:
  return x + y

contract Foo:
  event Food:
    idx barge: u256
  pub fn __init__(x: address):
    pass
  fn baz(_ x: address) -> u256:
    add(10, 20)
    revert
  pub fn bar(_ x: u256) -> Array<u256, 10>:
    revert"#;

        let mut db = TestDb::default();
        let module = ModuleId::new_standalone(&mut db, "test_module", contract);

        if !module.diagnostics(&db).is_empty() {
            print_diagnostics(&db, &module.diagnostics(&db));
            panic!("failed to analyze source")
        }
        let abis = builder::module(&db, module).expect("unable to build ABI");

        if let Some(abi) = abis.get("Foo") {
            // event
            assert_eq!(abi.events[0].name, "Food");
            // function count
            assert_eq!(abi.functions.len(), 2);
            // bar
            assert_eq!(abi.functions[0].name, "bar",);
            assert_eq!(abi.functions[0].inputs[0].typ, "uint256",);
            assert_eq!(abi.functions[0].outputs[0].typ, "uint256[10]",);
            // __init__ always comes after normal functions
            assert_eq!(abi.functions[1].name, "");
            assert_eq!(abi.functions[1].inputs[0].typ, "address",);
        } else {
            panic!("contract \"Foo\" not found in module")
        }
    }
}
