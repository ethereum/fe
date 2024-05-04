#![allow(unused_imports, dead_code)]

use fe_abi::event::AbiEvent;
use fe_abi::types::{AbiTupleField, AbiType};
pub use fe_codegen::db::{CodegenDb, Db};

use fe_analyzer::namespace::items::{ContractId, FunctionId, IngotId, IngotMode, ModuleId};
use fe_common::diagnostics::Diagnostic;
use fe_common::files::FileKind;
use fe_common::{db::Upcast, utils::files::BuildFiles};
use fe_parser::ast::SmolStr;
use fe_test_runner::ethabi::{Event, EventParam, ParamType};
use fe_test_runner::TestSink;
use indexmap::{indexmap, IndexMap};
use serde_json::Value;
use std::fmt::Display;

/// The artifacts of a compiled module.
pub struct CompiledModule {
    pub src_ast: String,
    pub lowered_ast: String,
    pub contracts: IndexMap<String, CompiledContract>,
}

/// The artifacts of a compiled contract.
pub struct CompiledContract {
    pub json_abi: String,
    pub yul: String,
    pub origin: ContractId,
    #[cfg(feature = "solc-backend")]
    pub bytecode: String,
    #[cfg(feature = "solc-backend")]
    pub runtime_bytecode: String,
}

#[cfg(feature = "solc-backend")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledTest {
    pub name: SmolStr,
    events: Vec<AbiEvent>,
    bytecode: String,
}

#[cfg(feature = "solc-backend")]
impl CompiledTest {
    pub fn new(name: SmolStr, events: Vec<AbiEvent>, bytecode: String) -> Self {
        Self {
            name,
            events,
            bytecode,
        }
    }

    pub fn execute(&self, sink: &mut TestSink) -> bool {
        let events = map_abi_events(&self.events);
        fe_test_runner::execute(&self.name, &events, &self.bytecode, sink)
    }
}

fn map_abi_events(events: &[AbiEvent]) -> Vec<Event> {
    events.iter().map(map_abi_event).collect()
}

fn map_abi_event(event: &AbiEvent) -> Event {
    let inputs = event
        .inputs
        .iter()
        .map(|input| {
            let kind = map_abi_type(&input.ty);
            EventParam {
                name: input.name.to_owned(),
                kind,
                indexed: input.indexed,
            }
        })
        .collect();
    Event {
        name: event.name.to_owned(),
        inputs,
        anonymous: event.anonymous,
    }
}

fn map_abi_type(typ: &AbiType) -> ParamType {
    match typ {
        AbiType::UInt(value) => ParamType::Uint(*value),
        AbiType::Int(value) => ParamType::Int(*value),
        AbiType::Address => ParamType::Address,
        AbiType::Bool => ParamType::Bool,
        AbiType::Function => panic!("function cannot be mapped to an actual ABI value type"),
        AbiType::Array { elem_ty, len } => {
            ParamType::FixedArray(Box::new(map_abi_type(elem_ty)), *len)
        }
        AbiType::Tuple(params) => ParamType::Tuple(map_abi_types(params)),
        AbiType::Bytes => ParamType::Bytes,
        AbiType::String => ParamType::String,
    }
}

fn map_abi_types(fields: &[AbiTupleField]) -> Vec<ParamType> {
    fields.iter().map(|field| map_abi_type(&field.ty)).collect()
}

#[derive(Debug)]
pub struct CompileError(pub Vec<Diagnostic>);

pub fn check_single_file(db: &mut Db, path: &str, src: &str) -> Vec<Diagnostic> {
    let module = ModuleId::new_standalone(db, path, src);
    module.diagnostics(db)
}

pub fn compile_single_file(
    db: &mut Db,
    path: &str,
    src: &str,
    with_bytecode: bool,
    with_runtime_bytecode: bool,
    optimize: bool,
) -> Result<CompiledModule, CompileError> {
    let module = ModuleId::new_standalone(db, path, src);
    let diags = module.diagnostics(db);

    if diags.is_empty() {
        compile_module(db, module, with_bytecode, with_runtime_bytecode, optimize)
    } else {
        Err(CompileError(diags))
    }
}

#[cfg(feature = "solc-backend")]
pub fn compile_single_file_tests(
    db: &mut Db,
    path: &str,
    src: &str,
    optimize: bool,
) -> Result<(SmolStr, Vec<CompiledTest>), CompileError> {
    let module = ModuleId::new_standalone(db, path, src);
    let diags = module.diagnostics(db);

    if diags.is_empty() {
        Ok((module.name(db), compile_module_tests(db, module, optimize)))
    } else {
        Err(CompileError(diags))
    }
}

// Run analysis with ingot
// Return vector error,waring...
pub fn check_ingot(db: &mut Db, build_files: &BuildFiles) -> Vec<Diagnostic> {
    let ingot = IngotId::from_build_files(db, build_files);

    let mut diags = ingot.diagnostics(db);
    ingot.sink_external_ingot_diagnostics(db, &mut diags);
    diags
}

/// Compiles the main module of a project.
///
/// If `with_bytecode` is set to false, the compiler will skip the final Yul ->
/// Bytecode pass. This is useful when debugging invalid Yul code.
pub fn compile_ingot(
    db: &mut Db,
    build_files: &BuildFiles,
    with_bytecode: bool,
    with_runtime_bytecode: bool,
    optimize: bool,
) -> Result<CompiledModule, CompileError> {
    let ingot = IngotId::from_build_files(db, build_files);

    let mut diags = ingot.diagnostics(db);
    ingot.sink_external_ingot_diagnostics(db, &mut diags);
    if !diags.is_empty() {
        return Err(CompileError(diags));
    }
    let main_module = ingot
        .root_module(db)
        .expect("missing root module, with no diagnostic");
    compile_module(
        db,
        main_module,
        with_bytecode,
        with_runtime_bytecode,
        optimize,
    )
}

#[cfg(feature = "solc-backend")]
pub fn compile_ingot_tests(
    db: &mut Db,
    build_files: &BuildFiles,
    optimize: bool,
) -> Result<Vec<(SmolStr, Vec<CompiledTest>)>, CompileError> {
    let ingot = IngotId::from_build_files(db, build_files);

    let mut diags = ingot.diagnostics(db);
    ingot.sink_external_ingot_diagnostics(db, &mut diags);
    if !diags.is_empty() {
        return Err(CompileError(diags));
    }

    if diags.is_empty() {
        Ok(ingot
            .all_modules(db)
            .iter()
            .fold(vec![], |mut accum, module| {
                accum.push((module.name(db), compile_module_tests(db, *module, optimize)));
                accum
            }))
    } else {
        Err(CompileError(diags))
    }
}

/// Returns graphviz string.
// TODO: This is temporary function for debugging.
pub fn dump_mir_single_file(db: &mut Db, path: &str, src: &str) -> Result<String, CompileError> {
    let module = ModuleId::new_standalone(db, path, src);

    let diags = module.diagnostics(db);
    if !diags.is_empty() {
        return Err(CompileError(diags));
    }

    let mut text = vec![];
    fe_mir::graphviz::write_mir_graphs(db, module, &mut text).unwrap();
    Ok(String::from_utf8(text).unwrap())
}

#[cfg(feature = "solc-backend")]
fn compile_test(db: &mut Db, test: FunctionId, optimize: bool) -> CompiledTest {
    let yul_test = fe_codegen::yul::isel::lower_test(db, test)
        .to_string()
        .replace('"', "\\\"");
    let bytecode = compile_to_evm("test", &yul_test, optimize, false).bytecode;
    let events = db.codegen_abi_module_events(test.module(db));
    CompiledTest::new(test.name(db), events, bytecode)
}

#[cfg(feature = "solc-backend")]
fn compile_module_tests(db: &mut Db, module_id: ModuleId, optimize: bool) -> Vec<CompiledTest> {
    module_id
        .tests(db)
        .iter()
        .map(|test| compile_test(db, *test, optimize))
        .collect()
}

#[cfg(feature = "solc-backend")]
fn compile_module(
    db: &mut Db,
    module_id: ModuleId,
    with_bytecode: bool,
    with_runtime_bytecode: bool,
    optimize: bool,
) -> Result<CompiledModule, CompileError> {
    let mut contracts = IndexMap::default();

    for contract in module_id.all_contracts(db.upcast()) {
        let name = &contract.data(db.upcast()).name;
        let abi = db.codegen_abi_contract(contract);
        let yul_contract = compile_to_yul(db, contract);

        let (bytecode, runtime_bytecode) = if with_bytecode || with_runtime_bytecode {
            let deployable_name = db.codegen_contract_deployer_symbol_name(contract);
            let bytecode = compile_to_evm(
                deployable_name.as_str(),
                &yul_contract,
                optimize,
                with_runtime_bytecode,
            );
            (bytecode.bytecode, bytecode.runtime_bytecode)
        } else {
            ("".to_string(), "".to_string())
        };

        contracts.insert(
            name.to_string(),
            // Maybe put the ContractID here so we can trace it back to the source file
            CompiledContract {
                json_abi: serde_json::to_string_pretty(&abi).unwrap(),
                yul: yul_contract,
                origin: contract,
                bytecode,
                runtime_bytecode,
            },
        );
    }

    Ok(CompiledModule {
        src_ast: format!("{:#?}", module_id.ast(db)),
        lowered_ast: format!("{:#?}", module_id.ast(db)),
        contracts,
    })
}

#[cfg(not(feature = "solc-backend"))]
fn compile_module(
    db: &mut Db,
    module_id: ModuleId,
    _with_bytecode: bool,
    _with_runtime_bytecode: bool,
    _optimize: bool,
) -> Result<CompiledModule, CompileError> {
    let mut contracts = IndexMap::default();
    for contract in module_id.all_contracts(db.upcast()) {
        let name = &contract.data(db.upcast()).name;
        let abi = db.codegen_abi_contract(contract);
        let yul_contract = compile_to_yul(db, contract);

        contracts.insert(
            name.to_string(),
            CompiledContract {
                json_abi: serde_json::to_string_pretty(&abi).unwrap(),
                yul: yul_contract,
                origin: contract,
            },
        );
    }

    Ok(CompiledModule {
        src_ast: format!("{:#?}", module_id.ast(db)),
        lowered_ast: format!("{:#?}", module_id.ast(db)),
        contracts,
    })
}

fn compile_to_yul(db: &mut Db, contract: ContractId) -> String {
    let yul_contract = fe_codegen::yul::isel::lower_contract_deployable(db, contract);
    yul_contract.to_string().replace('"', "\\\"")
}

#[cfg(feature = "solc-backend")]
fn compile_to_evm(
    name: &str,
    yul_object: &str,
    optimize: bool,
    verify_runtime_bytecode: bool,
) -> fe_yulc::ContractBytecode {
    match fe_yulc::compile_single_contract(name, yul_object, optimize, verify_runtime_bytecode) {
        Ok(bytecode) => bytecode,

        Err(error) => {
            for error in serde_json::from_str::<Value>(&error.0)
                .expect("unable to deserialize json output")["errors"]
                .as_array()
                .expect("errors not an array")
            {
                eprintln!(
                    "Error: {}",
                    error["formattedMessage"]
                        .as_str()
                        .expect("error value not a string")
                        .replace("\\\n", "\n")
                )
            }
            panic!("Yul compilation failed with the above errors")
        }
    }
}
