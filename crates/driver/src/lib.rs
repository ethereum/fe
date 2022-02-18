#![allow(unused_imports, dead_code)]

pub use fe_mir::db::NewDb;
pub use fe_yulgen::Db;

use fe_analyzer::context::Analysis;
use fe_analyzer::namespace::items::{IngotId, IngotMode, ModuleId};
use fe_analyzer::AnalyzerDb;
use fe_common::diagnostics::{print_diagnostics, Diagnostic};
use fe_common::files::{FileKind, SourceFileId};
use fe_mir::db::MirDb;
use fe_parser::ast::SmolStr;
use fe_yulgen::YulgenDb;
use indexmap::{indexmap, IndexMap};
#[cfg(feature = "solc-backend")]
use serde_json::Value;
use std::ops::Deref;
use std::path::Path;

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
    #[cfg(feature = "solc-backend")]
    pub bytecode: String,
}

#[derive(Debug)]
pub struct CompileError(pub Vec<Diagnostic>);

pub fn compile_single_file(
    db: &mut Db,
    path: &str,
    src: &str,
    with_bytecode: bool,
    optimize: bool,
) -> Result<CompiledModule, CompileError> {
    let module = ModuleId::new_standalone(db, path, src);

    let diags = module.diagnostics(db);
    if diags.is_empty() {
        compile_module_id(db, module, with_bytecode, optimize)
    } else {
        Err(CompileError(diags))
    }
}

/// Compiles the main module of a project.
///
/// If `with_bytecode` is set to false, the compiler will skip the final Yul ->
/// Bytecode pass. This is useful when debugging invalid Yul code.
pub fn compile_ingot(
    db: &mut Db,
    name: &str,
    files: &[(impl AsRef<str>, impl AsRef<str>)],
    with_bytecode: bool,
    optimize: bool,
) -> Result<CompiledModule, CompileError> {
    let std = IngotId::std_lib(db);
    let ingot = IngotId::from_files(
        db,
        name,
        IngotMode::Main,
        FileKind::Local,
        files,
        indexmap! { "std".into() => std },
    );

    let mut diags = ingot.diagnostics(db);
    ingot.sink_external_ingot_diagnostics(db, &mut diags);
    if !diags.is_empty() {
        return Err(CompileError(diags));
    }
    let main_module = ingot
        .root_module(db)
        .expect("missing root module, with no diagnostic");
    compile_module_id(db, main_module, with_bytecode, optimize)
}

/// Returns graphviz string.
// TODO: This is temporary function for debugging.
pub fn dump_mir_single_file(db: &mut NewDb, path: &str, src: &str) -> Result<String, CompileError> {
    let module = ModuleId::new_standalone(db, path, src);

    let diags = module.diagnostics(db);
    if !diags.is_empty() {
        return Err(CompileError(diags));
    }

    let mut text = vec![];
    fe_mir::graphviz::write_mir_graphs(db, module, &mut text).unwrap();
    Ok(String::from_utf8(text).unwrap())
}

fn compile_module_id(
    db: &mut Db,
    module_id: ModuleId,
    _with_bytecode: bool,
    _optimize: bool,
) -> Result<CompiledModule, CompileError> {
    // build abi
    let json_abis = fe_abi::build(db, module_id).expect("failed to generate abi");

    // lower the AST
    let lowered_module_id = fe_lowering::lower_main_module(db, module_id);
    let lowered_ast = format!("{:#?}", &lowered_module_id.ast(db));

    if !lowered_module_id.diagnostics(db).is_empty() {
        eprintln!("Error: Analysis of lowered module resulted in the following errors:");
        print_diagnostics(db, &lowered_module_id.diagnostics(db));
        panic!("Lowered module has errors. Unfortunately, this is a bug in the Fe compiler.")
    }

    // compile to yul
    let yul_contracts = fe_yulgen::compile(db, lowered_module_id);

    // compile to bytecode if required
    let _bytecode_contracts = if _with_bytecode {
        compile_yul(yul_contracts.iter(), _optimize)
    } else {
        IndexMap::new()
    };

    // combine all of the named contract maps
    let contracts = json_abis
        .keys()
        .map(|name| {
            (
                name.clone(),
                CompiledContract {
                    json_abi: json_abis[name].clone(),
                    yul: yul_contracts[name].clone(),
                    #[cfg(feature = "solc-backend")]
                    bytecode: if _with_bytecode {
                        _bytecode_contracts[name].to_owned()
                    } else {
                        "".to_string()
                    },
                },
            )
        })
        .collect::<IndexMap<_, _>>();

    Ok(CompiledModule {
        src_ast: format!("{:?}", module_id.ast(db)),
        lowered_ast,
        contracts,
    })
}

fn compile_yul(
    _contracts: impl Iterator<Item = (impl AsRef<str>, impl AsRef<str>)>,
    _optimize: bool,
) -> IndexMap<String, String> {
    #[cfg(feature = "solc-backend")]
    {
        match fe_yulc::compile(_contracts, _optimize) {
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
            Ok(contracts) => contracts,
        }
    }

    #[cfg(not(feature = "solc-backend"))]
    IndexMap::new()
}
