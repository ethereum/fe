use fe_parser::ast as fe;
use std::collections::HashMap;

/// The name of a Fe contract.
pub type ContractName = String;
/// The AST of a Fe module.
pub type FeModuleAst = fe::Module;
/// The ABI of a contract as a string.
pub type JsonAbi = String;
/// The source of a Fe module as a static string.
pub type FeSrc<'a> = &'a str;
/// The intermediate representation of a contract as a string object.
pub type YulIr = String;
/// The bytecode of a contract as string object.
pub type Bytecode = String;

/// A mapping of contract names and their ABIs.
pub type NamedAbis = HashMap<ContractName, JsonAbi>;
/// A mapping of contract names and their Yul IR.
pub type NamedYulContracts = HashMap<ContractName, YulIr>;
/// A mapping of contract names and their bytecode.
pub type NamedBytecodeContracts = HashMap<ContractName, Bytecode>;

/// The artifacts of a compiled contract.
pub struct CompiledContract {
    pub json_abi: JsonAbi,
    pub yul: YulIr,
    #[cfg(feature = "solc-backend")]
    pub bytecode: Bytecode,
}

/// A mapping of contract names and their artifacts.
pub type NamedContracts = HashMap<ContractName, CompiledContract>;

/// The artifacts of a compiled module.
pub struct CompiledModule {
    pub src_tokens: String,
    pub src_ast: String,
    pub lowered_ast: String,
    pub contracts: NamedContracts,
}
