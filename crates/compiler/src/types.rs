use fe_parser::ast as fe;
use std::collections::HashMap;

/// The name of a Fe contract.
pub type ContractName = String;
/// The AST of a Fe module.
pub type FeModuleAst = fe::Module;
/// The source of a Fe module as a static string.
pub type FeSrc<'a> = &'a str;
/// The intermediate representation of a contract as a string object.
pub type YulIr = String;
/// The bytecode of a contract as string object.
pub type Bytecode = String;

/// A mapping of contract names and their Yul IR.
pub type NamedYulContracts = HashMap<ContractName, YulIr>;
/// A mapping of contract names and their bytecode.
pub type NamedBytecodeContracts = HashMap<ContractName, Bytecode>;
