use fe_parser::ast as fe;
use std::collections::HashMap;

/// The AST of a Fe module.
pub type FeModuleAst = fe::Module;
/// The source of a Fe module as a static string.
pub type FeSrc<'a> = &'a str;
/// The bytecode of a contract as string object.
pub type Bytecode = String;
/// The name of a Fe contract.
pub type ContractName = String;

/// A mapping of contract names and their bytecode.
pub type NamedBytecodeContracts = HashMap<ContractName, Bytecode>;
