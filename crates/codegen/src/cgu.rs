//! This module contains definitions for Codegen Units (CGUs) which are used to
//! emit sonatina/yul ir.

use std::hash::{Hash, Hasher};

use fe_analyzer::namespace::items::{ContractId, ModuleId};
use fe_common::impl_intern_key;
use fe_mir::ir::{FunctionBody, FunctionSigId};
use indexmap::IndexSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CodegenUnit {
    pub module: ModuleId,

    /// All contracts in the CGU.
    pub contracts: Vec<ContractId>,

    /// All functions in the CGU.
    pub functions: Vec<CguFunction>,
}

impl CodegenUnit {
    pub fn new(module: ModuleId) -> Self {
        Self {
            module,
            contracts: Vec::new(),
            functions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CodegenUnitId(pub u32);
impl_intern_key!(CodegenUnitId);

#[derive(Debug, Clone)]
pub struct CguFunction {
    pub sig: FunctionSigId,
    pub body: FunctionBody,
    pub callees: IndexSet<FunctionSigId>,
}

impl PartialEq for CguFunction {
    fn eq(&self, other: &Self) -> bool {
        self.sig == other.sig
    }
}

impl Eq for CguFunction {}

impl Hash for CguFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.sig.hash(state);
    }
}
