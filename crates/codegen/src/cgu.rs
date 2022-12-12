//! This module contains definitions for Codegen Units (CGUs) which are used to
//! emit sonatina/yul ir.
use std::{
    hash::{Hash, Hasher},
    rc::Rc,
};

use fe_analyzer::namespace::items::{ContractId, ModuleId};
use fe_common::impl_intern_key;
use fe_mir::ir::{FunctionBody, FunctionSigId};
use indexmap::IndexMap;

use crate::db::CodegenDb;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodegenUnit {
    pub module: ModuleId,

    /// All contracts in the CGU.
    pub contracts: Vec<ContractId>,

    /// All functions in the CGU.
    pub functions: IndexMap<FunctionSigId, CguFunction>,
}

impl CodegenUnit {
    pub fn new(module: ModuleId) -> Self {
        Self {
            module,
            contracts: Vec::new(),
            functions: IndexMap::default(),
        }
    }
}

impl Hash for CodegenUnit {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.module.hash(state);
        self.contracts.hash(state);
        self.functions.iter().for_each(|(sig, func)| {
            sig.hash(state);
            func.hash(state);
        });
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CodegenUnitId(pub u32);
impl_intern_key!(CodegenUnitId);

impl CodegenUnitId {
    pub fn data(self, db: &dyn CodegenDb) -> Rc<CodegenUnit> {
        db.lookup_codegen_intern_cgu(self)
    }
}

#[derive(Debug, Clone)]
pub struct CguFunction {
    pub sig: FunctionSigId,
    pub body: FunctionBody,
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
