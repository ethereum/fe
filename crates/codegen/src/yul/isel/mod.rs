#![allow(unused)]
use fe_mir::{
    analysis::ControlFlowGraph,
    ir::{FunctionBody, FunctionSignature, ValueId},
};
use fxhash::FxHashMap;
use smol_str::SmolStr;

use crate::db::CodegenDb;

struct FuncLowerHelper<'db, 'a> {
    db: &'db dyn CodegenDb,
    value_map: FxHashMap<ValueId, SmolStr>,
    sig: &'a FunctionSignature,
    body: &'a FunctionBody,
    cfg: ControlFlowGraph,
    sink: Vec<u8>,
    ret_value: Option<SmolStr>,
}

impl<'db, 'a> FuncLowerHelper<'db, 'a> {
    fn lower_func(self) -> Vec<u8> {
        todo!()
    }
}
