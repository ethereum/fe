use fe_analyzer::namespace::items as analyzer_items;
use fe_common::impl_intern_key;
use fxhash::FxHashMap;
use id_arena::Arena;
use smol_str::SmolStr;

use super::{
    basic_block::BasicBlock,
    body_order::BodyOrder,
    inst::{BranchInfo, Inst, InstId},
    types::TypeId,
    value::{Immediate, Local, Value, ValueId},
    BasicBlockId, SourceInfo,
};

/// Represents function signature.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
    pub params: Vec<FunctionParam>,
    pub return_type: TypeId,
    pub module_id: analyzer_items::ModuleId,
    pub analyzer_func_id: analyzer_items::FunctionId,
    pub linkage: Linkage,
    pub has_self: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionParam {
    pub name: SmolStr,
    pub ty: TypeId,
    pub source: SourceInfo,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub(crate) u32);
impl_intern_key!(FunctionId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Linkage {
    /// A function can only be called within the same module.
    Private,

    /// A function can be called from other modules, but can NOT be called from
    /// other accounts and transactions.
    Public,

    /// A function can be called from other modules, and also can be called from
    /// other accounts and transactions.
    Export,
}

/// A function body, which is not stored in salsa db to enable in-place
/// transformation.
#[derive(Debug, PartialEq, Eq)]
pub struct FunctionBody {
    pub fid: FunctionId,

    pub store: BodyDataStore,

    /// Tracks order of basic blocks and instructions in a function body.
    pub order: BodyOrder,

    pub source: SourceInfo,
}

impl FunctionBody {
    pub fn new(fid: FunctionId, source: SourceInfo) -> Self {
        let mut store = BodyDataStore::default();
        let entry_bb = store.store_block(BasicBlock {});
        Self {
            fid,
            store,
            order: BodyOrder::new(entry_bb),
            source,
        }
    }
}

/// A collection of basic block, instructions and values appear in a function
/// body.
#[derive(Default, Debug, PartialEq, Eq)]
pub struct BodyDataStore {
    /// Instructions appear in a function body.
    insts: Arena<Inst>,

    /// All values in a function.
    values: Arena<Value>,

    blocks: Arena<BasicBlock>,

    /// Maps an immediate to a value to ensure the same immediate results in the
    /// same value.
    immediates: FxHashMap<Immediate, ValueId>,

    unit_value: Option<ValueId>,

    /// Maps an instruction to a value.
    inst_results: FxHashMap<InstId, ValueId>,

    /// All declared local variables in a function.
    locals: Vec<ValueId>,
}

impl BodyDataStore {
    pub fn store_inst(&mut self, inst: Inst) -> InstId {
        self.insts.alloc(inst)
    }

    pub fn inst_data(&self, inst: InstId) -> &Inst {
        &self.insts[inst]
    }

    pub fn store_value(&mut self, value: Value) -> ValueId {
        match value {
            Value::Immediate(imm) => self.store_immediate(imm),

            Value::Unit(_) => {
                if let Some(unit_value) = self.unit_value {
                    unit_value
                } else {
                    let unit_value = self.values.alloc(value);
                    self.unit_value = Some(unit_value);
                    unit_value
                }
            }

            Value::Local(ref local) => {
                let is_user_defined = !local.is_tmp;
                let value_id = self.values.alloc(value);
                if is_user_defined {
                    self.locals.push(value_id);
                }
                value_id
            }

            _ => self.values.alloc(value),
        }
    }

    pub fn is_terminator(&self, inst: InstId) -> bool {
        self.inst_data(inst).is_terminator()
    }

    pub fn branch_info(&self, inst: InstId) -> BranchInfo {
        self.inst_data(inst).branch_info()
    }

    pub fn value_data(&self, value: ValueId) -> &Value {
        &self.values[value]
    }

    pub fn store_block(&mut self, block: BasicBlock) -> BasicBlockId {
        self.blocks.alloc(block)
    }

    /// Returns an instruction result. A returned value is guaranteed to be a
    /// temporary value.
    pub fn inst_result(&self, inst: InstId) -> Option<ValueId> {
        self.inst_results.get(&inst).copied()
    }

    pub fn value_ty(&self, vid: ValueId) -> TypeId {
        self.values[vid].ty()
    }

    pub fn replace_inst(&mut self, inst: InstId, new: Inst) {
        self.insts[inst] = new;
    }

    pub fn map_result(&mut self, inst: InstId, result: ValueId) {
        self.inst_results.insert(inst, result);
    }

    pub fn locals(&self) -> &[ValueId] {
        &self.locals
    }

    /// Returns Some(`local_name`) if value is `Value::Local`.
    pub fn local_name(&self, value: ValueId) -> Option<&str> {
        match self.value_data(value) {
            Value::Local(Local { name, .. }) => Some(name),
            _ => None,
        }
    }

    fn store_immediate(&mut self, imm: Immediate) -> ValueId {
        if let Some(value) = self.immediates.get(&imm) {
            *value
        } else {
            let id = self.values.alloc(Value::Immediate(imm.clone()));
            self.immediates.insert(imm, id);
            id
        }
    }
}
