pub mod analysis;
pub mod graphviz;
pub mod ir;
pub mod pretty_print;

mod lower;

#[salsa::jar(db = MirDb)]
pub struct Jar(
    ir::BasicBlock,
    ir::BasicBlockId,
    ir::Constant,
    ir::ConstantId,
    ir::FunctionBody,
    ir::FunctionId,
    ir::FunctionParam,
    ir::FunctionSignature,
    ir::Inst,
    ir::InstId,
    ir::Type,
    ir::TypeId,
    ir::TypeKind,
    ir::Value,
    ir::ValueId,
);
