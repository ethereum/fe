use fe_common::Span;
use fe_parser::node::NodeId;

pub mod basic_block;
pub mod body_builder;
pub mod body_cursor;
pub mod body_order;
pub mod constant;
pub mod function;
pub mod inst;
pub mod module;
pub mod types;
pub mod value;

pub use basic_block::{BasicBlock, BasicBlockId};
pub use constant::{Constant, ConstantId};
pub use function::{FunctionBody, FunctionId, FunctionParam, FunctionSignature};
pub use inst::{Inst, InstId};
pub use module::{Module, ModuleId};
pub use types::{Type, TypeId};
pub use value::{Value, ValueId};

/// An original source information that indicates where `mir` entities derive
/// from. `SourceInfo` is mainly used for diagnostics.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceInfo {
    pub span: Span,
    pub id: NodeId,
}
