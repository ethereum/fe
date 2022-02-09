use fe_common::Span;
use fe_parser::node::NodeId;

pub mod basic_block;
pub mod body_order;
pub mod constant;
pub mod function;
pub mod inst;
pub mod module;
pub mod types;
pub mod value;

/// An original source information that indicates where `mir` entities derive
/// from. `SourceInfo` is mainly used for diagnostics.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceInfo {
    pub span: Span,
    pub id: NodeId,
}
