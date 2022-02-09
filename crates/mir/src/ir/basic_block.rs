use id_arena::Id;

use super::SourceInfo;

pub type BasicBlockId = Id<BasicBlock>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BasicBlock {
    source: SourceInfo,
}
