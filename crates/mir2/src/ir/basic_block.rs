#[salsa::interned]
pub struct BasicBlockId {
    #[return_ref]
    pub data: BasicBlock,
}

#[salsa::tracked]
pub struct BasicBlock {}
