use crate::hir_def::Partial;

use super::IdentId;

#[salsa::interned]
pub struct PathId {
    #[return_ref]
    pub segments: Vec<Partial<IdentId>>,
}
