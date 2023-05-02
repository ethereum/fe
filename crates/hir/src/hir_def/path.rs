use crate::hir_def::Partial;

use super::IdentId;

#[salsa::interned]
pub struct PathId {
    pub segments: Vec<Partial<IdentId>>,
}
