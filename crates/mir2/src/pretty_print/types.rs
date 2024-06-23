use std::fmt::{self, Write};

use hir::hir_def::TypeId;

use crate::{ir::function::BodyDataStore, MirDb};

use super::PrettyPrint;

impl PrettyPrint for TypeId {
    fn pretty_print<W: Write>(
        &self,
        db: &dyn MirDb,
        _store: &BodyDataStore,
        w: &mut W,
    ) -> fmt::Result {
        self.print(db, w)
    }
}
