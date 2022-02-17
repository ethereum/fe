use std::io::{self, Write};

use crate::{db::MirDb, ir::function::BodyDataStore};

mod inst;
mod types;
mod value;

pub trait PrettyPrint {
    fn pretty_print<W: Write>(
        &self,
        db: &dyn MirDb,
        store: &BodyDataStore,
        w: &mut W,
    ) -> io::Result<()>;
}
