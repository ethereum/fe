use common::InputFile;
use parser::GreenNode;
use salsa::Accumulator;

use crate::{hir_def::TopLevelMod, HirDb};

#[salsa::tracked]
pub fn parse_file_impl<'db>(db: &'db dyn HirDb, top_mod: TopLevelMod<'db>) -> GreenNode {
    let file = top_mod.file(db);
    let text = file.text(db.as_input_db());
    let (node, parse_errors) = parser::parse_source_file(text);

    for error in parse_errors {
        ParseErrorAccumulator::push(db, ParserError { file, error });
    }
    node
}

// xxx remove
#[doc(hidden)]
#[salsa::accumulator]
pub struct ParseErrorAccumulator(pub ParserError);

impl ParseErrorAccumulator {
    fn push(db: &dyn HirDb, err: ParserError) {
        Self(err).accumulate(db);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]

pub struct ParserError {
    pub file: InputFile,
    pub error: parser::ParseError,
}
