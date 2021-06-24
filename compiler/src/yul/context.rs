use fe_analyzer::context::Context as AnalyzerContext;
use indexmap::IndexSet;

// This is contract context, but it's used all over so it has a short name.
pub struct Context<'a> {
    pub analysis: &'a AnalyzerContext,

    /// String literals used in the contrat
    pub string_literals: IndexSet<String>,
}

impl<'a> Context<'a> {
    pub fn new(analysis: &'a AnalyzerContext) -> Self {
        Self {
            analysis,
            string_literals: IndexSet::new(),
        }
    }
}
