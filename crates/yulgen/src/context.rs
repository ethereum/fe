use crate::AnalyzerContext;
use fe_analyzer::namespace::types::{FeString, Struct};
use indexmap::IndexSet;

// This is contract context, but it's used all over so it has a short name.
pub struct Context<'a> {
    pub analysis: &'a AnalyzerContext,

    /// String literals used in the contract
    pub string_literals: IndexSet<String>,

    /// Names of contracts that have been created inside of this contract.
    pub created_contracts: IndexSet<String>,

    /// Strings that can be used as revert error in assertions
    pub assert_strings: IndexSet<FeString>,

    // Structs that can be used as errors in revert statements
    pub revert_errors: IndexSet<Struct>,
}

impl<'a> Context<'a> {
    pub fn new(analysis: &'a AnalyzerContext) -> Self {
        Self {
            analysis,
            string_literals: IndexSet::new(),
            created_contracts: IndexSet::new(),
            assert_strings: IndexSet::new(),
            revert_errors: IndexSet::new(),
        }
    }
}
