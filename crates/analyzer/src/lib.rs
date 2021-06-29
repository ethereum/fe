//! Fe semantic analysis.
//!
//! This library is used to analyze the semantics of a given Fe AST. It detects
//! any semantic errors within a given AST and produces a `Context` instance
//! that can be used to query contextual information attributed to AST nodes.

pub mod builtins;
pub mod constants;
pub mod context;
pub mod errors;
pub mod namespace;
mod operations;
mod traversal;

use crate::errors::{AnalyzerError, FatalError};
use context::Context;
use fe_common::files::SourceFileId;
use fe_parser::ast as fe;

/// Performs semantic analysis of the source program and returns a `Context`
/// instance.
pub fn analyze(module: &fe::Module, file_id: SourceFileId) -> Result<Context, AnalyzerError> {
    let mut context = Context::new(file_id);
    let result = traversal::module::module(&mut context, module);

    match result {
        Ok(()) => {
            if context.diagnostics.is_empty() {
                Ok(context)
            } else {
                Err(AnalyzerError(context.diagnostics))
            }
        }
        Err(FatalError) => {
            if context.diagnostics.is_empty() {
                panic!("Expected at least one error")
            }
            Err(AnalyzerError(context.diagnostics))
        }
    }
}

#[cfg(feature = "fix-context-harness")]
pub mod test_utils {
    use crate::namespace::types::FixedSize;
    use crate::{Context, ExpressionAttributes};
    use fe_parser::ast as fe;
    use fe_parser::node::{Node, Span};

    pub struct ContextHarness {
        pub context: Context,
        pub src: String,
    }

    impl ContextHarness {
        pub fn new(src: &str) -> Self {
            ContextHarness {
                context: Context::new(),
                src: src.to_string(),
            }
        }

        fn find_span(&self, substr: &str) -> Span {
            let start = self
                .src
                .find(substr)
                .unwrap_or_else(|| panic!("unable to find '{}' in '{}'", substr, self.src));

            Span {
                start,
                end: start + substr.len(),
            }
        }

        pub fn add_expression(&mut self, substr: &str, attributes: ExpressionAttributes) {
            let span = self.find_span(substr);
            let mock_node = Node::new(fe::Expr::Name("foo"), span);
            self.context.add_expression(&mock_node, attributes)
        }

        pub fn add_expressions(&mut self, substrs: Vec<&str>, attributes: ExpressionAttributes) {
            for substr in substrs {
                self.add_expression(substr, attributes.clone())
            }
        }

        pub fn add_declaration(&mut self, substr: &str, typ: FixedSize) {
            let span = self.find_span(substr);
            let mock_node = Node::new(
                fe::FuncStmt::Expr {
                    value: fe::Expr::Name("foo"),
                },
                span,
            );
            self.context.add_declaration(&mock_node, typ)
        }
    }
}
