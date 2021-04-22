//! Errors returned by the compilers and ABI builder.

use fe_analyzer::errors::SemanticError;
use fe_common::diagnostics::Diagnostic;
use once_cell::sync::Lazy;
use std::borrow::Cow;
use std::panic;

const BUG_REPORT_URL: &str = "https://github.com/ethereum/fe/issues/new";
static DEFAULT_PANIC_HOOK: Lazy<Box<dyn Fn(&panic::PanicInfo<'_>) + Sync + Send + 'static>> =
    Lazy::new(|| {
        let hook = panic::take_hook();
        panic::set_hook(Box::new(|info| report_ice(info)));
        hook
    });

pub fn install_compiler_panic_hook() {
    Lazy::force(&DEFAULT_PANIC_HOOK);
}

/// Errors can either be an object or static reference.
#[derive(Debug)]
pub enum ErrorKind {
    Str(Cow<'static, str>),
    Analyzer(SemanticError),
    Parser(Vec<Diagnostic>),
}

/// List of errors encountered during compilation.
#[derive(Debug)]
pub struct CompileError {
    pub errors: Vec<ErrorKind>,
}

impl Default for CompileError {
    fn default() -> Self {
        Self::new()
    }
}

impl CompileError {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    /// Create a single error with a static string.
    pub fn static_str(val: &'static str) -> Self {
        Self {
            errors: vec![ErrorKind::Str(val.into())],
        }
    }

    /// Create a single error with a string object.
    pub fn str(val: &str) -> Self {
        Self {
            errors: vec![ErrorKind::Str(val.to_string().into())],
        }
    }

    pub fn analyzer(err: SemanticError) -> Self {
        Self {
            errors: vec![ErrorKind::Analyzer(err)],
        }
    }
}

impl<'a> From<serde_json::error::Error> for CompileError {
    fn from(_: serde_json::error::Error) -> Self {
        CompileError::static_str("JSON serialization error")
    }
}

impl<'a> From<ethabi::Error> for CompileError {
    fn from(e: ethabi::Error) -> Self {
        CompileError::str(&format!("ethabi error: {}", e))
    }
}

impl<'a> From<SemanticError> for CompileError {
    fn from(e: SemanticError) -> Self {
        CompileError::analyzer(e)
    }
}

fn report_ice(info: &panic::PanicInfo) {
    (*DEFAULT_PANIC_HOOK)(info);

    eprintln!();
    eprintln!("You've hit an internal compiler error. This is a bug in the Fe compiler.");
    eprintln!("Fe is still under heavy development, and isn't yet ready for production use.");
    eprintln!();
    eprintln!("If you would, please report this bug at the following URL:");
    eprintln!("  {}", BUG_REPORT_URL);
}
