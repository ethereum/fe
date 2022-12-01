pub mod functions;
pub mod pattern_analysis;
pub mod pragma;
pub mod types;

pub(crate) mod const_expr;
pub(crate) mod expressions;

mod assignments;
mod borrowck;
mod call_args;
mod declarations;
mod matching_anomaly;
mod utils;
