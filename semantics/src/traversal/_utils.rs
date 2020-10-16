use fe_parser::ast as fe;
use fe_parser::span::{
  Span,
  Spanned,
};


/// Creates a new spanned expression. Useful in cases where an `Expr` is nested
/// within the node of a `Spanned` object.
pub fn spanned_expression<'a>(span: &Span, exp: &fe::Expr<'a>) -> Spanned<fe::Expr<'a>> {
  Spanned {
      node: (*exp).clone(),
      span: (*span).to_owned(),
  }
}