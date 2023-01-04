use std::fmt::Write;

use fe_common::Span;
use fe_parser::{ast::MatchArm, node::Node, Label};

use crate::{
    context::AnalyzerContext,
    display::Displayable,
    errors::FatalError,
    namespace::{scopes::BlockScope, types::TypeId},
    AnalyzerDb,
};

use super::pattern_analysis::{PatternMatrix, SimplifiedPattern};

pub(super) fn check_match_exhaustiveness(
    scope: &mut BlockScope,
    arms: &[Node<MatchArm>],
    match_span: Span,
    ty: TypeId,
) -> Result<(), FatalError> {
    if arms.is_empty() {
        let err = scope.fancy_error(
            "patterns is not exhaustive",
            vec![Label::primary(
                match_span,
                "expected at least one match arm here",
            )],
            vec![],
        );
        return Err(FatalError::new(err));
    }

    let pattern_matrix = PatternMatrix::from_arms(scope, arms, ty);
    match pattern_matrix.find_non_exhaustiveness(scope.db()) {
        Some(pats) => {
            let err = scope.fancy_error(
                "patterns is not exhaustive",
                vec![Label::primary(
                    match_span,
                    format! {"`{}` not covered", display_non_exhaustive_patterns(scope.db(), &pats)},
                )],
                vec![],
            );
            Err(FatalError::new(err))
        }
        None => Ok(()),
    }
}

pub(super) fn check_unreachable_pattern(
    scope: &mut BlockScope,
    arms: &[Node<MatchArm>],
    match_span: Span,
    ty: TypeId,
) -> Result<(), FatalError> {
    if arms.is_empty() {
        let err = scope.fancy_error(
            "patterns is not exhaustive",
            vec![Label::primary(
                match_span,
                "expected at least one match arm here",
            )],
            vec![],
        );
        return Err(FatalError::new(err));
    }

    let pattern_matrix = PatternMatrix::from_arms(scope, arms, ty);
    let mut res = Ok(());
    for (i, arms) in arms.iter().enumerate() {
        if !pattern_matrix.is_row_useful(scope.db(), i) {
            let err = scope.fancy_error(
                "unreachable pattern ",
                vec![Label::primary(
                    arms.kind.pat.span,
                    "this arm is unreachable",
                )],
                vec![],
            );
            res = Err(FatalError::new(err));
        }
    }
    res
}

fn display_non_exhaustive_patterns(db: &dyn AnalyzerDb, pats: &[SimplifiedPattern]) -> String {
    if pats.len() == 1 {
        format!("{}", pats[0].display(db))
    } else {
        let mut s = "(".to_string();
        let mut delim = "";
        for pat in pats {
            let pat = pat.display(db);
            write!(s, "{delim}{pat}").unwrap();
            delim = ", ";
        }
        s.push(')');
        s
    }
}
