//! The algorithm to build efficient decision tree is based on [Compiling pattern matching to good decision trees](https://dl.acm.org/doi/10.1145/1411304.1411311)

use fe_analyzer::pattern_analysis::PatternMatrix;
use fe_parser::{
    ast::{Expr, MatchArm},
    node::Node,
};

use self::decision_tree::ColumnSelectionPolicy;

use super::function::BodyLowerHelper;

pub mod decision_tree;
mod tree_vis;

pub(super) fn lower_match<'db, 'a>(
    helper: &mut BodyLowerHelper<'db, 'a>,
    mat: &PatternMatrix,
    scrutinee: &Node<Expr>,
    _arms: &[Node<MatchArm>],
) {
    let _scrutinee = helper.lower_expr_to_value(scrutinee);
    let mut policy = ColumnSelectionPolicy::default();
    // PBA heuristics described in the paper.
    policy.needed_prefix().small_branching().arity();

    let decision_tree = decision_tree::build_decision_tree(helper.db().upcast(), mat, policy);

    let mut tree_dot = vec![];
    decision_tree
        .dump_dot(helper.db().upcast(), &mut tree_dot)
        .unwrap();
    println!("{}", String::from_utf8(tree_dot).unwrap());

    todo!()
}
