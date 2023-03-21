use fe_parser2::ast;

use crate::{
    hir_def::{pat::*, IdentId, LitKind, PathId},
    span::HirOriginKind,
};

use super::body::BodyCtxt;

impl Pat {
    pub(super) fn push_to_body(ctxt: &mut BodyCtxt<'_>, ast: ast::Pat) -> PatId {
        let pat = match &ast.kind() {
            ast::PatKind::WildCard(_) => Pat::WildCard,

            ast::PatKind::Rest(_) => Pat::Rest,

            ast::PatKind::Lit(lit_pat) => {
                let lit_kind = lit_pat
                    .lit()
                    .map(|lit| LitKind::from_ast(ctxt.db, lit))
                    .into();
                Pat::Lit(lit_kind)
            }

            ast::PatKind::Tuple(tup) => {
                let elems = match tup.elems() {
                    Some(elems) => elems
                        .iter()
                        .map(|pat| Pat::push_to_body(ctxt, pat))
                        .collect(),
                    None => vec![],
                };
                Pat::Tuple(elems)
            }

            ast::PatKind::Path(path) => {
                let path = PathId::maybe_from_ast(ctxt.db, path.path());
                Pat::Path(path)
            }

            ast::PatKind::PathTuple(path_tup) => {
                let path = PathId::maybe_from_ast(ctxt.db, path_tup.path());
                let elems = match path_tup.elems() {
                    Some(elems) => elems
                        .iter()
                        .map(|pat| Pat::push_to_body(ctxt, pat))
                        .collect(),
                    None => vec![],
                };
                Pat::PathTuple(path, elems)
            }

            ast::PatKind::Record(record) => {
                let path = PathId::maybe_from_ast(ctxt.db, record.path());
                let fields = match record.fields() {
                    Some(fields) => fields
                        .iter()
                        .map(|f| RecordPatField::from_ast(ctxt, &f))
                        .collect(),
                    None => vec![],
                };
                Pat::Record(path, fields)
            }

            ast::PatKind::Or(or) => {
                let lhs = Self::push_to_body_opt(ctxt, or.lhs());
                let rhs = Self::push_to_body_opt(ctxt, or.rhs());
                Pat::Or(lhs, rhs)
            }
        };

        ctxt.push_pat(pat, HirOriginKind::raw(&ast))
    }

    pub(super) fn push_to_body_opt(ctxt: &mut BodyCtxt<'_>, ast: Option<ast::Pat>) -> PatId {
        if let Some(ast) = ast {
            Pat::push_to_body(ctxt, ast)
        } else {
            ctxt.push_missing_pat()
        }
    }
}

impl RecordPatField {
    fn from_ast(ctxt: &mut BodyCtxt<'_>, ast: &ast::RecordPatField) -> RecordPatField {
        let label = IdentId::maybe_from_token(ctxt.db, ast.name());
        let pat = ast
            .pat()
            .map(|pat| Pat::push_to_body(ctxt, pat))
            .unwrap_or_else(|| ctxt.push_missing_pat());
        RecordPatField { label, pat }
    }
}
