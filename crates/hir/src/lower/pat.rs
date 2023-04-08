use parser::ast;

use crate::{
    hir_def::{pat::*, IdentId, LitKind, PathId},
    span::HirOrigin,
};

use super::body::BodyCtxt;

impl Pat {
    pub(super) fn lower_ast(ctxt: &mut BodyCtxt<'_, '_>, ast: ast::Pat) -> PatId {
        let pat = match &ast.kind() {
            ast::PatKind::WildCard(_) => Pat::WildCard,

            ast::PatKind::Rest(_) => Pat::Rest,

            ast::PatKind::Lit(lit_pat) => {
                let lit_kind = lit_pat
                    .lit()
                    .map(|lit| LitKind::lower_ast(ctxt.f_ctxt, lit))
                    .into();
                Pat::Lit(lit_kind)
            }

            ast::PatKind::Tuple(tup) => {
                let elems = match tup.elems() {
                    Some(elems) => elems.iter().map(|pat| Pat::lower_ast(ctxt, pat)).collect(),
                    None => vec![],
                };
                Pat::Tuple(elems)
            }

            ast::PatKind::Path(path) => {
                let path = PathId::lower_ast_partial(ctxt.f_ctxt, path.path());
                Pat::Path(path)
            }

            ast::PatKind::PathTuple(path_tup) => {
                let path = PathId::lower_ast_partial(ctxt.f_ctxt, path_tup.path());
                let elems = match path_tup.elems() {
                    Some(elems) => elems.iter().map(|pat| Pat::lower_ast(ctxt, pat)).collect(),
                    None => vec![],
                };
                Pat::PathTuple(path, elems)
            }

            ast::PatKind::Record(record) => {
                let path = PathId::lower_ast_partial(ctxt.f_ctxt, record.path());
                let fields = match record.fields() {
                    Some(fields) => fields
                        .iter()
                        .map(|f| RecordPatField::lower_ast(ctxt, &f))
                        .collect(),
                    None => vec![],
                };
                Pat::Record(path, fields)
            }

            ast::PatKind::Or(or) => {
                let lhs = Self::lower_ast_opt(ctxt, or.lhs());
                let rhs = Self::lower_ast_opt(ctxt, or.rhs());
                Pat::Or(lhs, rhs)
            }
        };

        ctxt.push_pat(pat, HirOrigin::raw(&ast))
    }

    pub(super) fn lower_ast_opt(ctxt: &mut BodyCtxt<'_, '_>, ast: Option<ast::Pat>) -> PatId {
        if let Some(ast) = ast {
            Pat::lower_ast(ctxt, ast)
        } else {
            ctxt.push_missing_pat()
        }
    }
}

impl RecordPatField {
    fn lower_ast(ctxt: &mut BodyCtxt<'_, '_>, ast: &ast::RecordPatField) -> RecordPatField {
        let label = IdentId::lower_token_partial(ctxt.f_ctxt, ast.name());
        let pat = ast
            .pat()
            .map(|pat| Pat::lower_ast(ctxt, pat))
            .unwrap_or_else(|| ctxt.push_missing_pat());
        RecordPatField { label, pat }
    }
}
