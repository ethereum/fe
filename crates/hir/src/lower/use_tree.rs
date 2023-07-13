use parser::ast::{self, prelude::*};

use crate::{
    hir_def::{kw, use_tree::*, IdentId, ItemModifier, Partial, TrackedItemId, Use},
    span::{HirOrigin, UseDesugared},
};

use super::FileLowerCtxt;

impl Use {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Use) -> Vec<Self> {
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();

        let Some(use_tree) = ast.use_tree() else {
            let id = ctxt.joined_id(TrackedItemId::Use(Partial::Absent));
            ctxt.enter_scope(id.clone(), false);
            let path = Partial::Absent;
            let alias = None;
            let top_mod = ctxt.top_mod();
            let origin = HirOrigin::raw(&ast);
            let use_ = Self::new(ctxt.db(), id, path, alias, vis, top_mod, origin);
            ctxt.leave_item_scope(use_);
            return vec![use_];
        };

        // If the use tree has no subtree, then there is no need to decompose it.
        if !use_tree.has_subtree() {
            let path = UsePathId::lower_ast_partial(ctxt, use_tree.path());
            let id = ctxt.joined_id(TrackedItemId::Use(path));
            ctxt.enter_scope(id.clone(), false);
            let alias = use_tree
                .alias()
                .map(|alias| UseAlias::lower_ast_partial(ctxt, alias));
            let top_mod = ctxt.top_mod();
            let origin = HirOrigin::raw(&ast);
            let use_ = Self::new(ctxt.db(), id, path, alias, vis, top_mod, origin);
            ctxt.leave_item_scope(use_);
            return vec![use_];
        }

        let decomposed_paths = decompose_tree(ctxt, ast, use_tree);
        decomposed_paths
            .into_iter()
            .map(|(path, alias, origin)| {
                let id = ctxt.joined_id(TrackedItemId::Use(path));
                ctxt.enter_scope(id.clone(), false);
                let top_mod = ctxt.top_mod();
                let alias = alias;
                let origin = HirOrigin::desugared(origin);
                let use_ = Self::new(ctxt.db(), id, path, alias, vis, top_mod, origin);
                ctxt.leave_item_scope(use_)
            })
            .collect()
    }
}

impl UsePathId {
    fn lower_ast_partial(ctxt: &mut FileLowerCtxt<'_>, ast: Option<ast::UsePath>) -> Partial<Self> {
        let Some(ast) = ast else {
            return Partial::Absent;
        };

        let segments = ast
            .into_iter()
            .map(|ast| UsePathSegment::lower_ast_partial(ctxt, ast))
            .collect();
        Some(Self::new(ctxt.db(), segments)).into()
    }

    fn from_segments(
        ctxt: &mut FileLowerCtxt<'_>,
        ast_segs: Vec<ast::UsePathSegment>,
    ) -> Partial<Self> {
        if ast_segs.is_empty() {
            Partial::Absent
        } else {
            let segs = ast_segs
                .into_iter()
                .map(|seg| UsePathSegment::lower_ast_partial(ctxt, seg))
                .collect();
            Partial::Present(Self::new(ctxt.db(), segs))
        }
    }
}

impl UsePathSegment {
    fn lower_ast_partial(ctxt: &mut FileLowerCtxt<'_>, ast: ast::UsePathSegment) -> Partial<Self> {
        ast.kind()
            .map(|kind| match kind {
                ast::UsePathSegmentKind::Ingot(_) => Self::Ident(kw::INGOT),
                ast::UsePathSegmentKind::Super(_) => Self::Ident(kw::SUPER),
                ast::UsePathSegmentKind::Ident(ident) => {
                    Self::Ident(IdentId::lower_token(ctxt, ident))
                }
                ast::UsePathSegmentKind::Self_(_) => Self::Ident(kw::SELF),
                ast::UsePathSegmentKind::Glob(_) => Self::Glob,
            })
            .into()
    }
}

impl UseAlias {
    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: ast::UseAlias,
    ) -> Partial<Self> {
        if let Some(ident) = ast.ident() {
            Some(Self::Ident(IdentId::lower_token(ctxt, ident)))
        } else if ast.underscore().is_some() {
            Some(Self::Underscore)
        } else {
            None
        }
        .into()
    }
}

fn decompose_tree(
    ctxt: &mut FileLowerCtxt<'_>,
    ast: ast::Use,
    use_tree: ast::UseTree,
) -> Vec<(Partial<UsePathId>, Option<Partial<UseAlias>>, UseDesugared)> {
    let use_desugared = UseDesugared::new(&ast);
    decompose_subtree(ctxt, use_tree, (vec![], use_desugared))
        .into_iter()
        .map(|(ast_segs, alias, desugared)| {
            let path = UsePathId::from_segments(ctxt, ast_segs);
            (path, alias, desugared)
        })
        .collect()
}

fn decompose_subtree(
    ctxt: &mut FileLowerCtxt,
    subtree: ast::UseTree,
    succ: (Vec<ast::UsePathSegment>, UseDesugared),
) -> Vec<(
    Vec<ast::UsePathSegment>,
    Option<Partial<UseAlias>>,
    UseDesugared,
)> {
    let (mut succ_path, mut succ_desugared) = succ;
    if let Some(path) = subtree.path() {
        for seg in path {
            succ_desugared.push_seg(&seg);
            succ_path.push(seg.clone());
        }
    }

    if let Some(alias) = subtree.alias() {
        succ_desugared.add_alias(&alias);
        let alias = UseAlias::lower_ast_partial(ctxt, alias);
        assert!(subtree.children().is_none());
        return vec![(succ_path, Some(alias), succ_desugared)];
    }

    let Some(children) = subtree.children() else {
        return vec![(succ_path, None, succ_desugared)];
    };

    children
        .into_iter()
        .flat_map(|subtree| {
            decompose_subtree(ctxt, subtree, (succ_path.clone(), succ_desugared.clone()))
        })
        .collect()
}
