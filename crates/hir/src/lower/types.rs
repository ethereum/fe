use parser::ast::{self, prelude::*};

use crate::hir_def::{
    Body, GenericArgListId, Partial, PathId, TraitRef, TupleTypeId, TypeId, TypeKind,
};

use super::FileLowerCtxt;

impl TypeId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Type) -> Self {
        let kind = match ast.kind() {
            ast::TypeKind::Ptr(ty) => {
                let inner = Self::lower_ast_partial(ctxt, ty.inner());
                TypeKind::Ptr(inner)
            }

            ast::TypeKind::Path(ty) => {
                let path = PathId::lower_ast_partial(ctxt, ty.path());
                let generic_args = GenericArgListId::lower_ast_opt(ctxt, ty.generic_args());
                TypeKind::Path(path, generic_args)
            }

            ast::TypeKind::SelfType(ty) => {
                let generic_args = GenericArgListId::lower_ast_opt(ctxt, ty.generic_args());
                TypeKind::SelfType(generic_args)
            }

            ast::TypeKind::Tuple(ty) => TypeKind::Tuple(TupleTypeId::lower_ast(ctxt, ty)),

            ast::TypeKind::Array(ty) => {
                let elem_ty = Self::lower_ast_partial(ctxt, ty.elem_ty());
                let body = ty
                    .len()
                    .map(|ast| Body::lower_ast_nameless(ctxt, ast))
                    .into();
                TypeKind::Array(elem_ty, body)
            }
        };

        TypeId::new(ctxt.db(), kind)
    }

    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: Option<ast::Type>,
    ) -> Partial<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }
}

impl TupleTypeId {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::TupleType) -> Self {
        let mut elem_tys = Vec::new();
        for elem in ast {
            elem_tys.push(Some(TypeId::lower_ast(ctxt, elem)).into());
        }
        TupleTypeId::new(ctxt.db(), elem_tys)
    }
}

impl TraitRef {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::TraitRef) -> Self {
        let path = ast.path().map(|ast| PathId::lower_ast(ctxt, ast)).into();
        let generic_args = ast
            .generic_args()
            .map(|args| GenericArgListId::lower_ast(ctxt, args));
        Self { path, generic_args }
    }

    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: Option<ast::TraitRef>,
    ) -> Partial<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }
}
