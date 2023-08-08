use parser::ast::{self, prelude::*};

use crate::hir_def::{Body, GenericArgListId, Partial, PathId, TraitRef, TypeId, TypeKind};

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

            ast::TypeKind::SelfType(_) => TypeKind::SelfType,

            ast::TypeKind::Tuple(ty) => {
                let mut elem_tys = Vec::new();
                for elem in ty {
                    elem_tys.push(Some(TypeId::lower_ast(ctxt, elem)).into());
                }
                TypeKind::Tuple(elem_tys)
            }

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

impl TraitRef {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::PathType) -> Self {
        let path = PathId::lower_ast_partial(ctxt, ast.path());
        let generic_args = GenericArgListId::lower_ast_opt(ctxt, ast.generic_args());
        Self { path, generic_args }
    }

    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'_>,
        ast: Option<ast::PathType>,
    ) -> Partial<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }
}
