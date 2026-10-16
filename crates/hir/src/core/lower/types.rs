use parser::ast::{self};

use super::FileLowerCtxt;
use crate::core::hir_def::{
    AssocTypeGenericArg, Body, GenericArg, GenericArgListId, IdentId, Partial, PathId, TraitRefId,
    TupleTypeId, TypeGenericArg, TypeId, TypeKind, TypeMode,
    params::{FuncParamMode, callable_shape_name},
};

impl<'db> TypeId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Type) -> Self {
        let kind = match ast.kind() {
            ast::TypeKind::Ptr(ty) => {
                let inner = Self::lower_ast_partial(ctxt, ty.inner());
                if ty.star2().is_some() {
                    let inner = Partial::Present(TypeId::new(ctxt.db(), TypeKind::Ptr(inner)));
                    TypeKind::Ptr(inner)
                } else {
                    TypeKind::Ptr(inner)
                }
            }

            ast::TypeKind::Mode(ty) => {
                let mode = match ty.mode() {
                    Some(ast::TypeMode::Mut(_)) => TypeMode::Mut,
                    Some(ast::TypeMode::Ref(_)) => TypeMode::Ref,
                    Some(ast::TypeMode::Own(_)) => TypeMode::Own,
                    None => TypeMode::Ref,
                };
                let inner = Self::lower_ast_partial(ctxt, ty.inner());
                TypeKind::Mode(mode, inner)
            }

            ast::TypeKind::Path(ty) => {
                let path = PathId::lower_ast_partial(ctxt, ty.path());
                TypeKind::Path(path)
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

            ast::TypeKind::Never(_) => TypeKind::Never,
        };

        TypeId::new(ctxt.db(), kind)
    }

    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::Type>,
    ) -> Partial<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }
}

impl<'db> TupleTypeId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TupleType) -> Self {
        let mut elem_tys = Vec::new();
        for elem in ast {
            elem_tys.push(Some(TypeId::lower_ast(ctxt, elem)).into());
        }
        TupleTypeId::new(ctxt.db(), elem_tys)
    }
}

impl<'db> TraitRefId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitRef) -> Self {
        let path = match ast.fn_shape() {
            // `Fn(own A, B) -> R` names the core trait of its shape,
            // `Fn_ov<A, B, Out = R>`; `Fn(own T) -> U` is `Fn<T, U>`.
            Some(shape) => shape.name().zip(shape.params()).map(|(name, params)| {
                let db = ctxt.db();
                let mut modes = Vec::new();
                let mut args = Vec::new();
                for param in params {
                    let ty = TypeId::lower_ast(ctxt, param);
                    let (mode, ty) = match ty.data(db) {
                        TypeKind::Mode(TypeMode::Own, inner) => (FuncParamMode::Own, *inner),
                        TypeKind::Mode(TypeMode::Mut, inner) => (FuncParamMode::Mut, *inner),
                        TypeKind::Mode(TypeMode::Ref, inner) => (FuncParamMode::View, *inner),
                        _ => (FuncParamMode::View, Partial::Present(ty)),
                    };
                    modes.push(mode);
                    args.push(GenericArg::Type(TypeGenericArg { ty }));
                }
                let ret = Partial::Present(shape.ret_ty().map_or_else(
                    || TupleTypeId::new(db, Vec::new()).to_ty(db),
                    |ty| TypeId::lower_ast(ctxt, ty),
                ));
                let trait_name = callable_shape_name(name.text() == "FnMut", &modes);
                args.push(if trait_name == "Fn" {
                    GenericArg::Type(TypeGenericArg { ty: ret })
                } else {
                    GenericArg::AssocType(AssocTypeGenericArg {
                        name: Partial::Present(IdentId::new(db, "Out".to_string())),
                        ty: ret,
                    })
                });
                ctxt.core_path().push_str(db, "functional").push_str_args(
                    db,
                    &trait_name,
                    GenericArgListId::new(db, args, true),
                )
            }),
            None => ast.path().map(|ast| PathId::lower_ast(ctxt, ast)),
        };
        Self::new(ctxt.db(), Partial::from(path))
    }

    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::TraitRef>,
    ) -> Partial<Self> {
        ast.map(|ast| Self::lower_ast(ctxt, ast)).into()
    }
}
