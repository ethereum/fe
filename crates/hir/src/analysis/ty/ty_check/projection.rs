//! Projection bodies and access uses.
//!
//! A projection's yield sites must grant the accesses its return shape
//! promises. A yield site is the body's tail or a returned expression; blocks,
//! `if`, `match` and `with` yield through their tails. Each yield site's shape
//! is recorded for lowering.
use super::{ExprProp, TyChecker, ValuePathRef, expr::IndexMutOp};
use crate::{
    analysis::ty::{diagnostics::BodyDiag, shape::Shape, ty_def::BorrowKind},
    hir_def::{BinOp, CallableDef, Expr, ExprId, Partial, Stmt, UnOp},
};

impl<'db> TyChecker<'db> {
    pub(super) fn check_yield_site(&mut self, expr: ExprId, shape: &Shape<'db>) {
        let Some(prop) = self.env.typed_expr(expr) else {
            return;
        };
        if prop.ty.has_invalid(self.db) {
            return;
        }
        self.env.record_yield_shape(expr, shape.clone());
        // A path that diverges yields nothing.
        let diverges = prop.ty.is_never(self.db)
            || self
                .env
                .callable_expr(expr)
                .is_some_and(|callable| callable.ret_ty(self.db).is_never(self.db));
        let Partial::Present(data) = expr.data(self.db, self.body()) else {
            return;
        };
        if diverges {
            return;
        }
        match data {
            Expr::Block(stmts, _) => {
                if let Some(Partial::Present(Stmt::Expr(tail))) =
                    stmts.last().map(|stmt| stmt.data(self.db, self.body()))
                {
                    self.check_yield_site(*tail, shape);
                }
            }
            Expr::If(_, then, Some(else_)) => {
                self.check_yield_site(*then, shape);
                self.check_yield_site(*else_, shape);
            }
            Expr::Match(_, Partial::Present(arms)) => {
                for arm in arms {
                    self.check_yield_site(arm.body, shape);
                }
            }
            Expr::With(_, body) => self.check_yield_site(*body, shape),
            _ => return self.check_yield_leaf(expr, data, shape),
        }
        // It yields what its tails do.
        self.env.consume_access(expr);
    }

    fn check_yield_leaf(&mut self, expr: ExprId, data: &Expr<'db>, shape: &Shape<'db>) {
        let prop = self.env.typed_expr(expr).expect("yield site is typed");
        // A projection call or `ref p`/`mut p` grants its own accesses.
        if let Some(given) = &prop.shape {
            if given.grants(shape) {
                self.consume_access(expr);
            } else {
                self.invalid_yield(expr, shape);
            }
            return;
        }
        match shape {
            Shape::Owned(ty) => self.record_implicit_move_for_owned_expr(expr, *ty),
            Shape::Access(kind, _) => {
                let binding_access = prop
                    .binding
                    .filter(|_| matches!(data, Expr::Path(..)))
                    .and_then(|binding| self.env.binding_access(&binding));
                match (kind, binding_access) {
                    // Re-yield an access binding or parameter.
                    (BorrowKind::Mut, Some(access)) if access.is_mut() => {}
                    (BorrowKind::Ref, Some(_)) => {}
                    // A view of a place, or of a value held by the session.
                    (BorrowKind::Ref, None) => {}
                    _ => self.invalid_yield(expr, shape),
                }
            }
            Shape::Tuple(elems) => match data {
                Expr::Tuple(values) if values.len() == elems.len() => {
                    for (value, elem) in values.iter().zip(elems) {
                        self.check_yield_site(*value, elem);
                    }
                }
                _ => self.invalid_yield(expr, shape),
            },
            Shape::Sum {
                variant, payload, ..
            } => {
                let ctor = match self.env.value_path_ref(expr) {
                    Some(ValuePathRef::UnitVariant(variant)) => Some(variant.variant.idx),
                    _ => self.env.callable_expr(expr).and_then(|callable| {
                        match callable.callable_def() {
                            CallableDef::VariantCtor(ctor) => Some(ctor.idx),
                            CallableDef::Func(_) => None,
                        }
                    }),
                };
                match (ctor, data) {
                    (Some(ctor), Expr::Call(_, args)) if ctor == *variant && args.len() == 1 => {
                        // The payload is yielded, not moved into the variant.
                        self.env.forget_implicit_move(args[0].expr);
                        self.check_yield_site(args[0].expr, payload);
                    }
                    // The empty variant grants nothing.
                    (Some(_), _) => {}
                    _ => self.invalid_yield(expr, shape),
                }
            }
        }
    }

    /// Makes `expr`, a place a `mut` context needs, mutable by selecting
    /// `index_mut` for the trait-lowered indexes along its place chain:
    /// `a[i].x = v`, `mut a[i]` and `a[i].bump()` with a `mut self` method.
    /// Returns whether the place is mutable.
    pub(super) fn select_mut_place(&mut self, expr: ExprId) -> bool {
        let Some(prop) = self.env.typed_expr(expr) else {
            return false;
        };
        if prop.is_mut {
            return true;
        }
        let selected = match self.env.expr_data(expr) {
            Partial::Present(Expr::Field(base, _)) => self.select_mut_place(*base),
            Partial::Present(Expr::Bin(base, index, BinOp::Index)) => {
                let Some(base_prop) = self.env.typed_expr(*base) else {
                    return false;
                };
                if base_prop.ty.is_array(self.db) {
                    self.select_mut_place(*base)
                } else {
                    // `index_mut` takes `mut self`: a place base must be writable.
                    let is_place =
                        self.env.expr_place(*base).is_some() || self.is_pointer_deref_expr(*base);
                    if is_place && !self.select_mut_place(*base) {
                        return false;
                    }
                    self.env.forget_call(expr);
                    let indexed =
                        self.check_ops_trait(expr, base_prop.ty, &IndexMutOp, Some(*index));
                    !indexed.ty.has_invalid(self.db)
                }
            }
            _ => false,
        };
        if selected {
            let shape = prop.shape.as_ref().map(|shape| match shape {
                Shape::Access(_, ty) => Shape::Access(BorrowKind::Mut, *ty),
                shape => shape.clone(),
            });
            self.env.type_expr(
                expr,
                ExprProp {
                    is_mut: true,
                    shape,
                    ..prop
                },
            );
        }
        selected
    }

    /// Marks `expr` as used as an access. A block forwards the access of its
    /// tail.
    pub(super) fn consume_access(&mut self, expr: ExprId) {
        self.env.consume_access(expr);
        if let Partial::Present(Expr::Block(stmts, _)) = expr.data(self.db, self.body())
            && let Some(Partial::Present(Stmt::Expr(tail))) =
                stmts.last().map(|stmt| stmt.data(self.db, self.body()))
        {
            self.consume_access(*tail);
        }
    }

    fn invalid_yield(&mut self, expr: ExprId, shape: &Shape<'db>) {
        self.push_diag(BodyDiag::InvalidYield {
            primary: expr.span(self.body()).into(),
            shape: shape.pretty_print(self.db),
        });
    }

    /// `ref p`, `mut p` and tuple or sum yield shapes are not values: any use
    /// other than binding, passing, projecting or yielding them is an error.
    pub(super) fn check_access_uses(&mut self) {
        let misused = self
            .body()
            .exprs(self.db)
            .keys()
            .filter(|&expr| {
                let Some(shape) = self.env.typed_expr(expr).and_then(|prop| prop.shape) else {
                    return false;
                };
                let explicit = matches!(
                    expr.data(self.db, self.body()),
                    Partial::Present(Expr::Un(_, UnOp::Ref | UnOp::Mut))
                );
                (explicit || matches!(shape, Shape::Tuple(_) | Shape::Sum { .. }))
                    && !self.env.is_consumed_access(expr)
            })
            .collect::<Vec<_>>();
        for expr in misused {
            self.push_diag(BodyDiag::AccessNotValue {
                primary: expr.span(self.body()).into(),
            });
        }
    }
}
