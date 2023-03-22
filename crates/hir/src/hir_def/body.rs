use cranelift_entity::{PrimaryMap, SecondaryMap};
use parser::ast::{self};

use crate::span::HirOrigin;

use super::{Expr, ExprId, MaybeInvalid, Pat, PatId, Stmt, StmtId, TrackedItemId};

#[salsa::tracked]
pub struct Body {
    #[id]
    id: TrackedBodyId,

    #[return_ref]
    pub stmts: BodyNodeMap<StmtId, MaybeInvalid<Stmt>>,
    #[return_ref]
    pub exprs: BodyNodeMap<ExprId, MaybeInvalid<Expr>>,
    #[return_ref]
    pub pats: BodyNodeMap<PatId, MaybeInvalid<Pat>>,

    #[return_ref]
    pub(crate) stmt_source_map: BodySourceMap<StmtId, ast::Stmt>,
    #[return_ref]
    pub(crate) expr_source_map: BodySourceMap<ExprId, ast::Expr>,
    #[return_ref]
    pub(crate) pat_source_map: BodySourceMap<PatId, ast::Pat>,

    #[return_fer]
    pub(crate) ast: HirOrigin<ast::Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TrackedBodyId {
    ItemBody(Box<TrackedItemId>),
    NestedBody(Box<Self>),
    NamelessBody,
}

pub type BodyNodeMap<K, V> = PrimaryMap<K, V>;
pub type BodySourceMap<K, V> = SecondaryMap<K, HirOrigin<V>>;
