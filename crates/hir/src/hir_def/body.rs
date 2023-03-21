use cranelift_entity::{PrimaryMap, SecondaryMap};
use fe_parser2::ast::{self};

use crate::span::HirOrigin;

use super::{Expr, ExprId, ItemKind, MaybeInvalid, Pat, PatId, Stmt, StmtId};

#[salsa::tracked]
pub struct Body {
    #[id]
    pub kind: BodyKind,

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BodyKind {
    /// This is a body appearing in a item, e.g., a function or const item.
    ItemBody(ItemKind),
    /// This is a body appearing in array types or
    NamelessConst,
}

impl From<Option<ItemKind>> for BodyKind {
    fn from(item: Option<ItemKind>) -> Self {
        match item {
            Some(item) => Self::ItemBody(item),
            None => Self::NamelessConst,
        }
    }
}

pub type BodyNodeMap<K, V> = PrimaryMap<K, V>;
pub type BodySourceMap<K, V> = SecondaryMap<K, HirOrigin<V>>;
