// This is necessary because `salsa::tracked` structs generates a
// constructor
// that may take many arguments depending on the number of fields in the struct.
#![allow(clippy::too_many_arguments)]

use std::hash::Hash;

use cranelift_entity::{EntityRef, PrimaryMap, SecondaryMap};
use parser::ast::{self, prelude::*};
use rustc_hash::FxHashMap;

use crate::span::{HirOrigin, LocalOrigin};

use super::{Expr, ExprId, Partial, Pat, PatId, Stmt, StmtId, TrackedItemId};

#[salsa::tracked]
pub struct Body {
    #[id]
    id: TrackedBodyId,

    #[return_ref]
    pub stmts: NodeStore<StmtId, Partial<Stmt>>,
    #[return_ref]
    pub exprs: NodeStore<ExprId, Partial<Expr>>,
    #[return_ref]
    pub pats: NodeStore<PatId, Partial<Pat>>,

    #[return_ref]
    pub(crate) source_map: BodySourceMap,

    #[return_fer]
    pub(crate) ast: HirOrigin<ast::Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TrackedBodyId {
    ItemBody(Box<TrackedItemId>),
    NestedBody(Box<Self>),
    NamelessBody,
}

pub type NodeStore<K, V> = PrimaryMap<K, V>;

pub trait SourceAst: AstNode + Clone + Hash + PartialEq + Eq {}
impl<T> SourceAst for T where T: AstNode + Clone + Hash + PartialEq + Eq {}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct BodySourceMap {
    pub stmt_map: SourceNodeMap<ast::Stmt, StmtId>,
    pub expr_map: SourceNodeMap<ast::Expr, ExprId>,
    pub pat_map: SourceNodeMap<ast::Pat, PatId>,
}

#[derive(Clone, Debug)]
pub struct SourceNodeMap<Ast, Node>
where
    Ast: SourceAst,
    Node: EntityRef,
{
    pub node_to_source: SecondaryMap<Node, LocalOrigin<Ast>>,
    pub source_to_node: FxHashMap<LocalOrigin<Ast>, Node>,
}

impl<Ast, Node> SourceNodeMap<Ast, Node>
where
    Ast: SourceAst,
    Node: EntityRef,
{
    pub(crate) fn insert(&mut self, node: Node, ast: LocalOrigin<Ast>) {
        self.node_to_source[node] = ast.clone();
        self.source_to_node.insert(ast, node);
    }
}

impl<Ast, Node> PartialEq for SourceNodeMap<Ast, Node>
where
    Ast: SourceAst,
    Node: EntityRef,
{
    fn eq(&self, other: &Self) -> bool {
        self.node_to_source == other.node_to_source
    }
}

impl<Ast, Node> Eq for SourceNodeMap<Ast, Node>
where
    Ast: SourceAst,
    Node: EntityRef,
{
}

impl<Ast, Node> Default for SourceNodeMap<Ast, Node>
where
    Ast: SourceAst,
    Node: EntityRef,
{
    fn default() -> Self {
        Self {
            source_to_node: FxHashMap::default(),
            node_to_source: SecondaryMap::new(),
        }
    }
}
