use common::InputFile;
use parser::{ast, SyntaxNode};

use crate::{
    hir_def::{Body, ExprId},
    parse_file,
    span::{params::LazyGenericArgListSpan, path::LazyPathSpan, LazySpanAtom},
};

use super::{
    db::SpannedHirDb,
    define_lazy_span_node,
    transition::{ChainRoot, SpanTransitionChain},
};

define_lazy_span_node!(LazyExprSpan, ast::Expr,);
impl LazyExprSpan {
    pub fn new(expr: ExprId, body: Body) -> Self {
        let root = ExprRoot { expr, body };
        Self(SpanTransitionChain::new(root))
    }

    pub fn into_bin_expr(self) -> LazyBinExprSpan {
        LazyBinExprSpan(self.0)
    }

    pub fn into_un_expr(self) -> LazyUnExprSpan {
        LazyUnExprSpan(self.0)
    }

    pub fn into_call_expr(self) -> LazyCallExprSpan {
        LazyCallExprSpan(self.0)
    }

    pub fn into_method_call_expr(self) -> LazyMethodCallExprSpan {
        LazyMethodCallExprSpan(self.0)
    }

    pub fn into_path_expr(self) -> LazyPathExprSpan {
        LazyPathExprSpan(self.0)
    }

    pub fn into_record_init_expr(self) -> LazyRecordInitExprSpan {
        LazyRecordInitExprSpan(self.0)
    }

    pub fn into_field_expr(self) -> LazyFieldExprSpan {
        LazyFieldExprSpan(self.0)
    }

    pub fn into_match_expr(self) -> LazyMatchExprSpan {
        LazyMatchExprSpan(self.0)
    }
}

define_lazy_span_node!(
    LazyBinExprSpan,
    ast::BinExpr,
    @node {
        (op, op, LazySpanAtom),
    }
);

define_lazy_span_node!(
    LazyUnExprSpan,
    ast::UnExpr,
    @node {
        (op, op, LazySpanAtom),
    }
);

define_lazy_span_node!(
    LazyCallExprSpan,
    ast::CallExpr,
    @node {
        (generic_args, generic_args, LazyGenericArgListSpan),
        (args, args, LazyCallArgListSpan),
    }
);

define_lazy_span_node!(
    LazyMethodCallExprSpan,
    ast::MethodCallExpr,
    @token {
        (method_name, method_name),
    }
    @node {
        (generic_args, generic_args, LazyGenericArgListSpan),
        (args, args, LazyCallArgListSpan),
    }
);

define_lazy_span_node! {
    LazyPathExprSpan,
    ast::PathExpr,
    @node {
        (path, path, LazyPathSpan),
    }
}

define_lazy_span_node!(
    LazyRecordInitExprSpan,
    ast::RecordInitExpr,
    @node {
        (path, path, LazyPathSpan),
        (fields, fields, LazyRecordFieldListSpan),
    }
);

define_lazy_span_node!(
    LazyFieldExprSpan,
    ast::FieldExpr,
    @token {
        (accessor, name_or_index),
    }
);

define_lazy_span_node!(
    LazyMatchExprSpan,
    ast::MatchExpr,
    @node {
        (arms, arms, LazyMatchArmListSpan),
    }
);

define_lazy_span_node!(
    LazyCallArgListSpan,
    ast::CallArgList,
    @idx {
        (arg, LazyCallArgSpan),
    }
);

define_lazy_span_node!(
    LazyCallArgSpan,
    ast::CallArg,
    @token {
        (label, label),
    }
);

define_lazy_span_node!(
    LazyRecordFieldListSpan,
    ast::RecordFieldList,
    @idx {
        (field, LazyRecordFieldSpan),
    }
);

define_lazy_span_node!(
    LazyRecordFieldSpan,
    ast::RecordField,
    @token {
        (label, label),
    }
);

define_lazy_span_node!(
    LazyMatchArmListSpan,
    ast::MatchArmList,
    @idx {
        (arm, LazySpanAtom),
    }
);

#[derive(Clone, Copy)]
struct ExprRoot {
    expr: ExprId,
    body: Body,
}

impl ChainRoot for ExprRoot {
    fn root(&self, db: &dyn SpannedHirDb) -> (InputFile, Option<SyntaxNode>) {
        let body_ast = db.body_ast(self.body);
        let file = body_ast.file;
        let source_map = db.body_source_map(self.body);
        let pat_source = source_map.expr_map.node_to_source(self.expr);
        let Some(ptr) = pat_source.syntax_ptr() else {
            return (file, None);
        };

        let root_node = SyntaxNode::new_root(parse_file(db.upcast(), file));
        let node = ptr.to_node(&root_node);
        (file, node.into())
    }
}
