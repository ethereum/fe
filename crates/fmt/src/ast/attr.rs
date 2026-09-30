//! Formatting for attributes and doc comments.

use pretty::DocAllocator;

use crate::RewriteContext;
use parser::ast::{self, AttrArgValueKind, AttrKind, prelude::AstNode};
use parser::syntax_kind::SyntaxKind;

use super::types::{Doc, ToDoc, block_list_auto, intersperse};

impl ToDoc for ast::AttrList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        match attr_lines(self, ctx) {
            Some(doc) => doc.append(alloc.hardline()),
            None => alloc.nil(),
        }
    }
}

/// Renders the attributes of a list one per line, without a line break after
/// the last one. Returns `None` for an empty list.
pub(crate) fn attr_lines<'a>(list: &ast::AttrList, ctx: &'a RewriteContext<'a>) -> Option<Doc<'a>> {
    let alloc = &ctx.alloc;

    let attrs: Vec<_> = list.iter().map(|attr| attr.to_doc(ctx)).collect();
    if attrs.is_empty() {
        None
    } else {
        Some(intersperse(alloc, attrs, alloc.hardline()))
    }
}

impl ToDoc for ast::Attr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.kind() {
            AttrKind::Normal(attr) => attr.to_doc(ctx),
            AttrKind::DocComment(attr) => attr.to_doc(ctx),
        }
    }
}

impl ToDoc for ast::NormalAttr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let path = match self.path() {
            Some(p) => p.to_doc(ctx),
            None => return alloc.text("#[]"),
        };

        // Handle both forms:
        // - #[attr(arg1, arg2)]  -> args()
        // - #[attr = value]      -> value()
        let suffix_doc = if let Some(args) = self.args() {
            args.to_doc(ctx)
        } else if let Some(val) = self.value() {
            let val_doc = match val {
                AttrArgValueKind::Ident(tok) => alloc.text(tok.text().to_string()),
                AttrArgValueKind::Lit(lit) => lit.to_doc(ctx),
                AttrArgValueKind::Expr(expr) => expr.to_doc(ctx),
            };
            alloc.text(" = ").append(val_doc)
        } else {
            alloc.nil()
        };

        // Inner attributes (`#![...]`) have a `!` after the `#`.
        let open = if is_inner_attr(self) { "#![" } else { "#[" };

        alloc
            .text(open)
            .append(path)
            .append(suffix_doc)
            .append(alloc.text("]"))
    }
}

fn is_inner_attr(attr: &ast::NormalAttr) -> bool {
    let mut tokens = attr
        .syntax()
        .children_with_tokens()
        .filter_map(|child| child.into_token())
        .filter(|token| !token.kind().is_trivia() && token.kind() != SyntaxKind::Newline);
    matches!(
        (
            tokens.next().map(|t| t.kind()),
            tokens.next().map(|t| t.kind())
        ),
        (Some(SyntaxKind::Pound), Some(SyntaxKind::Not))
    )
}

impl ToDoc for ast::DocCommentAttr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        match self.doc() {
            Some(tok) => alloc.text(tok.text().to_string()),
            None => alloc.nil(),
        }
    }
}

impl ToDoc for ast::AttrArgList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let indent = ctx.config.indent_width as isize;
        block_list_auto(
            ctx,
            self.syntax(),
            "(",
            ")",
            ast::AttrArg::cast,
            indent,
            true,
        )
    }
}

impl ToDoc for ast::AttrArg {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let val_doc = self.value().map(|val| match val {
            AttrArgValueKind::Ident(tok) => alloc.text(tok.text().to_string()),
            AttrArgValueKind::Lit(lit) => lit.to_doc(ctx),
            AttrArgValueKind::Expr(expr) => expr.to_doc(ctx),
        });
        match (self.key(), val_doc) {
            (Some(key), Some(val_doc)) => key.to_doc(ctx).append(alloc.text(" = ")).append(val_doc),
            (Some(key), None) => key.to_doc(ctx),
            // An expression argument has no key.
            (None, Some(val_doc)) => val_doc,
            (None, None) => alloc.nil(),
        }
    }
}

impl ToDoc for ast::Lit {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        ctx.alloc.text(ctx.snippet_trimmed(self))
    }
}
