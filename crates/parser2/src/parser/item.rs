use std::{cell::Cell, rc::Rc};

use crate::{parser::func::FuncScope, SyntaxKind};

use super::{
    attr, define_scope,
    expr::parse_expr,
    func::FuncDefScope,
    param::{parse_generic_params_opt, parse_where_clause_opt, TraitRefScope},
    struct_::RecordFieldDefListScope,
    token_stream::{LexicalToken, TokenStream},
    type_::{parse_type, TupleTypeScope},
    use_tree::UseTreeScope,
    Parser,
};

define_scope! {
    #[doc(hidden)]
    pub ItemListScope {inside_mod: bool},
    ItemList,
    Override(
        ModKw,
        FnKw,
        StructKw,
        ContractKw,
        EnumKw,
        TraitKw,
        ImplKw,
        UseKw,
        ConstKw,
        ExternKw,
        TypeKw,
        PubKw,
        UnsafeKw,
        DocComment,
        Pound
    )
}
impl super::Parse for ItemListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        use crate::SyntaxKind::*;

        if self.inside_mod {
            parser.bump_expected(LBrace);
        }

        loop {
            parser.set_newline_as_trivia(true);
            if self.inside_mod && parser.bump_if(RBrace) {
                break;
            }
            if parser.current_kind().is_none() {
                if self.inside_mod {
                    parser.error("expected `}` to close the module");
                }
                break;
            }

            parser.parse(ItemScope::default(), None);
        }
    }
}

define_scope! {
    #[doc(hidden)]
    pub(super) ItemScope,
    Item,
    Inheritance
}
impl super::Parse for ItemScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        use crate::SyntaxKind::*;

        let mut checkpoint = attr::parse_attr_list(parser);
        let modifier_scope = ItemModifierScope::default();
        let modifier = match parser.current_kind() {
            Some(kind) if kind.is_modifier_head() => {
                let (_, modifier_checkpoint) = parser.parse(modifier_scope.clone(), None);
                checkpoint.get_or_insert(modifier_checkpoint);
                modifier_scope.kind.get()
            }
            _ => ModifierKind::None,
        };

        if modifier.is_unsafe() && parser.current_kind() != Some(FnKw) {
            parser.error("expected `fn` after `unsafe` keyword");
        } else if modifier.is_pub() && matches!(parser.current_kind(), Some(ImplKw | ExternKw)) {
            let error_msg = format!(
                "`pub` can't be used for `{}`",
                parser.current_token().unwrap().text()
            );
            parser.error(&error_msg);
        }

        match parser.current_kind() {
            Some(ModKw) => {
                parser.parse(ModScope::default(), checkpoint);
            }
            Some(FnKw) => {
                parser.parse(FuncScope::default(), checkpoint);
            }
            Some(StructKw) => {
                parser.parse(super::struct_::StructScope::default(), checkpoint);
            }
            Some(ContractKw) => {
                parser.parse(ContractScope::default(), checkpoint);
            }
            Some(EnumKw) => {
                parser.parse(EnumScope::default(), checkpoint);
            }
            Some(TraitKw) => {
                parser.parse(TraitScope::default(), checkpoint);
            }
            Some(ImplKw) => {
                parser.parse(ImplScope::default(), checkpoint);
            }
            Some(UseKw) => {
                parser.parse(UseScope::default(), checkpoint);
            }
            Some(ConstKw) => {
                parser.parse(ConstScope::default(), checkpoint);
            }
            Some(ExternKw) => {
                parser.parse(ExternScope::default(), checkpoint);
            }
            Some(TypeKw) => {
                parser.parse(TypeAliasScope::default(), checkpoint);
            }
            tok => {
                parser.error_and_recover(&format! {"expected item: but got {tok:?}"}, checkpoint)
            }
        }

        parser.set_newline_as_trivia(false);
        if parser.current_kind().is_some() && !parser.bump_if(SyntaxKind::Newline) {
            parser.bump_or_recover(
                SyntaxKind::Newline,
                "expected newline after item definition",
                checkpoint,
            )
        }
    }
}

define_scope! {
    ItemModifierScope {kind: Rc<Cell<ModifierKind>>},
    ItemModifier,
    Inheritance
}
impl super::Parse for ItemModifierScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        let mut modifier_kind = ModifierKind::None;

        loop {
            match parser.current_kind() {
                Some(kind) if kind.is_modifier_head() => {
                    let new_kind = modifier_kind.union(kind);
                    if new_kind == modifier_kind {
                        parser.unexpected_token_error("duplicate modifier", None);
                    } else if kind == SyntaxKind::PubKw && modifier_kind.is_unsafe() {
                        parser.unexpected_token_error(
                            "`pub` modifier must come before `unsafe`",
                            None,
                        );
                    } else {
                        parser.bump();
                    }
                    modifier_kind = new_kind;
                }
                _ => break,
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ModifierKind {
    None,
    Pub,
    Unsafe,
    PubAndUnsafe,
}
impl Default for ModifierKind {
    fn default() -> Self {
        Self::None
    }
}
impl ModifierKind {
    fn union(&self, kind: SyntaxKind) -> ModifierKind {
        match kind {
            SyntaxKind::PubKw => {
                if self.is_unsafe() {
                    Self::PubAndUnsafe
                } else {
                    Self::Pub
                }
            }
            SyntaxKind::UnsafeKw => {
                if self.is_pub() {
                    Self::PubAndUnsafe
                } else {
                    Self::Unsafe
                }
            }
            _ => unreachable!(),
        }
    }

    fn is_pub(&self) -> bool {
        matches!(self, Self::Pub | Self::PubAndUnsafe)
    }

    fn is_unsafe(&self) -> bool {
        matches!(self, Self::Unsafe | Self::PubAndUnsafe)
    }
}

define_scope! { ModScope, Mod, Inheritance }
impl super::Parse for ModScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::ModKw);
        parser.with_next_expected_tokens(
            |parser| {
                parser.bump_or_recover(
                    SyntaxKind::Ident,
                    "expected identifier for the module name",
                    None,
                )
            },
            &[SyntaxKind::LBrace],
        );
        if parser.current_kind() == Some(SyntaxKind::LBrace) {
            parser.parse(ItemListScope::new(true), None);
        } else {
            parser.error_and_recover("expected contract field definition", None);
        }
    }
}

define_scope! { ContractScope, Contract, Inheritance }
impl super::Parse for ContractScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::ContractKw);

        parser.with_next_expected_tokens(
            |parser| {
                parser.bump_or_recover(
                    SyntaxKind::Ident,
                    "expected identifier for the contract name",
                    None,
                )
            },
            &[SyntaxKind::LBrace],
        );

        if parser.current_kind() == Some(SyntaxKind::LBrace) {
            parser.parse(RecordFieldDefListScope::default(), None);
        } else {
            parser.error_and_recover("expected contract field definition", None);
        }
    }
}

define_scope! { EnumScope, Enum, Inheritance }
impl super::Parse for EnumScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::EnumKw);

        parser.with_next_expected_tokens(
            |parser| {
                parser.bump_or_recover(
                    SyntaxKind::Ident,
                    "expected identifier for the enum name",
                    None,
                );
            },
            &[SyntaxKind::Lt, SyntaxKind::LBrace, SyntaxKind::WhereKw],
        );

        parser.with_next_expected_tokens(
            |parser| parse_generic_params_opt(parser, false),
            &[SyntaxKind::LBrace, SyntaxKind::WhereKw],
        );

        parser.with_next_expected_tokens(parse_where_clause_opt, &[SyntaxKind::LBrace]);

        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected enum body", None);
            return;
        }

        parser.parse(VariantDefListScope::default(), None);
    }
}

define_scope! { VariantDefListScope, VariantDefList, Override(RBrace, Newline) }
impl super::Parse for VariantDefListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBrace);
        parser.set_newline_as_trivia(true);

        loop {
            if parser.current_kind() == Some(SyntaxKind::RBrace) || parser.current_kind().is_none()
            {
                break;
            }
            parser.parse(VariantDefScope::default(), None);

            if !parser.bump_if(SyntaxKind::Comma)
                && parser.current_kind() != Some(SyntaxKind::RBrace)
            {
                parser.error("expected comma after enum variant definition");
            }
        }

        parser.bump_or_recover(
            SyntaxKind::RBrace,
            "expected the closing brace of the enum definition",
            None,
        );
    }
}

define_scope! { VariantDefScope, VariantDef, Inheritance }
impl super::Parse for VariantDefScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_or_recover(
            SyntaxKind::Ident,
            "expected ident for the variant name",
            None,
        );

        if parser.current_kind() == Some(SyntaxKind::LParen) {
            parser.parse(TupleTypeScope::default(), None);
        } else if parser.current_kind() == Some(SyntaxKind::LBrace) {
            parser.parse(RecordFieldDefListScope::default(), None);
        }
    }
}

define_scope! { TraitScope, Trait, Inheritance }
impl super::Parse for TraitScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::TraitKw);

        parser.bump_or_recover(
            SyntaxKind::Ident,
            "expected identifier for the trait name",
            None,
        );

        parser.with_next_expected_tokens(
            |parser| parse_generic_params_opt(parser, false),
            &[SyntaxKind::LBrace, SyntaxKind::WhereKw, SyntaxKind::Colon],
        );

        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.with_next_expected_tokens(
                |parser| {
                    parser.parse(SuperTraitListScope::default(), None);
                },
                &[SyntaxKind::LBrace, SyntaxKind::WhereKw],
            );
        }

        parser.with_next_expected_tokens(parse_where_clause_opt, &[SyntaxKind::LBrace]);

        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected trait body", None);
            return;
        }

        parser.parse(TraitItemListScope::default(), None);
    }
}

define_scope! {SuperTraitListScope, SuperTraitList, Inheritance(Plus)}
impl super::Parse for SuperTraitListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Colon);
        parser.parse(TraitRefScope::default(), None);
        while parser.bump_if(SyntaxKind::Plus) {
            parser.parse(TraitRefScope::default(), None);
        }
    }
}

define_scope! { TraitItemListScope, TraitItemList, Override(RBrace, Newline, FnKw) }
impl super::Parse for TraitItemListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parse_fn_item_block(parser, false, FuncDefScope::TraitDef)
    }
}

define_scope! { ImplScope, Impl, Inheritance }
impl super::Parse for ImplScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::ImplKw);
        parser.with_recovery_tokens(
            |parser| parse_generic_params_opt(parser, false),
            &[SyntaxKind::LBrace, SyntaxKind::WhereKw, SyntaxKind::ForKw],
        );

        let is_impl_trait = parser.dry_run(|parser| {
            parser.with_next_expected_tokens(
                |parser| parse_type(parser, None),
                &[SyntaxKind::LBrace, SyntaxKind::WhereKw, SyntaxKind::ForKw],
            );
            parser.bump_if(SyntaxKind::ForKw)
        });

        if is_impl_trait {
            self.set_kind(SyntaxKind::ImplTrait);
            parser.with_next_expected_tokens(
                |parser| {
                    parser.parse(TraitRefScope::default(), None);
                },
                &[SyntaxKind::ForKw],
            );
            parser.bump_expected(SyntaxKind::ForKw);
            parser.with_next_expected_tokens(
                |parser| {
                    parse_type(parser, None);
                },
                &[SyntaxKind::LBrace, SyntaxKind::WhereKw],
            );
        } else {
            parser.with_next_expected_tokens(
                |parser| {
                    parse_type(parser, None);
                },
                &[SyntaxKind::LBrace, SyntaxKind::WhereKw],
            )
        }

        parser.with_next_expected_tokens(parse_where_clause_opt, &[SyntaxKind::LBrace]);
        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected impl body", None);
            return;
        }

        if is_impl_trait {
            parser.parse(ImplTraitItemListScope::default(), None);
        } else {
            parser.parse(ImplItemListScope::default(), None);
        }
    }
}

define_scope! { ImplTraitItemListScope, ImplTraitItemList, Override(RBrace, FnKw) }
impl super::Parse for ImplTraitItemListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parse_fn_item_block(parser, false, FuncDefScope::Impl)
    }
}

define_scope! { ImplItemListScope, ImplItemList, Override(RBrace, FnKw) }
impl super::Parse for ImplItemListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parse_fn_item_block(parser, true, FuncDefScope::Impl)
    }
}

define_scope! { UseScope, Use, Inheritance }
impl super::Parse for UseScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::UseKw);
        parser.parse(UseTreeScope::default(), None);
    }
}

define_scope! { ConstScope, Const, Inheritance }
impl super::Parse for ConstScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::ConstKw);

        parser.set_newline_as_trivia(false);

        parser.with_next_expected_tokens(
            |parser| parser.bump_or_recover(SyntaxKind::Ident, "expected identifier", None),
            &[SyntaxKind::Colon, SyntaxKind::Eq],
        );

        parser.with_next_expected_tokens(
            |parser| {
                if parser.bump_if(SyntaxKind::Colon) {
                    parse_type(parser, None);
                } else {
                    parser.error_and_recover("expected type annotation for `const`", None);
                }
            },
            &[SyntaxKind::Eq],
        );

        if parser.bump_if(SyntaxKind::Eq) {
            parse_expr(parser);
        } else {
            parser.error_and_recover("expected `=` for const value definition", None);
        }
    }
}

define_scope! { ExternScope, Extern, Inheritance }
impl super::Parse for ExternScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::ExternKw);

        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected extern block", None)
        }

        parser.parse(ExternItemListScope::default(), None);
    }
}

define_scope! { ExternItemListScope, ExternItemList, Override(RBrace, PubKw, UnsafeKw, FnKw) }
impl super::Parse for ExternItemListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parse_fn_item_block(parser, true, FuncDefScope::Extern);
    }
}

define_scope! { TypeAliasScope, TypeAlias, Inheritance }
impl super::Parse for TypeAliasScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::TypeKw);

        parser.with_next_expected_tokens(
            |parser| {
                parser.bump_or_recover(
                    SyntaxKind::Ident,
                    "expected identifier for type alias name",
                    None,
                );
            },
            &[SyntaxKind::Lt, SyntaxKind::Eq],
        );

        parser.with_next_expected_tokens(
            |parser| {
                parse_generic_params_opt(parser, true);
            },
            &[SyntaxKind::Eq],
        );

        if !parser.bump_if(SyntaxKind::Eq) {
            parser.error_and_recover("expected `=` for type alias definition", None);
            return;
        }

        parse_type(parser, None);
    }
}

/// Currently, `impl` block, `impl trait` block, `trait` block and `extern`
/// block only allow `fn` as their items. This function is used to parse the
/// `fn` item in these blocks. NOTE: This function will be invalidated when
/// these block have their own allowed items, eg. `trait` block will allow
/// `type` item.
fn parse_fn_item_block<S: TokenStream>(
    parser: &mut Parser<S>,
    allow_modifier: bool,
    fn_def_scope: FuncDefScope,
) {
    parser.bump_expected(SyntaxKind::LBrace);
    loop {
        parser.set_newline_as_trivia(true);
        if matches!(parser.current_kind(), Some(SyntaxKind::RBrace) | None) {
            break;
        }

        let mut checkpoint = attr::parse_attr_list(parser);

        let is_modifier = |kind: Option<SyntaxKind>| match kind {
            Some(kind) => kind.is_modifier_head(),
            _ => false,
        };

        if is_modifier(parser.current_kind()) {
            if allow_modifier {
                let (_, modifier_checkpoint) = parser.parse(ItemModifierScope::default(), None);
                checkpoint.get_or_insert(modifier_checkpoint);
            } else {
                while is_modifier(parser.current_kind()) {
                    parser.unexpected_token_error("modifier is not allowed in this block", None);
                }
            }
        }

        match parser.current_kind() {
            Some(SyntaxKind::FnKw) => {
                parser.parse(FuncScope::new(fn_def_scope), checkpoint);
            }
            _ => {
                parser.error_msg_on_current_token("only `fn` is allowed in this block");
                parser.recover(checkpoint);
            }
        }

        parser.set_newline_as_trivia(false);
        if !matches!(
            parser.current_kind(),
            Some(SyntaxKind::RBrace | SyntaxKind::Newline)
        ) {
            parser.error_and_recover("expected newline after item definition", None)
        }
    }

    parser.bump_or_recover(SyntaxKind::RBrace, "expected `}` to close the block", None);
}
