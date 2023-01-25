use std::{cell::Cell, rc::Rc};

use crate::{parser::func::FnScope, SyntaxKind};

use super::{
    attr, define_scope,
    expr::parse_expr,
    param::GenericParamListScope,
    struct_::RecordFieldDefListScope,
    token_stream::TokenStream,
    type_::{parse_type, TupleTypeScope},
    use_tree::UseTreeScope,
    Parser,
};

define_scope! {
    #[doc(hidden)]
    pub ItemListScope,
    ItemList,
    Override(
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

        loop {
            parser.set_newline_as_trivia(true);
            if parser.current_kind().is_none() {
                break;
            }

            let mut checkpoint = attr::parse_attr_list(parser);
            let modifier_scope = ItemModifierScope::default();
            let modifier = match parser.current_kind() {
                Some(kind) if is_modifier_head(kind) => {
                    let (_, modifier_checkpoint) = parser.parse(modifier_scope.clone(), None);
                    checkpoint.get_or_insert(modifier_checkpoint);
                    modifier_scope.kind.get()
                }
                _ => ModifierKind::None,
            };

            if modifier.is_unsafe() && parser.current_kind() != Some(FnKw) {
                parser.error("expected `fn` after `unsafe` keyword");
            } else if modifier.is_pub() && parser.current_kind() == Some(ExternKw) {
                parser.error("`pub` can't be used for `extern` block");
            }

            match parser.current_kind() {
                Some(FnKw) => {
                    parser.parse(FnScope::default(), checkpoint);
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
                tok => parser
                    .error_and_recover(&format! {"expected item: but got {:?}", tok}, checkpoint),
            }

            parser.set_newline_as_trivia(false);
            if parser.current_kind().is_some() && !parser.bump_if(SyntaxKind::Newline) {
                parser.error_and_recover("expected newline after item definition", checkpoint)
            }
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
        if parser.bump_if(SyntaxKind::PubKw) {
            if parser.bump_if(SyntaxKind::UnsafeKw) {
                self.kind.set(ModifierKind::PubAndUnsafe);
            } else {
                self.kind.set(ModifierKind::Pub);
            }
        } else if parser.bump_if(SyntaxKind::UnsafeKw) {
            self.kind.set(ModifierKind::Unsafe);
        } else {
            self.kind.set(ModifierKind::None);
        }
    }
}

#[derive(Debug, Clone, Copy)]
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
    fn is_pub(&self) -> bool {
        matches!(self, Self::Pub | Self::PubAndUnsafe)
    }

    fn is_unsafe(&self) -> bool {
        matches!(self, Self::Unsafe | Self::PubAndUnsafe)
    }
}

define_scope! { ContractScope, Contract, Inheritance }
impl super::Parse for ContractScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::ContractKw);

        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected ident for the struct name", None)
        }

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

        parser.with_recovery_tokens(&[SyntaxKind::Lt, SyntaxKind::LBrace], |parser| {
            if !parser.bump_if(SyntaxKind::Ident) {
                parser.error_and_recover("expected ident for the enum name", None)
            }
        });

        parser.with_recovery_tokens(&[SyntaxKind::LBrace], |parser| {
            if parser.current_kind() == Some(SyntaxKind::Lt) {
                parser.parse(GenericParamListScope::default(), None);
            }
        });

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

        loop {
            parser.set_newline_as_trivia(true);
            if parser.current_kind() == Some(SyntaxKind::RBrace) || parser.current_kind().is_none()
            {
                break;
            }
            parser.parse(VariantDefScope::default(), None);
            parser.set_newline_as_trivia(false);
            if !parser.bump_if(SyntaxKind::Newline)
                && parser.current_kind() != Some(SyntaxKind::RBrace)
            {
                parser.error_and_recover("expected newline after variant definition", None);
            }
        }

        if !parser.bump_if(SyntaxKind::RBrace) {
            parser.error_and_recover(
                "expected the closing brace of the enum variants definition",
                None,
            );
            parser.bump_if(SyntaxKind::RBrace);
        }
    }
}

define_scope! { VariantDefScope, VariantDef, Override(RBrace, Newline) }
impl super::Parse for VariantDefScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected ident for the variant name", None);
            return;
        }

        if parser.current_kind() == Some(SyntaxKind::LParen) {
            parser.parse(TupleTypeScope::default(), None);
        }
    }
}

define_scope! { TraitScope, Trait, Inheritance }
impl super::Parse for TraitScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::TraitKw);

        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected ident for the trait name", None)
        }

        parser.with_recovery_tokens(&[SyntaxKind::LBrace], |parser| {
            if parser.current_kind() == Some(SyntaxKind::Lt) {
                parser.parse(GenericParamListScope::default(), None);
            }
        });

        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected trait body", None);
            return;
        }

        parser.parse(TraitItemListScope::default(), None);
    }
}

define_scope! { TraitItemListScope, TraitItemList, Override(RBrace, Newline, FnKw) }
impl super::Parse for TraitItemListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBrace);
        loop {
            parser.set_newline_as_trivia(true);
            if matches!(parser.current_kind(), Some(SyntaxKind::RBrace) | None) {
                break;
            }

            let checkpoint = attr::parse_attr_list(parser);

            match parser.current_kind() {
                Some(SyntaxKind::FnKw) => {
                    parser.parse(FnScope::default(), checkpoint);
                }
                _ => {
                    parser.error_and_recover("trait item is restricted to `fn`", checkpoint);
                }
            }

            parser.set_newline_as_trivia(false);
            if !matches!(
                parser.current_kind(),
                Some(SyntaxKind::RBrace | SyntaxKind::Newline)
            ) {
                parser.error_and_recover("expected newline after trait item definition", checkpoint)
            }
        }

        if !parser.bump_if(SyntaxKind::RBrace) {
            parser.error_and_recover("expected `}` to close the trait body", None)
        }
    }
}

define_scope! { ImplScope, Impl, Inheritance }
impl super::Parse for ImplScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::ImplKw);

        parser.start_dry_run();
        let is_trait_impl = parse_type(parser, None, true) && parser.bump_if(SyntaxKind::ForKw);
        parser.end_dry_run();

        if is_trait_impl {
            self.set_kind(SyntaxKind::ImplTrait);
            parse_type(parser, None, false);
            parser.bump_expected(SyntaxKind::ForKw);
            parse_type(parser, None, false);
        } else {
            parse_type(parser, None, true);
        }

        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected impl body", None);
            return;
        }

        if is_trait_impl {
            parser.parse(ImplTraitItemListScope::default(), None);
        } else {
            parser.parse(ImplItemListScope::default(), None);
        }
    }
}

define_scope! { ImplTraitItemListScope, ImplTraitItemList, Override(RBrace, FnKw) }
impl super::Parse for ImplTraitItemListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parse_fn_item_block(parser, false, true)
    }
}

define_scope! { ImplItemListScope, ImplItemList, Override(RBrace, FnKw) }
impl super::Parse for ImplItemListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parse_fn_item_block(parser, true, true)
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

        parser.with_recovery_tokens(&[SyntaxKind::Eq], |parser| {
            if !parser.bump_if(SyntaxKind::Ident) {
                parser.error_and_recover("expected identifier", None);
            }
        });

        parser.with_recovery_tokens(&[SyntaxKind::Eq], |parser| {
            if !parser.bump_if(SyntaxKind::Colon) {
                parser.error_and_recover("expected type annotation for `const`", None);
            }
            parse_type(parser, None, false);
        });

        if !parser.bump_if(SyntaxKind::Eq) {
            parser.error_and_recover("expected `=` for const value definition", None);
            return;
        }

        parse_expr(parser);
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

define_scope! { ExternItemListScope, ExternItemList, Override(RBrace, FnKw) }
impl super::Parse for ExternItemListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parse_fn_item_block(parser, true, false);
    }
}

define_scope! { TypeAliasScope, TypeAlias, Inheritance }
impl super::Parse for TypeAliasScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::TypeKw);

        parser.with_recovery_tokens(&[SyntaxKind::Lt, SyntaxKind::Eq], |parser| {
            if !parser.bump_if(SyntaxKind::Ident) {
                parser.error_and_recover("expected identifier for type alias name", None)
            }
        });

        parser.with_recovery_tokens(&[SyntaxKind::Eq], |parser| {
            if parser.current_kind() == Some(SyntaxKind::Lt) {
                parser.parse(GenericParamListScope::default(), None);
            }
        });

        if !parser.bump_if(SyntaxKind::Eq) {
            parser.error_and_recover("expected `=` for type alias definition", None);
            return;
        }

        parse_type(parser, None, false);
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
    allow_fn_def: bool,
) {
    parser.bump_expected(SyntaxKind::LBrace);
    loop {
        parser.set_newline_as_trivia(true);
        if matches!(parser.current_kind(), Some(SyntaxKind::RBrace) | None) {
            break;
        }

        let mut checkpoint = attr::parse_attr_list(parser);
        let modifier_scope = ItemModifierScope::default();
        match parser.current_kind() {
            Some(kind) if is_modifier_head(kind) && allow_modifier => {
                if allow_modifier {
                    let (_, modifier_checkpoint) = parser.parse(modifier_scope, None);
                    checkpoint.get_or_insert(modifier_checkpoint);
                } else {
                    parser.error_and_recover("modifier is not allowed in the block", checkpoint);
                }
            }
            _ => {}
        }

        match parser.current_kind() {
            Some(SyntaxKind::FnKw) => {
                let scope = if allow_fn_def {
                    FnScope::default()
                } else {
                    FnScope::disallow_def()
                };
                parser.parse(scope, checkpoint);
            }
            _ => {
                parser.error_and_recover("only `fn` is allowed in the block", checkpoint);
            }
        }

        parser.set_newline_as_trivia(false);
        if !matches!(
            parser.current_kind(),
            Some(SyntaxKind::RBrace | SyntaxKind::Newline)
        ) {
            parser.error_and_recover("expected newline after item definition", checkpoint)
        }
    }

    if !parser.bump_if(SyntaxKind::RBrace) {
        parser.error_and_recover("expected `}` to close the block", None);
        parser.bump_if(SyntaxKind::RBrace);
    }
}

fn is_modifier_head(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::PubKw | SyntaxKind::UnsafeKw)
}
