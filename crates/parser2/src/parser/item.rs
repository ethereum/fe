use std::cell::Cell;

use crate::SyntaxKind;

use super::{
    attr, define_scope, expr::parse_expr, param::GenericParamListScope,
    struct_::RecordFieldDefListScope, token_stream::TokenStream, type_::parse_type,
    use_tree::UseTreeScope, Parser,
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
            let (_, modifier_checkpoint) = parser.parse(modifier_scope.clone(), None);
            checkpoint.get_or_insert(modifier_checkpoint);
            let modifier = modifier_scope.kind.get();

            if modifier.is_unsafe() && parser.current_kind() != Some(FnKw) {
                parser.error("expected `fn` after `unsafe` keyword");
            } else if modifier.is_pub() && parser.current_kind() == Some(ExternKw) {
                parser.error("`pub` can't be used for `extern` block");
            }

            match parser.current_kind() {
                Some(FnKw) => {
                    parser.parse(super::func::FnScope::default(), checkpoint);
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
    ItemModifierScope {kind: Cell<ModifierKind>},
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
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
    }
}

define_scope! { TraitScope, Trait, Inheritance }
impl super::Parse for TraitScope {
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
    }
}

define_scope! { ImplScope, Impl, Inheritance }
impl super::Parse for ImplScope {
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
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

        parser.add_recovery_token(SyntaxKind::Colon);
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected identifier", None);
        }
        parser.remove_recovery_token(SyntaxKind::Colon);

        parser.add_recovery_token(SyntaxKind::Eq);
        if !parser.bump_if(SyntaxKind::Colon) {
            parser.error_and_recover("expected type annotation for `const`", None);
        }
        parse_type(parser, None);
        parser.remove_recovery_token(SyntaxKind::Eq);

        if !parser.bump_if(SyntaxKind::Eq) {
            parser.error_and_recover("expected `=` for const value definition", None);
            return;
        }

        parse_expr(parser);
    }
}

define_scope! { ExternScope, Extern, Inheritance }
impl super::Parse for ExternScope {
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
    }
}

define_scope! { TypeAliasScope, TypeAlias, Inheritance }
impl super::Parse for TypeAliasScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::TypeKw);

        parser.add_recovery_token(SyntaxKind::Lt);
        parser.add_recovery_token(SyntaxKind::Eq);
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected identifier for type alias name", None)
        }
        parser.remove_recovery_token(SyntaxKind::Lt);

        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericParamListScope::default(), None);
        }
        parser.remove_recovery_token(SyntaxKind::Eq);

        if !parser.bump_if(SyntaxKind::Eq) {
            parser.error_and_recover("expected `=` for type alias definition", None);
            return;
        }

        parse_type(parser, None);
    }
}
