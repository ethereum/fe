use std::cell::Cell;

use crate::SyntaxKind;

use super::{attr, define_scope, token_stream::TokenStream, Parser};

define_scope! {
    ItemListScope,
    ItemList,
    Override(
        FnKw,
        StructKw,
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
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
    }
}

define_scope! { ConstScope, Const, Inheritance }
impl super::Parse for ConstScope {
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
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
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
    }
}
