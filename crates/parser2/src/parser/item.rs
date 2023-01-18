use std::cell::RefCell;

use crate::SyntaxKind;

use super::{define_scope, token_stream::TokenStream, Parser};

define_scope! {
    RootScope,
    Root,
    Override()
}
impl super::Parse for RootScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.parse(ItemListScope::default(), None);
    }
}

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
            parser.bump_trivias(true);
            if parser.current_kind().is_none() {
                break;
            }

            let mut checkpoint = None;
            parser.bump_trivias(true);
            if let Some(DocComment) | Some(Pound) = parser.current_kind() {
                checkpoint.get_or_insert_with(|| parser.checkpoint());
                parser.parse(super::attr::AttrListScope::default(), None);
            }

            parser.bump_trivias(true);
            let modifier = match parser.current_kind() {
                Some(PubKw) => {
                    checkpoint.get_or_insert_with(|| parser.checkpoint());
                    parser.bump();
                    parser.bump_trivias(true);

                    if parser.current_kind() == Some(UnsafeKw) {
                        parser.bump();
                        Modifier::PubAndUnsafe
                    } else {
                        Modifier::Pub
                    }
                }

                Some(UnsafeKw) => {
                    checkpoint.get_or_insert_with(|| parser.checkpoint());
                    parser.bump();
                    Modifier::Unsafe
                }

                Some(_) => Modifier::None,

                None => {
                    parser.error_and_recover("expected item", checkpoint);
                    continue;
                }
            };
            parser.bump_trivias(true);

            if modifier.is_unsafe() && parser.current_kind() != Some(FnKw) {
                parser.error("expected `fn` after `unsafe` keyword");
            } else if modifier.is_pub() && parser.current_kind() == Some(ExternKw) {
                parser.error("`pub` can't be used for `extern` block");
            }

            match parser.current_kind() {
                Some(FnKw) => parser.parse(super::func::FnScope::default(), checkpoint),
                Some(StructKw) => parser.parse(super::struct_::StructScope::default(), checkpoint),
                Some(EnumKw) => parser.parse(EnumScope::default(), checkpoint),
                Some(TraitKw) => parser.parse(TraitScope::default(), checkpoint),
                Some(ImplKw) => parser.parse(ImplScope::default(), checkpoint),
                Some(UseKw) => parser.parse(UseScope::default(), checkpoint),
                Some(ConstKw) => parser.parse(ConstScope::default(), checkpoint),
                Some(ExternKw) => parser.parse(ExternScope::default(), checkpoint),
                Some(TypeKw) => parser.parse(TypeAliasScope::default(), checkpoint),
                tok => parser
                    .error_and_recover(&format! {"expected item: but got {:?}", tok}, checkpoint),
            }
        }
    }
}

enum Modifier {
    None,
    Pub,
    Unsafe,
    PubAndUnsafe,
}

impl Modifier {
    fn is_pub(&self) -> bool {
        matches!(self, Modifier::Pub | Modifier::PubAndUnsafe)
    }

    fn is_unsafe(&self) -> bool {
        matches!(self, Modifier::Unsafe | Modifier::PubAndUnsafe)
    }
}

define_scope! {
    EnumScope,
    Enum,
    Inheritance
}
impl super::Parse for EnumScope {
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
    }
}

define_scope! {
    TraitScope,
    Trait,
    Inheritance
}
impl super::Parse for TraitScope {
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
    }
}

// We can't use `define_scope` here since the `syntax_kind` of the scope can be
// determined after parsing.
#[derive(Debug, Clone)]
struct ImplScope {
    syntax_kind: RefCell<SyntaxKind>,
    recovery_method: super::RecoveryMethod,
}
impl Default for ImplScope {
    fn default() -> Self {
        Self {
            syntax_kind: SyntaxKind::Impl.into(),
            recovery_method: super::RecoveryMethod::inheritance_empty(),
        }
    }
}
impl super::ParsingScope for ImplScope {
    fn recovery_method(&self) -> &super::RecoveryMethod {
        &self.recovery_method
    }

    fn syntax_kind(&self) -> SyntaxKind {
        *self.syntax_kind.borrow()
    }
}
impl super::Parse for ImplScope {
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
    }
}

define_scope! {
    UseScope,
    Use,
    Inheritance
}
impl super::Parse for UseScope {
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
    }
}

define_scope! {
    ConstScope,
    Const,
    Inheritance
}
impl super::Parse for ConstScope {
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
    }
}

define_scope! {
    ExternScope,
    Extern,
    Inheritance
}
impl super::Parse for ExternScope {
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
    }
}

define_scope! {
    TypeAliasScope,
    TypeAlias,
    Inheritance
}
impl super::Parse for TypeAliasScope {
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
    }
}
