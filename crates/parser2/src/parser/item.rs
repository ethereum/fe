use std::{cell::Cell, convert::Infallible, rc::Rc};

use unwrap_infallible::UnwrapInfallible;

use crate::{parser::func::FuncScope, SyntaxKind};

use super::{
    attr, define_scope,
    expr::parse_expr,
    func::FuncDefScope,
    param::{parse_generic_params_opt, parse_where_clause_opt, TraitRefScope},
    parse_list,
    struct_::RecordFieldDefListScope,
    token_stream::{LexicalToken, TokenStream},
    type_::{parse_type, TupleTypeScope},
    use_tree::UseTreeScope,
    ErrProof, Parser, Recovery,
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
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
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
                    parser.error("`}` to close the module");
                }
                break;
            }

            parser.parse(ItemScope::default())?;
        }
        Ok(())
    }
}

define_scope! {
    #[doc(hidden)]
    pub(super) ItemScope,
    Item,
    Inheritance
}
impl super::Parse for ItemScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        use crate::SyntaxKind::*;

        let mut checkpoint = attr::parse_attr_list(parser)?;
        let modifier_scope = ItemModifierScope::default();
        let modifier = match parser.current_kind() {
            Some(kind) if kind.is_modifier_head() => {
                let modifier_checkpoint = parser.parse_cp(modifier_scope.clone(), None).unwrap();
                checkpoint.get_or_insert(modifier_checkpoint);
                modifier_scope.kind.get()
            }
            _ => ModifierKind::None,
        };

        if modifier.is_unsafe() && parser.current_kind() != Some(FnKw) {
            parser.error("expected `fn` after `unsafe` keyword"); // xxx
        } else if modifier.is_pub() && matches!(parser.current_kind(), Some(ImplKw | ExternKw)) {
            let error_msg = format!(
                "`pub` can't be used for `{}`",
                parser.current_token().unwrap().text()
            );
            parser.error(&error_msg);
        }

        match parser.current_kind() {
            Some(ModKw) => parser.parse_cp(ModScope::default(), checkpoint),
            Some(FnKw) => parser.parse_cp(FuncScope::default(), checkpoint),
            Some(StructKw) => parser.parse_cp(super::struct_::StructScope::default(), checkpoint),
            Some(ContractKw) => parser.parse_cp(ContractScope::default(), checkpoint),
            Some(EnumKw) => parser.parse_cp(EnumScope::default(), checkpoint),
            Some(TraitKw) => parser.parse_cp(TraitScope::default(), checkpoint),
            Some(ImplKw) => parser.parse_cp(ImplScope::default(), checkpoint),
            Some(UseKw) => parser.parse_cp(UseScope::default(), checkpoint),
            Some(ConstKw) => parser.parse_cp(ConstScope::default(), checkpoint),
            Some(ExternKw) => parser.parse_cp(ExternScope::default(), checkpoint),
            Some(TypeKw) => parser.parse_cp(TypeAliasScope::default(), checkpoint),
            tok => parser
                .error_and_recover(&format! {"expected item: but got {tok:?}"})
                .map(|_| checkpoint.unwrap_or_else(|| parser.checkpoint())),
        }?;

        parser.set_newline_as_trivia(false);
        if parser.current_kind().is_some() && !parser.bump_if(SyntaxKind::Newline) {
            // xxx
            parser.bump_or_recover(
                SyntaxKind::Newline,
                "expected newline after item definition",
            )
        } else {
            Ok(())
        }
    }
}

define_scope! {
    ItemModifierScope {kind: Rc<Cell<ModifierKind>>},
    ItemModifier,
    Inheritance
}
impl super::Parse for ItemModifierScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
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
        Ok(())
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
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ModKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::LBrace]);

        if parser.find_and_pop(SyntaxKind::Ident, None)? {
            parser.bump();
        }
        if parser.find_and_pop(SyntaxKind::LBrace, Some("`mod` body"))? {
            parser.parse(ItemListScope::new(true))?;
        }
        Ok(())
    }
}

define_scope! { ContractScope, Contract, Inheritance }
impl super::Parse for ContractScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ContractKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::LBrace]);

        if parser.find_and_pop(SyntaxKind::Ident, None)? {
            parser.bump();
        }
        if parser.find_and_pop(SyntaxKind::LBrace, Some("contract field definition"))? {
            parser.parse(RecordFieldDefListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { EnumScope, Enum, Inheritance }
impl super::Parse for EnumScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::EnumKw);

        parser.set_scope_recovery_stack(&[
            SyntaxKind::Ident,
            SyntaxKind::Lt,
            SyntaxKind::WhereKw,
            SyntaxKind::LBrace,
        ]);

        if parser.find_and_pop(SyntaxKind::Ident, None)? {
            parser.bump();
        }

        parser.pop_recovery_stack();
        parse_generic_params_opt(parser, false)?;

        parser.pop_recovery_stack();
        parse_where_clause_opt(parser)?;

        if parser.find_and_pop(SyntaxKind::LBrace, Some("enum body definition"))? {
            parser.parse(VariantDefListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { VariantDefListScope, VariantDefList, Override(Comma, RBrace) }
impl super::Parse for VariantDefListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            true,
            (SyntaxKind::LBrace, SyntaxKind::RBrace),
            |parser| parser.parse(VariantDefScope::default()),
        )
    }
}

define_scope! { VariantDefScope, VariantDef, Inheritance }
impl super::Parse for VariantDefScope {
    type Error = Recovery<ErrProof>;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        // xxx check for keywords when bumping ident
        parser.bump_or_recover(SyntaxKind::Ident, "expected ident for the variant name")?;

        if parser.current_kind() == Some(SyntaxKind::LParen) {
            parser.parse(TupleTypeScope::default())?;
        } else if parser.current_kind() == Some(SyntaxKind::LBrace) {
            parser.parse(RecordFieldDefListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { TraitScope, Trait, Inheritance }
impl super::Parse for TraitScope {
    type Error = Recovery<ErrProof>;

    // xxx add TraitSignature syntax kind

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::TraitKw);
        parser.set_scope_recovery_stack(&[
            SyntaxKind::Ident,
            SyntaxKind::Lt,
            SyntaxKind::Colon,
            SyntaxKind::WhereKw,
            SyntaxKind::LBrace,
        ]);
        if parser.find_and_pop(
            SyntaxKind::Ident,
            Some("expected identifier for the trait name"),
        )? {
            parser.bump();
        }

        parser.expect_and_pop_recovery_stack()?;
        parse_generic_params_opt(parser, false)?;

        parser.expect_and_pop_recovery_stack()?;
        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.parse(SuperTraitListScope::default())?;
        }

        parser.expect_and_pop_recovery_stack()?;
        parse_where_clause_opt(parser)?;

        if parser.find(SyntaxKind::LBrace, Some("missing trait body"))? {
            parser.parse(TraitItemListScope::default())?;
        }
        Ok(())
    }
}

define_scope! {SuperTraitListScope, SuperTraitList, Inheritance(Plus)} // xxx remove Plus?
impl super::Parse for SuperTraitListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::Colon);
        parser.parse(TraitRefScope::default())?;
        while parser.bump_if(SyntaxKind::Plus) {
            parser.parse(TraitRefScope::default())?;
        }
        Ok(())
    }
}

define_scope! { TraitItemListScope, TraitItemList, Override(RBrace, Newline, FnKw) }
impl super::Parse for TraitItemListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_fn_item_block(parser, false, FuncDefScope::TraitDef)
    }
}

define_scope! { ImplScope, Impl, Override(ForKw, LBrace) }
impl super::Parse for ImplScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ImplKw);

        parse_generic_params_opt(parser, false)?;

        let is_impl_trait = parser.dry_run(|parser| {
            // xxx reconsider, with new changes to parses_without_error
            // We don't use `parses_without_error` here, because it's too strict;
            // we only care whether the token after the trait/type is `for`.
            let _ = parse_type(parser, None);
            parser.bump_if(SyntaxKind::ForKw)
        });

        if is_impl_trait {
            self.set_kind(SyntaxKind::ImplTrait);
            parser.set_scope_recovery_stack(&[
                SyntaxKind::ForKw,
                SyntaxKind::WhereKw,
                SyntaxKind::LBrace,
            ]);

            parser.parse(TraitRefScope::default())?;
            parser.pop_recovery_stack();
            parser.bump_expected(SyntaxKind::ForKw);
        } else {
            parser.set_scope_recovery_stack(&[SyntaxKind::WhereKw, SyntaxKind::LBrace]);
        }

        parse_type(parser, None)?;

        parser.expect_and_pop_recovery_stack()?;
        parse_where_clause_opt(parser)?;

        if parser.find_and_pop(SyntaxKind::LBrace, Some("impl body"))? {
            if is_impl_trait {
                parser.parse(ImplTraitItemListScope::default())?;
            } else {
                parser.parse(ImplItemListScope::default())?;
            }
        }
        Ok(())
    }
}

define_scope! { ImplTraitItemListScope, ImplTraitItemList, Override(RBrace, FnKw) }
impl super::Parse for ImplTraitItemListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_fn_item_block(parser, false, FuncDefScope::Impl)
    }
}

define_scope! { ImplItemListScope, ImplItemList, Override(RBrace, FnKw) }
impl super::Parse for ImplItemListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_fn_item_block(parser, true, FuncDefScope::Impl)
    }
}

define_scope! { UseScope, Use, Inheritance }
impl super::Parse for UseScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::UseKw);
        parser.parse(UseTreeScope::default())
    }
}

define_scope! { ConstScope, Const, Inheritance }
impl super::Parse for ConstScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ConstKw);

        parser.set_newline_as_trivia(false);
        parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::Colon, SyntaxKind::Eq]);

        if parser.find_and_pop(SyntaxKind::Ident, None)? {
            parser.bump();
        }
        if parser.find_and_pop(SyntaxKind::Colon, None)? {
            parser.bump();
            parse_type(parser, None)?;
        }
        if parser.find_and_pop(
            SyntaxKind::Eq,
            Some("expected `=` for const value definition"),
        )? {
            parser.bump();
            parse_expr(parser)?;
        }
        Ok(())
    }
}

define_scope! { ExternScope, Extern, Inheritance }
impl super::Parse for ExternScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ExternKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::LBrace]);
        if parser.find(SyntaxKind::LBrace, Some("missing `extern` body"))? {
            parser.parse(ExternItemListScope::default())?;
        }
        Ok(())
    }
}

define_scope! { ExternItemListScope, ExternItemList, Override(PubKw, UnsafeKw, FnKw) }
impl super::Parse for ExternItemListScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_fn_item_block(parser, true, FuncDefScope::Extern)
    }
}

define_scope! { TypeAliasScope, TypeAlias, Inheritance }
impl super::Parse for TypeAliasScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::TypeKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::Ident, SyntaxKind::Lt, SyntaxKind::Eq]);
        if parser.find_and_pop(
            SyntaxKind::Ident,
            Some("expected identifier for type alias name"),
        )? {
            parser.bump();
        }

        parser.pop_recovery_stack();
        parse_generic_params_opt(parser, true)?;

        if parser.find_and_pop(
            SyntaxKind::Eq,
            Some("expected `=` for type alias definition"),
        )? {
            parser.bump();
            parse_type(parser, None)?;
        }
        Ok(())
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
) -> Result<(), Recovery<ErrProof>> {
    parser.bump_expected(SyntaxKind::LBrace);
    loop {
        parser.set_newline_as_trivia(true);
        if matches!(parser.current_kind(), Some(SyntaxKind::RBrace) | None) {
            break;
        }

        let mut checkpoint = attr::parse_attr_list(parser)?;

        let is_modifier = |kind: Option<SyntaxKind>| match kind {
            Some(kind) => kind.is_modifier_head(),
            _ => false,
        };

        if is_modifier(parser.current_kind()) {
            if allow_modifier {
                let modifier_checkpoint = parser
                    .parse_cp(ItemModifierScope::default(), None)
                    .unwrap_infallible();
                checkpoint.get_or_insert(modifier_checkpoint);
            } else {
                while is_modifier(parser.current_kind()) {
                    parser.unexpected_token_error("modifier is not allowed in this block", None);
                }
            }
        }

        match parser.current_kind() {
            Some(SyntaxKind::FnKw) => {
                parser.parse_cp(FuncScope::new(fn_def_scope), checkpoint)?;

                parser.set_newline_as_trivia(false);
                parser.expect(
                    &[SyntaxKind::Newline, SyntaxKind::RBrace],
                    Some("expected newline after item definition"),
                )?;
            }
            _ => {
                let proof = parser.error_msg_on_current_token("only `fn` is allowed in this block");
                parser.try_recover().map_err(|r| r.add_err_proof(proof))?;
            }
        }
    }

    parser.bump_or_recover(SyntaxKind::RBrace, "expected `}` to close the block")
}
