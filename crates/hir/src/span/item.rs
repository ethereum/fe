use parser::ast::{self, prelude::AstNode};

use crate::{
    hir_def::{
        Body, Const, Contract, Enum, Func, Impl, ImplTrait, ItemKind, Mod, Struct, TopLevelMod,
        Trait, TypeAlias, Use,
    },
    span::{
        transition::{LazyArg, LazyTransitionFn, ResolvedOrigin, ResolvedOriginKind},
        use_tree::LazyUsePathSpan,
        DesugaredOrigin, DesugaredUseFocus,
    },
};

use super::{
    attr::LazyAttrListSpan,
    define_lazy_span_node,
    params::{LazyFuncParamListSpan, LazyGenericParamListSpan, LazyWhereClauseSpan},
    transition::SpanTransitionChain,
    types::{LazyPathTypeSpan, LazyTypeSpan},
    use_tree::LazyUseAliasSpan,
};

define_lazy_span_node!(LazyTopModSpan, ast::Root, new(TopLevelMod),);

define_lazy_span_node!(LazyItemSpan);
impl LazyItemSpan {
    pub fn new(item: ItemKind) -> Self {
        Self(SpanTransitionChain::new(item))
    }
}

define_lazy_span_node!(
    LazyModSpan,
    ast::Mod,
    new(Mod),
    @token
    {
        (name, name),
    }
    @node
    {
        (attributes, attr_list, LazyAttrListSpan),
        (modifier, modifier, LazyItemModifierSpan),
    }
);

define_lazy_span_node!(
    LazyFuncSpan,
    ast::Func,
    new(Func),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (params, params, LazyFuncParamListSpan),
        (ret_ty, ret_ty, LazyTypeSpan),
    }
);

define_lazy_span_node!(
    LazyStructSpan,
    ast::Struct,
    new(Struct),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (fields, fields, LazyRecordFieldDefListSpan),
    }
);

define_lazy_span_node!(
    LazyContractSpan,
    ast::Contract,
    new(Contract),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (fields, fields, LazyRecordFieldDefListSpan),
    }
);

define_lazy_span_node!(
    LazyEnumSpan,
    ast::Enum,
    new(Enum),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (variants, variants, LazyEnumVariantListSpan),
    }
);

define_lazy_span_node!(
    LazyTypeAliasSpan,
    ast::TypeAlias,
    new(TypeAlias),
    @token {
        (alias, alias),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_node!(
    LazyImplSpan,
    ast::Impl,
    new(Impl),
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (target_ty, ty, LazyTypeSpan),
    }
);
define_lazy_span_node!(
    LazyTraitSpan,
    ast::Trait,
    new(Trait),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
    }
);

define_lazy_span_node!(
    LazyImplTraitSpan,
    ast::ImplTrait,
    new(ImplTrait),
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (trait_ref, trait_ref, LazyPathTypeSpan),
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_node!(
    LazyConstSpan,
    ast::Const,
    new(Const),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_node!(
    LazyUseSpan,
    ast::Use,
    new(Use),
    @node {
        (attributes, attr_list, LazyAttrListSpan),
    }
);

impl LazyUseSpan {
    pub fn path(&self) -> LazyUsePathSpan {
        self.clone().path_moved()
    }

    pub fn path_moved(mut self) -> LazyUsePathSpan {
        fn f(origin: ResolvedOrigin, _: LazyArg) -> ResolvedOrigin {
            origin
                .map(|node| {
                    ast::Use::cast(node)
                        .and_then(|use_| use_.use_tree())
                        .and_then(|tree| tree.path())
                        .map(|n| n.syntax().clone().into())
                })
                .map_desugared(|root, desugared| match desugared {
                    DesugaredOrigin::Use(mut use_) => {
                        use_.focus = DesugaredUseFocus::Path;
                        ResolvedOriginKind::Desugared(root, DesugaredOrigin::Use(use_))
                    }
                    _ => ResolvedOriginKind::None,
                })
        }

        let lazy_transition = LazyTransitionFn {
            f,
            arg: LazyArg::None,
        };

        self.0.push(lazy_transition);
        LazyUsePathSpan(self.0)
    }

    pub fn alias(&self) -> LazyUseAliasSpan {
        self.clone().alias_moved()
    }

    pub fn alias_moved(mut self) -> LazyUseAliasSpan {
        fn f(origin: ResolvedOrigin, _: LazyArg) -> ResolvedOrigin {
            origin
                .map(|node| {
                    ast::Use::cast(node)
                        .and_then(|use_| use_.use_tree())
                        .and_then(|tree| tree.alias())
                        .map(|n| n.syntax().clone().into())
                })
                .map_desugared(|root, desugared| match desugared {
                    DesugaredOrigin::Use(mut use_) => {
                        use_.focus = DesugaredUseFocus::Alias;
                        ResolvedOriginKind::Desugared(root, DesugaredOrigin::Use(use_))
                    }
                    _ => ResolvedOriginKind::None,
                })
        }

        let lazy_transition = LazyTransitionFn {
            f,
            arg: LazyArg::None,
        };

        self.0.push(lazy_transition);
        LazyUseAliasSpan(self.0)
    }
}

define_lazy_span_node!(LazyBodySpan, ast::Expr, new(Body),);

define_lazy_span_node!(
    LazyRecordFieldDefListSpan,
    ast::RecordFieldDefList,
    @idx {
        (field, LazyRecordFieldDefSpan),
    }
);

define_lazy_span_node!(
    LazyRecordFieldDefSpan,
    ast::RecordFieldDef,
    @token {
        (pub_span, pub_kw),
        (name, name),
    }
    @node {
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_node!(
    LazyEnumVariantListSpan,
    ast::EnumVariantDefList,
    @idx {
        (variant, LazyEnumVariantSpan),
    }
);

define_lazy_span_node!(
    LazyEnumVariantSpan,
    ast::EnumVariantDef,
    @token {
        (name, name),
    }
    @node {
        (ty, ty, LazyTypeSpan),
    }
);

define_lazy_span_node!(
    LazyItemModifierSpan,
    ast::ItemModifier,
    @token {
        (pub_kw, pub_kw),
        (unsafe_kw, unsafe_kw),
    }
);

#[cfg(test)]
mod tests {
    use crate::{
        hir_def::{Enum, Func, Mod, Struct, TypeAlias, Use},
        test_db::TestDb,
        HirDb,
    };

    #[test]
    fn top_mod_span() {
        let mut db = TestDb::default();

        let text = r#"
            mod foo {
                fn bar() {}
            }
        
            mod baz {
                fn qux() {}
            }
        "#;

        let file = db.standalone_file(text);
        let item_tree = db.parse_source(file);
        let top_mod = item_tree.top_mod;
        assert_eq!(text, db.text_at(top_mod, &top_mod.lazy_span()));
    }

    #[test]
    fn mod_span() {
        let mut db = TestDb::default();

        let text = r#"
            
            mod foo {
                fn bar() {}
            }
        "#;

        let mod_ = db.expect_item::<Mod>(text);
        let top_mod = mod_.top_mod(db.as_hir_db());
        let mod_span = mod_.lazy_span();
        assert_eq!(
            r#"mod foo {
                fn bar() {}
            }"#,
            db.text_at(top_mod, &mod_span)
        );
        assert_eq!("foo", db.text_at(top_mod, &mod_span.name()));
    }

    #[test]
    fn fn_span() {
        let mut db = TestDb::default();

        let text = r#"
            fn my_func<T: Debug, U, const LEN: usize>(x: u32, label y: foo::Bar<2>) -> FooResult
                where U: Add
        "#;

        let fn_ = db.expect_item::<Func>(text);
        let top_mod = fn_.top_mod(db.as_hir_db());
        let fn_span = fn_.lazy_span();
        assert_eq!("my_func", db.text_at(top_mod, &fn_span.name()));

        let generic_params = fn_span.generic_params();
        let type_generic_param_1 = generic_params.param(0).into_type_param();
        let type_generic_param_2 = generic_params.param(1).into_type_param();
        let const_generic_param = generic_params.param(2).into_const_param();

        assert_eq!("T", db.text_at(top_mod, &type_generic_param_1.name()));
        assert_eq!(
            "Debug",
            db.text_at(top_mod, &type_generic_param_1.bounds().bound(0))
        );
        assert_eq!("U", db.text_at(top_mod, &type_generic_param_2.name()));
        assert_eq!(
            "const",
            db.text_at(top_mod, &const_generic_param.const_token())
        );
        assert_eq!("LEN", db.text_at(top_mod, &const_generic_param.name()));
        assert_eq!("usize", db.text_at(top_mod, &const_generic_param.ty()));

        let params = fn_span.params();
        let param_1 = params.param(0);
        let param_2 = params.param(1);

        assert_eq!("x", db.text_at(top_mod, &param_1.name()));
        assert_eq!("u32", db.text_at(top_mod, &param_1.ty()));
        assert_eq!("label", db.text_at(top_mod, &param_2.label()));
        assert_eq!("foo::Bar<2>", db.text_at(top_mod, &param_2.ty()));

        assert_eq!("FooResult", db.text_at(top_mod, &fn_span.ret_ty()));

        let where_clause = fn_span.where_clause();
        let where_predicate = where_clause.predicate(0);
        assert_eq!("where", db.text_at(top_mod, &where_clause.where_token()));
        assert_eq!("U", db.text_at(top_mod, &where_predicate.ty()));
        assert_eq!(": Add", db.text_at(top_mod, &where_predicate.bounds()));
    }

    #[test]
    fn struct_span() {
        let mut db = TestDb::default();

        let text = r#"
            struct Foo {
                x: u32
                pub y: foo::Bar<2>
            }"#;

        let struct_ = db.expect_item::<Struct>(text);
        let top_mod = struct_.top_mod(db.as_hir_db());
        let struct_span = struct_.lazy_span();
        assert_eq!("Foo", db.text_at(top_mod, &struct_span.name()));

        let fields = struct_span.fields();
        let field_1 = fields.field(0);
        let field_2 = fields.field(1);

        assert_eq!("x", db.text_at(top_mod, &field_1.name()));
        assert_eq!("u32", db.text_at(top_mod, &field_1.ty()));

        assert_eq!("pub", db.text_at(top_mod, &field_2.pub_span()));
        assert_eq!("y", db.text_at(top_mod, &field_2.name()));
        assert_eq!("foo::Bar<2>", db.text_at(top_mod, &field_2.ty()));
    }

    #[test]
    fn enum_span() {
        let mut db = TestDb::default();

        let text = r#"
            enum Foo {
                Bar
                Baz(u32, i32)
            }"#;

        let enum_ = db.expect_item::<Enum>(text);
        let top_mod = enum_.top_mod(db.as_hir_db());
        let enum_span = enum_.lazy_span();
        assert_eq!("Foo", db.text_at(top_mod, &enum_span.name()));

        let variants = enum_span.variants();
        let variant_1 = variants.variant(0);
        let variant_2 = variants.variant(1);

        assert_eq!("Bar", db.text_at(top_mod, &variant_1.name()));
        assert_eq!("Baz", db.text_at(top_mod, &variant_2.name()));
        assert_eq!("(u32, i32)", db.text_at(top_mod, &variant_2.ty()));
    }

    #[test]
    fn type_alias_span() {
        let mut db = TestDb::default();

        let text = r#"
            pub type Foo = u32
        "#;

        let type_alias = db.expect_item::<TypeAlias>(text);
        let top_mod = type_alias.top_mod(db.as_hir_db());
        let type_alias_span = type_alias.lazy_span();
        assert_eq!("Foo", db.text_at(top_mod, &type_alias_span.alias()));
        assert_eq!("u32", db.text_at(top_mod, &type_alias_span.ty()));
        assert_eq!("pub", db.text_at(top_mod, &type_alias_span.modifier()));
    }

    #[test]
    fn use_span() {
        let mut db = TestDb::default();

        let text = r#"
            use foo::bar::baz::Trait as _
        "#;

        let use_ = db.expect_item::<Use>(text);

        let top_mod = use_.top_mod(db.as_hir_db());
        let use_span = use_.lazy_span();
        let use_path_span = use_span.path();
        assert_eq!("foo", db.text_at(top_mod, &use_path_span.segment(0)));
        assert_eq!("bar", db.text_at(top_mod, &use_path_span.segment(1)));
        assert_eq!("baz", db.text_at(top_mod, &use_path_span.segment(2)));
        assert_eq!("Trait", db.text_at(top_mod, &use_path_span.segment(3)));
        assert_eq!("as _", db.text_at(top_mod, &use_span.alias()));
        assert_eq!("_", db.text_at(top_mod, &use_span.alias().name()));
    }

    #[test]
    fn use_span_desugared() {
        let mut db = TestDb::default();

        let text = r#"
            use foo::bar::{baz::*, qux as Alias}
        "#;

        let uses = db.expect_items::<Use>(text);
        assert_eq!(uses.len(), 2);

        let top_mod = uses[0].top_mod(db.as_hir_db());

        let use_span = uses[0].lazy_span();
        let use_path_span = use_span.path();
        assert_eq!("foo", db.text_at(top_mod, &use_path_span.segment(0)));
        assert_eq!("bar", db.text_at(top_mod, &use_path_span.segment(1)));
        assert_eq!("qux", db.text_at(top_mod, &use_path_span.segment(2)));
        assert_eq!("as Alias", db.text_at(top_mod, &use_span.alias()));
        assert_eq!("Alias", db.text_at(top_mod, &use_span.alias().name()));

        let use_span = uses[1].lazy_span();
        let use_path_span = use_span.path();
        assert_eq!("foo", db.text_at(top_mod, &use_path_span.segment(0)));
        assert_eq!("bar", db.text_at(top_mod, &use_path_span.segment(1)));
        assert_eq!("baz", db.text_at(top_mod, &use_path_span.segment(2)));
        assert_eq!("*", db.text_at(top_mod, &use_path_span.segment(3)));
    }
}
