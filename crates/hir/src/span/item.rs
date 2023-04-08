use parser::ast;

use crate::hir_def::{
    Body, Const, Contract, Enum, ExternFunc, Func, Impl, ImplTrait, Mod, Struct, TopLevelMod,
    Trait, TypeAlias, Use,
};

use super::{
    attr::LazyAttrListSpan,
    define_lazy_span_node,
    params::{LazyFnParamListSpan, LazyGenericParamListSpan, LazyWhereClauseSpan},
    types::{LazyPathTypeSpan, LazyTypeSpan},
    use_tree::LazyUseTreeSpan,
};

define_lazy_span_node!(LazyTopLevelModSpan, ast::Root, new(TopLevelMod),);

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
    ast::Fn,
    new(Func),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (params, params, LazyFnParamListSpan),
        (ret_ty, ret_ty, LazyTypeSpan),
    }
);

define_lazy_span_node!(
    LazyExternFuncSpan,
    ast::Fn,
    new(ExternFunc),
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (params, params, LazyFnParamListSpan),
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
        (use_tree, use_tree, LazyUseTreeSpan),
    }
);

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

        let (file, item_tree) = db.parse_source(text);
        let top_mod = item_tree.top_mod;
        assert_eq!(text, db.text_at(file, &top_mod.lazy_span()));
    }

    #[test]
    fn mod_span() {
        let mut db = TestDb::default();

        let text = r#"
            
            mod foo {
                fn bar() {}
            }
        "#;

        let (file, mod_) = db.parse_source_to_first_item::<Mod>(text);
        let mod_span = mod_.lazy_span();
        assert_eq!(
            r#"mod foo {
                fn bar() {}
            }"#,
            db.text_at(file, &mod_span)
        );
        assert_eq!("foo", db.text_at(file, &mod_span.name()));
    }

    #[test]
    fn fn_span() {
        let mut db = TestDb::default();

        let text = r#"
            fn my_func<T: Debug, U, const LEN: usize>(x: u32, label y: foo::Bar<2>) -> FooResult
                where U: Add
        "#;

        let (file, fn_) = db.parse_source_to_first_item::<Func>(text);
        let fn_span = fn_.lazy_span();
        assert_eq!("my_func", db.text_at(file, &fn_span.name()));

        let generic_params = fn_span.generic_params();
        let type_generic_param_1 = generic_params.param(0).into_type_param();
        let type_generic_param_2 = generic_params.param(1).into_type_param();
        let const_generic_param = generic_params.param(2).into_const_param();

        assert_eq!("T", db.text_at(file, &type_generic_param_1.name()));
        assert_eq!(
            "Debug",
            db.text_at(file, &type_generic_param_1.bounds().bound(0))
        );
        assert_eq!("U", db.text_at(file, &type_generic_param_2.name()));
        assert_eq!(
            "const",
            db.text_at(file, &const_generic_param.const_token())
        );
        assert_eq!("LEN", db.text_at(file, &const_generic_param.name()));
        assert_eq!("usize", db.text_at(file, &const_generic_param.ty()));

        let params = fn_span.params();
        let param_1 = params.param(0);
        let param_2 = params.param(1);

        assert_eq!("x", db.text_at(file, &param_1.name()));
        assert_eq!("u32", db.text_at(file, &param_1.ty()));
        assert_eq!("label", db.text_at(file, &param_2.label()));
        assert_eq!("foo::Bar<2>", db.text_at(file, &param_2.ty()));

        assert_eq!("FooResult", db.text_at(file, &fn_span.ret_ty()));

        let where_clause = fn_span.where_clause();
        let where_predicate = where_clause.predicate(0);
        assert_eq!("where", db.text_at(file, &where_clause.where_token()));
        assert_eq!("U", db.text_at(file, &where_predicate.ty()));
        assert_eq!(": Add", db.text_at(file, &where_predicate.bounds()));
    }

    #[test]
    fn struct_span() {
        let mut db = TestDb::default();

        let text = r#"
            struct Foo {
                x: u32
                pub y: foo::Bar<2>
            }"#;

        let (file, struct_) = db.parse_source_to_first_item::<Struct>(text);
        let struct_span = struct_.lazy_span();
        assert_eq!("Foo", db.text_at(file, &struct_span.name()));

        let fields = struct_span.fields();
        let field_1 = fields.field(0);
        let field_2 = fields.field(1);

        assert_eq!("x", db.text_at(file, &field_1.name()));
        assert_eq!("u32", db.text_at(file, &field_1.ty()));

        assert_eq!("pub", db.text_at(file, &field_2.pub_span()));
        assert_eq!("y", db.text_at(file, &field_2.name()));
        assert_eq!("foo::Bar<2>", db.text_at(file, &field_2.ty()));
    }

    #[test]
    fn enum_span() {
        let mut db = TestDb::default();

        let text = r#"
            enum Foo {
                Bar
                Baz(u32, i32)
            }"#;

        let (file, enum_) = db.parse_source_to_first_item::<Enum>(text);
        let enum_span = enum_.lazy_span();
        assert_eq!("Foo", db.text_at(file, &enum_span.name()));

        let variants = enum_span.variants();
        let variant_1 = variants.variant(0);
        let variant_2 = variants.variant(1);

        assert_eq!("Bar", db.text_at(file, &variant_1.name()));
        assert_eq!("Baz", db.text_at(file, &variant_2.name()));
        assert_eq!("(u32, i32)", db.text_at(file, &variant_2.ty()));
    }

    #[test]
    fn type_alias_span() {
        let mut db = TestDb::default();

        let text = r#"
            pub type Foo = u32
        "#;

        let (file, type_alias) = db.parse_source_to_first_item::<TypeAlias>(text);
        let type_alias_span = type_alias.lazy_span();
        assert_eq!("Foo", db.text_at(file, &type_alias_span.alias()));
        assert_eq!("u32", db.text_at(file, &type_alias_span.ty()));
        assert_eq!("pub", db.text_at(file, &type_alias_span.modifier()));
    }

    #[test]
    fn use_span() {
        let mut db = TestDb::default();

        let text = r#"
            use foo::bar::{baz::*, qux as Alias}
        "#;

        let (file, use_) = db.parse_source_to_first_item::<Use>(text);
        let use_tree = use_.lazy_span().use_tree();

        assert_eq!("foo::bar", db.text_at(file, &use_tree.path()));
        let use_tree_list = use_tree.subtree();
        let use_tree_1 = use_tree_list.tree(0);
        let use_tree_2 = use_tree_list.tree(1);

        assert_eq!("baz::*", db.text_at(file, &use_tree_1.path()));
        assert_eq!("qux", db.text_at(file, &use_tree_2.path()));
        assert_eq!("as Alias", db.text_at(file, &use_tree_2.alias()));
        assert_eq!("Alias", db.text_at(file, &use_tree_2.alias().alias_name()));
    }
}
