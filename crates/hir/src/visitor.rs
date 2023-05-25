#![allow(unused)]
use std::{marker::PhantomData, mem};

use crate::{
    span::{
        item::{
            LazyBodySpan, LazyConstSpan, LazyContractSpan, LazyEnumSpan, LazyFuncSpan,
            LazyImplSpan, LazyImplTraitSpan, LazyItemSpan, LazyModSpan, LazyStructSpan,
            LazyTopModSpan, LazyTraitSpan, LazyTypeAliasSpan, LazyUseSpan,
        },
        DynLazySpan, LazySpan, LazySpanAtom, SpanDowncast,
    },
    HirDb,
};

pub struct SpanCtxt<T>
where
    T: LazySpan,
{
    span: DynLazySpan,
    _t: PhantomData<T>,
}

impl<T> SpanCtxt<T>
where
    T: LazySpan,
{
    fn cast<U: LazySpan>(self) -> SpanCtxt<U> {
        SpanCtxt {
            span: self.span,
            _t: PhantomData,
        }
    }

    fn pop_span(&mut self) {
        self.span.0.pop_transition();
    }

    pub fn current_span(&self) -> Option<T>
    where
        T: SpanDowncast,
    {
        let dyn_span: DynLazySpan = self.span.clone();
        T::downcast(dyn_span)
    }
}

macro_rules! define_ctxt_ctor {
    ($(($hir_ty:ty, $span_ty:ty, $ctor_name:ident),)*) => {
        $(impl SpanCtxt<$span_ty> {
            pub fn $ctor_name(item: $hir_ty) -> Self {
                Self {
                    span: item.lazy_span().into(),
                    _t: PhantomData,
                }
            }
        })*
    };
}

define_ctxt_ctor! {
    (ItemKind, LazyItemSpan, with_item),
    (TopLevelMod, LazyTopModSpan, with_top_mod),
    (Mod, LazyModSpan, with_mod),
    (Func, LazyFuncSpan, with_func),
    (Struct, LazyStructSpan, with_struct),
    (Contract, LazyContractSpan, with_contract),
    (Enum, LazyEnumSpan, with_enum),
    (TypeAlias, LazyTypeAliasSpan, with_type_alias),
    (Impl, LazyImplSpan, with_impl),
    (Trait, LazyTraitSpan, with_trait),
    (ImplTrait, LazyImplTraitSpan, with_impl_trait),
    (Const, LazyConstSpan, with_const),
    (Use, LazyUseSpan, with_use),
    (Body, LazyBodySpan, with_body),
}
