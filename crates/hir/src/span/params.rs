use super::SpanTransitionChain;

pub struct LazyGenericParamListSpan(pub(super) SpanTransitionChain);

pub struct LazyGenericArgListSpan(pub(super) SpanTransitionChain);

pub struct LazyWhereClauseSpan(pub(super) SpanTransitionChain);

pub struct TypeGenericParamListSpan(pub(super) SpanTransitionChain);

pub struct LazyFnParamListSpan(pub(super) SpanTransitionChain);
