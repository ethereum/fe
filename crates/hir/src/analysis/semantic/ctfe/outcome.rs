use salsa::Update;

use crate::analysis::{
    semantic::{SemOrigin, SemanticInstanceKey},
    ty::{
        assoc_const::{AssocConstUse, InherentConstUse},
        const_expr::ConstExprId,
        ty_def::TyId,
    },
};

use super::{
    machine::CtfeError,
    request::{ConstComputationId, VerifiedConstValueId},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum ConstDemandKind {
    Value,
    Boolean,
    Integer,
    Index,
    ArrayLength,
    EnumVariant,
    Layout,
    CallableSelection,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum ConstDependency<'db> {
    Value(TyId<'db>),
    Type(TyId<'db>),
    Selection(ConstExprId<'db>),
    Computation(ConstComputationId<'db>),
    AssociatedSelection(AssocConstUse<'db>),
    InherentSelection(InherentConstUse<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct BlockedInfo<'db> {
    pub demand: ConstDemandKind,
    pub first_dependency: ConstDependency<'db>,
    pub other_dependencies: Vec<ConstDependency<'db>>,
    pub origin: SemOrigin<'db>,
    pub trace: Vec<SemanticInstanceKey<'db>>,
}

impl<'db> BlockedInfo<'db> {
    pub fn new(
        demand: ConstDemandKind,
        dependency: ConstDependency<'db>,
        origin: SemOrigin<'db>,
    ) -> Self {
        Self {
            demand,
            first_dependency: dependency,
            other_dependencies: Vec::new(),
            origin,
            trace: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum EvalFailure<'db> {
    Ctfe(CtfeError<'db>),
    Invariant {
        origin: SemOrigin<'db>,
        message: String,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum EvalOutcome<'db, T: Update + PartialEq> {
    Ready(T),
    Blocked(BlockedInfo<'db>),
    Failed(EvalFailure<'db>),
}

impl<'db, T: Update + PartialEq> EvalOutcome<'db, T> {
    pub(crate) fn into_result(self) -> EvalResult<'db, T> {
        match self {
            Self::Ready(value) => Ok(value),
            Self::Blocked(info) => Err(EvalStop::blocked(info)),
            Self::Failed(failure) => Err(EvalStop::Failed(failure)),
        }
    }

    pub fn into_ready(self) -> Option<T> {
        match self {
            Self::Ready(value) => Some(value),
            Self::Blocked(_) | Self::Failed(_) => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub(crate) enum EvalStop<'db> {
    // Keep the exceptional propagation path small without boxing cached outcomes.
    Blocked(Box<BlockedInfo<'db>>),
    Failed(EvalFailure<'db>),
}

impl<'db> EvalStop<'db> {
    pub(crate) fn blocked(info: BlockedInfo<'db>) -> Self {
        Self::Blocked(Box::new(info))
    }
}

pub(crate) type EvalResult<'db, T> = Result<T, EvalStop<'db>>;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum FoldMissReason<'db> {
    UnknownRuntimeInput,
    Dependent(BlockedInfo<'db>),
    ReachedFailure(CtfeError<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum FoldAttempt<'db> {
    Folded(VerifiedConstValueId<'db>),
    NotFoldable(FoldMissReason<'db>),
    InvariantFailure(EvalFailure<'db>),
}

impl<'db> From<CtfeError<'db>> for EvalStop<'db> {
    fn from(error: CtfeError<'db>) -> Self {
        Self::Failed(EvalFailure::Ctfe(error))
    }
}

impl<'db, T: Update + PartialEq> From<EvalResult<'db, T>> for EvalOutcome<'db, T> {
    fn from(result: EvalResult<'db, T>) -> Self {
        match result {
            Ok(value) => Self::Ready(value),
            Err(EvalStop::Blocked(info)) => Self::Blocked(*info),
            Err(EvalStop::Failed(failure)) => Self::Failed(failure),
        }
    }
}
