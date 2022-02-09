use fe_common::{impl_intern_key, SourceFileId};
use smol_str::SmolStr;

use super::{constant::ConstantId, function::FunctionId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    name: SmolStr,
    file_id: SourceFileId,

    /// Functions defined in a module.
    functions: Vec<FunctionId>,

    /// Constants defined in a module.
    constants: Vec<ConstantId>,
}

/// An interned Id for [`Module`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(u32);
impl_intern_key!(ModuleId);
