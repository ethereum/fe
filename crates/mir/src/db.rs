use std::rc::Rc;

use crate::ir;

#[salsa::query_group(MirDbStorage)]
pub trait MirDb {
    #[salsa::interned]
    fn intern_module(&self, data: Rc<ir::Module>) -> ir::ModuleId;
    #[salsa::interned]
    fn intern_const(&self, data: Rc<ir::Constant>) -> ir::ConstantId;
    #[salsa::interned]
    fn intern_type(&self, data: Rc<ir::Type>) -> ir::TypeId;
    #[salsa::interned]
    fn intern_function(&self, data: Rc<ir::FunctionSignature>) -> ir::FunctionId;
}
