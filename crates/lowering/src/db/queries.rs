use crate::db::LoweringDb;
use crate::mappers;
use fe_analyzer::namespace::items::ModuleId;
use fe_parser::ast;
use std::rc::Rc;

pub fn lowered_module_ast(db: &dyn LoweringDb, module: ModuleId) -> Rc<ast::Module> {
    mappers::module::module(db.upcast(), module).into()
}
