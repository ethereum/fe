use crate::YulgenDb;
use fe_analyzer::namespace::items::StructId;
use yultsur::*;

pub fn init(db: &dyn YulgenDb, struct_: StructId, params: Vec<yul::Expression>) -> yul::Expression {
    let function_name = identifier! { (db.struct_init_name(struct_)) };
    expression! { [function_name]([params...]) }
}

pub fn get_attribute(
    db: &dyn YulgenDb,
    struct_: StructId,
    field_name: &str,
    val: yul::Expression,
) -> yul::Expression {
    let function_name = identifier! { (db.struct_getter_name(struct_, field_name.into())) };
    expression! { [function_name]([val]) }
}
