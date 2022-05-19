use crate::operations::data as data_operations;
use crate::types::AsEvmSized;
use crate::YulgenDb;
use fe_analyzer::{context::Location, namespace::items::StructId, namespace::types::Struct};
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
    location: Location,
) -> yul::Expression {
    let function_name = identifier! { (db.struct_getter_name(struct_, field_name.into())) };
    if struct_.is_base_type(db.upcast(), field_name) {
        expression! { [function_name]([val]) }
    } else if matches!(location, Location::Storage { .. }) {
        let index = struct_
            .field_index(db.upcast(), field_name)
            .expect("unknown field");
        let index = literal_expression! { (index)};
        // non-base type fields in storage use the same compile time reference scheme as map types
        data_operations::keyed_map(val, index)
    } else {
        expression! { mload(([function_name]([val]))) }
    }
}

pub fn copy_to_storage(
    db: &dyn YulgenDb,
    struct_: &Struct,
    target: yul::Expression,
    value: yul::Expression,
) -> yul::Statement {
    let yul_body = [
        // We first copy the entire struct from memory to storage *including* the memory references.
        // The memory references are a pointless waste of storage space and are never read or written to.
        // We'll fix that later.
        vec![data_operations::mcopys(
            Box::new(struct_.clone()),
            target.clone(),
            value.clone(),
        )],
        struct_
            .id
            .fields(db.upcast())
            .values()
            .filter_map(|field| {
                if field.is_base_type(db.upcast()) {
                    None
                } else {
                    let typ = field.typ(db.upcast()).expect("not a fixed size");
                    let field_to = get_attribute(
                        db,
                        struct_.id,
                        field.name(db.upcast()).as_str(),
                        target.clone(),
                        Location::Storage { nonce: None },
                    );
                    let field_from = get_attribute(
                        db,
                        struct_.id,
                        field.name(db.upcast()).as_str(),
                        value.clone(),
                        Location::Memory,
                    );
                    // We have to go over all struct fields to copy the actual data of the reference type fields
                    // to their respective location in storage because all we copied so far were useless memory references
                    Some(data_operations::mcopys(
                        typ.as_evm_sized(),
                        field_to,
                        field_from,
                    ))
                }
            })
            .collect(),
    ]
    .concat();

    return block_statement! {
        [yul_body...]
    };
}
