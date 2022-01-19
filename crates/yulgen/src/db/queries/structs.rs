use crate::db::YulgenDb;
use crate::types::{AbiType, AsAbiType, EvmSized};
use fe_analyzer::namespace::items::{Item, StructId, TypeDef};
use fe_analyzer::namespace::types::FixedSize;
use smol_str::SmolStr;
use std::rc::Rc;
use yultsur::*;

pub fn struct_field_abi_types(db: &dyn YulgenDb, struct_: StructId) -> Rc<[AbiType]> {
    let db = db.upcast();
    struct_
        .fields(db)
        .values()
        .map(|field| {
            field
                .typ(db)
                .expect("struct field type error")
                .as_abi_type(db)
        })
        .collect::<Vec<_>>()
        .into()
}

pub fn struct_abi_type(db: &dyn YulgenDb, struct_: StructId) -> AbiType {
    let components = db.struct_field_abi_types(struct_).to_vec();
    AbiType::Tuple { components }
}

pub fn struct_qualified_name(db: &dyn YulgenDb, struct_: StructId) -> SmolStr {
    // foo::Bar => $$foo$Bar
    format!(
        "$${}",
        Item::Type(TypeDef::Struct(struct_))
            .path(db.upcast())
            .join("$")
    )
    .into()
}

pub fn struct_getter_name(
    db: &dyn YulgenDb,
    struct_: StructId,
    field: SmolStr,
    deref: bool,
) -> SmolStr {
    let suffix = if deref { "" } else { "_raw" };
    format!(
        "{}.get_{}_ptr{}",
        db.struct_qualified_name(struct_),
        field,
        suffix
    )
    .into()
}

pub fn struct_getter_fn(
    db: &dyn YulgenDb,
    struct_: StructId,
    field: SmolStr,
    deref: bool,
) -> yul::Statement {
    let fields = struct_.fields(db.upcast());

    let (index, _, field_id) = fields
        .get_full(field.as_str())
        .expect("invalid struct field name");

    let field_type = field_id.typ(db.upcast()).expect("struct field error");
    // The value of each field occupies 32 bytes. This includes values with sizes
    // less than 32 bytes. So, when we get the pointer to the value of a struct
    // field, we must take into consideration the left-padding. The left-padding is
    // equal to the difference between the value's size and 32 bytes, so we end up
    // adding the word offset and the byte offset.
    let field_offset = if !field_type.is_base() {
        // For now we just assume that non-base types are always stored as references and so the size of the field
        // is always of the size of a pointer (32 bytes)
        index * 32
    } else if field_type.size() < 32 {
        index * 32 + (32 - field_type.size())
    } else {
        index * field_type.size()
    };

    let function_name = identifier! { (db.struct_getter_name(struct_, field, deref)) };
    let offset = literal_expression! { (field_offset) };

    let normal_getter = function_definition! {
        function [function_name.clone()](ptr) -> return_val {
            (return_val := add(ptr, [offset.clone()]))
        }
    };

    let deref_getter = function_definition! {
        function [function_name](ptr) -> return_val {
            (return_val := mload((add(ptr, [offset]))))
        }
    };

    match field_type {
        // We generate two different kind of getters:
        // 1. get_x_ptr:     Returns a pointer to the memory location where the data is set.
        //                   If `x` is a reference type, we will have to perform one dereferenceing step
        //                   to get to that actual pointer. That is because the field contains a 32 byte
        //                   reference itself.
        // 2. get_x_ptr_raw: Returns a pointer that does not automatically dereference contained references.
        //                   This is useful for assignments where one wants to override a field `foo` of a struct
        //                   that might be of type Array<u256, 2> with an entirely new array e.g. val.foo = [100, 200]
        //                   In that case, we don't want to follow the stored reference because we want to override
        //                   it entirely.
        FixedSize::Base(_) => normal_getter,
        _ => {
            if deref {
                deref_getter
            } else {
                normal_getter
            }
        }
    }
}

pub fn struct_init_name(db: &dyn YulgenDb, struct_: StructId) -> SmolStr {
    format!("{}.new", db.struct_qualified_name(struct_)).into()
}

pub fn struct_init_fn(db: &dyn YulgenDb, struct_: StructId) -> yul::Statement {
    let function_name = identifier! { (db.struct_init_name(struct_)) };
    let fields = struct_.fields(db.upcast());

    if fields.is_empty() {
        // We return 0 here because it is safe to assume that we never write to an empty
        // struct. If we end up writing to an empty struct that's an actual Fe
        // bug.
        return function_definition! {
            function [function_name]() -> return_val {
                (return_val := 0)
            }
        };
    }

    let params = fields
        .keys()
        .map(|name| {
            identifier! {(name)}
        })
        .collect::<Vec<_>>();

    let body = fields
        .iter()
        .enumerate()
        .map(|(index, (name, _))| {
            if index == 0 {
                let param_identifier_exp = identifier_expression! {(name)};
                statements! {
                    (return_val := alloc(32))
                        (mstore(return_val, [param_identifier_exp]))
                }
            } else {
                let ptr_identifier = format!("{}_ptr", name);
                let ptr_identifier = identifier! {(ptr_identifier)};
                let ptr_identifier_exp = identifier_expression! {(ptr_identifier)};
                let param_identifier_exp = identifier_expression! {(name)};
                statements! {
                    (let [ptr_identifier] := alloc(32))
                        (mstore([ptr_identifier_exp], [param_identifier_exp]))
                }
            }
        })
        .flatten()
        .collect::<Vec<_>>();

    function_definition! {
        function [function_name]([params...]) -> return_val {
            [body...]
        }
    }
}

pub fn struct_api_fns(db: &dyn YulgenDb, struct_: StructId) -> Vec<yul::Statement> {
    [
        vec![db.struct_init_fn(struct_)],
        struct_
            .fields(db.upcast())
            .keys()
            .map(|name| db.struct_getter_fn(struct_, name.clone(), false))
            .collect(),
        struct_
            .fields(db.upcast())
            .keys()
            .map(|name| db.struct_getter_fn(struct_, name.clone(), true))
            .collect(),
    ]
    .concat()
}
