use crate::yul::operations::abi as abi_operations;
use fe_analyzer::namespace::events::Event;
use fe_analyzer::namespace::types::{
    Array,
    FeSized,
    FixedSize,
};
use yultsur::*;

/// Loads a value from storage.
///
/// The returned expression evaluates to a 256 bit value.
pub fn sload<T: FeSized>(typ: T, sptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (typ.size()) };
    expression! { sloadn([sptr], [size]) }
}

/// Stores a 256 bit value in storage.
pub fn sstore<T: FeSized>(typ: T, sptr: yul::Expression, value: yul::Expression) -> yul::Statement {
    let size = literal_expression! { (typ.size()) };
    statement! { sstoren([sptr], [value], [size]) }
}

/// Stores a 256 bit value in memory.
pub fn mstore<T: FeSized>(typ: T, mptr: yul::Expression, value: yul::Expression) -> yul::Statement {
    let size = literal_expression! { (typ.size()) };
    statement! { mstoren([mptr], [value], [size]) }
}

/// Copies a segment of memory into storage.
pub fn mcopys<T: FeSized>(typ: T, sptr: yul::Expression, mptr: yul::Expression) -> yul::Statement {
    let size = literal_expression! { (typ.size()) };
    statement! { mcopys([mptr], [sptr], [size]) }
}

/// Copies a segment of storage into memory.
///
/// The returned expression evaluates to a memory pointer.
pub fn scopym<T: FeSized>(typ: T, sptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (typ.size()) };
    expression! { scopym([sptr], [size]) }
}

/// Copies a segment of storage to another segment of storage.
pub fn scopys<T: FeSized>(
    typ: T,
    dest_ptr: yul::Expression,
    origin_ptr: yul::Expression,
) -> yul::Statement {
    let size = literal_expression! { (typ.size()) };
    statement! { scopys([origin_ptr], [dest_ptr], [size]) }
}

/// Copies a segment of memory to another segment of memory.
pub fn mcopym<T: FeSized>(typ: T, ptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (typ.size()) };
    expression! { mcopym([ptr], [size]) }
}

/// Loads a value in memory.
///
/// The returned expression evaluates to a 256 bit value.
pub fn mload<T: FeSized>(typ: T, mptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (typ.size()) };
    expression! { mloadn([mptr], [size]) }
}

/// Logs an event.
pub fn emit_event(event: Event, vals: Vec<yul::Expression>) -> yul::Statement {
    let mut topics = vec![literal_expression! { (event.topic) }];

    let (field_vals, field_types): (Vec<yul::Expression>, Vec<FixedSize>) = event
        .non_indexed_fields()
        .into_iter()
        .map(|(index, typ)| (vals[index].to_owned(), typ))
        .unzip();

    // field types will be relevant when we implement indexed array values
    let (mut indexed_field_vals, _): (Vec<yul::Expression>, Vec<FixedSize>) = event
        .indexed_fields()
        .into_iter()
        .map(|(index, typ)| (vals[index].to_owned(), typ))
        .unzip();

    let encoding = abi_operations::encode(field_types.clone(), field_vals);
    let encoding_size = abi_operations::encode_size(field_types, vals);

    // for now we assume these are all base type values and therefore do not need to
    // be hashed
    topics.append(&mut indexed_field_vals);

    let log_func = identifier! { (format!("log{}", topics.len())) };

    return statement! { [log_func]([encoding], [encoding_size], [topics...]) };
}

/// Sums a list of expressions using nested add operations.
pub fn sum(vals: Vec<yul::Expression>) -> yul::Expression {
    if vals.is_empty() {
        return expression! { 0 };
    }

    vals.into_iter()
        .fold_first(|val1, val2| expression! { add([val1], [val2]) })
        .unwrap()
}

/// Hashes the storage nonce of a map with a key to determine the value's
/// location in storage.
pub fn keyed_map(map: yul::Expression, key: yul::Expression) -> yul::Expression {
    expression! { dualkeccak256([map], [key]) }
}

/// Finds the location of an array element base on the element size, element
/// index, and array location.
pub fn indexed_array(
    typ: Array,
    array: yul::Expression,
    index: yul::Expression,
) -> yul::Expression {
    let inner_size = literal_expression! { (typ.inner.size()) };
    expression! { add([array], (mul([index], [inner_size]))) }
}

#[cfg(test)]
mod tests {
    use crate::yul::operations::data::{
        emit_event,
        sum,
    };
    use fe_analyzer::namespace::events::Event;
    use fe_analyzer::namespace::types::{
        Base,
        FixedSize,
        U256,
    };
    use yultsur::*;

    #[test]
    fn test_emit_event_no_indexed() {
        let event = Event::new(
            "MyEvent",
            vec![FixedSize::Base(U256), FixedSize::Base(Base::Address)],
            vec![],
        );

        assert_eq!(
            emit_event(event, vec![expression! { 26 }, expression! { 0x00 }]).to_string(),
            "log1(abi_encode_uint256_address(26, 0x00), add(64, 0), 0x74bffa18f2b20140b65de9264a54040b23ab0a34e7643d52f67f7fb18be9bbcb)"
        )
    }

    #[test]
    fn test_emit_event_one_indexed() {
        let event = Event::new(
            "MyEvent",
            vec![FixedSize::Base(U256), FixedSize::Base(Base::Address)],
            vec![0],
        );

        assert_eq!(
            emit_event(event, vec![expression! { 26 }, expression! { 0x00 }]).to_string(),
            "log2(abi_encode_address(0x00), add(32, 0), 0x74bffa18f2b20140b65de9264a54040b23ab0a34e7643d52f67f7fb18be9bbcb, 26)"
        )
    }

    #[test]
    fn test_sum() {
        assert_eq!(
            sum(vec![
                expression! { 42 },
                expression! { 26 },
                expression! { 22 }
            ])
            .to_string(),
            "add(add(42, 26), 22)"
        )
    }
}
