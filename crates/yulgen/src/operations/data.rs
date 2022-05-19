use crate::operations::abi as abi_operations;
use crate::types::{AbiType, EvmSized};
use fe_analyzer::namespace::types::Array;
use yultsur::*;

/// Loads a value of the given type from storage.
pub fn sload(typ: Box<dyn EvmSized>, sptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (typ.size()) };
    expression! { bytes_sloadn([sptr], [size]) }
}

/// Stores a value of the given type in storage.
pub fn sstore(
    typ: Box<dyn EvmSized>,
    sptr: yul::Expression,
    value: yul::Expression,
) -> yul::Statement {
    let size = literal_expression! { (typ.size()) };
    statement! { bytes_sstoren([sptr], [size], [value]) }
}

/// Loads a value of the given type from memory.
pub fn mload(typ: Box<dyn EvmSized>, mptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (typ.size()) };
    expression! { mloadn([mptr], [size]) }
}

/// Stores a value of the given type in memory.
pub fn mstore(
    typ: Box<dyn EvmSized>,
    mptr: yul::Expression,
    value: yul::Expression,
) -> yul::Statement {
    let size = literal_expression! { (typ.size()) };
    statement! { mstoren([mptr], [size], [value]) }
}

/// Copies a segment of memory into storage.
pub fn mcopys(
    typ: Box<dyn EvmSized>,
    sptr: yul::Expression,
    mptr: yul::Expression,
) -> yul::Statement {
    let size = literal_expression! { (typ.size()) };
    let word_ptr = expression! { div([sptr], 32) };
    statement! { mcopys([mptr], [word_ptr], [size]) }
}

/// Copies a segment of storage into memory.
///
/// Returns the address of the data in memory.
pub fn scopym(typ: Box<dyn EvmSized>, sptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (typ.size()) };
    let word_ptr = expression! { div([sptr], 32) };
    expression! { scopym([word_ptr], [size]) }
}

/// Copies a segment of storage to another segment of storage.
pub fn scopys(
    typ: Box<dyn EvmSized>,
    dest_ptr: yul::Expression,
    origin_ptr: yul::Expression,
) -> yul::Statement {
    let size = literal_expression! { (typ.size()) };
    let origin_word = expression! { div([origin_ptr], 32) };
    let dest_word = expression! { div([dest_ptr], 32) };
    statement! { scopys([origin_word], [dest_word], [size]) }
}

/// Copies a segment of memory to another segment of memory.
pub fn mcopym(typ: Box<dyn EvmSized>, ptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (typ.size()) };
    expression! { mcopym([ptr], [size]) }
}

/// Logs an event.
pub fn emit_event(
    event_name: &str,
    fields: &[(AbiType, bool)], // is_idx
    vals: Vec<yul::Expression>,
) -> yul::Statement {
    // (abi_type, is_idx)
    let topics = {
        // the first topic is the hash of the event signature
        let topic_0 = fe_abi::utils::event_topic(
            event_name,
            &fields
                .iter()
                .map(|(abi_type, _)| abi_type.selector_name())
                .collect::<Vec<_>>(),
        );
        let mut topics = vec![literal_expression! { (topic_0) }];

        // Types will be relevant here when we implement indexed array values.
        // For now we assume these are all base type values and therefore do not need to
        // be hashed.
        let mut idx_field_vals = fields
            .iter()
            .zip(vals.iter())
            .filter_map(|((_field_type, is_idx), val)| is_idx.then(|| val.clone()))
            .collect::<Vec<_>>();

        topics.append(&mut idx_field_vals);
        topics
    };

    let (non_idx_field_types, non_idx_field_vals): (Vec<_>, Vec<_>) = fields
        .iter()
        .zip(vals.iter())
        .filter_map(|((abi_type, is_idx), val)| (!is_idx).then(|| (abi_type.clone(), val.clone())))
        .unzip();

    let encoding_size = abi_operations::encoding_size(&non_idx_field_types, &non_idx_field_vals);
    let encoding = abi_operations::encode(&non_idx_field_types, non_idx_field_vals);

    let log_func = identifier! { (format!("log{}", topics.len())) };

    return statement! { [log_func]([encoding], [encoding_size], [topics...]) };
}

/// Sums a list of expressions using nested add operations.
pub fn sum(vals: Vec<yul::Expression>) -> yul::Expression {
    if vals.is_empty() {
        return expression! { 0 };
    }

    vals.into_iter()
        .reduce(|val1, val2| expression! { add([val1], [val2]) })
        .unwrap()
}

/// Hashes the storage nonce of a map with a key to determine the value's
/// location in storage.
pub fn keyed_map(map: yul::Expression, key: yul::Expression) -> yul::Expression {
    expression! { map_value_ptr([map], [key]) }
}

/// Finds the location of an array element base on the element size, element
/// index, and array location.
pub fn indexed_array(
    typ: Array,
    array: yul::Expression,
    index: yul::Expression,
) -> yul::Expression {
    let inner_size = literal_expression! { (typ.inner.size()) };
    let array_length = literal_expression! { (typ.size) };
    expression! { get_array_item([array], [array_length], [index], [inner_size] ) }
}
