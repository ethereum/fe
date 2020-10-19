use fe_semantics::namespace::events::Event;
use fe_semantics::namespace::types::{
    AbiEncoding,
    Base,
    FeSized,
    FixedSize,
};
use yultsur::*;

/// Loads a value from storage.
///
/// The returned expression evaluates to a 256 bit value.
pub fn sto_to_val<T: FeSized>(typ: T, sptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (typ.size()) };
    expression! { sloadn([sptr], [size]) }
}

/// Copies a segment of storage into memory.
///
/// The returned expression evaluates to a memory pointer.
pub fn sto_to_mem<T: FeSized>(typ: T, sptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (typ.size()) };
    expression! { scopy([sptr], [size]) }
}

/// Stores a 256 bit value in storage.
pub fn val_to_sto<T: FeSized>(
    typ: T,
    sptr: yul::Expression,
    value: yul::Expression,
) -> yul::Statement {
    let size = literal_expression! { (typ.size()) };
    statement! { sstoren([sptr], [value], [size]) }
}

/// Stores a 256 bit value in memory.
pub fn val_to_mem<T: FeSized>(
    typ: T,
    mptr: yul::Expression,
    value: yul::Expression,
) -> yul::Statement {
    let size = literal_expression! { (typ.size()) };
    statement! { mstoren([mptr], [value], [size]) }
}

/// Copies a segment of memory into storage.
pub fn mem_to_sto<T: FeSized>(
    typ: T,
    sptr: yul::Expression,
    mptr: yul::Expression,
) -> yul::Statement {
    let size = literal_expression! { (typ.size()) };
    statement! { mcopy([mptr], [sptr], [size]) }
}

/// Loads a value in memory.
///
/// The returned expression evaluates to a 256 bit value.
pub fn mem_to_val<T: FeSized>(typ: T, mptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (typ.size()) };
    expression! { mloadn([mptr], [size]) }
}

/// Logs an event.
///
/// Note: Currently this only supports events that log a single array value.
pub fn emit_event(event: Event, values: Vec<yul::Expression>) -> yul::Statement {
    if let (Some(FixedSize::Array(array)), Some(value)) = (event.fields.first(), values.first()) {
        let size = literal_expression! {(array.padded_size())};
        let topic = literal_expression! {(event.topic)};

        return statement! { log1([(*value).clone()], [size], [topic]) };
    }

    unimplemented!()
}

/// Encode a sized value.
///
/// Note: This currently only works for base values and u256 and byte arrays.
/// The reason why it only works for these array types is because the tightly
/// packed array format we use is the same as the ABI encoding.
pub fn encode(typ: FixedSize, val: yul::Expression) -> yul::Expression {
    match typ {
        FixedSize::Base(_) => expression! { alloc_mstoren([val], 32) },
        FixedSize::Array(array) => {
            if array.inner == Base::U256 || array.inner == Base::Byte {
                return val;
            }

            unimplemented!()
        }
    }
}

/// Decode a sized value.
///
/// The same note over `decode` holds true for this. We are only able to encode
/// u256 and byte array types.
pub fn decode(typ: FixedSize, ptr: yul::Expression) -> yul::Expression {
    let size = literal_expression! { (typ.size()) };

    match typ {
        FixedSize::Base(_) => expression! { calldataload([ptr]) },
        FixedSize::Array(array) => {
            if array.inner == Base::U256 || array.inner == Base::Byte {
                return expression! { ccopy([ptr], [size]) };
            }

            unimplemented!()
        }
    }
}
