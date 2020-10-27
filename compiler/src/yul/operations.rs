use crate::yul::runtime::functions;
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
pub fn emit_event(event: Event, vals: Vec<yul::Expression>) -> yul::Statement {
    let size = literal_expression! { (event.fields.iter().fold(0, |size, field| size + field.abi_size())) };
    let topic = literal_expression! { (event.topic) };
    let encoded_val = encode(event.fields, vals);

    return statement! { log1([encoded_val], [size], [topic]) };
}

/// Encode sized values.
pub fn encode(types: Vec<FixedSize>, vals: Vec<yul::Expression>) -> yul::Expression {
    let func_name = functions::abi_encode_name(types.iter().map(|typ| typ.abi_name()).collect());
    expression! { [func_name]([vals...]) }
}

/// Decode a sized value.
///
/// We currently support byte and u256 arrays and all base types.
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

#[cfg(test)]
mod tests {
    use crate::yul::operations::{
        emit_event,
        encode,
    };
    use fe_semantics::namespace::events::Event;
    use fe_semantics::namespace::types::{
        Base,
        FixedSize,
    };
    use yultsur::*;

    #[test]
    fn test_encode() {
        assert_eq!(
            encode(vec![FixedSize::Base(Base::U256)], vec![expression! { 42 }]).to_string(),
            "abi_encode_uint256(42)"
        )
    }

    #[test]
    fn test_emit_event() {
        let event = Event::new(
            "MyEvent".to_string(),
            vec![FixedSize::Base(Base::U256), FixedSize::Base(Base::Address)],
        );

        assert_eq!(
            emit_event(event, vec![expression! { 26 }, expression! { 0x00 }]).to_string(),
            "log1(abi_encode_uint256_address(26, 0x00), 64, 0x74bffa18f2b20140b65de9264a54040b23ab0a34e7643d52f67f7fb18be9bbcb)"
        )
    }
}
