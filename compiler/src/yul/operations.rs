use crate::yul::abi::operations as abi_operations;
use fe_semantics::namespace::events::Event;
use fe_semantics::namespace::types::FeSized;
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
    let topic = literal_expression! { (event.topic) };
    let encoding = abi_operations::encode(event.fields.clone(), vals.clone());
    let size = abi_operations::encode_size(event.fields, vals);

    return statement! { log1([encoding], [size], [topic]) };
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

#[cfg(test)]
mod tests {
    use crate::yul::operations::{
        emit_event,
        sum,
    };
    use fe_semantics::namespace::events::Event;
    use fe_semantics::namespace::types::{
        Base,
        FixedSize,
    };
    use yultsur::*;

    #[test]
    fn test_emit_event() {
        let event = Event::new(
            "MyEvent".to_string(),
            vec![FixedSize::Base(Base::U256), FixedSize::Base(Base::Address)],
        );

        assert_eq!(
            emit_event(event, vec![expression! { 26 }, expression! { 0x00 }]).to_string(),
            "log1(abi_encode_uint256_address(26, 0x00), add(64, 0), 0x74bffa18f2b20140b65de9264a54040b23ab0a34e7643d52f67f7fb18be9bbcb)"
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
