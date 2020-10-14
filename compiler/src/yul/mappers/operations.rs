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
