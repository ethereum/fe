use yultsur::*;

/// Executes the `create2` operation for a given contract with the given value
/// and salt.
pub fn create2(name: &str, value: yul::Expression, salt: yul::Expression) -> yul::Expression {
    let name = literal_expression! { (format!("\"{}\"", name)) };
    expression! {
        contract_create2(
            (dataoffset([name.clone()])),
            (datasize([name])),
            [value],
            [salt]
        )
    }
}

/// Executes the `create` operation for a given contract with the given value.
pub fn create(name: &str, value: yul::Expression) -> yul::Expression {
    let name = literal_expression! { (format!("\"{}\"", name)) };
    expression! {
        contract_create(
            (dataoffset([name.clone()])),
            (datasize([name])),
            [value]
        )
    }
}
