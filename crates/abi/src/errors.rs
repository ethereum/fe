#[derive(Debug)]
pub enum AbiError {
    DuplicateContractDefinition(String),
    SerializationFailed,
}
