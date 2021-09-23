use std::str::FromStr;
use strum::{AsRefStr, EnumString, IntoStaticStr};

#[derive(Debug, PartialEq, EnumString, AsRefStr)]
#[strum(serialize_all = "lowercase")]
pub enum ReservedName {
    Type,
    Object,
    // Module, // someday: "std"
    Function,
}

/// Check if a name shadows a built-in type, object, or function.
/// Ideally, some of these things will move into a .fe standard library,
/// and we won't need some of this special name collision logic.
pub fn reserved_name(name: &str) -> Option<ReservedName> {
    if TypeName::from_str(name).is_ok() {
        Some(ReservedName::Type)
    } else if Object::from_str(name).is_ok() {
        Some(ReservedName::Object)
    } else if GlobalMethod::from_str(name).is_ok() {
        Some(ReservedName::Function)
    } else {
        None
    }
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum TypeName {
    // Array, // when syntax is changed
    #[strum(serialize = "Map")]
    Map,
    #[strum(serialize = "String")]
    String_,

    Address,
    Bool,
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum ValueMethod {
    Clone,
    ToMem,
    AbiEncode,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, EnumString, IntoStaticStr, Hash)]
#[strum(serialize_all = "snake_case")]
pub enum GlobalMethod {
    Keccak256,
}

#[derive(strum::ToString, Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum ContractTypeMethod {
    Create,
    Create2,
}

#[derive(strum::ToString, Debug, PartialEq, EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum Object {
    Block,
    Chain,
    Msg,
    Tx,
    #[strum(serialize = "self")]
    Self_,
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum BlockField {
    Coinbase,
    Difficulty,
    Number,
    Timestamp,
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum ChainField {
    Id,
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum MsgField {
    Sender,
    Sig,
    Value,
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum TxField {
    GasPrice,
    Origin,
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum SelfField {
    Address,
}
