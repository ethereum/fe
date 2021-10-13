use strum::{AsRefStr, EnumIter, EnumString};

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum ValueMethod {
    Clone,
    ToMem,
    AbiEncode,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, EnumString, AsRefStr, Hash, EnumIter)]
#[strum(serialize_all = "snake_case")]
pub enum GlobalMethod {
    Keccak256,
    SendValue,
}

#[derive(Debug, PartialEq, EnumString, AsRefStr)]
#[strum(serialize_all = "snake_case")]
pub enum ContractTypeMethod {
    Create,
    Create2,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, EnumString, EnumIter, AsRefStr)]
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
