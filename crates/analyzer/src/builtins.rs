use strum::{AsRefStr, EnumIter, EnumString};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, EnumString, AsRefStr)]
#[strum(serialize_all = "snake_case")]
pub enum ValueMethod {
    Clone,
    ToMem,
    AbiEncode,
}

#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString, AsRefStr, EnumIter,
)]
#[strum(serialize_all = "snake_case")]
pub enum GlobalFunction {
    Keccak256,
    SendValue,
    Balance,
    BalanceOf,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, EnumString, AsRefStr)]
#[strum(serialize_all = "snake_case")]
pub enum ContractTypeMethod {
    Create,
    Create2,
}

impl ContractTypeMethod {
    pub fn arg_count(&self) -> usize {
        match self {
            ContractTypeMethod::Create => 1,
            ContractTypeMethod::Create2 => 2,
        }
    }
}

#[derive(
    Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash, EnumString, EnumIter, AsRefStr,
)]
#[strum(serialize_all = "lowercase")]
pub enum GlobalObject {
    Block,
    Chain,
    Msg,
    Tx,
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
pub enum ContractSelfField {
    Address,
}
