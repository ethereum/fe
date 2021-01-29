use strum::EnumString;

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum Method {
    Clone,
    ToMem,
    Keccak256,
    AbiEncode,
    AbiEncodePacked,
}

#[derive(Debug, PartialEq, EnumString)]
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
    Data,
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
