use strum::EnumString;

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum Object {
    Block,
    Chain,
    Msg,
    #[strum(serialize = "self")]
    Self_,
}

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
pub enum MsgField {
    Data,
    Sender,
    Sig,
    Value,
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum BlockField {
    Coinbase,
    Difficulty,
    Number,
    Timestamp,
}

#[derive(Debug, PartialEq, EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum ChainField {
    Id,
}
