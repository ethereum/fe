use tiny_keccak::{Hasher, Keccak};

/// Formats the name and fields and calculates the 32 byte keccak256 value of the signature.
pub fn event_topic(name: String, fields: Vec<String>) -> String {
    sig_keccak256(name, fields, 32)
}
/// Formats the name and params and calculates the 4 byte keccak256 value of the signature.
pub fn func_selector(name: String, params: Vec<String>) -> String {
    sig_keccak256(name, params, 4)
}

fn sig_keccak256(name: String, params: Vec<String>, size: usize) -> String {
    let signature = format!("{}({})", name, params.join(","));

    let mut keccak = Keccak::v256();
    let mut selector = [0u8; 32];

    keccak.update(signature.as_bytes());
    keccak.finalize(&mut selector);

    format!("0x{}", hex::encode(&selector[0..size]))
}
