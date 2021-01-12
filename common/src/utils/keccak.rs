use tiny_keccak::{
    Hasher,
    Keccak,
};

pub fn get_full_signature(content: &[u8]) -> String {
    get_partial_signature(content, 32)
}

pub fn get_partial_signature(content: &[u8], size: usize) -> String {
    let mut keccak = Keccak::v256();
    let mut selector = [0u8; 32];

    keccak.update(content);
    keccak.finalize(&mut selector);

    format!("0x{}", hex::encode(&selector[0..size]))
}
