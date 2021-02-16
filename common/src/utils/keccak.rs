use tiny_keccak::{
    Hasher,
    Keccak,
};

pub fn get_full_signature(content: &[u8]) -> String {
    get_partial_signature(content, 32)
}

/// Return the keccak256 hash of the given content as an array of bytes
pub fn get_keccak256(content: &[u8]) -> [u8; 32] {
    let mut keccak = Keccak::v256();
    let mut selector = [0u8; 32];

    keccak.update(content);
    keccak.finalize(&mut selector);

    selector
}

pub fn get_partial_signature(content: &[u8], size: usize) -> String {
    format!("0x{}", hex::encode(&get_keccak256(content)[0..size]))
}
