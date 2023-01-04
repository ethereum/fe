use tiny_keccak::{Hasher, Keccak};

/// Get the full 32 byte hash of the content.
pub fn full(content: &[u8]) -> String {
    partial(content, 32)
}

/// Take the first `size` number of bytes of the hash and pad the right side
/// with zeros to 32 bytes.
pub fn partial_right_padded(content: &[u8], size: usize) -> String {
    let result = full_as_bytes(content);
    let padded_output: Vec<u8> = result
        .iter()
        .enumerate()
        .map(|(index, byte)| if index >= size { 0 } else { *byte })
        .collect();

    hex::encode(padded_output)
}

/// Take the first `size` number of bytes of the hash with no padding.
pub fn partial(content: &[u8], size: usize) -> String {
    let result = full_as_bytes(content);
    hex::encode(&result[0..size])
}

/// Get the full 32 byte hash of the content as a byte array.
pub fn full_as_bytes(content: &[u8]) -> [u8; 32] {
    let mut keccak = Keccak::v256();
    let mut selector = [0_u8; 32];

    keccak.update(content);
    keccak.finalize(&mut selector);

    selector
}
