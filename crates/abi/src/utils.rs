use fe_common::utils::keccak;

/// Formats the name and fields and calculates the 32 byte keccak256 value of
/// the signature.
pub fn event_topic(name: &str, fields: &[String]) -> String {
    hash_signature(name, fields, 32)
}

/// Formats the name and params and calculates the 4 byte keccak256 value of the
/// signature.
pub fn func_selector(name: &str, params: &[String]) -> String {
    hash_signature(name, params, 4)
}

fn hash_signature(name: &str, params: &[String], size: usize) -> String {
    let signature = format!("{}({})", name, params.join(","));
    format!("0x{}", keccak::partial(signature.as_bytes(), size))
}
