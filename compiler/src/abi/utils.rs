use fe_common::utils::keccak::get_partial_signature;

/// Formats the name and fields and calculates the 32 byte keccak256 value of
/// the signature.
pub fn event_topic(name: String, fields: Vec<String>) -> String {
    sign_event_or_func(name, fields, 32)
}
/// Formats the name and params and calculates the 4 byte keccak256 value of the
/// signature.
pub fn func_selector(name: String, params: Vec<String>) -> String {
    sign_event_or_func(name, params, 4)
}

fn sign_event_or_func(name: String, params: Vec<String>, size: usize) -> String {
    let signature = format!("{}({})", name, params.join(","));
    get_partial_signature(signature.as_bytes(), size)
}
