use fe_common::utils::keccak;

/// Formats the name and fields and calculates the 32 byte keccak256 value of
/// the signature.
pub fn event_topic(name: &str, fields: &[String]) -> String {
    sign_event_or_func(name, fields, 32)
}
/// Formats the name and params and calculates the 4 byte keccak256 value of the
/// signature.
pub fn func_selector(name: &str, params: &[String]) -> String {
    sign_event_or_func(name, params, 4)
}

fn sign_event_or_func(name: &str, params: &[String], size: usize) -> String {
    let signature = format!("{}({})", name, params.join(","));
    keccak::partial(signature.as_bytes(), size)
}
