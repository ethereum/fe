/// Rounds up to nearest multiple of 32.
pub fn ceil_32(n: usize) -> usize {
    ((n + 31) / 32) * 32
}
