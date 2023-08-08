fn main() {
    #[cfg(test)]
    println!("cargo:rerun-if-changed=./fixtures");
}
