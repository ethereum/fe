#![cfg(feature = "kevm-backend")]
use fe_yulgen::Db;
use std::path::Path;
use std::process::Command;
use std::{env, fs};

const SPECS_DIR: &str = "tests/specs/fe/";

pub fn kevm_path() -> String {
    env::var("KEVM_PATH").expect("`KEVM_PATH` not set")
}

pub fn run_spec(name: &str, src_path: &str, src: &str, spec: &str) -> Result<(), String> {
    let kevm_path = kevm_path();

    let spec = build_spec(name, src_path, src, spec);
    let spec_path = Path::new(&kevm_path)
        .join(SPECS_DIR)
        .join(name)
        .with_extension("k");
    fs::write(spec_path.clone(), spec).expect("unable to write file");

    let path = env::var("PATH").expect("PATH is not set");

    let out = Command::new("kevm")
        .arg("prove")
        .arg(spec_path.to_str().unwrap())
        .arg("--backend")
        .arg("haskell")
        .arg("--format-failures")
        .arg("--directory")
        .arg("tests/specs/fe/verification/haskell")
        .env("PATH", format!(".build/usr/bin:{}", path))
        .current_dir(&kevm_path)
        .output()
        .expect("failed to execute process");

    if out.status.code() != Some(0) {
        Err(format!(
            "{}\n{}",
            String::from_utf8_lossy(&out.stderr),
            String::from_utf8_lossy(&out.stdout)
        ))
    } else {
        Ok(())
    }
}

pub fn build_spec(name: &str, src_path: &str, src: &str, spec: &str) -> String {
    let mut db = Db::default();
    let module = fe_driver::compile_single_file(&mut db, src_path, src, true, true).unwrap();

    // replace placeholders
    let mut new_spec = spec.to_owned().replace("$TEST_NAME", &name.to_uppercase());
    for (name, contract) in module.contracts.iter() {
        new_spec = new_spec.replace(
            &format!("${}::RUNTIME", name),
            &format!("\"0x{}\"", contract.runtime_bytecode),
        )
    }

    new_spec
}
