use std::{
    env,
    fmt::Display,
    fs,
    io::Write,
    process::Command,
    sync::{Arc, Mutex},
    thread,
};

use indexmap::{indexmap, IndexMap};

const KSPEC_TEMPLATE: &str = include_str!("../template.k");

#[derive(Clone)]
pub enum KSpecType {
    U256,
    U128,
    U64,
    U32,
    U16,
    U8,
    Bool,
    Context,
}

impl KSpecType {
    pub fn bounds(&self, arg_name: &str) -> String {
        match self {
            KSpecType::U256 => {
                format!("andBool 0 <=Int {arg_name} andBool {arg_name} <Int (2 ^Int 256)")
            }
            KSpecType::U128 => {
                format!("andBool 0 <=Int {arg_name} andBool {arg_name} <Int (2 ^Int 128)")
            }
            KSpecType::U64 => {
                format!("andBool 0 <=Int {arg_name} andBool {arg_name} <Int (2 ^Int 64)")
            }
            KSpecType::U32 => {
                format!("andBool 0 <=Int {arg_name} andBool {arg_name} <Int (2 ^Int 32)")
            }
            KSpecType::U16 => {
                format!("andBool 0 <=Int {arg_name} andBool {arg_name} <Int (2 ^Int 16)")
            }
            KSpecType::U8 => {
                format!("andBool 0 <=Int {arg_name} andBool {arg_name} <Int (2 ^Int 8)")
            }
            KSpecType::Bool => format!("andBool (0 ==Int {arg_name} orBool {arg_name} ==Int 1)"),
            KSpecType::Context => {
                format!("andBool 0 ==Int {arg_name}")
            }
        }
    }
}

#[derive(Clone)]
pub struct KSpec {
    args: Vec<KSpecType>,
    code: String,
}

impl KSpec {
    pub fn new(args: Vec<KSpecType>, code: String) -> Self {
        Self { args, code }
    }

    pub fn execute(&self, fs_id: &str) -> bool {
        let kevm_dir_path = env::var("KEVM_DIR").unwrap();
        let spec_dir_path = format!("{kevm_dir_path}/tests/specs/fe/{fs_id}");
        let spec_file_path = format!("{spec_dir_path}/invariant-spec.k");

        if fs::metadata(&spec_dir_path).is_err() {
            fs::create_dir(&spec_dir_path).unwrap();
        }

        let mut file = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(spec_file_path)
            .unwrap();
        file.write_all(self.to_string().as_bytes()).unwrap();

        let mut cmd = Command::new("make");

        cmd.arg("build")
            .arg("test-prove-fe")
            .arg("-j8")
            .current_dir(kevm_dir_path)
            .env("FS_ID", fs_id)
            .status()
            .unwrap()
            .success()
    }
}

impl Display for KSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let calldata = if self.args.len() != 0 {
            (0..self.args.len())
                .map(|n| format!("#buf(32, ARG_{})", n))
                .collect::<Vec<_>>()
                .join(" +Bytes ")
        } else {
            "_ => #buf(0, 0)".to_string()
        };

        let requirements = self
            .args
            .iter()
            .enumerate()
            .map(|(n, typ)| typ.bounds(&format!("ARG_{}", n)))
            .collect::<Vec<_>>()
            .join("\n");

        write!(
            f,
            "{}",
            KSPEC_TEMPLATE
                .replace("$CODE", &format!("\"0x{}\"", &self.code))
                .replace("$CALLDATA", &calldata)
                .replace("$REQUIREMENTS", &requirements)
        )
    }
}

pub struct KSpecExecPool {
    cur_executing: IndexMap<u64, Arc<Mutex<Option<bool>>>>,
}

impl KSpecExecPool {
    pub fn new() -> Self {
        Self {
            cur_executing: indexmap! {},
        }
    }

    pub fn execute_spec(&mut self, id: u64, spec: KSpec) {
        let result = Arc::new(Mutex::new(None));
        let result_clone = Arc::clone(&result);

        thread::spawn(move || {
            let result = spec.execute(&id.to_string());
            *result_clone.lock().unwrap() = Some(result)
        });

        if self.cur_executing.insert(id, result).is_some() {
            panic!("already executing spec for this ID")
        }
    }

    pub fn get_status(&self, id: u64) -> Option<bool> {
        self.cur_executing
            .get(&id)
            .unwrap()
            .lock()
            .unwrap()
            .to_owned()
    }

    pub fn remove(&mut self, id: u64) {
        if self.cur_executing.remove(&id).is_none() {
            panic!("no spec to remove")
        }
    }
}

impl Display for &KSpecExecPool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for id in self.cur_executing.keys() {
            writeln!(f, "{}", id)?
        }

        Ok(())
    }
}

// #[test]
// fn test_returns() {
//     let k_spec = KSpec::new(vec![], "".to_string());
//     assert!(k_spec.execute())
// }
