use std::{
    env,
    fmt::Display,
    fs::{self, File},
    io::Write,
    process::Command,
    sync::{Arc, Mutex},
    thread,
};

use indexmap::{indexmap, IndexMap};
use smol_str::SmolStr;

const KSPEC_TEMPLATE: &str = include_str!("../templates/spec.k");
const KACCT_TEMPLATE: &str = include_str!("../templates/account.k");

#[derive(Clone)]
pub enum KSymbol {
    Name(SmolStr),
    None,
}

impl Display for KSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KSymbol::Name(name) => write!(f, "{}", name),
            KSymbol::None => write!(f, "_"),
        }
    }
}

#[derive(Clone)]
pub enum KInt {
    Literal(usize),
    // a ^ b
    Pow { base: Box<KInt>, exp: Box<KInt> },
}

impl KInt {
    const TWO: Self = KInt::Literal(2);

    pub fn literal(value: usize) -> Self {
        Self::Literal(value)
    }

    pub fn pow_2(exp: usize) -> Self {
        let exp = Self::Literal(exp);

        Self::Pow {
            base: Box::new(Self::TWO),
            exp: Box::new(exp),
        }
    }
}

impl Display for KInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KInt::Literal(value) => write!(f, "{}", value),
            KInt::Pow { base, exp } => write!(f, "{} ^Int {}", base, exp),
        }
    }
}

#[derive(Clone)]
pub enum KBuf {
    Single { size: KInt, symbol: KSymbol },
    Concatenated { inners: Vec<KBuf> },
}

impl Display for KBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KBuf::Single { size, symbol } => {
                write!(f, "#buf({}, {})", size, symbol)
            }
            KBuf::Concatenated { inners } => {
                let s = inners
                    .iter()
                    .map(|inner| inner.to_string())
                    .collect::<Vec<_>>()
                    .join(" +Bytes ");
                write!(f, "{}", s)
            }
        }
    }
}

#[derive(Clone)]
pub enum KBool {
    And { a: Box<KBool>, b: Box<KBool> },
    Or { a: Box<KBool>, b: Box<KBool> },
    Gte { a: KInt, b: KInt },
    Lt { a: KInt, b: KInt },
}

impl Display for KBool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KBool::And { a, b } => write!(f, "{} andBool {}", a, b),
            KBool::Or { a, b } => write!(f, "{} orBool {}", a, b),
            KBool::Gte { a, b } => write!(f, "{} >=Int {}", a, b),
            KBool::Lt { a, b } => write!(f, "{} <Int {}", a, b),
        }
    }
}

#[derive(Clone)]
pub struct KSet {
    members: Vec<KSymbol>,
}

impl KSet {
    pub fn new() -> Self {
        KSet { members: vec![] }
    }

    pub fn insert(&mut self, name: KSymbol) {
        self.members.push(name)
    }
}

impl Display for KSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} _:Set",
            self.members
                .iter()
                .map(|symbol| format!("SetItem({})", symbol))
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}

#[derive(Clone)]
struct KAccount {
    name: KSymbol,
    code: String,
}

impl Display for KAccount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            KACCT_TEMPLATE
                .replace("$ACCT_ID", &format!("{}", self.name))
                .replace("$CODE", &format!("\"0x{}\"", self.code))
        )
    }
}

#[derive(Clone)]
pub struct KSpec {
    local_mem: KBuf,
    code: String,
    origin: KSymbol,
    accounts: Vec<KAccount>,
    requirements: KBool,
}

fn account_symbol_set(accounts: &[KAccount]) -> KSet {
    let mut set = KSet::new();

    for account in accounts {
        set.insert(account.name.clone())
    }

    set
}

impl Display for KSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let active_accounts = account_symbol_set(&self.accounts);
        let accounts = self
            .accounts
            .iter()
            .map(|account| account.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        write!(
            f,
            "{}",
            KSPEC_TEMPLATE
                .replace("$LOCAL_MEM", &format!("{}", self.local_mem))
                .replace("$CODE", &format!("\"0x{}\"", self.code))
                .replace("$ORIGIN", &format!("{}", self.origin))
                .replace("$ACTIVE_ACCOUNTS", &format!("{}", active_accounts))
                .replace("$ACCOUNTS", &format!("{}", accounts))
                .replace("$REQUIREMENTS", &format!("{}", self.requirements))
        )
    }
}

impl KSpec {
    pub fn execute(&self, fs_id: &str) -> bool {
        let kevm_dir_path = env::var("KEVM_DIR").unwrap();
        let spec_dir_path = format!("{kevm_dir_path}/tests/specs/fe/{fs_id}");
        let spec_file_path = format!("{spec_dir_path}/invariant-spec.k");
        let log_file_path = format!("{fs_id}.out");

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
        let log = File::create(log_file_path).expect("failed to open log");

        cmd.arg("build")
            .arg("test-prove-fe")
            .arg("-j8")
            .current_dir(kevm_dir_path)
            .env("FS_ID", fs_id)
            .stdout(log)
            .status()
            .unwrap()
            .success()
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
        // todo: actually terminate the process isntead of just forgetting about it
        self.cur_executing.remove(&id);
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
