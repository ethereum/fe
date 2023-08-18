use std::{
    fmt::Display,
    io::{self},
    sync::{Arc, Mutex},
    thread,
    time::{self, Duration},
};

use crossterm::{
    cursor::MoveTo,
    execute,
    terminal::{Clear, ClearType},
};
use fe_proof_service::{symbolic_test::SymbolicTest, ProofStatus};
use indexmap::{indexmap, IndexMap};
use kevm::{KSpec, KSpecExecPool};
use smol_str::SmolStr;

mod db;

use self::db::Db;

pub struct Server {
    state: Arc<Mutex<ServerState>>,
}

impl Server {
    pub fn new(db_path: &str, max_proofs: usize) -> Self {
        let state = Arc::new(Mutex::new(ServerState::new(db_path)));
        let state_clone = Arc::clone(&state);

        thread::spawn(move || loop {
            thread::sleep(Duration::from_millis(100));

            if let Ok(mut state) = state_clone.lock() {
                state.update();
            }
        });

        Self { state }
    }

    pub fn forget(&mut self, invariant_id: u64) {
        self.state.lock().unwrap().db.evict(invariant_id);
        self.state.lock().unwrap().queue.remove(invariant_id);
        self.state.lock().unwrap().exec_pool.remove(invariant_id);
    }

    pub fn verify(&mut self, invariant: Invariant) -> ProofStatus {
        let id = invariant.id();
        let spec = Spec::new_from_invariant(invariant);
        // println!("{}", &spec.k_spec);
        self.state.lock().unwrap().add_spec(spec);
        self.state.lock().unwrap().update();
        self.state.lock().unwrap().proof_status(id)
    }

    pub fn display(&self) {
        let state_clone = Arc::clone(&self.state);

        thread::spawn(move || loop {
            let mut stdout = io::stdout();
            execute!(stdout, Clear(ClearType::All)).unwrap();

            let content = format!("{}", state_clone.lock().unwrap());
            print!("{content}");

            execute!(stdout, MoveTo(0, 0)).unwrap();
            thread::sleep(Duration::from_millis(1000));
        });
    }
}

pub struct ServerState {
    // contains all known tests
    tests: IndexMap<u64, SymbolicTest>,
    // tests waiting to be added to the pool
    // first in first out allows for easy test prioritization
    stack: Vec<u64>,
    // currently executing tests
    pool: KSpecExecPool,
    // all tests that have been completed
    db: Db,
}

impl Display for ServerState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", time::SystemTime::now())?;
        writeln!(f, "")?;

        writeln!(f, "queue:")?;
        writeln!(f, "{:?}", &self.queue)?;

        writeln!(f, "execution pool:")?;
        writeln!(f, "{:?}", &self.exec_pool)?;

        writeln!(f, "db:")?;
        writeln!(f, "{}", &self.db)
    }
}

impl ServerState {
    pub fn new(db_path: &str) -> Self {
        ServerState {
            tests: indexmap! {},
            stack: vec![],
            pool: KSpecExecPool::new(),
            db: Db::new(db_path),
        }
    }

    pub fn add_test(&mut self, test: SymbolicTest) {
        let id = test.id();

        if !self.stack.contains(id) && !self.db.contains(id) {}
    }

    pub fn test_status(&self, id: u64) -> ProofStatus {
        if self.queue.contains(id) {
            // queued
        } else if Some(result) = self.db.get(id) {
            // valid or invalid
        } else {
            // unknown
        }
    }

    pub fn update(&mut self) {
        let mut first_indices: IndexMap<u64, usize> = indexmap! {};

        let mut renames: Vec<(usize, SmolStr)> = vec![];
        let mut removals: Vec<usize> = vec![];
        let mut db_changed = false;

        for (index, spec) in self.queue.specs.iter_mut().enumerate() {
            if let Some(first_index) = first_indices.get(&spec.invariant_id) {
                // duplicate spec
                renames.push((*first_index, spec.name.clone()));
                removals.push(index);
            } else {
                // non-duplicate spec
                first_indices.insert(spec.invariant_id, index);

                match spec.status {
                    ProofStatus::New => {
                        if let Some(entry) = self.db.get_mut(spec.invariant_id) {
                            spec.status = if entry.complete {
                                ProofStatus::Complete
                            } else {
                                ProofStatus::Incomplete
                            }
                        } else {
                            spec.status = ProofStatus::Ready
                        }
                    }
                    ProofStatus::Ready => {
                        self.exec_pool
                            .execute_spec(spec.invariant_id, spec.k_spec.clone());
                        spec.status = ProofStatus::Proving
                    }
                    ProofStatus::Proving => {
                        if let Some(result) = self.exec_pool.get_status(spec.invariant_id) {
                            if result {
                                spec.status = ProofStatus::Complete
                            } else {
                                spec.status = ProofStatus::Incomplete
                            }

                            self.exec_pool.remove(spec.invariant_id)
                        }
                    }
                    ProofStatus::Complete => {
                        self.db
                            .update(spec.invariant_id, DbEntry::new(spec.name.clone(), true));
                        removals.push(index);
                        db_changed = true
                    }
                    ProofStatus::Incomplete => {
                        self.db
                            .update(spec.invariant_id, DbEntry::new(spec.name.clone(), false));
                        removals.push(index);
                        db_changed = true
                    }
                }
            }
        }

        if db_changed {
            self.db.write();
        }

        for (index, name) in renames {
            self.queue.specs[index].name = name
        }

        removals.reverse();

        for index in removals {
            self.queue.specs.remove(index);
        }
    }
}
