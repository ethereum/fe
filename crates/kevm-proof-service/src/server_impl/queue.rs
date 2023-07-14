use std::fmt::Display;

use fe_proof_service::{
    invariant::{Invariant, InvariantBody},
    ProofStatus,
};
use kevm::{KSpec, KSpecType};
use smol_str::SmolStr;

pub struct Spec {
    pub name: SmolStr,
    pub k_spec: KSpec,
    pub invariant_id: u64,
    pub status: ProofStatus,
}

pub fn k_type_from_fe_type(typ: &str) -> KSpecType {
    match typ {
        "u256" => KSpecType::U256,
        "u128" => KSpecType::U128,
        "u64" => KSpecType::U64,
        "u32" => KSpecType::U32,
        "u16" => KSpecType::U16,
        "u8" => KSpecType::U8,
        "bool" => KSpecType::Bool,
        "Context" => KSpecType::Context,
        "mut Context" => KSpecType::Context,
        _ => panic!("invalid fe type string: {}", typ),
    }
}

fn k_spec_from_invariant(invariant: Invariant) -> KSpec {
    let Invariant { name: _, body } = invariant;
    let InvariantBody { args, code } = body;
    KSpec::new(
        args.iter().map(|typ| k_type_from_fe_type(&typ)).collect(),
        code,
    )
}

impl Spec {
    pub fn new_from_invariant(invariant: Invariant) -> Self {
        let invariant_id = invariant.id();
        let name = invariant.name.clone();
        let k_spec = k_spec_from_invariant(invariant);

        Self {
            name,
            k_spec,
            invariant_id,
            status: ProofStatus::New,
        }
    }
}

impl Display for &Spec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "name: {} id: {} status: {}",
            self.name, self.invariant_id, &self.status
        )
    }
}

pub struct Queue {
    pub specs: Vec<Spec>,
}

impl Display for &Queue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for spec in &self.specs {
            writeln!(f, "{}", spec)?
        }

        Ok(())
    }
}

impl Queue {
    pub fn new() -> Self {
        Self { specs: vec![] }
    }

    pub fn push_spec(&mut self, spec: Spec) {
        self.specs.push(spec)
    }

    pub fn remove(&mut self, id: u64) {
        self.specs.retain(|spec| spec.invariant_id != id);
    }
}
