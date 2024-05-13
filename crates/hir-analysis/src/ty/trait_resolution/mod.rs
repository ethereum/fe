//! The algorithm for the trait resolution here is based on [`Tabled Typeclass Resolution`](https://arxiv.org/abs/2001.04301).
//! Also, [`XSB: Extending Prolog with Tabled Logic Programming`](https://arxiv.org/pdf/1012.5123) is a nice entry point for more detailed discussions about tabled logic solver.

use std::collections::BTreeSet;

use cranelift_entity::{entity_impl, PrimaryMap};
use hir::hir_def::IngotId;
use rustc_hash::{FxHashMap, FxHashSet};

use super::{
    binder::Binder,
    canonical::{Canonical, Canonicalized},
    fold::{TyFoldable, TyFolder},
    trait_def::{impls_of_trait, Implementor, TraitInstId},
    ty_def::TyFlags,
    unify::PersistentUnificationTable,
};
use crate::{ty::visitor::collect_flags, HirAnalysisDb};

const MAXIMUM_SOLUTION_NUM: usize = 3;

#[salsa::tracked(return_ref)]
pub fn is_goal_satisfiable(
    db: &dyn HirAnalysisDb,
    assumptions: PredicateListId,
    goal: Goal,
) -> GoalSatisfiability {
    let flags = collect_flags(db, goal.value);
    if flags.contains(TyFlags::HAS_INVALID) {
        return GoalSatisfiability::ContainsInvalid;
    };

    ProofForest::new(db, goal, assumptions).solve()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GoalSatisfiability {
    /// Goal is satisfied with the unique solution.
    Satisfied(Solution),
    /// Goal is satisfied, but with multiple solutions.
    NeedsConfirmation(BTreeSet<Solution>),

    /// Goal contains invalid.
    ContainsInvalid,
    /// The goal is not satisfied.
    UnSat,
}

impl GoalSatisfiability {
    pub fn is_satisfied(&self) -> bool {
        matches!(
            self,
            Self::Satisfied(_) | Self::NeedsConfirmation(_) | Self::ContainsInvalid
        )
    }
}

/// The query goal.
/// Since `TraitInstId` contains `Self` type as its first argument,
/// the query for `Implements<Ty, Trait<i32>>` is represented as
/// `Trait<Ty, i32>`.
pub type Goal = Canonical<TraitInstId>;
pub type Solution = super::canonical::Solution<TraitInstId>;

#[salsa::interned]
pub struct PredicateListId {
    #[return_ref]
    pub list: BTreeSet<TraitInstId>,
    pub ingot: IngotId,
}

impl PredicateListId {
    pub(super) fn remove(self, db: &dyn HirAnalysisDb, pred: TraitInstId) -> Self {
        if !self.contains(db, pred) {
            return self;
        };

        let mut predicates = self.list(db).clone();
        predicates.remove(&pred);
        PredicateListId::new(db, predicates, self.ingot(db))
    }

    pub(super) fn merge(self, db: &dyn HirAnalysisDb, other: Self) -> Self {
        let mut predicates = self.list(db).clone();
        predicates.extend(other.list(db));
        PredicateListId::new(db, predicates, self.ingot(db))
    }

    pub(super) fn empty_list(db: &dyn HirAnalysisDb) -> Self {
        Self::new(db, BTreeSet::new(), IngotId::dummy())
    }

    pub(super) fn contains(self, db: &dyn HirAnalysisDb, pred: TraitInstId) -> bool {
        self.list(db).contains(&pred)
    }

    fn extend_by_super(self, db: &dyn HirAnalysisDb) -> Self {
        let mut list = self.list(db).clone();
        for &pred in self.list(db) {
            for &super_trait in pred.def(db).super_traits(db).iter() {
                let super_trait = super_trait.instantiate(db, pred.args(db));
                list.insert(super_trait);
            }
        }

        Self::new(db, list, self.ingot(db))
    }
}

struct ProofForest<'db> {
    root: GeneratorNode,

    g_nodes: PrimaryMap<GeneratorNode, GeneratorNodeData<'db>>,
    c_nodes: PrimaryMap<ConsumerNode, ConsumerNodeData<'db>>,
    g_stack: Vec<GeneratorNode>,
    c_stack: Vec<(ConsumerNode, Solution)>,

    goal_to_node: FxHashMap<Goal, GeneratorNode>,

    assumptions: PredicateListId,
    maximum_solution_num: usize,
    db: &'db dyn HirAnalysisDb,
}

impl<'db> ProofForest<'db> {
    fn new(db: &'db dyn HirAnalysisDb, goal: Goal, assumptions: PredicateListId) -> Self {
        let assumptions = assumptions.extend_by_super(db);

        let mut forest = Self {
            root: GeneratorNode(0), // Set temporary root.
            g_nodes: PrimaryMap::new(),
            c_nodes: PrimaryMap::new(),
            g_stack: Vec::new(),
            c_stack: Vec::new(),
            goal_to_node: FxHashMap::default(),
            assumptions,
            maximum_solution_num: MAXIMUM_SOLUTION_NUM,
            db,
        };

        let root = forest.new_generator_node(goal);
        forest.root = root;
        forest
    }

    fn solve(mut self) -> GoalSatisfiability {
        loop {
            if self.g_nodes[self.root].solutions.len() >= self.maximum_solution_num {
                break;
            }

            if let Some((c_node, solution)) = self.c_stack.pop() {
                c_node.apply_solution(&mut self, solution);
                continue;
            }

            if let Some(&g_node) = self.g_stack.last() {
                if !g_node.step(&mut self) {
                    self.g_stack.pop();
                }
                continue;
            }

            break;
        }

        let solutions = std::mem::take(&mut self.g_nodes[self.root].solutions);
        match solutions.len() {
            1 => GoalSatisfiability::Satisfied(solutions.into_iter().next().unwrap()),
            0 => GoalSatisfiability::UnSat,
            _ => GoalSatisfiability::NeedsConfirmation(solutions),
        }
    }

    fn new_generator_node(&mut self, goal: Goal) -> GeneratorNode {
        let g_node_data = GeneratorNodeData::new(self.db, goal, self.assumptions);
        let g_node = self.g_nodes.push(g_node_data);
        self.goal_to_node.insert(goal, g_node);
        self.g_stack.push(g_node);
        g_node
    }

    fn new_consumer_node(
        &mut self,
        root: GeneratorNode,
        mut remaining_goals: Vec<TraitInstId>,
        table: PersistentUnificationTable<'db>,
    ) {
        let query = remaining_goals.pop().unwrap();
        let canonicalized_query = Canonicalized::new(self.db, query);
        let goal = canonicalized_query.value;

        let c_node_data = ConsumerNodeData {
            applied_solutions: FxHashSet::default(),
            remaining_goals,
            root,
            query: (query, canonicalized_query),
            table,
        };

        let c_node = self.c_nodes.push(c_node_data);
        if !self.goal_to_node.contains_key(&goal) {
            self.new_generator_node(goal);
        }

        self.goal_to_node[&goal].add_dependent(self, c_node);
    }
}

struct GeneratorNodeData<'db> {
    table: PersistentUnificationTable<'db>,
    goal: Goal,
    extracted_goal: TraitInstId,
    /// Solutions that is already obtained.
    solutions: BTreeSet<Solution>,
    dependents: Vec<ConsumerNode>,
    cands: &'db [Binder<Implementor>],
    assumptions: PredicateListId,
    next_cand: usize,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct GeneratorNode(u32);
entity_impl!(GeneratorNode);

impl<'db> GeneratorNodeData<'db> {
    fn new(db: &'db dyn HirAnalysisDb, goal: Goal, assumptions: PredicateListId) -> Self {
        let mut table = PersistentUnificationTable::new(db);
        let extracted_goal = goal.extract_identity(&mut table);
        let cands = impls_of_trait(db, assumptions.ingot(db), goal);

        Self {
            table,
            goal,
            extracted_goal,
            solutions: BTreeSet::default(),
            dependents: Vec::new(),
            cands,
            assumptions,
            next_cand: 0,
        }
    }
}

impl GeneratorNode {
    fn register_solution(self, pt: &mut ProofForest, solution: TraitInstId) {
        let g_node = &mut pt.g_nodes[self];
        let solution = g_node.goal.make_solution(g_node.table.db(), solution);
        g_node.solutions.insert(solution);
        if self == pt.root {
            return;
        }

        for &c_node in g_node.dependents.iter() {
            pt.c_stack.push((c_node, solution));
        }
    }

    /// Returns `false` if there is no candidate to apply.
    fn step(self, pt: &mut ProofForest) -> bool {
        let g_node = &mut pt.g_nodes[self];
        let db = g_node.table.db();

        while let Some(&cand) = g_node.cands.get(g_node.next_cand) {
            g_node.next_cand += 1;

            let mut table = g_node.table.clone();
            let gen_cand = table.instantiate_with_fresh_vars(cand);
            if table
                .unify(gen_cand.trait_(db), g_node.extracted_goal)
                .is_err()
            {
                continue;
            }

            let solution = gen_cand.fold_with(&mut table);
            let constraints = solution.constraints(table.db());

            if constraints.list(db).is_empty() {
                // Register solution.
                self.register_solution(pt, solution.trait_(db))
            } else {
                pt.new_consumer_node(
                    self,
                    constraints.list(db).clone().into_iter().collect(),
                    table,
                );
            }

            return true;
        }

        let mut next_cand = g_node.next_cand - g_node.cands.len();
        while let Some(&assumption) = g_node.assumptions.list(db).iter().nth(next_cand) {
            g_node.next_cand += 1;
            next_cand += 1;
            let mut table = g_node.table.clone();
            if table.unify(assumption, g_node.extracted_goal).is_ok() {
                self.register_solution(pt, assumption);
                return true;
            }
        }

        false
    }

    fn add_dependent(self, pt: &mut ProofForest, dependent: ConsumerNode) {
        let g_node = &mut pt.g_nodes[self];
        g_node.dependents.push(dependent);
        for &solution in g_node.solutions.iter() {
            pt.c_stack.push((dependent, solution))
        }
    }
}

struct ConsumerNodeData<'db> {
    /// Holds solutions that are already applied.
    applied_solutions: FxHashSet<Solution>,
    remaining_goals: Vec<TraitInstId>,
    /// The root generator node of the consumer node.
    root: GeneratorNode,

    /// The current pending query that is resolved by another [`GeneratorNode`].
    query: (TraitInstId, Canonicalized<TraitInstId>),
    table: PersistentUnificationTable<'db>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ConsumerNode(u32);
entity_impl!(ConsumerNode);

impl ConsumerNode {
    fn apply_solution(self, pt: &mut ProofForest, solution: Solution) {
        let c_node = &mut pt.c_nodes[self];

        // If the solutions is already applied, do nothing.
        if !c_node.applied_solutions.insert(solution) {
            return;
        }

        let mut table = c_node.table.clone();

        // Extract solution to the current env.
        let (pending_inst, canonicalized_pending_inst) = &c_node.query;
        let solution = canonicalized_pending_inst
            .extract_solution(&mut table, solution)
            .fold_with(&mut table);

        let tree_root = c_node.root;

        if c_node.remaining_goals.is_empty() {
            // If no remaining goals in the consumer node, it's the solution for the root
            // goal.
            tree_root.register_solution(pt, solution);
        } else {
            // Create a child consumer node for the subgoals.
            let remaining_goals = c_node.remaining_goals.clone();
            if table.unify(*pending_inst, solution).is_ok() {
                pt.new_consumer_node(tree_root, remaining_goals, table);
            }
        }
    }
}
