//! The algorithm for the trait resolution here is based on [`Tabled Typeclass Resolution`](https://arxiv.org/abs/2001.04301).
//! Also, [`XSB: Extending Prolog with Tabled Logic Programming`](https://arxiv.org/pdf/1012.5123) is a nice entry point for more detailed discussions about tabled logic solver.

use std::collections::BTreeSet;

use cranelift_entity::{entity_impl, PrimaryMap};
use rustc_hash::{FxHashMap, FxHashSet};

use super::{GoalSatisfiability, PredicateListId};
use crate::{
    ty::{
        binder::Binder,
        canonical::{Canonical, Canonicalized},
        fold::{TyFoldable, TyFolder},
        trait_def::{impls_for_trait, Implementor, TraitInstId},
        unify::PersistentUnificationTable,
    },
    HirAnalysisDb,
};

const MAXIMUM_SOLUTION_NUM: usize = 3;

/// The query goal.
/// Since `TraitInstId` contains `Self` type as its first argument,
/// the query for `Implements<Ty, Trait<i32>>` is represented as
/// `Trait<Ty, i32>`.
type Goal = Canonical<TraitInstId>;
type Solution = crate::ty::canonical::Solution<TraitInstId>;

/// A structure representing a proof forest used for solving trait goals.
///
/// The `ProofForest` contains generator and consumer nodes which work together
/// to find solutions to trait goals. It maintains stacks for generator and
/// consumer nodes to keep track of the solving process, and a mapping from
/// goals to generator nodes to avoid redundant computations.
pub(super) struct ProofForest<'db> {
    /// The root generator node.
    root: GeneratorNode,

    /// An arena of generator nodes.
    g_nodes: PrimaryMap<GeneratorNode, GeneratorNodeData<'db>>,
    /// An arena of consumer nodes.
    c_nodes: PrimaryMap<ConsumerNode, ConsumerNodeData<'db>>,
    /// A stack of generator nodes to be processed.
    g_stack: Vec<GeneratorNode>,
    /// A stack of consumer nodes and their solutions to be processed.
    c_stack: Vec<(ConsumerNode, Solution)>,

    /// A mapping from goals to generator nodes.
    goal_to_node: FxHashMap<Goal, GeneratorNode>,

    /// The list of assumptions.
    assumptions: PredicateListId,

    /// The maximum number of solutions.
    maximum_solution_num: usize,
    /// The database for HIR analysis.
    db: &'db dyn HirAnalysisDb,
}

impl<'db> ProofForest<'db> {
    /// Creates a new `ProofForest` with the given initial goal and assumptions.
    ///
    /// This function initializes the proof forest with a root generator node
    /// for the given goal and sets up the necessary data structures for
    /// solving trait goals.
    ///
    /// # Parameters
    /// - `db`: A reference to the HIR analysis database.
    /// - `goal`: The initial trait goal to be solved.
    /// - `assumptions`: The list of assumptions to be used during the solving
    ///   process.
    ///
    /// # Returns
    /// A new instance of `ProofForest` initialized with the given goal and
    /// assumptions.
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        goal: Goal,
        assumptions: PredicateListId,
    ) -> Self {
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

    /// Solves the trait goal using a proof forest approach.
    ///
    /// This function iteratively processes generator and consumer nodes until
    /// either the maximum number of solutions is found or no more nodes can
    /// be processed. The solving process involves:
    /// - Popping solutions from the consumer stack and applying them.
    /// - Stepping through generator nodes to find new solutions or sub-goals.
    /// - Registering solutions and propagating them to dependent consumer
    ///   nodes.
    ///
    /// The function returns `GoalSatisfiability` indicating the status of the
    /// goal:
    /// - `Satisfied` if exactly one solution is found.
    /// - `UnSat` if no solutions are found and an unresolved subgoal is
    ///   identified.
    /// - `NeedsConfirmation` if multiple solutions are found.
    pub(super) fn solve(mut self) -> GoalSatisfiability {
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
            0 => {
                let unresolved_subgoal = self.root.unresolved_subgoal(&mut self);
                GoalSatisfiability::UnSat(unresolved_subgoal)
            }
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

    /// Creates a new consumer node and registers it with the proof forest.
    ///
    /// This function takes a root generator node, a list of remaining goals,
    /// and a persistent unification table. It creates a consumer node that
    /// represents a sub-goal that needs to be solved and remaining
    /// subgoals. If the goal is not already associated with a generator
    /// node, a new generator node is created for it.
    ///
    /// The consumer node is then registered as a dependent of the corresponding
    /// generator node, ensuring that solutions found for the generator node are
    /// propagated to the consumer node.
    ///
    /// # Parameters
    /// - `root`: The root generator node of the consumer node.
    /// - `remaining_goals`: A list of trait instances that represent the
    ///   remaining goals to be solved.
    /// - `table`: A persistent unification table used for managing unification
    ///   operations.
    ///
    /// # Returns
    /// A new `ConsumerNode` that is registered with the proof forest.
    fn new_consumer_node(
        &mut self,
        root: GeneratorNode,
        mut remaining_goals: Vec<TraitInstId>,

        table: PersistentUnificationTable<'db>,
    ) -> ConsumerNode {
        let query = remaining_goals.pop().unwrap();
        let canonicalized_query = Canonicalized::new(self.db, query);
        let goal = canonicalized_query.value;

        let c_node_data = ConsumerNodeData {
            applied_solutions: FxHashSet::default(),
            remaining_goals,
            root,
            query: (query, canonicalized_query),
            table,
            children: Vec::new(),
        };

        let c_node = self.c_nodes.push(c_node_data);
        if !self.goal_to_node.contains_key(&goal) {
            self.new_generator_node(goal);
        }

        self.goal_to_node[&goal].add_dependent(self, c_node);
        c_node
    }
}

/// A structure representing the data associated with a generator node in the
/// proof forest.
///
/// The `GeneratorNodeData` contains information about the goal, the unification
/// table, the candidate implementors, the solutions found, and the dependents
/// of the generator node. It also keeps track of the assumptions, the next
/// candidate to be processed, and the child consumer nodes.
struct GeneratorNodeData<'db> {
    table: PersistentUnificationTable<'db>,
    /// The canonical goal associated with the generator node.
    goal: Goal,
    /// The trait instance extracted from the goal.
    extracted_goal: TraitInstId,
    /// A set of solutions found for the goal.
    solutions: BTreeSet<Solution>,
    ///  A list of consumer nodes that depend on this generator node.
    dependents: Vec<ConsumerNode>,
    ///  A list of candidate implementors for the trait.
    cands: &'db [Binder<Implementor>],
    /// The list of assumptions for the goal.
    assumptions: PredicateListId,
    /// The index of the next candidate to be tried.
    next_cand: usize,
    /// A list of child consumer nodes created for sub-goals.
    children: Vec<ConsumerNode>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct GeneratorNode(u32);
entity_impl!(GeneratorNode);

impl<'db> GeneratorNodeData<'db> {
    fn new(db: &'db dyn HirAnalysisDb, goal: Goal, assumptions: PredicateListId) -> Self {
        let mut table = PersistentUnificationTable::new(db);
        let extracted_goal = goal.extract_identity(&mut table);
        let cands = impls_for_trait(db, assumptions.ingot(db), goal);

        Self {
            table,
            goal,
            extracted_goal,
            solutions: BTreeSet::default(),
            dependents: Vec::new(),
            cands,
            assumptions,
            next_cand: 0,
            children: Vec::new(),
        }
    }
}

impl GeneratorNode {
    /// Registers the given solution with the proof forest and propagates it to
    /// dependent consumer nodes.
    ///
    /// This function canonicalizes the solution and inserts it into the set of
    /// solutions for the generator node. If the solution is new, it
    /// propagates the solution to all dependent consumer nodes.
    ///
    /// # Parameters
    /// - `pf`: A mutable reference to the `ProofForest`.
    /// - `table`: A mutable reference to the `PersistentUnificationTable` used
    ///   for managing unification operations.
    fn register_solution_with(self, pf: &mut ProofForest, table: &mut PersistentUnificationTable) {
        let g_node = &mut pf.g_nodes[self];
        let solution = g_node
            .goal
            .canonicalize_solution(table.db(), table, g_node.extracted_goal);
        if g_node.solutions.insert(solution) {
            for &c_node in g_node.dependents.iter() {
                pf.c_stack.push((c_node, solution));
            }
        }
    }

    /// Advances the solving process for the generator node.
    ///
    /// This function attempts to find a new solution or sub-goal for the
    /// generator node. It iterates through the candidate implementors and
    /// assumptions, unifying them with the goal. If a solution is found, it
    /// is registered. If a sub-goal is found, a new consumer node is
    /// created to handle it.
    ///
    /// # Parameters
    /// - `pf`: A mutable reference to the `ProofForest`.
    ///
    /// # Returns
    /// `true` if a new solution or sub-goal was found and processed; `false`
    /// otherwise.
    fn step(self, pf: &mut ProofForest) -> bool {
        let g_node = &mut pf.g_nodes[self];
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

            let constraints = gen_cand.constraints(db);

            if constraints.list(db).is_empty() {
                self.register_solution_with(pf, &mut table);
            } else {
                let sub_goals = constraints
                    .list(db)
                    .iter()
                    .map(|c| c.fold_with(&mut table))
                    .collect();
                let child = pf.new_consumer_node(self, sub_goals, table);
                pf.g_nodes[self].children.push(child);
            }

            return true;
        }

        let mut next_cand = g_node.next_cand - g_node.cands.len();
        while let Some(&assumption) = g_node.assumptions.list(db).iter().nth(next_cand) {
            g_node.next_cand += 1;
            next_cand += 1;
            let mut table = g_node.table.clone();
            if table.unify(assumption, g_node.extracted_goal).is_ok() {
                self.register_solution_with(pf, &mut table);
                return true;
            }
        }

        false
    }

    fn add_dependent(self, pf: &mut ProofForest, dependent: ConsumerNode) {
        let g_node = &mut pf.g_nodes[self];
        g_node.dependents.push(dependent);
        for &solution in g_node.solutions.iter() {
            pf.c_stack.push((dependent, solution))
        }
    }

    fn unresolved_subgoal(self, pf: &mut ProofForest) -> Option<Solution> {
        let g_node = &pf.g_nodes[self];
        // If the child nodes branch out more than one, we give up identifying the
        // unresolved subgoal to avoid generating a large number of uncertain unresolved
        // subgoals.
        if g_node.children.len() != 1 {
            return None;
        }

        let child = g_node.children[0];
        child.unresolved_subgoal(pf)
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
    children: Vec<ConsumerNode>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ConsumerNode(u32);
entity_impl!(ConsumerNode);

impl ConsumerNode {
    /// Applies a given solution to the consumer node.
    ///
    /// This function checks if the solution has already been applied. If not,
    /// it attempts to unify the solution with the pending query of the
    /// consumer node. If the unification is successful and there are no
    /// remaining goals, the solution is registered with the root generator
    /// node. If there are remaining goals, a new consumer node is created
    /// to handle them.
    ///
    /// # Parameters
    /// - `pf`: A mutable reference to the `ProofForest`.
    /// - `solution`: The solution to be applied.
    fn apply_solution(self, pf: &mut ProofForest, solution: Solution) {
        let c_node = &mut pf.c_nodes[self];

        // If the solutions is already applied, do nothing.
        if !c_node.applied_solutions.insert(solution) {
            return;
        }

        let mut table = c_node.table.clone();

        // Extract solution to the current env.
        let (pending_inst, canonicalized_pending_inst) = &c_node.query;
        let solution = canonicalized_pending_inst.extract_solution(&mut table, solution);

        // Unifies pending inst and solution.
        if table.unify(*pending_inst, solution).is_err() {
            return;
        }

        let tree_root = c_node.root;

        if c_node.remaining_goals.is_empty() {
            // If no remaining goals in the consumer node, it's the solution for the root
            // goal.
            tree_root.register_solution_with(pf, &mut table);
        } else {
            // Create a child consumer node for the subgoals.
            let remaining_goals = c_node.remaining_goals.clone();
            let child = pf.new_consumer_node(tree_root, remaining_goals, table);
            pf.c_nodes[self].children.push(child);
        }
    }

    fn unresolved_subgoal(self, pf: &mut ProofForest) -> Option<Solution> {
        let c_node = &mut pf.c_nodes[self];
        if c_node.children.len() != 1 {
            let unsat = c_node.query.0;
            let unsat = pf.g_nodes[c_node.root].goal.canonicalize_solution(
                c_node.table.db(),
                &mut c_node.table,
                unsat,
            );
            return Some(unsat);
        }

        c_node.children[0].unresolved_subgoal(pf)
    }
}
