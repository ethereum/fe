//! Core data types for decision tree representation

use rustc_hash::FxHashMap;

use crate::ty::pattern_analysis::Constructor;

/// A decision tree for efficient pattern matching compilation
///
/// Decision trees represent the control flow for pattern matching in an optimized form.
/// They are generated from pattern matrices and can be efficiently lowered to executable code.
#[derive(Debug, Clone)]
pub enum DecisionTree<'db> {
    /// Leaf node: execute a specific match arm
    ///
    /// When execution reaches a leaf, we know exactly which match arm should be executed
    /// and what variable bindings need to be established.
    Leaf(LeafNode),

    /// Switch node: branch based on constructor discrimination
    ///
    /// Switch nodes examine a value at a specific occurrence (path through data structure)
    /// and branch to different sub-trees based on the constructor/value found.
    Switch(SwitchNode<'db>),
}

/// Terminal node representing execution of a specific match arm
///
/// Leaf nodes contain all the information needed to execute a match arm:
/// - Which arm to execute (by index)
/// - What variable bindings to establish
#[derive(Debug, Clone)]
pub struct LeafNode {
    /// Index of the match arm to execute (0-based)
    pub arm_idx: usize,

    /// Variable bindings to establish before executing the arm
    ///
    /// Maps from (variable_name, arm_index) to the occurrence where the value can be found.
    /// The arm_index disambiguates variables with the same name in different arms.
    pub bindings: FxHashMap<(String, usize), Occurrence>,
}

/// Branching node that discriminates based on constructor/value
///
/// Switch nodes represent the core branching logic of pattern matching.
/// They examine a value at a specific occurrence and branch based on its constructor.
#[derive(Debug, Clone)]
pub struct SwitchNode<'db> {
    /// The occurrence (data path) to examine for discrimination
    ///
    /// This tells us which value to examine (e.g., the root value, or field .0.1 of the root)
    pub occurrence: Occurrence,

    /// The possible branches from this switch
    ///
    /// Each branch consists of:
    /// - A case pattern (specific constructor or default wildcard)
    /// - The sub-tree to execute if that case matches
    pub arms: Vec<(Case<'db>, DecisionTree<'db>)>,
}

/// A case in a switch node representing a specific branch condition
#[derive(Debug, Clone)]
pub enum Case<'db> {
    /// Branch for a specific constructor
    ///
    /// Examples:
    /// - `Some` constructor for Option enum
    /// - `true` literal for boolean
    /// - Tuple constructor for tuple types
    Constructor(Constructor<'db>),

    /// Default/wildcard branch
    ///
    /// This branch is taken when no specific constructor matches.
    /// It's used for incomplete constructor sets (e.g., when not all enum variants are covered).
    Default,
}

/// Represents a path through nested data structures
///
/// Occurrences track "where" in a nested data structure we're looking.
/// They're represented as a sequence of field/element indices.
///
/// # Examples
/// - `[]` = the root value being matched
/// - `[0]` = the first field/element of the root value  
/// - `[0, 1]` = the second field of the first field of the root
/// - `[2, 0, 3]` = field 3 of field 0 of field 2 of the root
///
/// For example, if matching `(a, (b, c))` and we want to access `c`,
/// the occurrence would be `[1, 1]` (second element of the tuple, then second element of that tuple).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Occurrence {
    /// The path as a sequence of field/element indices
    path: Vec<usize>,
}

impl Occurrence {
    /// Creates a new occurrence representing the root value
    pub fn new() -> Self {
        Self { path: Vec::new() }
    }

    /// Creates an occurrence from a path vector
    pub fn from_path(path: Vec<usize>) -> Self {
        Self { path }
    }

    /// Extends this occurrence by one more level of nesting
    ///
    /// # Arguments
    /// * `index` - The field/element index to access
    ///
    /// # Returns
    /// A new occurrence representing the path to that field/element
    ///
    /// # Example
    /// ```ignore
    /// let root = Occurrence::new();           // []
    /// let field_0 = root.extend(0);          // [0]
    /// let field_0_1 = field_0.extend(1);     // [0, 1]
    /// ```
    pub fn extend(&self, index: usize) -> Self {
        let mut new_path = self.path.clone();
        new_path.push(index);
        Self { path: new_path }
    }

    /// Gets the parent occurrence (removes the last path element)
    ///
    /// # Returns
    /// - `Some(parent)` if this occurrence has a parent (path is not empty)
    /// - `None` if this is the root occurrence
    ///
    /// # Example
    /// ```ignore
    /// let occ = Occurrence::from_path(vec![0, 1, 2]);
    /// assert_eq!(occ.parent(), Some(Occurrence::from_path(vec![0, 1])));
    ///
    /// let root = Occurrence::new();
    /// assert_eq!(root.parent(), None);
    /// ```
    pub fn parent(&self) -> Option<Self> {
        if self.path.is_empty() {
            None
        } else {
            let mut parent_path = self.path.clone();
            parent_path.pop();
            Some(Self { path: parent_path })
        }
    }

    /// Gets the last index in the path
    ///
    /// # Returns
    /// - `Some(index)` if the path is not empty
    /// - `None` if this is the root occurrence
    pub fn last_index(&self) -> Option<usize> {
        self.path.last().copied()
    }

    /// Gets an iterator over the path indices
    pub fn path_iter(&self) -> impl Iterator<Item = &usize> {
        self.path.iter()
    }

    /// Gets the path as a slice
    pub fn path(&self) -> &[usize] {
        &self.path
    }

    /// Gets the depth of this occurrence (length of the path)
    pub fn depth(&self) -> usize {
        self.path.len()
    }

    /// Specializes this occurrence for a constructor with the given arity
    ///
    /// When we specialize a pattern matrix for a constructor, we need to create
    /// new occurrences for each field of that constructor.
    ///
    /// # Arguments
    /// * `arity` - Number of fields/elements the constructor has
    ///
    /// # Returns
    /// Vector of new occurrences, one for each field
    ///
    /// # Example
    /// ```ignore
    /// let root = Occurrence::new();
    /// let specialized = root.specialize_for_arity(3);
    /// // specialized = [Occurrence([0]), Occurrence([1]), Occurrence([2])]
    /// ```
    pub fn specialize_for_arity(&self, arity: usize) -> Vec<Self> {
        (0..arity).map(|i| self.extend(i)).collect()
    }
}

impl Default for Occurrence {
    fn default() -> Self {
        Self::new()
    }
}

impl LeafNode {
    /// Creates a new leaf node
    pub fn new(arm_idx: usize) -> Self {
        Self {
            arm_idx,
            bindings: FxHashMap::default(),
        }
    }

    /// Creates a new leaf node with the given bindings
    pub fn with_bindings(arm_idx: usize, bindings: FxHashMap<(String, usize), Occurrence>) -> Self {
        Self { arm_idx, bindings }
    }

    /// Adds a variable binding to this leaf node
    pub fn add_binding(&mut self, var_name: String, arm_idx: usize, occurrence: Occurrence) {
        self.bindings.insert((var_name, arm_idx), occurrence);
    }
}

impl<'db> SwitchNode<'db> {
    /// Creates a new switch node
    pub fn new(occurrence: Occurrence) -> Self {
        Self {
            occurrence,
            arms: Vec::new(),
        }
    }

    /// Adds a branch to this switch node
    pub fn add_arm(&mut self, case: Case<'db>, subtree: DecisionTree<'db>) {
        self.arms.push((case, subtree));
    }

    /// Creates a switch node with the given arms
    pub fn with_arms(occurrence: Occurrence, arms: Vec<(Case<'db>, DecisionTree<'db>)>) -> Self {
        Self { occurrence, arms }
    }
}
