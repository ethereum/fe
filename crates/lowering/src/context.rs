use fe_analyzer::context::{ExpressionAttributes, FunctionBody};
use fe_analyzer::namespace::items::FunctionId;
use fe_analyzer::namespace::types::{Array, FixedSize, Tuple};
use fe_analyzer::AnalyzerDb;
use fe_parser::ast;
use fe_parser::node::Node;
use indexmap::IndexSet;
use std::rc::Rc;

pub struct ModuleContext<'db> {
    pub db: &'db dyn AnalyzerDb,
    /// Tuples that are used in the module
    tuples: IndexSet<Tuple>,
}

impl<'db> ModuleContext<'db> {
    pub fn new(db: &'db dyn AnalyzerDb) -> Self {
        Self {
            db,
            tuples: IndexSet::new(),
        }
    }

    pub fn add_tuple(&mut self, tuple: Tuple) {
        self.tuples.insert(tuple);
    }

    pub fn into_tuples(self) -> IndexSet<Tuple> {
        self.tuples
    }
}

pub struct ContractContext<'a, 'db> {
    pub module: &'a mut ModuleContext<'db>,

    /// List expressions that are used in the contract
    pub list_expressions: IndexSet<Array>,
}

impl<'a, 'db> ContractContext<'a, 'db> {
    pub fn new(module: &'a mut ModuleContext<'db>) -> Self {
        Self {
            module,
            list_expressions: IndexSet::new(),
        }
    }

    pub fn db(&self) -> &'db dyn AnalyzerDb {
        self.module.db
    }
}

pub struct FnContext<'a, 'b, 'db> {
    pub contract: &'a mut ContractContext<'b, 'db>,
    pub body: Rc<FunctionBody>,
    pub id: FunctionId,

    /// Holds fresh id for [`FnContext::make_unique_name`]
    fresh_id: u64,
}

impl<'a, 'b, 'db> FnContext<'a, 'b, 'db> {
    pub fn new(
        id: FunctionId,
        contract: &'a mut ContractContext<'b, 'db>,
        body: Rc<FunctionBody>,
    ) -> Self {
        Self {
            contract,
            body,
            id,
            fresh_id: 0,
        }
    }

    /// Makes a unique name from the given name, keeping it as readable as possible.
    pub fn make_unique_name(&mut self, name: &str) -> String {
        let id = self.fresh_id;
        self.fresh_id += 1;
        format!("${}_{}", name, id)
    }

    pub fn db(&self) -> &'db dyn AnalyzerDb {
        self.contract.db()
    }

    pub fn expression_attributes(&self, node: &Node<ast::Expr>) -> Option<&ExpressionAttributes> {
        self.body.expressions.get(&node.id)
    }
    pub fn var_decl_type(&self, node: &Node<ast::TypeDesc>) -> Option<&FixedSize> {
        self.body.var_decl_types.get(&node.id)
    }
}

impl<'a, 'b, 'db> AsMut<ModuleContext<'db>> for FnContext<'a, 'b, 'db> {
    fn as_mut(&mut self) -> &mut ModuleContext<'db> {
        self.contract.module
    }
}
