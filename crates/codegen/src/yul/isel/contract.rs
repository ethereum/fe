use fe_analyzer::namespace::items::{ContractId, ModuleId};
use fe_mir::ir::{function::Linkage, FunctionSigId};
use fxhash::FxHashMap;
use indexmap::IndexSet;
use yultsur::{yul, *};

use crate::{
    db::CodegenDb,
    yul::{
        cgu::{CguLowerContext, CompiledCgu},
        dependency_graph::DependencyGraph,
        runtime::AbiSrcLocation,
        YulVariable,
    },
};

pub(crate) fn lower_deployable_contract(
    db: &dyn CodegenDb,
    cgus: &FxHashMap<ModuleId, CompiledCgu>,
    dep_graph: &DependencyGraph,
    contract: ContractId,
) -> yul::Object {
    let mut ctx = CguLowerContext::default();

    let constructor = if let Some(init_func) = contract.init_function(db.upcast()) {
        let init_sig = init_func.sig(db.upcast());
        let init_sig = db.mir_lowered_func_signature(init_sig);
        make_init(db, &mut ctx, contract, init_sig)
    } else {
        statements! {}
    };
    let deploy_code = make_deploy(db, contract);

    let dep_collector = ContractDependencyCollector::new(db, cgus, dep_graph, &ctx, contract);

    let functions = dep_collector.collect_funcs_for_deploy();
    let runtime_funcs = dep_collector.collect_runtime_funcs_for_deploy();
    let contracts = dep_collector.collect_contracts_for_deploy();
    let constants = dep_collector.collect_constants_for_deploy();

    let name = identifier! {(
        db.codegen_contract_deployer_symbol_name(contract).as_ref()
    )};
    let deploy_block = block_statement! {
        [constructor...]
        [deploy_code...]
    };
    let code = code! {
        [deploy_block]
        [functions...]
        [runtime_funcs...]
    };
    let object = yul::Object {
        name,
        code,
        objects: contracts,
        data: constants,
    };

    normalize_object(object)
}

fn lower_contract(
    db: &dyn CodegenDb,
    cgus: &FxHashMap<ModuleId, CompiledCgu>,
    dep_graph: &DependencyGraph,
    contract: ContractId,
) -> yul::Object {
    let mut context = CguLowerContext::default();
    let dispatcher = if let Some(call_fn) = contract.call_function(db.upcast()) {
        let call_fn_sig = call_fn.sig(db.upcast());
        let call_fn_sig = db.mir_lowered_func_signature(call_fn_sig);
        let call_symbol = identifier! { (db.codegen_function_symbol_name(call_fn_sig)) };
        statement! {
            ([call_symbol]())
        }
    } else {
        let exported_funcs: Vec<_> = db
            .mir_lower_contract_all_functions(contract)
            .iter()
            .filter_map(|(sig, _)| {
                if sig.linkage(db.upcast()) == Linkage::Export {
                    Some(*sig)
                } else {
                    None
                }
            })
            .collect();
        make_dispatcher(db, &mut context, &exported_funcs)
    };

    let dep_collector = ContractDependencyCollector::new(db, cgus, dep_graph, &context, contract);

    let functions = dep_collector.collect_funcs();
    let runtime_funcs = dep_collector.collect_runtime_funcs();
    let contracts = dep_collector.collect_contracts();
    let constants = dep_collector.collect_constants();

    let name = identifier! {(
        db.codegen_contract_symbol_name(contract).as_ref()
    )};
    let code = code! {
        ([dispatcher])
        [functions...]
        [runtime_funcs...]
    };
    yul::Object {
        name,
        code,
        objects: contracts,
        data: constants,
    }
}

fn make_dispatcher(
    db: &dyn CodegenDb,
    context: &mut CguLowerContext,
    funcs: &[FunctionSigId],
) -> yul::Statement {
    let arms = funcs
        .iter()
        .map(|sig| dispatch_arm(db, context, *sig))
        .collect::<Vec<_>>();

    if arms.is_empty() {
        statement! { return(0, 0) }
    } else {
        let selector = expression! {
            and((shr((sub(256, 32)), (calldataload(0)))), 0xffffffff)
        };
        switch! {
            switch ([selector])
            [arms...]
            (default { (return(0, 0)) })
        }
    }
}

fn dispatch_arm(
    db: &dyn CodegenDb,
    context: &mut CguLowerContext,
    sig: FunctionSigId,
) -> yul::Case {
    let func_sig = db.codegen_legalized_signature(sig);
    let mut param_vars = Vec::with_capacity(func_sig.params.len());
    let mut param_tys = Vec::with_capacity(func_sig.params.len());
    func_sig.params.iter().for_each(|param| {
        param_vars.push(YulVariable::new(param.name.as_str()));
        param_tys.push(param.ty);
    });

    let decode_params = if func_sig.params.is_empty() {
        statements! {}
    } else {
        let ident_params: Vec<_> = param_vars.iter().map(YulVariable::ident).collect();
        let param_size = YulVariable::new("param_size");
        statements! {
            (let [param_size.ident()] := sub((calldatasize()), 4))
            (let [ident_params...] := [context.runtime.abi_decode(db, expression! { 4 }, param_size.expr(), &param_tys, AbiSrcLocation::CallData)])
        }
    };

    let call_and_encode_return = {
        let name = identifier! { (db.codegen_function_symbol_name(sig)) };
        let call = expression! {[name]([(param_vars.iter().map(YulVariable::expr).collect::<Vec<_>>())...])};
        if let Some(mut return_type) = func_sig.return_type {
            if return_type.is_aggregate(db.upcast()) {
                return_type = return_type.make_mptr(db.upcast());
            }

            let ret = YulVariable::new("ret");
            let enc_start = YulVariable::new("enc_start");
            let enc_size = YulVariable::new("enc_size");
            let abi_encode = context.runtime.abi_encode_seq(
                db,
                &[ret.expr()],
                enc_start.expr(),
                &[return_type],
                false,
            );
            statements! {
                (let [ret.ident()] := [call])
                (let [enc_start.ident()] := [context.runtime.avail(db)])
                (let [enc_size.ident()] := [abi_encode])
                (return([enc_start.expr()], [enc_size.expr()]))
            }
        } else {
            statements! {
            ([yul::Statement::Expression(call)])
            (return(0, 0)) }
        }
    };

    let abi_sig = db.codegen_abi_function(sig);
    let selector = literal! { (format!("0x{}", abi_sig.selector().hex())) };
    case! {
        case [selector] {
            [decode_params...]
            [call_and_encode_return...]
        }
    }
}

fn make_init(
    db: &dyn CodegenDb,
    context: &mut CguLowerContext,
    contract: ContractId,
    init_sig: FunctionSigId,
) -> Vec<yul::Statement> {
    let init_func_name = identifier! { (db.codegen_function_symbol_name(init_sig)) };
    let contract_name = identifier_expression! { (format!{r#""{}""#, db.codegen_contract_deployer_symbol_name(contract)}) };

    let func_sig = db.codegen_legalized_signature(init_sig);
    let mut param_vars = Vec::with_capacity(func_sig.params.len());
    let mut param_tys = Vec::with_capacity(func_sig.params.len());
    let program_size = YulVariable::new("$program_size");
    let arg_size = YulVariable::new("$arg_size");
    let code_size = YulVariable::new("$code_size");
    let memory_data_offset = YulVariable::new("$memory_data_offset");
    func_sig.params.iter().for_each(|param| {
        param_vars.push(YulVariable::new(param.name.as_str()));
        param_tys.push(param.ty);
    });

    let decode_params = if func_sig.params.is_empty() {
        statements! {}
    } else {
        let ident_params: Vec<_> = param_vars.iter().map(YulVariable::ident).collect();
        statements! {
            (let [ident_params...] := [context.runtime.abi_decode(db, memory_data_offset.expr(), arg_size.expr(), &param_tys, AbiSrcLocation::Memory)])
        }
    };

    let call = expression! {[init_func_name]([(param_vars.iter().map(YulVariable::expr).collect::<Vec<_>>())...])};
    statements! {
        (let [program_size.ident()] := datasize([contract_name]))
        (let [code_size.ident()] := codesize())
        (let [arg_size.ident()] := sub([code_size.expr()], [program_size.expr()]))
        (let [memory_data_offset.ident()] := [context.runtime.alloc(db, arg_size.expr())])
        (codecopy([memory_data_offset.expr()], [program_size.expr()], [arg_size.expr()]))
        [decode_params...]
        ([yul::Statement::Expression(call)])
    }
}

fn make_deploy(db: &dyn CodegenDb, contract: ContractId) -> Vec<yul::Statement> {
    let contract_symbol =
        identifier_expression! { (format!{r#""{}""#, db.codegen_contract_symbol_name(contract)}) };
    let size = YulVariable::new("$$size");
    statements! {
       (let [size.ident()] := (datasize([contract_symbol.clone()])))
       (datacopy(0, (dataoffset([contract_symbol])), [size.expr()]))
       (return (0, [size.expr()]))
    }
}

fn normalize_object(obj: yul::Object) -> yul::Object {
    let data = obj
        .data
        .into_iter()
        .map(|data| yul::Data {
            name: data.name,
            value: data
                .value
                .replace('\\', "\\\\\\\\")
                .replace('\n', "\\\\n")
                .replace('"', "\\\\\"")
                .replace('\r', "\\\\r")
                .replace('\t', "\\\\t"),
        })
        .collect::<Vec<_>>();
    yul::Object {
        name: obj.name,
        code: obj.code,
        objects: obj
            .objects
            .into_iter()
            .map(normalize_object)
            .collect::<Vec<_>>(),
        data,
    }
}

struct ContractDependencyCollector<'db, 'a> {
    contract: ContractId,
    db: &'db dyn CodegenDb,
    cgus: &'a FxHashMap<ModuleId, CompiledCgu>,
    dep_graph: &'a DependencyGraph,
    ctx: &'a CguLowerContext,
}

impl<'db, 'a> ContractDependencyCollector<'db, 'a> {
    fn new(
        db: &'db dyn CodegenDb,
        cgus: &'a FxHashMap<ModuleId, CompiledCgu>,
        dep_graph: &'a DependencyGraph,
        ctx: &'a CguLowerContext,
        contract: ContractId,
    ) -> Self {
        Self {
            contract,
            db,
            cgus,
            dep_graph,
            ctx,
        }
    }

    fn collect_funcs_for_deploy(&self) -> Vec<yul::Statement> {
        let Some(init) = self.init_func() else {
            return vec![]
        };
        self.dep_graph
            .collect_dep_funcs(init)
            .iter()
            .map(|func| {
                let module = func.module(self.db.upcast());
                yul::Statement::FunctionDefinition(self.cgus[&module].functions[func].clone())
            })
            .collect()
    }

    fn collect_funcs(&self) -> Vec<yul::Statement> {
        self.dep_funcs()
            .into_iter()
            .map(|func| {
                let module = func.module(self.db.upcast());
                yul::Statement::FunctionDefinition(self.cgus[&module].functions[&func].clone())
            })
            .collect()
    }

    fn collect_runtime_funcs_for_deploy(&self) -> Vec<yul::Statement> {
        let mut runtime_funcs: IndexSet<_> = self
            .ctx
            .runtime
            .collect_definitions()
            .into_iter()
            .map(yul::Statement::FunctionDefinition)
            .collect();
        let Some(init) = self.init_func() else {
            return runtime_funcs.into_iter().collect();
        };

        for dep_func in self.dep_graph.collect_dep_funcs(init) {
            runtime_funcs.extend(self.collect_runtime_func_impl(dep_func));
        }
        runtime_funcs.into_iter().collect()
    }

    fn collect_runtime_funcs(&self) -> Vec<yul::Statement> {
        let mut runtime_funcs: IndexSet<_> = self
            .ctx
            .runtime
            .collect_definitions()
            .into_iter()
            .map(yul::Statement::FunctionDefinition)
            .collect();
        for dep_func in self.dep_funcs() {
            runtime_funcs.extend(self.collect_runtime_func_impl(dep_func));
        }

        runtime_funcs.into_iter().collect()
    }

    fn collect_runtime_func_impl(&self, func: FunctionSigId) -> Vec<yul::Statement> {
        let module = func.module(self.db.upcast());
        let cgu = &self.cgus[&module];
        cgu.runtime_func_deps[&func]
            .iter()
            .map(|runtime_func| {
                yul::Statement::FunctionDefinition(cgu.runtime_funcs[runtime_func].clone())
            })
            .collect()
    }

    fn collect_contracts_for_deploy(&self) -> Vec<yul::Object> {
        let mut contracts = vec![lower_contract(
            self.db,
            self.cgus,
            self.dep_graph,
            self.contract,
        )];
        let Some(init) = self.init_func() else {
            return contracts;
        };

        contracts.extend(
            self.dep_graph
                .collect_dep_contracts(init)
                .iter()
                .map(|contract| {
                    lower_deployable_contract(self.db, self.cgus, self.dep_graph, *contract)
                }),
        );
        contracts
    }

    fn collect_contracts(&self) -> Vec<yul::Object> {
        let mut visited = IndexSet::new();
        let mut contracts = vec![];

        for func in self.dep_funcs() {
            for contract in self.dep_graph.collect_dep_contracts(func) {
                if visited.insert(contract) {
                    contracts.push(lower_deployable_contract(
                        self.db,
                        self.cgus,
                        self.dep_graph,
                        contract,
                    ));
                }
            }
        }

        contracts
    }

    fn collect_constants_for_deploy(self) -> Vec<yul::Data> {
        let Some(init) = self.init_func() else {
            return vec![]
        };
        let module = init.module(self.db.upcast());
        self.cgus[&module].constants.iter().cloned().collect()
    }

    fn collect_constants(self) -> Vec<yul::Data> {
        let mut constants = IndexSet::new();
        for func in self.dep_funcs() {
            let module = func.module(self.db.upcast());
            constants.extend(self.cgus[&module].constants.iter().cloned());
        }
        constants.into_iter().collect()
    }

    fn init_func(&self) -> Option<FunctionSigId> {
        self.contract.init_function(self.db.upcast()).map(|init| {
            let init = init.sig(self.db.upcast());
            self.db.mir_lowered_func_signature(init)
        })
    }

    fn dep_funcs(&self) -> Vec<FunctionSigId> {
        let mut funcs = IndexSet::new();
        self.db
            .mir_lower_contract_all_functions(self.contract)
            .iter()
            .filter(|(func, _)| !func.is_contract_init(self.db.upcast()))
            .for_each(|(func, _)| {
                if func.linkage(self.db.upcast()) == Linkage::Export {
                    funcs.insert(*func);
                }
                funcs.extend(self.dep_graph.collect_dep_funcs(*func));
            });

        funcs.into_iter().collect()
    }
}
