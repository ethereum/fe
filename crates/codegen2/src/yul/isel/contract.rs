use fe_analyzer::namespace::items::ContractId;
use fe_mir::ir::{function::Linkage, FunctionId};
use yultsur::{yul, *};

use crate::{
    db::CodegenDb,
    yul::{runtime::AbiSrcLocation, YulVariable},
};

use super::context::Context;

pub fn lower_contract_deployable(db: &dyn CodegenDb, contract: ContractId) -> yul::Object {
    let mut context = Context::default();

    let constructor = if let Some(init) = contract.init_function(db.upcast()) {
        let init = db.mir_lowered_func_signature(init);
        make_init(db, &mut context, contract, init)
    } else {
        statements! {}
    };

    let deploy_code = make_deploy(db, contract);

    let dep_functions: Vec<_> = context
        .resolve_function_dependency(db)
        .into_iter()
        .map(yul::Statement::FunctionDefinition)
        .collect();
    let runtime_funcs: Vec<_> = context
        .runtime
        .collect_definitions()
        .into_iter()
        .map(yul::Statement::FunctionDefinition)
        .collect();

    let deploy_block = block_statement! {
        [constructor...]
        [deploy_code...]
    };

    let code = code! {
        [deploy_block]
        [dep_functions...]
        [runtime_funcs...]
    };

    let mut dep_contracts = context.resolve_contract_dependency(db);
    dep_contracts.push(lower_contract(db, contract));
    let dep_constants = context.resolve_constant_dependency(db);

    let name = identifier! {(
        db.codegen_contract_deployer_symbol_name(contract).as_ref()
    )};
    let object = yul::Object {
        name,
        code,
        objects: dep_contracts,
        data: dep_constants,
    };

    normalize_object(object)
}

pub fn lower_contract(db: &dyn CodegenDb, contract: ContractId) -> yul::Object {
    let exported_funcs: Vec<_> = db
        .mir_lower_contract_all_functions(contract)
        .iter()
        .filter_map(|fid| {
            if fid.signature(db.upcast()).linkage == Linkage::Export {
                Some(*fid)
            } else {
                None
            }
        })
        .collect();

    let mut context = Context::default();
    let dispatcher = if let Some(call_fn) = contract.call_function(db.upcast()) {
        let call_fn = db.mir_lowered_func_signature(call_fn);
        context.function_dependency.insert(call_fn);
        let call_symbol = identifier! { (db.codegen_function_symbol_name(call_fn)) };
        statement! {
            ([call_symbol]())
        }
    } else {
        make_dispatcher(db, &mut context, &exported_funcs)
    };

    let dep_functions: Vec<_> = context
        .resolve_function_dependency(db)
        .into_iter()
        .map(yul::Statement::FunctionDefinition)
        .collect();
    let runtime_funcs: Vec<_> = context
        .runtime
        .collect_definitions()
        .into_iter()
        .map(yul::Statement::FunctionDefinition)
        .collect();

    let code = code! {
        ([dispatcher])
        [dep_functions...]
        [runtime_funcs...]
    };

    // Lower dependant contracts.
    let dep_contracts = context.resolve_contract_dependency(db);

    // Collect string constants.
    let dep_constants = context.resolve_constant_dependency(db);
    let contract_symbol = identifier! { (db.codegen_contract_symbol_name(contract)) };

    yul::Object {
        name: contract_symbol,
        code,
        objects: dep_contracts,
        data: dep_constants,
    }
}

fn make_dispatcher(
    db: &dyn CodegenDb,
    context: &mut Context,
    funcs: &[FunctionId],
) -> yul::Statement {
    let arms = funcs
        .iter()
        .map(|func| dispatch_arm(db, context, *func))
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

fn dispatch_arm(db: &dyn CodegenDb, context: &mut Context, func: FunctionId) -> yul::Case {
    context.function_dependency.insert(func);
    let func_sig = db.codegen_legalized_signature(func);
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
        let name = identifier! { (db.codegen_function_symbol_name(func)) };
        // we pass in a `0` for the expected `Context` argument
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
                (return(0, 0))
            }
        }
    };

    let abi_sig = db.codegen_abi_function(func);
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
    context: &mut Context,
    contract: ContractId,
    init: FunctionId,
) -> Vec<yul::Statement> {
    context.function_dependency.insert(init);
    let init_func_name = identifier! { (db.codegen_function_symbol_name(init)) };
    let contract_name = identifier_expression! { (format!{r#""{}""#, db.codegen_contract_deployer_symbol_name(contract)}) };

    let func_sig = db.codegen_legalized_signature(init);
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
