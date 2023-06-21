use super::context::Context;
use crate::db::CodegenDb;
use fe_analyzer::namespace::items::FunctionId;
use yultsur::{yul, *};

pub fn lower_test(db: &dyn CodegenDb, test: FunctionId) -> yul::Object {
    let mut context = Context::default();
    let test = db.mir_lowered_func_signature(test);
    context.function_dependency.insert(test);

    let dep_constants = context.resolve_constant_dependency(db);
    let dep_functions: Vec<_> = context
        .resolve_function_dependency(db)
        .into_iter()
        .map(yul::Statement::FunctionDefinition)
        .collect();
    let dep_contracts = context.resolve_contract_dependency(db);
    let runtime_funcs: Vec<_> = context
        .runtime
        .collect_definitions()
        .into_iter()
        .map(yul::Statement::FunctionDefinition)
        .collect();
    let call = {
        let test_func_name = identifier! { (db.codegen_function_symbol_name(test)) };
        let mut assignments = vec![];
        let mut call_args = vec![];

        let mut arg_num = 0;
        for param in test.signature(db.upcast()).params.iter() {
            if param.name != "ctx" {
                let arg_name = format!("arg_{}", arg_num);
                let arg_ident = identifier! { (arg_name) };
                let arg_offset = literal_expression! { (arg_num * 32) };

                assignments.append(&mut statements! {
                    (let [arg_ident.clone()] := mload([arg_offset.clone()]))
                    (mstore([arg_offset], 0))
                });

                call_args.push(identifier_expression! { [arg_ident] });

                arg_num += 1
            }
        }

        let call = function_call_statement! {[test_func_name]([call_args...])};

        statements! {
            [assignments...]
            [call]
        }
    };

    let code = code! {
        [dep_functions...]
        [runtime_funcs...]
        [call...]
        (stop())
    };

    let name = identifier! { test };
    let object = yul::Object {
        name,
        code,
        objects: dep_contracts,
        data: dep_constants,
    };

    normalize_object(object)
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
