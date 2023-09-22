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
    let test_func_name = identifier! { (db.codegen_function_symbol_name(test)) };
    let call = function_call_statement! {[test_func_name]()};

    let code = code! {
        [dep_functions...]
        [runtime_funcs...]
        [call]
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
