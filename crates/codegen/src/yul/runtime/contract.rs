use crate::{
    db::CodegenDb,
    yul::{runtime::AbiSrcLocation, YulVariable},
};

use super::{DefaultRuntimeProvider, RuntimeFunction, RuntimeProvider};

use fe_analyzer::namespace::items::ContractId;
use fe_mir::ir::{FunctionId, Type, TypeKind};

use yultsur::*;

pub(super) fn make_create(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    contract: ContractId,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let contract_symbol = literal_expression! {
        (format!(r#""{}""#, db.codegen_contract_deployer_symbol_name(contract)))
    };

    let size = YulVariable::new("size");
    let value = YulVariable::new("value");
    let func = function_definition! {
        function [func_name.ident()]([value.ident()]) -> addr {
            (let [size.ident()] := datasize([contract_symbol.clone()]))
            (let mem_ptr := [provider.avail(db)])
            (let contract_ptr := dataoffset([contract_symbol]))
            (datacopy(mem_ptr, contract_ptr, [size.expr()]))
            (addr := create([value.expr()], mem_ptr, [size.expr()]))
        }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_create2(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    contract: ContractId,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let contract_symbol = literal_expression! {
        (format!(r#""{}""#, db.codegen_contract_deployer_symbol_name(contract)))
    };

    let size = YulVariable::new("size");
    let value = YulVariable::new("value");
    let func = function_definition! {
        function [func_name.ident()]([value.ident()], salt) -> addr {
            (let [size.ident()] := datasize([contract_symbol.clone()]))
            (let mem_ptr := [provider.avail(db)])
            (let contract_ptr := dataoffset([contract_symbol]))
            (datacopy(mem_ptr, contract_ptr, [size.expr()]))
            (addr := create2([value.expr()], mem_ptr, [size.expr()], salt))
        }
    };

    RuntimeFunction::from_statement(func)
}

pub(super) fn make_external_call(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    function: FunctionId,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let sig = db.codegen_legalized_signature(function);
    let param_num = sig.params.len();

    let mut args = Vec::with_capacity(param_num);
    let mut arg_tys = Vec::with_capacity(param_num);
    for param in &sig.params {
        args.push(YulVariable::new(param.name.as_str()));
        arg_tys.push(param.ty);
    }
    let ret_ty = sig.return_type;

    let func_addr = YulVariable::new("func_addr");
    let params: Vec<_> = args.iter().map(YulVariable::ident).collect();
    let params_expr: Vec<_> = args.iter().map(YulVariable::expr).collect();
    let input = YulVariable::new("input");
    let input_size = YulVariable::new("input_size");
    let output_size = YulVariable::new("output_size");
    let output = YulVariable::new("output");

    let func_selector = literal_expression! { (format!{"0x{}", db.codegen_abi_function(function).selector().hex()}) };
    let selector_ty = db.mir_intern_type(Type::new(TypeKind::U32, None).into());

    let mut body = statements! {
            (let [input.ident()] := [provider.avail(db)])
            [yul::Statement::Expression(provider.ptr_store(db, input.expr(), func_selector, selector_ty.make_mptr(db.upcast())))]
            (let [input_size.ident()] := add(4, [provider.abi_encode_seq(db, &params_expr, expression!{ add([input.expr()], 4) }, &arg_tys, false)]))
            (let [output.ident()] := add([provider.avail(db)], [input_size.expr()]))
            (let success := call((gas()), [func_addr.expr()], 0, [input.expr()], [input_size.expr()], 0, 0))
            (let [output_size.ident()] := returndatasize())
            (returndatacopy([output.expr()], 0, [output_size.expr()]))
            (if (iszero(success)) {
                (revert([output.expr()], [output_size.expr()]))
            })
    };
    let func = if let Some(ret_ty) = ret_ty {
        let ret = YulVariable::new("$ret");
        body.push(
            statement!{
                [ret.ident()] := [provider.abi_decode(db, output.expr(), output_size.expr(), &[ret_ty], AbiSrcLocation::Memory)]
            }
        );
        function_definition! {
            function [func_name.ident()]([func_addr.ident()], [params...]) -> [ret.ident()] {
                [body...]
            }
        }
    } else {
        function_definition! {
            function [func_name.ident()]([func_addr.ident()], [params...]) {
                [body...]
            }
        }
    };

    RuntimeFunction::from_statement(func)
}
