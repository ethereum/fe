use crate::{
    db::CodegenDb,
    yul::{slot_size::function_hash_type, YulVariable},
};

use super::{DefaultRuntimeProvider, RuntimeFunction, RuntimeProvider};

use fe_abi::function::{AbiFunction, AbiFunctionType, StateMutability};
use fe_mir::ir::{self, TypeId};
use yultsur::*;

pub(super) fn make_revert(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
    arg_name: &str,
    arg_ty: TypeId,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let arg = YulVariable::new("arg");

    let abi_size = YulVariable::new("abi_size");
    let abi_tmp_ptr = YulVariable::new("$abi_tmp_ptr");
    let signature = type_signature_for_revert(db, arg_name, arg_ty);

    let signature_store = yul::Statement::Expression(provider.ptr_store(
        db,
        abi_tmp_ptr.expr(),
        signature,
        function_hash_type(db).make_mptr(db.upcast()),
    ));

    let func = if arg_ty.deref(db.upcast()).is_zero_sized(db.upcast()) {
        function_definition! {
            function [func_name.ident()]() {
                (let [abi_tmp_ptr.ident()] := [provider.avail(db)])
                ([signature_store])
                (revert([abi_tmp_ptr.expr()], [literal_expression!{4}]))
            }
        }
    } else {
        let encode = provider.abi_encode_seq(
            db,
            &[arg.expr()],
            expression! { add([abi_tmp_ptr.expr()], 4) },
            &[arg_ty],
            false,
        );

        function_definition! {
            function [func_name.ident()]([arg.ident()]) {
                (let [abi_tmp_ptr.ident()] := [provider.avail(db)])
                ([signature_store])
                (let [abi_size.ident()] := add([encode], 4))
                (revert([abi_tmp_ptr.expr()], [abi_size.expr()]))
            }
        }
    };

    RuntimeFunction::from_statement(func)
}

/// Returns signature hash of the type.
fn type_signature_for_revert(db: &dyn CodegenDb, name: &str, ty: TypeId) -> yul::Expression {
    let deref_ty = ty.deref(db.upcast());
    let ty_data = deref_ty.data(db.upcast());
    let args = match &ty_data.kind {
        ir::TypeKind::Struct(def) => def
            .fields
            .iter()
            .map(|(_, ty)| ("".to_string(), db.codegen_abi_type(*ty)))
            .collect(),
        _ => {
            let abi_ty = db.codegen_abi_type(deref_ty);
            vec![("_".to_string(), abi_ty)]
        }
    };

    // selector and state mutability is independent we can set has_self and has_ctx any value.
    let selector = AbiFunction::new(
        AbiFunctionType::Function,
        name.to_string(),
        args,
        None,
        StateMutability::Pure,
    )
    .selector();
    let type_sig = selector.hex();
    literal_expression! {(format!{"0x{type_sig}" })}
}
