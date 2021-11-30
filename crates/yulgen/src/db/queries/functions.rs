use crate::context::FnContext;
use crate::db::YulgenDb;
use crate::mappers::functions::multiple_func_stmt;
use crate::names;
use crate::operations::abi as abi_operations;
use crate::runtime::functions;
use crate::types::{to_abi_selector_names, to_abi_types, AbiDecodeLocation, AbiType, AsAbiType};
use fe_abi::utils as abi_utils;
use fe_analyzer::namespace::items::{Class, FunctionId, Item, StructId, TypeDef};
use fe_analyzer::namespace::types::{Struct, Type};
use fe_parser::{ast, node::Node};
use indexmap::IndexSet;
use smol_str::SmolStr;
use std::rc::Rc;
use yultsur::*;

pub fn function_yul_name(db: &dyn YulgenDb, function: FunctionId) -> SmolStr {
    // foo::Bar::new => $$foo$Bar$new
    format!("$${}", Item::Function(function).path(db.upcast()).join("$")).into()
}

pub fn function_def(db: &dyn YulgenDb, function: FunctionId) -> yul::Statement {
    let analyzer_db = db.upcast();
    let sig = function.signature(analyzer_db);

    let mut param_names = if sig.self_decl.is_some()
        && matches!(function.parent(analyzer_db), Item::Type(TypeDef::Struct(_)))
    {
        // struct member functions take `$self` in yul
        vec![names::var_name("self")]
    } else {
        vec![]
    };
    param_names.extend(sig.params.iter().map(|param| names::var_name(&param.name)));

    let mut fn_context = FnContext::new(db, function.body(analyzer_db));
    let function_statements =
        multiple_func_stmt(&mut fn_context, &function.data(analyzer_db).ast.kind.body);

    let function_name = identifier! { (db.function_yul_name(function)) };
    // all user-defined functions are given a return value during lowering
    function_definition! {
        function [function_name]([param_names...]) -> return_val {
            [function_statements...]
        }
    }
}

pub fn function_sig_abi_types(
    db: &dyn YulgenDb,
    function: FunctionId,
) -> (Rc<[AbiType]>, Option<AbiType>) {
    let adb = db.upcast();
    let sig = function.signature(adb);
    let return_type = sig.return_type.clone().expect("return type error");
    (
        to_abi_types(adb, &sig.param_types()).into(),
        if !return_type.is_unit() {
            Some(return_type.as_abi_type(adb))
        } else {
            None
        },
    )
}

pub fn revert_types(db: &dyn YulgenDb, function: FunctionId) -> Rc<IndexSet<StructId>> {
    let body = function.body(db.upcast());

    let mut structs = IndexSet::new();
    for_each_stmt(&function.data(db.upcast()).ast.kind.body, &mut |stmt| {
        if let ast::FuncStmt::Revert { error: Some(node) } = stmt {
            let attr = body
                .expressions
                .get(&node.id)
                .expect("missing expr attributes");
            if let Type::Struct(Struct { id, .. }) = &attr.typ {
                structs.insert(*id);
            }
        }
    });

    Rc::new(structs)
}

pub fn assert_string_types(db: &dyn YulgenDb, function: FunctionId) -> Rc<IndexSet<AbiType>> {
    let body = function.body(db.upcast());

    let mut strings = IndexSet::new();
    for_each_stmt(&function.data(db.upcast()).ast.kind.body, &mut |stmt| {
        if let ast::FuncStmt::Assert {
            msg: Some(node), ..
        } = stmt
        {
            let attr = body
                .expressions
                .get(&node.id)
                .expect("missing expr attributes");
            if let Type::String(string) = &attr.typ {
                strings.insert(string.as_abi_type(db.upcast()));
            }
        }
    });

    Rc::new(strings)
}

pub fn function_external_call_name(db: &dyn YulgenDb, function: FunctionId) -> SmolStr {
    // foo::Bar::new => $$foo$Bar$new
    format!("call_{}", db.function_yul_name(function)).into()
}

/// Create a yul function to make a call to an external contract function.
/// Includes required encode/decode functions.
pub fn function_external_call_fn(db: &dyn YulgenDb, function: FunctionId) -> Vec<yul::Statement> {
    let adb = db.upcast();
    if !matches!(function.class(adb), Some(Class::Contract(_))) {
        panic!("external call to non-contract fn")
    };

    let function_name = function.name(adb);
    // get the name of the call function and its parameters
    let call_fn_name = identifier! { (db.function_external_call_name(function)) };
    let (param_types, return_type) = db.function_sig_abi_types(function);

    // create a pair of identifiers and expressions for the parameters
    let (param_idents, param_exprs) = names::abi::vals("param", param_types.len());
    // the function selector must be added to the first 4 bytes of the calldata
    let selector = {
        let selector =
            abi_utils::func_selector(&function_name, &to_abi_selector_names(&param_types));
        literal_expression! { (selector) }
    };

    // the size of the encoded data
    let encoding_size = abi_operations::encoding_size(&param_types, &param_exprs);
    // the operations used to encode the parameters
    let encoding_operation = abi_operations::encode(&param_types, param_exprs);

    let mut fns = vec![functions::abi::encode(&param_types)];

    if let Some(return_type) = return_type {
        fns.extend(functions::abi::decode_functions(
            &[return_type.clone()],
            AbiDecodeLocation::Memory,
        ));
        let decoding_operation = abi_operations::decode_data(
            &[return_type],
            expression! { outstart },
            expression! { add(outstart, outsize) },
            AbiDecodeLocation::Memory,
        );
        // return data must be captured and decoded
        fns.push(function_definition! {
            function [call_fn_name](addr, [param_idents...]) -> return_val {
                (let instart := alloc_mstoren([selector], 4))
                    (let insize := add(4, [encoding_size]))
                    (pop([encoding_operation]))
                    (let success := call((gas()), addr, 0, instart, insize, 0, 0))
                    (let outsize := returndatasize())
                    (let outstart := alloc(outsize))
                    (returndatacopy(outstart, 0, outsize))
                    (if (iszero(success)) { (revert(outstart, outsize)) })
                    (return_val := [decoding_operation])
            }
        })
    } else {
        // unit type; there is no return data to handle
        fns.push(function_definition! {
            function [call_fn_name](addr, [param_idents...]) -> return_val {
                (let instart := alloc_mstoren([selector], 4))
                    (let insize := add(4, [encoding_size]))
                    (pop([encoding_operation]))
                    (let success := call((gas()), addr, 0, instart, insize, 0, 0))
                    (if (iszero(success)) {
                        (let outsize := returndatasize())
                            (let outstart := alloc(outsize))
                            (returndatacopy(outstart, 0, outsize))
                            (revert(outstart, outsize))
                    })
            }
        })
    }
    fns
}

fn for_each_stmt<F>(stmts: &[Node<ast::FuncStmt>], f: &mut F)
where
    F: FnMut(&ast::FuncStmt),
{
    for node in stmts {
        f(&node.kind);
        match &node.kind {
            ast::FuncStmt::For { body, .. } => for_each_stmt(body, f),
            ast::FuncStmt::While { body, .. } => for_each_stmt(body, f),
            ast::FuncStmt::If { body, or_else, .. } => {
                for_each_stmt(body, f);
                for_each_stmt(or_else, f);
            }
            _ => {}
        }
    }
}
