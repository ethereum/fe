use common::InputDb;
use contract_harness::{ExecutionOptions, RuntimeInstance as EvmRuntime};
use driver::DriverDataBase;
use hir::analysis::semantic::FieldIndex;
use mir::{
    ConstScalar, MirDb, PlaceElem, PlaceRoot, RBlock, RExpr, RLocal, RLocalId, RStmt, RTerminator,
    RefKind, RefView, RuntimeBody, RuntimeCarrier, RuntimeClass, RuntimeInstance, RuntimeLocalRoot,
    RuntimePackage, RuntimePlace, build_runtime_package, verify_runtime_body,
};
#[cfg(all(
    feature = "cranelift",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]
use mir::{RuntimeLinkage, build_native_executable_package};
use sonatina_ir::{builder::ModuleBuilder, isa::Isa, module::ModuleCtx};
use url::Url;

use super::{FunctionLowerer, ModuleLowerer};
use crate::{
    OptLevel,
    sonatina::{create_evm_isa, emit_runtime_module_sonatina_bytecode_with_options},
};

#[cfg(all(
    feature = "cranelift",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]
#[test]
fn native_extern_cannot_claim_the_memory_copy_symbol() {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///native_memmove_extern_collision.fe").unwrap(),
        Some(
            r#"
use core::ptr
use std::io::{Write, host, write_char}
pub fn main() -> i32 {
    let data = ptr::alloc_bytes(1)
    ptr::copy_raw(dest: data, source: data, len: 1)
    with (Write = host()) { write_char(65) }
    0
}
"#
            .into(),
        ),
    );
    let package = build_native_executable_package(&db, db.top_mod(file)).unwrap();
    let host_import = package
        .functions(&db)
        .into_iter()
        .find(|function| function.linkage(&db) == RuntimeLinkage::External)
        .expect("reachable host import");
    // Model a fixed host import claiming the symbol reserved by native copy lowering.
    let isa = crate::sonatina::create_native_isa().unwrap();
    let mut lowerer = ModuleLowerer::new(
        &db,
        ModuleBuilder::new(ModuleCtx::new(&isa)),
        isa.inst_set(),
        &package,
        Some((host_import.instance(&db), "memmove")),
    );
    let error = lowerer
        .declare_functions()
        .expect_err("extern memmove must not collide with the memory-copy host symbol");
    assert!(
        error
            .to_string()
            .contains("native extern symbol `memmove` is reserved for memory copies"),
        "unexpected error: {error}"
    );
}

/// Deploys `contract` from `package` with `target`'s body replaced by `body`.
fn deploy_with_body<'db>(
    db: &'db DriverDataBase,
    package: RuntimePackage<'db>,
    target: RuntimeInstance<'db>,
    body: RuntimeBody<'db>,
    opt: OptLevel,
    contract: &str,
) -> EvmRuntime {
    let program: &dyn MirDb = db;
    verify_runtime_body(db, &program, &body).expect("valid runtime body");

    let isa = create_evm_isa();
    let mut lowerer = ModuleLowerer::new(
        db,
        ModuleBuilder::new(ModuleCtx::new(&isa)),
        isa.inst_set(),
        &package,
        None,
    );
    lowerer.declare_functions().unwrap();
    lowerer.lower_const_regions().unwrap();
    for function in package.functions(db) {
        let instance = function.instance(db);
        let selected = if instance == target {
            body.clone()
        } else {
            instance.body(db)
        };
        let func_ref = lowerer.func_ref(instance).unwrap();
        FunctionLowerer::new(&mut lowerer, selected, func_ref)
            .unwrap()
            .lower()
            .unwrap();
    }
    lowerer.declare_objects().unwrap();
    let (mut artifacts, _) = emit_runtime_module_sonatina_bytecode_with_options(
        db,
        &package,
        lowerer.finish().0,
        opt,
        false,
        None,
    )
    .unwrap_or_else(|error| panic!("compile {contract} at {opt:?}: {error}"));
    let artifact = artifacts.remove(contract).unwrap();
    EvmRuntime::deploy(&hex::encode(artifact.deploy)).unwrap()
}

#[test]
fn stack_native_reference_borrows_preserve_slot_contents_and_identity() {
    for (opt, replace) in [
        (OptLevel::O0, false),
        (OptLevel::O0, true),
        (OptLevel::O2, false),
        (OptLevel::O2, true),
    ] {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///stack_native_reference.fe").unwrap(),
            Some(
                r#"
#[inline(never)]
fn borrow_slot() -> u256 { 0 }
msg SlotMsg {
    #[selector = 1]
    Read -> u256,
}
pub contract SlotContract {
    recv SlotMsg { Read -> u256 { borrow_slot() } }
}
"#
                .into(),
            ),
        );
        let top_mod = db.top_mod(file);
        let diagnostics = db.run_on_top_mod(top_mod);
        assert!(diagnostics.is_empty(), "{}", diagnostics.format_diags(&db));
        let package = build_runtime_package(&db, top_mod).expect("runtime package");
        let target = package
            .functions(&db)
            .iter()
            .find(|function| function.symbol(&db) == "borrow_slot")
            .expect("slot helper")
            .instance(&db);
        let mut body = target.body(&db);
        let scalar = body.signature.ret.clone().expect("word return");
        let semantic_ty = body.locals[0].semantic_ty;
        let origin = body.terminator_origins[0];
        let object_ref = RuntimeClass::Ref {
            pointee: Box::new(scalar.clone()),
            kind: RefKind::Object,
            view: RefView::Whole,
        };
        let native_ref = RuntimeClass::Ref {
            pointee: Box::new(scalar.clone()),
            kind: RefKind::Native,
            view: RefView::Whole,
        };
        let slot_ref = RuntimeClass::Ref {
            pointee: Box::new(native_ref.clone()),
            kind: RefKind::Native,
            view: RefView::Whole,
        };
        body.locals = [
            (scalar.clone(), true),
            (object_ref.clone(), false),
            (native_ref.clone(), false),
            (native_ref.clone(), true),
            (slot_ref, false),
            (native_ref.clone(), false),
            (scalar.clone(), false),
            (scalar, true),
            (object_ref, false),
            (native_ref, false),
        ]
        .into_iter()
        .map(|(class, stored)| RLocal {
            semantic_ty,
            carrier: RuntimeCarrier::Value(class.clone()),
            root: if stored {
                RuntimeLocalRoot::Slot(class)
            } else {
                RuntimeLocalRoot::None
            },
        })
        .collect();
        let local = RLocalId::from_u32;
        let place = |root| RuntimePlace {
            root,
            path: Box::default(),
        };
        let assign = |dst, expr| RStmt::Assign {
            dst: local(dst),
            expr,
        };
        let constant = |value: u64| {
            RExpr::ConstScalar(ConstScalar::Int {
                words: value.to_be_bytes().to_vec(),
                bits: 256,
                signed: false,
            })
        };
        // Exercise the verified runtime-IR contract directly: slot 3 contains a
        // native carrier, and taking its address must create a second descriptor.
        let mut stmts = vec![
            assign(0, constant(37)),
            assign(
                1,
                RExpr::AddrOf {
                    place: place(PlaceRoot::Slot(local(0))),
                },
            ),
            assign(2, RExpr::NativeRef { value: local(1) }),
            assign(3, RExpr::Use(local(2))),
            assign(
                4,
                RExpr::AddrOf {
                    place: place(PlaceRoot::Slot(local(3))),
                },
            ),
        ];
        if replace {
            stmts.extend([
                assign(7, constant(73)),
                assign(
                    8,
                    RExpr::AddrOf {
                        place: place(PlaceRoot::Slot(local(7))),
                    },
                ),
                assign(9, RExpr::NativeRef { value: local(8) }),
                RStmt::Store {
                    dst: place(PlaceRoot::Ref(local(4))),
                    src: local(9),
                },
            ]);
        }
        // Read through the borrow, or read the original slot after updating it
        // through the borrow. Both must preserve the stored carrier's identity.
        let read = if replace {
            PlaceRoot::Slot(local(3))
        } else {
            PlaceRoot::Ref(local(4))
        };
        stmts.extend([
            assign(5, RExpr::Load { place: place(read) }),
            assign(
                6,
                RExpr::Load {
                    place: place(PlaceRoot::Ref(local(5))),
                },
            ),
        ]);
        body.stmt_origins = vec![vec![origin; stmts.len()]];
        body.terminator_origins = vec![origin];
        body.blocks = vec![RBlock {
            stmts,
            terminator: RTerminator::Return(Some(local(6))),
        }];
        let mut runtime = deploy_with_body(&db, package, target, body, opt, "SlotContract");
        let result = runtime
            .call_raw(&[0, 0, 0, 1], ExecutionOptions::default())
            .unwrap_or_else(|error| panic!("{opt:?}, replace={replace}: {error}"));
        let mut expected = vec![0; 32];
        expected[31] = if replace { 73 } else { 37 };
        assert_eq!(result.return_data, expected, "{opt:?}, replace={replace}");
    }
}

#[test]
fn stored_references_to_storage_keep_packed_newtype_lanes() {
    // A stored reference to a storage `S`, projected to the value of a packed
    // newtype that shares its slot (`tag`, slot 0 from byte 1) and of one
    // narrowed in a slot of its own (`lone`, slot 2). No source program
    // stores a reference to storage, so the reference is built by hand.
    const SOURCE: &str = r#"
use std::abi::sol::Bytes4
use std::evm::RawStorage

struct S {
    flag: bool,
    tag: Bytes4,
    big: u256,
    lone: Bytes4,
    after: u256,
}

#[inline(never)]
fn probe(_ s: mut S, _ value: u256) -> u256 {
    value
}

msg M {
    #[selector = 1]
    Probe { value: u256 } -> u256,
    #[selector = 2]
    Slot { slot: u256 } -> u256,
    #[selector = 3]
    Dirty { slot: u256, value: u256 },
}

pub contract C {
    mut s: S,

    recv M {
        Probe { value } -> u256 uses (mut s) {
            probe(mut s, value)
        }
        Slot { slot } -> u256 uses (raw: RawStorage) {
            unsafe { raw.sload(slot) }
        }
        Dirty { slot, value } uses (raw: mut RawStorage) {
            unsafe { raw.sstore(slot, value) }
        }
    }
}
"#;
    // A word with `low` in its low bytes, and bit 200 set when `high`.
    let word = |low: u64, high: bool| {
        let mut word = [0; 32];
        word[24..].copy_from_slice(&low.to_be_bytes());
        word[6] = u8::from(high);
        word
    };
    let call = |runtime: &mut EvmRuntime, selector: u8, args: &[[u8; 32]]| {
        let mut calldata = vec![0, 0, 0, selector];
        calldata.extend(args.iter().flatten());
        runtime.call_raw(&calldata, ExecutionOptions::default())
    };
    for opt in [OptLevel::O0, OptLevel::O2] {
        for (field, slot, offset) in [(1, 0, 1), (3, 2, 0)] {
            for write in [false, true] {
                let mut db = DriverDataBase::default();
                let file = db.workspace().touch(
                    &mut db,
                    Url::parse("file:///stored_lane_reference.fe").unwrap(),
                    Some(SOURCE.into()),
                );
                let top_mod = db.top_mod(file);
                let diagnostics = db.run_on_top_mod(top_mod);
                assert!(diagnostics.is_empty(), "{}", diagnostics.format_diags(&db));
                let package = build_runtime_package(&db, top_mod).expect("runtime package");
                let target = package
                    .functions(&db)
                    .iter()
                    .find(|function| function.symbol(&db) == "probe")
                    .expect("probe")
                    .instance(&db);
                let mut body = target.body(&db);
                let [s, value] = [0, 1].map(|idx| body.signature.params[idx].local);
                let RuntimeClass::Ref { pointee, .. } = body.signature.params[0].class.clone()
                else {
                    panic!("`s` is a reference");
                };
                let reference = RLocalId::from_u32(body.locals.len() as u32);
                let read = RLocalId::from_u32(reference.as_u32() + 1);
                let semantic_ty = body.locals[0].semantic_ty;
                body.locals.extend(
                    [
                        RuntimeClass::Ref {
                            pointee,
                            kind: RefKind::Native,
                            view: RefView::Whole,
                        },
                        body.signature.ret.clone().expect("word return"),
                    ]
                    .map(|class| RLocal {
                        semantic_ty,
                        carrier: RuntimeCarrier::Value(class),
                        root: RuntimeLocalRoot::None,
                    }),
                );
                // `s.<field>.val`, through the stored reference.
                let place = RuntimePlace {
                    root: PlaceRoot::Ref(reference),
                    path: Box::new([
                        PlaceElem::Field(FieldIndex(field)),
                        PlaceElem::Field(FieldIndex(0)),
                    ]),
                };
                let stmts = vec![
                    RStmt::Assign {
                        dst: reference,
                        expr: RExpr::NativeRef { value: s },
                    },
                    if write {
                        RStmt::Store {
                            dst: place,
                            src: value,
                        }
                    } else {
                        RStmt::Assign {
                            dst: read,
                            expr: RExpr::Load { place },
                        }
                    },
                ];
                let origin = body.terminator_origins[0];
                body.stmt_origins = vec![vec![origin; stmts.len()]];
                body.terminator_origins = vec![origin];
                body.blocks = vec![RBlock {
                    stmts,
                    terminator: RTerminator::Return(Some(if write { value } else { read })),
                }];
                let mut runtime = deploy_with_body(&db, package, target, body, opt, "C");

                // The shared lane's slot also holds `flag`.
                let rest = u64::from(offset == 1);
                let case = format!("{opt:?}, field {field}, write={write}");
                if write {
                    call(&mut runtime, 3, &[word(slot, false), word(rest, false)]).unwrap();
                    assert!(
                        call(&mut runtime, 1, &[word(1 << 32, false)]).is_err(),
                        "{case}: a value wider than the lane must revert"
                    );
                    call(&mut runtime, 1, &[word(0x11223344, false)]).unwrap();
                    let stored = call(&mut runtime, 2, &[word(slot, false)]).unwrap();
                    assert_eq!(
                        stored.return_data,
                        word(rest | (0x11223344 << (8 * offset)), false),
                        "{case}"
                    );
                } else {
                    // Bits above the lane are not part of its value.
                    let dirty = rest | (0x11223344 << (8 * offset));
                    call(&mut runtime, 3, &[word(slot, false), word(dirty, true)]).unwrap();
                    let read = call(&mut runtime, 1, &[word(0, false)]).unwrap();
                    assert_eq!(read.return_data, word(0x11223344, false), "{case}");
                }
            }
        }
    }
}
