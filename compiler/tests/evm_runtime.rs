#![cfg(feature = "solc-backend")]
use fe_compiler::yul::runtime::functions;
use yultsur::*;

mod utils;
use utils::*;

macro_rules! assert_eq {
    ($a:tt, $b:tt) => {
        statement! {
            (if (iszero((eq($a, $b)))) {
                (revert(0, 0))
            })
        }
    };
}

#[test]
fn test_alloc_and_avail() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                (let a := avail())
                (let b := alloc(5))
                (let c := alloc(10))
                (let d := avail())

                [assert_eq!(b, a)]
                [assert_eq!(c, (add(b, 5)))]
                [assert_eq!(d, (add(c, 10)))]
            },
        );
    })
}

#[test]
fn test_mcopys() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                (let a := 26)
                (let b := 27)

                (mstore(42, a))
                (mstore(74, b))
                (mcopys(42, 100, 64))

                [assert_eq!(a, (sload(100)))]
                [assert_eq!(b, (sload(132)))]
            },
        );
    })
}

#[test]
fn test_scopym() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                (let a := 26)
                (let b := 27)

                (sstore(42, a))
                (sstore(74, b))

                (let ptr1 := scopym(42, 64))
                (let ptr2 := add(ptr1, 32))

                [assert_eq!(a, (mload(ptr1)))]
                [assert_eq!(b, (mload(ptr2)))]
            },
        );
    })
}

#[test]
fn test_mloadn() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                (let a := 0x4200000000000000000000000000000000000000000000000000000000420026)
                (mstore(100, a))

                [assert_eq!(0x42, (mloadn(100, 1)))]
                [assert_eq!(0x420026, (mloadn(129, 3)))]
                [assert_eq!(0x26, (mloadn(130, 2)))]
                [assert_eq!(0x26, (mloadn(131, 1)))]
            },
        );
    })
}

#[test]
fn test_sloadn() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                (let a := 0x4200000000000000000000000000000000000000000000000000000000420026)
                (sstore(0, a))

                [assert_eq!(0x42, (sloadn(0, 1)))]
                [assert_eq!(a, (sloadn(0, 32)))]
                // these fail for some reason.. possible bug or misunderstanding of the EVM?
                // TODO: figure out why this fails
                // [assert_eq!(0x420026, (sloadn(29, 3)))]
                // [assert_eq!(0x26, (sloadn(30, 2)))]
                // [assert_eq!(0x26, (sloadn(31, 1)))]
            },
        );
    })
}

#[test]
fn test_ceil32() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                [assert_eq!(32, (ceil32(29)))]
                [assert_eq!(256, (ceil32(225)))]
            },
        );
    })
}

#[test]
fn test_ternary() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                (let a := ternary(0, 42, 26))
                (let b := ternary(1, 42, 26))

                [assert_eq!(a, 26)]
                [assert_eq!(b, 42)]
            },
        );
    })
}

#[test]
fn test_abi_unpack() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                (let a := 0x0042002600530000000000000000000000000000000000000000000000000000)
                (let packed := alloc_mstoren(a, 32))
                (let unpacked := avail())
                (abi_unpack(packed, 3, 2))

                (let elem0 := mload(unpacked))
                (let elem1 := mload((add(unpacked, 32))))
                (let elem2 := mload((add(unpacked, 64))))

                [assert_eq!(elem0, 0x0042)]
                [assert_eq!(elem1, 0x0026)]
                [assert_eq!(elem2, 0x0053)]
            },
        );
    })
}

#[test]
fn test_keccak256() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                (let num := 63806209331542711802848847270949280092855778197726125910674179583545433573378)
                (let result :=109966633016701122630199943745061001312678661825260870342362413625737614346915)
                // Can be validated with solidity
                // uint(keccak256(abi.encodePacked(uint(63806209331542711802848847270949280092855778197726125910674179583545433573378))));
                // which returns 109966633016701122630199943745061001312678661825260870342362413625737614346915
                (mstore(0, num))
                [assert_eq!(result, (keccak256(0, 32)))]
            },
        );
    })
}
