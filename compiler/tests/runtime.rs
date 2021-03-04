//! Tests for the Fe runtime

#![cfg(feature = "solc-backend")]
use fe_compiler::yul::runtime::functions;
use yultsur::*;

mod utils;
use fe_analyzer::namespace::types::{
    Base,
    FixedSize,
    Integer,
    Struct,
};
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
fn test_runtime_alloc_and_avail() {
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
fn test_runtime_mcopys() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                (let a := 0x0111114211111111011111112342311101111112221151110111111111111111)
                (let b := 0x0111111234111111011123411111111101112431111111110111111234411111)
                (let c := 0x0111341111111111011111111123411101111123411111110111111234111111)

                (let d := 0x0111112344111111011111145111111101111111111111110111111111111111)
                (let e := 0x0111112344111111011111145111111101111111110000000000000000000000)

                (mstore(100, a))
                (mstore(132, b))
                (mstore(164, c))
                (mstore(196, d))

                (mcopys(100, 42, 117))

                [assert_eq!(a, (sload(42)))]
                [assert_eq!(b, (sload(43)))]
                [assert_eq!(c, (sload(44)))]
                [assert_eq!(e, (sload(45)))]

                (mcopys(100, 46, 128))

                [assert_eq!(a, (sload(46)))]
                [assert_eq!(b, (sload(47)))]
                [assert_eq!(c, (sload(48)))]
                [assert_eq!(d, (sload(49)))]
            },
        );
    })
}

#[test]
fn test_runtime_scopym() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                (let a := 0x0111114211111111011111112342311101111112221151110111111111111111)
                (let b := 0x0111111234111111011123411111111101112431111111110111111234411111)
                (let c := 0x0111341111111111011111111123411101111123411111110111111234111111)

                (let d := 0x0111112344111111011111145111111101111111111111110111111111111111)
                (let e := 0x0111112344111111011111145111111101111111110000000000000000000000)

                (sstore(42, a))
                (sstore(43, b))
                (sstore(44, c))
                (sstore(45, d))

                (let ptr1 := scopym(42, 117))
                (let ptr2 := add(ptr1, 32))
                (let ptr3 := add(ptr2, 32))
                (let ptr4 := add(ptr3, 32))

                [assert_eq!(a, (mload(ptr1)))]
                [assert_eq!(b, (mload(ptr2)))]
                [assert_eq!(c, (mload(ptr3)))]
                [assert_eq!(e, (mload(ptr4)))]

                (let ptr5 := scopym(42, 128))
                (let ptr6 := add(ptr5, 32))
                (let ptr7 := add(ptr6, 32))
                (let ptr8 := add(ptr7, 32))

                [assert_eq!(a, (mload(ptr5)))]
                [assert_eq!(b, (mload(ptr6)))]
                [assert_eq!(c, (mload(ptr7)))]
                [assert_eq!(d, (mload(ptr8)))]
            },
        );
    })
}

#[test]
fn test_runtime_mloadn() {
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
fn test_runtime_storage_sanity() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            vec![],
            statements! {
                (let a := 0x4200000000000000000000000000000000000000000000000000000000000026)
                (let b := 0x9900000000000000000000000000000000000000000000000000000000000077)
                (sstore(0, a))
                (sstore(1, b))

                [assert_eq!(a, (sload(0)))]
                [assert_eq!(b, (sload(1)))]
            },
        );
    })
}

#[test]
fn test_runtime_sloadn() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                (let a := 0x4200000000000000000000000000000000000000000000000000000000000026)
                (let b := 0x9900530000003900000000000000000000000000000000000000000000000077)
                (sstore(1000, a))
                (sstore(1001, b))

                [assert_eq!(a, (sloadn(1000, 0, 32)))]
                [assert_eq!(b, (sloadn(1001, 0, 32)))]

                [assert_eq!(0x42, (sloadn(1000, 0, 1)))]
                [assert_eq!(0x26, (sloadn(1000, 31, 1)))]
                [assert_eq!(0x4200, (sloadn(1000, 0, 2)))]

                [assert_eq!(0x99, (sloadn(1001, 0, 1)))]
                [assert_eq!(0x77, (sloadn(1001, 31, 1)))]
                [assert_eq!(0x990053, (sloadn(1001, 0, 3)))]
                [assert_eq!(0x5300000039, (sloadn(1001, 2, 5)))]
            },
        );
    })
}

#[test]
fn test_runtime_sstoren() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                (let a := 0x0111111111111111011111111111111101111111111111110111111111111111)
                //         dashes indicate which bytes are to be replaced in this test
                //         0----2          8----10      15----------------23            31--32
                (let b := 0x4201111111111111123411111111119999999998998999110111111111111126)
                (sstore(1000, a))

                (sstoren(1000, 0, 2, 0x4201))
                (sstoren(1000, 8, 2, 0x1234))
                (sstoren(1000, 15, 8, 0x9999999998998999))
                (sstoren(1000, 31, 1, 0x26))

                [assert_eq!(b, (sload(1000)))]

                (let c := 0x4242424242424242424242424242424242424242424242424242424242424242)
                (sstoren(1000, 0, 32, c))

                [assert_eq!(c, (sload(1000)))]
            },
        );
    })
}

#[test]
fn test_runtime_ceil32() {
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
fn test_runtime_ternary() {
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
fn test_runtime_abi_unpack() {
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

#[test]
fn test_runtime_set_zero() {
    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            functions::std(),
            statements! {
                (let a := 0x1111111111111111111111111111111111111111111111111111111111111111)
                (let b := 0x1111110000000000000000000000000000000000000000000000001111111111)
                (let c := 0x1111100000000000000000000000000000000000000000000000000000000000)

                [assert_eq!(0, (set_zero(0, 256, a)))]
                [assert_eq!(0x11, (set_zero(0, 248, a)))]
                [assert_eq!(b, (set_zero(24, 216, a)))]
                [assert_eq!(c, (set_zero(20, 256, a)))]
            },
        );
    })
}

#[test]
fn test_runtime_house_struct() {
    let mut house = Struct::new("House");
    house.add_field("price", &FixedSize::Base(Base::Numeric(Integer::U256)));
    house.add_field("size", &FixedSize::Base(Base::Numeric(Integer::U256)));
    house.add_field("rooms", &FixedSize::Base(Base::Numeric(Integer::U8)));
    house.add_field("vacant", &FixedSize::Base(Base::Bool));
    let house_api = functions::structs::struct_apis(house);

    with_executor(&|mut executor| {
        test_runtime_functions(
            &mut executor,
            [functions::std(), house_api.clone()].concat(),
            statements! {
                (let price := 42)
                (let size := 26)
                (let rooms := 5)
                (let vacant := true)

                (let house := struct_House_new(price, size, rooms, vacant))

                // For now, all values in a struct occupy a full word. Here we test
                // that the word at the given word offset contains the correct 32
                // byte value.
                //
                // This test confirms that each value is being stored right-aligned
                // and in the correct word.
                [assert_eq!(price, (mload(house)))]
                [assert_eq!(size, (mload((add(house, 32)))))]
                [assert_eq!(rooms, (mload((add(house, 64)))))]
                [assert_eq!(vacant, (mload((add(house, 96)))))]

                // To retrieve an individual struct value, we need a pointer that
                // references the start of the value.
                [assert_eq!(price, (mloadn((struct_House_get_price_ptr(house)), 32)))]
                [assert_eq!(size, (mloadn((struct_House_get_size_ptr(house)), 32)))]
                [assert_eq!(rooms, (mloadn((struct_House_get_rooms_ptr(house)), 1)))]
                [assert_eq!(vacant, (mloadn((struct_House_get_vacant_ptr(house)), 1)))]


                // We test the same thing in storage.

                // Note that the storage pointer for `house` is a multiple of 32.
                (let house_storage := 2048)
                (bytes_mcopys(house, house_storage, 128))

                [assert_eq!(price, (bytes_sloadn((struct_House_get_price_ptr(house_storage)), 32)))]
                [assert_eq!(size, (bytes_sloadn((struct_House_get_size_ptr(house_storage)), 32)))]
                [assert_eq!(rooms, (bytes_sloadn((struct_House_get_rooms_ptr(house_storage)), 1)))]
                [assert_eq!(vacant, (bytes_sloadn((struct_House_get_vacant_ptr(house_storage)), 1)))]
            },
        );
    })
}
