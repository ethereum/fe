use crate::namespace::types::Base;
use strum::{AsRefStr, EnumIter, EnumString};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, EnumString, AsRefStr)]
#[strum(serialize_all = "snake_case")]
pub enum ValueMethod {
    ToMem,
    AbiEncode,
}

#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString, AsRefStr, EnumIter,
)]
#[strum(serialize_all = "snake_case")]
pub enum GlobalFunction {
    Keccak256,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, EnumString, AsRefStr)]
#[strum(serialize_all = "snake_case")]
pub enum ContractTypeMethod {
    Create,
    Create2,
}

impl ContractTypeMethod {
    pub fn arg_count(&self) -> usize {
        match self {
            ContractTypeMethod::Create => 2,
            ContractTypeMethod::Create2 => 3,
        }
    }
}

/// The evm functions exposed by yul.
#[allow(non_camel_case_types)]
#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString, AsRefStr, EnumIter,
)]
pub enum Intrinsic {
    __stop,           // () -> ()
    __add,            // (x, y)
    __sub,            // (x, y)
    __mul,            // (x, y)
    __div,            // (x, y)
    __sdiv,           // (x, y)
    __mod,            // (x, y)
    __smod,           // (x, y)
    __exp,            // (x, y)
    __not,            // (x)
    __lt,             // (x, y)
    __gt,             // (x, y)
    __slt,            // (x, y)
    __sgt,            // (x, y)
    __eq,             // (x, y)
    __iszero,         // (x)
    __and,            // (x, y)
    __or,             // (x, y)
    __xor,            // (x, y)
    __byte,           // (n, x)
    __shl,            // (x, y)
    __shr,            // (x, y)
    __sar,            // (x, y)
    __addmod,         // (x, y, m)
    __mulmod,         // (x, y, m)
    __signextend,     // (i, x)
    __keccak256,      // (p, n)
    __pc,             // ()
    __pop,            // (x) -> ()
    __mload,          // (p)
    __mstore,         // (p, v) -> ()
    __mstore8,        // (p, v) -> ()
    __sload,          // (p)
    __sstore,         // (p, v) -> ()
    __msize,          // ()
    __gas,            // ()
    __address,        // ()
    __balance,        // (a)
    __selfbalance,    // ()
    __caller,         // ()
    __callvalue,      // ()
    __calldataload,   // (p)
    __calldatasize,   // ()
    __calldatacopy,   // (t, f, s) -> ()
    __codesize,       // ()
    __codecopy,       // (t, f, s) -> ()
    __extcodesize,    // (a)
    __extcodecopy,    // (a, t, f, s) -> ()
    __returndatasize, // ()
    __returndatacopy, // (t, f, s) -> ()
    __extcodehash,    // (a)
    __create,         // (v, p, n)
    __create2,        // (v, p, n, s)
    __call,           // (g, a, v, in, insize, out, outsize)
    __callcode,       // (g, a, v, in, insize, out, outsize)
    __delegatecall,   // (g, a, in, insize, out, outsize)
    __staticcall,     // (g, a, in, insize, out, outsize)
    __return,         // (p, s) -> ()
    __revert,         // (p, s) -> ()
    __selfdestruct,   // (a) -> ()
    __invalid,        // () -> ()
    __log0,           // (p, s) -> ()
    __log1,           // (p, s, t1) -> ()
    __log2,           // (p, s, t1, t2) -> ()
    __log3,           // (p, s, t1, t2, t3) -> ()
    __log4,           // (p, s, t1, t2, t3, t4) -> ()
    __chainid,        // ()
    __basefee,        // ()
    __origin,         // ()
    __gasprice,       // ()
    __blockhash,      // (b)
    __coinbase,       // ()
    __timestamp,      // ()
    __number,         // ()
    __prevrandao,     // ()
    __gaslimit,       // ()
}

impl Intrinsic {
    pub fn arg_count(&self) -> usize {
        use Intrinsic::*;
        match self {
            __stop | __basefee | __origin | __gasprice | __coinbase | __timestamp | __number
            | __prevrandao | __gaslimit | __pc | __msize | __gas | __address | __selfbalance
            | __caller | __callvalue | __calldatasize | __codesize | __returndatasize
            | __invalid | __chainid => 0,

            __not | __iszero | __pop | __mload | __balance | __sload | __calldataload
            | __extcodesize | __extcodehash | __selfdestruct | __blockhash => 1,

            __add | __sub | __mul | __div | __sdiv | __mod | __smod | __exp | __lt | __gt
            | __slt | __sgt | __eq | __and | __or | __xor | __byte | __shl | __shr | __sar
            | __signextend | __keccak256 | __mstore | __mstore8 | __sstore | __return
            | __revert | __log0 => 2,

            __addmod | __mulmod | __calldatacopy | __codecopy | __returndatacopy | __create
            | __log1 => 3,
            __extcodecopy | __create2 | __log2 => 4,
            __log3 => 5,
            __delegatecall | __staticcall | __log4 => 6,
            __call | __callcode => 7,
        }
    }

    pub fn return_type(&self) -> Base {
        use Intrinsic::*;
        match self {
            __stop | __pop | __mstore | __mstore8 | __sstore | __calldatacopy | __codecopy
            | __extcodecopy | __returndatacopy | __return | __revert | __selfdestruct
            | __invalid | __log0 | __log1 | __log2 | __log3 | __log4 => Base::Unit,
            _ => Base::u256(),
        }
    }
}
