pub mod types;
use revm::Log;
pub use types::*;

use bytes::Bytes;
use fe_common::diagnostics::print_diagnostics;
use fe_common::files::FileStore;
use fe_common::utils::keccak;
use fe_driver as driver;
use fe_yulgen::runtime::functions;
use std::borrow::{Borrow, BorrowMut};
use std::collections::HashMap;
use std::str::FromStr;
use yultsur::*;
pub use revm::{
    self, 
    Return, 
    InMemoryDB, 
    EVM, 
    AccountInfo, 
    TransactTo, 
    TransactOut,
    SpecId, 
    NoOpInspector, 
    Host
};
pub use primitive_types::{self, H160, U256};
use std::cell::RefCell;

pub type CallResult = (Return, TransactOut, u64, Vec<Log>);

pub trait ToBeBytes {
    fn to_be_bytes(&self) -> [u8; 32];
}

impl ToBeBytes for U256 {
    fn to_be_bytes(&self) -> [u8; 32] {
        let mut input_bytes: [u8; 32] = [0; 32];
        self.to_big_endian(&mut input_bytes);
        input_bytes
    }
}

pub struct Fevm<'a>{
    inner: RefCell<EVM<InMemoryDB>>,
    contracts: HashMap<&'a ContractId, Contract<'a>> 

}

impl Fevm<'_> {
    pub fn call(&self, input: Vec<u8>, addr: &Address, caller: &Caller) -> CallResult {
        let mut vm = self.inner.borrow_mut();
        vm.env.tx.caller = caller.0;
        vm.env.tx.transact_to = TransactTo::Call(addr.clone());
        vm.env.tx.data = input.into();
        vm.inspect_commit(NoOpInspector{})
    }

    pub fn deploy(&self, contract: &Contract, deployer: &Caller) -> Address {
        todo!()
    }

    pub fn create_account(&self, address: &Address, balance: impl Into<U256>) -> Address {
        todo!()
    }

    pub fn fund_account(&self, address: &Address, amt: impl Into<U256>) {

    }

    pub fn balance_of(&self, address: &Address) -> U256 {
        todo!()
    }

    pub fn get_account(&self, address: &Address) -> Option<&AccountInfo> {
        todo!()
    }

    pub fn erase(&self, address: &Address) -> Address {
        todo!()
    } 


}