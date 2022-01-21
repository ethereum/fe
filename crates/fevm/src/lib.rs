pub mod types;
use revm::Log;
pub use types::*;


use bytes::Bytes;
use fe_common::diagnostics::print_diagnostics;
use fe_common::files::FileStore;
use fe_common::utils::keccak;
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
pub mod conversion;
pub use conversion::*;

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

    pub fn new() -> Self {
        let mut vm = revm::new();
        vm.database(InMemoryDB::default());
        Self {
            inner: RefCell::new(vm),
            contracts: HashMap::new()
        }
    }
    pub fn call(&self, input: Vec<u8>, addr: &Address, caller: &Caller) -> CallResult {
        let mut vm = self.inner.borrow_mut();
        vm.env.tx.caller = caller.0;
        vm.env.tx.transact_to = TransactTo::Call(addr.clone());
        vm.env.tx.data = input.into();
        vm.inspect_commit(NoOpInspector{})
    }

    pub fn deploy(&self, contract: &mut Contract, deployer: &Caller, init_params: &[ethabi::Token]) {
        if let ContractCode::Bytes(bin) = &contract.code {
            let mut bytecode = bin.clone();

            if let Some(constructor) = &contract.abi.constructor {
                bytecode = constructor.encode_input(bytecode, init_params).unwrap()
            }
        
            let mut vm = self.inner.borrow_mut();
            match vm.db().expect("DB not found").cache().get_key_value(deployer.as_ref()) {
                Some(_) => {
                    vm.env.tx.caller = deployer.as_ref().clone();
                    vm.env.tx.transact_to = TransactTo::create();
                    vm.env.tx.data = Bytes::from(bytecode);
                    let (_, out, _, _) = vm.transact_commit();
                    let contract_address = match out {
                        TransactOut::Create(_, Some(contract)) => contract,
                        _ => panic!("Invalid create. This is a bug in the EVM"),
                    };
                    contract.code = ContractCode::Deployed;
                    contract.address = Some(contract_address);
                    
                },
                None => panic!("Invalid caller. Please deploy with an existing address."),
            }
        } else {
            panic!("Contract does not have bytecode. If you wish to redeploy, please re-instantiate with bytecode");
        }
    }

    pub fn create_account(&self, address: impl AsRef<Address>, balance: impl Into<U256>) {
        let acc = AccountInfo::from_balance(balance.into());
        self.inner.borrow_mut().db().unwrap().insert_cache(address.as_ref().clone(), acc);
    }

    pub fn fund_account(&self, address: &Address, amt: impl Into<U256>) {
        let mut vm = self.inner.borrow_mut();
        
       let mut acc =  vm.db().unwrap().cache().get(address)
        .expect(format!("Cannot find account for address {:?}. Please create account first", address).as_str())
        .clone(); 
        acc.balance += amt.into();
        vm.db().unwrap().insert_cache(address.clone(), acc);
    }

    pub fn balance_of(&self, address: &Address) -> U256 {
        match self.inner.borrow_mut().db().unwrap().cache().get(address) {
            Some(acc) => acc.balance,
            None => 0.into()
        }
    }

    pub fn get_account(&self, address: &Address) -> Option<AccountInfo> {
        self.inner.borrow_mut().db().unwrap().cache().get(address).cloned()
    }

    pub fn erase(&self, address: &Address) -> Address {
        todo!()
    } 


}