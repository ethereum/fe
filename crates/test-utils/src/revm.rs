use bytes::Bytes;
use fe_common::diagnostics::print_diagnostics;
use fe_common::files::FileStore;
use fe_common::utils::keccak;
use fe_driver as driver;
use fe_yulgen::runtime::functions;
use std::borrow::{Borrow, BorrowMut};
use std::collections::HashMap;
use std::{collections::BTreeMap, thread::AccessError};
use std::str::FromStr;
use yultsur::*;
pub use revm::{self, Return, InMemoryDB, EVM, AccountInfo, TransactTo, TransactOut,SpecId, NoOpInspector};
pub use primitive_types_new::{self as primitive_types, H160, U256};
pub use ethabi_new as ethabi;
use getrandom::getrandom;

#[allow(dead_code)]
pub fn uint_token(n: u64) -> ethabi::Token {
    ethabi::Token::Uint(U256::from(n))
}
pub fn address_token(addr: primitive_types::H160) -> ethabi::Token {
    ethabi::Token::Address(addr)
}

#[allow(dead_code)]
pub fn address_token_from_str(s: &str) -> ethabi::Token {
    // left pads to 40 characters
    ethabi::Token::Address(address(&format!("{:0>40}", s)))
}

#[allow(dead_code)]
pub fn string_token(s: &str) -> ethabi::Token {
    ethabi::Token::String(s.to_string())
}

#[allow(dead_code)]
pub fn bool_token(val: bool) -> ethabi::Token {
    ethabi::Token::Bool(val)
}

#[allow(dead_code)]
pub fn uint_token_from_dec_str(val: &str) -> ethabi::Token {
    ethabi::Token::Uint(U256::from_dec_str(val).expect("Not a valid dec string"))
}
#[allow(dead_code)]
pub const DEFAULT_CALLER: &str = "0x1000000000000000000000000000000000000000";
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

fn random_address() -> H160 {
    let mut buf = [0u8; 20];
    match getrandom(&mut buf) {
        Ok(_) =>  H160::from(buf),
        Err(e) => panic!("Unable to generate random address: {:?}",e)
    }
   
}



#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum ContractId {
    Name(String),
    Address(H160),
}

impl From<String> for ContractId {
    fn from(other: String) -> Self {
        ContractId::Name(other)
    }
}

impl From<H160> for ContractId {
    fn from(other: H160) -> Self {
        ContractId::Address(other)
    }
}

impl From<&str> for ContractId {
    fn from(other: &str) -> Self {
        let as_address = H160::from_str(other);
        match as_address {
            Ok(addr) => ContractId::Address(addr),
            Err(_) => ContractId::Name(other.to_owned())
        }
    }
}

pub struct Fevm {
    pub vm: EVM<InMemoryDB>,
    // To do: make contractId -> contractharness surjective
    pub contracts: HashMap<ContractId, ContractHarness>,
    pub callers: Vec<H160>,
}

impl Default for Fevm {
    fn default() -> Self {
        let mut vm = revm::new();
        //vm.env.cfg.spec_id = SpecId::from("Istanbul");
        vm.database(InMemoryDB::default());
        Self { 
            vm, 
            contracts: Default::default(), 
            callers: vec![address(DEFAULT_CALLER)] 
        }
    }
}


impl From<EVM<InMemoryDB>> for Fevm {
    fn from(other: EVM<InMemoryDB>) -> Self {
        Fevm {
            vm: other,
            ..Default::default()
        }
    }
}

impl AsMut<EVM<InMemoryDB>> for Fevm {
    fn as_mut(&mut self) -> &mut EVM<InMemoryDB> {
        &mut self.vm
    }
}

impl Fevm {
    pub fn new() -> Self {
        Fevm::default()
    }

    pub fn new_with_callers(callers: Vec<H160>) -> Self {
        Fevm {
            callers,
            ..Default::default()
        }
    }

    pub fn create_account_with_balance(&mut self, balance: impl Into<U256>) -> H160 {
        let address = random_address();
        let account = AccountInfo::from_balance(balance.into());
        self.vm.db().unwrap().insert_cache(address.clone(), account);
        address
    }

    pub fn fund(&mut self, address: &H160, amt: impl Into<U256>) {
        let mut account = self.vm.db().unwrap().cache().get(address)
        .expect(format!("Cannot find address {:?}", address).as_str())
        .clone();
        account.balance += amt.into();
        self.vm.db().unwrap().insert_cache(address.clone(), account)
    }

    pub fn balance_of(&self, address: &H160) -> U256 {
       if let Some(acc) = self.vm.db.as_ref().unwrap().cache().get(address) {
        acc.balance
       } else {
           U256::zero()
       }
           
    }

    pub fn get_account_by_address(&mut self, address: &H160) -> Option<(H160, AccountInfo)> {
        self.vm.db().unwrap().cache().get_key_value(address).clone()
        .map(|(addr, acc)| (addr.clone(), acc.clone()))
    }

    pub fn load_contract_from_file(&self, address: H160, fixture: &str, contract_name: &str) -> ContractHarness {
            let mut files = FileStore::new();
        let deps = files.add_included_libraries();
        let src = test_files::fixture(fixture);
        let id = files.add_file(fixture, src);
        let compiled_module = match driver::compile_module(&files, id, &deps, true, true) {
            Ok(module) => module,
            Err(err) => {
                print_diagnostics(&err.0, &files);
                panic!("failed to compile fixture: {}", fixture);
            }
        };
        let compiled_contract = compiled_module
            .contracts
            .get(contract_name)
            .expect("could not find contract in fixture");
        let abi = ethabi::Contract::load(compiled_contract.json_abi.as_bytes())
            .expect("unable to load the ABI");

        ContractHarness::new(address, abi, H160::from_str(DEFAULT_CALLER).unwrap())
    }
    pub fn call_contract<'a>(
        &mut self,
        contract_id: impl Into<ContractId>,
        fn_name: &str,
        input: &[ethabi::Token]
    ) -> Option<ethabi::Token> 
    {
        
        let id = contract_id.into();
        let contract = self.contracts
            .get(&id).expect(format!("No contract {:?} in contracts collection", &id).as_str()).clone();
        contract.call_function(self, fn_name, input)
    }


    #[cfg(feature = "solc-backend")]
    pub fn deploy_contract_from_fixture(
        &mut self,
        fixture: &str, 
        contract_name: &str, 
        init_params: &[ethabi::Token]
    ) -> ContractHarness {
        let src = test_files::fixture(fixture);
        let mut files = FileStore::new();
        let id = files.add_file(fixture, src);
        let deps = files.add_included_libraries();
    
        let compiled_module = match driver::compile_module(&files, id, &deps, true, true) {
            Ok(module) => module,
            Err(error) => {
                fe_common::diagnostics::print_diagnostics(&error.0, &files);
                panic!("failed to compile module: {}", fixture)
            }
        };
        let compiled_contract = compiled_module
            .contracts
            .get(contract_name)
            .expect("could not find contract in fixture");
        
        let harness = self.deploy_contract(
            &compiled_contract.bytecode,
            &compiled_contract.json_abi,
            init_params,
        );
        self.contracts.insert(contract_name.into(), harness.clone());
        harness
    
    }

    

    pub fn deploy_contract(
        &mut self,
        bytecode: &str,
        abi: &str,
        init_params: &[ethabi::Token],
    ) -> ContractHarness {
        let abi = ethabi::Contract::load(abi.as_bytes()).expect("unable to load the ABI");
    
        let mut bytecode = hex::decode(bytecode).expect("failed to decode bytecode");
    
        if let Some(constructor) = &abi.constructor {
            bytecode = constructor.encode_input(bytecode, init_params).unwrap()
        }
    
        let caller = random_address();
        let caller_account = AccountInfo::from_balance(U256::from(10000000_u64));
    
      
        self.vm.env.tx.caller = caller.clone();
        self.vm.env.tx.transact_to = TransactTo::create();
        self.vm.db().unwrap().insert_cache(caller.clone(), caller_account);
    
        self.vm.env.tx.data = Bytes::from(bytecode);
        let (_, out, _, _) = self.vm.transact_commit();
        let contract_address = match out {
            TransactOut::Create(a, Some(contract)) => contract,
            _ => panic!("Invalid create. This is a bug in the EVM"),
        };
        self.callers.push(caller.clone());
         
        let harness = ContractHarness::new(contract_address.clone(), abi, caller);
        self.contracts.insert(contract_address.into(), harness.clone());
        harness
    }
}

#[allow(dead_code)]
pub fn address(s: &str) -> primitive_types::H160 {
    H160::from_str(s).unwrap_or_else(|_| panic!("couldn't create address from: {}", s))
}

#[derive(Clone)]
#[allow(dead_code)]
pub struct ContractHarness {
    pub address: H160,
    pub abi: ethabi::Contract,
    pub caller: H160,
    pub value: U256,
}

pub type TransactionResult = (Return, TransactOut, u64);

#[allow(dead_code)]
impl ContractHarness {
    pub fn new(contract_address: H160, abi: ethabi::Contract, caller: H160) -> Self {
        ContractHarness {
            address: contract_address,
            abi,
            caller,
            value: U256::zero(),
        }
    }

   
    pub fn build_calldata(&self, name: &str, input: &[ethabi::Token]) -> Vec<u8> {
        let function = &self.abi.functions[name][0];
        let encoded = function
            .encode_input(input)
            .unwrap_or_else(|_| panic!("Unable to encode input for {}", name));
        encoded
    }

    pub fn capture_call(
        &self,
        vm: impl AsMut<EVM<InMemoryDB>>,
        name: &str,
        input: &[ethabi::Token],
    ) -> TransactionResult {
        
        let input = self.build_calldata(name, input);
        self.capture_call_raw_bytes(vm, input)

    }

    pub fn capture_call_raw_bytes(
        &self,
        mut vm: impl AsMut<EVM<InMemoryDB>>,
        input: Vec<u8>,
    ) -> TransactionResult {
        let vm = vm.as_mut();
        vm.env.tx.data = input.into();
        let (return_code, tx_result, gas,  logs) = vm.inspect_commit(NoOpInspector{});
        println!("LOGS in contract call {:?}", logs);
        println!("Transaction result: {:?}", tx_result);
        if let TransactOut::Call(data) = &tx_result {
            let encoded = hex::encode(data);
            println!("Transaction result: {:?}", encoded);
        }
        (return_code, tx_result, gas)
    }
    pub fn call_function(
        &self, 
        mut vm: impl AsMut<EVM<InMemoryDB>>,
        name: &str,
        input: &[ethabi::Token],
    ) -> Option<ethabi::Token> {
        let evm = vm.as_mut();
        let function = &self.abi.functions[name][0];
        evm.env.tx.caller = self.caller.clone();
        println!("CALLER CALLING {} function, with args: {:?} with msg.sender == {:?}", name,input, address_token(self.caller.clone()));
        evm.env.tx.transact_to = TransactTo::Call(self.address.clone());
        let (return_code, tx_result, gas) = self.capture_call(vm, name, input);
        match return_code {
            Return::Return | Return::Stop => {
                if let TransactOut::Call(data) = tx_result {
                     function.decode_output(&data.to_vec())
                    .unwrap_or_else(|e| panic!("unable to decode output of {}: {:?}\nError: {:?}", name, &data, e))
                    .pop()
                } else {
                    panic!("Unexpected result of function call!");
                }
            },
            Return::Revert => {
                if let TransactOut::Call(data) = &tx_result {
                    function.decode_output(&data.to_vec())
                    .unwrap_or_else(|e| panic!("Tx Revert! Unable to decode output of {}: {:?}\nError: {:?}", name, &data, e))
                    .pop();
                    panic!("Tx Revert! Tx Data: {:?}", tx_result)
                }
                panic!("Tx Revert! Tx Data: {:?}", tx_result)
            }
            _ => panic!("Unexpected return code! {:?}", return_code)

        }

    }

    pub fn test_function(
        &self,
        vm: impl AsMut<EVM<InMemoryDB>>,
        name: &str,
        input: &[ethabi::Token],
        output: Option<&ethabi::Token>,
    ) {
        let actual_output = self.call_function(vm, name, input);
        assert_eq!(
            output.map(|token| token.to_owned()),
            actual_output,
            "unexpected output from `fn {}`",
            name
        )
    }


}

// fn _deploy_contract(
//     mut vm: impl AsMut<EVM<InMemoryDB>>,
//     bytecode: &str,
//     abi: &str,
//     init_params: &[ethabi::Token],
// ) -> ContractHarness {
//     let vm = vm.as_mut();
//     let abi = ethabi::Contract::load(abi.as_bytes()).expect("unable to load the ABI");

//     let mut bytecode = hex::decode(bytecode).expect("failed to decode bytecode");

//     if let Some(constructor) = &abi.constructor {
//         bytecode = constructor.encode_input(bytecode, init_params).unwrap()
//     }

//     let caller = H160::from_str(DEFAULT_CALLER).unwrap();
//     let caller_account = AccountInfo::from_balance(U256::from(10000000_u64));

  
//     vm.env.tx.caller = caller.clone();
//     vm.env.tx.transact_to = TransactTo::create();
//     vm.db().unwrap().insert_cache(address(DEFAULT_CALLER), caller_account);

//     vm.env.tx.data = Bytes::from(bytecode);
//     let (_, out, _, _) = vm.transact_commit();
//     let contract_address = match out {
//         TransactOut::Create(a, Some(contract)) => contract,
//         _ => panic!("Invalid create. This is a bug in the EVM"),
//     };
     
//     return ContractHarness::new(contract_address, abi);
// }

// #[cfg(feature = "solc-backend")]
// pub fn deploy_contract(
//     vm: impl AsMut<EVM<InMemoryDB>>, 
//     fixture: &str, 
//     contract_name: &str, 
//     init_params: &[ethabi::Token]
// ) -> ContractHarness {
//     let src = test_files::fixture(fixture);
//     let mut files = FileStore::new();
//     let id = files.add_file(fixture, src);
//     let deps = files.add_included_libraries();

//     let compiled_module = match driver::compile_module(&files, id, &deps, true, true) {
//         Ok(module) => module,
//         Err(error) => {
//             fe_common::diagnostics::print_diagnostics(&error.0, &files);
//             panic!("failed to compile module: {}", fixture)
//         }
//     };
//     let compiled_contract = compiled_module
//         .contracts
//         .get(contract_name)
//         .expect("could not find contract in fixture");
    
//     _deploy_contract(
//         vm,
//         &compiled_contract.bytecode,
//         &compiled_contract.json_abi,
//         init_params,
//     )

//}