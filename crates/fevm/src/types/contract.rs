use std::{str::FromStr, path::{PathBuf, Path}};
use bytes::Bytes;
use fe_common::files::FileStore;
use primitive_types::{H160, U256};
use revm::{TransactOut, Return};
use crate::{Fevm, Caller, CallResult, Address};

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
#[derive(Clone)]
pub enum ContractCode {
    Bytes(Vec<u8>),
    Deployed
}


#[derive(Default)]
pub struct ContractBuilder<'a> {
    vm: Option<&'a Fevm<'a>>,
    abi: Option<ethabi::Contract>,
    address: Option<Address>,
    code: Option<ContractCode>,
}

impl<'a> ContractBuilder<'a> {

    pub fn new(vm: &'a Fevm<'a>) -> Self {
        Self {
            vm: Some(vm),
            ..Default::default()
        }
    }

    pub fn address(mut self, addr: Address) -> Self {
        self.address = Some(addr);
        self
    }
    #[cfg(feature = "solc-backend")]
    pub fn fixture(
        mut self, 
        fixture: &str, 
        contract_name: &str, 
    ) -> Contract<'a> 
    {
        let src = test_files::fixture(fixture);
        let mut files = FileStore::new();
        let id = files.add_file(fixture, src);
        let deps = files.add_included_libraries();
    
        let compiled_module = match fe_driver::compile_module(&files, id, &deps, true, true) {
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

     
        let mut bytecode = hex::decode(&compiled_contract.bytecode)
        .expect("Failed to decode bytecode");
        Contract {
            vm: self.vm.unwrap(),
            abi: ethabi::Contract::load(compiled_contract.json_abi.as_bytes()).expect("Unable to load contract abi from compiled module"),
            address: self.address,
            code: ContractCode::Bytes(bytecode)
        }

    }
}


#[derive(Clone)]
pub struct Contract<'a> {
    vm: &'a Fevm<'a>,
    pub abi: ethabi::Contract,
    pub address: Option<Address>,
    pub code: ContractCode,
    
}





impl Contract<'_> {
    pub fn call(&self, name: &str, input: &[ethabi::Token], caller: &Caller) -> Option<ethabi::Token> {
        if self.address.is_none() {
            panic!("Please deploy contract prior to making calls!");
        }
        let function = &self.abi.functions[name][0];
        let input = self.build_calldata(name, input);

        let (return_code, tx_result, gas, logs) = 
            self.vm.call(input, self.address.as_ref().unwrap(), caller);
        match return_code {
            Return::Return | Return::Stop => {
                if let TransactOut::Call(data) = tx_result {
                     function.decode_output(&data)
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

    pub fn build_calldata(&self, name: &str, input: &[ethabi::Token]) -> Vec<u8> {
        let function = &self.abi.functions[name][0];
        let encoded = function
            .encode_input(input)
            .unwrap_or_else(|_| panic!("Unable to encode input for {}", name));
        encoded
    }

    pub fn deploy(mut self, deployer: &Caller, init_params: &[ethabi::Token]) -> Self {
      self.vm.deploy(&mut self, deployer, init_params);

      self
    }

    pub fn address(&self) -> Address {
        self.address.clone().unwrap()
    }
}


