use std::{str::FromStr, path::{PathBuf, Path}};
use bytes::Bytes;
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

pub enum ContractCode {
    Bytes(Vec<u8>),
    Deployed
}
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

    pub fn build_calldata(&self, name: &str, input: &[ethabi::Token]) -> Vec<u8> {
        let function = &self.abi.functions[name][0];
        let encoded = function
            .encode_input(input)
            .unwrap_or_else(|_| panic!("Unable to encode input for {}", name));
        encoded
    }

    pub fn deploy(&self, deployer: &Caller) -> Address {
        self.vm.deploy(&self, deployer)
    }
}


// Load contract fixture, compile
impl From<PathBuf> for Contract<'_>
{
    fn from(other: PathBuf) -> Self {
        todo!()
    }
}
