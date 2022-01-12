use bytes::Bytes;
use fe_common::diagnostics::print_diagnostics;
use fe_common::files::FileStore;
use fe_common::utils::keccak;
use fe_driver as driver;
use fe_yulgen::runtime::functions;
use std::collections::BTreeMap;
use std::str::FromStr;
use yultsur::*;
pub use revm::{self, InMemoryDB, EVM, AccountInfo, TransactTo, TransactOut};
pub use primitive_types_new::{self as primitive_types, H160, U256};


#[allow(dead_code)]
pub const DEFAULT_CALLER: &str = "1000000000000000000000000000000000000001";
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

#[allow(dead_code)]
pub fn address(s: &str) -> H160 {
    H160::from_str(s).unwrap_or_else(|_| panic!("couldn't create address from: {}", s))
}

#[allow(dead_code)]
pub struct ContractHarness {
    pub address: H160,
    pub abi: ethabi::Contract,
    pub caller: H160,
    pub value: U256,
}

#[allow(dead__code)]
impl ContractHarness {
    pub fn new(contract_address: H160, abi: ethabi::Contract) -> Self {
        let caller = address(DEFAULT_CALLER);
        ContractHarness {
            address: contract_address,
            abi,
            caller,
            value: U256::zero(),
        }
    }

   
    pub fn call_function(&self, vm: EVM<InMemoryDB>) {

    }



}


fn _deploy_contract(
    vm: &mut EVM<InMemoryDB>,
    bytecode: &str,
    abi: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    let abi = ethabi::Contract::load(abi.as_bytes()).expect("unable to load the ABI");

    let mut bytecode = hex::decode(bytecode).expect("failed to decode bytecode");

    if let Some(constructor) = &abi.constructor {
        bytecode = constructor.encode_input(bytecode, init_params).unwrap()
    }

    let caller = H160::from_str("0x1000000000000000000000000000000000000000").unwrap();
    let caller_account = AccountInfo::from_balance(U256::from(10000000_u64));

  
    vm.env.tx.caller = caller.clone();
    vm.env.tx.transact_to = TransactTo::create();
    vm.db().unwrap().insert_cache(address(DEFAULT_CALLER), caller_account);

    vm.env.tx.data = Bytes::from(bytecode);
    let (_, out, _, __) = vm.transact();
    let contract_address = match out {
        TransactOut::Create(a, Some(contract)) => contract,
        _ => panic!("Invalid create. This is a bug in the EVM"),
    };
     
    return ContractHarness::new(contract_address, abi);
}

#[cfg(feature = "solc-backend")]
pub fn deploy_contract(
    vm: &mut EVM<InMemoryDB>, 
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
    
    _deploy_contract(
        vm,
        &compiled_contract.bytecode,
        &compiled_contract.json_abi,
        init_params,
    )

}