#![no_main]
extern crate fe_fuzzer;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    fe_fuzzer::erc20::fuzz_fe_erc20_constructor(data);
    fe_fuzzer::erc20::fuzz_fe_erc20_transfer(data);
    fe_fuzzer::erc20::fuzz_sol_erc20_constructor(data);
    fe_fuzzer::erc20::fuzz_sol_erc20_transfer(data)
});
