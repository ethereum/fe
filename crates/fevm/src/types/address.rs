use crate::primitive_types::H160;
use ethabi::token::Token;
pub type Address = H160;
fn random_address() -> Address {
   Address::random()
}



pub struct Caller(pub Address);

impl Caller {
    pub fn random() -> Self {
        Self(random_address())
    }
}

impl AsRef<Address> for Caller {
    fn as_ref(&self) -> &Address {
        &self.0
    }
}


impl From<Address> for Caller {
    fn from(addr: Address) -> Self {
        Self(addr)
    }
}