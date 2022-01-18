use crate::primitive_types::H160;

pub type Address = H160;
fn random_address() -> Address {
   Address::random()
}



pub struct Caller(pub Address);


impl AsRef<Address> for Caller {
    fn as_ref(&self) -> &Address {
        &self.0
    }
}