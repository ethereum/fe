use crate::primitive_types::H160;
use crate::AsToken;
use crate::conversion::address_token;
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

    pub fn address(&self) -> Address {
        self.0
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

impl Into<Token> for Caller {
    fn into(self) -> Token {
        Token::Address(self.address())
    }
}

impl AsToken for Address {
    fn as_token(&self) -> Token {
        address_token(self.clone())
    }
}

impl AsToken for Caller {
    fn as_token(&self) -> Token {
        Token::Address(self.address())
    }
}