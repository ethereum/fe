#[salsa::interned]
pub struct IdentId {
    #[return_ref]
    pub data: String,
}
impl IdentId {
    pub fn is_super(self) -> bool {
        self == kw::SUPER
    }

    pub fn is_ingot(self) -> bool {
        self == kw::INGOT
    }

    pub fn is_self(self) -> bool {
        self == kw::SELF
    }

    pub fn is_self_ty(self) -> bool {
        self == kw::SELF_TY
    }
}

pub mod kw {
    use macros::define_keywords;

    define_keywords! {
        (INGOT, "ingot"),
        (SUPER, "super"),
        (SELF, "self"),
        (SELF_TY, "Self"),
        (BOOL, "bool"),
        (U8, "u8"),
        (U16, "u16"),
        (U32, "u32"),
        (U64, "u64"),
        (U128, "u128"),
        (U256, "u256"),
        (I8, "i8"),
        (I16, "i16"),
        (I32, "i32"),
        (I64, "i64"),
        (I128, "i128"),
        (I256, "i256"),
    }
}
