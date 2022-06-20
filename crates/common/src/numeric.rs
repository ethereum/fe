use num_bigint::{BigInt, Sign};

/// A type that represents the radix of a numeric literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Radix {
    Hexadecimal,
    Decimal,
    Octal,
    Binary,
}

impl Radix {
    /// Returns number representation of the radix.
    pub fn as_num(self) -> u32 {
        match self {
            Self::Hexadecimal => 16,
            Self::Decimal => 10,
            Self::Octal => 8,
            Self::Binary => 2,
        }
    }
}

/// A helper type to interpret a numeric literal represented by string.
#[derive(Debug, Clone)]
pub struct Literal<'a> {
    /// The number part of the string.
    num: &'a str,
    /// The radix of the literal.
    radix: Radix,
}

impl<'a> Literal<'a> {
    pub fn new(src: &'a str) -> Self {
        debug_assert!(!src.is_empty());
        debug_assert_ne!(src.chars().next(), Some('-'));
        let (radix, prefix) = if src.len() < 2 {
            (Radix::Decimal, None)
        } else {
            match &src[0..2] {
                "0x" | "0X" => (Radix::Hexadecimal, Some(&src[..2])),
                "0o" | "0O" => (Radix::Octal, Some(&src[..2])),
                "0b" | "0B" => (Radix::Binary, Some(&src[..2])),
                _ => (Radix::Decimal, None),
            }
        };

        Self {
            num: &src[prefix.map_or(0, str::len)..],
            radix,
        }
    }

    /// Parse the numeric literal to `T`.
    pub fn parse<T: num_traits::Num>(&self) -> Result<T, T::FromStrRadixErr> {
        T::from_str_radix(self.num, self.radix.as_num())
    }

    /// Returns radix of the numeric literal.
    pub fn radix(&self) -> Radix {
        self.radix
    }
}

// Converts any positive or negative `BigInt` into a hex str using 2s complement representation for negative values.
pub fn to_hex_str(val: &BigInt) -> String {
    format!(
        "0x{}",
        BigInt::from_bytes_be(Sign::Plus, &val.to_signed_bytes_be()).to_str_radix(16)
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_radix() {
        assert_eq!(Literal::new("0XFF").radix(), Radix::Hexadecimal);
        assert_eq!(Literal::new("0xFF").radix(), Radix::Hexadecimal);
        assert_eq!(Literal::new("0O77").radix(), Radix::Octal);
        assert_eq!(Literal::new("0o77").radix(), Radix::Octal);
        assert_eq!(Literal::new("0B77").radix(), Radix::Binary);
        assert_eq!(Literal::new("0b77").radix(), Radix::Binary);
        assert_eq!(Literal::new("1").radix(), Radix::Decimal);

        // Invalid radix is treated as `Decimal`.
        assert_eq!(Literal::new("0D15").radix(), Radix::Decimal);
    }

    #[test]
    fn test_to_hex_str() {
        assert_eq!(to_hex_str(&BigInt::from(-1i8)), "0xff");
        assert_eq!(to_hex_str(&BigInt::from(-2i8)), "0xfe");
        assert_eq!(to_hex_str(&BigInt::from(1i8)), "0x1");
        assert_eq!(to_hex_str(&BigInt::from(2i8)), "0x2");
    }
}
