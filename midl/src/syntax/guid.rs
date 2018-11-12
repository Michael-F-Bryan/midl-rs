use failure_derive::Fail;
use regex::Regex;
use std::fmt::{self, Display, Formatter};
use std::str::FromStr;

/// A Globally unique identifier.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Guid {
    pub data1: u32,
    pub data2: u16,
    pub data3: u16,
    pub data4: [u8; 8],
}

impl FromStr for Guid {
    type Err = ParseGuidError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static::lazy_static! {
            static ref PATTERN: Regex = Regex::new(r"^([[:xdigit:]]{8})-([[:xdigit:]]{4})-([[:xdigit:]]{4})-([[:xdigit:]]{4})-([[:xdigit:]]{12})$").unwrap();
        }

        let got = PATTERN.captures(s).ok_or(ParseGuidError)?;

        let first_u16 = u16::from_str_radix(&got[4], 16).unwrap();
        let data4 = &got[5];
        let data4 = [
            (first_u16 >> 8) as u8,
            (first_u16 & 0xFF) as u8,
            u8::from_str_radix(&data4[0..2], 16).unwrap(),
            u8::from_str_radix(&data4[2..4], 16).unwrap(),
            u8::from_str_radix(&data4[4..6], 16).unwrap(),
            u8::from_str_radix(&data4[6..8], 16).unwrap(),
            u8::from_str_radix(&data4[8..10], 16).unwrap(),
            u8::from_str_radix(&data4[10..12], 16).unwrap(),
        ];

        Ok(Guid {
            data1: u32::from_str_radix(&got[1], 16).unwrap(),
            data2: u16::from_str_radix(&got[2], 16).unwrap(),
            data3: u16::from_str_radix(&got[3], 16).unwrap(),
            data4,
        })
    }
}

/// GUID parsing failed.
#[derive(Debug, Copy, Clone, PartialEq, Fail)]
#[fail(display = "Invalid GUID format")]
pub struct ParseGuidError;

impl Display for Guid {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{:08X}-{:04X}-{:04X}-{:02X}{:02X}-",
            self.data1, self.data2, self.data3, self.data4[0], self.data4[1],
        )?;
        for byte in &self.data4[2..] {
            write!(f, "{:02X}", byte)?;
        }

        Ok(())
    }
}
