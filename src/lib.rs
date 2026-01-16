use std::fmt::Display;

pub mod lexer;
pub mod parser;

#[derive(Debug, Clone, PartialEq)]
pub enum Access {
    RW,
    RO,
    WO,
}

impl Display for Access {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Access::RW => "RW",
            Access::RO => "RO",
            Access::WO => "WO",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ByteOrder {
    BE,
    LE,
}

impl Display for ByteOrder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ByteOrder::BE => "BE",
            ByteOrder::LE => "LE",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BitOrder {
    Lsb0,
    Msb0,
}

impl Display for BitOrder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BitOrder::Lsb0 => "lsb0",
            BitOrder::Msb0 => "msb0",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaseType {
    Uint,
    U8,
    U16,
    U32,
    U64,
    Int,
    I8,
    I16,
    I32,
    I64,
    Bool,
}

impl Display for BaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BaseType::Uint => "uint",
            BaseType::U8 => "u8",
            BaseType::U16 => "u16",
            BaseType::U32 => "u32",
            BaseType::U64 => "u64",
            BaseType::Int => "int",
            BaseType::I8 => "i8",
            BaseType::I16 => "i16",
            BaseType::I32 => "i32",
            BaseType::I64 => "i64",
            BaseType::Bool => "bool",
        };

        write!(f, "{s}")
    }
}
