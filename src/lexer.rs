#![expect(clippy::from_str_radix_10, reason = "Other radixes are used")]

use std::{borrow::Cow, fmt::Display};

use logos::Logos;

use crate::{Access, BaseType, BitOrder, ByteOrder};

// fn parse_num(slice: &str) -> Result<i128, Error> {
//     let (multiplier, positive) = slice
//         .strip_prefix('-')
//         .map(|rest| (-1, rest))
//         .unwrap_or((1, slice));

//     let pos_num = if let Some(num_slice) = positive.strip_prefix("0b") {
//         i128::from_str_radix(&num_slice.replace('_', ""), 2)
//     } else if let Some(num_slice) = positive.strip_prefix("0o") {
//         i128::from_str_radix(&num_slice.replace('_', ""), 8)
//     } else if let Some(num_slice) = positive.strip_prefix("0x") {
//         i128::from_str_radix(&num_slice.replace('_', ""), 16)
//     } else {
//         i128::from_str_radix(&positive.replace('_', ""), 10)
//     };

//     pos_num
//         .map(|n| n * multiplier)
//         .map_err(Error::ParseIntError)
// }

#[derive(Debug, Clone, PartialEq, Logos)]
#[logos(skip r"[ \t\r\n]+")]
#[logos(skip r"//[^\n]*")] // Skip comments
pub enum Token<'src> {
    #[regex(r"///[^\n]*", allow_greedy = true, callback = |lex| lex.slice().trim_start_matches("///"))]
    DocCommentLine(&'src str),
    #[regex(r"[_\p{XID_Start}][\p{XID_Continue}-]*")]
    #[regex(r"r#[_\p{XID_Start}][\p{XID_Continue}-]*", callback = |lex| lex.slice().trim_start_matches("r#"))]
    Ident(&'src str),
    #[token("{")]
    CurlyOpen,
    #[token("}")]
    CurlyClose,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[token("<")]
    AngleOpen,
    #[token(">")]
    AngleClose,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("->")]
    Arrow,
    #[token("by")]
    By,
    #[token("try")]
    Try,
    #[token("as")]
    As,
    #[token("allow")]
    Allow,
    #[token("default")]
    Default,
    #[token("catch-all")]
    CatchAll,
    #[regex(r"-?0b_*[01][_01]*")] // Binary
    #[regex(r"-?0o_*[0-7][_0-7]*")] // Octal
    #[regex(r"-?[0-9][_0-9]*")] // Decimal
    #[regex(r"-?0x_*[0-9a-fA-F][_0-9a-fA-F]*")] // Hex
    Num(&'src str),
    #[token("RW", |_| Access::RW)]
    #[token("RO", |_| Access::RO)]
    #[token("WO", |_| Access::WO)]
    Access(Access),
    #[token("BE", |_| ByteOrder::BE)]
    #[token("LE", |_| ByteOrder::LE)]
    ByteOrder(ByteOrder),
    #[token("lsb0", |_| BitOrder::Lsb0)]
    #[token("msb0", |_| BitOrder::Msb0)]
    BitOrder(BitOrder),
    #[token("uint", |_| BaseType::Uint)]
    #[token("u8", |_| BaseType::U8)]
    #[token("u16", |_| BaseType::U16)]
    #[token("u32", |_| BaseType::U32)]
    #[token("u64", |_| BaseType::U64)]
    #[token("int", |_| BaseType::Int)]
    #[token("i8", |_| BaseType::I8)]
    #[token("i16", |_| BaseType::I16)]
    #[token("i32", |_| BaseType::I32)]
    #[token("i64", |_| BaseType::I64)]
    #[token("bool", |_| BaseType::Bool)]
    BaseType(BaseType),
    Error,
}

impl<'src> Display for Token<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::DocCommentLine(_) => write!(f, "doc comment"),
            Token::Ident(_) => write!(f, "identifier"),
            Token::CurlyOpen => write!(f, "{{"),
            Token::CurlyClose => write!(f, "}}"),
            Token::BracketOpen => write!(f, "["),
            Token::BracketClose => write!(f, "]"),
            Token::AngleOpen => write!(f, "<"),
            Token::AngleClose => write!(f, ">"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Arrow => write!(f, "->"),
            Token::By => write!(f, "by"),
            Token::Try => write!(f, "try"),
            Token::As => write!(f, "as"),
            Token::Allow => write!(f, "allow"),
            Token::Default => write!(f, "default"),
            Token::CatchAll => write!(f, "catch-all"),
            Token::Num(_) => write!(f, "number"),
            Token::Access(_) => write!(f, "access-specifier"),
            Token::ByteOrder(_) => write!(f, "byte-order"),
            Token::BitOrder(_) => write!(f, "bit-order"),
            Token::BaseType(_) => write!(f, "base type"),
            Token::Error => write!(f, "ERROR"),
        }
    }
}

impl<'src> Token<'src> {
    fn get_human_string(&self) -> Cow<'static, str> {
        match self {
            Token::DocCommentLine(line) => format!("///{line}").into(),
            Token::Ident(ident) => format!("#{ident}").into(),
            Token::CurlyOpen => "{".into(),
            Token::CurlyClose => "}".into(),
            Token::BracketOpen => "[".into(),
            Token::BracketClose => "]".into(),
            Token::AngleOpen => "<".into(),
            Token::AngleClose => ">".into(),
            Token::Colon => ":".into(),
            Token::Comma => ",".into(),
            Token::Arrow => "->".into(),
            Token::Try => "try".into(),
            Token::By => "by".into(),
            Token::As => "as".into(),
            Token::Num(n) => n.to_string().into(),
            Token::Error => "ERROR".into(),
            Token::Access(val) => val.to_string().into(),
            Token::ByteOrder(val) => val.to_string().into(),
            Token::BitOrder(val) => val.to_string().into(),
            Token::BaseType(val) => val.to_string().into(),
            Token::Allow => "allow".into(),
            Token::Default => "default".into(),
            Token::CatchAll => "catch-all".into(),
        }
    }

    /// New line before, new line after, indent change
    fn get_print_format(&self) -> (bool, bool, i32) {
        match self {
            Token::DocCommentLine(_) => (true, true, 0),
            Token::Comma => (false, true, 0),
            Token::CurlyOpen | Token::BracketOpen => (false, true, 1),
            Token::CurlyClose | Token::BracketClose => (true, false, -1),
            _ => (false, false, 0),
        }
    }

    pub fn formatted_print<'a, I: Iterator<Item = &'a Token<'src>>>(
        stream: &mut impl std::io::Write,
        tokens: I,
    ) -> std::io::Result<()>
    where
        'src: 'a,
    {
        let mut indent = 0i32;
        for token in tokens {
            let (newline_before, newline_after, indent_change) = token.get_print_format();

            indent += indent_change;
            if newline_before {
                write!(stream, "\n{:width$}", "", width = indent as usize * 4)?;
            }

            write!(stream, "{} ", token.get_human_string())?;

            if newline_after {
                write!(stream, "\n{:width$}", "", width = indent as usize * 4)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::io::stdout;

    use super::*;

    const CODE: &str =
        "// Everything separately specified. Inline externs, enums and fieldsets are possible too
/// Doc comment for Foo


// Everything separately specified. Inline externs, enums and fieldsets are possible too
device Foo {
    register-address-type: u8,

    register Bar {
        address: 0,
        repeat: <10 by 2>,
        repeat: <MyEnum by 2>, // Or repeat with enum
        reset: 0x1234,
        reset: [0x12, 0x34], // Or reset with array
        address-overlap: allow,
        access: RW,

        fields: MyFs
    },

    enum Purr -> u8 {
        A,
        B: 3:1,
        C,
        D: default 5,
        r#BE: catch-all 012345678,
    },

    extern Rah -> u64,

    fieldset MyFs {
        size-bits: 16,
        bit-overlap: allow,

        field xena 7:4 RW <3 by 3> -> uint as try Purr,
        field quux 3:0 RO -> uint,
    }
}";

    #[test]
    fn test_lexer() {
        println!("Token size: {}", std::mem::size_of::<Token>());

        let code = CODE.to_string() + CODE + CODE + CODE + CODE + CODE + CODE + CODE;

        let start = std::time::Instant::now();
        let output = super::Token::lexer(&code).collect::<Vec<_>>();
        let elapsed = start.elapsed();
        println!("{elapsed:?} for {} bytes", code.len());
        println!(
            "{} MB/s",
            code.len() as f64 / elapsed.as_secs_f64() / 1024.0 / 1024.0
        );

        let mut stdout = stdout().lock();
        Token::formatted_print(
            &mut stdout,
            output
                .iter()
                .map(|maybe_token| maybe_token.as_ref().unwrap_or(&Token::Error)),
        )
        .unwrap();
    }
}
