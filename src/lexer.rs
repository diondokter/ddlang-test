use std::fmt::Display;

use chumsky::{prelude::*, text::Char};

use crate::{Access, BaseType, BitOrder, ByteOrder};

// A few type definitions to be used by our parsers below
pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

pub fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char>>> {
    let doc_comment = just("///")
        .ignore_then(
            any()
                .and_is(text::newline().or(end()).not())
                .repeated()
                .to_slice()
                .map(Token::DocCommentLine),
        )
        .labelled("///")
        .boxed();

    let ident_string = choice((
        just("r#").ignore_then(
            any()
                .filter(|c: &char| c.is_ident_start() || *c == '_')
                .then(
                    any()
                        .filter(|c: &char| c.is_ident_continue() || *c == '_' || *c == '-')
                        .repeated(),
                ),
        ),
        any()
            .filter(|c: &char| c.is_ident_start() || *c == '_')
            .then(
                any()
                    .filter(|c: &char| c.is_ident_continue() || *c == '_' || *c == '-')
                    .repeated(),
            ),
    ))
    .to_slice()
    .boxed();

    // A parser for identifiers and keywords
    let ident = ident_string
        .map(|ident: &str| match ident {
            ident if ident.starts_with("r#") => Token::Ident(ident.strip_prefix("r#").unwrap()),
            ident => Token::Ident(ident),
        })
        .labelled("identifier");

    let ctrl = one_of("[]{}<>,:=").map(|c| {
        Token::Ctrl(match c {
            '[' => Control::BracketOpen,
            ']' => Control::BracketClose,
            '{' => Control::CurlyOpen,
            '}' => Control::CurlyClose,
            '<' => Control::AngleOpen,
            '>' => Control::AngleClose,
            ',' => Control::Comma,
            ':' => Control::Colon,
            '=' => Control::Equals,
            _ => unreachable!(),
        })
    });
    let arrow = just("->").to(Token::Ctrl(Control::Arrow)).labelled("->");
    let r#try = just("try").to(Token::Try).labelled("try");
    let by = just("by").to(Token::By).labelled("by");
    let r#as = just("as").to(Token::As).labelled("as");
    let allow = just("allow").to(Token::Allow).labelled("allow");
    let default = just("default").to(Token::Default).labelled("default");
    let catch_all = just("catch-all").to(Token::CatchAll).labelled("catch-all");

    let access = choice((
        just("RW").to(Token::Access(Access::RW)).labelled("RW"),
        just("RO").to(Token::Access(Access::RO)).labelled("RO"),
        just("WO").to(Token::Access(Access::WO)).labelled("WO"),
    ));

    let byte_order = choice((
        just("BE")
            .to(Token::ByteOrder(ByteOrder::BE))
            .labelled("BE"),
        just("LE")
            .to(Token::ByteOrder(ByteOrder::LE))
            .labelled("LE"),
    ));

    let bit_order = choice((
        just("lsb0")
            .to(Token::BitOrder(BitOrder::Lsb0))
            .labelled("lsb0"),
        just("msb0")
            .to(Token::BitOrder(BitOrder::Msb0))
            .labelled("msb0"),
    ));

    // TODO: Add underscore support in the middle of numbers
    let positive_number = choice((
        just("0x").ignore_then(text::int(16).map(|s: &str| i128::from_str_radix(s, 16).unwrap())),
        just("0b").ignore_then(text::int(2).map(|s: &str| i128::from_str_radix(s, 2).unwrap())),
        just("0o").ignore_then(text::int(8).map(|s: &str| i128::from_str_radix(s, 8).unwrap())),
        text::int(10).map(|s: &str| i128::from_str_radix(s, 10).unwrap()),
    ))
    .labelled("number")
    .boxed();
    let num = just('-')
        .to(-1i128)
        .or(empty().to(1))
        .then(positive_number)
        .map(|(pos, num)| Token::Num(pos * num))
        .labelled("number")
        .boxed();

    let base_type = choice((
        just("uint")
            .to(Token::BaseType(BaseType::U8))
            .labelled("uint"),
        just("u8").to(Token::BaseType(BaseType::U8)).labelled("u8"),
        just("u16")
            .to(Token::BaseType(BaseType::U8))
            .labelled("u16"),
        just("u32")
            .to(Token::BaseType(BaseType::U8))
            .labelled("u32"),
        just("u64")
            .to(Token::BaseType(BaseType::U8))
            .labelled("u64"),
        just("int")
            .to(Token::BaseType(BaseType::U8))
            .labelled("int"),
        just("i8").to(Token::BaseType(BaseType::U8)).labelled("i8"),
        just("i16")
            .to(Token::BaseType(BaseType::U8))
            .labelled("i16"),
        just("i32")
            .to(Token::BaseType(BaseType::U8))
            .labelled("i32"),
        just("i64")
            .to(Token::BaseType(BaseType::U8))
            .labelled("i64"),
        just("bool")
            .to(Token::BaseType(BaseType::U8))
            .labelled("bool"),
    ));

    let token = choice((
        doc_comment,
        ctrl,
        arrow,
        r#try,
        r#as,
        by,
        num,
        access,
        byte_order,
        bit_order,
        base_type,
        allow,
        default,
        catch_all,
        ident,
    ));

    let comment = just("//")
        .then_ignore(just("/").not())
        .ignore_then(any().and_is(text::newline().not()).repeated())
        .padded()
        .labelled("//");

    token
        .clone()
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(via_parser(
            any()
                .filter(|c: &char| !c.is_whitespace())
                .and_is(token.not())
                .repeated()
                .at_least(1)
                .map_with(|_, e| (Token::Error, e.span())),
        ))
        .repeated()
        .collect()
        .then_ignore(comment.repeated())
        .then_ignore(end())
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    DocCommentLine(&'src str),
    Ident(&'src str),
    Ctrl(Control),
    By,
    Try,
    As,
    Allow,
    Default,
    CatchAll,
    Num(i128),
    Access(Access),
    ByteOrder(ByteOrder),
    BitOrder(BitOrder),
    BaseType(BaseType),
    Error,
}

impl<'src> Display for Token<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::DocCommentLine(line) => write!(f, "/// {line}"),
            Token::Ident(ident) => write!(f, "#{ident}"),
            Token::Ctrl(val) => val.fmt(f),
            Token::Try => write!(f, "try"),
            Token::By => write!(f, "by"),
            Token::As => write!(f, "as"),
            Token::Num(n) => write!(f, "{n}"),
            Token::Error => write!(f, "ERROR"),
            Token::Access(val) => val.fmt(f),
            Token::ByteOrder(val) => val.fmt(f),
            Token::BitOrder(val) => val.fmt(f),
            Token::BaseType(val) => val.fmt(f),
            Token::Allow => write!(f, "allow"),
            Token::Default => write!(f, "default"),
            Token::CatchAll => write!(f, "catch-all"),
        }
    }
}

impl<'src> Token<'src> {
    /// New line before, new line after, indent change
    fn get_print_format(&self) -> (bool, bool, i32) {
        match self {
            Token::DocCommentLine(_) | Token::Ctrl(Control::Comma) => (false, true, 0),
            Token::Ctrl(Control::CurlyOpen) | Token::Ctrl(Control::BracketOpen) => (false, true, 1),
            Token::Ctrl(Control::CurlyClose) | Token::Ctrl(Control::BracketClose) => {
                (true, false, -1)
            }
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

            write!(stream, "{token} ")?;

            if newline_after {
                write!(stream, "\n{:width$}", "", width = indent as usize * 4)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Control {
    CurlyOpen,
    CurlyClose,
    BracketOpen,
    BracketClose,
    AngleOpen,
    AngleClose,
    Colon,
    Comma,
    Equals,
    Arrow,
}

impl Display for Control {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Control::CurlyOpen => "{",
            Control::CurlyClose => "}",
            Control::BracketOpen => "[",
            Control::BracketClose => "]",
            Control::AngleOpen => "<",
            Control::AngleClose => ">",
            Control::Colon => ":",
            Control::Comma => ",",
            Control::Equals => "=",
            Control::Arrow => "->",
        };

        write!(f, "{s}")
    }
}

#[cfg(test)]
mod tests {
    use std::io::stdout;

    use ariadne::{Label, Report, Source};

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
    }(),

    enum Purr -> u8 {
        A,
        B = 3:1,
        C,
        D = default 5,
        r#BE = catch-all 6,
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
        println!("Token size: {}", std::mem::size_of::<Spanned<Token>>());

        let start = std::time::Instant::now();
        let output = super::lexer().parse(CODE);
        let elapsed = start.elapsed();
        println!("{elapsed:?}");

        for error in output.errors() {
            let mut error_string = Vec::new();
            Report::build(
                ariadne::ReportKind::Error,
                ("input.ddlang", error.span().into_range()),
            )
            .with_message(error)
            .with_label(
                Label::new(("input.ddlang", error.span().into_range())).with_message("Here"),
            )
            .finish()
            .write(("input.ddlang", Source::from(CODE)), &mut error_string)
            .unwrap();
            eprintln!("{}", std::str::from_utf8(&error_string).unwrap())
        }

        if let Some(output) = output.output() {
            let mut stdout = stdout().lock();
            Token::formatted_print(&mut stdout, output.iter().map(|(token, _)| token)).unwrap();
        }
    }
}
