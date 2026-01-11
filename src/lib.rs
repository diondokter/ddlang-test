use chumsky::{prelude::*, text::Char};

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
        .labelled("///").boxed();

    let ident_string = any()
        .filter(|c: &char| c.is_ident_start() || *c == '_')
        .then(
            any()
                .filter(|c: &char| c.is_ident_continue() || *c == '_' || *c == '-')
                .repeated(),
        )
        .to_slice().boxed();

    // A parser for identifiers and keywords
    let ident = ident_string.map(|ident: &str| match ident {
        "device" => Token::Device,
        "register" => Token::Register,
        "command" => Token::Command,
        "buffer" => Token::Buffer,
        "fieldset" => Token::FieldSet,
        "enum" => Token::Enum,
        "extern" => Token::Extern,
        _ => Token::Ident(ident),
    });

    let ctrl = one_of("()[]{}<>,?:").map(Token::Ctrl);
    let arrow = just("->").map(|_| Token::Arrow).labelled("->");
    let equals = just('=').map(|_| Token::Equals);
    let by = just("by").map(|_| Token::By).labelled("by");

    // TODO: Add underscore support in the middle of numbers
    let positive_number = choice((
        text::int(10)
            .map(|s: &str| i128::from_str_radix(s, 10).unwrap())
            .then_ignore(none_of("xbo")),
        just("0x").ignore_then(text::int(16).map(|s: &str| i128::from_str_radix(s, 16).unwrap())),
        just("0b").ignore_then(text::int(2).map(|s: &str| i128::from_str_radix(s, 2).unwrap())),
        just("0o").ignore_then(text::int(8).map(|s: &str| i128::from_str_radix(s, 8).unwrap())),
    ))
    .labelled("number").boxed();
    let num = just('-')
        .map(|_| -1i128)
        .or(empty().map(|_| 1))
        .then(positive_number)
        .map(|(pos, num)| Token::Num(pos * num))
        .labelled("number").boxed();

    let token = choice((doc_comment, ident, ctrl, arrow, equals, by, num));

    let comment = just("//")
        .then_ignore(just("/").not())
        .ignore_then(any().and_is(text::newline().not()).repeated())
        .padded()
        .labelled("//");

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
        .then_ignore(comment.repeated())
        .then_ignore(end())
}

#[derive(Debug)]
pub enum Token<'src> {
    DocCommentLine(&'src str),
    Device,
    Register,
    Command,
    Buffer,
    FieldSet,
    Enum,
    Extern,
    // TODO: Add config options, register options, etc
    Ident(&'src str),
    Ctrl(char),
    /// `->`
    Arrow,
    /// `=`
    Equals,
    By,
    Num(i128),
    Error,
}

#[cfg(test)]
mod tests {
    use ariadne::{Label, Report, Source};

    use super::*;

    const CODE: &str =
        "// Everything separately specified. Inline externs, enums and fieldsets are possible too
/// Doc comment for Foo


device Foo {
    register-address-type: u8,

    register Bar {
        address: 0,
        repeat: <10 by -2>,
        repeat: <MyEnum by 2>, // Or repeat with enum
        reset: 0x1234,
        reset: [0x12, 0x34], // Or reset with array
        address-overlap: allow,
        access: RW,

        fields: MyFs
    },

    enum Purr : u8 {
        A,
        B = 3:1,
        C,
        D = default 5,
        E = catch-all 6,
    },

    extern Rah : u64,

    fieldset MyFs {
        size-bits: 16,
        bit-overlap: allow,

        xena 7:4 RW <3 by 3> -> uint as Purr?,
        quux 3:0 RO -> uint,
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

        println!("{:?}", output.output());
    }
}
