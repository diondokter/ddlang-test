use crate::lexer::{Control, Token};
use chumsky::{input::ValueInput, prelude::*};

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

pub struct Ast<'src> {
    pub nodes: Vec<Node<'src>>,
}

#[derive(Debug)]
pub struct Node<'src> {
    pub doc_comments: Vec<&'src str>,
    pub r#type: Ident<'src>,
    pub name: Ident<'src>,
    pub properties: Vec<Property<'src>>,
    pub sub_nodes: Vec<Node<'src>>,
}

#[derive(Debug)]
pub enum Property<'src> {
    Full {
        name: Ident<'src>,
        expression: Expression<'src>,
    },
    Empty {
        name: Ident<'src>,
    },
    Anonymous {
        expression: Expression<'src>,
    },
}

#[derive(Debug)]
pub enum Expression<'src> {
    Address { end: i128, start: i128 },
    Repeat(Repeat<'src>),
    Reset(ResetValue),
    Allow,
    Number(i128),
    TypeReference(Ident<'src>),
    SubNode(Box<Node<'src>>),
}

#[derive(Debug)]
pub struct Repeat<'src> {
    pub source: RepeatSource<'src>,
    pub stride: i32,
}

#[derive(Debug)]
pub enum RepeatSource<'src> {
    Count(u16),
    Enum(Ident<'src>),
}

#[derive(Debug)]
pub enum ResetValue {
    Integer(u128),
    Array(Vec<u8>),
}

#[derive(Debug)]
pub struct Ident<'src> {
    pub val: &'src str,
    pub span: Span,
}

pub fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Vec<Node<'src>>, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    let node = recursive(|node| {
        let any_ident = select! {
            Token::Ident(val) = e => Ident { val, span: e.span() }
        };

        let any_doc_comment = select! {
            Token::DocCommentLine(val) => val
        }
        .labelled("doc comment");

        let expression = any()
            .filter(|t| !matches!(t, Token::Ctrl(Control::Comma)))
            .repeated();
        let property = any_ident
            .then_ignore(just(Token::Ctrl(Control::Colon)))
            .then(expression);

        let node_body = just(Token::Ctrl(Control::CurlyOpen))
            .ignore_then(
                property
                    .separated_by(just(Token::Ctrl(Control::Comma)))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then(
                node.separated_by(just(Token::Ctrl(Control::Comma)))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::Ctrl(Control::CurlyClose)));

        any_doc_comment
            .repeated()
            .collect()
            .then(any_ident.labelled("node-type"))
            .then(any_ident.labelled("node-name"))
            .then(node_body)
            .map(|(((doc_comments, r#type), name), body)| Node {
                doc_comments,
                r#type,
                name,
                properties: Vec::new(),
                sub_nodes: Vec::new(),
            })
    });

    node.separated_by(just(Token::Ctrl(Control::Comma)))
        .collect()
}

#[cfg(test)]
mod tests {
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
    },

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
    fn test_parser() {
        println!("Node size: {}", std::mem::size_of::<Spanned<Node>>());

        let (tokens, errors) = crate::lexer::lexer().parse(CODE).into_output_errors();

        for error in errors {
            let mut error_string = Vec::new();
            Report::build(
                ariadne::ReportKind::Error,
                ("input.ddlang", error.span().into_range()),
            )
            .with_message(&error)
            .with_label(
                Label::new(("input.ddlang", error.span().into_range())).with_message("Here"),
            )
            .finish()
            .write(("input.ddlang", Source::from(CODE)), &mut error_string)
            .unwrap();
            eprintln!("{}", std::str::from_utf8(&error_string).unwrap())
        }

        if let Some(tokens) = tokens {
            let start = std::time::Instant::now();

            let (ast, parse_errs) = super::parser()
                .map_with(|ast, e| (ast, e.span()))
                .parse(
                    tokens
                        .as_slice()
                        .map((CODE.len()..CODE.len()).into(), |(t, s)| (t, s)),
                )
                .into_output_errors();
            let elapsed = start.elapsed();
            println!("{elapsed:?}");

            for error in parse_errs {
                let mut error_string = Vec::new();
                Report::build(
                    ariadne::ReportKind::Error,
                    ("input.ddlang", error.span().into_range()),
                )
                .with_message(&error)
                .with_label(
                    Label::new(("input.ddlang", error.span().into_range())).with_message("Here"),
                )
                .finish()
                .write(("input.ddlang", Source::from(CODE)), &mut error_string)
                .unwrap();
                eprintln!("{}", std::str::from_utf8(&error_string).unwrap())
            }

            println!("{:?}", ast);
        }
    }
}
