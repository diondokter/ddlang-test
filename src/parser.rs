use crate::{
    Access, BaseType, BitOrder, ByteOrder,
    lexer::{Control, Token},
};
use chumsky::{input::ValueInput, prelude::*};

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

pub struct Ast<'src> {
    pub nodes: Vec<Node<'src>>,
}

#[derive(Debug)]
pub struct Node<'src> {
    pub doc_comments: Vec<Spanned<&'src str>>,
    pub object_type: Ident<'src>,
    pub name: Ident<'src>,
    pub type_specifier: Option<TypeSpecifier<'src>>,
    pub properties: Vec<Spanned<Property<'src>>>,
    pub sub_nodes: Vec<Node<'src>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct TypeSpecifier<'src> {
    pub base_type: BaseType,
    pub use_try: bool,
    pub conversion: Option<TypeConversion<'src>>,
}

#[derive(Debug)]
pub enum TypeConversion<'src> {
    Reference(Ident<'src>),
    Subnode(Box<Node<'src>>),
}

#[derive(Debug)]
pub enum Property<'src> {
    Full {
        name: Ident<'src>,
        expression: Spanned<Expression<'src>>,
    },
    Anonymous {
        expression: Spanned<Expression<'src>>,
    },
}

#[derive(Debug)]
pub enum Expression<'src> {
    Address { end: i128, start: i128 },
    Repeat(Repeat<'src>),
    ResetNumber(i128),
    ResetArray(Vec<u8>),
    BaseType(BaseType),
    Allow,
    Number(i128),
    DefaultNumber(i128),
    CatchAllNumber(i128),
    Access(Access),
    ByteOrder(ByteOrder),
    BitOrder(BitOrder),
    TypeReference(Ident<'src>),
    SubNode(Box<Node<'src>>),
}

#[derive(Debug)]
pub struct Repeat<'src> {
    pub source: RepeatSource<'src>,
    pub stride: i128,
}

#[derive(Debug)]
pub enum RepeatSource<'src> {
    Count(i128),
    Enum(Ident<'src>),
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
        }
        .labelled("'identifier'");

        let any_doc_comment = select! {
            Token::DocCommentLine(val) => val
        }
        .map_with(|line, extra| (line, extra.span()))
        .labelled("'doc comment'");

        let any_num = select! {
            Token::Num(val) => val
        }
        .labelled("'number'");

        let address = any_num
            .then_ignore(just(Token::Ctrl(Control::Colon)))
            .then(any_num)
            .map(|(end, start)| Expression::Address { end, start })
            .labelled("'address'");
        let any_base_type = select! { Token::BaseType(bt) => bt }.labelled("'base type'");
        let repeat_expression = just(Token::Ctrl(Control::AngleOpen))
            .ignore_then(
                any_num
                    .map(RepeatSource::Count)
                    .or(any_ident.map(RepeatSource::Enum))
                    .then(just(Token::By).ignore_then(any_num).or_not()),
            )
            .then_ignore(just(Token::Ctrl(Control::AngleClose)))
            .map(|(source, stride)| {
                Expression::Repeat(Repeat {
                    source,
                    stride: stride.unwrap_or(1),
                })
            })
            .labelled("'<repeat>'");
        let reset_expression = just(Token::Ctrl(Control::BracketOpen))
            .ignore_then(
                any_num
                    .map_with(|num, extra| (num, extra.span()))
                    .separated_by(just(Token::Ctrl(Control::Comma)))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .try_map(|numbers, _| {
                        if numbers.len() == 1 {
                            Ok(Expression::ResetNumber(numbers[0].0))
                        } else {
                            numbers
                                .into_iter()
                                .map(|(num, num_span)| {
                                    u8::try_from(num)
                                        .map_err(|_| Rich::custom(num_span, "Value must be a byte"))
                                })
                                .collect::<Result<Vec<u8>, _>>()
                                .map(Expression::ResetArray)
                        }
                    })
                    .then_ignore(just(Token::Ctrl(Control::BracketClose))),
            )
            .labelled("'[reset]'");

        let expression = choice((
            address,
            any_base_type.map(Expression::BaseType),
            any_num.map(Expression::Number),
            just(Token::Default)
                .ignore_then(any_num)
                .map(Expression::DefaultNumber)
                .labelled("'default number'"),
            just(Token::CatchAll)
                .ignore_then(any_num)
                .map(Expression::CatchAllNumber)
                .labelled("'catch-all number'"),
            repeat_expression,
            reset_expression,
            just(Token::Allow).map(|_| Expression::Allow),
            select! { Token::Access(val) => val }.map(Expression::Access),
            select! { Token::ByteOrder(val) => val }.map(Expression::ByteOrder),
            select! { Token::BitOrder(val) => val }.map(Expression::BitOrder),
            any_ident.map(Expression::TypeReference),
        ))
        .map_with(|expression, extra| (expression, extra.span()));
        let property = any_ident
            .then(just(Token::Ctrl(Control::Colon)).ignore_then(expression.clone()))
            .map_with(|(name, expression), extra| {
                (Property::Full { name, expression }, extra.span())
            })
            .labelled("'property'");

        let type_conversion = just(Token::As).ignore_then(just(Token::Try).or_not()).then(
            node.clone()
                .map(|node| TypeConversion::Subnode(Box::new(node)))
                .or(any_ident.map(TypeConversion::Reference)),
        );
        let type_specifier = just(Token::Ctrl(Control::Arrow))
            .ignore_then(any_base_type)
            .then(type_conversion.or_not())
            .map(|(base_type, conversion)| TypeSpecifier {
                base_type,
                use_try: conversion
                    .as_ref()
                    .map(|(try_token, _)| try_token.is_some())
                    .unwrap_or_default(),
                conversion: conversion.map(|(_, conversion)| conversion),
            });

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
            .then_ignore(just(Token::Ctrl(Control::CurlyClose)))
            .labelled("'node body'");

        any_doc_comment
            .repeated()
            .collect()
            .then(any_ident.labelled("'object-type'"))
            .then(any_ident.labelled("'object-name'"))
            .then(expression.repeated().collect::<Vec<_>>())
            .then(type_specifier.or_not())
            .then(node_body.or_not())
            .map_with(
                |(((((doc_comments, object_type), name), expressions), type_specifier), body),
                 extra| {
                    let (properties, sub_nodes) = body.unwrap_or_default();

                    Node {
                        doc_comments,
                        object_type,
                        name,
                        type_specifier,
                        properties: expressions
                            .into_iter()
                            .map(|expression| {
                                let span = expression.1;
                                (Property::Anonymous { expression }, span)
                            })
                            .chain(properties)
                            .collect(),
                        sub_nodes,
                        span: extra.span(),
                    }
                },
            )
    });

    node.separated_by(just(Token::Ctrl(Control::Comma)))
        .allow_trailing()
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
        reset: [0x1234],
        reset: [0x12, 0x134], // Or reset with array
        address-overlap: allow,
        access: RW,

        fields: MyFs
    },

    enum Purr -> u8 {
        A: 0,
        B: 3:1,
        C: 4,
        D: default 5,
        r#BE: catch-all 6,
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

            println!("{:#?}", ast);
        }
    }
}
