use core::fmt;

use logos::{Lexer, Logos, Skip};

fn int(lex: &mut Lexer<Token>) -> Option<i64> {
    let slice = lex.slice();
    if slice.starts_with('-') {
        let int: i64 = slice[1..slice.len()].parse().ok()?;
        Some(-int)
    } else {
        let int: i64 = slice[..slice.len()].parse().ok()?;
        Some(int)
    }
}

fn ident(lex: &mut Lexer<Token>) -> Option<String> {
    let slice = lex.slice();
    let ident: String = slice[..slice.len()].parse().ok()?;
    Some(ident)
}

#[derive(Default)]
pub struct Extras {
    pub line_breaks: usize,
    pub column_offset: usize,
}

fn newline(lex: &mut Lexer<Token>) -> Skip {
    lex.extras.line_breaks += 1;
    lex.extras.column_offset = lex.span().end;
    Skip
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(extras = Extras)]
#[logos(skip r"[ \t\f\r]+")]
pub enum Token {
    #[token("let")]
    Let,
    #[token("=")]
    Equal,
    #[token("in")]
    In,
    #[token("fun")]
    Fun,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[token("forall")]
    ForAll,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("match")]
    Match,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(".")]
    Dot,
    #[token("|")]
    Pipe,
    #[token(":")]
    Colon,
    #[token("\\")]
    Backslash,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("!")]
    Negate,
    #[token("==")]
    EqualEqual,
    #[regex("-?[0-9]+", int)]
    Int(i64),
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("elif")]
    Elif,
    #[token("else")]
    Else,
    #[token("<")]
    LessThan,
    #[token("<=")]
    LessThanOrEqual,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterThanOrEqual,
    #[token("?")]
    QuestionMark,
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", ident)]
    Ident(String),
    #[regex(r"\n", newline)]
    Newline,
}

impl Token {
    pub fn empty_ident() -> Self {
        Self::Ident("".to_owned())
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Token::Let => "let",
            Token::Equal => "=",
            Token::In => "in",
            Token::Fun => "fun",
            Token::Arrow => "->",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::Comma => ",",
            Token::ForAll => "forall",
            Token::LBracket => "[",
            Token::RBracket => "]",
            Token::Match => "match",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::Dot => ".",
            Token::Pipe => "|",
            Token::Colon => ":",
            Token::Backslash => "\\",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Multiply => "*",
            Token::Divide => "/",
            Token::Negate => "!",
            Token::EqualEqual => "==",
            Token::Int(i) => return write!(f, "{}", i),
            Token::True => "true",
            Token::False => "false",
            Token::Ident(i) => i,
            Token::FatArrow => "=>",
            Token::If => "if",
            Token::Then => "then",
            Token::Elif => "elif",
            Token::Else => "else",
            Token::LessThan => "<",
            Token::LessThanOrEqual => "<=",
            Token::GreaterThan => ">",
            Token::GreaterThanOrEqual => ">=",
            Token::QuestionMark => "?",
            Token::Newline => "\n",
        };
        write!(f, "{}", s)
    }
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use super::Token;

    #[test]
    fn tests() {
        let cases = vec![
            ("", vec![]),
            ("  \t\n\n\t\r\n\r", vec![]),
            (
                "())in,let_ _1Ma->==",
                vec![
                    Ok(Token::LParen),
                    Ok(Token::RParen),
                    Ok(Token::RParen),
                    Ok(Token::In),
                    Ok(Token::Comma),
                    Ok(Token::Ident("let_".to_owned())),
                    Ok(Token::Ident("_1Ma".to_owned())),
                    Ok(Token::Arrow),
                    Ok(Token::EqualEqual),
                ],
            ),
            (
                "let fun in",
                vec![Ok(Token::Let), Ok(Token::Fun), Ok(Token::In)],
            ),
            (";", vec![Err(())]),
            ("1", vec![Ok(Token::Int(1))]),
            ("-1", vec![Ok(Token::Int(-1))]),
            ("10", vec![Ok(Token::Int(10))]),
            ("-10", vec![Ok(Token::Int(-10))]),
        ];
        for (input, expected) in cases {
            let lex = Token::lexer(input);
            let mut actual = Vec::new();
            for token in lex {
                actual.push(token);
            }
            assert_eq!(expected, actual, "{}", input);
        }
    }
}
