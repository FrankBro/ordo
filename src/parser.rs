use std::{collections::BTreeMap, fmt};

use logos::{Lexer, Logos};

use crate::{
    expr::{Expr, IntBinOp, Pattern, Type},
    lexer::Token,
};

const EOF: &str = "<eof>";

#[derive(Debug, PartialEq)]
pub enum Error {
    Lexer,
    Expected(Vec<Token>, Option<Token>),
    ExpectedEof(Token),
    UnexpectedEof,
    InvalidPrefix(Option<Token>),
    DuplicateLabel(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Lexer => write!(f, "lexer encountered an unexpected token"),
            Error::Expected(tokens, token) => {
                let token_str = match token {
                    Some(token) => token.to_string(),
                    None => EOF.to_owned(),
                };
                let mut tokens_str = String::new();
                let mut sep = "";
                for token in tokens {
                    tokens_str.push_str(sep);
                    tokens_str.push_str(&token.to_string());
                    sep = ", ";
                }
                write!(f, "got {}, expected {}", token_str, tokens_str)
            }
            Error::ExpectedEof(token) => {
                write!(f, "got {}, expected {}", token, EOF)
            }
            Error::UnexpectedEof => write!(f, "got {}, expected something", EOF),
            Error::InvalidPrefix(token) => {
                let token_str = match token {
                    Some(token) => token.to_string(),
                    None => EOF.to_owned(),
                };
                write!(f, "invalid prefix: {}", token_str)
            }
            Error::DuplicateLabel(label) => write!(f, "duplicate label: {}", label),
        }
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

pub struct Parser<'a> {
    lexer: Lexer<'a, Token>,
    token: Option<Token>,
}

impl<'a> Parser<'a> {
    fn check(&mut self, token: Token) -> bool {
        self.token == Some(token)
    }

    fn matches(&mut self, token: Token) -> Result<bool> {
        if !self.check(token) {
            return Ok(false);
        }
        self.advance()?;
        Ok(true)
    }

    fn matches_ident(&mut self) -> Result<Option<String>> {
        let token = self.token.take();
        if let Some(Token::Ident(ident)) = token {
            self.advance()?;
            return Ok(Some(ident));
        }
        self.token = token;
        Ok(None)
    }

    fn matches_int(&mut self) -> Result<Option<i64>> {
        let token = self.token.take();
        if let Some(Token::Int(i)) = token {
            self.advance()?;
            return Ok(Some(i));
        }
        self.token = token;
        Ok(None)
    }

    fn matches_eof(&self) -> bool {
        self.token.is_none()
    }

    fn advance(&mut self) -> Result<()> {
        self.token = self.lexer.next().transpose().map_err(|()| Error::Lexer)?;
        Ok(())
    }

    fn expected<T>(&mut self, tokens: Vec<Token>) -> Result<T> {
        Err(Error::Expected(tokens, self.token.take()))
    }

    fn expect(&mut self, expected: Token) -> Result<()> {
        if self.token.as_ref() != Some(&expected) {
            return self.expected(vec![expected]);
        }
        self.advance()?;
        Ok(())
    }

    fn expect_ident(&mut self) -> Result<String> {
        let token = self.token.take();
        if let Some(Token::Ident(ident)) = token {
            self.advance()?;
            return Ok(ident);
        }
        Err(Error::Expected(vec![Token::empty_ident()], token))
    }

    pub fn expr(source: &str) -> Result<Expr> {
        let lexer = Token::lexer(source);
        let token = None;
        let mut parser = Parser { lexer, token };
        parser.advance()?;
        let expr = parser.expr_inner(0)?;
        if let Some(token) = parser.token.take() {
            return Err(Error::ExpectedEof(token));
        }
        Ok(expr)
    }

    fn paren_expr(&mut self) -> Result<Expr> {
        let expr = self.expr_inner(0)?;
        self.expect(Token::RParen)?;
        Ok(expr)
    }

    fn pattern_expr(&mut self) -> Result<Pattern> {
        if let Some(var) = self.matches_ident()? {
            Ok(Pattern::Var(var))
        } else if self.matches(Token::LBrace)? {
            if self.matches(Token::RBrace)? {
                return Ok(Pattern::Record(BTreeMap::new()));
            }
            let mut labels = BTreeMap::new();
            loop {
                let label = self.expect_ident()?;
                let var = if self.matches(Token::Equal)? {
                    let var = self.expect_ident()?;
                    Pattern::Var(var)
                } else {
                    Pattern::Var(label.clone())
                };
                if labels.insert(label.clone(), var).is_some() {
                    return Err(Error::DuplicateLabel(label));
                }
                if self.matches(Token::RBrace)? {
                    break;
                } else if self.matches(Token::Comma)? {
                    continue;
                } else {
                    return self.expected(vec![Token::empty_ident(), Token::RBrace]);
                }
            }
            Ok(Pattern::Record(labels))
        } else {
            self.expected(vec![Token::empty_ident(), Token::LBrace])
        }
    }

    fn let_expr(&mut self) -> Result<Expr> {
        let pattern = self.pattern_expr()?;
        self.expect(Token::Equal)?;
        let value = self.expr_inner(0)?;
        self.expect(Token::In)?;
        let body = self.expr_inner(0)?;
        Ok(Expr::Let(pattern, value.into(), body.into()))
    }

    fn fun_expr(&mut self) -> Result<Expr> {
        self.expect(Token::LParen)?;
        let mut params = Vec::new();
        loop {
            let param = self.pattern_expr()?;
            params.push(param);
            if self.matches(Token::RParen)? {
                break;
            } else if self.matches(Token::Comma)? {
                continue;
            } else {
                return self.expected(vec![Token::RParen, Token::Comma]);
            }
        }
        self.expect(Token::Arrow)?;
        let body = self.expr_inner(0)?;
        Ok(Expr::Fun(params, body.into()))
    }

    fn record_expr(&mut self) -> Result<Expr> {
        if self.matches(Token::RBrace)? {
            return Ok(Expr::RecordEmpty);
        }
        let mut labels = BTreeMap::new();
        let mut rest = Expr::RecordEmpty;
        loop {
            let label = self.expect_ident()?;
            let expr = if self.matches(Token::Equal)? {
                self.expr_inner(0)?
            } else {
                Expr::Var(label.clone())
            };

            if labels.insert(label.clone(), expr).is_some() {
                return Err(Error::DuplicateLabel(label));
            }

            if self.matches(Token::Comma)? {
                continue;
            } else if self.matches(Token::RBrace)? {
                break;
            } else if self.matches(Token::Pipe)? {
                rest = self.expr_inner(0)?;
                self.expect(Token::RBrace)?;
                break;
            } else {
                return self.expected(vec![Token::Pipe, Token::Comma, Token::RBrace]);
            }
        }
        Ok(Expr::RecordExtend(labels, rest.into()))
    }

    fn variant_expr(&mut self) -> Result<Expr> {
        let label = self.expect_ident()?;
        let expr = self.expr_inner(0)?;
        Ok(Expr::Variant(label, expr.into()))
    }

    fn match_expr(&mut self) -> Result<Expr> {
        let expr = self.expr_inner(0)?;
        self.expect(Token::LBrace)?;
        let mut cases = Vec::new();
        let mut default_case = None;
        loop {
            if self.matches(Token::Colon)? {
                let variant = self.expect_ident()?;
                let var = self.expect_ident()?;
                self.expect(Token::Arrow)?;
                let expr = self.expr_inner(0)?;
                cases.push((variant, var, expr));
            } else if let Some(var) = self.matches_ident()? {
                self.expect(Token::Arrow)?;
                let expr = self.expr_inner(0)?;
                default_case = Some((var, Box::new(expr)));
                self.expect(Token::RBrace)?;
                break;
            } else {
                return self.expected(vec![Token::Colon, Token::empty_ident()]);
            }
            if self.matches(Token::Pipe)? {
                continue;
            } else if self.matches(Token::RBrace)? {
                break;
            } else {
                return self.expected(vec![Token::Pipe, Token::RBrace]);
            }
        }
        Ok(Expr::Case(expr.into(), cases, default_case))
    }

    fn expr_lhs(&mut self) -> Result<Expr> {
        if let Some(var) = self.matches_ident()? {
            Ok(Expr::Var(var))
        } else if let Some(i) = self.matches_int()? {
            Ok(Expr::Int(i))
        } else if self.matches(Token::True)? {
            Ok(Expr::Bool(true))
        } else if self.matches(Token::False)? {
            Ok(Expr::Bool(false))
        } else if self.matches(Token::LParen)? {
            self.paren_expr()
        } else if self.matches(Token::Let)? {
            self.let_expr()
        } else if self.matches(Token::Fun)? {
            self.fun_expr()
        } else if self.matches(Token::LBrace)? {
            self.record_expr()
        } else if self.matches(Token::Colon)? {
            self.variant_expr()
        } else if self.matches(Token::Match)? {
            self.match_expr()
        } else {
            let r_bp = self.prefix_bp()?;
            if self.matches(Token::Negate)? {
                let rhs = self.expr_inner(r_bp)?;
                Ok(Expr::Negate(rhs.into()))
            } else {
                Err(Error::InvalidPrefix(self.token.take()))
            }
        }
    }

    fn call_expr(&mut self, lhs: Expr) -> Result<Expr> {
        let mut args = vec![self.expr_inner(0)?];
        loop {
            if self.matches(Token::RParen)? {
                break;
            } else if self.matches(Token::Comma)? {
            } else {
                return self.expected(vec![Token::Comma, Token::RParen]);
            }
            let arg = self.expr_inner(0)?;
            args.push(arg);
        }
        Ok(Expr::Call(lhs.into(), args))
    }

    fn record_select_expr(&mut self, lhs: Expr) -> Result<Expr> {
        let field = self.expect_ident()?;
        Ok(Expr::RecordSelect(lhs.into(), field))
    }

    fn record_restrict_expr(&mut self, lhs: Expr) -> Result<Expr> {
        let field = self.expect_ident()?;
        Ok(Expr::RecordRestrict(lhs.into(), field))
    }

    fn expr_postfix(&mut self, lhs: Expr) -> Result<Expr> {
        if self.matches(Token::LParen)? {
            self.call_expr(lhs)
        } else if self.matches(Token::Dot)? {
            self.record_select_expr(lhs)
        } else if self.matches(Token::Backslash)? {
            self.record_restrict_expr(lhs)
        } else {
            self.expected(vec![Token::LParen, Token::Dot, Token::Backslash])
        }
    }

    fn expr_infix(&mut self, lhs: Expr, r_bp: u8) -> Result<Expr> {
        if self.matches(Token::Plus)? {
            let rhs = self.expr_inner(r_bp)?;
            Ok(Expr::IntBinOp(IntBinOp::Plus, lhs.into(), rhs.into()))
        } else if self.matches(Token::Minus)? {
            let rhs = self.expr_inner(r_bp)?;
            Ok(Expr::IntBinOp(IntBinOp::Minus, lhs.into(), rhs.into()))
        } else if self.matches(Token::Multiply)? {
            let rhs = self.expr_inner(r_bp)?;
            Ok(Expr::IntBinOp(IntBinOp::Multiply, lhs.into(), rhs.into()))
        } else if self.matches(Token::Divide)? {
            let rhs = self.expr_inner(r_bp)?;
            Ok(Expr::IntBinOp(IntBinOp::Divide, lhs.into(), rhs.into()))
        } else if self.matches(Token::EqualEqual)? {
            let rhs = self.expr_inner(r_bp)?;
            Ok(Expr::EqualEqual(lhs.into(), rhs.into()))
        } else {
            self.expected(vec![
                Token::Plus,
                Token::Minus,
                Token::Multiply,
                Token::Divide,
                Token::EqualEqual,
            ])
        }
    }

    fn expr_inner(&mut self, min_bp: u8) -> Result<Expr> {
        let mut lhs = self.expr_lhs()?;
        loop {
            if self.matches_eof() {
                break;
            }
            if let Some(l_bp) = self.postfix_bp() {
                if l_bp < min_bp {
                    break;
                }
                lhs = self.expr_postfix(lhs)?;
                continue;
            }
            if let Some((l_bp, r_bp)) = self.infix_bp() {
                if l_bp < min_bp {
                    break;
                }
                lhs = self.expr_infix(lhs, r_bp)?;
                continue;
            }
            break;
        }
        Ok(lhs)
    }

    fn prefix_bp(&self) -> Result<u8> {
        match self.token {
            Some(Token::Negate) => Ok(7),
            None => Err(Error::UnexpectedEof),
            _ => Err(Error::InvalidPrefix(self.token.clone())),
        }
    }

    // TODO: Not sure how to determine precedence
    fn postfix_bp(&self) -> Option<u8> {
        match self.token {
            Some(Token::LParen) => Some(8),
            Some(Token::Dot) => Some(10),
            Some(Token::Backslash) => Some(9),
            _ => None,
        }
    }

    fn infix_bp(&self) -> Option<(u8, u8)> {
        match self.token {
            Some(Token::EqualEqual) => Some((2, 1)),
            Some(Token::Plus) | Some(Token::Minus) => Some((3, 4)),
            Some(Token::Multiply) | Some(Token::Divide) => Some((5, 6)),
            _ => None,
        }
    }
}

// Type
impl<'a> Parser<'a> {
    pub fn ty(source: &str) -> Result<(Vec<String>, Type)> {
        let lexer = Token::lexer(source);
        let mut parser = Parser { lexer, token: None };
        parser.advance()?;

        let mut vars = Vec::new();
        if parser.matches(Token::ForAll)? {
            parser.expect(Token::LBracket)?;
            let var = parser.expect_ident()?;
            vars.push(var);
            loop {
                if parser.matches(Token::RBracket)? {
                    break;
                } else if let Some(var) = parser.matches_ident()? {
                    vars.push(var);
                } else {
                    return parser.expected(vec![Token::RBracket, Token::empty_ident()]);
                }
            }
        }

        let ty = parser.ty_inner()?;
        if let Some(token) = parser.token.take() {
            return Err(Error::ExpectedEof(token));
        }
        Ok((vars, ty))
    }

    fn ty_inner(&mut self) -> Result<Type> {
        let mut ty = if self.matches(Token::LParen)? {
            self.paren_ty()?
        } else if let Some(name) = self.matches_ident()? {
            Type::Const(name)
        } else if self.matches(Token::LBrace)? {
            self.record_ty()?
        } else if self.matches(Token::LBracket)? {
            self.variant_ty()?
        } else {
            return self.expected(vec![Token::LParen, Token::empty_ident()]);
        };

        if self.matches(Token::LBracket)? {
            let mut args = Vec::new();
            loop {
                let arg = self.ty_inner()?;
                args.push(arg);
                if self.matches(Token::RBracket)? {
                    break;
                }
                self.expect(Token::Comma)?;
            }
            ty = Type::App(ty.into(), args);
        }

        if self.matches(Token::Arrow)? {
            let ret = self.ty_inner()?;
            ty = Type::Arrow(vec![ty], ret.into());
        }

        Ok(ty)
    }

    fn record_ty(&mut self) -> Result<Type> {
        if self.matches(Token::RBrace)? {
            return Ok(Type::Record(Type::RowEmpty.into()));
        }
        let mut labels = BTreeMap::new();
        let mut rest = Type::RowEmpty;
        loop {
            let label = self.expect_ident()?;
            if labels.is_empty() && self.matches(Token::RBrace)? {
                return Ok(Type::Record(Type::Const(label).into()));
            }
            self.expect(Token::Colon)?;
            let ty = self.ty_inner()?;
            if labels.insert(label.clone(), ty).is_some() {
                return Err(Error::DuplicateLabel(label));
            }
            if self.matches(Token::Comma)? {
                continue;
            } else if self.matches(Token::RBrace)? {
                break;
            } else if self.matches(Token::Pipe)? {
                rest = self.ty_inner()?;
                self.expect(Token::RBrace)?;
                break;
            } else {
                return self.expected(vec![Token::Comma, Token::RBrace, Token::Pipe]);
            }
        }
        Ok(Type::Record(Type::RowExtend(labels, rest.into()).into()))
    }

    fn variant_ty(&mut self) -> Result<Type> {
        let mut labels = BTreeMap::new();
        let mut rest = Type::RowEmpty;
        loop {
            let label = self.expect_ident()?;
            self.expect(Token::Colon)?;
            let ty = self.ty_inner()?;
            if labels.insert(label.clone(), ty).is_some() {
                return Err(Error::DuplicateLabel(label));
            }
            if self.matches(Token::Comma)? {
                continue;
            } else if self.matches(Token::RBracket)? {
                break;
            } else if self.matches(Token::Pipe)? {
                rest = self.ty_inner()?;
                self.expect(Token::RBracket)?;
                break;
            } else {
                return self.expected(vec![Token::Comma, Token::RBracket, Token::Pipe]);
            }
        }
        Ok(Type::Variant(Type::RowExtend(labels, rest.into()).into()))
    }

    fn paren_ty(&mut self) -> Result<Type> {
        let mut args = Vec::new();
        loop {
            let arg = self.ty_inner()?;
            args.push(arg);
            if self.matches(Token::RParen)? {
                break;
            }
            self.expect(Token::Comma)?;
        }
        if args.len() == 1 && !self.check(Token::Arrow) {
            return Ok(args.pop().unwrap());
        }
        self.expect(Token::Arrow)?;
        let ret = self.ty_inner()?;
        Ok(Type::Arrow(args, ret.into()))
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::util::*;
    use crate::expr::Expr;
    use crate::lexer::Token;

    use super::Error;
    use super::Parser;

    enum Expected {
        Pass(Expr),
        Fail(Error),
    }

    fn pass(expr: Expr) -> Expected {
        Expected::Pass(expr)
    }

    fn fail(error: Error) -> Expected {
        Expected::Fail(error)
    }

    #[test]
    fn precedence() {
        let cases: Vec<(&str, Expr)> = vec![
            ("1 + 2", plus(int(1), int(2))),
            ("1 + 2 * 3", plus(int(1), multiply(int(2), int(3)))),
            ("1 - 2 / 3", minus(int(1), divide(int(2), int(3)))),
            (
                "false == !true",
                equalequal(bool(false), negate(bool(true))),
            ),
        ];
        for (source, expected) in cases {
            let actual = Parser::expr(source).unwrap();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn patterns() {
        let cases: Vec<(&str, Expected)> = vec![
            (
                "let {a} = {a = 1} in a",
                pass(let_(
                    precord(vec![("a", pvar("a"))]),
                    record(vec![("a", int(1))], empty()),
                    var("a"),
                )),
            ),
            //
        ];
        for (source, expected) in cases {
            let res = Parser::expr(source);
            match expected {
                Expected::Pass(expected) => match res {
                    Ok(actual) => assert_eq!(expected, actual),
                    Err(e) => panic!("expected {:?}, got {:?} from {}", expected, e, source),
                },
                Expected::Fail(expected) => match res {
                    Ok(actual) => panic!("expected failure, got {:?} from {}", actual, source),
                    Err(actual) => {
                        assert_eq!(expected, actual, "for {}", source)
                    }
                },
            }
        }
    }

    #[test]
    fn exprs() {
        let cases: Vec<(&str, Expected)> = vec![
            ("", fail(Error::UnexpectedEof)),
            ("a", pass(var("a"))),
            ("f(x, y)", pass(call(var("f"), vec![var("x"), var("y")]))),
            (
                "f(x)(y)",
                pass(call(call(var("f"), vec![var("x")]), vec![var("y")])),
            ),
            (
                "let f = fun(x, y) -> g(x, y) in f(a, b)",
                pass(let_(
                    pvar("f"),
                    fun(
                        vec![pvar("x"), pvar("y")],
                        call(var("g"), vec![var("x"), var("y")]),
                    ),
                    call(var("f"), vec![var("a"), var("b")]),
                )),
            ),
            (
                "let x = a in
                let y = b in
                f(x, y)",
                pass(let_(
                    pvar("x"),
                    var("a"),
                    let_(
                        pvar("y"),
                        var("b"),
                        call(var("f"), vec![var("x"), var("y")]),
                    ),
                )),
            ),
            (
                "f x",
                fail(Error::ExpectedEof(Token::Ident("x".to_owned()))),
            ),
            ("let a = one", fail(Error::Expected(vec![Token::In], None))),
            ("a, b", fail(Error::ExpectedEof(Token::Comma))),
            ("a = b", fail(Error::ExpectedEof(Token::Equal))),
            // TODO: Not an ideal error here
            ("()", fail(Error::InvalidPrefix(Some(Token::RParen)))),
            ("fun(x) -> x", pass(fun(vec![pvar("x")], var("x")))),
            // records
            ("{}", pass(empty())),
            ("{ }", pass(empty())),
            ("{", fail(Error::Expected(vec![Token::empty_ident()], None))),
            ("a.x", pass(select(var("a"), "x"))),
            ("m \\ a", pass(restrict(var("m"), "a"))),
            ("{a = x}", pass(record(vec![("a", var("x"))], empty()))),
            (
                "{a = x",
                fail(Error::Expected(
                    vec![Token::Pipe, Token::Comma, Token::RBrace],
                    None,
                )),
            ),
            (
                "{a=x, b = y}",
                pass(record(vec![("a", var("x")), ("b", var("y"))], empty())),
            ),
            (
                "{b = y ,a=x}",
                pass(record(vec![("a", var("x")), ("b", var("y"))], empty())),
            ),
            (
                "{a=x,h=w,d=y,b=q,g=z,c=t,e=s,f=r}",
                pass(record(
                    vec![
                        ("a", var("x")),
                        ("b", var("q")),
                        ("c", var("t")),
                        ("d", var("y")),
                        ("e", var("s")),
                        ("f", var("r")),
                        ("g", var("z")),
                        ("h", var("w")),
                    ],
                    empty(),
                )),
            ),
            ("{a = x|m}", pass(record(vec![("a", var("x"))], var("m")))),
            (
                "{|m}",
                fail(Error::Expected(
                    vec![Token::empty_ident()],
                    Some(Token::Pipe),
                )),
            ),
            (
                "{ a = x, b = y | m}",
                pass(record(vec![("a", var("x")), ("b", var("y"))], var("m"))),
            ),
            (
                "{ a = x, b = y | m \\ a }",
                pass(record(
                    vec![("a", var("x")), ("b", var("y"))],
                    restrict(var("m"), "a"),
                )),
            ),
            (
                "let x = {a = f(x), b = y.b} in { a = fun(z) -> z | x \\ a }",
                pass(let_(
                    pvar("x"),
                    record(
                        vec![
                            ("a", call(var("f"), vec![var("x")])),
                            ("b", select(var("y"), "b")),
                        ],
                        empty(),
                    ),
                    record(
                        vec![("a", fun(vec![pvar("z")], var("z")))],
                        restrict(var("x"), "a"),
                    ),
                )),
            ),
            (
                "{a = x, a = y}",
                fail(Error::DuplicateLabel("a".to_owned())),
            ),
        ];
        for (source, expected) in cases {
            let res = Parser::expr(source);
            match expected {
                Expected::Pass(expected) => match res {
                    Ok(actual) => assert_eq!(expected, actual),
                    Err(e) => panic!("expected {:?}, got {:?} from {}", expected, e, source),
                },
                Expected::Fail(expected) => match res {
                    Ok(actual) => panic!("expected failure, got {:?} from {}", actual, source),
                    Err(actual) => {
                        assert_eq!(expected, actual, "for {}", source)
                    }
                },
            }
        }
    }
}
