use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};

use itertools::Itertools;
use logos::{Lexer, Logos};

use crate::{
    expr::{Constraints, Expr, IntBinOp, Pattern, Type},
    lexer::Token,
};

const EOF: &str = "<eof>";

#[derive(Debug, PartialEq)]
pub enum Error {
    Lexer,
    Expected(&'static str, Vec<Token>, Option<Token>),
    ExpectedEof(Token),
    UnexpectedEof,
    InvalidPrefix(Option<Token>),
    DuplicateLabel(String),
    InvalidTypeVarName(String),
    NoRowForConstraints(String),
    RowConstraintsAlreadyDefined(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Lexer => write!(f, "lexer encountered an unexpected token"),
            Error::Expected(context, tokens, token) => {
                let token= match token {
                    Some(token) => token.to_string(),
                    None => EOF.to_owned(),
                };
                let tokens = tokens.iter().map(|token| token.to_string()).join(", ");
                write!(f, "got {}, expected {} in context {}", token, tokens, context)
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
            Error::InvalidTypeVarName(name) => write!(f, "invalid type var name, should be either one letter or the letter 'r' and one letter, was: {}", name),
            Error::NoRowForConstraints(name) => write!(f, "found constraint for a row that isn't defined: {}", name),
            Error::RowConstraintsAlreadyDefined(name) => write!(f, "constraints for a row with constraints already defined: {}", name),
        }
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

pub struct Parser<'a> {
    is_repl: bool,
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

    fn expected<T>(&mut self, tokens: Vec<Token>, context: &'static str) -> Result<T> {
        Err(Error::Expected(context, tokens, self.token.take()))
    }

    fn expect(&mut self, expected: Token, context: &'static str) -> Result<()> {
        if self.token.as_ref() != Some(&expected) {
            return self.expected(vec![expected], context);
        }
        self.advance()?;
        Ok(())
    }

    fn expect_ident(&mut self, context: &'static str) -> Result<String> {
        let token = self.token.take();
        if let Some(Token::Ident(ident)) = token {
            self.advance()?;
            return Ok(ident);
        }
        Err(Error::Expected(context, vec![Token::empty_ident()], token))
    }

    pub fn repl(source: &str) -> Result<Expr> {
        Self::expr_source(source, true)
    }

    #[cfg(test)]
    pub fn expr(source: &str) -> Result<Expr> {
        Self::expr_source(source, false)
    }

    fn expr_source(source: &str, is_repl: bool) -> Result<Expr> {
        let lexer = Token::lexer(source);
        let token = None;
        let mut parser = Parser {
            lexer,
            token,
            is_repl,
        };
        parser.advance()?;
        let expr = parser.expr_inner(0)?;
        if let Some(token) = parser.token.take() {
            return Err(Error::ExpectedEof(token));
        }
        Ok(expr)
    }

    fn paren_expr(&mut self) -> Result<Expr> {
        let expr = self.expr_inner(0)?;
        self.expect(Token::RParen, "paren expr")?;
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
                let label = self.expect_ident("pattern expr record label")?;
                let pattern = if self.matches(Token::Equal)? {
                    self.pattern_expr()?
                } else {
                    Pattern::Var(label.clone())
                };
                if labels.insert(label.clone(), pattern).is_some() {
                    return Err(Error::DuplicateLabel(label));
                }
                if self.matches(Token::RBrace)? {
                    break;
                } else if self.matches(Token::Comma)? {
                    continue;
                } else {
                    return self.expected(
                        vec![Token::empty_ident(), Token::RBrace],
                        "pattern expr record",
                    );
                }
            }
            Ok(Pattern::Record(labels))
        } else {
            self.expected(vec![Token::empty_ident(), Token::LBrace], "pattern expr")
        }
    }

    fn pattern_list(&mut self) -> Result<Vec<Pattern>> {
        let mut patterns = Vec::new();
        loop {
            let pattern = self.pattern_expr()?;
            patterns.push(pattern);
            if self.matches(Token::RParen)? {
                break;
            } else if self.matches(Token::Comma)? {
                continue;
            } else {
                return self.expected(vec![Token::RParen, Token::Comma], "pattern list");
            }
        }
        Ok(patterns)
    }

    fn let_expr(&mut self) -> Result<Expr> {
        let pattern = self.pattern_expr()?;
        match pattern {
            Pattern::Var(name) if self.matches(Token::LParen)? => {
                let params = self.pattern_list()?;
                self.expect(Token::Equal, "let expr fun")?;
                let fun_body = self.expr_inner(0)?;
                let body = if self.is_repl && self.token.is_none() {
                    Expr::Var(name.clone())
                } else {
                    self.expect(Token::In, "let expr fun")?;
                    self.expr_inner(0)?
                };
                let fun = Expr::Fun(params, fun_body.into());
                Ok(Expr::Let(Pattern::Var(name), fun.into(), body.into()))
            }
            pattern => {
                self.expect(Token::Equal, "let expr")?;
                let value = self.expr_inner(0)?;
                if self.is_repl && self.token.is_none() {
                    let body = pattern.expr();
                    Ok(Expr::Let(pattern, value.into(), body.into()))
                } else {
                    self.expect(Token::In, "let expr")?;
                    let body = self.expr_inner(0)?;
                    Ok(Expr::Let(pattern, value.into(), body.into()))
                }
            }
        }
    }

    fn fun_expr(&mut self) -> Result<Expr> {
        self.expect(Token::LParen, "fun expr")?;
        let params = self.pattern_list()?;
        self.expect(Token::Arrow, "fun expr")?;
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
            let label = self.expect_ident("record expr label")?;
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
                self.expect(Token::RBrace, "record expr rest")?;
                break;
            } else {
                return self.expected(
                    vec![Token::Pipe, Token::Comma, Token::RBrace],
                    "record expr",
                );
            }
        }
        Ok(Expr::RecordExtend(labels, rest.into()))
    }

    fn variant_expr(&mut self) -> Result<Expr> {
        let label = self.expect_ident("variant expr")?;
        let expr = self.expr_inner(0)?;
        Ok(Expr::Variant(label, expr.into()))
    }

    fn match_expr(&mut self) -> Result<Expr> {
        let expr = self.expr_inner(0)?;
        self.expect(Token::LBrace, "match expr")?;
        let mut cases = Vec::new();
        let mut default_case = None;
        loop {
            if self.matches(Token::Colon)? {
                let variant = self.expect_ident("match expr case variant")?;
                let var = self.expect_ident("match expr case value")?;
                self.expect(Token::Arrow, "match expr case")?;
                let expr = self.expr_inner(0)?;
                cases.push((variant, var, expr));
            } else if let Some(var) = self.matches_ident()? {
                self.expect(Token::Arrow, "match expr default case")?;
                let expr = self.expr_inner(0)?;
                default_case = Some((var, Box::new(expr)));
                self.expect(Token::RBrace, "match expr default case")?;
                break;
            } else {
                return self.expected(vec![Token::Colon, Token::empty_ident()], "match expr case");
            }
            if self.matches(Token::Comma)? {
                continue;
            } else if self.matches(Token::RBrace)? {
                break;
            } else {
                return self.expected(vec![Token::Comma, Token::RBrace], "match expr");
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
                return self.expected(vec![Token::Comma, Token::RParen], "call expr");
            }
            let arg = self.expr_inner(0)?;
            args.push(arg);
        }
        Ok(Expr::Call(lhs.into(), args))
    }

    fn record_select_expr(&mut self, lhs: Expr) -> Result<Expr> {
        let field = self.expect_ident("record select expr")?;
        Ok(Expr::RecordSelect(lhs.into(), field))
    }

    fn record_restrict_expr(&mut self, lhs: Expr) -> Result<Expr> {
        let field = self.expect_ident("record restrict expr")?;
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
            self.expected(
                vec![Token::LParen, Token::Dot, Token::Backslash],
                "expr postfix",
            )
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
            self.expected(
                vec![
                    Token::Plus,
                    Token::Minus,
                    Token::Multiply,
                    Token::Divide,
                    Token::EqualEqual,
                ],
                "expr infix",
            )
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

#[derive(Default)]
pub struct ForAll {
    pub vars: BTreeSet<String>,
    pub row_vars: BTreeMap<String, Constraints>,
}

// Type
impl<'a> Parser<'a> {
    fn forall_ty(&mut self) -> Result<ForAll> {
        let mut vars = BTreeSet::new();
        let mut row_vars = BTreeMap::new();
        loop {
            if let Some(name) = self.matches_ident()? {
                // TODO: Could be better
                if name.len() == 2 && name.starts_with('r') {
                    row_vars.insert(name, Constraints::new());
                } else if name.len() == 1 {
                    vars.insert(name);
                } else {
                    return Err(Error::InvalidTypeVarName(name));
                }
            } else if self.matches(Token::Dot)? {
                self.expect(Token::LParen, "forall constraints")?;
                'outer: loop {
                    let row_name = self.expect_ident("forall constraints name")?;
                    let row_constraints = row_vars
                        .get_mut(&row_name)
                        .ok_or_else(|| Error::NoRowForConstraints(row_name.clone()))?;
                    if !row_constraints.is_empty() {
                        return Err(Error::RowConstraintsAlreadyDefined(row_name));
                    }
                    'inner: loop {
                        self.expect(Token::Backslash, "forall constraints label")?;
                        let label = self.expect_ident("forall constraints label")?;
                        row_constraints.insert(label);
                        if self.matches(Token::RParen)? {
                            break 'outer;
                        } else if self.matches(Token::Comma)? {
                            break 'inner;
                        }
                    }
                }
                self.expect(Token::FatArrow, "forall constraints")?;
                return Ok(ForAll { vars, row_vars });
            } else if self.matches(Token::FatArrow)? {
                return Ok(ForAll { vars, row_vars });
            } else {
                return self.expected(
                    vec![Token::empty_ident(), Token::Dot, Token::FatArrow],
                    "forall",
                );
            }
        }
    }

    pub fn ty(source: &str) -> Result<(ForAll, Type)> {
        let lexer = Token::lexer(source);
        let mut parser = Parser {
            is_repl: false,
            lexer,
            token: None,
        };
        parser.advance()?;

        let forall = if parser.matches(Token::ForAll)? {
            parser.forall_ty()?
        } else {
            ForAll::default()
        };

        let ty = parser.ty_inner()?;
        if let Some(token) = parser.token.take() {
            return Err(Error::ExpectedEof(token));
        }
        Ok((forall, ty))
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
            return self.expected(vec![Token::LParen, Token::empty_ident()], "ty");
        };

        if self.matches(Token::LBracket)? {
            let mut args = Vec::new();
            loop {
                let arg = self.ty_inner()?;
                args.push(arg);
                if self.matches(Token::RBracket)? {
                    break;
                }
                self.expect(Token::Comma, "ty app")?;
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
            let label = self.expect_ident("record ty label")?;
            if labels.is_empty() && self.matches(Token::RBrace)? {
                return Ok(Type::Record(Type::Const(label).into()));
            }
            self.expect(Token::Colon, "record ty")?;
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
                self.expect(Token::RBrace, "record ty rest")?;
                break;
            } else {
                return self.expected(vec![Token::Comma, Token::RBrace, Token::Pipe], "record ty");
            }
        }
        Ok(Type::Record(Type::RowExtend(labels, rest.into()).into()))
    }

    fn variant_ty(&mut self) -> Result<Type> {
        let mut labels = BTreeMap::new();
        let mut rest = Type::RowEmpty;
        loop {
            let label = self.expect_ident("variant ty")?;
            self.expect(Token::Colon, "variant ty")?;
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
                self.expect(Token::RBracket, "variant ty")?;
                break;
            } else {
                return self.expected(
                    vec![Token::Comma, Token::RBracket, Token::Pipe],
                    "variant ty",
                );
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
            self.expect(Token::Comma, "paren ty")?;
        }
        if args.len() == 1 && !self.check(Token::Arrow) {
            return Ok(args.pop().unwrap());
        }
        self.expect(Token::Arrow, "paren ty")?;
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

    #[track_caller]
    fn pass(source: &str, expected: Expr) {
        let actual = Parser::expr(source).unwrap();
        assert_eq!(expected, actual);
    }

    #[track_caller]
    fn fail(source: &str, expected: Error) {
        let actual = Parser::expr(source).unwrap_err();
        assert_eq!(expected, actual, "for {}", source);
    }

    #[track_caller]
    fn pass_repl(source: &str, expected: Expr) {
        let actual = Parser::repl(source).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn precedence() {
        pass("1 + 2", plus(int(1), int(2)));
        pass("1 + 2 * 3", plus(int(1), multiply(int(2), int(3))));
        pass("1 - 2 / 3", minus(int(1), divide(int(2), int(3))));
        pass(
            "false == !true",
            equalequal(bool(false), negate(bool(true))),
        );
    }

    #[test]
    fn patterns() {
        pass(
            "let {a} = {a = 1} in a",
            let_(
                precord(vec![("a", pvar("a"))]),
                record(vec![("a", int(1))], empty()),
                var("a"),
            ),
        );
        pass(
            "let f(a, b) = a + b in f(1, 2)",
            let_(
                pvar("f"),
                fun(vec![pvar("a"), pvar("b")], plus(var("a"), var("b"))),
                call(var("f"), vec![int(1), int(2)]),
            ),
        );
        pass(
            "let {x = {y = y}} = {x = {y = 1}} in y",
            let_(
                precord(vec![("x", precord(vec![("y", pvar("y"))]))]),
                record(vec![("x", record(vec![("y", int(1))], empty()))], empty()),
                var("y"),
            ),
        );
    }

    #[test]
    fn exprs() {
        fail("", Error::UnexpectedEof);
        pass("a", var("a"));
        pass("f(x, y)", call(var("f"), vec![var("x"), var("y")]));
        pass(
            "f(x)(y)",
            call(call(var("f"), vec![var("x")]), vec![var("y")]),
        );
        pass(
            "let f = fun(x, y) -> g(x, y) in f(a, b)",
            let_(
                pvar("f"),
                fun(
                    vec![pvar("x"), pvar("y")],
                    call(var("g"), vec![var("x"), var("y")]),
                ),
                call(var("f"), vec![var("a"), var("b")]),
            ),
        );
        pass(
            "let x = a in
                let y = b in
                f(x, y)",
            let_(
                pvar("x"),
                var("a"),
                let_(
                    pvar("y"),
                    var("b"),
                    call(var("f"), vec![var("x"), var("y")]),
                ),
            ),
        );
        fail("f x", Error::ExpectedEof(Token::Ident("x".to_owned())));
        fail(
            "let a = one",
            Error::Expected("let expr", vec![Token::In], None),
        );
        fail("a, b", Error::ExpectedEof(Token::Comma));
        fail("a = b", Error::ExpectedEof(Token::Equal));
        // TODO: Not an ideal error here
        fail("()", Error::InvalidPrefix(Some(Token::RParen)));
        pass("fun(x) -> x", fun(vec![pvar("x")], var("x")));
        // records
        pass("{}", empty());
        pass("{ }", empty());
        fail(
            "{",
            Error::Expected("record expr label", vec![Token::empty_ident()], None),
        );
        pass("a.x", select(var("a"), "x"));
        pass("m \\ a", restrict(var("m"), "a"));
        pass("{a = x}", record(vec![("a", var("x"))], empty()));
        fail(
            "{a = x",
            Error::Expected(
                "record expr",
                vec![Token::Pipe, Token::Comma, Token::RBrace],
                None,
            ),
        );
        pass(
            "{a=x, b = y}",
            record(vec![("a", var("x")), ("b", var("y"))], empty()),
        );
        pass(
            "{b = y ,a=x}",
            record(vec![("a", var("x")), ("b", var("y"))], empty()),
        );
        pass(
            "{a=x,h=w,d=y,b=q,g=z,c=t,e=s,f=r}",
            record(
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
            ),
        );
        pass("{a = x|m}", record(vec![("a", var("x"))], var("m")));
        fail(
            "{|m}",
            Error::Expected(
                "record expr label",
                vec![Token::empty_ident()],
                Some(Token::Pipe),
            ),
        );
        pass(
            "{ a = x, b = y | m}",
            record(vec![("a", var("x")), ("b", var("y"))], var("m")),
        );
        pass(
            "{ a = x, b = y | m \\ a }",
            record(
                vec![("a", var("x")), ("b", var("y"))],
                restrict(var("m"), "a"),
            ),
        );
        pass(
            "let x = {a = f(x), b = y.b} in { a = fun(z) -> z | x \\ a }",
            let_(
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
            ),
        );
        fail("{a = x, a = y}", Error::DuplicateLabel("a".to_owned()));
        pass(
            "{x,y}",
            record(vec![("x", var("x")), ("y", var("y"))], empty()),
        );
        pass(
            "f({x,y})",
            call(
                var("f"),
                vec![record(vec![("x", var("x")), ("y", var("y"))], empty())],
            ),
        );
    }

    #[test]
    fn repl() {
        pass_repl("let a = 0", let_(pvar("a"), int(0), var("a")));
        pass_repl(
            "let f(x) = x",
            let_(pvar("f"), fun(vec![pvar("x")], var("x")), var("f")),
        );
        pass_repl(
            "let {x = x} = {x = 1}",
            let_(
                precord(vec![("x", pvar("x"))]),
                record(vec![("x", int(1))], empty()),
                record(vec![("x", var("x"))], empty()),
            ),
        );
        pass_repl(
            "let f({ x = x }) = x",
            let_(
                pvar("f"),
                fun(vec![precord(vec![("x", pvar("x"))])], var("x")),
                var("f"),
            ),
        );
        pass_repl(
            "let default_with(default, value) = match value { :some value -> value, :none x -> default }",
            let_(
                pvar("default_with"),
                fun(vec![pvar("default"), pvar("value")],
                match_(var("value"), vec![
                    ("some", "value", var("value")),
                    ("none", "x", var("default"))
                ], None)),
                var("default_with")));
    }
}
