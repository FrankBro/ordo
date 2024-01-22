use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};

use itertools::Itertools;
use logos::{Lexer, Logos};

use crate::{
    expr::{At, Constraints, Expr, ExprAt, IntBinOp, Pattern, PatternAt, Position, Type},
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
    DuplicateLabel(At<String>),
    InvalidTypeVarName(At<String>),
    NoRowForConstraints(At<String>),
    RowConstraintsAlreadyDefined(At<String>),
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
            Error::DuplicateLabel(label) => write!(f, "duplicate label: {}", label.value),
            Error::InvalidTypeVarName(name) => write!(f, "invalid type var name, should be either one letter or the letter 'r' and one letter, was: {}", name.value),
            Error::NoRowForConstraints(name) => write!(f, "found constraint for a row that isn't defined: {}", name.value),
            Error::RowConstraintsAlreadyDefined(name) => write!(f, "constraints for a row with constraints already defined: {}", name.value),
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
    fn at<T>(&self, value: T) -> At<T> {
        let line = self.lexer.extras.line_breaks;
        let offset = self.lexer.extras.column_offset;
        let span = self.lexer.span();
        At {
            value,
            start: Position {
                line,
                column: span.start - offset,
            },
            end: Position {
                line,
                column: span.end - offset,
            },
        }
    }

    fn check(&mut self, token: Token) -> bool {
        self.token == Some(token)
    }

    fn matches(&mut self, token: Token) -> Result<Option<At<()>>> {
        if !self.check(token) {
            return Ok(None);
        }
        let at = self.at(());
        self.advance()?;
        Ok(Some(at))
    }

    fn matches_ident(&mut self) -> Result<Option<At<String>>> {
        let token = self.token.take();
        if let Some(Token::Ident(ident)) = token {
            let ident = self.at(ident);
            self.advance()?;
            return Ok(Some(ident));
        }
        self.token = token;
        Ok(None)
    }

    fn matches_int(&mut self) -> Result<Option<At<i64>>> {
        let token = self.token.take();
        if let Some(Token::Int(i)) = token {
            let i = self.at(i);
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

    fn expect(&mut self, expected: Token, context: &'static str) -> Result<At<()>> {
        if self.token.as_ref() != Some(&expected) {
            return self.expected(vec![expected], context);
        }
        let at = self.at(());
        self.advance()?;
        Ok(at)
    }

    fn expect_ident(&mut self, context: &'static str) -> Result<At<String>> {
        let token = self.token.take();
        if let Some(Token::Ident(ident)) = token {
            let ident = self.at(ident);
            self.advance()?;
            return Ok(ident);
        }
        Err(Error::Expected(context, vec![Token::empty_ident()], token))
    }

    pub fn repl(source: &str) -> Result<ExprAt> {
        Self::expr_source(source, true)
    }

    #[cfg(test)]
    pub fn expr(source: &str) -> Result<ExprAt> {
        Self::expr_source(source, false)
    }

    fn expr_source(source: &str, is_repl: bool) -> Result<ExprAt> {
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

    fn paren_expr(&mut self) -> Result<ExprAt> {
        let expr = self.expr_inner(0)?;
        self.expect(Token::RParen, "paren expr")?;
        Ok(expr)
    }

    /// Return the labels and closing brace location
    fn pattern_expr_inner(&mut self) -> Result<(BTreeMap<String, ExprAt>, At<()>)> {
        let mut labels = BTreeMap::new();
        loop {
            let label = self.expect_ident("pattern expr record label")?;
            let pattern = if self.matches(Token::Equal)?.is_some() {
                self.pattern_expr()?
            } else {
                label.clone().map(Pattern::Var).into()
            };
            if labels.insert(label.clone().value, pattern).is_some() {
                return Err(Error::DuplicateLabel(label));
            }
            if let Some(r) = self.matches(Token::RBrace)? {
                return Ok((labels, r));
            } else if self.matches(Token::Comma)?.is_some() {
                continue;
            } else {
                return self.expected(
                    vec![Token::empty_ident(), Token::RBrace],
                    "pattern expr record",
                );
            }
        }
    }

    fn pattern_expr(&mut self) -> Result<PatternAt> {
        if let Some(var) = self.matches_ident()? {
            Ok(var.map(Pattern::Var).into())
        } else if let Some(l) = self.matches(Token::LBrace)? {
            if let Some(r) = self.matches(Token::RBrace)? {
                return Ok(l.span_with(r, Expr::RecordEmpty).into());
            }
            let (labels, r) = self.pattern_expr_inner()?;
            Ok(l.span_with(
                r.clone(),
                Expr::RecordExtend(labels, r.map(|()| Expr::RecordEmpty).into()),
            )
            .into())
        } else {
            self.expected(vec![Token::empty_ident(), Token::LBrace], "pattern expr")
        }
    }

    fn pattern_list(&mut self) -> Result<Vec<PatternAt>> {
        let mut patterns = Vec::new();
        loop {
            let pattern = self.pattern_expr()?;
            patterns.push(pattern);
            if self.matches(Token::RParen)?.is_some() {
                break;
            } else if self.matches(Token::Comma)?.is_some() {
                continue;
            } else {
                return self.expected(vec![Token::RParen, Token::Comma], "pattern list");
            }
        }
        Ok(patterns)
    }

    fn let_expr(&mut self, at: At<()>) -> Result<ExprAt> {
        let pattern = self.pattern_expr()?;
        match *pattern.expr {
            Pattern::Var(_) if self.matches(Token::LParen)?.is_some() => {
                let params = self.pattern_list()?;
                self.expect(Token::Equal, "let expr fun")?;
                let fun_body = self.expr_inner(0)?;
                let body = if self.is_repl && self.token.is_none() {
                    pattern.clone()
                } else {
                    self.expect(Token::In, "let expr fun")?;
                    self.expr_inner(0)?
                };
                let fun = ExprAt {
                    context: at.clone().into(),
                    expr: Expr::Fun(params, fun_body).into(),
                };
                let expr = Expr::Let(pattern, fun, body);
                Ok(ExprAt {
                    context: at.into(),
                    expr: expr.into(),
                })
            }
            _ => {
                self.expect(Token::Equal, "let expr")?;
                let value = self.expr_inner(0)?;
                if self.is_repl && self.token.is_none() {
                    let body = pattern.clone();
                    Ok(ExprAt {
                        context: at.into(),
                        expr: Expr::Let(pattern, value, body).into(),
                    })
                } else {
                    self.expect(Token::In, "let expr")?;
                    let body = self.expr_inner(0)?;
                    Ok(ExprAt {
                        context: at.into(),
                        expr: Expr::Let(pattern, value, body).into(),
                    })
                }
            }
        }
    }

    fn fun_expr(&mut self, at: At<()>) -> Result<ExprAt> {
        self.expect(Token::LParen, "fun expr")?;
        let params = self.pattern_list()?;
        self.expect(Token::Arrow, "fun expr")?;
        let body = self.expr_inner(0)?;
        Ok(ExprAt {
            context: at.into(),
            expr: Expr::Fun(params, body).into(),
        })
    }

    /// Return is labels, rest and closing brace location
    fn record_expr_inner(&mut self) -> Result<(BTreeMap<String, ExprAt>, ExprAt, At<()>)> {
        let mut labels = BTreeMap::new();
        loop {
            let label = self.expect_ident("record expr label")?;
            let expr = if self.matches(Token::Equal)?.is_some() {
                self.expr_inner(0)?
            } else {
                label.clone().map(Expr::Var).into()
            };

            if labels.insert(label.value.clone(), expr).is_some() {
                return Err(Error::DuplicateLabel(label));
            }

            if self.matches(Token::Comma)?.is_some() {
                continue;
            } else if let Some(r) = self.matches(Token::RBrace)? {
                let rest = ExprAt {
                    context: r.clone().into(),
                    expr: Expr::RecordEmpty.into(),
                };
                return Ok((labels, rest, r));
            } else if self.matches(Token::Pipe)?.is_some() {
                let rest = self.expr_inner(0)?;
                let r = self.expect(Token::RBrace, "record expr rest")?;
                return Ok((labels, rest, r));
            } else {
                return self.expected(
                    vec![Token::Pipe, Token::Comma, Token::RBrace],
                    "record expr",
                );
            }
        }
    }

    fn record_expr(&mut self, l: At<()>) -> Result<ExprAt> {
        if let Some(r) = self.matches(Token::RBrace)? {
            return Ok(l.span_with(r, Expr::RecordEmpty).into());
        }
        let (labels, rest, r) = self.record_expr_inner()?;
        Ok(ExprAt {
            context: l.span_with(r, ()).into(),
            expr: Expr::RecordExtend(labels, rest).into(),
        })
    }

    fn variant_expr(&mut self, at: At<()>) -> Result<ExprAt> {
        let label = self.expect_ident("variant expr")?;
        let expr = self.expr_inner(0)?;
        let label_only = label.value.clone();
        let context = at.span_with(label, ()).into();
        Ok(ExprAt {
            context,
            expr: Expr::Variant(label_only, expr).into(),
        })
    }

    fn match_expr(&mut self, at: At<()>) -> Result<ExprAt> {
        let expr = self.expr_inner(0)?;
        self.expect(Token::LBrace, "match expr")?;
        let mut cases = Vec::new();
        let mut default_case = None;
        loop {
            if self.matches(Token::Colon)?.is_some() {
                let variant = self.expect_ident("match expr case variant")?;
                let var = self.expect_ident("match expr case value")?;
                self.expect(Token::Arrow, "match expr case")?;
                let expr = self.expr_inner(0)?;
                cases.push((variant.value, var.value, expr));
            } else if let Some(var) = self.matches_ident()? {
                self.expect(Token::Arrow, "match expr default case")?;
                let expr = self.expr_inner(0)?;
                default_case = Some((var.value, expr));
                self.expect(Token::RBrace, "match expr default case")?;
                break;
            } else {
                return self.expected(vec![Token::Colon, Token::empty_ident()], "match expr case");
            }
            if self.matches(Token::Comma)?.is_some() {
                continue;
            } else if self.matches(Token::RBrace)?.is_some() {
                break;
            } else {
                return self.expected(vec![Token::Comma, Token::RBrace], "match expr");
            }
        }
        Ok(ExprAt {
            context: at.into(),
            expr: Expr::Case(expr, cases, default_case).into(),
        })
    }

    fn if_expr(&mut self, at: At<()>) -> Result<ExprAt> {
        let if_expr = self.expr_inner(0)?;
        self.expect(Token::Then, "if expr")?;
        let if_body = self.expr_inner(0)?;
        let mut elifs = Vec::new();
        loop {
            if self.matches(Token::Else)?.is_some() {
                break;
            } else if self.matches(Token::Elif)?.is_some() {
                let elif_expr = self.expr_inner(0)?;
                self.expect(Token::Then, "if expr elif")?;
                let elif_body = self.expr_inner(0)?;
                elifs.push((elif_expr, elif_body));
            } else {
                return self.expected(vec![Token::Elif, Token::Else], "if expr elif");
            }
        }
        let else_body = self.expr_inner(0)?;
        Ok(ExprAt {
            context: at.into(),
            expr: Expr::If(if_expr, if_body, elifs, else_body).into(),
        })
    }

    fn expr_lhs(&mut self) -> Result<ExprAt> {
        if let Some(var) = self.matches_ident()? {
            Ok(var.map(Expr::Var).into())
        } else if let Some(i) = self.matches_int()? {
            Ok(i.map(Expr::Int).into())
        } else if let Some(at) = self.matches(Token::True)? {
            Ok(at.map(|()| Expr::Bool(true)).into())
        } else if let Some(at) = self.matches(Token::False)? {
            Ok(at.map(|()| Expr::Bool(false)).into())
        } else if self.matches(Token::LParen)?.is_some() {
            self.paren_expr()
        } else if let Some(at) = self.matches(Token::Let)? {
            self.let_expr(at)
        } else if let Some(at) = self.matches(Token::Fun)? {
            self.fun_expr(at)
        } else if let Some(l) = self.matches(Token::LBrace)? {
            self.record_expr(l)
        } else if let Some(at) = self.matches(Token::Colon)? {
            self.variant_expr(at)
        } else if let Some(at) = self.matches(Token::Match)? {
            self.match_expr(at)
        } else if let Some(at) = self.matches(Token::If)? {
            self.if_expr(at)
        } else {
            let r_bp = self.prefix_bp()?;
            if let Some(at) = self.matches(Token::Negate)? {
                let rhs = self.expr_inner(r_bp)?;
                Ok(at.map(|()| Expr::Negate(rhs)).into())
            } else {
                Err(Error::InvalidPrefix(self.token.take()))
            }
        }
    }

    fn call_expr(&mut self, lhs: ExprAt) -> Result<ExprAt> {
        let mut args = vec![self.expr_inner(0)?];
        loop {
            if self.matches(Token::RParen)?.is_some() {
                break;
            } else if self.matches(Token::Comma)?.is_some() {
            } else {
                return self.expected(vec![Token::Comma, Token::RParen], "call expr");
            }
            let arg = self.expr_inner(0)?;
            args.push(arg);
        }
        Ok(ExprAt {
            context: lhs.context.clone(),
            expr: Expr::Call(lhs, args).into(),
        })
    }

    fn record_select_expr(&mut self, lhs: ExprAt) -> Result<ExprAt> {
        let field = self.expect_ident("record select expr")?;
        Ok(field.map(|field| Expr::RecordSelect(lhs, field)).into())
    }

    fn record_restrict_expr(&mut self, lhs: ExprAt) -> Result<ExprAt> {
        let field = self.expect_ident("record restrict expr")?;
        Ok(field.map(|field| Expr::RecordRestrict(lhs, field)).into())
    }

    fn unwrap_expr(&mut self, lhs: ExprAt, at: At<()>) -> Result<ExprAt> {
        Ok(at.map(|()| Expr::Unwrap(lhs)).into())
    }

    fn expr_postfix(&mut self, lhs: ExprAt) -> Result<ExprAt> {
        if self.matches(Token::LParen)?.is_some() {
            self.call_expr(lhs)
        } else if self.matches(Token::Dot)?.is_some() {
            self.record_select_expr(lhs)
        } else if self.matches(Token::Backslash)?.is_some() {
            self.record_restrict_expr(lhs)
        } else if let Some(at) = self.matches(Token::QuestionMark)? {
            self.unwrap_expr(lhs, at)
        } else {
            self.expected(
                vec![Token::LParen, Token::Dot, Token::Backslash],
                "expr postfix",
            )
        }
    }

    fn binop_expr(&mut self, at: At<()>, op: IntBinOp, lhs: ExprAt, r_bp: u8) -> Result<ExprAt> {
        let rhs = self.expr_inner(r_bp)?;
        Ok(at.map(|()| Expr::IntBinOp(op, lhs, rhs)).into())
    }

    fn expr_infix(&mut self, lhs: ExprAt, r_bp: u8) -> Result<ExprAt> {
        if let Some(at) = self.matches(Token::Plus)? {
            self.binop_expr(at, IntBinOp::Plus, lhs, r_bp)
        } else if let Some(at) = self.matches(Token::Minus)? {
            self.binop_expr(at, IntBinOp::Minus, lhs, r_bp)
        } else if let Some(at) = self.matches(Token::Multiply)? {
            self.binop_expr(at, IntBinOp::Multiply, lhs, r_bp)
        } else if let Some(at) = self.matches(Token::Divide)? {
            self.binop_expr(at, IntBinOp::Divide, lhs, r_bp)
        } else if let Some(at) = self.matches(Token::EqualEqual)? {
            let rhs = self.expr_inner(r_bp)?;
            Ok(at.map(|()| Expr::EqualEqual(lhs, rhs)).into())
        } else if let Some(at) = self.matches(Token::LessThan)? {
            self.binop_expr(at, IntBinOp::LessThan, lhs, r_bp)
        } else if let Some(at) = self.matches(Token::LessThanOrEqual)? {
            self.binop_expr(at, IntBinOp::LessThanOrEqual, lhs, r_bp)
        } else if let Some(at) = self.matches(Token::GreaterThan)? {
            self.binop_expr(at, IntBinOp::GreaterThan, lhs, r_bp)
        } else if let Some(at) = self.matches(Token::GreaterThanOrEqual)? {
            self.binop_expr(at, IntBinOp::GreaterThanOrEqual, lhs, r_bp)
        } else {
            self.expected(
                vec![
                    Token::Plus,
                    Token::Minus,
                    Token::Multiply,
                    Token::Divide,
                    Token::EqualEqual,
                    Token::LessThan,
                    Token::LessThanOrEqual,
                    Token::GreaterThan,
                    Token::GreaterThanOrEqual,
                ],
                "expr infix",
            )
        }
    }

    fn expr_inner(&mut self, min_bp: u8) -> Result<ExprAt> {
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
            Some(Token::Negate) => Ok(10),
            None => Err(Error::UnexpectedEof),
            _ => Err(Error::InvalidPrefix(self.token.clone())),
        }
    }

    // TODO: Not sure how to determine precedence
    fn postfix_bp(&self) -> Option<u8> {
        match self.token {
            Some(Token::LParen) => Some(11),
            Some(Token::Dot) => Some(13),
            Some(Token::Backslash) => Some(12),
            Some(Token::QuestionMark) => Some(9),
            _ => None,
        }
    }

    fn infix_bp(&self) -> Option<(u8, u8)> {
        match self.token {
            Some(Token::EqualEqual) => Some((2, 1)),
            Some(Token::Plus) | Some(Token::Minus) => Some((5, 6)),
            Some(Token::Multiply) | Some(Token::Divide) => Some((7, 8)),
            Some(Token::LessThan)
            | Some(Token::LessThanOrEqual)
            | Some(Token::GreaterThan)
            | Some(Token::GreaterThanOrEqual) => Some((3, 4)),
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
                if name.value.len() == 2 && name.value.starts_with('r') {
                    row_vars.insert(name.value, Constraints::new());
                } else if name.value.len() == 1 {
                    vars.insert(name.value);
                } else {
                    return Err(Error::InvalidTypeVarName(name));
                }
            } else if self.matches(Token::Dot)?.is_some() {
                self.expect(Token::LParen, "forall constraints")?;
                'outer: loop {
                    let row_name = self.expect_ident("forall constraints name")?;
                    let row_constraints = row_vars
                        .get_mut(&row_name.value)
                        .ok_or_else(|| Error::NoRowForConstraints(row_name.clone()))?;
                    if !row_constraints.is_empty() {
                        return Err(Error::RowConstraintsAlreadyDefined(row_name));
                    }
                    'inner: loop {
                        self.expect(Token::Backslash, "forall constraints label")?;
                        let label = self.expect_ident("forall constraints label")?;
                        row_constraints.insert(label.value);
                        if self.matches(Token::RParen)?.is_some() {
                            break 'outer;
                        } else if self.matches(Token::Comma)?.is_some() {
                            break 'inner;
                        }
                    }
                }
                self.expect(Token::FatArrow, "forall constraints")?;
                return Ok(ForAll { vars, row_vars });
            } else if self.matches(Token::FatArrow)?.is_some() {
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

        let forall = if parser.matches(Token::ForAll)?.is_some() {
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
        let mut ty = if self.matches(Token::LParen)?.is_some() {
            self.paren_ty()?
        } else if let Some(name) = self.matches_ident()? {
            Type::Const(name.value)
        } else if self.matches(Token::LBrace)?.is_some() {
            self.record_ty()?
        } else if self.matches(Token::LBracket)?.is_some() {
            self.variant_ty()?
        } else {
            return self.expected(vec![Token::LParen, Token::empty_ident()], "ty");
        };

        if self.matches(Token::LBracket)?.is_some() {
            let mut args = Vec::new();
            loop {
                let arg = self.ty_inner()?;
                args.push(arg);
                if self.matches(Token::RBracket)?.is_some() {
                    break;
                }
                self.expect(Token::Comma, "ty app")?;
            }
            ty = Type::App(ty.into(), args);
        }

        if self.matches(Token::Arrow)?.is_some() {
            let ret = self.ty_inner()?;
            ty = Type::Arrow(vec![ty], ret.into());
        }

        Ok(ty)
    }

    fn record_ty(&mut self) -> Result<Type> {
        if self.matches(Token::RBrace)?.is_some() {
            return Ok(Type::Record(Type::RowEmpty.into()));
        }
        let mut labels = BTreeMap::new();
        let mut rest = Type::RowEmpty;
        loop {
            let label = self.expect_ident("record ty label")?;
            if labels.is_empty() && self.matches(Token::RBrace)?.is_some() {
                return Ok(Type::Record(Type::Const(label.value).into()));
            }
            self.expect(Token::Colon, "record ty")?;
            let ty = self.ty_inner()?;
            if labels.insert(label.value.clone(), ty).is_some() {
                return Err(Error::DuplicateLabel(label));
            }
            if self.matches(Token::Comma)?.is_some() {
                continue;
            } else if self.matches(Token::RBrace)?.is_some() {
                break;
            } else if self.matches(Token::Pipe)?.is_some() {
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
            if labels.insert(label.value.clone(), ty).is_some() {
                return Err(Error::DuplicateLabel(label));
            }
            if self.matches(Token::Comma)?.is_some() {
                continue;
            } else if self.matches(Token::RBracket)?.is_some() {
                break;
            } else if self.matches(Token::Pipe)?.is_some() {
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
            if self.matches(Token::RParen)?.is_some() {
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
