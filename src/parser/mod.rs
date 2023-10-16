pub mod ast;
pub mod error;
pub mod operators;

use std::rc::Rc;

use crate::{
    lexer::{error::LexerError, tokens::Token, Lexer},
    source::{AmpereSource, CodeArea, CodeSpan},
};

use self::{
    ast::{ExprNode, ExprType, StmtNode, StmtType, Stmts},
    error::ParserError,
    operators::unary_prec,
};

macro_rules! list_helper {
    ($self:ident, $closing_tok:ident $code:block) => {
        while !$self.next_is(Token::$closing_tok)? {
            $code;
            if !$self.skip_tok(Token::Comma)? {
                break;
            }
        }
        $self.expect_tok(Token::$closing_tok)?;
    };

    ($self:ident, $first:ident, $closing_tok:ident $code:block) => {
        let mut $first = true;
        while !$self.next_is(Token::$closing_tok)? {
            $code;
            $first = false;
            if !$self.skip_tok(Token::Comma)? {
                break;
            }
        }
        $self.expect_tok(Token::$closing_tok)?;
    };
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    src: &'a Rc<AmpereSource>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>, src: &'a Rc<AmpereSource>) -> Self {
        Self { lexer, src }
    }

    pub fn change_next_result(
        &self,
        v: Result<Option<Token>, LexerError>,
    ) -> Result<Token, ParserError> {
        v.map(|v| v.unwrap_or(Token::Eof))
            .map_err(|v| ParserError::LexingError {
                error: v,
                area: self.make_area(self.span()),
            })
    }

    pub fn next(&mut self) -> Result<Token, ParserError> {
        let next = self.lexer.next();
        self.change_next_result(next)
    }
    pub fn peek(&self) -> Result<Token, ParserError> {
        let mut peek = self.lexer.clone();
        self.change_next_result(peek.next())
    }
    pub fn span(&self) -> CodeSpan {
        self.lexer.span()
    }
    pub fn slice(&self) -> &str {
        self.lexer.slice()
    }

    pub fn make_area(&self, span: CodeSpan) -> CodeArea {
        CodeArea {
            span,
            src: self.src.clone(),
        }
    }

    pub fn next_is(&mut self, tok: Token) -> Result<bool, ParserError> {
        let mut peek = self.lexer.clone();
        Ok(self.change_next_result(peek.next())? == tok)
    }
    pub fn next_are<const N: usize>(&mut self, toks: [Token; N]) -> Result<bool, ParserError> {
        let mut peek = self.lexer.clone();
        for tok in toks {
            if !(self.change_next_result(peek.next())? == tok) {
                return Ok(false);
            }
        }
        Ok(true)
    }
    pub fn skip_tok(&mut self, tok: Token) -> Result<bool, ParserError> {
        if self.next_is(tok)? {
            self.next()?;
            return Ok(true);
        }
        Ok(false)
    }
    pub fn expect_tok(&mut self, tok: Token) -> Result<(), ParserError> {
        let next = self.next()?;
        if next != tok {
            return Err(ParserError::UnexpectedToken {
                expected: tok.to_str(),
                found: next,
                area: self.make_area(self.span()),
            });
        }
        Ok(())
    }

    pub fn parse_unit(&mut self) -> Result<ExprNode, ParserError> {
        let t = self.next()?;
        let start = self.span();
        let unary;
        Ok(ExprNode {
            typ: match t {
                Token::Int => {
                    let value = self.slice().parse().unwrap();

                    ExprType::Int(value)
                }
                Token::Float => {
                    let value = self.slice().parse().unwrap();

                    ExprType::Float(value)
                }
                Token::True => ExprType::Bool(true),

                Token::False => ExprType::Bool(false),

                Token::String => {
                    let s = self.slice();

                    ExprType::String(s[1..(s.len() - 1)].into())
                }
                Token::Identifier => ExprType::Var(self.slice().into()),

                Token::OpenParen => {
                    let inner = self.parse_expr()?;
                    if self.skip_tok(Token::Comma)? {
                        let mut out = vec![inner];
                        if !self.skip_tok(Token::ClosedParen)? {
                            list_helper! {self, ClosedParen {
                                out.push(self.parse_expr()?);
                            }}
                        }
                        ExprType::Tuple(out)
                    } else {
                        self.expect_tok(Token::ClosedParen)?;
                        inner.typ
                    }
                }
                Token::OpenSqBracket => {
                    let mut out = vec![];
                    list_helper! {self, ClosedSqBracket {
                        out.push(self.parse_expr()?);
                    }}
                    ExprType::Array(out)
                }

                unary_op
                    if {
                        unary = unary_prec(unary_op);
                        unary.is_some()
                    } =>
                {
                    // self.next()?;
                    let unary_prec = unary.unwrap();
                    let next_prec = operators::next_infix(unary_prec);
                    let val = match next_prec {
                        Some(next_prec) => self.parse_op(next_prec)?,
                        None => self.parse_value()?,
                    };

                    ExprType::Unary(unary_op.to_unary_op().unwrap(), Box::new(val))
                }
                t => {
                    return Err(ParserError::UnexpectedToken {
                        expected: "expression",
                        found: t,
                        area: self.make_area(self.span()),
                    })
                }
            },
            span: start.extended(self.span()),
        })
    }

    pub fn parse_value(&mut self) -> Result<ExprNode, ParserError> {
        let mut out = self.parse_unit()?;
        loop {
            let prev_span = out.span;
            let typ = match self.peek()? {
                Token::OpenSqBracket => {
                    self.next()?;
                    let index = self.parse_expr()?;
                    self.expect_tok(Token::ClosedSqBracket)?;

                    ExprType::Index {
                        base: Box::new(out),
                        index: Box::new(index),
                    }
                }
                Token::Period => {
                    self.next()?;
                    self.expect_tok(Token::Identifier)?;
                    let member = self.slice();

                    ExprType::Member {
                        base: Box::new(out),
                        member: member.into(),
                    }
                }
                _ => break,
            };
            out = ExprNode {
                typ,
                span: prev_span.extended(self.span()),
            }
        }
        Ok(out)
    }

    pub fn parse_expr(&mut self) -> Result<ExprNode, ParserError> {
        self.parse_op(0)
    }

    pub fn parse_op(&mut self, prec: usize) -> Result<ExprNode, ParserError> {
        let next_prec = operators::next_infix(prec);

        let mut left = match next_prec {
            Some(next_prec) => self.parse_op(next_prec)?,
            None => self.parse_value()?,
        };

        while operators::is_infix_prec(self.peek()?, prec) {
            let op = self.next()?;

            let right = if operators::prec_type(prec) == operators::OpType::Left {
                match next_prec {
                    Some(next_prec) => self.parse_op(next_prec)?,
                    None => self.parse_value()?,
                }
            } else {
                self.parse_op(prec)?
            };
            let new_span = left.span.extended(right.span);
            left = ExprNode {
                typ: ExprType::Op(Box::new(left), op.to_bin_op().unwrap(), Box::new(right)),
                span: new_span,
            }
        }
        Ok(left)
    }

    pub fn parse_stmt(&mut self) -> Result<StmtNode, ParserError> {
        let start = self.span();

        let typ = match self.peek()? {
            Token::Let => {
                self.next()?;
                self.expect_tok(Token::Identifier)?;
                let name = self.slice().to_string();
                self.expect_tok(Token::Assign)?;
                let expr = self.parse_expr()?;
                StmtType::Let(name, expr)
            }
            _ => StmtType::Expr(self.parse_expr()?),
        };

        Ok(StmtNode {
            typ,
            span: start.extended(self.span()),
        })
    }

    pub fn parse_stmts(&mut self) -> Result<Stmts, ParserError> {
        let mut list = vec![];

        while !matches!(self.peek()?, Token::ClosedBracket | Token::Eof) {
            let stmt = self.parse_stmt()?;
            if self.skip_tok(Token::Semicolon)? {
                list.push(stmt);
            } else {
                return Ok(Stmts::Ret(list, stmt));
            }
        }

        Ok(Stmts::Normal(list))
    }

    pub fn parse(&mut self) -> Result<Stmts, ParserError> {
        let out = self.parse_stmts()?;
        self.expect_tok(Token::Eof)?;
        Ok(out)
    }
}
