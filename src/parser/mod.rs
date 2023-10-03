mod ast;
pub mod error;
mod operators;

use std::rc::Rc;

use crate::{
    lexer::{error::LexerError, tokens::Token, Lexer},
    source::{AmpereSource, CodeArea, CodeSpan},
};

use self::{
    ast::{ExprNode, ExprType},
    error::ParserError,
    operators::unary_prec,
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    src: Rc<AmpereSource>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>, src: Rc<AmpereSource>) -> Self {
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
                    self.expect_tok(Token::ClosedParen)?;
                    inner.typ
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
        self.parse_unit()
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
}
