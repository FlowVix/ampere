pub mod ast;
pub mod error;
pub mod operators;

use std::rc::Rc;

use lasso::{Rodeo, Spur};

use crate::{
    lexer::{error::LexerError, tokens::Token, Lexer},
    source::{AmpereSource, CodeArea, CodeSpan},
};

use self::{
    ast::{
        AssignPath, AssignPatternNode, AssignPatternType, ExprNode, ExprType, LetPatternNode,
        LetPatternType, StmtNode, StmtType, Stmts,
    },
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

// #[derive(Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    src: &'a Rc<AmpereSource>,
    interner: &'a mut Rodeo,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>, src: &'a Rc<AmpereSource>, interner: &'a mut Rodeo) -> Self {
        Self {
            lexer,
            src,
            interner,
        }
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
    pub fn slice_interned(&mut self) -> Spur {
        self.lexer.slice_interned(self.interner)
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

    pub fn intern<T>(&mut self, s: T) -> Spur
    where
        T: AsRef<str>,
    {
        self.interner.get_or_intern(s)
    }

    pub fn parse_let_pattern(&mut self) -> Result<LetPatternNode, ParserError> {
        let t = self.next()?;
        let start = self.span();
        Ok(LetPatternNode {
            typ: match t {
                Token::Identifier => {
                    let name = self.slice_interned();
                    LetPatternType::Var(name)
                }
                Token::OpenSqBracket => {
                    let mut out = vec![];
                    list_helper! {self, ClosedSqBracket {
                        out.push(self.parse_let_pattern()?);
                    }}
                    LetPatternType::ArrayDestructure(out)
                }
                Token::OpenParen => {
                    let mut out = vec![];
                    list_helper! {self, ClosedParen {
                        out.push(self.parse_let_pattern()?);
                    }}
                    LetPatternType::TupleDestructure(out)
                }
                t => {
                    return Err(ParserError::UnexpectedToken {
                        expected: "pattern",
                        found: t,
                        area: self.make_area(self.span()),
                    })
                }
            },
            span: start.extended(self.span()),
        })
    }
    pub fn parse_assign_pattern(&mut self) -> Result<AssignPatternNode, ParserError> {
        let t = self.next()?;
        let start = self.span();
        Ok(AssignPatternNode {
            typ: match t {
                Token::Identifier => {
                    let name = self.slice_interned();
                    let mut path = vec![];

                    let mut out_span = start.extended(self.span());

                    loop {
                        match self.peek()? {
                            Token::OpenSqBracket => {
                                self.next()?;
                                let index = self.parse_expr()?;
                                self.expect_tok(Token::ClosedSqBracket)?;

                                path.push(AssignPath::Index(index));
                            }
                            Token::Period => {
                                self.next()?;
                                self.expect_tok(Token::Identifier)?;
                                let member = self.slice_interned();
                                path.push(AssignPath::Member(member));
                            }
                            _ => break,
                        };
                        out_span = out_span.extended(self.span());
                    }
                    return Ok(AssignPatternNode {
                        typ: AssignPatternType::Path { var: name, path },
                        span: out_span,
                    });
                }
                Token::OpenSqBracket => {
                    let mut out = vec![];
                    list_helper! {self, ClosedSqBracket {
                        out.push(self.parse_assign_pattern()?);
                    }}
                    AssignPatternType::ArrayDestructure(out)
                }
                Token::OpenParen => {
                    let mut out = vec![];
                    list_helper! {self, ClosedParen {
                        out.push(self.parse_assign_pattern()?);
                    }}
                    AssignPatternType::TupleDestructure(out)
                }
                t => {
                    return Err(ParserError::UnexpectedToken {
                        expected: "pattern",
                        found: t,
                        area: self.make_area(self.span()),
                    })
                }
            },
            span: start.extended(self.span()),
        })
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
                    let s = self.slice().to_string();
                    let s = self.intern(&s[1..(s.len() - 1)]);

                    ExprType::String(s)
                }
                Token::Identifier => ExprType::Var(self.slice_interned()),

                Token::Dbg => {
                    let v = self.parse_expr()?;

                    ExprType::Dbg(Box::new(v))
                }

                Token::OpenParen => {
                    let old_lexer = self.lexer.clone();
                    let mut depth = 1usize;
                    loop {
                        match self.next()? {
                            Token::OpenParen => {
                                depth += 1;
                            }
                            Token::ClosedParen => {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            }
                            _ => {}
                        }
                    }
                    let t = self.peek()?;
                    self.lexer = old_lexer;
                    match t {
                        Token::Arrow | Token::FatArrow => {
                            let mut params = vec![];
                            list_helper! {self, ClosedParen {
                                let pat = self.parse_let_pattern()?;
                                let typ = if self.skip_tok(Token::Colon)? {
                                    Some(self.parse_expr()?)
                                } else {
                                    None
                                };
                                params.push((pat, typ))
                            }}
                            let ret_type = if self.skip_tok(Token::Arrow)? {
                                Some(self.parse_expr()?)
                            } else {
                                None
                            }
                            .map(Box::new);
                            self.expect_tok(Token::FatArrow)?;
                            let body = Box::new(self.parse_expr()?);
                            ExprType::Function {
                                params,
                                ret_type,
                                body,
                            }
                        }
                        _ => {
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
                    }
                }
                Token::OpenSqBracket => {
                    let mut out = vec![];
                    list_helper! {self, ClosedSqBracket {
                        out.push(self.parse_expr()?);
                    }}
                    ExprType::Array(out)
                }
                Token::OpenBracket => {
                    let out = self.parse_stmts()?;
                    self.expect_tok(Token::ClosedBracket)?;
                    ExprType::Block(Box::new(out))
                }
                Token::If => {
                    let cond = Box::new(self.parse_expr()?);
                    let then = Box::new(self.parse_expr()?);
                    let otherwise = if self.skip_tok(Token::Else)? {
                        Some(Box::new(self.parse_expr()?))
                    } else {
                        None
                    };
                    ExprType::If {
                        cond,
                        then,
                        otherwise,
                    }
                }
                Token::While => {
                    let cond = Box::new(self.parse_expr()?);
                    let body = Box::new(self.parse_expr()?);
                    ExprType::While { cond, body }
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
                    let member = self.slice_interned();

                    ExprType::Member {
                        base: Box::new(out),
                        member,
                    }
                }
                Token::OpenParen => {
                    self.next()?;
                    let mut args = vec![];

                    list_helper! {self, ClosedParen {
                        args.push(self.parse_expr()?);
                    }}

                    ExprType::Call {
                        base: Box::new(out),
                        args,
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
                let pat = self.parse_let_pattern()?;
                self.expect_tok(Token::Assign)?;
                let expr = self.parse_expr()?;
                StmtType::Let(pat, expr)
            }
            _ => {
                // let mut check = self.clone();
                let old_lexer = self.lexer.clone();

                match self.parse_assign_pattern() {
                    Ok(pat) => {
                        println!("ga");
                        let tok = self.peek()?;
                        if tok == Token::Assign {
                            self.next()?;
                            let e = self.parse_expr()?;
                            StmtType::Assign(pat, e)
                        } else if let Some(op) = tok.to_assign_op() {
                            self.next()?;
                            let e = self.parse_expr()?;
                            StmtType::AssignOp(pat, op, e)
                        } else {
                            self.lexer = old_lexer;
                            let e = self.parse_expr()?;
                            StmtType::Expr(e)
                        }
                    }
                    Err(pat_err) => {
                        self.lexer = old_lexer;
                        let e = self.parse_expr()?;
                        if self.next_is(Token::Assign)? {
                            return Err(pat_err);
                        }
                        StmtType::Expr(e)
                    }
                }
            }
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
