use std::str::CharIndices;

use crate::{parser::error::ParserError, source::CodeSpan};

use self::{error::LexerError, tokens::Token};

pub mod error;
pub mod tokens;

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    src: &'a str,
    chars: std::iter::Peekable<CharIndices<'a>>,
    span: CodeSpan,
    // pos: usize,
}

fn is_identifier(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            chars: src.char_indices().peekable(),
            span: CodeSpan::ZEROSPAN,
            // pos: 0,
        }
    }
    fn next_char(&mut self) -> Option<char> {
        self.chars.next().map(|v| v.1)
    }
    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|v| v.1)
    }

    #[inline]
    pub fn span(&self) -> CodeSpan {
        self.span
    }
    #[inline]
    pub fn slice(&self) -> &str {
        &self.src[self.span.start..self.span.end]
    }

    fn update_span(&mut self) {
        self.span.end = if let Some(&(t, _)) = self.chars.peek() {
            t
        } else {
            self.src.len()
        };
    }

    fn next_token(&mut self) -> Result<Option<Token>, LexerError> {
        while let Some(&(idx, c)) = self.chars.peek() {
            if c.is_whitespace() {
                self.next_char();
                continue;
            }
            self.span.start = idx;
            break;
        }

        match self.next_char() {
            Some('+') => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Ok(Some(Token::PlusEq))
                }
                _ => Ok(Some(Token::Plus)),
            },
            Some('-') => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Ok(Some(Token::MinusEq))
                }
                Some('>') => {
                    self.next_char();
                    Ok(Some(Token::Arrow))
                }
                _ => Ok(Some(Token::Minus)),
            },
            Some('*') => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Ok(Some(Token::MultEq))
                }
                Some('*') => match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        Ok(Some(Token::PowEq))
                    }
                    _ => Ok(Some(Token::Pow)),
                },
                _ => Ok(Some(Token::Mult)),
            },
            Some('/') => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Ok(Some(Token::DivEq))
                }
                _ => Ok(Some(Token::Div)),
            },
            Some('%') => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Ok(Some(Token::ModEq))
                }
                _ => Ok(Some(Token::Mod)),
            },

            Some('(') => Ok(Some(Token::OpenParen)),
            Some(')') => Ok(Some(Token::ClosedParen)),
            Some('[') => Ok(Some(Token::OpenSqBracket)),
            Some(']') => Ok(Some(Token::ClosedSqBracket)),
            Some('{') => Ok(Some(Token::OpenBracket)),
            Some('}') => Ok(Some(Token::ClosedBracket)),
            Some(';') => Ok(Some(Token::Semicolon)),
            Some(':') => Ok(Some(Token::Colon)),
            Some('.') => Ok(Some(Token::Period)),
            Some(',') => Ok(Some(Token::Comma)),

            Some('=') => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Ok(Some(Token::Eq))
                }
                Some('>') => {
                    self.next_char();
                    Ok(Some(Token::FatArrow))
                }
                _ => Ok(Some(Token::Assign)),
            },
            Some('!') => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Ok(Some(Token::NotEq))
                }
                _ => Ok(Some(Token::Not)),
            },

            Some('>') => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Ok(Some(Token::Gte))
                }
                _ => Ok(Some(Token::Gt)),
            },
            Some('<') => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Ok(Some(Token::Lte))
                }
                _ => Ok(Some(Token::Lt)),
            },

            Some(t) => {
                if t.is_ascii_digit() {
                    let mut is_float = false;
                    loop {
                        if self.peek_char().is_some_and(|c| c.is_ascii_digit()) {
                            self.next_char();
                            continue;
                        }
                        if self.peek_char() == Some('.') {
                            self.next_char();
                            is_float = true;
                            continue;
                        }
                        break;
                    }

                    return Ok(Some(if is_float { Token::Float } else { Token::Int }));
                }
                if is_identifier(t) {
                    while self.peek_char().is_some_and(is_identifier) {
                        self.next_char();
                    }
                    self.update_span();

                    return Ok(Some(match self.slice() {
                        "true" => Token::True,
                        "false" => Token::False,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "while" => Token::While,
                        "for" => Token::For,
                        "let" => Token::Let,
                        "dbg" => Token::Dbg,
                        _ => Token::Identifier,
                    }));
                }
                if t == '"' {
                    loop {
                        let Some(next) = self.next_char() else {
                            return Err(LexerError::UnterminatedString);
                        };
                        match next {
                            '\\' => {
                                self.next_char();
                            }
                            '"' => {
                                break;
                            }
                            _ => {}
                        }
                    }
                    return Ok(Some(Token::String));
                }
                Err(LexerError::UnknownChar(t))
            }
            None => Ok(None),
        }
    }
    pub fn next(&mut self) -> Result<Option<Token>, LexerError> {
        self.span.start = self.span.end;
        let out = self.next_token();
        self.update_span();
        // println!("-> {:?} {:?}", out, self.span);
        out
    }
}
