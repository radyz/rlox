use std::{fmt::Display, iter::Peekable};

use crate::lexer::{Lexer, LexerError, LexerErrorKind, Token, TokenKind};

enum ParserErrorKind {
    UnterminatedString,
    UnexpectedCharacter,
    InvalidNumberFormat,
    MissingParen,
}

struct ParserError {
    kind: ParserErrorKind,
    start: usize,
    end: usize,
}

impl From<LexerError> for ParserError {
    fn from(value: LexerError) -> Self {
        ParserError {
            kind: match value.kind {
                LexerErrorKind::UnterminatedString => ParserErrorKind::UnterminatedString,
                LexerErrorKind::UnexpectedCharacter => ParserErrorKind::UnexpectedCharacter,
                LexerErrorKind::InvalidNumberFormat => ParserErrorKind::InvalidNumberFormat,
            },
            start: value.start,
            end: value.end,
        }
    }
}

#[derive(Debug)]
pub enum LiteralExpr<'a> {
    Bool(bool),
    Number(f64),
    String(&'a str),
    Nil,
}

#[derive(Debug)]
pub enum Expr<'a> {
    Binary {
        left: Box<Expr<'a>>,
        operator: Token<'a>,
        right: Box<Expr<'a>>,
    },

    Unary {
        operator: Token<'a>,
        right: Box<Expr<'a>>,
    },

    Grouping(Box<Expr<'a>>),

    Literal(LiteralExpr<'a>),

    Temp,
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", operator.lexeme, left, right),
            Expr::Unary { operator, right } => write!(f, "({} {})", operator.lexeme, right),
            Expr::Grouping(expr) => write!(f, "(group {})", expr),
            Expr::Literal(literal_expr) => match literal_expr {
                LiteralExpr::Bool(b) => write!(f, "{:?}", b),
                LiteralExpr::Number(n) => write!(f, "{:?}", n),
                LiteralExpr::String(s) => write!(f, "{}", s),
                LiteralExpr::Nil => write!(f, "nil"),
            },
            Expr::Temp => write!(f, "Where Am I!!!"),
        }
    }
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    errors: Vec<ParserError>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Parser {
            lexer: Lexer::new(source).peekable(),
            errors: Vec::<ParserError>::new(),
        }
    }

    pub fn parse(mut self) -> Expr<'a> {
        self.expression()
    }

    fn expression(&mut self) -> Expr<'a> {
        self.equality()
    }

    fn equality(&mut self) -> Expr<'a> {
        let mut expr = self.comparison();

        loop {
            let item = self.lexer.next_if(|peeked| {
                matches!(
                    peeked,
                    Ok(Token {
                        kind: TokenKind::BangEqual | TokenKind::EqualEqual,
                        ..
                    }) | Err(_)
                )
            });

            match item {
                Some(Ok(
                    operator @ Token {
                        kind: TokenKind::BangEqual | TokenKind::EqualEqual,
                        ..
                    },
                )) => {
                    let right = self.comparison();

                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    }
                }
                Some(Err(e)) => {
                    self.errors.push(e.into());
                    self.synchronize();
                    continue;
                }
                _ => break,
            }
        }

        expr
    }

    fn comparison(&mut self) -> Expr<'a> {
        let mut expr = self.term();

        loop {
            let item = self.lexer.next_if(|peeked| {
                matches!(
                    peeked,
                    Ok(Token {
                        kind: TokenKind::Greater
                            | TokenKind::GreaterEqual
                            | TokenKind::Less
                            | TokenKind::LessEqual,
                        ..
                    }) | Err(_)
                )
            });

            match item {
                Some(Ok(
                    operator @ Token {
                        kind:
                            TokenKind::Greater
                            | TokenKind::GreaterEqual
                            | TokenKind::Less
                            | TokenKind::LessEqual,
                        ..
                    },
                )) => {
                    let right = self.term();

                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    }
                }
                Some(Err(e)) => {
                    self.errors.push(e.into());
                    self.synchronize();
                    continue;
                }
                _ => break,
            }
        }

        expr
    }

    fn term(&mut self) -> Expr<'a> {
        let mut expr = self.factor();

        loop {
            let item = self.lexer.next_if(|peeked| {
                matches!(
                    peeked,
                    Ok(Token {
                        kind: TokenKind::Minus | TokenKind::Plus,
                        ..
                    }) | Err(_)
                )
            });

            match item {
                Some(Ok(
                    operator @ Token {
                        kind: TokenKind::Minus | TokenKind::Plus,
                        ..
                    },
                )) => {
                    let right = self.factor();

                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    }
                }
                Some(Err(e)) => {
                    self.errors.push(e.into());
                    self.synchronize();
                    continue;
                }
                _ => break,
            }
        }

        expr
    }

    fn factor(&mut self) -> Expr<'a> {
        let mut expr = self.unary();

        loop {
            let item = self.lexer.next_if(|peeked| {
                matches!(
                    peeked,
                    Ok(Token {
                        kind: TokenKind::Slash | TokenKind::Star,
                        ..
                    }) | Err(_)
                )
            });

            match item {
                Some(Ok(
                    operator @ Token {
                        kind: TokenKind::Slash | TokenKind::Star,
                        ..
                    },
                )) => {
                    let right = self.unary();

                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    }
                }
                Some(Err(e)) => {
                    self.errors.push(e.into());
                    self.synchronize();
                    continue;
                }
                _ => break,
            }
        }

        expr
    }

    fn unary(&mut self) -> Expr<'a> {
        let item = self.lexer.next_if(|peeked| {
            matches!(
                peeked,
                Ok(Token {
                    kind: TokenKind::Bang | TokenKind::Minus,
                    ..
                }) | Err(_)
            )
        });

        match item {
            Some(Ok(
                operator @ Token {
                    kind: TokenKind::Bang | TokenKind::Minus,
                    ..
                },
            )) => {
                let right = self.unary();

                Expr::Unary {
                    operator,
                    right: Box::new(right),
                }
            }
            Some(Err(e)) => {
                self.errors.push(e.into());

                self.synchronize()
            }
            None => self.primary(),
            _ => todo!(),
        }
    }

    fn primary(&mut self) -> Expr<'a> {
        match self.lexer.next() {
            Some(current) => match current {
                Ok(token) => match token.kind {
                    TokenKind::False => Expr::Literal(LiteralExpr::Bool(false)),
                    TokenKind::True => Expr::Literal(LiteralExpr::Bool(true)),
                    TokenKind::Nil => Expr::Literal(LiteralExpr::Nil),
                    TokenKind::Number(n) => Expr::Literal(LiteralExpr::Number(n)),
                    TokenKind::String(s) => Expr::Literal(LiteralExpr::String(s)),

                    TokenKind::LeftParen => {
                        let expr = self.expression();

                        match self.lexer.next() {
                            Some(Ok(Token {
                                kind: TokenKind::RightParen,
                                ..
                            })) => Expr::Grouping(Box::new(expr)),
                            _ => {
                                self.errors.push(ParserError {
                                    kind: ParserErrorKind::MissingParen,
                                    start: token.position,
                                    end: token.lexeme.len(),
                                });

                                self.synchronize()
                            }
                        }
                    }

                    _ => Expr::Temp,
                },
                Err(e) => {
                    self.errors.push(e.into());

                    self.synchronize()
                }
            },
            _ => Expr::Temp,
        }
    }

    fn synchronize(&mut self) -> Expr<'a> {
        Expr::Temp
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;

    #[test]
    fn parses_expression() {
        let input = "(5 - (3 - 1)) + -1";

        let parser = Parser::new(input);

        let expr = parser.parse();

        assert_eq!(
            format!("{}", expr),
            "(+ (group (- 5.0 (group (- 3.0 1.0)))) (- 1.0))"
        )
    }
}
