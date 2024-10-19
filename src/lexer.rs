use std::fmt::Display;

#[derive(Debug)]
pub enum ErrorKind {
    UnterminatedString,
    UnexpectedCharacter,
    InvalidNumberFormat,
}

#[derive(Debug)]
pub struct ScanError {
    kind: ErrorKind,
    start: usize,
    end: usize,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number(f32),

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

#[derive(Debug)]
pub struct Token<'a> {
    kind: TokenKind,
    lexeme: &'a str,
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lexeme = self.lexeme;

        match self.kind {
            TokenKind::LeftParen => write!(f, "LEFT_PAREN {lexeme} null"),
            TokenKind::RightParen => write!(f, "RIGHT_PAREN {lexeme} null"),
            TokenKind::LeftBrace => write!(f, "LEFT_BRACE {lexeme} null"),
            TokenKind::RightBrace => write!(f, "RIGHT_BRACE {lexeme} null"),
            TokenKind::Comma => write!(f, "COMMA {lexeme} null"),
            TokenKind::Dot => write!(f, "DOT {lexeme} null"),
            TokenKind::Minus => write!(f, "MINUS {lexeme} null"),
            TokenKind::Plus => write!(f, "PLUS {lexeme} null"),
            TokenKind::Semicolon => write!(f, "SEMICOLON {lexeme} null"),
            TokenKind::Slash => write!(f, "SLASH {lexeme} null"),
            TokenKind::Star => write!(f, "STAR {lexeme} null"),
            TokenKind::Bang => write!(f, "BANG {lexeme} null"),
            TokenKind::BangEqual => write!(f, "BANG_EQUAL {lexeme} null"),
            TokenKind::Equal => write!(f, "EQUAL {lexeme} null"),
            TokenKind::EqualEqual => write!(f, "EQUAL_EQUAL {lexeme} null"),
            TokenKind::Greater => write!(f, "GREATER {lexeme} null"),
            TokenKind::GreaterEqual => write!(f, "GREATER_EQUAL {lexeme} null"),
            TokenKind::Less => write!(f, "LESS {lexeme} null"),
            TokenKind::LessEqual => write!(f, "LESS_EQUAL {lexeme} null"),
            TokenKind::Identifier => write!(f, "IDENTIFIER {lexeme} null"),
            TokenKind::String => write!(f, "STRING {lexeme} {}", lexeme.trim_matches('"')),
            TokenKind::Number(literal) => write!(f, "NUMBER {lexeme} {:?}", literal),
            TokenKind::And => write!(f, "AND {lexeme} null"),
            TokenKind::Class => write!(f, "CLASS {lexeme} null"),
            TokenKind::Else => write!(f, "ELSE {lexeme} null"),
            TokenKind::False => write!(f, "FALSE {lexeme} null"),
            TokenKind::Fun => write!(f, "FUN {lexeme} null"),
            TokenKind::For => write!(f, "FOR {lexeme} null"),
            TokenKind::If => write!(f, "IF {lexeme} null"),
            TokenKind::Nil => write!(f, "NIL {lexeme} null"),
            TokenKind::Or => write!(f, "OR {lexeme} null"),
            TokenKind::Print => write!(f, "PRINT {lexeme} null"),
            TokenKind::Return => write!(f, "RETURN {lexeme} null"),
            TokenKind::Super => write!(f, "SUPER {lexeme} null"),
            TokenKind::This => write!(f, "THIS {lexeme} null"),
            TokenKind::True => write!(f, "TRUE {lexeme} null"),
            TokenKind::Var => write!(f, "VAR {lexeme} null"),
            TokenKind::While => write!(f, "WHILE {lexeme} null"),
        }
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Lexer { source, pos: 0 }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, ScanError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut lexemes = self.source.char_indices().skip(self.pos);

        loop {
            let (position, character) = lexemes.next()?;

            let token = match character {
                // Ignored characters.
                ' ' | '\r' | '\t' | '\n' => {
                    continue;
                }
                // P1 - Resolve punctuator characters.
                '(' => Some(Ok(Token {
                    kind: TokenKind::LeftParen,
                    lexeme: &self.source[position..position + 1],
                })),
                ')' => Some(Ok(Token {
                    kind: TokenKind::RightParen,
                    lexeme: &self.source[position..position + 1],
                })),
                '{' => Some(Ok(Token {
                    kind: TokenKind::LeftBrace,
                    lexeme: &self.source[position..position + 1],
                })),
                '}' => Some(Ok(Token {
                    kind: TokenKind::RightBrace,
                    lexeme: &self.source[position..position + 1],
                })),
                ',' => Some(Ok(Token {
                    kind: TokenKind::Comma,
                    lexeme: &self.source[position..position + 1],
                })),
                '.' => Some(Ok(Token {
                    kind: TokenKind::Dot,
                    lexeme: &self.source[position..position + 1],
                })),
                '-' => Some(Ok(Token {
                    kind: TokenKind::Minus,
                    lexeme: &self.source[position..position + 1],
                })),
                '+' => Some(Ok(Token {
                    kind: TokenKind::Plus,
                    lexeme: &self.source[position..position + 1],
                })),
                ';' => Some(Ok(Token {
                    kind: TokenKind::Semicolon,
                    lexeme: &self.source[position..position + 1],
                })),
                '*' => Some(Ok(Token {
                    kind: TokenKind::Star,
                    lexeme: &self.source[position..position + 1],
                })),
                '/' => match lexemes.next() {
                    Some((_, '/')) => {
                        for (_, inner) in lexemes.by_ref() {
                            if inner == '\n' {
                                break;
                            }
                        }

                        continue;
                    }
                    _ => Some(Ok(Token {
                        kind: TokenKind::Slash,
                        lexeme: &self.source[position..position + 1],
                    })),
                },
                '!' => match lexemes.next() {
                    Some((_, '=')) => Some(Ok(Token {
                        kind: TokenKind::BangEqual,
                        lexeme: &self.source[position..position + 2],
                    })),
                    _ => Some(Ok(Token {
                        kind: TokenKind::Bang,
                        lexeme: &self.source[position..position + 1],
                    })),
                },
                '=' => match lexemes.next() {
                    Some((_, '=')) => Some(Ok(Token {
                        kind: TokenKind::EqualEqual,
                        lexeme: &self.source[position..position + 2],
                    })),
                    _ => Some(Ok(Token {
                        kind: TokenKind::Equal,
                        lexeme: &self.source[position..position + 1],
                    })),
                },
                '>' => match lexemes.next() {
                    Some((_, '=')) => Some(Ok(Token {
                        kind: TokenKind::GreaterEqual,
                        lexeme: &self.source[position..position + 2],
                    })),
                    _ => Some(Ok(Token {
                        kind: TokenKind::Greater,
                        lexeme: &self.source[position..position + 1],
                    })),
                },
                '<' => match lexemes.next() {
                    Some((_, '=')) => Some(Ok(Token {
                        kind: TokenKind::LessEqual,
                        lexeme: &self.source[position..position + 2],
                    })),
                    _ => Some(Ok(Token {
                        kind: TokenKind::Less,
                        lexeme: &self.source[position..position + 1],
                    })),
                },
                // P2 - Resolve literals.
                '"' => loop {
                    match lexemes.next() {
                        Some((next_position, c)) => match c {
                            '"' => {
                                break Some(Ok(Token {
                                    kind: TokenKind::String,
                                    lexeme: &self.source[position..next_position + 1],
                                }));
                            }
                            _ => continue,
                        },
                        _ => {
                            break Some(Err(ScanError {
                                kind: ErrorKind::UnterminatedString,
                                start: position,
                                end: self.source.len(),
                            }));
                        }
                    }
                },
                '0'..='9' => {
                    let mut is_decimal = false;

                    let next_position = loop {
                        match lexemes.next() {
                            Some((next_position, c)) => match c {
                                '0'..='9' => continue,
                                '.' if !is_decimal => {
                                    is_decimal = true;
                                    continue;
                                }
                                _ => break next_position,
                            },
                            _ => break self.source.len(),
                        }
                    };

                    let lexeme = &self.source[position..next_position].trim_matches('.');

                    match lexeme.trim().parse::<f32>() {
                        Ok(n) => Some(Ok(Token {
                            kind: TokenKind::Number(n),
                            lexeme,
                        })),
                        Err(_) => Some(Err(ScanError {
                            kind: ErrorKind::InvalidNumberFormat,
                            start: position,
                            end: next_position,
                        })),
                    }
                }
                '_' | 'a'..='z' | 'A'..='Z' => {
                    let next_position = loop {
                        match lexemes.next() {
                            Some((next_position, c)) => match c {
                                '_' | 'a'..='z' | 'A'..='Z' | '0'..='9' => continue,
                                _ => break next_position,
                            },
                            _ => break self.source.len(),
                        }
                    };

                    let lexeme = &self.source[position..next_position];

                    match lexeme {
                        "and" => Some(Ok(Token {
                            kind: TokenKind::And,
                            lexeme,
                        })),
                        "class" => Some(Ok(Token {
                            kind: TokenKind::Class,
                            lexeme,
                        })),
                        "else" => Some(Ok(Token {
                            kind: TokenKind::Else,
                            lexeme,
                        })),
                        "false" => Some(Ok(Token {
                            kind: TokenKind::False,
                            lexeme,
                        })),
                        "fun" => Some(Ok(Token {
                            kind: TokenKind::Fun,
                            lexeme,
                        })),
                        "for" => Some(Ok(Token {
                            kind: TokenKind::For,
                            lexeme,
                        })),
                        "if" => Some(Ok(Token {
                            kind: TokenKind::If,
                            lexeme,
                        })),
                        "nil" => Some(Ok(Token {
                            kind: TokenKind::Nil,
                            lexeme,
                        })),
                        "or" => Some(Ok(Token {
                            kind: TokenKind::Or,
                            lexeme,
                        })),
                        "print" => Some(Ok(Token {
                            kind: TokenKind::Print,
                            lexeme,
                        })),
                        "return" => Some(Ok(Token {
                            kind: TokenKind::Return,
                            lexeme,
                        })),
                        "super" => Some(Ok(Token {
                            kind: TokenKind::Super,
                            lexeme,
                        })),
                        "this" => Some(Ok(Token {
                            kind: TokenKind::This,
                            lexeme,
                        })),
                        "true" => Some(Ok(Token {
                            kind: TokenKind::True,
                            lexeme,
                        })),
                        "var" => Some(Ok(Token {
                            kind: TokenKind::Var,
                            lexeme,
                        })),
                        "while" => Some(Ok(Token {
                            kind: TokenKind::While,
                            lexeme,
                        })),
                        _ => Some(Ok(Token {
                            kind: TokenKind::Identifier,
                            lexeme,
                        })),
                    }
                }
                _ => Some(Err(ScanError {
                    kind: ErrorKind::UnexpectedCharacter,
                    start: position,
                    end: position + 1,
                })),
            };

            match &token {
                Some(ref result) => match result {
                    Ok(t) => self.pos = position + t.lexeme.len(),
                    Err(e) => self.pos = position + e.end,
                },
                None => self.pos += 1,
            };

            return token;
        }
    }
}

#[cfg(test)]
mod lexer_test {
    use super::*;

    #[test]
    fn strings_extracted() {
        let input = r#"
            ""
            "string"
        "#;

        let mut lexer = Lexer::new(input);

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "STRING \"\" "
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "STRING \"string\" string"
        );

        assert!(lexer.next().is_none());
    }

    #[test]
    fn punctuators_extracted() {
        let input = "(){};,+-*!===<=>=!=<>/.";

        let mut lexer = Lexer::new(input);

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "LEFT_PAREN ( null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "RIGHT_PAREN ) null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "LEFT_BRACE { null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "RIGHT_BRACE } null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "SEMICOLON ; null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "COMMA , null"
        );

        assert_eq!(format!("{}", lexer.next().unwrap().unwrap()), "PLUS + null");

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "MINUS - null"
        );

        assert_eq!(format!("{}", lexer.next().unwrap().unwrap()), "STAR * null");

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "BANG_EQUAL != null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "EQUAL_EQUAL == null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "LESS_EQUAL <= null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "GREATER_EQUAL >= null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "BANG_EQUAL != null"
        );

        assert_eq!(format!("{}", lexer.next().unwrap().unwrap()), "LESS < null");

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "GREATER > null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "SLASH / null"
        );

        assert_eq!(format!("{}", lexer.next().unwrap().unwrap()), "DOT . null");

        assert!(lexer.next().is_none());
    }

    #[test]
    fn identifiers_extracted() {
        let input = r#"
            andy formless fo _ _123 _abc ab123
            abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_
        "#;

        let mut lexer = Lexer::new(input);

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "IDENTIFIER andy null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "IDENTIFIER formless null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "IDENTIFIER fo null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "IDENTIFIER _ null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "IDENTIFIER _123 null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "IDENTIFIER _abc null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "IDENTIFIER ab123 null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "IDENTIFIER abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_ null"
        );

        assert!(lexer.next().is_none());
    }

    #[test]
    fn whitespace_ignored() {
        let input = r#"
            space    tabs				newlines




            end
        "#;

        let mut lexer = Lexer::new(input);

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "IDENTIFIER space null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "IDENTIFIER tabs null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "IDENTIFIER newlines null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "IDENTIFIER end null"
        );

        assert!(lexer.next().is_none());
    }

    #[test]
    fn numbers_extracted() {
        let input = r#"
            123
            123.456
            .456
            123.
        "#;

        let mut lexer = Lexer::new(input);

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "NUMBER 123 123.0"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "NUMBER 123.456 123.456"
        );

        assert_eq!(format!("{}", lexer.next().unwrap().unwrap()), "DOT . null");

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "NUMBER 456 456.0"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "NUMBER 123 123.0"
        );

        assert_eq!(format!("{}", lexer.next().unwrap().unwrap()), "DOT . null");

        assert!(lexer.next().is_none());
    }

    #[test]
    fn keywords_extracted() {
        let input = "and class else false for fun if nil or return super this true var while";

        let mut lexer = Lexer::new(input);

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "AND and null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "CLASS class null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "ELSE else null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "FALSE false null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "FOR for null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "FUN fun null"
        );

        assert_eq!(format!("{}", lexer.next().unwrap().unwrap()), "IF if null");

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "NIL nil null"
        );

        assert_eq!(format!("{}", lexer.next().unwrap().unwrap()), "OR or null");

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "RETURN return null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "SUPER super null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "THIS this null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "TRUE true null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "VAR var null"
        );

        assert_eq!(
            format!("{}", lexer.next().unwrap().unwrap()),
            "WHILE while null"
        );

        assert!(lexer.next().is_none());
    }
}
