use crate::CompileError;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Int,
    Return,
    If,
    Else,
    While,
    Do,
    For,
    Char,
    Identifier(String),
    IntLiteral(i64),
    StringLiteral(String),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Assign,
    EqualEqual,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    AmpAmp,
    PipePipe,
    Tilde,
    Bang,
    Question,
    Colon,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Comma,
    Ampersand,
    OpenBracket,
    CloseBracket,
    Struct,
    Union,
    Dot,
    Arrow,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub col: usize,
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, CompileError> {
    let mut tokens = Vec::new();
    let mut chars = source.chars().peekable();
    let mut line = 1;
    let mut col = 1;

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\t' | '\r' => {
                chars.next();
                col += 1;
            }
            '\n' => {
                chars.next();
                line += 1;
                col = 1;
            }
            '+' => {
                tokens.push(Token { kind: TokenKind::Plus, line, col });
                chars.next();
                col += 1;
            }
            '-' => {
                let start_col = col;
                chars.next();
                col += 1;
                if chars.peek() == Some(&'>') {
                    chars.next();
                    col += 1;
                    tokens.push(Token { kind: TokenKind::Arrow, line, col: start_col });
                } else {
                    tokens.push(Token { kind: TokenKind::Minus, line, col: start_col });
                }
            }
            '*' => {
                tokens.push(Token { kind: TokenKind::Star, line, col });
                chars.next();
                col += 1;
            }
            '/' => {
                tokens.push(Token { kind: TokenKind::Slash, line, col });
                chars.next();
                col += 1;
            }
            '%' => {
                tokens.push(Token { kind: TokenKind::Percent, line, col });
                chars.next();
                col += 1;
            }
            '=' => {
                let start_col = col;
                chars.next();
                col += 1;
                if chars.peek() == Some(&'=') {
                    chars.next();
                    col += 1;
                    tokens.push(Token { kind: TokenKind::EqualEqual, line, col: start_col });
                } else {
                    tokens.push(Token { kind: TokenKind::Assign, line, col: start_col });
                }
            }
            '!' => {
                let start_col = col;
                chars.next();
                col += 1;
                if chars.peek() == Some(&'=') {
                    chars.next();
                    col += 1;
                    tokens.push(Token { kind: TokenKind::BangEqual, line, col: start_col });
                } else {
                    tokens.push(Token { kind: TokenKind::Bang, line, col: start_col });
                }
            }
            '<' => {
                let start_col = col;
                chars.next();
                col += 1;
                if chars.peek() == Some(&'=') {
                    chars.next();
                    col += 1;
                    tokens.push(Token { kind: TokenKind::LessEqual, line, col: start_col });
                } else {
                    tokens.push(Token { kind: TokenKind::Less, line, col: start_col });
                }
            }
            '>' => {
                let start_col = col;
                chars.next();
                col += 1;
                if chars.peek() == Some(&'=') {
                    chars.next();
                    col += 1;
                    tokens.push(Token { kind: TokenKind::GreaterEqual, line, col: start_col });
                } else {
                    tokens.push(Token { kind: TokenKind::Greater, line, col: start_col });
                }
            }
            '&' => {
                let start_col = col;
                chars.next();
                col += 1;
                if chars.peek() == Some(&'&') {
                    chars.next();
                    col += 1;
                    tokens.push(Token { kind: TokenKind::AmpAmp, line, col: start_col });
                } else {
                    tokens.push(Token { kind: TokenKind::Ampersand, line, col: start_col });
                }
            }
            '|' => {
                let start_col = col;
                chars.next();
                col += 1;
                if chars.peek() == Some(&'|') {
                    chars.next();
                    col += 1;
                    tokens.push(Token { kind: TokenKind::PipePipe, line, col: start_col });
                } else {
                    return Err(CompileError {
                        message: "unexpected character: '|'".to_string(),
                        line,
                        col: start_col,
                    });
                }
            }
            '~' => {
                tokens.push(Token { kind: TokenKind::Tilde, line, col });
                chars.next();
                col += 1;
            }
            '?' => {
                tokens.push(Token { kind: TokenKind::Question, line, col });
                chars.next();
                col += 1;
            }
            ':' => {
                tokens.push(Token { kind: TokenKind::Colon, line, col });
                chars.next();
                col += 1;
            }
            '(' => {
                tokens.push(Token { kind: TokenKind::OpenParen, line, col });
                chars.next();
                col += 1;
            }
            ')' => {
                tokens.push(Token { kind: TokenKind::CloseParen, line, col });
                chars.next();
                col += 1;
            }
            '{' => {
                tokens.push(Token { kind: TokenKind::OpenBrace, line, col });
                chars.next();
                col += 1;
            }
            '}' => {
                tokens.push(Token { kind: TokenKind::CloseBrace, line, col });
                chars.next();
                col += 1;
            }
            ';' => {
                tokens.push(Token { kind: TokenKind::Semicolon, line, col });
                chars.next();
                col += 1;
            }
            ',' => {
                tokens.push(Token { kind: TokenKind::Comma, line, col });
                chars.next();
                col += 1;
            }
            '[' => {
                tokens.push(Token { kind: TokenKind::OpenBracket, line, col });
                chars.next();
                col += 1;
            }
            ']' => {
                tokens.push(Token { kind: TokenKind::CloseBracket, line, col });
                chars.next();
                col += 1;
            }
            '.' => {
                tokens.push(Token { kind: TokenKind::Dot, line, col });
                chars.next();
                col += 1;
            }
            '"' => {
                let start_col = col;
                chars.next();
                col += 1;
                let mut string = String::new();
                loop {
                    match chars.next() {
                        Some('"') => {
                            col += 1;
                            break;
                        }
                        Some('\\') => {
                            col += 1;
                            match chars.next() {
                                Some('n') => { string.push('\n'); col += 1; }
                                Some('t') => { string.push('\t'); col += 1; }
                                Some('\\') => { string.push('\\'); col += 1; }
                                Some('"') => { string.push('"'); col += 1; }
                                Some('0') => { string.push('\0'); col += 1; }
                                Some(c) => {
                                    return Err(CompileError {
                                        message: format!("unknown escape sequence: \\{c}"),
                                        line,
                                        col: col - 1,
                                    });
                                }
                                None => {
                                    return Err(CompileError {
                                        message: "unterminated string literal".to_string(),
                                        line,
                                        col: start_col,
                                    });
                                }
                            }
                        }
                        Some('\n') | None => {
                            return Err(CompileError {
                                message: "unterminated string literal".to_string(),
                                line,
                                col: start_col,
                            });
                        }
                        Some(c) => {
                            string.push(c);
                            col += 1;
                        }
                    }
                }
                tokens.push(Token { kind: TokenKind::StringLiteral(string), line, col: start_col });
            }
            '0'..='9' => {
                let start_col = col;
                let mut num_str = String::new();
                while let Some(&d) = chars.peek() {
                    if d.is_ascii_digit() {
                        num_str.push(d);
                        chars.next();
                        col += 1;
                    } else {
                        break;
                    }
                }
                let value: i64 = num_str.parse().map_err(|_| CompileError {
                    message: format!("invalid integer literal: {num_str}"),
                    line,
                    col: start_col,
                })?;
                tokens.push(Token { kind: TokenKind::IntLiteral(value), line, col: start_col });
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let start_col = col;
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        ident.push(c);
                        chars.next();
                        col += 1;
                    } else {
                        break;
                    }
                }
                let kind = match ident.as_str() {
                    "int" => TokenKind::Int,
                    "char" => TokenKind::Char,
                    "return" => TokenKind::Return,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "while" => TokenKind::While,
                    "do" => TokenKind::Do,
                    "for" => TokenKind::For,
                    "struct" => TokenKind::Struct,
                    "union" => TokenKind::Union,
                    _ => TokenKind::Identifier(ident),
                };
                tokens.push(Token { kind, line, col: start_col });
            }
            _ => {
                return Err(CompileError {
                    message: format!("unexpected character: '{ch}'"),
                    line,
                    col,
                });
            }
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_return_42() {
        let tokens = tokenize("int main() { return 42; }").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Int,
                &TokenKind::Identifier("main".to_string()),
                &TokenKind::OpenParen,
                &TokenKind::CloseParen,
                &TokenKind::OpenBrace,
                &TokenKind::Return,
                &TokenKind::IntLiteral(42),
                &TokenKind::Semicolon,
                &TokenKind::CloseBrace,
            ]
        );
    }

    #[test]
    fn tokenize_comparison_operators() {
        let tokens = tokenize("== != < <= > >=").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::EqualEqual,
                &TokenKind::BangEqual,
                &TokenKind::Less,
                &TokenKind::LessEqual,
                &TokenKind::Greater,
                &TokenKind::GreaterEqual,
            ]
        );
    }

    #[test]
    fn tokenize_logical_operators() {
        let tokens = tokenize("&& ||").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(kinds, vec![&TokenKind::AmpAmp, &TokenKind::PipePipe]);
    }

    #[test]
    fn tokenize_ternary() {
        let tokens = tokenize("x ? 1 : 0").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Identifier("x".to_string()),
                &TokenKind::Question,
                &TokenKind::IntLiteral(1),
                &TokenKind::Colon,
                &TokenKind::IntLiteral(0),
            ]
        );
    }

    #[test]
    fn tokenize_control_flow_keywords() {
        let tokens = tokenize("if else while do for").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::If,
                &TokenKind::Else,
                &TokenKind::While,
                &TokenKind::Do,
                &TokenKind::For,
            ]
        );
    }

    #[test]
    fn tokenize_assign_vs_equal() {
        let tokens = tokenize("x = 1 == 2").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Identifier("x".to_string()),
                &TokenKind::Assign,
                &TokenKind::IntLiteral(1),
                &TokenKind::EqualEqual,
                &TokenKind::IntLiteral(2),
            ]
        );
    }

    #[test]
    fn tokenize_bang_vs_bangequal() {
        let tokens = tokenize("!x != y").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Bang,
                &TokenKind::Identifier("x".to_string()),
                &TokenKind::BangEqual,
                &TokenKind::Identifier("y".to_string()),
            ]
        );
    }

    #[test]
    fn tokenize_tracks_position() {
        let tokens = tokenize("int\nmain").unwrap();
        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[0].col, 1);
        assert_eq!(tokens[1].line, 2);
        assert_eq!(tokens[1].col, 1);
    }

    #[test]
    fn tokenize_address_of() {
        let tokens = tokenize("&x").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(kinds, vec![&TokenKind::Ampersand, &TokenKind::Identifier("x".to_string())]);
    }

    #[test]
    fn tokenize_ampersand_vs_ampamp() {
        let tokens = tokenize("&x && y").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Ampersand,
                &TokenKind::Identifier("x".to_string()),
                &TokenKind::AmpAmp,
                &TokenKind::Identifier("y".to_string()),
            ]
        );
    }

    #[test]
    fn tokenize_brackets() {
        let tokens = tokenize("arr[0]").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Identifier("arr".to_string()),
                &TokenKind::OpenBracket,
                &TokenKind::IntLiteral(0),
                &TokenKind::CloseBracket,
            ]
        );
    }

    #[test]
    fn tokenize_rejects_invalid_char() {
        let err = tokenize("@").unwrap_err();
        assert_eq!(err.line, 1);
        assert_eq!(err.col, 1);
        assert!(err.message.contains("unexpected character"));
    }

    #[test]
    fn tokenize_string_literal() {
        let tokens = tokenize("\"hello\"").unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::StringLiteral("hello".to_string()));
    }

    #[test]
    fn tokenize_string_escape_sequences() {
        let tokens = tokenize("\"a\\nb\\tc\\\\d\\\"e\\0f\"").unwrap();
        assert_eq!(
            tokens[0].kind,
            TokenKind::StringLiteral("a\nb\tc\\d\"e\0f".to_string())
        );
    }

    #[test]
    fn tokenize_empty_string() {
        let tokens = tokenize("\"\"").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::StringLiteral(String::new()));
    }

    #[test]
    fn tokenize_unterminated_string() {
        let err = tokenize("\"hello").unwrap_err();
        assert!(err.message.contains("unterminated"));
    }

    #[test]
    fn tokenize_struct_keyword() {
        let tokens = tokenize("struct Point").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Struct,
                &TokenKind::Identifier("Point".to_string()),
            ]
        );
    }

    #[test]
    fn tokenize_union_keyword() {
        let tokens = tokenize("union Data").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Union,
                &TokenKind::Identifier("Data".to_string()),
            ]
        );
    }

    #[test]
    fn tokenize_dot_operator() {
        let tokens = tokenize("p.x").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Identifier("p".to_string()),
                &TokenKind::Dot,
                &TokenKind::Identifier("x".to_string()),
            ]
        );
    }

    #[test]
    fn tokenize_arrow_operator() {
        let tokens = tokenize("p->x").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Identifier("p".to_string()),
                &TokenKind::Arrow,
                &TokenKind::Identifier("x".to_string()),
            ]
        );
    }

    #[test]
    fn tokenize_minus_vs_arrow() {
        let tokens = tokenize("a - b->c").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Identifier("a".to_string()),
                &TokenKind::Minus,
                &TokenKind::Identifier("b".to_string()),
                &TokenKind::Arrow,
                &TokenKind::Identifier("c".to_string()),
            ]
        );
    }

    #[test]
    fn tokenize_char_keyword() {
        let tokens = tokenize("char *s").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Char,
                &TokenKind::Star,
                &TokenKind::Identifier("s".to_string()),
            ]
        );
    }
}
