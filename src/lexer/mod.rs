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
    Identifier(String),
    IntLiteral(i64),
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
                tokens.push(Token { kind: TokenKind::Minus, line, col });
                chars.next();
                col += 1;
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
                    return Err(CompileError {
                        message: "unexpected character: '&'".to_string(),
                        line,
                        col: start_col,
                    });
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
                    "return" => TokenKind::Return,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "while" => TokenKind::While,
                    "do" => TokenKind::Do,
                    "for" => TokenKind::For,
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
    fn tokenize_rejects_invalid_char() {
        let err = tokenize("@").unwrap_err();
        assert_eq!(err.line, 1);
        assert_eq!(err.col, 1);
        assert!(err.message.contains("unexpected character"));
    }
}
