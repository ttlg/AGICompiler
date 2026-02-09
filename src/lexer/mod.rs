use crate::CompileError;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Int,
    Return,
    Identifier(String),
    IntLiteral(i64),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Tilde,
    Bang,
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
            '~' => {
                tokens.push(Token { kind: TokenKind::Tilde, line, col });
                chars.next();
                col += 1;
            }
            '!' => {
                tokens.push(Token { kind: TokenKind::Bang, line, col });
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
    fn tokenize_unary_operators() {
        let tokens = tokenize("return -~!5;").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Return,
                &TokenKind::Minus,
                &TokenKind::Tilde,
                &TokenKind::Bang,
                &TokenKind::IntLiteral(5),
                &TokenKind::Semicolon,
            ]
        );
    }

    #[test]
    fn tokenize_binary_operators() {
        let tokens = tokenize("1 + 2 * 3 / 4 % 5 - 6").unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::IntLiteral(1),
                &TokenKind::Plus,
                &TokenKind::IntLiteral(2),
                &TokenKind::Star,
                &TokenKind::IntLiteral(3),
                &TokenKind::Slash,
                &TokenKind::IntLiteral(4),
                &TokenKind::Percent,
                &TokenKind::IntLiteral(5),
                &TokenKind::Minus,
                &TokenKind::IntLiteral(6),
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
