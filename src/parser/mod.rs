use crate::CompileError;
use crate::lexer::{Token, TokenKind};

#[derive(Debug)]
pub struct Program {
    pub function: Function,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: Statement,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub enum Expression {
    IntLiteral(i64),
}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn current_location(&self) -> (usize, usize) {
        self.tokens
            .get(self.pos)
            .map(|t| (t.line, t.col))
            .unwrap_or((0, 0))
    }

    fn expect(&mut self, expected: &TokenKind) -> Result<&Token, CompileError> {
        match self.tokens.get(self.pos) {
            Some(token) if token.kind == *expected => {
                self.pos += 1;
                Ok(&self.tokens[self.pos - 1])
            }
            Some(token) => Err(CompileError {
                message: format!("expected {expected:?}, found {:?}", token.kind),
                line: token.line,
                col: token.col,
            }),
            None => {
                let (line, col) = self.current_location();
                Err(CompileError {
                    message: format!("expected {expected:?}, found end of input"),
                    line,
                    col,
                })
            }
        }
    }

    fn expect_identifier(&mut self) -> Result<String, CompileError> {
        match self.tokens.get(self.pos) {
            Some(Token { kind: TokenKind::Identifier(name), .. }) => {
                let name = name.clone();
                self.pos += 1;
                Ok(name)
            }
            Some(token) => Err(CompileError {
                message: format!("expected identifier, found {:?}", token.kind),
                line: token.line,
                col: token.col,
            }),
            None => {
                let (line, col) = self.current_location();
                Err(CompileError {
                    message: "expected identifier, found end of input".to_string(),
                    line,
                    col,
                })
            }
        }
    }

    fn parse_program(&mut self) -> Result<Program, CompileError> {
        let function = self.parse_function()?;
        if let Some(token) = self.tokens.get(self.pos) {
            return Err(CompileError {
                message: format!("unexpected token after function: {:?}", token.kind),
                line: token.line,
                col: token.col,
            });
        }
        Ok(Program { function })
    }

    fn parse_function(&mut self) -> Result<Function, CompileError> {
        self.expect(&TokenKind::Int)?;
        let name = self.expect_identifier()?;
        self.expect(&TokenKind::OpenParen)?;
        self.expect(&TokenKind::CloseParen)?;
        self.expect(&TokenKind::OpenBrace)?;
        let body = self.parse_statement()?;
        self.expect(&TokenKind::CloseBrace)?;
        Ok(Function { name, body })
    }

    fn parse_statement(&mut self) -> Result<Statement, CompileError> {
        self.expect(&TokenKind::Return)?;
        let expr = self.parse_expression()?;
        self.expect(&TokenKind::Semicolon)?;
        Ok(Statement::Return(expr))
    }

    fn parse_expression(&mut self) -> Result<Expression, CompileError> {
        match self.tokens.get(self.pos) {
            Some(Token { kind: TokenKind::IntLiteral(value), .. }) => {
                let value = *value;
                self.pos += 1;
                Ok(Expression::IntLiteral(value))
            }
            Some(token) => Err(CompileError {
                message: format!("expected expression, found {:?}", token.kind),
                line: token.line,
                col: token.col,
            }),
            None => {
                let (line, col) = self.current_location();
                Err(CompileError {
                    message: "expected expression, found end of input".to_string(),
                    line,
                    col,
                })
            }
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Program, CompileError> {
    let mut parser = Parser { tokens, pos: 0 };
    parser.parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    #[test]
    fn parse_return_0() {
        let tokens = tokenize("int main() { return 0; }").unwrap();
        let program = parse(&tokens).unwrap();
        assert_eq!(program.function.name, "main");
        let Statement::Return(Expression::IntLiteral(val)) = &program.function.body;
        assert_eq!(*val, 0);
    }

    #[test]
    fn parse_missing_semicolon() {
        let tokens = tokenize("int main() { return 42 }").unwrap();
        let err = parse(&tokens).unwrap_err();
        assert!(err.message.contains("expected Semicolon"));
    }

    #[test]
    fn parse_empty_tokens() {
        let tokens = tokenize("").unwrap();
        let err = parse(&tokens).unwrap_err();
        assert!(err.message.contains("end of input"));
    }
}
