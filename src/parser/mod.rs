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
pub enum UnaryOperator {
    Negate,
    BitwiseNot,
    LogicalNot,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug)]
pub enum Expression {
    IntLiteral(i64),
    UnaryOp {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    BinaryOp {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
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
        self.parse_additive()
    }

    fn parse_additive(&mut self) -> Result<Expression, CompileError> {
        let mut left = self.parse_multiplicative()?;
        loop {
            let operator = match self.tokens.get(self.pos) {
                Some(Token { kind: TokenKind::Plus, .. }) => BinaryOperator::Add,
                Some(Token { kind: TokenKind::Minus, .. }) => BinaryOperator::Subtract,
                _ => break,
            };
            self.pos += 1;
            let right = self.parse_multiplicative()?;
            left = Expression::BinaryOp {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Expression, CompileError> {
        let mut left = self.parse_unary()?;
        loop {
            let operator = match self.tokens.get(self.pos) {
                Some(Token { kind: TokenKind::Star, .. }) => BinaryOperator::Multiply,
                Some(Token { kind: TokenKind::Slash, .. }) => BinaryOperator::Divide,
                Some(Token { kind: TokenKind::Percent, .. }) => BinaryOperator::Modulo,
                _ => break,
            };
            self.pos += 1;
            let right = self.parse_unary()?;
            left = Expression::BinaryOp {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expression, CompileError> {
        let operator = match self.tokens.get(self.pos) {
            Some(Token { kind: TokenKind::Minus, .. }) => Some(UnaryOperator::Negate),
            Some(Token { kind: TokenKind::Tilde, .. }) => Some(UnaryOperator::BitwiseNot),
            Some(Token { kind: TokenKind::Bang, .. }) => Some(UnaryOperator::LogicalNot),
            _ => None,
        };
        if let Some(operator) = operator {
            self.pos += 1;
            let operand = self.parse_unary()?;
            return Ok(Expression::UnaryOp {
                operator,
                operand: Box::new(operand),
            });
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expression, CompileError> {
        match self.tokens.get(self.pos) {
            Some(Token { kind: TokenKind::IntLiteral(value), .. }) => {
                let value = *value;
                self.pos += 1;
                Ok(Expression::IntLiteral(value))
            }
            Some(Token { kind: TokenKind::OpenParen, .. }) => {
                self.pos += 1;
                let expr = self.parse_expression()?;
                self.expect(&TokenKind::CloseParen)?;
                Ok(expr)
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
        assert!(matches!(
            program.function.body,
            Statement::Return(Expression::IntLiteral(0))
        ));
    }

    #[test]
    fn parse_unary_negate() {
        let tokens = tokenize("int main() { return -5; }").unwrap();
        let program = parse(&tokens).unwrap();
        let Statement::Return(ref expr) = program.function.body;
        assert!(matches!(
            expr,
            Expression::UnaryOp {
                operator: UnaryOperator::Negate,
                operand,
            } if matches!(**operand, Expression::IntLiteral(5))
        ));
    }

    #[test]
    fn parse_nested_unary() {
        let tokens = tokenize("int main() { return -~!5; }").unwrap();
        let program = parse(&tokens).unwrap();
        let Statement::Return(ref expr) = program.function.body;
        assert!(matches!(expr, Expression::UnaryOp { operator: UnaryOperator::Negate, .. }));
    }

    #[test]
    fn parse_parenthesized_expression() {
        let tokens = tokenize("int main() { return -(42); }").unwrap();
        let program = parse(&tokens).unwrap();
        let Statement::Return(ref expr) = program.function.body;
        assert!(matches!(
            expr,
            Expression::UnaryOp {
                operand,
                ..
            } if matches!(**operand, Expression::IntLiteral(42))
        ));
    }

    #[test]
    fn parse_binary_add() {
        let tokens = tokenize("int main() { return 1 + 2; }").unwrap();
        let program = parse(&tokens).unwrap();
        let Statement::Return(ref expr) = program.function.body;
        assert!(matches!(
            expr,
            Expression::BinaryOp { operator: BinaryOperator::Add, .. }
        ));
    }

    #[test]
    fn parse_precedence_mul_over_add() {
        let tokens = tokenize("int main() { return 1 + 2 * 3; }").unwrap();
        let program = parse(&tokens).unwrap();
        let Statement::Return(ref expr) = program.function.body;
        assert!(matches!(
            expr,
            Expression::BinaryOp {
                operator: BinaryOperator::Add,
                right,
                ..
            } if matches!(**right, Expression::BinaryOp { operator: BinaryOperator::Multiply, .. })
        ));
    }

    #[test]
    fn parse_left_associativity() {
        let tokens = tokenize("int main() { return 1 - 2 - 3; }").unwrap();
        let program = parse(&tokens).unwrap();
        let Statement::Return(ref expr) = program.function.body;
        assert!(matches!(
            expr,
            Expression::BinaryOp {
                operator: BinaryOperator::Subtract,
                left,
                ..
            } if matches!(**left, Expression::BinaryOp { operator: BinaryOperator::Subtract, .. })
        ));
    }

    #[test]
    fn parse_parens_override_precedence() {
        let tokens = tokenize("int main() { return (1 + 2) * 3; }").unwrap();
        let program = parse(&tokens).unwrap();
        let Statement::Return(ref expr) = program.function.body;
        assert!(matches!(
            expr,
            Expression::BinaryOp {
                operator: BinaryOperator::Multiply,
                left,
                ..
            } if matches!(**left, Expression::BinaryOp { operator: BinaryOperator::Add, .. })
        ));
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
