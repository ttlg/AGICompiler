use crate::CompileError;
use crate::lexer::{Token, TokenKind};

#[derive(Debug, Clone)]
pub enum DeclType {
    Int,
    IntPointer,
    IntArray(i64),
    CharPointer,
    Struct(String),
    StructPointer(String),
    Union(String),
    UnionPointer(String),
}

#[derive(Debug, Clone)]
pub enum GlobalInit {
    Integer(i64),
    StringLiteral(String),
}

#[derive(Debug)]
pub struct GlobalDecl {
    pub decl_type: DeclType,
    pub name: String,
    pub initializer: Option<GlobalInit>,
}

#[derive(Debug, Clone)]
pub enum MemberType {
    Int,
    IntPointer,
    CharPointer,
}

#[derive(Debug)]
pub struct TypeDef {
    pub name: String,
    pub members: Vec<(MemberType, String)>,
    pub is_union: bool,
}

#[derive(Debug)]
pub struct Program {
    pub type_defs: Vec<TypeDef>,
    pub globals: Vec<GlobalDecl>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<(DeclType, String)>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Declaration {
        decl_type: DeclType,
        name: String,
        initializer: Option<Expression>,
    },
    Expression(Expression),
    Compound(Vec<Statement>),
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
    },
    DoWhile {
        body: Box<Statement>,
        condition: Expression,
    },
    For {
        init: Option<Box<Statement>>,
        condition: Option<Expression>,
        post: Option<Expression>,
        body: Box<Statement>,
    },
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
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug)]
pub enum Expression {
    IntLiteral(i64),
    StringLiteral(String),
    Variable(String),
    UnaryOp {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    BinaryOp {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Assignment {
        target: Box<Expression>,
        value: Box<Expression>,
    },
    Ternary {
        condition: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
    FunctionCall {
        name: String,
        arguments: Vec<Expression>,
    },
    AddressOf {
        operand: Box<Expression>,
    },
    Dereference {
        operand: Box<Expression>,
    },
    ArrayIndex {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    MemberAccess {
        object: Box<Expression>,
        member: String,
    },
    ArrowAccess {
        pointer: Box<Expression>,
        member: String,
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
        let mut type_defs = Vec::new();
        let mut globals = Vec::new();
        let mut functions = Vec::new();
        while self.tokens.get(self.pos).is_some() {
            match self.tokens.get(self.pos) {
                Some(Token { kind: TokenKind::Struct, .. }) => {
                    self.pos += 1;
                    let tag_name = self.expect_identifier()?;
                    if matches!(
                        self.tokens.get(self.pos),
                        Some(Token { kind: TokenKind::OpenBrace, .. })
                    ) {
                        type_defs.push(self.parse_type_def_body(tag_name, false)?);
                    } else if matches!(
                        self.tokens.get(self.pos),
                        Some(Token { kind: TokenKind::Star, .. })
                    ) {
                        self.pos += 1;
                        let var_name = self.expect_identifier()?;
                        globals.push(self.parse_global_rest(DeclType::StructPointer(tag_name), var_name)?);
                    } else {
                        let var_name = self.expect_identifier()?;
                        self.expect(&TokenKind::Semicolon)?;
                        globals.push(GlobalDecl {
                            decl_type: DeclType::Struct(tag_name),
                            name: var_name,
                            initializer: None,
                        });
                    }
                }
                Some(Token { kind: TokenKind::Union, .. }) => {
                    self.pos += 1;
                    let tag_name = self.expect_identifier()?;
                    if matches!(
                        self.tokens.get(self.pos),
                        Some(Token { kind: TokenKind::OpenBrace, .. })
                    ) {
                        type_defs.push(self.parse_type_def_body(tag_name, true)?);
                    } else if matches!(
                        self.tokens.get(self.pos),
                        Some(Token { kind: TokenKind::Star, .. })
                    ) {
                        self.pos += 1;
                        let var_name = self.expect_identifier()?;
                        globals.push(self.parse_global_rest(DeclType::UnionPointer(tag_name), var_name)?);
                    } else {
                        let var_name = self.expect_identifier()?;
                        self.expect(&TokenKind::Semicolon)?;
                        globals.push(GlobalDecl {
                            decl_type: DeclType::Union(tag_name),
                            name: var_name,
                            initializer: None,
                        });
                    }
                }
                Some(Token { kind: TokenKind::Char, .. }) => {
                    self.pos += 1;
                    self.expect(&TokenKind::Star)?;
                    let name = self.expect_identifier()?;
                    if matches!(
                        self.tokens.get(self.pos),
                        Some(Token { kind: TokenKind::OpenParen, .. })
                    ) {
                        functions.push(self.parse_function_body(name)?);
                    } else {
                        globals.push(self.parse_global_rest(DeclType::CharPointer, name)?);
                    }
                }
                _ => {
                    self.expect(&TokenKind::Int)?;
                    if matches!(
                        self.tokens.get(self.pos),
                        Some(Token { kind: TokenKind::Star, .. })
                    ) {
                        self.pos += 1;
                        let name = self.expect_identifier()?;
                        if matches!(
                            self.tokens.get(self.pos),
                            Some(Token { kind: TokenKind::OpenParen, .. })
                        ) {
                            functions.push(self.parse_function_body(name)?);
                        } else {
                            globals.push(self.parse_global_rest(DeclType::IntPointer, name)?);
                        }
                    } else {
                        let name = self.expect_identifier()?;
                        if matches!(
                            self.tokens.get(self.pos),
                            Some(Token { kind: TokenKind::OpenParen, .. })
                        ) {
                            functions.push(self.parse_function_body(name)?);
                        } else if matches!(
                            self.tokens.get(self.pos),
                            Some(Token { kind: TokenKind::OpenBracket, .. })
                        ) {
                            self.pos += 1;
                            let size = match self.tokens.get(self.pos) {
                                Some(Token { kind: TokenKind::IntLiteral(n), .. }) => {
                                    let n = *n;
                                    self.pos += 1;
                                    n
                                }
                                _ => {
                                    let (line, col) = self.current_location();
                                    return Err(CompileError {
                                        message: "expected array size".to_string(),
                                        line,
                                        col,
                                    });
                                }
                            };
                            self.expect(&TokenKind::CloseBracket)?;
                            self.expect(&TokenKind::Semicolon)?;
                            globals.push(GlobalDecl {
                                decl_type: DeclType::IntArray(size),
                                name,
                                initializer: None,
                            });
                        } else {
                            globals.push(self.parse_global_rest(DeclType::Int, name)?);
                        }
                    }
                }
            }
        }
        if functions.is_empty() && globals.is_empty() {
            let (line, col) = self.current_location();
            return Err(CompileError {
                message: "expected declaration, found end of input".to_string(),
                line,
                col,
            });
        }
        Ok(Program { type_defs, globals, functions })
    }

    fn parse_type_def_body(
        &mut self,
        name: String,
        is_union: bool,
    ) -> Result<TypeDef, CompileError> {
        self.expect(&TokenKind::OpenBrace)?;
        let mut members = Vec::new();
        while !matches!(
            self.tokens.get(self.pos),
            Some(Token { kind: TokenKind::CloseBrace, .. })
        ) {
            let member_type = self.parse_member_type()?;
            let member_name = self.expect_identifier()?;
            self.expect(&TokenKind::Semicolon)?;
            members.push((member_type, member_name));
        }
        self.expect(&TokenKind::CloseBrace)?;
        self.expect(&TokenKind::Semicolon)?;
        Ok(TypeDef { name, members, is_union })
    }

    fn parse_member_type(&mut self) -> Result<MemberType, CompileError> {
        if matches!(
            self.tokens.get(self.pos),
            Some(Token { kind: TokenKind::Char, .. })
        ) {
            self.pos += 1;
            self.expect(&TokenKind::Star)?;
            Ok(MemberType::CharPointer)
        } else {
            self.expect(&TokenKind::Int)?;
            if matches!(
                self.tokens.get(self.pos),
                Some(Token { kind: TokenKind::Star, .. })
            ) {
                self.pos += 1;
                Ok(MemberType::IntPointer)
            } else {
                Ok(MemberType::Int)
            }
        }
    }

    fn parse_global_rest(
        &mut self,
        decl_type: DeclType,
        name: String,
    ) -> Result<GlobalDecl, CompileError> {
        let initializer = if matches!(
            self.tokens.get(self.pos),
            Some(Token { kind: TokenKind::Assign, .. })
        ) {
            self.pos += 1;
            match self.tokens.get(self.pos) {
                Some(Token { kind: TokenKind::IntLiteral(n), .. }) => {
                    let n = *n;
                    self.pos += 1;
                    Some(GlobalInit::Integer(n))
                }
                Some(Token { kind: TokenKind::Minus, .. }) => {
                    self.pos += 1;
                    match self.tokens.get(self.pos) {
                        Some(Token { kind: TokenKind::IntLiteral(n), .. }) => {
                            let n = *n;
                            self.pos += 1;
                            Some(GlobalInit::Integer(-n))
                        }
                        _ => {
                            let (line, col) = self.current_location();
                            return Err(CompileError {
                                message: "expected constant initializer".to_string(),
                                line,
                                col,
                            });
                        }
                    }
                }
                Some(Token { kind: TokenKind::StringLiteral(s), .. }) => {
                    let s = s.clone();
                    self.pos += 1;
                    Some(GlobalInit::StringLiteral(s))
                }
                _ => {
                    let (line, col) = self.current_location();
                    return Err(CompileError {
                        message: "expected constant initializer".to_string(),
                        line,
                        col,
                    });
                }
            }
        } else {
            None
        };
        self.expect(&TokenKind::Semicolon)?;
        Ok(GlobalDecl { decl_type, name, initializer })
    }

    fn parse_param_type(&mut self) -> Result<DeclType, CompileError> {
        if matches!(
            self.tokens.get(self.pos),
            Some(Token { kind: TokenKind::Char, .. })
        ) {
            self.pos += 1;
            self.expect(&TokenKind::Star)?;
            Ok(DeclType::CharPointer)
        } else if matches!(
            self.tokens.get(self.pos),
            Some(Token { kind: TokenKind::Struct, .. })
        ) {
            self.pos += 1;
            let tag_name = self.expect_identifier()?;
            self.expect(&TokenKind::Star)?;
            Ok(DeclType::StructPointer(tag_name))
        } else if matches!(
            self.tokens.get(self.pos),
            Some(Token { kind: TokenKind::Union, .. })
        ) {
            self.pos += 1;
            let tag_name = self.expect_identifier()?;
            self.expect(&TokenKind::Star)?;
            Ok(DeclType::UnionPointer(tag_name))
        } else {
            self.expect(&TokenKind::Int)?;
            if matches!(
                self.tokens.get(self.pos),
                Some(Token { kind: TokenKind::Star, .. })
            ) {
                self.pos += 1;
                Ok(DeclType::IntPointer)
            } else {
                Ok(DeclType::Int)
            }
        }
    }

    fn parse_function_body(&mut self, name: String) -> Result<Function, CompileError> {
        self.expect(&TokenKind::OpenParen)?;
        let mut params = Vec::new();
        if !matches!(
            self.tokens.get(self.pos),
            Some(Token { kind: TokenKind::CloseParen, .. })
        ) {
            let decl_type = self.parse_param_type()?;
            params.push((decl_type, self.expect_identifier()?));
            while matches!(
                self.tokens.get(self.pos),
                Some(Token { kind: TokenKind::Comma, .. })
            ) {
                self.pos += 1;
                let decl_type = self.parse_param_type()?;
                params.push((decl_type, self.expect_identifier()?));
            }
        }
        self.expect(&TokenKind::CloseParen)?;
        self.expect(&TokenKind::OpenBrace)?;
        let mut body = Vec::new();
        while let Some(token) = self.tokens.get(self.pos) {
            if token.kind == TokenKind::CloseBrace {
                break;
            }
            body.push(self.parse_statement()?);
        }
        self.expect(&TokenKind::CloseBrace)?;
        Ok(Function { name, params, body })
    }

    fn parse_statement(&mut self) -> Result<Statement, CompileError> {
        match self.tokens.get(self.pos) {
            Some(Token { kind: TokenKind::Return, .. }) => {
                self.pos += 1;
                let expr = self.parse_expression()?;
                self.expect(&TokenKind::Semicolon)?;
                Ok(Statement::Return(expr))
            }
            Some(Token { kind: TokenKind::Struct, .. }) => {
                self.pos += 1;
                let tag_name = self.expect_identifier()?;
                if matches!(
                    self.tokens.get(self.pos),
                    Some(Token { kind: TokenKind::Star, .. })
                ) {
                    self.pos += 1;
                    let var_name = self.expect_identifier()?;
                    let initializer = if matches!(
                        self.tokens.get(self.pos),
                        Some(Token { kind: TokenKind::Assign, .. })
                    ) {
                        self.pos += 1;
                        Some(self.parse_expression()?)
                    } else {
                        None
                    };
                    self.expect(&TokenKind::Semicolon)?;
                    Ok(Statement::Declaration {
                        decl_type: DeclType::StructPointer(tag_name),
                        name: var_name,
                        initializer,
                    })
                } else {
                    let var_name = self.expect_identifier()?;
                    self.expect(&TokenKind::Semicolon)?;
                    Ok(Statement::Declaration {
                        decl_type: DeclType::Struct(tag_name),
                        name: var_name,
                        initializer: None,
                    })
                }
            }
            Some(Token { kind: TokenKind::Union, .. }) => {
                self.pos += 1;
                let tag_name = self.expect_identifier()?;
                if matches!(
                    self.tokens.get(self.pos),
                    Some(Token { kind: TokenKind::Star, .. })
                ) {
                    self.pos += 1;
                    let var_name = self.expect_identifier()?;
                    let initializer = if matches!(
                        self.tokens.get(self.pos),
                        Some(Token { kind: TokenKind::Assign, .. })
                    ) {
                        self.pos += 1;
                        Some(self.parse_expression()?)
                    } else {
                        None
                    };
                    self.expect(&TokenKind::Semicolon)?;
                    Ok(Statement::Declaration {
                        decl_type: DeclType::UnionPointer(tag_name),
                        name: var_name,
                        initializer,
                    })
                } else {
                    let var_name = self.expect_identifier()?;
                    self.expect(&TokenKind::Semicolon)?;
                    Ok(Statement::Declaration {
                        decl_type: DeclType::Union(tag_name),
                        name: var_name,
                        initializer: None,
                    })
                }
            }
            Some(Token { kind: TokenKind::Char, .. }) => {
                self.pos += 1;
                self.expect(&TokenKind::Star)?;
                let name = self.expect_identifier()?;
                let initializer = if matches!(
                    self.tokens.get(self.pos),
                    Some(Token { kind: TokenKind::Assign, .. })
                ) {
                    self.pos += 1;
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                self.expect(&TokenKind::Semicolon)?;
                Ok(Statement::Declaration { decl_type: DeclType::CharPointer, name, initializer })
            }
            Some(Token { kind: TokenKind::Int, .. }) => {
                self.pos += 1;
                if matches!(
                    self.tokens.get(self.pos),
                    Some(Token { kind: TokenKind::Star, .. })
                ) {
                    self.pos += 1;
                    let name = self.expect_identifier()?;
                    let initializer = if matches!(
                        self.tokens.get(self.pos),
                        Some(Token { kind: TokenKind::Assign, .. })
                    ) {
                        self.pos += 1;
                        Some(self.parse_expression()?)
                    } else {
                        None
                    };
                    self.expect(&TokenKind::Semicolon)?;
                    Ok(Statement::Declaration { decl_type: DeclType::IntPointer, name, initializer })
                } else {
                    let name = self.expect_identifier()?;
                    if matches!(
                        self.tokens.get(self.pos),
                        Some(Token { kind: TokenKind::OpenBracket, .. })
                    ) {
                        self.pos += 1;
                        let size = match self.tokens.get(self.pos) {
                            Some(Token { kind: TokenKind::IntLiteral(n), .. }) => {
                                let n = *n;
                                self.pos += 1;
                                n
                            }
                            _ => {
                                let (line, col) = self.current_location();
                                return Err(CompileError {
                                    message: "expected array size".to_string(),
                                    line,
                                    col,
                                });
                            }
                        };
                        self.expect(&TokenKind::CloseBracket)?;
                        self.expect(&TokenKind::Semicolon)?;
                        Ok(Statement::Declaration {
                            decl_type: DeclType::IntArray(size),
                            name,
                            initializer: None,
                        })
                    } else {
                        let initializer = if matches!(
                            self.tokens.get(self.pos),
                            Some(Token { kind: TokenKind::Assign, .. })
                        ) {
                            self.pos += 1;
                            Some(self.parse_expression()?)
                        } else {
                            None
                        };
                        self.expect(&TokenKind::Semicolon)?;
                        Ok(Statement::Declaration { decl_type: DeclType::Int, name, initializer })
                    }
                }
            }
            Some(Token { kind: TokenKind::If, .. }) => {
                self.pos += 1;
                self.expect(&TokenKind::OpenParen)?;
                let condition = self.parse_expression()?;
                self.expect(&TokenKind::CloseParen)?;
                let then_branch = Box::new(self.parse_statement()?);
                let else_branch = if matches!(
                    self.tokens.get(self.pos),
                    Some(Token { kind: TokenKind::Else, .. })
                ) {
                    self.pos += 1;
                    Some(Box::new(self.parse_statement()?))
                } else {
                    None
                };
                Ok(Statement::If { condition, then_branch, else_branch })
            }
            Some(Token { kind: TokenKind::While, .. }) => {
                self.pos += 1;
                self.expect(&TokenKind::OpenParen)?;
                let condition = self.parse_expression()?;
                self.expect(&TokenKind::CloseParen)?;
                let body = Box::new(self.parse_statement()?);
                Ok(Statement::While { condition, body })
            }
            Some(Token { kind: TokenKind::Do, .. }) => {
                self.pos += 1;
                let body = Box::new(self.parse_statement()?);
                self.expect(&TokenKind::While)?;
                self.expect(&TokenKind::OpenParen)?;
                let condition = self.parse_expression()?;
                self.expect(&TokenKind::CloseParen)?;
                self.expect(&TokenKind::Semicolon)?;
                Ok(Statement::DoWhile { body, condition })
            }
            Some(Token { kind: TokenKind::For, .. }) => {
                self.pos += 1;
                self.expect(&TokenKind::OpenParen)?;
                let init = if matches!(
                    self.tokens.get(self.pos),
                    Some(Token { kind: TokenKind::Semicolon, .. })
                ) {
                    self.pos += 1;
                    None
                } else {
                    Some(Box::new(self.parse_statement()?))
                };
                let condition = if matches!(
                    self.tokens.get(self.pos),
                    Some(Token { kind: TokenKind::Semicolon, .. })
                ) {
                    None
                } else {
                    Some(self.parse_expression()?)
                };
                self.expect(&TokenKind::Semicolon)?;
                let post = if matches!(
                    self.tokens.get(self.pos),
                    Some(Token { kind: TokenKind::CloseParen, .. })
                ) {
                    None
                } else {
                    Some(self.parse_expression()?)
                };
                self.expect(&TokenKind::CloseParen)?;
                let body = Box::new(self.parse_statement()?);
                Ok(Statement::For { init, condition, post, body })
            }
            Some(Token { kind: TokenKind::OpenBrace, .. }) => {
                self.pos += 1;
                let mut stmts = Vec::new();
                while !matches!(
                    self.tokens.get(self.pos),
                    Some(Token { kind: TokenKind::CloseBrace, .. })
                ) {
                    if self.tokens.get(self.pos).is_none() {
                        let (line, col) = self.current_location();
                        return Err(CompileError {
                            message: "expected CloseBrace, found end of input".to_string(),
                            line,
                            col,
                        });
                    }
                    stmts.push(self.parse_statement()?);
                }
                self.pos += 1;
                Ok(Statement::Compound(stmts))
            }
            Some(_) => {
                let expr = self.parse_expression()?;
                self.expect(&TokenKind::Semicolon)?;
                Ok(Statement::Expression(expr))
            }
            None => {
                let (line, col) = self.current_location();
                Err(CompileError {
                    message: "expected statement, found end of input".to_string(),
                    line,
                    col,
                })
            }
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, CompileError> {
        let expr = self.parse_ternary()?;
        if matches!(
            self.tokens.get(self.pos),
            Some(Token { kind: TokenKind::Assign, .. })
        ) {
            self.pos += 1;
            let value = self.parse_expression()?;
            return Ok(Expression::Assignment {
                target: Box::new(expr),
                value: Box::new(value),
            });
        }
        Ok(expr)
    }

    fn parse_ternary(&mut self) -> Result<Expression, CompileError> {
        let condition = self.parse_logical_or()?;
        if matches!(self.tokens.get(self.pos), Some(Token { kind: TokenKind::Question, .. })) {
            self.pos += 1;
            let then_expr = self.parse_expression()?;
            self.expect(&TokenKind::Colon)?;
            let else_expr = self.parse_ternary()?;
            Ok(Expression::Ternary {
                condition: Box::new(condition),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            })
        } else {
            Ok(condition)
        }
    }

    fn parse_logical_or(&mut self) -> Result<Expression, CompileError> {
        let mut left = self.parse_logical_and()?;
        while matches!(self.tokens.get(self.pos), Some(Token { kind: TokenKind::PipePipe, .. })) {
            self.pos += 1;
            let right = self.parse_logical_and()?;
            left = Expression::BinaryOp {
                operator: BinaryOperator::LogicalOr,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_logical_and(&mut self) -> Result<Expression, CompileError> {
        let mut left = self.parse_equality()?;
        while matches!(self.tokens.get(self.pos), Some(Token { kind: TokenKind::AmpAmp, .. })) {
            self.pos += 1;
            let right = self.parse_equality()?;
            left = Expression::BinaryOp {
                operator: BinaryOperator::LogicalAnd,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Expression, CompileError> {
        let mut left = self.parse_relational()?;
        loop {
            let operator = match self.tokens.get(self.pos) {
                Some(Token { kind: TokenKind::EqualEqual, .. }) => BinaryOperator::Equal,
                Some(Token { kind: TokenKind::BangEqual, .. }) => BinaryOperator::NotEqual,
                _ => break,
            };
            self.pos += 1;
            let right = self.parse_relational()?;
            left = Expression::BinaryOp {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_relational(&mut self) -> Result<Expression, CompileError> {
        let mut left = self.parse_additive()?;
        loop {
            let operator = match self.tokens.get(self.pos) {
                Some(Token { kind: TokenKind::Less, .. }) => BinaryOperator::LessThan,
                Some(Token { kind: TokenKind::LessEqual, .. }) => BinaryOperator::LessEqual,
                Some(Token { kind: TokenKind::Greater, .. }) => BinaryOperator::GreaterThan,
                Some(Token { kind: TokenKind::GreaterEqual, .. }) => BinaryOperator::GreaterEqual,
                _ => break,
            };
            self.pos += 1;
            let right = self.parse_additive()?;
            left = Expression::BinaryOp {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
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
        match self.tokens.get(self.pos) {
            Some(Token { kind: TokenKind::Minus, .. }) => {
                self.pos += 1;
                Ok(Expression::UnaryOp {
                    operator: UnaryOperator::Negate,
                    operand: Box::new(self.parse_unary()?),
                })
            }
            Some(Token { kind: TokenKind::Tilde, .. }) => {
                self.pos += 1;
                Ok(Expression::UnaryOp {
                    operator: UnaryOperator::BitwiseNot,
                    operand: Box::new(self.parse_unary()?),
                })
            }
            Some(Token { kind: TokenKind::Bang, .. }) => {
                self.pos += 1;
                Ok(Expression::UnaryOp {
                    operator: UnaryOperator::LogicalNot,
                    operand: Box::new(self.parse_unary()?),
                })
            }
            Some(Token { kind: TokenKind::Star, .. }) => {
                self.pos += 1;
                Ok(Expression::Dereference {
                    operand: Box::new(self.parse_unary()?),
                })
            }
            Some(Token { kind: TokenKind::Ampersand, .. }) => {
                self.pos += 1;
                Ok(Expression::AddressOf {
                    operand: Box::new(self.parse_unary()?),
                })
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Expression, CompileError> {
        let mut expr = self.parse_primary()?;
        loop {
            if matches!(
                self.tokens.get(self.pos),
                Some(Token { kind: TokenKind::OpenBracket, .. })
            ) {
                self.pos += 1;
                let index = self.parse_expression()?;
                self.expect(&TokenKind::CloseBracket)?;
                expr = Expression::ArrayIndex {
                    array: Box::new(expr),
                    index: Box::new(index),
                };
            } else if matches!(
                self.tokens.get(self.pos),
                Some(Token { kind: TokenKind::Dot, .. })
            ) {
                self.pos += 1;
                let member = self.expect_identifier()?;
                expr = Expression::MemberAccess {
                    object: Box::new(expr),
                    member,
                };
            } else if matches!(
                self.tokens.get(self.pos),
                Some(Token { kind: TokenKind::Arrow, .. })
            ) {
                self.pos += 1;
                let member = self.expect_identifier()?;
                expr = Expression::ArrowAccess {
                    pointer: Box::new(expr),
                    member,
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expression, CompileError> {
        match self.tokens.get(self.pos) {
            Some(Token { kind: TokenKind::IntLiteral(value), .. }) => {
                let value = *value;
                self.pos += 1;
                Ok(Expression::IntLiteral(value))
            }
            Some(Token { kind: TokenKind::StringLiteral(s), .. }) => {
                let s = s.clone();
                self.pos += 1;
                Ok(Expression::StringLiteral(s))
            }
            Some(Token { kind: TokenKind::Identifier(name), .. }) => {
                let name = name.clone();
                self.pos += 1;
                if matches!(
                    self.tokens.get(self.pos),
                    Some(Token { kind: TokenKind::OpenParen, .. })
                ) {
                    self.pos += 1;
                    let mut arguments = Vec::new();
                    if !matches!(
                        self.tokens.get(self.pos),
                        Some(Token { kind: TokenKind::CloseParen, .. })
                    ) {
                        arguments.push(self.parse_expression()?);
                        while matches!(
                            self.tokens.get(self.pos),
                            Some(Token { kind: TokenKind::Comma, .. })
                        ) {
                            self.pos += 1;
                            arguments.push(self.parse_expression()?);
                        }
                    }
                    self.expect(&TokenKind::CloseParen)?;
                    Ok(Expression::FunctionCall { name, arguments })
                } else {
                    Ok(Expression::Variable(name))
                }
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

    fn parse_source(source: &str) -> Program {
        let tokens = tokenize(source).unwrap();
        parse(&tokens).unwrap()
    }

    #[test]
    fn parse_return_0() {
        let program = parse_source("int main() { return 0; }");
        assert_eq!(program.functions[0].name, "main");
        assert_eq!(program.functions[0].body.len(), 1);
        assert!(matches!(
            program.functions[0].body[0],
            Statement::Return(Expression::IntLiteral(0))
        ));
    }

    #[test]
    fn parse_unary_negate() {
        let program = parse_source("int main() { return -5; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
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
        let program = parse_source("int main() { return -~!5; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(expr, Expression::UnaryOp { operator: UnaryOperator::Negate, .. }));
    }

    #[test]
    fn parse_parenthesized_expression() {
        let program = parse_source("int main() { return -(42); }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
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
        let program = parse_source("int main() { return 1 + 2; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(
            expr,
            Expression::BinaryOp { operator: BinaryOperator::Add, .. }
        ));
    }

    #[test]
    fn parse_precedence_mul_over_add() {
        let program = parse_source("int main() { return 1 + 2 * 3; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
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
        let program = parse_source("int main() { return 1 - 2 - 3; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
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
        let program = parse_source("int main() { return (1 + 2) * 3; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
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
    fn parse_declaration_with_init() {
        let program = parse_source("int main() { int x = 5; return x; }");
        assert_eq!(program.functions[0].body.len(), 2);
        assert!(matches!(
            &program.functions[0].body[0],
            Statement::Declaration { name, initializer: Some(Expression::IntLiteral(5)), .. }
            if name == "x"
        ));
    }

    #[test]
    fn parse_declaration_without_init() {
        let program = parse_source("int main() { int x; return 0; }");
        assert!(matches!(
            &program.functions[0].body[0],
            Statement::Declaration { name, initializer: None, .. }
            if name == "x"
        ));
    }

    #[test]
    fn parse_assignment_expression() {
        let program = parse_source("int main() { int x; x = 10; return x; }");
        assert_eq!(program.functions[0].body.len(), 3);
        let Statement::Expression(ref expr) = program.functions[0].body[1] else { panic!() };
        assert!(matches!(
            expr,
            Expression::Assignment { target, .. }
            if matches!(target.as_ref(), Expression::Variable(n) if n == "x")
        ));
    }

    #[test]
    fn parse_chained_assignment() {
        let program = parse_source("int main() { int a; int b; a = b = 5; return a; }");
        let Statement::Expression(ref expr) = program.functions[0].body[2] else { panic!() };
        assert!(matches!(
            expr,
            Expression::Assignment {
                target,
                value,
            } if matches!(target.as_ref(), Expression::Variable(n) if n == "a")
              && matches!(value.as_ref(), Expression::Assignment { .. })
        ));
    }

    #[test]
    fn parse_variable_in_expression() {
        let program = parse_source("int main() { int x = 3; return x + 1; }");
        let Statement::Return(ref expr) = program.functions[0].body[1] else { panic!() };
        assert!(matches!(
            expr,
            Expression::BinaryOp {
                operator: BinaryOperator::Add,
                left,
                ..
            } if matches!(**left, Expression::Variable(ref n) if n == "x")
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

    #[test]
    fn parse_if_statement() {
        let program = parse_source("int main() { int x = 0; if (1) x = 1; return x; }");
        assert!(matches!(&program.functions[0].body[1], Statement::If { else_branch: None, .. }));
    }

    #[test]
    fn parse_if_else_statement() {
        let program = parse_source("int main() { int x; if (0) x = 1; else x = 2; return x; }");
        assert!(matches!(&program.functions[0].body[1], Statement::If { else_branch: Some(_), .. }));
    }

    #[test]
    fn parse_while_statement() {
        let program = parse_source("int main() { while (1) return 0; }");
        assert!(matches!(&program.functions[0].body[0], Statement::While { .. }));
    }

    #[test]
    fn parse_do_while_statement() {
        let program = parse_source("int main() { do return 0; while (1); }");
        assert!(matches!(&program.functions[0].body[0], Statement::DoWhile { .. }));
    }

    #[test]
    fn parse_for_statement() {
        let program = parse_source("int main() { for (int i = 0; i < 10; i = i + 1) return 0; }");
        assert!(matches!(&program.functions[0].body[0], Statement::For { .. }));
    }

    #[test]
    fn parse_for_empty_init() {
        let program = parse_source("int main() { for (; 1; ) return 0; }");
        assert!(matches!(
            &program.functions[0].body[0],
            Statement::For { init: None, condition: Some(_), post: None, .. }
        ));
    }

    #[test]
    fn parse_for_all_empty() {
        let program = parse_source("int main() { for (;;) return 0; }");
        assert!(matches!(
            &program.functions[0].body[0],
            Statement::For { init: None, condition: None, post: None, .. }
        ));
    }

    #[test]
    fn parse_compound_statement() {
        let program = parse_source("int main() { { return 0; } }");
        assert!(matches!(&program.functions[0].body[0], Statement::Compound(_)));
    }

    #[test]
    fn parse_ternary_expression() {
        let program = parse_source("int main() { return 1 ? 2 : 3; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(expr, Expression::Ternary { .. }));
    }

    #[test]
    fn parse_comparison_equal() {
        let program = parse_source("int main() { return 1 == 2; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(expr, Expression::BinaryOp { operator: BinaryOperator::Equal, .. }));
    }

    #[test]
    fn parse_comparison_not_equal() {
        let program = parse_source("int main() { return 1 != 2; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(expr, Expression::BinaryOp { operator: BinaryOperator::NotEqual, .. }));
    }

    #[test]
    fn parse_relational_operators() {
        let program = parse_source("int main() { return 1 < 2; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(expr, Expression::BinaryOp { operator: BinaryOperator::LessThan, .. }));
    }

    #[test]
    fn parse_logical_and() {
        let program = parse_source("int main() { return 1 && 2; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(expr, Expression::BinaryOp { operator: BinaryOperator::LogicalAnd, .. }));
    }

    #[test]
    fn parse_logical_or() {
        let program = parse_source("int main() { return 1 || 2; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(expr, Expression::BinaryOp { operator: BinaryOperator::LogicalOr, .. }));
    }

    #[test]
    fn parse_logical_and_lower_than_equality() {
        let program = parse_source("int main() { return 1 == 2 && 3 == 4; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(
            expr,
            Expression::BinaryOp {
                operator: BinaryOperator::LogicalAnd,
                left,
                right,
            } if matches!(**left, Expression::BinaryOp { operator: BinaryOperator::Equal, .. })
              && matches!(**right, Expression::BinaryOp { operator: BinaryOperator::Equal, .. })
        ));
    }

    #[test]
    fn parse_logical_or_lower_than_and() {
        let program = parse_source("int main() { return 1 || 2 && 3; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(
            expr,
            Expression::BinaryOp {
                operator: BinaryOperator::LogicalOr,
                right,
                ..
            } if matches!(**right, Expression::BinaryOp { operator: BinaryOperator::LogicalAnd, .. })
        ));
    }

    #[test]
    fn parse_ternary_right_associative() {
        let program = parse_source("int main() { return 1 ? 2 : 3 ? 4 : 5; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(
            expr,
            Expression::Ternary {
                else_expr,
                ..
            } if matches!(**else_expr, Expression::Ternary { .. })
        ));
    }

    #[test]
    fn parse_function_call_no_args() {
        let program = parse_source("int main() { return foo(); }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(
            expr,
            Expression::FunctionCall { name, arguments }
            if name == "foo" && arguments.is_empty()
        ));
    }

    #[test]
    fn parse_function_call_with_args() {
        let program = parse_source("int main() { return add(1, 2); }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(
            expr,
            Expression::FunctionCall { name, arguments }
            if name == "add" && arguments.len() == 2
        ));
    }

    #[test]
    fn parse_function_with_params() {
        let program = parse_source("int add(int a, int b) { return a + b; }");
        assert_eq!(program.functions[0].name, "add");
        assert_eq!(program.functions[0].params.len(), 2);
    }

    #[test]
    fn parse_multiple_functions() {
        let program = parse_source("int foo() { return 1; } int main() { return foo(); }");
        assert_eq!(program.functions.len(), 2);
        assert_eq!(program.functions[0].name, "foo");
        assert_eq!(program.functions[1].name, "main");
    }

    #[test]
    fn parse_function_call_as_argument() {
        let program = parse_source("int main() { return f(g(1)); }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(
            expr,
            Expression::FunctionCall { name, arguments }
            if name == "f" && arguments.len() == 1
                && matches!(&arguments[0], Expression::FunctionCall { name: inner, .. } if inner == "g")
        ));
    }

    #[test]
    fn parse_function_call_in_expression() {
        let program = parse_source("int main() { return f() + 1; }");
        let Statement::Return(ref expr) = program.functions[0].body[0] else { panic!() };
        assert!(matches!(
            expr,
            Expression::BinaryOp {
                operator: BinaryOperator::Add,
                left,
                ..
            } if matches!(**left, Expression::FunctionCall { .. })
        ));
    }

    #[test]
    fn parse_pointer_declaration() {
        let program = parse_source("int main() { int *p; return 0; }");
        assert!(matches!(
            &program.functions[0].body[0],
            Statement::Declaration { decl_type: DeclType::IntPointer, name, initializer: None }
            if name == "p"
        ));
    }

    #[test]
    fn parse_pointer_declaration_with_init() {
        let program = parse_source("int main() { int x; int *p = &x; return 0; }");
        assert!(matches!(
            &program.functions[0].body[1],
            Statement::Declaration { decl_type: DeclType::IntPointer, name, initializer: Some(_) }
            if name == "p"
        ));
    }

    #[test]
    fn parse_array_declaration() {
        let program = parse_source("int main() { int arr[5]; return 0; }");
        assert!(matches!(
            &program.functions[0].body[0],
            Statement::Declaration { decl_type: DeclType::IntArray(5), name, initializer: None }
            if name == "arr"
        ));
    }

    #[test]
    fn parse_address_of() {
        let program = parse_source("int main() { int x; return &x; }");
        let Statement::Return(ref expr) = program.functions[0].body[1] else { panic!() };
        assert!(matches!(
            expr,
            Expression::AddressOf { operand }
            if matches!(operand.as_ref(), Expression::Variable(n) if n == "x")
        ));
    }

    #[test]
    fn parse_dereference() {
        let program = parse_source("int main() { int x; int *p = &x; return *p; }");
        let Statement::Return(ref expr) = program.functions[0].body[2] else { panic!() };
        assert!(matches!(expr, Expression::Dereference { .. }));
    }

    #[test]
    fn parse_dereference_assignment() {
        let program = parse_source("int main() { int x; int *p = &x; *p = 5; return x; }");
        let Statement::Expression(ref expr) = program.functions[0].body[2] else { panic!() };
        assert!(matches!(
            expr,
            Expression::Assignment { target, .. }
            if matches!(target.as_ref(), Expression::Dereference { .. })
        ));
    }

    #[test]
    fn parse_array_index() {
        let program = parse_source("int main() { int arr[3]; return arr[1]; }");
        let Statement::Return(ref expr) = program.functions[0].body[1] else { panic!() };
        assert!(matches!(expr, Expression::ArrayIndex { .. }));
    }

    #[test]
    fn parse_array_index_assignment() {
        let program = parse_source("int main() { int arr[3]; arr[0] = 42; return 0; }");
        let Statement::Expression(ref expr) = program.functions[0].body[1] else { panic!() };
        assert!(matches!(
            expr,
            Expression::Assignment { target, .. }
            if matches!(target.as_ref(), Expression::ArrayIndex { .. })
        ));
    }

    #[test]
    fn parse_pointer_param() {
        let program = parse_source("int f(int *p) { return *p; }");
        assert!(matches!(&program.functions[0].params[0].0, DeclType::IntPointer));
    }

    #[test]
    fn parse_deref_higher_than_add() {
        let program = parse_source("int main() { int x; int *p = &x; return *p + 1; }");
        let Statement::Return(ref expr) = program.functions[0].body[2] else { panic!() };
        assert!(matches!(
            expr,
            Expression::BinaryOp {
                operator: BinaryOperator::Add,
                left,
                ..
            } if matches!(left.as_ref(), Expression::Dereference { .. })
        ));
    }

    #[test]
    fn parse_string_literal() {
        let program = parse_source("int main() { return 0; }");
        let tokens = tokenize("int main() { char *s = \"hello\"; return 0; }").unwrap();
        let p = parse(&tokens).unwrap();
        assert!(matches!(
            &p.functions[0].body[0],
            Statement::Declaration { decl_type: DeclType::CharPointer, name, initializer: Some(Expression::StringLiteral(_)) }
            if name == "s"
        ));
        let _ = program;
    }

    #[test]
    fn parse_global_int() {
        let program = parse_source("int x = 5; int main() { return x; }");
        assert_eq!(program.globals.len(), 1);
        assert_eq!(program.globals[0].name, "x");
        assert!(matches!(&program.globals[0].initializer, Some(GlobalInit::Integer(5))));
    }

    #[test]
    fn parse_global_uninitialized() {
        let program = parse_source("int x; int main() { return 0; }");
        assert_eq!(program.globals.len(), 1);
        assert!(matches!(&program.globals[0].initializer, None));
    }

    #[test]
    fn parse_global_pointer() {
        let program = parse_source("int *p; int main() { return 0; }");
        assert!(matches!(&program.globals[0].decl_type, DeclType::IntPointer));
    }

    #[test]
    fn parse_global_array() {
        let program = parse_source("int arr[10]; int main() { return 0; }");
        assert!(matches!(&program.globals[0].decl_type, DeclType::IntArray(10)));
    }

    #[test]
    fn parse_global_string() {
        let tokens = tokenize("char *msg = \"hello\"; int main() { return 0; }").unwrap();
        let program = parse(&tokens).unwrap();
        assert!(matches!(&program.globals[0].decl_type, DeclType::CharPointer));
        assert!(matches!(&program.globals[0].initializer, Some(GlobalInit::StringLiteral(s)) if s == "hello"));
    }

    #[test]
    fn parse_multiple_globals() {
        let program = parse_source("int x = 1; int y = 2; int main() { return x + y; }");
        assert_eq!(program.globals.len(), 2);
        assert_eq!(program.functions.len(), 1);
    }

    #[test]
    fn parse_char_pointer_param() {
        let tokens = tokenize("int f(char *s) { return 0; }").unwrap();
        let program = parse(&tokens).unwrap();
        assert!(matches!(&program.functions[0].params[0].0, DeclType::CharPointer));
    }

    #[test]
    fn parse_negative_global_init() {
        let program = parse_source("int x = -1; int main() { return x; }");
        assert!(matches!(&program.globals[0].initializer, Some(GlobalInit::Integer(-1))));
    }

    #[test]
    fn parse_struct_definition() {
        let program = parse_source("struct Point { int x; int y; }; int main() { return 0; }");
        assert_eq!(program.type_defs.len(), 1);
        assert_eq!(program.type_defs[0].name, "Point");
        assert_eq!(program.type_defs[0].members.len(), 2);
        assert!(!program.type_defs[0].is_union);
    }

    #[test]
    fn parse_union_definition() {
        let program = parse_source("union Data { int i; int *p; }; int main() { return 0; }");
        assert_eq!(program.type_defs.len(), 1);
        assert_eq!(program.type_defs[0].name, "Data");
        assert!(program.type_defs[0].is_union);
    }

    #[test]
    fn parse_struct_variable() {
        let program = parse_source("struct Point { int x; int y; }; int main() { struct Point p; return 0; }");
        assert!(matches!(
            &program.functions[0].body[0],
            Statement::Declaration { decl_type: DeclType::Struct(name), .. }
            if name == "Point"
        ));
    }

    #[test]
    fn parse_struct_pointer_variable() {
        let program = parse_source("struct Point { int x; }; int main() { struct Point *p; return 0; }");
        assert!(matches!(
            &program.functions[0].body[0],
            Statement::Declaration { decl_type: DeclType::StructPointer(name), .. }
            if name == "Point"
        ));
    }

    #[test]
    fn parse_member_access() {
        let program = parse_source("struct Point { int x; }; int main() { struct Point p; return p.x; }");
        let Statement::Return(ref expr) = program.functions[0].body[1] else { panic!() };
        assert!(matches!(
            expr,
            Expression::MemberAccess { member, .. }
            if member == "x"
        ));
    }

    #[test]
    fn parse_arrow_access() {
        let program = parse_source("struct Point { int x; }; int main() { struct Point *p; return p->x; }");
        let Statement::Return(ref expr) = program.functions[0].body[1] else { panic!() };
        assert!(matches!(
            expr,
            Expression::ArrowAccess { member, .. }
            if member == "x"
        ));
    }

    #[test]
    fn parse_member_assignment() {
        let program = parse_source("struct Point { int x; }; int main() { struct Point p; p.x = 10; return 0; }");
        let Statement::Expression(ref expr) = program.functions[0].body[1] else { panic!() };
        assert!(matches!(
            expr,
            Expression::Assignment { target, .. }
            if matches!(target.as_ref(), Expression::MemberAccess { member, .. } if member == "x")
        ));
    }

    #[test]
    fn parse_arrow_assignment() {
        let program = parse_source("struct Point { int x; }; int main() { struct Point *p; p->x = 10; return 0; }");
        let Statement::Expression(ref expr) = program.functions[0].body[1] else { panic!() };
        assert!(matches!(
            expr,
            Expression::Assignment { target, .. }
            if matches!(target.as_ref(), Expression::ArrowAccess { member, .. } if member == "x")
        ));
    }

    #[test]
    fn parse_struct_pointer_param() {
        let program = parse_source("struct Point { int x; }; int getX(struct Point *p) { return p->x; }");
        assert!(matches!(
            &program.functions[0].params[0].0,
            DeclType::StructPointer(name) if name == "Point"
        ));
    }

    #[test]
    fn parse_union_variable() {
        let program = parse_source("union Data { int i; int *p; }; int main() { union Data d; return 0; }");
        assert!(matches!(
            &program.functions[0].body[0],
            Statement::Declaration { decl_type: DeclType::Union(name), .. }
            if name == "Data"
        ));
    }

    #[test]
    fn parse_struct_with_pointer_member() {
        let program = parse_source("struct Node { int val; int *next; }; int main() { return 0; }");
        assert_eq!(program.type_defs[0].members.len(), 2);
        assert!(matches!(&program.type_defs[0].members[0].0, MemberType::Int));
        assert!(matches!(&program.type_defs[0].members[1].0, MemberType::IntPointer));
    }

    #[test]
    fn parse_global_struct_variable() {
        let program = parse_source("struct Point { int x; int y; }; struct Point g; int main() { return 0; }");
        assert_eq!(program.globals.len(), 1);
        assert!(matches!(&program.globals[0].decl_type, DeclType::Struct(name) if name == "Point"));
    }

    #[test]
    fn parse_struct_pointer_init() {
        let program = parse_source(concat!(
            "struct Point { int x; }; ",
            "int main() { struct Point p; struct Point *pp = &p; return 0; }",
        ));
        assert!(matches!(
            &program.functions[0].body[1],
            Statement::Declaration { decl_type: DeclType::StructPointer(_), initializer: Some(_), .. }
        ));
    }
}
