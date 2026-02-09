pub mod preprocessor;
pub mod lexer;
pub mod parser;
pub mod codegen;

use std::fmt;

#[derive(Debug)]
pub struct CompileError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error at {}:{}: {}", self.line, self.col, self.message)
    }
}

impl std::error::Error for CompileError {}

pub fn compile(source: &str) -> Result<String, CompileError> {
    let preprocessed = preprocessor::preprocess(source)?;
    let tokens = lexer::tokenize(&preprocessed)?;
    let ast = parser::parse(&tokens)?;
    let asm = codegen::generate(&ast)?;
    Ok(asm)
}
