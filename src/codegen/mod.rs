use crate::CompileError;
use crate::parser::{Expression, Program, Statement};

pub fn generate(program: &Program) -> Result<String, CompileError> {
    let Statement::Return(Expression::IntLiteral(value)) = &program.function.body;
    Ok(format!(
        "    .globl {name}\n{name}:\n    movl ${value}, %eax\n    ret\n",
        name = program.function.name,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;
    use crate::parser::parse;

    #[test]
    fn generate_return_42() {
        let tokens = tokenize("int main() { return 42; }").unwrap();
        let ast = parse(&tokens).unwrap();
        let asm = generate(&ast).unwrap();
        assert_eq!(asm, "    .globl main\nmain:\n    movl $42, %eax\n    ret\n");
    }
}
