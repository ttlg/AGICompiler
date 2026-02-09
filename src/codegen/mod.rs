use crate::CompileError;
use crate::parser::{Expression, Program, Statement, UnaryOperator};

fn generate_expression(expr: &Expression) -> Vec<String> {
    match expr {
        Expression::IntLiteral(value) => {
            vec![format!("    movl ${value}, %eax")]
        }
        Expression::UnaryOp { operator, operand } => {
            let mut instructions = generate_expression(operand);
            match operator {
                UnaryOperator::Negate => {
                    instructions.push("    negl %eax".to_string());
                }
                UnaryOperator::BitwiseNot => {
                    instructions.push("    notl %eax".to_string());
                }
                UnaryOperator::LogicalNot => {
                    instructions.push("    cmpl $0, %eax".to_string());
                    instructions.push("    sete %al".to_string());
                    instructions.push("    movzbl %al, %eax".to_string());
                }
            }
            instructions
        }
    }
}

pub fn generate(program: &Program) -> Result<String, CompileError> {
    let Statement::Return(ref expr) = program.function.body;
    let body = generate_expression(expr).join("\n");
    Ok(format!(
        "    .globl {name}\n{name}:\n{body}\n    ret\n",
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

    #[test]
    fn generate_negate() {
        let tokens = tokenize("int main() { return -5; }").unwrap();
        let ast = parse(&tokens).unwrap();
        let asm = generate(&ast).unwrap();
        assert!(asm.contains("movl $5, %eax"));
        assert!(asm.contains("negl %eax"));
    }

    #[test]
    fn generate_bitwise_not() {
        let tokens = tokenize("int main() { return ~0; }").unwrap();
        let ast = parse(&tokens).unwrap();
        let asm = generate(&ast).unwrap();
        assert!(asm.contains("movl $0, %eax"));
        assert!(asm.contains("notl %eax"));
    }

    #[test]
    fn generate_logical_not() {
        let tokens = tokenize("int main() { return !5; }").unwrap();
        let ast = parse(&tokens).unwrap();
        let asm = generate(&ast).unwrap();
        assert!(asm.contains("cmpl $0, %eax"));
        assert!(asm.contains("sete %al"));
        assert!(asm.contains("movzbl %al, %eax"));
    }

    #[test]
    fn generate_nested_unary() {
        let tokens = tokenize("int main() { return -(-42); }").unwrap();
        let ast = parse(&tokens).unwrap();
        let asm = generate(&ast).unwrap();
        assert!(asm.contains("movl $42, %eax"));
        assert_eq!(asm.matches("negl %eax").count(), 2);
    }
}
