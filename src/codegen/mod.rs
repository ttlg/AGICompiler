use crate::CompileError;
use crate::parser::{BinaryOperator, Expression, Program, Statement, UnaryOperator};

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
        Expression::BinaryOp { operator, left, right } => {
            let mut instructions = generate_expression(left);
            instructions.push("    push %eax".to_string());
            instructions.extend(generate_expression(right));
            instructions.push("    movl %eax, %ecx".to_string());
            instructions.push("    pop %eax".to_string());
            match operator {
                BinaryOperator::Add => {
                    instructions.push("    addl %ecx, %eax".to_string());
                }
                BinaryOperator::Subtract => {
                    instructions.push("    subl %ecx, %eax".to_string());
                }
                BinaryOperator::Multiply => {
                    instructions.push("    imull %ecx, %eax".to_string());
                }
                BinaryOperator::Divide => {
                    instructions.push("    cdq".to_string());
                    instructions.push("    idivl %ecx".to_string());
                }
                BinaryOperator::Modulo => {
                    instructions.push("    cdq".to_string());
                    instructions.push("    idivl %ecx".to_string());
                    instructions.push("    movl %edx, %eax".to_string());
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

    fn compile(source: &str) -> String {
        let tokens = tokenize(source).unwrap();
        let ast = parse(&tokens).unwrap();
        generate(&ast).unwrap()
    }

    #[test]
    fn generate_return_42() {
        let asm = compile("int main() { return 42; }");
        assert_eq!(asm, "    .globl main\nmain:\n    movl $42, %eax\n    ret\n");
    }

    #[test]
    fn generate_negate() {
        let asm = compile("int main() { return -5; }");
        assert!(asm.contains("movl $5, %eax"));
        assert!(asm.contains("negl %eax"));
    }

    #[test]
    fn generate_bitwise_not() {
        let asm = compile("int main() { return ~0; }");
        assert!(asm.contains("movl $0, %eax"));
        assert!(asm.contains("notl %eax"));
    }

    #[test]
    fn generate_logical_not() {
        let asm = compile("int main() { return !5; }");
        assert!(asm.contains("cmpl $0, %eax"));
        assert!(asm.contains("sete %al"));
        assert!(asm.contains("movzbl %al, %eax"));
    }

    #[test]
    fn generate_nested_unary() {
        let asm = compile("int main() { return -(-42); }");
        assert!(asm.contains("movl $42, %eax"));
        assert_eq!(asm.matches("negl %eax").count(), 2);
    }

    #[test]
    fn generate_add() {
        let asm = compile("int main() { return 1 + 2; }");
        assert!(asm.contains("movl $1, %eax"));
        assert!(asm.contains("push %eax"));
        assert!(asm.contains("movl $2, %eax"));
        assert!(asm.contains("addl %ecx, %eax"));
    }

    #[test]
    fn generate_subtract() {
        let asm = compile("int main() { return 5 - 3; }");
        assert!(asm.contains("subl %ecx, %eax"));
    }

    #[test]
    fn generate_multiply() {
        let asm = compile("int main() { return 2 * 3; }");
        assert!(asm.contains("imull %ecx, %eax"));
    }

    #[test]
    fn generate_divide() {
        let asm = compile("int main() { return 10 / 3; }");
        assert!(asm.contains("cdq"));
        assert!(asm.contains("idivl %ecx"));
    }

    #[test]
    fn generate_modulo() {
        let asm = compile("int main() { return 10 % 3; }");
        assert!(asm.contains("cdq"));
        assert!(asm.contains("idivl %ecx"));
        assert!(asm.contains("movl %edx, %eax"));
    }
}
