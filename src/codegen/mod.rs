use std::collections::HashMap;

use crate::CompileError;
use crate::parser::{BinaryOperator, Expression, Program, Statement, UnaryOperator};

struct Context {
    variables: HashMap<String, i64>,
    stack_offset: i64,
}

impl Context {
    fn declare(&mut self, name: &str) -> Result<i64, CompileError> {
        if self.variables.contains_key(name) {
            return Err(CompileError {
                message: format!("variable already declared: {name}"),
                line: 0,
                col: 0,
            });
        }
        self.stack_offset += 4;
        self.variables.insert(name.to_string(), self.stack_offset);
        Ok(self.stack_offset)
    }

    fn lookup(&self, name: &str) -> Result<i64, CompileError> {
        self.variables.get(name).copied().ok_or_else(|| CompileError {
            message: format!("undefined variable: {name}"),
            line: 0,
            col: 0,
        })
    }
}

fn generate_expression(ctx: &Context, expr: &Expression) -> Result<Vec<String>, CompileError> {
    match expr {
        Expression::IntLiteral(value) => {
            Ok(vec![format!("    movl ${value}, %eax")])
        }
        Expression::Variable(name) => {
            let offset = ctx.lookup(name)?;
            Ok(vec![format!("    movl -{offset}(%rbp), %eax")])
        }
        Expression::Assignment { name, value } => {
            let mut instructions = generate_expression(ctx, value)?;
            let offset = ctx.lookup(name)?;
            instructions.push(format!("    movl %eax, -{offset}(%rbp)"));
            Ok(instructions)
        }
        Expression::UnaryOp { operator, operand } => {
            let mut instructions = generate_expression(ctx, operand)?;
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
            Ok(instructions)
        }
        Expression::BinaryOp { operator, left, right } => {
            let mut instructions = generate_expression(ctx, left)?;
            instructions.push("    pushq %rax".to_string());
            instructions.extend(generate_expression(ctx, right)?);
            instructions.push("    movl %eax, %ecx".to_string());
            instructions.push("    popq %rax".to_string());
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
            Ok(instructions)
        }
    }
}

fn generate_statement(ctx: &mut Context, stmt: &Statement) -> Result<Vec<String>, CompileError> {
    match stmt {
        Statement::Return(expr) => {
            let mut instructions = generate_expression(ctx, expr)?;
            instructions.push("    movq %rbp, %rsp".to_string());
            instructions.push("    popq %rbp".to_string());
            instructions.push("    ret".to_string());
            Ok(instructions)
        }
        Statement::Declaration { name, initializer } => {
            let offset = ctx.declare(name)?;
            if let Some(expr) = initializer {
                let mut instructions = generate_expression(ctx, expr)?;
                instructions.push(format!("    movl %eax, -{offset}(%rbp)"));
                Ok(instructions)
            } else {
                Ok(vec![])
            }
        }
        Statement::Expression(expr) => generate_expression(ctx, expr),
    }
}

fn count_declarations(statements: &[Statement]) -> usize {
    statements
        .iter()
        .filter(|s| matches!(s, Statement::Declaration { .. }))
        .count()
}

pub fn generate(program: &Program) -> Result<String, CompileError> {
    let function = &program.function;
    let var_count = count_declarations(&function.body);
    let stack_size = ((var_count * 4 + 15) / 16) * 16;

    let mut ctx = Context {
        variables: HashMap::new(),
        stack_offset: 0,
    };

    let mut lines = vec![
        format!("    .globl {}", function.name),
        format!("{}:", function.name),
        "    pushq %rbp".to_string(),
        "    movq %rsp, %rbp".to_string(),
    ];

    if stack_size > 0 {
        lines.push(format!("    subq ${stack_size}, %rsp"));
    }

    for stmt in &function.body {
        lines.extend(generate_statement(&mut ctx, stmt)?);
    }

    lines.push(String::new());
    Ok(lines.join("\n"))
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

    fn compile_err(source: &str) -> CompileError {
        let tokens = tokenize(source).unwrap();
        let ast = parse(&tokens).unwrap();
        generate(&ast).unwrap_err()
    }

    #[test]
    fn generate_return_42() {
        let asm = compile("int main() { return 42; }");
        assert!(asm.contains("pushq %rbp"));
        assert!(asm.contains("movq %rsp, %rbp"));
        assert!(asm.contains("movl $42, %eax"));
        assert!(asm.contains("movq %rbp, %rsp"));
        assert!(asm.contains("popq %rbp"));
        assert!(asm.contains("ret"));
    }

    #[test]
    fn generate_negate() {
        let asm = compile("int main() { return -5; }");
        assert!(asm.contains("movl $5, %eax"));
        assert!(asm.contains("negl %eax"));
    }

    #[test]
    fn generate_add() {
        let asm = compile("int main() { return 1 + 2; }");
        assert!(asm.contains("pushq %rax"));
        assert!(asm.contains("popq %rax"));
        assert!(asm.contains("addl %ecx, %eax"));
    }

    #[test]
    fn generate_variable_declaration() {
        let asm = compile("int main() { int x = 5; return x; }");
        assert!(asm.contains("subq $16, %rsp"));
        assert!(asm.contains("movl $5, %eax"));
        assert!(asm.contains("movl %eax, -4(%rbp)"));
        assert!(asm.contains("movl -4(%rbp), %eax"));
    }

    #[test]
    fn generate_assignment() {
        let asm = compile("int main() { int x; x = 10; return x; }");
        assert!(asm.contains("movl $10, %eax"));
        assert!(asm.contains("movl %eax, -4(%rbp)"));
        assert!(asm.contains("movl -4(%rbp), %eax"));
    }

    #[test]
    fn generate_multiple_variables() {
        let asm = compile("int main() { int a = 1; int b = 2; return a + b; }");
        assert!(asm.contains("movl %eax, -4(%rbp)"));
        assert!(asm.contains("movl %eax, -8(%rbp)"));
        assert!(asm.contains("movl -4(%rbp), %eax"));
        assert!(asm.contains("movl -8(%rbp), %eax"));
    }

    #[test]
    fn generate_undefined_variable() {
        let err = compile_err("int main() { return x; }");
        assert!(err.message.contains("undefined variable"));
    }

    #[test]
    fn generate_redeclaration() {
        let err = compile_err("int main() { int x = 1; int x = 2; return x; }");
        assert!(err.message.contains("already declared"));
    }
}
