use std::collections::HashMap;

use crate::CompileError;
use crate::parser::{BinaryOperator, Expression, Program, Statement, UnaryOperator};

struct Context {
    scopes: Vec<HashMap<String, i64>>,
    stack_offset: i64,
    label_counter: usize,
}

impl Context {
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &str) -> Result<i64, CompileError> {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(name) {
            return Err(CompileError {
                message: format!("variable already declared: {name}"),
                line: 0,
                col: 0,
            });
        }
        self.stack_offset += 4;
        scope.insert(name.to_string(), self.stack_offset);
        Ok(self.stack_offset)
    }

    fn lookup(&self, name: &str) -> Result<i64, CompileError> {
        for scope in self.scopes.iter().rev() {
            if let Some(&offset) = scope.get(name) {
                return Ok(offset);
            }
        }
        Err(CompileError {
            message: format!("undefined variable: {name}"),
            line: 0,
            col: 0,
        })
    }

    fn next_label(&mut self) -> String {
        let label = format!(".L{}", self.label_counter);
        self.label_counter += 1;
        label
    }
}

fn generate_expression(ctx: &mut Context, expr: &Expression) -> Result<Vec<String>, CompileError> {
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
            match operator {
                BinaryOperator::LogicalAnd => {
                    let false_label = ctx.next_label();
                    let end_label = ctx.next_label();
                    let mut instructions = generate_expression(ctx, left)?;
                    instructions.push("    cmpl $0, %eax".to_string());
                    instructions.push(format!("    je {false_label}"));
                    instructions.extend(generate_expression(ctx, right)?);
                    instructions.push("    cmpl $0, %eax".to_string());
                    instructions.push(format!("    je {false_label}"));
                    instructions.push("    movl $1, %eax".to_string());
                    instructions.push(format!("    jmp {end_label}"));
                    instructions.push(format!("{false_label}:"));
                    instructions.push("    movl $0, %eax".to_string());
                    instructions.push(format!("{end_label}:"));
                    Ok(instructions)
                }
                BinaryOperator::LogicalOr => {
                    let true_label = ctx.next_label();
                    let end_label = ctx.next_label();
                    let mut instructions = generate_expression(ctx, left)?;
                    instructions.push("    cmpl $0, %eax".to_string());
                    instructions.push(format!("    jne {true_label}"));
                    instructions.extend(generate_expression(ctx, right)?);
                    instructions.push("    cmpl $0, %eax".to_string());
                    instructions.push(format!("    jne {true_label}"));
                    instructions.push("    movl $0, %eax".to_string());
                    instructions.push(format!("    jmp {end_label}"));
                    instructions.push(format!("{true_label}:"));
                    instructions.push("    movl $1, %eax".to_string());
                    instructions.push(format!("{end_label}:"));
                    Ok(instructions)
                }
                _ => {
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
                        BinaryOperator::Equal
                        | BinaryOperator::NotEqual
                        | BinaryOperator::LessThan
                        | BinaryOperator::GreaterThan
                        | BinaryOperator::LessEqual
                        | BinaryOperator::GreaterEqual => {
                            instructions.push("    cmpl %ecx, %eax".to_string());
                            let set_instr = match operator {
                                BinaryOperator::Equal => "sete",
                                BinaryOperator::NotEqual => "setne",
                                BinaryOperator::LessThan => "setl",
                                BinaryOperator::GreaterThan => "setg",
                                BinaryOperator::LessEqual => "setle",
                                BinaryOperator::GreaterEqual => "setge",
                                _ => unreachable!(),
                            };
                            instructions.push(format!("    {set_instr} %al"));
                            instructions.push("    movzbl %al, %eax".to_string());
                        }
                        BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => unreachable!(),
                    }
                    Ok(instructions)
                }
            }
        }
        Expression::Ternary { condition, then_expr, else_expr } => {
            let else_label = ctx.next_label();
            let end_label = ctx.next_label();
            let mut instructions = generate_expression(ctx, condition)?;
            instructions.push("    cmpl $0, %eax".to_string());
            instructions.push(format!("    je {else_label}"));
            instructions.extend(generate_expression(ctx, then_expr)?);
            instructions.push(format!("    jmp {end_label}"));
            instructions.push(format!("{else_label}:"));
            instructions.extend(generate_expression(ctx, else_expr)?);
            instructions.push(format!("{end_label}:"));
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
        Statement::Compound(stmts) => {
            ctx.push_scope();
            let mut instructions = Vec::new();
            for stmt in stmts {
                instructions.extend(generate_statement(ctx, stmt)?);
            }
            ctx.pop_scope();
            Ok(instructions)
        }
        Statement::If { condition, then_branch, else_branch } => {
            let mut instructions = generate_expression(ctx, condition)?;
            instructions.push("    cmpl $0, %eax".to_string());
            if let Some(else_branch) = else_branch {
                let else_label = ctx.next_label();
                let end_label = ctx.next_label();
                instructions.push(format!("    je {else_label}"));
                instructions.extend(generate_statement(ctx, then_branch)?);
                instructions.push(format!("    jmp {end_label}"));
                instructions.push(format!("{else_label}:"));
                instructions.extend(generate_statement(ctx, else_branch)?);
                instructions.push(format!("{end_label}:"));
            } else {
                let end_label = ctx.next_label();
                instructions.push(format!("    je {end_label}"));
                instructions.extend(generate_statement(ctx, then_branch)?);
                instructions.push(format!("{end_label}:"));
            }
            Ok(instructions)
        }
        Statement::While { condition, body } => {
            let start_label = ctx.next_label();
            let end_label = ctx.next_label();
            let mut instructions = vec![format!("{start_label}:")];
            instructions.extend(generate_expression(ctx, condition)?);
            instructions.push("    cmpl $0, %eax".to_string());
            instructions.push(format!("    je {end_label}"));
            instructions.extend(generate_statement(ctx, body)?);
            instructions.push(format!("    jmp {start_label}"));
            instructions.push(format!("{end_label}:"));
            Ok(instructions)
        }
        Statement::DoWhile { body, condition } => {
            let start_label = ctx.next_label();
            let mut instructions = vec![format!("{start_label}:")];
            instructions.extend(generate_statement(ctx, body)?);
            instructions.extend(generate_expression(ctx, condition)?);
            instructions.push("    cmpl $0, %eax".to_string());
            instructions.push(format!("    jne {start_label}"));
            Ok(instructions)
        }
        Statement::For { init, condition, post, body } => {
            ctx.push_scope();
            let start_label = ctx.next_label();
            let end_label = ctx.next_label();
            let mut instructions = Vec::new();
            if let Some(init) = init {
                instructions.extend(generate_statement(ctx, init)?);
            }
            instructions.push(format!("{start_label}:"));
            if let Some(condition) = condition {
                instructions.extend(generate_expression(ctx, condition)?);
                instructions.push("    cmpl $0, %eax".to_string());
                instructions.push(format!("    je {end_label}"));
            }
            instructions.extend(generate_statement(ctx, body)?);
            if let Some(post) = post {
                instructions.extend(generate_expression(ctx, post)?);
            }
            instructions.push(format!("    jmp {start_label}"));
            instructions.push(format!("{end_label}:"));
            ctx.pop_scope();
            Ok(instructions)
        }
    }
}

fn count_declarations_in_statement(stmt: &Statement) -> usize {
    match stmt {
        Statement::Declaration { .. } => 1,
        Statement::Compound(stmts) => count_declarations(stmts),
        Statement::If { then_branch, else_branch, .. } => {
            let mut count = count_declarations_in_statement(then_branch);
            if let Some(else_branch) = else_branch {
                count += count_declarations_in_statement(else_branch);
            }
            count
        }
        Statement::While { body, .. } | Statement::DoWhile { body, .. } => {
            count_declarations_in_statement(body)
        }
        Statement::For { init, body, .. } => {
            let mut count = 0;
            if let Some(init) = init {
                count += count_declarations_in_statement(init);
            }
            count += count_declarations_in_statement(body);
            count
        }
        _ => 0,
    }
}

fn count_declarations(statements: &[Statement]) -> usize {
    statements.iter().map(count_declarations_in_statement).sum()
}

pub fn generate(program: &Program) -> Result<String, CompileError> {
    let function = &program.function;
    let var_count = count_declarations(&function.body);
    let stack_size = ((var_count * 4 + 15) / 16) * 16;

    let mut ctx = Context {
        scopes: vec![HashMap::new()],
        stack_offset: 0,
        label_counter: 0,
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

    #[test]
    fn generate_comparison_equal() {
        let asm = compile("int main() { return 1 == 2; }");
        assert!(asm.contains("cmpl %ecx, %eax"));
        assert!(asm.contains("sete %al"));
        assert!(asm.contains("movzbl %al, %eax"));
    }

    #[test]
    fn generate_comparison_not_equal() {
        let asm = compile("int main() { return 1 != 2; }");
        assert!(asm.contains("setne %al"));
    }

    #[test]
    fn generate_comparison_less() {
        let asm = compile("int main() { return 1 < 2; }");
        assert!(asm.contains("setl %al"));
    }

    #[test]
    fn generate_comparison_greater() {
        let asm = compile("int main() { return 2 > 1; }");
        assert!(asm.contains("setg %al"));
    }

    #[test]
    fn generate_comparison_less_equal() {
        let asm = compile("int main() { return 1 <= 2; }");
        assert!(asm.contains("setle %al"));
    }

    #[test]
    fn generate_comparison_greater_equal() {
        let asm = compile("int main() { return 2 >= 1; }");
        assert!(asm.contains("setge %al"));
    }

    #[test]
    fn generate_logical_and() {
        let asm = compile("int main() { return 1 && 2; }");
        assert!(asm.contains("cmpl $0, %eax"));
        assert!(asm.contains("je .L"));
        assert!(asm.contains("movl $1, %eax"));
        assert!(asm.contains("movl $0, %eax"));
    }

    #[test]
    fn generate_logical_or() {
        let asm = compile("int main() { return 0 || 1; }");
        assert!(asm.contains("cmpl $0, %eax"));
        assert!(asm.contains("jne .L"));
        assert!(asm.contains("movl $1, %eax"));
        assert!(asm.contains("movl $0, %eax"));
    }

    #[test]
    fn generate_ternary() {
        let asm = compile("int main() { return 1 ? 42 : 0; }");
        assert!(asm.contains("cmpl $0, %eax"));
        assert!(asm.contains("je .L"));
        assert!(asm.contains("jmp .L"));
        assert!(asm.contains("movl $42, %eax"));
    }

    #[test]
    fn generate_if_statement() {
        let asm = compile("int main() { int x = 0; if (1) x = 1; return x; }");
        assert!(asm.contains("cmpl $0, %eax"));
        assert!(asm.contains("je .L"));
    }

    #[test]
    fn generate_if_else() {
        let asm = compile("int main() { int x; if (1) x = 1; else x = 2; return x; }");
        assert!(asm.contains("je .L"));
        assert!(asm.contains("jmp .L"));
    }

    #[test]
    fn generate_while_loop() {
        let asm = compile("int main() { int x = 0; while (x < 5) x = x + 1; return x; }");
        assert!(asm.contains("jmp .L"));
        assert!(asm.contains("je .L"));
    }

    #[test]
    fn generate_do_while_loop() {
        let asm = compile("int main() { int x = 0; do x = x + 1; while (x < 5); return x; }");
        assert!(asm.contains("jne .L"));
    }

    #[test]
    fn generate_for_loop() {
        let asm = compile("int main() { int s = 0; for (int i = 0; i < 5; i = i + 1) s = s + i; return s; }");
        assert!(asm.contains("jmp .L"));
        assert!(asm.contains("je .L"));
    }

    #[test]
    fn generate_compound_statement() {
        compile("int main() { int x = 0; { x = 1; } return x; }");
    }

    #[test]
    fn generate_variable_shadowing() {
        compile("int main() { int x = 1; { int x = 2; } return x; }");
    }
}
