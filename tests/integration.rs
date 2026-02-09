use agi_cc::compile;

#[test]
fn return_0() {
    let asm = compile("int main() { return 0; }").unwrap();
    assert!(asm.contains(".globl main"));
    assert!(asm.contains("main:"));
    assert!(asm.contains("movl $0, %eax"));
    assert!(asm.contains("ret"));
}

#[test]
fn return_42() {
    let asm = compile("int main() { return 42; }").unwrap();
    assert!(asm.contains("movl $42, %eax"));
}

#[test]
fn return_255() {
    let asm = compile("int main() { return 255; }").unwrap();
    assert!(asm.contains("movl $255, %eax"));
}

#[test]
fn multiline_source() {
    let asm = compile("int main() {\n    return 123;\n}\n").unwrap();
    assert!(asm.contains("movl $123, %eax"));
}

#[test]
fn full_asm_format() {
    let asm = compile("int main() { return 7; }").unwrap();
    let expected = concat!(
        "    .globl main\n",
        "main:\n",
        "    pushq %rbp\n",
        "    movq %rsp, %rbp\n",
        "    movl $7, %eax\n",
        "    movq %rbp, %rsp\n",
        "    popq %rbp\n",
        "    ret\n",
    );
    assert_eq!(asm, expected);
}

#[test]
fn error_invalid_token() {
    let err = compile("int main() { return @; }").unwrap_err();
    assert!(err.message.contains("unexpected character"));
    assert!(err.line > 0);
}

#[test]
fn error_missing_semicolon() {
    let err = compile("int main() { return 42 }").unwrap_err();
    assert!(err.to_string().contains("expected"));
}

#[test]
fn error_empty_input() {
    let err = compile("").unwrap_err();
    assert!(err.to_string().contains("expected"));
}

#[test]
fn error_missing_brace() {
    let err = compile("int main() { return 0;").unwrap_err();
    assert!(err.to_string().contains("expected"));
}

#[test]
fn unary_negate() {
    let asm = compile("int main() { return -5; }").unwrap();
    assert!(asm.contains("movl $5, %eax"));
    assert!(asm.contains("negl %eax"));
}

#[test]
fn unary_bitwise_not() {
    let asm = compile("int main() { return ~0; }").unwrap();
    assert!(asm.contains("movl $0, %eax"));
    assert!(asm.contains("notl %eax"));
}

#[test]
fn unary_logical_not() {
    let asm = compile("int main() { return !5; }").unwrap();
    assert!(asm.contains("cmpl $0, %eax"));
    assert!(asm.contains("sete %al"));
    assert!(asm.contains("movzbl %al, %eax"));
}

#[test]
fn unary_nested_negate() {
    let asm = compile("int main() { return -(-42); }").unwrap();
    assert!(asm.contains("movl $42, %eax"));
    assert_eq!(asm.matches("negl %eax").count(), 2);
}

#[test]
fn unary_chained() {
    let asm = compile("int main() { return -~!5; }").unwrap();
    assert!(asm.contains("movl $5, %eax"));
    assert!(asm.contains("cmpl $0, %eax"));
    assert!(asm.contains("notl %eax"));
    assert!(asm.contains("negl %eax"));
}

#[test]
fn unary_with_parens() {
    let asm = compile("int main() { return -(42); }").unwrap();
    assert!(asm.contains("movl $42, %eax"));
    assert!(asm.contains("negl %eax"));
}

#[test]
fn error_unary_missing_operand() {
    let err = compile("int main() { return -; }").unwrap_err();
    assert!(err.to_string().contains("expected"));
}

#[test]
fn binary_add() {
    let asm = compile("int main() { return 1 + 2; }").unwrap();
    assert!(asm.contains("movl $1, %eax"));
    assert!(asm.contains("pushq %rax"));
    assert!(asm.contains("movl $2, %eax"));
    assert!(asm.contains("movl %eax, %ecx"));
    assert!(asm.contains("popq %rax"));
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn binary_subtract() {
    let asm = compile("int main() { return 5 - 3; }").unwrap();
    assert!(asm.contains("subl %ecx, %eax"));
}

#[test]
fn binary_multiply() {
    let asm = compile("int main() { return 2 * 3; }").unwrap();
    assert!(asm.contains("imull %ecx, %eax"));
}

#[test]
fn binary_divide() {
    let asm = compile("int main() { return 10 / 3; }").unwrap();
    assert!(asm.contains("cdq"));
    assert!(asm.contains("idivl %ecx"));
}

#[test]
fn binary_modulo() {
    let asm = compile("int main() { return 10 % 3; }").unwrap();
    assert!(asm.contains("cdq"));
    assert!(asm.contains("idivl %ecx"));
    assert!(asm.contains("movl %edx, %eax"));
}

#[test]
fn precedence_mul_over_add() {
    let asm = compile("int main() { return 2 + 3 * 4; }").unwrap();
    assert!(asm.contains("imull %ecx, %eax"));
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn precedence_parens_override() {
    let asm = compile("int main() { return (1 + 2) * 3; }").unwrap();
    assert!(asm.contains("addl %ecx, %eax"));
    assert!(asm.contains("imull %ecx, %eax"));
}

#[test]
fn left_associativity() {
    let asm = compile("int main() { return 10 - 3 - 2; }").unwrap();
    assert_eq!(asm.matches("subl %ecx, %eax").count(), 2);
}

#[test]
fn unary_with_binary() {
    let asm = compile("int main() { return -2 + 3; }").unwrap();
    assert!(asm.contains("negl %eax"));
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn complex_expression() {
    let asm = compile("int main() { return (1 + 2) * (3 - 4) / 5; }").unwrap();
    assert!(asm.contains("addl %ecx, %eax"));
    assert!(asm.contains("subl %ecx, %eax"));
    assert!(asm.contains("imull %ecx, %eax"));
    assert!(asm.contains("idivl %ecx"));
}

#[test]
fn error_binary_missing_right_operand() {
    let err = compile("int main() { return 1 +; }").unwrap_err();
    assert!(err.to_string().contains("expected"));
}

#[test]
fn error_binary_missing_left_operand() {
    let err = compile("int main() { return * 2; }").unwrap_err();
    assert!(err.to_string().contains("expected"));
}

#[test]
fn variable_declaration_and_return() {
    let asm = compile("int main() { int x = 42; return x; }").unwrap();
    assert!(asm.contains("subq $16, %rsp"));
    assert!(asm.contains("movl $42, %eax"));
    assert!(asm.contains("movl %eax, -4(%rbp)"));
    assert!(asm.contains("movl -4(%rbp), %eax"));
}

#[test]
fn variable_declaration_without_init() {
    let asm = compile("int main() { int x; x = 10; return x; }").unwrap();
    assert!(asm.contains("movl $10, %eax"));
    assert!(asm.contains("movl %eax, -4(%rbp)"));
    assert!(asm.contains("movl -4(%rbp), %eax"));
}

#[test]
fn multiple_variables() {
    let asm = compile("int main() { int a = 1; int b = 2; return a + b; }").unwrap();
    assert!(asm.contains("movl %eax, -4(%rbp)"));
    assert!(asm.contains("movl %eax, -8(%rbp)"));
}

#[test]
fn variable_reassignment() {
    let asm = compile("int main() { int x = 1; x = 2; return x; }").unwrap();
    assert_eq!(asm.matches("movl %eax, -4(%rbp)").count(), 2);
}

#[test]
fn chained_assignment() {
    let asm = compile("int main() { int a; int b; a = b = 5; return a; }").unwrap();
    assert!(asm.contains("movl $5, %eax"));
    assert!(asm.contains("movl %eax, -8(%rbp)"));
    assert!(asm.contains("movl %eax, -4(%rbp)"));
}

#[test]
fn variable_in_expression() {
    let asm = compile("int main() { int x = 10; return x * 2 + 1; }").unwrap();
    assert!(asm.contains("movl -4(%rbp), %eax"));
    assert!(asm.contains("imull %ecx, %eax"));
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn expression_statement() {
    compile("int main() { int x = 0; x = x + 1; return x; }").unwrap();
}

#[test]
fn full_asm_format_with_variables() {
    let asm = compile("int main() { int x = 7; return x; }").unwrap();
    let expected = concat!(
        "    .globl main\n",
        "main:\n",
        "    pushq %rbp\n",
        "    movq %rsp, %rbp\n",
        "    subq $16, %rsp\n",
        "    movl $7, %eax\n",
        "    movl %eax, -4(%rbp)\n",
        "    movl -4(%rbp), %eax\n",
        "    movq %rbp, %rsp\n",
        "    popq %rbp\n",
        "    ret\n",
    );
    assert_eq!(asm, expected);
}

#[test]
fn error_undefined_variable() {
    let err = compile("int main() { return x; }").unwrap_err();
    assert!(err.message.contains("undefined variable"));
}

#[test]
fn error_redeclaration() {
    let err = compile("int main() { int x = 1; int x = 2; return x; }").unwrap_err();
    assert!(err.message.contains("already declared"));
}

#[test]
fn error_assign_to_undeclared() {
    let err = compile("int main() { x = 5; return 0; }").unwrap_err();
    assert!(err.message.contains("undefined variable"));
}

#[test]
fn comparison_equal() {
    let asm = compile("int main() { return 1 == 1; }").unwrap();
    assert!(asm.contains("cmpl %ecx, %eax"));
    assert!(asm.contains("sete %al"));
    assert!(asm.contains("movzbl %al, %eax"));
}

#[test]
fn comparison_not_equal() {
    let asm = compile("int main() { return 1 != 2; }").unwrap();
    assert!(asm.contains("setne %al"));
}

#[test]
fn comparison_less_than() {
    let asm = compile("int main() { return 1 < 2; }").unwrap();
    assert!(asm.contains("setl %al"));
}

#[test]
fn comparison_less_equal() {
    let asm = compile("int main() { return 1 <= 2; }").unwrap();
    assert!(asm.contains("setle %al"));
}

#[test]
fn comparison_greater_than() {
    let asm = compile("int main() { return 2 > 1; }").unwrap();
    assert!(asm.contains("setg %al"));
}

#[test]
fn comparison_greater_equal() {
    let asm = compile("int main() { return 2 >= 1; }").unwrap();
    assert!(asm.contains("setge %al"));
}

#[test]
fn logical_and() {
    let asm = compile("int main() { return 1 && 2; }").unwrap();
    assert!(asm.contains("cmpl $0, %eax"));
    assert!(asm.contains("je .L"));
    assert!(asm.contains("movl $1, %eax"));
    assert!(asm.contains("movl $0, %eax"));
}

#[test]
fn logical_or() {
    let asm = compile("int main() { return 0 || 1; }").unwrap();
    assert!(asm.contains("cmpl $0, %eax"));
    assert!(asm.contains("jne .L"));
    assert!(asm.contains("movl $1, %eax"));
    assert!(asm.contains("movl $0, %eax"));
}

#[test]
fn logical_and_short_circuit() {
    let asm = compile("int main() { return 0 && 1; }").unwrap();
    assert!(asm.contains("je .L"));
}

#[test]
fn logical_or_short_circuit() {
    let asm = compile("int main() { return 1 || 0; }").unwrap();
    assert!(asm.contains("jne .L"));
}

#[test]
fn if_statement() {
    let asm = compile("int main() { int x = 0; if (1) x = 1; return x; }").unwrap();
    assert!(asm.contains("cmpl $0, %eax"));
    assert!(asm.contains("je .L"));
}

#[test]
fn if_else_statement() {
    let asm = compile("int main() { int x; if (0) x = 1; else x = 2; return x; }").unwrap();
    assert!(asm.contains("je .L"));
    assert!(asm.contains("jmp .L"));
}

#[test]
fn if_with_block() {
    compile("int main() { int x = 0; if (1) { x = 1; } return x; }").unwrap();
}

#[test]
fn nested_if_else() {
    compile("int main() { int x = 0; if (1) { if (0) x = 1; else x = 2; } return x; }").unwrap();
}

#[test]
fn ternary_expression() {
    let asm = compile("int main() { return 1 ? 42 : 0; }").unwrap();
    assert!(asm.contains("cmpl $0, %eax"));
    assert!(asm.contains("je .L"));
    assert!(asm.contains("jmp .L"));
    assert!(asm.contains("movl $42, %eax"));
}

#[test]
fn ternary_nested() {
    compile("int main() { return 1 ? 2 ? 3 : 4 : 5; }").unwrap();
}

#[test]
fn ternary_in_variable() {
    compile("int main() { int x = 1 ? 42 : 0; return x; }").unwrap();
}

#[test]
fn while_loop() {
    let asm = compile("int main() { int x = 0; while (x < 5) x = x + 1; return x; }").unwrap();
    assert!(asm.contains("jmp .L"));
    assert!(asm.contains("je .L"));
}

#[test]
fn while_with_block() {
    compile("int main() { int x = 0; while (x < 10) { x = x + 1; } return x; }").unwrap();
}

#[test]
fn do_while_loop() {
    let asm = compile("int main() { int x = 0; do x = x + 1; while (x < 5); return x; }").unwrap();
    assert!(asm.contains("jne .L"));
}

#[test]
fn do_while_with_block() {
    compile("int main() { int x = 0; do { x = x + 1; } while (x < 10); return x; }").unwrap();
}

#[test]
fn for_loop() {
    let asm = compile("int main() { int s = 0; for (int i = 0; i < 5; i = i + 1) s = s + i; return s; }").unwrap();
    assert!(asm.contains("jmp .L"));
    assert!(asm.contains("je .L"));
}

#[test]
fn for_loop_with_block() {
    compile("int main() { int s = 0; for (int i = 0; i < 10; i = i + 1) { s = s + i; } return s; }").unwrap();
}

#[test]
fn for_loop_empty_init() {
    compile("int main() { int i = 0; int s = 0; for (; i < 5; i = i + 1) s = s + 1; return s; }").unwrap();
}

#[test]
fn for_loop_empty_condition() {
    compile("int main() { for (int i = 0; ; i = i + 1) return i; }").unwrap();
}

#[test]
fn for_loop_all_empty() {
    compile("int main() { for (;;) return 0; }").unwrap();
}

#[test]
fn compound_statement() {
    compile("int main() { int x = 0; { x = 1; } return x; }").unwrap();
}

#[test]
fn compound_with_declaration() {
    compile("int main() { int x = 1; { int y = 2; x = y; } return x; }").unwrap();
}

#[test]
fn variable_shadowing() {
    compile("int main() { int x = 1; { int x = 2; } return x; }").unwrap();
}

#[test]
fn comparison_in_condition() {
    compile("int main() { int x = 5; if (x > 3) return 1; return 0; }").unwrap();
}

#[test]
fn logical_in_condition() {
    compile("int main() { int x = 5; if (x > 0 && x < 10) return 1; return 0; }").unwrap();
}

#[test]
fn nested_loops() {
    compile("int main() { int s = 0; for (int i = 0; i < 3; i = i + 1) for (int j = 0; j < 3; j = j + 1) s = s + 1; return s; }").unwrap();
}

#[test]
fn while_with_comparison() {
    compile("int main() { int x = 10; while (x > 0) x = x - 1; return x; }").unwrap();
}

#[test]
fn error_if_missing_paren() {
    let err = compile("int main() { if 1 return 0; }").unwrap_err();
    assert!(err.to_string().contains("expected"));
}

#[test]
fn error_while_missing_paren() {
    let err = compile("int main() { while 1 return 0; }").unwrap_err();
    assert!(err.to_string().contains("expected"));
}

#[test]
fn error_do_while_missing_semicolon() {
    let err = compile("int main() { do return 0; while (1) }").unwrap_err();
    assert!(err.to_string().contains("expected"));
}
