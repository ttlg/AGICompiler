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
    assert!(asm.contains("movq %rax, %rcx"));
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
    let err = compile("int main() { return / 2; }").unwrap_err();
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

#[test]
fn function_call_no_args() {
    let asm = compile("int foo() { return 42; } int main() { return foo(); }").unwrap();
    assert!(asm.contains(".globl foo"));
    assert!(asm.contains("foo:"));
    assert!(asm.contains(".globl main"));
    assert!(asm.contains("main:"));
    assert!(asm.contains("call foo"));
}

#[test]
fn function_with_one_param() {
    let asm = compile("int identity(int x) { return x; } int main() { return identity(42); }").unwrap();
    assert!(asm.contains("movl %edi, -4(%rbp)"));
    assert!(asm.contains("movl -4(%rbp), %eax"));
    assert!(asm.contains("movl $42, %eax"));
    assert!(asm.contains("popq %rdi"));
    assert!(asm.contains("call identity"));
}

#[test]
fn function_with_two_params() {
    let asm = compile("int add(int a, int b) { return a + b; } int main() { return add(3, 4); }").unwrap();
    assert!(asm.contains("movl %edi, -4(%rbp)"));
    assert!(asm.contains("movl %esi, -8(%rbp)"));
    assert!(asm.contains("popq %rsi"));
    assert!(asm.contains("popq %rdi"));
    assert!(asm.contains("call add"));
}

#[test]
fn function_call_in_expression() {
    let asm = compile("int f() { return 1; } int main() { return f() + 2; }").unwrap();
    assert!(asm.contains("call f"));
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn function_call_as_argument() {
    let asm = compile("int f() { return 5; } int g(int x) { return x; } int main() { return g(f()); }").unwrap();
    assert!(asm.contains("call f"));
    assert!(asm.contains("call g"));
}

#[test]
fn multiple_function_calls() {
    let asm = compile("int a() { return 1; } int b() { return 2; } int main() { return a() + b(); }").unwrap();
    assert!(asm.contains("call a"));
    assert!(asm.contains("call b"));
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn recursive_function() {
    let asm = compile(concat!(
        "int factorial(int n) { if (n <= 1) return 1; return n * factorial(n - 1); } ",
        "int main() { return factorial(5); }",
    )).unwrap();
    assert!(asm.contains("call factorial"));
    assert!(asm.contains(".globl factorial"));
}

#[test]
fn function_with_local_variables() {
    let asm = compile(concat!(
        "int f(int x) { int y = x + 1; return y; } ",
        "int main() { return f(10); }",
    )).unwrap();
    assert!(asm.contains("movl %edi, -4(%rbp)"));
    assert!(asm.contains("call f"));
}

#[test]
fn function_six_params() {
    let asm = compile(concat!(
        "int sum(int a, int b, int c, int d, int e, int f) { return a + b + c + d + e + f; } ",
        "int main() { return sum(1, 2, 3, 4, 5, 6); }",
    )).unwrap();
    assert!(asm.contains("movl %edi, -4(%rbp)"));
    assert!(asm.contains("movl %esi, -8(%rbp)"));
    assert!(asm.contains("movl %edx, -12(%rbp)"));
    assert!(asm.contains("movl %ecx, -16(%rbp)"));
    assert!(asm.contains("movl %r8d, -20(%rbp)"));
    assert!(asm.contains("movl %r9d, -24(%rbp)"));
}

#[test]
fn function_param_in_condition() {
    let asm = compile(concat!(
        "int abs(int x) { if (x < 0) return 0 - x; return x; } ",
        "int main() { return abs(5); }",
    )).unwrap();
    assert!(asm.contains("call abs"));
}

#[test]
fn function_call_result_in_variable() {
    let asm = compile(concat!(
        "int f() { return 42; } ",
        "int main() { int x = f(); return x; }",
    )).unwrap();
    assert!(asm.contains("call f"));
    assert!(asm.contains("movl %eax, -4(%rbp)"));
}

#[test]
fn full_asm_format_function_call() {
    let asm = compile("int f() { return 1; } int main() { return f(); }").unwrap();
    let expected = concat!(
        "    .globl f\n",
        "f:\n",
        "    pushq %rbp\n",
        "    movq %rsp, %rbp\n",
        "    movl $1, %eax\n",
        "    movq %rbp, %rsp\n",
        "    popq %rbp\n",
        "    ret\n",
        "    .globl main\n",
        "main:\n",
        "    pushq %rbp\n",
        "    movq %rsp, %rbp\n",
        "    call f\n",
        "    movq %rbp, %rsp\n",
        "    popq %rbp\n",
        "    ret\n",
    );
    assert_eq!(asm, expected);
}

#[test]
fn error_too_many_arguments() {
    let err = compile("int main() { return f(1, 2, 3, 4, 5, 6, 7); }").unwrap_err();
    assert!(err.message.contains("too many arguments"));
}

#[test]
fn pointer_declaration() {
    let asm = compile("int main() { int x = 42; int *p = &x; return *p; }").unwrap();
    assert!(asm.contains("leaq"));
    assert!(asm.contains("movq %rax, -12(%rbp)"));
    assert!(asm.contains("movl (%rax), %eax"));
}

#[test]
fn pointer_write() {
    let asm = compile("int main() { int x = 0; int *p = &x; *p = 42; return x; }").unwrap();
    assert!(asm.contains("leaq"));
    assert!(asm.contains("movl %eax, (%rcx)"));
}

#[test]
fn pointer_to_variable() {
    let asm = compile(concat!(
        "int main() { int a = 10; int *p = &a; ",
        "*p = 20; return a; }",
    )).unwrap();
    assert!(asm.contains("leaq"));
}

#[test]
fn address_of_variable() {
    let asm = compile("int main() { int x = 5; int *p = &x; return *p; }").unwrap();
    assert!(asm.contains("leaq -4(%rbp), %rax"));
}

#[test]
fn array_declaration() {
    let asm = compile("int main() { int arr[3]; arr[0] = 10; return arr[0]; }").unwrap();
    assert!(asm.contains("leaq"));
    assert!(asm.contains("shlq $2"));
}

#[test]
fn array_indexing() {
    let asm = compile(concat!(
        "int main() { int arr[3]; arr[0] = 1; arr[1] = 2; arr[2] = 3; ",
        "return arr[1]; }",
    )).unwrap();
    assert!(asm.contains("shlq $2"));
    assert!(asm.contains("addq %rcx, %rax"));
}

#[test]
fn array_sum() {
    let asm = compile(concat!(
        "int main() { int a[3]; a[0] = 10; a[1] = 20; a[2] = 30; ",
        "return a[0] + a[1] + a[2]; }",
    )).unwrap();
    assert!(asm.contains("leaq"));
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn pointer_param() {
    let asm = compile(concat!(
        "int deref(int *p) { return *p; } ",
        "int main() { int x = 99; return deref(&x); }",
    )).unwrap();
    assert!(asm.contains("movq %rdi, -8(%rbp)"));
    assert!(asm.contains("movl (%rax), %eax"));
    assert!(asm.contains("call deref"));
}

#[test]
fn pointer_param_write() {
    let asm = compile(concat!(
        "int set(int *p, int v) { *p = v; return 0; } ",
        "int main() { int x = 0; set(&x, 42); return x; }",
    )).unwrap();
    assert!(asm.contains("movq %rdi, -8(%rbp)"));
    assert!(asm.contains("call set"));
}

#[test]
fn pointer_arithmetic_add() {
    let asm = compile(concat!(
        "int main() { int arr[3]; int *p = &arr[0]; ",
        "arr[0] = 10; arr[1] = 20; arr[2] = 30; ",
        "p = p + 2; return *p; }",
    )).unwrap();
    assert!(asm.contains("shlq $2"));
}

#[test]
fn pointer_arithmetic_subtract() {
    let asm = compile(concat!(
        "int main() { int arr[3]; arr[0] = 5; arr[1] = 10; arr[2] = 15; ",
        "int *p = &arr[2]; p = p - 1; return *p; }",
    )).unwrap();
    assert!(asm.contains("shlq $2"));
    assert!(asm.contains("subq %rcx, %rax"));
}

#[test]
fn array_in_loop() {
    let asm = compile(concat!(
        "int main() { int arr[5]; int i = 0; ",
        "for (i = 0; i < 5; i = i + 1) arr[i] = i; ",
        "return arr[4]; }",
    )).unwrap();
    assert!(asm.contains("shlq $2"));
}

#[test]
fn array_passed_as_pointer() {
    let asm = compile(concat!(
        "int first(int *a) { return *a; } ",
        "int main() { int arr[3]; arr[0] = 42; return first(arr); }",
    )).unwrap();
    assert!(asm.contains("call first"));
}

#[test]
fn nested_dereference() {
    let asm = compile(concat!(
        "int main() { int x = 7; int *p = &x; ",
        "int *q = &x; return *p + *q; }",
    )).unwrap();
    assert!(asm.contains("movl (%rax), %eax"));
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn pointer_comparison() {
    let asm = compile(concat!(
        "int main() { int a = 1; int b = 2; ",
        "int *p = &a; int *q = &a; ",
        "if (p == q) return 1; return 0; }",
    )).unwrap();
    assert!(asm.contains("cmpq"));
}

#[test]
fn array_with_expression_index() {
    let asm = compile(concat!(
        "int main() { int arr[4]; int i = 1; ",
        "arr[i + 1] = 99; return arr[2]; }",
    )).unwrap();
    assert!(asm.contains("shlq $2"));
}

#[test]
fn error_undeclared_pointer() {
    let err = compile("int main() { *p = 42; return 0; }").unwrap_err();
    assert!(err.message.contains("undefined"));
}

#[test]
fn global_int_initialized() {
    let asm = compile("int x = 42; int main() { return x; }").unwrap();
    assert!(asm.contains(".data"));
    assert!(asm.contains(".globl x"));
    assert!(asm.contains("x:"));
    assert!(asm.contains(".long 42"));
    assert!(asm.contains("movl x(%rip), %eax"));
    assert!(asm.contains(".text"));
}

#[test]
fn global_int_uninitialized() {
    let asm = compile("int x; int main() { x = 10; return x; }").unwrap();
    assert!(asm.contains(".comm x,4,4"));
    assert!(asm.contains("movl %eax, x(%rip)"));
    assert!(asm.contains("movl x(%rip), %eax"));
}

#[test]
fn global_int_assignment() {
    let asm = compile("int g = 0; int main() { g = 99; return g; }").unwrap();
    assert!(asm.contains(".long 0"));
    assert!(asm.contains("movl %eax, g(%rip)"));
    assert!(asm.contains("movl g(%rip), %eax"));
}

#[test]
fn global_int_in_expression() {
    let asm = compile("int g = 10; int main() { return g + 5; }").unwrap();
    assert!(asm.contains("movl g(%rip), %eax"));
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn global_pointer() {
    let asm = compile("int *p; int main() { int x = 42; p = &x; return *p; }").unwrap();
    assert!(asm.contains(".comm p,8,8"));
    assert!(asm.contains("movq %rax, p(%rip)"));
    assert!(asm.contains("movq p(%rip), %rax"));
}

#[test]
fn global_array() {
    let asm = compile("int arr[3]; int main() { arr[0] = 7; return arr[0]; }").unwrap();
    assert!(asm.contains(".comm arr,12,4"));
    assert!(asm.contains("leaq arr(%rip), %rax"));
}

#[test]
fn address_of_global() {
    let asm = compile("int g = 5; int main() { int *p = &g; return *p; }").unwrap();
    assert!(asm.contains("leaq g(%rip), %rax"));
    assert!(asm.contains("movl (%rax), %eax"));
}

#[test]
fn global_negative_initializer() {
    let asm = compile("int x = -1; int main() { return x; }").unwrap();
    assert!(asm.contains(".long -1"));
}

#[test]
fn multiple_globals() {
    let asm = compile("int a = 1; int b = 2; int main() { return a + b; }").unwrap();
    assert!(asm.contains(".globl a"));
    assert!(asm.contains("a:"));
    assert!(asm.contains(".long 1"));
    assert!(asm.contains(".globl b"));
    assert!(asm.contains("b:"));
    assert!(asm.contains(".long 2"));
}

#[test]
fn global_used_in_function() {
    let asm = compile(concat!(
        "int counter = 0; ",
        "int increment() { counter = counter + 1; return counter; } ",
        "int main() { increment(); increment(); return counter; }",
    )).unwrap();
    assert!(asm.contains("movl counter(%rip), %eax"));
    assert!(asm.contains("movl %eax, counter(%rip)"));
    assert!(asm.contains("call increment"));
}

#[test]
fn global_and_local_same_name() {
    let asm = compile("int x = 10; int main() { int x = 20; return x; }").unwrap();
    assert!(asm.contains("movl $20, %eax"));
    assert!(asm.contains("movl %eax, -4(%rbp)"));
    assert!(asm.contains("movl -4(%rbp), %eax"));
}

#[test]
fn string_literal_in_function() {
    let asm = compile("int main() { char *s = \"hello\"; return 0; }").unwrap();
    assert!(asm.contains(".section .rodata"));
    assert!(asm.contains(".LC0:"));
    assert!(asm.contains(".string \"hello\""));
    assert!(asm.contains("leaq .LC0(%rip), %rax"));
}

#[test]
fn string_literal_escape_sequences() {
    let asm = compile("int main() { char *s = \"a\\nb\"; return 0; }").unwrap();
    assert!(asm.contains(".string \"a\\nb\""));
}

#[test]
fn string_literal_passed_to_function() {
    let asm = compile(concat!(
        "int first_char(char *s) { return *s; } ",
        "int main() { return first_char(\"A\"); }",
    )).unwrap();
    assert!(asm.contains("leaq .LC0(%rip), %rax"));
    assert!(asm.contains("call first_char"));
}

#[test]
fn global_string_initialization() {
    let asm = compile("char *msg = \"world\"; int main() { return 0; }").unwrap();
    assert!(asm.contains(".data"));
    assert!(asm.contains(".globl msg"));
    assert!(asm.contains("msg:"));
    assert!(asm.contains(".quad .LC0"));
    assert!(asm.contains(".section .rodata"));
    assert!(asm.contains(".LC0:"));
    assert!(asm.contains(".string \"world\""));
}

#[test]
fn char_pointer_param() {
    let asm = compile(concat!(
        "int len(char *s) { return 0; } ",
        "int main() { return len(\"test\"); }",
    )).unwrap();
    assert!(asm.contains("movq %rdi, -8(%rbp)"));
    assert!(asm.contains("call len"));
}

#[test]
fn no_sections_without_globals() {
    let asm = compile("int main() { return 0; }").unwrap();
    assert!(!asm.contains(".data"));
    assert!(!asm.contains(".section .rodata"));
    assert!(!asm.contains(".text"));
    assert!(!asm.contains(".comm"));
}

#[test]
fn full_asm_format_global() {
    let asm = compile("int g = 7; int main() { return g; }").unwrap();
    assert!(asm.starts_with("    .data\n"));
    assert!(asm.contains("    .text\n"));
    assert!(asm.contains("    .globl main\n"));
}

#[test]
fn full_asm_format_pointer() {
    let asm = compile("int main() { int x = 5; int *p = &x; return *p; }").unwrap();
    assert!(asm.contains("pushq %rbp"));
    assert!(asm.contains("movq %rsp, %rbp"));
    assert!(asm.contains("movl $5, %eax"));
    assert!(asm.contains("movl %eax, -4(%rbp)"));
    assert!(asm.contains("leaq -4(%rbp), %rax"));
    assert!(asm.contains("movq %rax, -12(%rbp)"));
    assert!(asm.contains("movq -12(%rbp), %rax"));
    assert!(asm.contains("movl (%rax), %eax"));
    assert!(asm.contains("movq %rbp, %rsp"));
    assert!(asm.contains("popq %rbp"));
    assert!(asm.contains("ret"));
}

#[test]
fn struct_member_write_and_read() {
    let asm = compile(
        "struct Point { int x; int y; }; int main() { struct Point p; p.x = 10; p.y = 20; return p.x; }",
    ).unwrap();
    assert!(asm.contains("leaq"));
    assert!(asm.contains("movl $10, %eax"));
    assert!(asm.contains("movl $20, %eax"));
}

#[test]
fn struct_member_sum() {
    let asm = compile(
        "struct Point { int x; int y; }; int main() { struct Point p; p.x = 3; p.y = 7; return p.x + p.y; }",
    ).unwrap();
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn struct_pointer_arrow_access() {
    let asm = compile(concat!(
        "struct Point { int x; }; ",
        "int main() { struct Point p; p.x = 42; struct Point *pp = &p; return pp->x; }",
    )).unwrap();
    assert!(asm.contains("leaq"));
    assert!(asm.contains("movq"));
}

#[test]
fn struct_pointer_arrow_write() {
    let asm = compile(concat!(
        "struct Point { int x; int y; }; ",
        "int main() { struct Point p; struct Point *pp = &p; pp->x = 5; pp->y = 10; return pp->x + pp->y; }",
    )).unwrap();
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn struct_address_of_member() {
    let asm = compile(concat!(
        "struct Point { int x; int y; }; ",
        "int main() { struct Point p; p.x = 1; p.y = 2; int *q = &p.y; return *q; }",
    )).unwrap();
    assert!(asm.contains("addq $4, %rax"));
}

#[test]
fn struct_passed_as_pointer_param() {
    let asm = compile(concat!(
        "struct Point { int x; int y; }; ",
        "int sum(struct Point *p) { return p->x + p->y; } ",
        "int main() { struct Point pt; pt.x = 3; pt.y = 4; return sum(&pt); }",
    )).unwrap();
    assert!(asm.contains("call sum"));
    assert!(asm.contains("movq %rdi,"));
}

#[test]
fn struct_pointer_param_write() {
    let asm = compile(concat!(
        "struct Point { int x; }; ",
        "int setX(struct Point *p, int v) { p->x = v; return 0; } ",
        "int main() { struct Point pt; setX(&pt, 99); return pt.x; }",
    )).unwrap();
    assert!(asm.contains("call setX"));
}

#[test]
fn union_member_write_and_read() {
    let asm = compile(
        "union Data { int i; int *p; }; int main() { union Data d; d.i = 42; return d.i; }",
    ).unwrap();
    assert!(asm.contains("movl $42, %eax"));
}

#[test]
fn global_struct_variable() {
    let asm = compile(concat!(
        "struct Point { int x; int y; }; ",
        "struct Point g; ",
        "int main() { g.x = 5; g.y = 10; return g.x + g.y; }",
    )).unwrap();
    assert!(asm.contains(".comm g,8,4"));
    assert!(asm.contains("leaq g(%rip), %rax"));
}

#[test]
fn global_struct_pointer() {
    let asm = compile(concat!(
        "struct Point { int x; }; ",
        "struct Point *gp; ",
        "int main() { struct Point p; p.x = 77; gp = &p; return gp->x; }",
    )).unwrap();
    assert!(asm.contains(".comm gp,8,8"));
}

#[test]
fn struct_with_pointer_member() {
    let asm = compile(concat!(
        "struct Node { int val; int *next; }; ",
        "int main() { int x = 42; struct Node n; n.val = 1; n.next = &x; return *n.next; }",
    )).unwrap();
    assert!(asm.contains("movq"));
}

#[test]
fn struct_in_loop() {
    let asm = compile(concat!(
        "struct Counter { int val; }; ",
        "int main() { struct Counter c; c.val = 0; ",
        "while (c.val < 5) c.val = c.val + 1; return c.val; }",
    )).unwrap();
    assert!(asm.contains("jmp .L"));
    assert!(asm.contains("je .L"));
}

#[test]
fn struct_nested_member_access() {
    let asm = compile(concat!(
        "struct Point { int x; int y; }; ",
        "int main() { struct Point a; struct Point b; ",
        "a.x = 1; b.x = 2; return a.x + b.x; }",
    )).unwrap();
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn union_pointer_member() {
    let asm = compile(concat!(
        "union Data { int i; int *p; }; ",
        "int main() { int x = 99; union Data d; d.p = &x; return *d.p; }",
    )).unwrap();
    assert!(asm.contains("movq"));
}

#[test]
fn struct_multiple_functions() {
    let asm = compile(concat!(
        "struct Point { int x; int y; }; ",
        "int getX(struct Point *p) { return p->x; } ",
        "int getY(struct Point *p) { return p->y; } ",
        "int main() { struct Point pt; pt.x = 10; pt.y = 20; return getX(&pt) + getY(&pt); }",
    )).unwrap();
    assert!(asm.contains("call getX"));
    assert!(asm.contains("call getY"));
}

#[test]
fn union_pointer_param() {
    let asm = compile(concat!(
        "union Data { int i; int *p; }; ",
        "int getI(union Data *d) { return d->i; } ",
        "int main() { union Data d; d.i = 55; return getI(&d); }",
    )).unwrap();
    assert!(asm.contains("call getI"));
}

#[test]
fn preprocess_line_comment_in_body() {
    let asm = compile("int main() {\n  // this is a comment\n  return 42;\n}").unwrap();
    assert!(asm.contains("movl $42, %eax"));
}

#[test]
fn preprocess_block_comment_in_body() {
    let asm = compile("int main() { return /* value */ 7; }").unwrap();
    assert!(asm.contains("movl $7, %eax"));
}

#[test]
fn preprocess_comment_between_functions() {
    let asm = compile(concat!(
        "int f() { return 1; }\n",
        "/* separator */\n",
        "int main() { return f(); }",
    )).unwrap();
    assert!(asm.contains("call f"));
}

#[test]
fn preprocess_define_constant_return() {
    let asm = compile("#define VALUE 42\nint main() { return VALUE; }").unwrap();
    assert!(asm.contains("movl $42, %eax"));
}

#[test]
fn preprocess_define_in_expression() {
    let asm = compile("#define A 10\n#define B 20\nint main() { return A + B; }").unwrap();
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn preprocess_undef_then_redefine() {
    let asm = compile(concat!(
        "#define X 1\n",
        "#undef X\n",
        "#define X 99\n",
        "int main() { return X; }",
    )).unwrap();
    assert!(asm.contains("movl $99, %eax"));
}

#[test]
fn preprocess_ifdef_included() {
    let asm = compile(concat!(
        "#define FEATURE\n",
        "#ifdef FEATURE\n",
        "int val() { return 10; }\n",
        "#endif\n",
        "int main() { return val(); }",
    )).unwrap();
    assert!(asm.contains("call val"));
}

#[test]
fn preprocess_ifdef_excluded() {
    let asm = compile(concat!(
        "#ifdef FEATURE\n",
        "int val() { return 10; }\n",
        "#endif\n",
        "int main() { return 0; }",
    )).unwrap();
    assert!(asm.contains("movl $0, %eax"));
    assert!(!asm.contains("call val"));
}

#[test]
fn preprocess_ifndef_included() {
    let asm = compile(concat!(
        "#ifndef FEATURE\n",
        "int val() { return 5; }\n",
        "#endif\n",
        "int main() { return val(); }",
    )).unwrap();
    assert!(asm.contains("call val"));
}

#[test]
fn preprocess_else_branch() {
    let asm = compile(concat!(
        "#ifdef FEATURE\n",
        "int val() { return 1; }\n",
        "#else\n",
        "int val() { return 2; }\n",
        "#endif\n",
        "int main() { return val(); }",
    )).unwrap();
    assert!(asm.contains("movl $2, %eax"));
}

#[test]
fn preprocess_nested_ifdef() {
    let asm = compile(concat!(
        "#define A\n",
        "#define B\n",
        "#ifdef A\n",
        "#ifdef B\n",
        "int val() { return 99; }\n",
        "#endif\n",
        "#endif\n",
        "int main() { return val(); }",
    )).unwrap();
    assert!(asm.contains("movl $99, %eax"));
}

#[test]
fn preprocess_define_empty_flag() {
    let asm = compile(concat!(
        "#define FLAG\n",
        "#ifdef FLAG\n",
        "int main() { return 1; }\n",
        "#else\n",
        "int main() { return 0; }\n",
        "#endif\n",
    )).unwrap();
    assert!(asm.contains("movl $1, %eax"));
}

#[test]
fn preprocess_comment_and_macro_combined() {
    let asm = compile(concat!(
        "#define RET 55\n",
        "int main() {\n",
        "  // return something\n",
        "  return RET; /* the value */\n",
        "}",
    )).unwrap();
    assert!(asm.contains("movl $55, %eax"));
}

#[test]
fn preprocess_multiline_block_comment() {
    let asm = compile(concat!(
        "int main() {\n",
        "  /*\n",
        "   * multi-line\n",
        "   * comment\n",
        "   */\n",
        "  return 3;\n",
        "}",
    )).unwrap();
    assert!(asm.contains("movl $3, %eax"));
}

#[test]
fn preprocess_comment_after_code() {
    let asm = compile("int main() { return 8; } // end").unwrap();
    assert!(asm.contains("movl $8, %eax"));
}

#[test]
fn preprocess_define_parenthesized_expression() {
    let asm = compile(concat!(
        "#define EXPR (2 + 3)\n",
        "int main() { return EXPR; }",
    )).unwrap();
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn preprocess_multiple_defines() {
    let asm = compile(concat!(
        "#define X 3\n",
        "#define Y 4\n",
        "#define Z 5\n",
        "int main() { return X + Y + Z; }",
    )).unwrap();
    assert!(asm.contains("addl %ecx, %eax"));
}

#[test]
fn preprocess_conditional_excludes_function() {
    let asm = compile(concat!(
        "#define USE_SIMPLE\n",
        "#ifdef USE_SIMPLE\n",
        "int compute() { return 1; }\n",
        "#else\n",
        "int compute() { return 2; }\n",
        "#endif\n",
        "int main() { return compute(); }",
    )).unwrap();
    assert!(asm.contains("movl $1, %eax"));
    assert!(asm.contains("call compute"));
}
