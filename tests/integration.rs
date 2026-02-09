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
    assert_eq!(asm, "    .globl main\nmain:\n    movl $7, %eax\n    ret\n");
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
fn error_missing_return() {
    let err = compile("int main() { 42; }").unwrap_err();
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
    assert!(asm.contains("sete %al"));
    assert!(asm.contains("movzbl %al, %eax"));
    assert!(asm.contains("notl %eax"));
    assert!(asm.contains("negl %eax"));
}

#[test]
fn unary_logical_not_zero() {
    let asm = compile("int main() { return !0; }").unwrap();
    assert!(asm.contains("movl $0, %eax"));
    assert!(asm.contains("cmpl $0, %eax"));
    assert!(asm.contains("sete %al"));
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
