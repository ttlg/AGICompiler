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
