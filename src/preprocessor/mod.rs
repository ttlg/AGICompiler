use std::collections::HashMap;

use crate::CompileError;

enum CommentState {
    Normal,
    InLineComment,
    InBlockComment,
    InString,
    InStringEscape,
}

pub fn strip_comments(source: &str) -> Result<String, CompileError> {
    let mut result = String::with_capacity(source.len());
    let mut state = CommentState::Normal;
    let mut chars = source.chars().peekable();
    let mut line = 1;
    let mut col = 1;
    let mut block_start_line = 0;
    let mut block_start_col = 0;

    while let Some(&ch) = chars.peek() {
        match state {
            CommentState::Normal => match ch {
                '/' => {
                    chars.next();
                    match chars.peek() {
                        Some(&'/') => {
                            chars.next();
                            state = CommentState::InLineComment;
                            col += 2;
                        }
                        Some(&'*') => {
                            chars.next();
                            block_start_line = line;
                            block_start_col = col;
                            state = CommentState::InBlockComment;
                            result.push(' ');
                            col += 2;
                        }
                        _ => {
                            result.push('/');
                            col += 1;
                        }
                    }
                }
                '"' => {
                    result.push(ch);
                    chars.next();
                    state = CommentState::InString;
                    col += 1;
                }
                '\n' => {
                    result.push('\n');
                    chars.next();
                    line += 1;
                    col = 1;
                }
                _ => {
                    result.push(ch);
                    chars.next();
                    col += 1;
                }
            },
            CommentState::InLineComment => {
                if ch == '\n' {
                    result.push('\n');
                    chars.next();
                    line += 1;
                    col = 1;
                    state = CommentState::Normal;
                } else {
                    chars.next();
                    col += 1;
                }
            }
            CommentState::InBlockComment => match ch {
                '*' => {
                    chars.next();
                    col += 1;
                    if chars.peek() == Some(&'/') {
                        chars.next();
                        col += 1;
                        state = CommentState::Normal;
                    }
                }
                '\n' => {
                    result.push('\n');
                    chars.next();
                    line += 1;
                    col = 1;
                }
                _ => {
                    chars.next();
                    col += 1;
                }
            },
            CommentState::InString => match ch {
                '\\' => {
                    result.push(ch);
                    chars.next();
                    state = CommentState::InStringEscape;
                    col += 1;
                }
                '"' => {
                    result.push(ch);
                    chars.next();
                    state = CommentState::Normal;
                    col += 1;
                }
                '\n' => {
                    result.push(ch);
                    chars.next();
                    line += 1;
                    col = 1;
                }
                _ => {
                    result.push(ch);
                    chars.next();
                    col += 1;
                }
            },
            CommentState::InStringEscape => {
                result.push(ch);
                chars.next();
                if ch == '\n' {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
                state = CommentState::InString;
            }
        }
    }

    if matches!(state, CommentState::InBlockComment) {
        return Err(CompileError {
            message: "unterminated block comment".to_string(),
            line: block_start_line,
            col: block_start_col,
        });
    }

    Ok(result)
}

fn substitute_macros(line: &str, macros: &HashMap<String, String>) -> String {
    if macros.is_empty() {
        return line.to_string();
    }

    let mut result = line.to_string();
    for _ in 0..100 {
        let mut next = String::with_capacity(result.len());
        let mut chars = result.chars().peekable();
        let mut changed = false;
        let mut in_string = false;

        while let Some(&ch) = chars.peek() {
            if in_string {
                next.push(ch);
                chars.next();
                if ch == '\\' {
                    if let Some(&esc) = chars.peek() {
                        next.push(esc);
                        chars.next();
                    }
                } else if ch == '"' {
                    in_string = false;
                }
                continue;
            }

            if ch == '"' {
                in_string = true;
                next.push(ch);
                chars.next();
                continue;
            }

            if ch.is_ascii_alphabetic() || ch == '_' {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        ident.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if let Some(replacement) = macros.get(&ident) {
                    next.push_str(replacement);
                    changed = true;
                } else {
                    next.push_str(&ident);
                }
            } else {
                next.push(ch);
                chars.next();
            }
        }

        if !changed {
            return next;
        }
        result = next;
    }
    result
}

pub fn preprocess(source: &str) -> Result<String, CompileError> {
    let stripped = strip_comments(source)?;
    let mut macros: HashMap<String, String> = HashMap::new();
    let mut condition_stack: Vec<bool> = Vec::new();
    let mut output_lines: Vec<String> = Vec::new();

    for (line_num, line) in stripped.lines().enumerate() {
        let trimmed = line.trim();

        if let Some(directive) = trimmed.strip_prefix('#') {
            let directive = directive.trim();

            if let Some(rest) = directive.strip_prefix("define") {
                if !rest.is_empty() && !rest.starts_with(char::is_whitespace) {
                    output_lines.push(String::new());
                    continue;
                }
                if condition_stack.iter().all(|&active| active) {
                    let rest = rest.trim();
                    let name_end = rest
                        .find(|c: char| c.is_whitespace())
                        .unwrap_or(rest.len());
                    let name = &rest[..name_end];
                    let value = rest[name_end..].trim();
                    if !name.is_empty() {
                        macros.insert(name.to_string(), value.to_string());
                    }
                }
                output_lines.push(String::new());
            } else if let Some(rest) = directive.strip_prefix("undef") {
                if !rest.is_empty() && !rest.starts_with(char::is_whitespace) {
                    output_lines.push(String::new());
                    continue;
                }
                if condition_stack.iter().all(|&active| active) {
                    let name = rest.trim();
                    macros.remove(name);
                }
                output_lines.push(String::new());
            } else if let Some(rest) = directive.strip_prefix("ifdef") {
                if !rest.is_empty() && !rest.starts_with(char::is_whitespace) {
                    output_lines.push(String::new());
                    continue;
                }
                let name = rest.trim();
                let parent_active = condition_stack.iter().all(|&active| active);
                condition_stack.push(parent_active && macros.contains_key(name));
                output_lines.push(String::new());
            } else if let Some(rest) = directive.strip_prefix("ifndef") {
                if !rest.is_empty() && !rest.starts_with(char::is_whitespace) {
                    output_lines.push(String::new());
                    continue;
                }
                let name = rest.trim();
                let parent_active = condition_stack.iter().all(|&active| active);
                condition_stack.push(parent_active && !macros.contains_key(name));
                output_lines.push(String::new());
            } else if directive == "else" || directive.starts_with("else ") {
                if condition_stack.is_empty() {
                    return Err(CompileError {
                        message: "unmatched #else".to_string(),
                        line: line_num + 1,
                        col: 1,
                    });
                }
                let idx = condition_stack.len() - 1;
                let parent_active = condition_stack[..idx].iter().all(|&active| active);
                condition_stack[idx] = parent_active && !condition_stack[idx];
                output_lines.push(String::new());
            } else if directive == "endif" || directive.starts_with("endif ") {
                if condition_stack.is_empty() {
                    return Err(CompileError {
                        message: "unmatched #endif".to_string(),
                        line: line_num + 1,
                        col: 1,
                    });
                }
                condition_stack.pop();
                output_lines.push(String::new());
            } else {
                output_lines.push(String::new());
            }
        } else if condition_stack.iter().all(|&active| active) {
            output_lines.push(substitute_macros(line, &macros));
        } else {
            output_lines.push(String::new());
        }
    }

    if !condition_stack.is_empty() {
        return Err(CompileError {
            message: "unclosed #ifdef/#ifndef".to_string(),
            line: 0,
            col: 0,
        });
    }

    let mut result = output_lines.join("\n");
    if source.ends_with('\n') && !result.ends_with('\n') {
        result.push('\n');
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strip_line_comment() {
        let result = strip_comments("int x; // comment\nreturn x;").unwrap();
        assert_eq!(result, "int x; \nreturn x;");
    }

    #[test]
    fn strip_block_comment() {
        let result = strip_comments("int /* val */ x;").unwrap();
        assert_eq!(result, "int   x;");
    }

    #[test]
    fn strip_multiline_block_comment() {
        let result = strip_comments("a\n/* line1\nline2 */\nb").unwrap();
        assert_eq!(result, "a\n \n\nb");
    }

    #[test]
    fn comment_in_string_preserved() {
        let result = strip_comments("char *s = \"a // b\";").unwrap();
        assert_eq!(result, "char *s = \"a // b\";");
    }

    #[test]
    fn block_comment_in_string_preserved() {
        let result = strip_comments("char *s = \"a /* b */ c\";").unwrap();
        assert_eq!(result, "char *s = \"a /* b */ c\";");
    }

    #[test]
    fn unterminated_block_comment_error() {
        let err = strip_comments("/* unterminated").unwrap_err();
        assert!(err.message.contains("unterminated block comment"));
    }

    #[test]
    fn define_simple() {
        let result = preprocess("#define VALUE 42\nreturn VALUE;").unwrap();
        assert_eq!(result, "\nreturn 42;");
    }

    #[test]
    fn define_expression_value() {
        let result = preprocess("#define SUM (1 + 2)\nreturn SUM;").unwrap();
        assert_eq!(result, "\nreturn (1 + 2);");
    }

    #[test]
    fn undef_removes_macro() {
        let result = preprocess("#define X 1\n#undef X\nreturn X;").unwrap();
        assert_eq!(result, "\n\nreturn X;");
    }

    #[test]
    fn ifdef_included() {
        let result = preprocess("#define FOO\n#ifdef FOO\nint x = 1;\n#endif").unwrap();
        assert_eq!(result, "\n\nint x = 1;\n");
    }

    #[test]
    fn ifdef_excluded() {
        let result = preprocess("#ifdef FOO\nint x = 1;\n#endif").unwrap();
        assert_eq!(result, "\n\n");
    }

    #[test]
    fn ifndef_included() {
        let result = preprocess("#ifndef FOO\nint x = 1;\n#endif").unwrap();
        assert_eq!(result, "\nint x = 1;\n");
    }

    #[test]
    fn ifndef_excluded() {
        let result = preprocess("#define FOO\n#ifndef FOO\nint x = 1;\n#endif").unwrap();
        assert_eq!(result, "\n\n\n");
    }

    #[test]
    fn else_branch() {
        let result = preprocess("#ifdef FOO\nint x = 1;\n#else\nint x = 2;\n#endif").unwrap();
        assert_eq!(result, "\n\n\nint x = 2;\n");
    }

    #[test]
    fn else_branch_first_active() {
        let result = preprocess("#define FOO\n#ifdef FOO\nint x = 1;\n#else\nint x = 2;\n#endif").unwrap();
        assert_eq!(result, "\n\nint x = 1;\n\n\n");
    }

    #[test]
    fn nested_ifdef() {
        let result = preprocess(
            "#define A\n#define B\n#ifdef A\n#ifdef B\nint x = 1;\n#endif\n#endif",
        )
        .unwrap();
        assert_eq!(result, "\n\n\n\nint x = 1;\n\n");
    }

    #[test]
    fn ifdef_with_undef() {
        let result = preprocess(
            "#define FOO 1\n#undef FOO\n#ifdef FOO\nint x = 1;\n#endif",
        )
        .unwrap();
        assert_eq!(result, "\n\n\n\n");
    }

    #[test]
    fn unmatched_else_error() {
        let err = preprocess("#else").unwrap_err();
        assert!(err.message.contains("unmatched #else"));
    }

    #[test]
    fn unmatched_endif_error() {
        let err = preprocess("#endif").unwrap_err();
        assert!(err.message.contains("unmatched #endif"));
    }

    #[test]
    fn unclosed_ifdef_error() {
        let err = preprocess("#ifdef FOO").unwrap_err();
        assert!(err.message.contains("unclosed"));
    }

    #[test]
    fn macro_in_expression() {
        let result = preprocess("#define A 1\n#define B 2\nreturn A + B;").unwrap();
        assert_eq!(result, "\n\nreturn 1 + 2;");
    }

    #[test]
    fn macro_not_in_string() {
        let result = preprocess("#define X 42\nchar *s = \"X\";").unwrap();
        assert_eq!(result, "\nchar *s = \"X\";");
    }

    #[test]
    fn macro_whole_identifier_only() {
        let result = preprocess("#define X 1\nint XRAY = 0;").unwrap();
        assert_eq!(result, "\nint XRAY = 0;");
    }

    #[test]
    fn multiple_macros() {
        let result = preprocess("#define W 10\n#define H 20\nint area = W * H;").unwrap();
        assert_eq!(result, "\n\nint area = 10 * 20;");
    }

    #[test]
    fn empty_define_flag() {
        let result = preprocess("#define FLAG\n#ifdef FLAG\nint x = 1;\n#endif").unwrap();
        assert_eq!(result, "\n\nint x = 1;\n");
    }

    #[test]
    fn comments_and_macros_combined() {
        let result = preprocess("#define X 5\nint a = X; // use X\nreturn a;").unwrap();
        assert_eq!(result, "\nint a = 5; \nreturn a;");
    }

    #[test]
    fn chained_define() {
        let result = preprocess("#define A B\n#define B 42\nreturn A;").unwrap();
        assert_eq!(result, "\n\nreturn 42;");
    }

    #[test]
    fn line_comment_at_end() {
        let result = strip_comments("int x = 5; // five").unwrap();
        assert_eq!(result, "int x = 5; ");
    }

    #[test]
    fn nested_ifdef_outer_false() {
        let result = preprocess(
            "#ifdef OUTER\n#ifdef INNER\nint x = 1;\n#endif\n#endif",
        )
        .unwrap();
        assert_eq!(result, "\n\n\n\n");
    }

    #[test]
    fn escape_in_string_with_comment_chars() {
        let result = strip_comments("char *s = \"a\\\"//b\";").unwrap();
        assert_eq!(result, "char *s = \"a\\\"//b\";");
    }
}
