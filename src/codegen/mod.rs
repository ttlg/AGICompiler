use std::collections::HashMap;

use crate::CompileError;
use crate::parser::{
    BinaryOperator, DeclType, Expression, Function, GlobalDecl, GlobalInit, MemberType, Program,
    Statement, TypeDef, UnaryOperator,
};

const ARG_REGISTERS_64: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
const ARG_REGISTERS_32: [&str; 6] = ["%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"];

#[derive(Debug, Clone, PartialEq)]
enum MemberVarType {
    Int,
    Pointer,
}

#[derive(Debug, Clone)]
struct MemberInfo {
    name: String,
    offset: usize,
    member_type: MemberVarType,
}

#[derive(Debug, Clone)]
struct CompositeLayout {
    members: Vec<MemberInfo>,
    size: usize,
    align: usize,
}

fn build_composite_layout(type_def: &TypeDef) -> CompositeLayout {
    let mut members = Vec::new();
    if type_def.is_union {
        let mut max_size = 0usize;
        let mut max_align = 1usize;
        for (mt, name) in &type_def.members {
            let size = match mt {
                MemberType::Int => 4,
                MemberType::IntPointer | MemberType::CharPointer => 8,
            };
            members.push(MemberInfo {
                name: name.clone(),
                offset: 0,
                member_type: match mt {
                    MemberType::Int => MemberVarType::Int,
                    MemberType::IntPointer | MemberType::CharPointer => MemberVarType::Pointer,
                },
            });
            max_size = max_size.max(size);
            max_align = max_align.max(size);
        }
        let padded = ((max_size + max_align - 1) / max_align) * max_align;
        CompositeLayout { members, size: padded, align: max_align }
    } else {
        let mut offset = 0usize;
        let mut max_align = 1usize;
        for (mt, name) in &type_def.members {
            let size = match mt {
                MemberType::Int => 4,
                MemberType::IntPointer | MemberType::CharPointer => 8,
            };
            let align = size;
            offset = ((offset + align - 1) / align) * align;
            members.push(MemberInfo {
                name: name.clone(),
                offset,
                member_type: match mt {
                    MemberType::Int => MemberVarType::Int,
                    MemberType::IntPointer | MemberType::CharPointer => MemberVarType::Pointer,
                },
            });
            offset += size;
            max_align = max_align.max(align);
        }
        let padded = ((offset + max_align - 1) / max_align) * max_align;
        CompositeLayout { members, size: padded, align: max_align }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum VarType {
    Int,
    Pointer,
    Array(usize),
    Struct(String),
    Union(String),
    StructPointer(String),
    UnionPointer(String),
}

#[derive(Debug, Clone, PartialEq)]
enum ExprType {
    Int,
    Pointer,
    StructPtr(String),
    UnionPtr(String),
}

#[derive(Debug)]
enum VarLocation {
    Local { offset: i64, var_type: VarType },
    Global { name: String, var_type: VarType },
}

struct Context<'a> {
    scopes: Vec<HashMap<String, (i64, VarType)>>,
    globals: &'a HashMap<String, VarType>,
    string_literals: &'a mut Vec<String>,
    type_registry: &'a HashMap<String, CompositeLayout>,
    stack_offset: i64,
    label_counter: usize,
    stack_depth: usize,
}

impl<'a> Context<'a> {
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &str, var_type: VarType) -> Result<i64, CompileError> {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(name) {
            return Err(CompileError {
                message: format!("variable already declared: {name}"),
                line: 0,
                col: 0,
            });
        }
        let size = match &var_type {
            VarType::Int => 4,
            VarType::Pointer | VarType::StructPointer(_) | VarType::UnionPointer(_) => 8,
            VarType::Array(n) => (*n as i64) * 4,
            VarType::Struct(type_name) | VarType::Union(type_name) => {
                self.type_registry.get(type_name).map(|l| l.size as i64).unwrap_or(0)
            }
        };
        self.stack_offset += size;
        scope.insert(name.to_string(), (self.stack_offset, var_type));
        Ok(self.stack_offset)
    }

    fn lookup(&self, name: &str) -> Result<VarLocation, CompileError> {
        for scope in self.scopes.iter().rev() {
            if let Some((offset, var_type)) = scope.get(name) {
                return Ok(VarLocation::Local { offset: *offset, var_type: var_type.clone() });
            }
        }
        if let Some(var_type) = self.globals.get(name) {
            return Ok(VarLocation::Global { name: name.to_string(), var_type: var_type.clone() });
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

fn decl_type_to_var_type(decl_type: &DeclType) -> VarType {
    match decl_type {
        DeclType::Int => VarType::Int,
        DeclType::IntPointer | DeclType::CharPointer => VarType::Pointer,
        DeclType::IntArray(n) => VarType::Array(*n as usize),
        DeclType::Struct(name) => VarType::Struct(name.clone()),
        DeclType::Union(name) => VarType::Union(name.clone()),
        DeclType::StructPointer(name) => VarType::StructPointer(name.clone()),
        DeclType::UnionPointer(name) => VarType::UnionPointer(name.clone()),
    }
}

fn decl_type_bytes(
    decl_type: &DeclType,
    type_registry: &HashMap<String, CompositeLayout>,
) -> usize {
    match decl_type {
        DeclType::Int => 4,
        DeclType::IntPointer | DeclType::CharPointer
        | DeclType::StructPointer(_) | DeclType::UnionPointer(_) => 8,
        DeclType::IntArray(n) => (*n as usize) * 4,
        DeclType::Struct(name) | DeclType::Union(name) => {
            type_registry.get(name).map(|l| l.size).unwrap_or(0)
        }
    }
}

fn zero_test(expr_type: &ExprType) -> String {
    match expr_type {
        ExprType::Int => "    cmpl $0, %eax".to_string(),
        ExprType::Pointer | ExprType::StructPtr(_) | ExprType::UnionPtr(_) => {
            "    testq %rax, %rax".to_string()
        }
    }
}

fn escape_for_asm(s: &str) -> String {
    let mut result = String::new();
    for c in s.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\t' => result.push_str("\\t"),
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\0' => result.push_str("\\0"),
            c => result.push(c),
        }
    }
    result
}

fn find_member(
    type_registry: &HashMap<String, CompositeLayout>,
    expr_type: &ExprType,
    member: &str,
) -> Result<(usize, MemberVarType), CompileError> {
    let type_name = match expr_type {
        ExprType::StructPtr(name) | ExprType::UnionPtr(name) => name,
        _ => {
            return Err(CompileError {
                message: "member access on non-struct/union type".to_string(),
                line: 0,
                col: 0,
            });
        }
    };
    let layout = type_registry.get(type_name).ok_or_else(|| CompileError {
        message: format!("undefined type: {type_name}"),
        line: 0,
        col: 0,
    })?;
    let info = layout
        .members
        .iter()
        .find(|m| m.name == member)
        .ok_or_else(|| CompileError {
            message: format!("no member '{member}' in type '{type_name}'"),
            line: 0,
            col: 0,
        })?;
    Ok((info.offset, info.member_type.clone()))
}

fn generate_var_read(loc: &VarLocation) -> (Vec<String>, ExprType) {
    match loc {
        VarLocation::Local { offset, var_type } => match var_type {
            VarType::Int => (vec![format!("    movl -{offset}(%rbp), %eax")], ExprType::Int),
            VarType::Pointer => (vec![format!("    movq -{offset}(%rbp), %rax")], ExprType::Pointer),
            VarType::Array(_) => (vec![format!("    leaq -{offset}(%rbp), %rax")], ExprType::Pointer),
            VarType::Struct(name) => {
                (vec![format!("    leaq -{offset}(%rbp), %rax")], ExprType::StructPtr(name.clone()))
            }
            VarType::Union(name) => {
                (vec![format!("    leaq -{offset}(%rbp), %rax")], ExprType::UnionPtr(name.clone()))
            }
            VarType::StructPointer(name) => {
                (vec![format!("    movq -{offset}(%rbp), %rax")], ExprType::StructPtr(name.clone()))
            }
            VarType::UnionPointer(name) => {
                (vec![format!("    movq -{offset}(%rbp), %rax")], ExprType::UnionPtr(name.clone()))
            }
        },
        VarLocation::Global { name, var_type } => match var_type {
            VarType::Int => (vec![format!("    movl {name}(%rip), %eax")], ExprType::Int),
            VarType::Pointer => (vec![format!("    movq {name}(%rip), %rax")], ExprType::Pointer),
            VarType::Array(_) => (vec![format!("    leaq {name}(%rip), %rax")], ExprType::Pointer),
            VarType::Struct(type_name) => {
                (vec![format!("    leaq {name}(%rip), %rax")], ExprType::StructPtr(type_name.clone()))
            }
            VarType::Union(type_name) => {
                (vec![format!("    leaq {name}(%rip), %rax")], ExprType::UnionPtr(type_name.clone()))
            }
            VarType::StructPointer(type_name) => {
                (vec![format!("    movq {name}(%rip), %rax")], ExprType::StructPtr(type_name.clone()))
            }
            VarType::UnionPointer(type_name) => {
                (vec![format!("    movq {name}(%rip), %rax")], ExprType::UnionPtr(type_name.clone()))
            }
        },
    }
}

fn generate_member_load(
    ctx: &mut Context,
    base_expr: &Expression,
    member: &str,
) -> Result<(Vec<String>, ExprType), CompileError> {
    let (mut instructions, expr_type) = generate_expression(ctx, base_expr)?;
    let (offset, member_type) = find_member(ctx.type_registry, &expr_type, member)?;
    match member_type {
        MemberVarType::Int => {
            instructions.push(format!("    movl {offset}(%rax), %eax"));
            Ok((instructions, ExprType::Int))
        }
        MemberVarType::Pointer => {
            instructions.push(format!("    movq {offset}(%rax), %rax"));
            Ok((instructions, ExprType::Pointer))
        }
    }
}

fn generate_member_store(
    ctx: &mut Context,
    base_expr: &Expression,
    member: &str,
    value: &Expression,
) -> Result<(Vec<String>, ExprType), CompileError> {
    let (mut instructions, _) = generate_expression(ctx, value)?;
    instructions.push("    pushq %rax".to_string());
    ctx.stack_depth += 1;
    let (base_insts, expr_type) = generate_expression(ctx, base_expr)?;
    instructions.extend(base_insts);
    let (offset, member_type) = find_member(ctx.type_registry, &expr_type, member)?;
    instructions.push("    movq %rax, %rcx".to_string());
    instructions.push("    popq %rax".to_string());
    ctx.stack_depth -= 1;
    match member_type {
        MemberVarType::Int => {
            instructions.push(format!("    movl %eax, {offset}(%rcx)"));
            Ok((instructions, ExprType::Int))
        }
        MemberVarType::Pointer => {
            instructions.push(format!("    movq %rax, {offset}(%rcx)"));
            Ok((instructions, ExprType::Pointer))
        }
    }
}

fn generate_member_addr(
    ctx: &mut Context,
    base_expr: &Expression,
    member: &str,
) -> Result<(Vec<String>, ExprType), CompileError> {
    let (mut instructions, expr_type) = generate_expression(ctx, base_expr)?;
    let (offset, _) = find_member(ctx.type_registry, &expr_type, member)?;
    if offset > 0 {
        instructions.push(format!("    addq ${offset}, %rax"));
    }
    Ok((instructions, ExprType::Pointer))
}

fn generate_expression(
    ctx: &mut Context,
    expr: &Expression,
) -> Result<(Vec<String>, ExprType), CompileError> {
    match expr {
        Expression::IntLiteral(value) => {
            Ok((vec![format!("    movl ${value}, %eax")], ExprType::Int))
        }
        Expression::StringLiteral(s) => {
            let label_idx = ctx.string_literals.len();
            ctx.string_literals.push(s.clone());
            Ok((vec![format!("    leaq .LC{label_idx}(%rip), %rax")], ExprType::Pointer))
        }
        Expression::Variable(name) => {
            let loc = ctx.lookup(name)?;
            Ok(generate_var_read(&loc))
        }
        Expression::Assignment { target, value } => {
            match target.as_ref() {
                Expression::Variable(name) => {
                    let (mut instructions, _) = generate_expression(ctx, value)?;
                    let loc = ctx.lookup(name)?;
                    match loc {
                        VarLocation::Local { offset, var_type } => match var_type {
                            VarType::Int => {
                                instructions.push(format!("    movl %eax, -{offset}(%rbp)"));
                                Ok((instructions, ExprType::Int))
                            }
                            VarType::Pointer => {
                                instructions.push(format!("    movq %rax, -{offset}(%rbp)"));
                                Ok((instructions, ExprType::Pointer))
                            }
                            VarType::StructPointer(type_name) => {
                                instructions.push(format!("    movq %rax, -{offset}(%rbp)"));
                                Ok((instructions, ExprType::StructPtr(type_name)))
                            }
                            VarType::UnionPointer(type_name) => {
                                instructions.push(format!("    movq %rax, -{offset}(%rbp)"));
                                Ok((instructions, ExprType::UnionPtr(type_name)))
                            }
                            VarType::Array(_) | VarType::Struct(_) | VarType::Union(_) => {
                                Err(CompileError {
                                    message: "cannot assign to this type".to_string(),
                                    line: 0,
                                    col: 0,
                                })
                            }
                        },
                        VarLocation::Global { name: gname, var_type } => match var_type {
                            VarType::Int => {
                                instructions.push(format!("    movl %eax, {gname}(%rip)"));
                                Ok((instructions, ExprType::Int))
                            }
                            VarType::Pointer => {
                                instructions.push(format!("    movq %rax, {gname}(%rip)"));
                                Ok((instructions, ExprType::Pointer))
                            }
                            VarType::StructPointer(type_name) => {
                                instructions.push(format!("    movq %rax, {gname}(%rip)"));
                                Ok((instructions, ExprType::StructPtr(type_name)))
                            }
                            VarType::UnionPointer(type_name) => {
                                instructions.push(format!("    movq %rax, {gname}(%rip)"));
                                Ok((instructions, ExprType::UnionPtr(type_name)))
                            }
                            VarType::Array(_) | VarType::Struct(_) | VarType::Union(_) => {
                                Err(CompileError {
                                    message: "cannot assign to this type".to_string(),
                                    line: 0,
                                    col: 0,
                                })
                            }
                        },
                    }
                }
                Expression::Dereference { operand } => {
                    let (mut instructions, _) = generate_expression(ctx, value)?;
                    instructions.push("    pushq %rax".to_string());
                    ctx.stack_depth += 1;
                    let (ptr_insts, _) = generate_expression(ctx, operand)?;
                    instructions.extend(ptr_insts);
                    instructions.push("    movq %rax, %rcx".to_string());
                    instructions.push("    popq %rax".to_string());
                    ctx.stack_depth -= 1;
                    instructions.push("    movl %eax, (%rcx)".to_string());
                    Ok((instructions, ExprType::Int))
                }
                Expression::ArrayIndex { array, index } => {
                    let (mut instructions, _) = generate_expression(ctx, value)?;
                    instructions.push("    pushq %rax".to_string());
                    ctx.stack_depth += 1;
                    let (array_insts, _) = generate_expression(ctx, array)?;
                    instructions.extend(array_insts);
                    instructions.push("    pushq %rax".to_string());
                    ctx.stack_depth += 1;
                    let (index_insts, _) = generate_expression(ctx, index)?;
                    instructions.extend(index_insts);
                    instructions.push("    movslq %eax, %rax".to_string());
                    instructions.push("    shlq $2, %rax".to_string());
                    instructions.push("    popq %rcx".to_string());
                    ctx.stack_depth -= 1;
                    instructions.push("    addq %rcx, %rax".to_string());
                    instructions.push("    movq %rax, %rcx".to_string());
                    instructions.push("    popq %rax".to_string());
                    ctx.stack_depth -= 1;
                    instructions.push("    movl %eax, (%rcx)".to_string());
                    Ok((instructions, ExprType::Int))
                }
                Expression::MemberAccess { object, member } => {
                    generate_member_store(ctx, object, member, value)
                }
                Expression::ArrowAccess { pointer, member } => {
                    generate_member_store(ctx, pointer, member, value)
                }
                _ => Err(CompileError {
                    message: "invalid assignment target".to_string(),
                    line: 0,
                    col: 0,
                }),
            }
        }
        Expression::AddressOf { operand } => {
            match operand.as_ref() {
                Expression::Variable(name) => {
                    let loc = ctx.lookup(name)?;
                    match loc {
                        VarLocation::Local { offset, .. } => {
                            Ok((vec![format!("    leaq -{offset}(%rbp), %rax")], ExprType::Pointer))
                        }
                        VarLocation::Global { name, .. } => {
                            Ok((vec![format!("    leaq {name}(%rip), %rax")], ExprType::Pointer))
                        }
                    }
                }
                Expression::ArrayIndex { array, index } => {
                    let (mut instructions, _) = generate_expression(ctx, array)?;
                    instructions.push("    pushq %rax".to_string());
                    ctx.stack_depth += 1;
                    let (index_insts, _) = generate_expression(ctx, index)?;
                    instructions.extend(index_insts);
                    instructions.push("    movslq %eax, %rax".to_string());
                    instructions.push("    shlq $2, %rax".to_string());
                    instructions.push("    popq %rcx".to_string());
                    ctx.stack_depth -= 1;
                    instructions.push("    addq %rcx, %rax".to_string());
                    Ok((instructions, ExprType::Pointer))
                }
                Expression::Dereference { operand: inner } => generate_expression(ctx, inner),
                Expression::MemberAccess { object, member } => {
                    generate_member_addr(ctx, object, member)
                }
                Expression::ArrowAccess { pointer, member } => {
                    generate_member_addr(ctx, pointer, member)
                }
                _ => Err(CompileError {
                    message: "cannot take address of this expression".to_string(),
                    line: 0,
                    col: 0,
                }),
            }
        }
        Expression::Dereference { operand } => {
            let (mut instructions, _) = generate_expression(ctx, operand)?;
            instructions.push("    movl (%rax), %eax".to_string());
            Ok((instructions, ExprType::Int))
        }
        Expression::ArrayIndex { array, index } => {
            let (mut instructions, _) = generate_expression(ctx, array)?;
            instructions.push("    pushq %rax".to_string());
            ctx.stack_depth += 1;
            let (index_insts, _) = generate_expression(ctx, index)?;
            instructions.extend(index_insts);
            instructions.push("    movslq %eax, %rax".to_string());
            instructions.push("    shlq $2, %rax".to_string());
            instructions.push("    popq %rcx".to_string());
            ctx.stack_depth -= 1;
            instructions.push("    addq %rcx, %rax".to_string());
            instructions.push("    movl (%rax), %eax".to_string());
            Ok((instructions, ExprType::Int))
        }
        Expression::MemberAccess { object, member } => {
            generate_member_load(ctx, object, member)
        }
        Expression::ArrowAccess { pointer, member } => {
            generate_member_load(ctx, pointer, member)
        }
        Expression::UnaryOp { operator, operand } => {
            let (mut instructions, operand_type) = generate_expression(ctx, operand)?;
            match operator {
                UnaryOperator::Negate => {
                    instructions.push("    negl %eax".to_string());
                }
                UnaryOperator::BitwiseNot => {
                    instructions.push("    notl %eax".to_string());
                }
                UnaryOperator::LogicalNot => {
                    instructions.push(zero_test(&operand_type));
                    instructions.push("    sete %al".to_string());
                    instructions.push("    movzbl %al, %eax".to_string());
                }
            }
            Ok((instructions, ExprType::Int))
        }
        Expression::BinaryOp { operator, left, right } => {
            match operator {
                BinaryOperator::LogicalAnd => {
                    let false_label = ctx.next_label();
                    let end_label = ctx.next_label();
                    let (mut instructions, left_type) = generate_expression(ctx, left)?;
                    instructions.push(zero_test(&left_type));
                    instructions.push(format!("    je {false_label}"));
                    let (right_insts, right_type) = generate_expression(ctx, right)?;
                    instructions.extend(right_insts);
                    instructions.push(zero_test(&right_type));
                    instructions.push(format!("    je {false_label}"));
                    instructions.push("    movl $1, %eax".to_string());
                    instructions.push(format!("    jmp {end_label}"));
                    instructions.push(format!("{false_label}:"));
                    instructions.push("    movl $0, %eax".to_string());
                    instructions.push(format!("{end_label}:"));
                    Ok((instructions, ExprType::Int))
                }
                BinaryOperator::LogicalOr => {
                    let true_label = ctx.next_label();
                    let end_label = ctx.next_label();
                    let (mut instructions, left_type) = generate_expression(ctx, left)?;
                    instructions.push(zero_test(&left_type));
                    instructions.push(format!("    jne {true_label}"));
                    let (right_insts, right_type) = generate_expression(ctx, right)?;
                    instructions.extend(right_insts);
                    instructions.push(zero_test(&right_type));
                    instructions.push(format!("    jne {true_label}"));
                    instructions.push("    movl $0, %eax".to_string());
                    instructions.push(format!("    jmp {end_label}"));
                    instructions.push(format!("{true_label}:"));
                    instructions.push("    movl $1, %eax".to_string());
                    instructions.push(format!("{end_label}:"));
                    Ok((instructions, ExprType::Int))
                }
                _ => {
                    let (mut instructions, left_type) = generate_expression(ctx, left)?;
                    instructions.push("    pushq %rax".to_string());
                    ctx.stack_depth += 1;
                    let (right_insts, right_type) = generate_expression(ctx, right)?;
                    instructions.extend(right_insts);
                    instructions.push("    movq %rax, %rcx".to_string());
                    instructions.push("    popq %rax".to_string());
                    ctx.stack_depth -= 1;

                    match operator {
                        BinaryOperator::Add => {
                            match (&left_type, &right_type) {
                                (ExprType::Int, ExprType::Int) => {
                                    instructions.push("    addl %ecx, %eax".to_string());
                                    Ok((instructions, ExprType::Int))
                                }
                                (ExprType::Pointer, ExprType::Int) => {
                                    instructions.push("    movslq %ecx, %rcx".to_string());
                                    instructions.push("    shlq $2, %rcx".to_string());
                                    instructions.push("    addq %rcx, %rax".to_string());
                                    Ok((instructions, ExprType::Pointer))
                                }
                                (ExprType::Int, ExprType::Pointer) => {
                                    instructions.push("    movslq %eax, %rax".to_string());
                                    instructions.push("    shlq $2, %rax".to_string());
                                    instructions.push("    addq %rcx, %rax".to_string());
                                    Ok((instructions, ExprType::Pointer))
                                }
                                _ => Err(CompileError {
                                    message: "cannot add two pointers".to_string(),
                                    line: 0,
                                    col: 0,
                                }),
                            }
                        }
                        BinaryOperator::Subtract => {
                            match (&left_type, &right_type) {
                                (ExprType::Int, ExprType::Int) => {
                                    instructions.push("    subl %ecx, %eax".to_string());
                                    Ok((instructions, ExprType::Int))
                                }
                                (ExprType::Pointer, ExprType::Int) => {
                                    instructions.push("    movslq %ecx, %rcx".to_string());
                                    instructions.push("    shlq $2, %rcx".to_string());
                                    instructions.push("    subq %rcx, %rax".to_string());
                                    Ok((instructions, ExprType::Pointer))
                                }
                                (ExprType::Pointer, ExprType::Pointer) => {
                                    instructions.push("    subq %rcx, %rax".to_string());
                                    instructions.push("    sarq $2, %rax".to_string());
                                    Ok((instructions, ExprType::Int))
                                }
                                _ => Err(CompileError {
                                    message: "cannot subtract pointer from integer".to_string(),
                                    line: 0,
                                    col: 0,
                                }),
                            }
                        }
                        BinaryOperator::Multiply => {
                            instructions.push("    imull %ecx, %eax".to_string());
                            Ok((instructions, ExprType::Int))
                        }
                        BinaryOperator::Divide => {
                            instructions.push("    cdq".to_string());
                            instructions.push("    idivl %ecx".to_string());
                            Ok((instructions, ExprType::Int))
                        }
                        BinaryOperator::Modulo => {
                            instructions.push("    cdq".to_string());
                            instructions.push("    idivl %ecx".to_string());
                            instructions.push("    movl %edx, %eax".to_string());
                            Ok((instructions, ExprType::Int))
                        }
                        BinaryOperator::Equal
                        | BinaryOperator::NotEqual
                        | BinaryOperator::LessThan
                        | BinaryOperator::GreaterThan
                        | BinaryOperator::LessEqual
                        | BinaryOperator::GreaterEqual => {
                            let cmp = if left_type == ExprType::Int && right_type == ExprType::Int {
                                "    cmpl %ecx, %eax"
                            } else {
                                "    cmpq %rcx, %rax"
                            };
                            instructions.push(cmp.to_string());
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
                            Ok((instructions, ExprType::Int))
                        }
                        BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => unreachable!(),
                    }
                }
            }
        }
        Expression::FunctionCall { name, arguments } => {
            if arguments.len() > 6 {
                return Err(CompileError {
                    message: format!("too many arguments (max 6): {name}"),
                    line: 0,
                    col: 0,
                });
            }
            let mut instructions = Vec::new();
            for arg in arguments {
                let (arg_insts, _) = generate_expression(ctx, arg)?;
                instructions.extend(arg_insts);
                instructions.push("    pushq %rax".to_string());
                ctx.stack_depth += 1;
            }
            for i in (0..arguments.len()).rev() {
                instructions.push(format!("    popq {}", ARG_REGISTERS_64[i]));
                ctx.stack_depth -= 1;
            }
            let needs_align = ctx.stack_depth % 2 != 0;
            if needs_align {
                instructions.push("    subq $8, %rsp".to_string());
            }
            instructions.push(format!("    call {name}"));
            if needs_align {
                instructions.push("    addq $8, %rsp".to_string());
            }
            Ok((instructions, ExprType::Int))
        }
        Expression::Ternary { condition, then_expr, else_expr } => {
            let else_label = ctx.next_label();
            let end_label = ctx.next_label();
            let (mut instructions, cond_type) = generate_expression(ctx, condition)?;
            instructions.push(zero_test(&cond_type));
            instructions.push(format!("    je {else_label}"));
            let (then_insts, then_type) = generate_expression(ctx, then_expr)?;
            instructions.extend(then_insts);
            instructions.push(format!("    jmp {end_label}"));
            instructions.push(format!("{else_label}:"));
            let (else_insts, _) = generate_expression(ctx, else_expr)?;
            instructions.extend(else_insts);
            instructions.push(format!("{end_label}:"));
            Ok((instructions, then_type))
        }
    }
}

fn generate_statement(ctx: &mut Context, stmt: &Statement) -> Result<Vec<String>, CompileError> {
    match stmt {
        Statement::Return(expr) => {
            let (mut instructions, _) = generate_expression(ctx, expr)?;
            instructions.push("    movq %rbp, %rsp".to_string());
            instructions.push("    popq %rbp".to_string());
            instructions.push("    ret".to_string());
            Ok(instructions)
        }
        Statement::Declaration { decl_type, name, initializer } => {
            let var_type = decl_type_to_var_type(decl_type);
            let offset = ctx.declare(name, var_type.clone())?;
            if let Some(expr) = initializer {
                let (mut instructions, _) = generate_expression(ctx, expr)?;
                match var_type {
                    VarType::Int => instructions.push(format!("    movl %eax, -{offset}(%rbp)")),
                    VarType::Pointer | VarType::StructPointer(_) | VarType::UnionPointer(_) => {
                        instructions.push(format!("    movq %rax, -{offset}(%rbp)"))
                    }
                    VarType::Array(_) | VarType::Struct(_) | VarType::Union(_) => {
                        return Err(CompileError {
                            message: "cannot initialize this type".to_string(),
                            line: 0,
                            col: 0,
                        });
                    }
                }
                Ok(instructions)
            } else {
                Ok(vec![])
            }
        }
        Statement::Expression(expr) => {
            let (instructions, _) = generate_expression(ctx, expr)?;
            Ok(instructions)
        }
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
            let (mut instructions, cond_type) = generate_expression(ctx, condition)?;
            instructions.push(zero_test(&cond_type));
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
            let (cond_insts, cond_type) = generate_expression(ctx, condition)?;
            instructions.extend(cond_insts);
            instructions.push(zero_test(&cond_type));
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
            let (cond_insts, cond_type) = generate_expression(ctx, condition)?;
            instructions.extend(cond_insts);
            instructions.push(zero_test(&cond_type));
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
                let (cond_insts, cond_type) = generate_expression(ctx, condition)?;
                instructions.extend(cond_insts);
                instructions.push(zero_test(&cond_type));
                instructions.push(format!("    je {end_label}"));
            }
            instructions.extend(generate_statement(ctx, body)?);
            if let Some(post) = post {
                let (post_insts, _) = generate_expression(ctx, post)?;
                instructions.extend(post_insts);
            }
            instructions.push(format!("    jmp {start_label}"));
            instructions.push(format!("{end_label}:"));
            ctx.pop_scope();
            Ok(instructions)
        }
    }
}

fn count_stack_bytes_in_statement(
    stmt: &Statement,
    type_registry: &HashMap<String, CompositeLayout>,
) -> usize {
    match stmt {
        Statement::Declaration { decl_type, .. } => decl_type_bytes(decl_type, type_registry),
        Statement::Compound(stmts) => count_stack_bytes(stmts, type_registry),
        Statement::If { then_branch, else_branch, .. } => {
            let mut count = count_stack_bytes_in_statement(then_branch, type_registry);
            if let Some(else_branch) = else_branch {
                count += count_stack_bytes_in_statement(else_branch, type_registry);
            }
            count
        }
        Statement::While { body, .. } | Statement::DoWhile { body, .. } => {
            count_stack_bytes_in_statement(body, type_registry)
        }
        Statement::For { init, body, .. } => {
            let mut count = 0;
            if let Some(init) = init {
                count += count_stack_bytes_in_statement(init, type_registry);
            }
            count += count_stack_bytes_in_statement(body, type_registry);
            count
        }
        _ => 0,
    }
}

fn count_stack_bytes(
    statements: &[Statement],
    type_registry: &HashMap<String, CompositeLayout>,
) -> usize {
    statements
        .iter()
        .map(|s| count_stack_bytes_in_statement(s, type_registry))
        .sum()
}

fn generate_function(
    function: &Function,
    label_counter: usize,
    globals: &HashMap<String, VarType>,
    string_literals: &mut Vec<String>,
    type_registry: &HashMap<String, CompositeLayout>,
) -> Result<(Vec<String>, usize), CompileError> {
    let body_bytes = count_stack_bytes(&function.body, type_registry);
    let param_bytes: usize = function
        .params
        .iter()
        .map(|(dt, _)| decl_type_bytes(dt, type_registry))
        .sum();
    let total_bytes = body_bytes + param_bytes;
    let stack_size = ((total_bytes + 15) / 16) * 16;

    let mut ctx = Context {
        scopes: vec![HashMap::new()],
        globals,
        string_literals,
        type_registry,
        stack_offset: 0,
        label_counter,
        stack_depth: 0,
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

    for (i, (decl_type, param)) in function.params.iter().enumerate() {
        let var_type = decl_type_to_var_type(decl_type);
        let offset = ctx.declare(param, var_type)?;
        match decl_type {
            DeclType::Int => {
                lines.push(format!("    movl {}, -{offset}(%rbp)", ARG_REGISTERS_32[i]));
            }
            DeclType::IntPointer | DeclType::CharPointer
            | DeclType::StructPointer(_) | DeclType::UnionPointer(_) => {
                lines.push(format!("    movq {}, -{offset}(%rbp)", ARG_REGISTERS_64[i]));
            }
            DeclType::IntArray(_) | DeclType::Struct(_) | DeclType::Union(_) => unreachable!(),
        }
    }

    for stmt in &function.body {
        lines.extend(generate_statement(&mut ctx, stmt)?);
    }

    Ok((lines, ctx.label_counter))
}

fn generate_globals(
    globals: &[GlobalDecl],
    globals_map: &mut HashMap<String, VarType>,
    string_literals: &mut Vec<String>,
    type_registry: &HashMap<String, CompositeLayout>,
) -> (Vec<String>, Vec<String>) {
    let mut data_lines = Vec::new();
    let mut bss_lines = Vec::new();

    for global in globals {
        let var_type = decl_type_to_var_type(&global.decl_type);
        globals_map.insert(global.name.clone(), var_type);

        match &global.initializer {
            Some(init) => {
                data_lines.push(format!("    .globl {}", global.name));
                data_lines.push(format!("{}:", global.name));
                match init {
                    GlobalInit::Integer(n) => match &global.decl_type {
                        DeclType::Int => data_lines.push(format!("    .long {n}")),
                        DeclType::IntPointer | DeclType::CharPointer
                        | DeclType::StructPointer(_) | DeclType::UnionPointer(_) => {
                            data_lines.push(format!("    .quad {n}"))
                        }
                        DeclType::IntArray(_) | DeclType::Struct(_) | DeclType::Union(_) => {}
                    },
                    GlobalInit::StringLiteral(s) => {
                        let str_label = format!(".LC{}", string_literals.len());
                        string_literals.push(s.clone());
                        data_lines.push(format!("    .quad {str_label}"));
                    }
                }
            }
            None => {
                let size = decl_type_bytes(&global.decl_type, type_registry);
                let align = match &global.decl_type {
                    DeclType::IntPointer | DeclType::CharPointer
                    | DeclType::StructPointer(_) | DeclType::UnionPointer(_) => 8,
                    DeclType::Struct(name) | DeclType::Union(name) => {
                        type_registry.get(name).map(|l| l.align).unwrap_or(4)
                    }
                    _ => 4,
                };
                bss_lines.push(format!("    .comm {},{},{}", global.name, size, align));
            }
        }
    }

    (data_lines, bss_lines)
}

pub fn generate(program: &Program) -> Result<String, CompileError> {
    let mut all_lines = Vec::new();
    let mut label_counter = 0;
    let mut string_literals: Vec<String> = Vec::new();
    let mut globals_map: HashMap<String, VarType> = HashMap::new();

    let mut type_registry: HashMap<String, CompositeLayout> = HashMap::new();
    for type_def in &program.type_defs {
        type_registry.insert(type_def.name.clone(), build_composite_layout(type_def));
    }

    let (data_lines, bss_lines) = generate_globals(
        &program.globals,
        &mut globals_map,
        &mut string_literals,
        &type_registry,
    );

    let mut function_lines = Vec::new();
    for function in &program.functions {
        let (lines, new_counter) = generate_function(
            function,
            label_counter,
            &globals_map,
            &mut string_literals,
            &type_registry,
        )?;
        function_lines.extend(lines);
        label_counter = new_counter;
    }

    let has_data = !data_lines.is_empty();
    let has_bss = !bss_lines.is_empty();

    if has_data {
        all_lines.push("    .data".to_string());
        all_lines.extend(data_lines);
    }

    all_lines.extend(bss_lines);

    if !string_literals.is_empty() {
        all_lines.push("    .section .rodata".to_string());
        for (i, s) in string_literals.iter().enumerate() {
            all_lines.push(format!(".LC{i}:"));
            all_lines.push(format!("    .string \"{}\"", escape_for_asm(s)));
        }
    }

    if has_data || !string_literals.is_empty() || has_bss {
        all_lines.push("    .text".to_string());
    }

    all_lines.extend(function_lines);
    all_lines.push(String::new());
    Ok(all_lines.join("\n"))
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

    #[test]
    fn generate_function_call_no_args() {
        let asm = compile("int foo() { return 42; } int main() { return foo(); }");
        assert!(asm.contains(".globl foo"));
        assert!(asm.contains("foo:"));
        assert!(asm.contains("call foo"));
    }

    #[test]
    fn generate_function_call_with_args() {
        let asm = compile("int add(int a, int b) { return a + b; } int main() { return add(1, 2); }");
        assert!(asm.contains("movl %edi, -4(%rbp)"));
        assert!(asm.contains("movl %esi, -8(%rbp)"));
        assert!(asm.contains("popq %rsi"));
        assert!(asm.contains("popq %rdi"));
        assert!(asm.contains("call add"));
    }

    #[test]
    fn generate_multiple_functions() {
        let asm = compile("int a() { return 1; } int b() { return 2; } int main() { return 0; }");
        assert!(asm.contains(".globl a"));
        assert!(asm.contains(".globl b"));
        assert!(asm.contains(".globl main"));
    }

    #[test]
    fn generate_function_params_stack_allocation() {
        let asm = compile("int f(int x) { return x; }");
        assert!(asm.contains("subq $16, %rsp"));
        assert!(asm.contains("movl %edi, -4(%rbp)"));
        assert!(asm.contains("movl -4(%rbp), %eax"));
    }

    #[test]
    fn generate_function_call_alignment() {
        let asm = compile("int f() { return 0; } int main() { return 1 + f(); }");
        assert!(asm.contains("subq $8, %rsp"));
        assert!(asm.contains("call f"));
        assert!(asm.contains("addq $8, %rsp"));
    }

    #[test]
    fn generate_too_many_arguments() {
        let err = compile_err("int main() { return f(1, 2, 3, 4, 5, 6, 7); }");
        assert!(err.message.contains("too many arguments"));
    }

    #[test]
    fn generate_address_of() {
        let asm = compile("int main() { int x = 5; int *p = &x; return *p; }");
        assert!(asm.contains("leaq -4(%rbp), %rax"));
        assert!(asm.contains("movq %rax, -12(%rbp)"));
        assert!(asm.contains("movl (%rax), %eax"));
    }

    #[test]
    fn generate_dereference_write() {
        let asm = compile("int main() { int x = 0; int *p = &x; *p = 42; return x; }");
        assert!(asm.contains("movl %eax, (%rcx)"));
    }

    #[test]
    fn generate_array_declaration() {
        let asm = compile("int main() { int arr[3]; arr[0] = 1; return arr[0]; }");
        assert!(asm.contains("leaq"));
        assert!(asm.contains("movl %eax, (%rcx)"));
        assert!(asm.contains("movl (%rax), %eax"));
    }

    #[test]
    fn generate_pointer_param() {
        let asm = compile("int f(int *p) { return *p; } int main() { int x = 5; return f(&x); }");
        assert!(asm.contains("movq %rdi, -8(%rbp)"));
        assert!(asm.contains("movl (%rax), %eax"));
    }

    #[test]
    fn generate_pointer_arithmetic() {
        let asm = compile("int main() { int arr[3]; int *p = arr; p = p + 1; return *p; }");
        assert!(asm.contains("shlq $2, %rcx"));
        assert!(asm.contains("addq %rcx, %rax"));
    }

    #[test]
    fn generate_global_int() {
        let asm = compile("int x = 42; int main() { return x; }");
        assert!(asm.contains(".data"));
        assert!(asm.contains(".globl x"));
        assert!(asm.contains("x:"));
        assert!(asm.contains(".long 42"));
        assert!(asm.contains("movl x(%rip), %eax"));
    }

    #[test]
    fn generate_global_uninitialized() {
        let asm = compile("int x; int main() { return x; }");
        assert!(asm.contains(".comm x,4,4"));
        assert!(asm.contains("movl x(%rip), %eax"));
    }

    #[test]
    fn generate_global_assignment() {
        let asm = compile("int x; int main() { x = 10; return x; }");
        assert!(asm.contains("movl %eax, x(%rip)"));
        assert!(asm.contains("movl x(%rip), %eax"));
    }

    #[test]
    fn generate_global_pointer() {
        let asm = compile("int *p; int main() { int x = 5; p = &x; return *p; }");
        assert!(asm.contains(".comm p,8,8"));
        assert!(asm.contains("movq %rax, p(%rip)"));
    }

    #[test]
    fn generate_global_array() {
        let asm = compile("int arr[3]; int main() { arr[0] = 42; return arr[0]; }");
        assert!(asm.contains(".comm arr,12,4"));
        assert!(asm.contains("leaq arr(%rip), %rax"));
    }

    #[test]
    fn generate_string_literal() {
        let asm = compile("int main() { char *s = \"hello\"; return 0; }");
        assert!(asm.contains(".section .rodata"));
        assert!(asm.contains(".LC0:"));
        assert!(asm.contains(".string \"hello\""));
        assert!(asm.contains("leaq .LC0(%rip), %rax"));
    }

    #[test]
    fn generate_string_escape() {
        let asm = compile("int main() { char *s = \"a\\nb\"; return 0; }");
        assert!(asm.contains(".string \"a\\nb\""));
    }

    #[test]
    fn generate_global_string_init() {
        let asm = compile("char *msg = \"world\"; int main() { return 0; }");
        assert!(asm.contains(".data"));
        assert!(asm.contains(".globl msg"));
        assert!(asm.contains("msg:"));
        assert!(asm.contains(".quad .LC0"));
        assert!(asm.contains(".section .rodata"));
        assert!(asm.contains(".string \"world\""));
    }

    #[test]
    fn generate_address_of_global() {
        let asm = compile("int x = 5; int main() { int *p = &x; return *p; }");
        assert!(asm.contains("leaq x(%rip), %rax"));
    }

    #[test]
    fn generate_no_sections_without_globals() {
        let asm = compile("int main() { return 42; }");
        assert!(!asm.contains(".data"));
        assert!(!asm.contains(".text"));
        assert!(!asm.contains(".rodata"));
    }

    #[test]
    fn generate_char_pointer_param() {
        let asm = compile("int f(char *s) { return *s; } int main() { return 0; }");
        assert!(asm.contains("movq %rdi, -8(%rbp)"));
    }

    #[test]
    fn generate_struct_member_access() {
        let asm = compile("struct Point { int x; int y; }; int main() { struct Point p; p.x = 10; p.y = 20; return p.x; }");
        assert!(asm.contains("leaq"));
        assert!(asm.contains("movl %eax,"));
    }

    #[test]
    fn generate_struct_arrow_access() {
        let asm = compile("struct Point { int x; }; int main() { struct Point p; struct Point *pp = &p; pp->x = 42; return pp->x; }");
        assert!(asm.contains("movq"));
    }

    #[test]
    fn generate_global_struct() {
        let asm = compile("struct Point { int x; int y; }; struct Point g; int main() { g.x = 5; return g.x; }");
        assert!(asm.contains(".comm g,8,4"));
    }

    #[test]
    fn generate_union_member() {
        let asm = compile("union Data { int i; int *p; }; int main() { union Data d; d.i = 42; return d.i; }");
        assert!(asm.contains("movl %eax,"));
    }

    #[test]
    fn generate_struct_address_of_member() {
        let asm = compile("struct Point { int x; int y; }; int main() { struct Point p; int *q = &p.y; return 0; }");
        assert!(asm.contains("addq $4, %rax"));
    }

    #[test]
    fn generate_struct_pointer_param() {
        let asm = compile(concat!(
            "struct Point { int x; }; ",
            "int getX(struct Point *p) { return p->x; } ",
            "int main() { struct Point pt; pt.x = 5; return getX(&pt); }",
        ));
        assert!(asm.contains("movq %rdi,"));
        assert!(asm.contains("call getX"));
    }

    #[test]
    fn generate_struct_layout_with_padding() {
        let layout = build_composite_layout(&TypeDef {
            name: "Mixed".to_string(),
            members: vec![
                (MemberType::Int, "i".to_string()),
                (MemberType::IntPointer, "p".to_string()),
            ],
            is_union: false,
        });
        assert_eq!(layout.members[0].offset, 0);
        assert_eq!(layout.members[1].offset, 8);
        assert_eq!(layout.size, 16);
        assert_eq!(layout.align, 8);
    }

    #[test]
    fn generate_union_layout() {
        let layout = build_composite_layout(&TypeDef {
            name: "Data".to_string(),
            members: vec![
                (MemberType::Int, "i".to_string()),
                (MemberType::IntPointer, "p".to_string()),
            ],
            is_union: true,
        });
        assert_eq!(layout.members[0].offset, 0);
        assert_eq!(layout.members[1].offset, 0);
        assert_eq!(layout.size, 8);
        assert_eq!(layout.align, 8);
    }
}
