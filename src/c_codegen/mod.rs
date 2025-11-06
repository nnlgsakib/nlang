use crate::ast::*;
use codemap::CodeMap;
use std::collections::HashMap;
use std::fmt::Write;
use std::rc::Rc;
use thiserror::Error;
#[derive(Error, Debug)]
pub enum CCodeGenError {
    #[error("Unsupported: {0}")]
    Unsupported(String),
    #[error("Var not found: {0}")]
    VarNotFound(String),
    #[error("Fmt error")]
    Fmt(#[from] std::fmt::Error),
}
pub struct CCodeGenerator {
    #[allow(dead_code)]
    map: CodeMap,
    buf: String,
    indent: usize,
    vars: std::collections::HashMap<String, String>,
    str_consts: std::collections::HashMap<String, String>,
    str_counter: usize,
    function_return_types: std::rc::Rc<std::collections::HashMap<String, String>>,
}
impl CCodeGenerator {
    pub fn new() -> Self {
        let mut generator = Self {
            map: CodeMap::new(),
            buf: String::new(),
            indent: 0,
            vars: Default::default(),
            str_consts: Default::default(),
            str_counter: 0,
            function_return_types: std::rc::Rc::new(Default::default()),
        };
        generator.line("#include <stdio.h>");
        generator.line("#include <string.h>");
        generator.line("#include <stdlib.h>");
        generator.line("#include <math.h>");
        generator.line("#include <stdint.h>"); // For int32_t
        generator.empty();    // Add built-in string conversion functions
    generator.line("// Built-in string conversion functions");
    generator.line("char* int_to_str(int value) {");
    generator.push();
    generator.line("static char buffer[20];");
    generator.line("snprintf(buffer, sizeof(buffer), \"%d\", value);");
    generator.line("return buffer;");
    generator.pop();
    generator.line("}");
    generator.empty();
   
    generator.line("char* float_to_str(double value) {");
    generator.push();
    generator.line("static char buffer[50];");
    generator.line("snprintf(buffer, sizeof(buffer), \"%f\", value);");
    generator.line("return buffer;");
    generator.pop();
    generator.line("}");
    generator.empty();
    generator
}
// --------------------------------------------------------------------- //
// Public API
// --------------------------------------------------------------------- //
pub fn generate_program(mut self, prog: &Program) -> Result<String, CCodeGenError> {
    self.collect_strings(prog);
    self.extract_function_return_types(prog);
    self.emit_string_consts()?;
    self.emit_forward_decls(prog)?;
    self.emit_functions(prog)?;
    Ok(self.buf)
}
// --------------------------------------------------------------------- //
// Tiny helpers – no raw C
// --------------------------------------------------------------------- //
fn line(&mut self, s: &str) {
    writeln!(self.buf, "{}{}", " ".repeat(self.indent), s).unwrap();
}
fn empty(&mut self) { self.buf.push('\n'); }
fn push(&mut self) { self.indent += 1; }
fn pop(&mut self) { self.indent = self.indent.saturating_sub(1); }
fn block(&mut self, f: impl FnOnce(&mut Self)) {
    self.line("{");
    self.push();
    f(self);
    self.pop();
    self.line("}");
}
fn write(&mut self, s: &str) { self.buf.push_str(s); }
// --------------------------------------------------------------------- //
// String constants
// --------------------------------------------------------------------- //
fn collect_strings(&mut self, prog: &Program) {
    fn walk(generator: &mut CCodeGenerator, stmt: &Statement) {
        match stmt {
            Statement::Expression(e) => walk_expr(generator, e),
            Statement::LetDeclaration { initializer, .. } => {
                if let Some(i) = initializer { walk_expr(generator, i); }
            }
            Statement::FunctionDeclaration { body, .. } => {
                for s in body { walk(generator, s); }
            }
            Statement::If { condition, then_branch, else_branch, .. } => {
                walk_expr(generator, condition);
                walk(generator, then_branch);
                if let Some(e) = else_branch { walk(generator, e); }
            }
            Statement::While { condition, body, .. } => {
                walk_expr(generator, condition);
                walk(generator, body);
            }
            Statement::Return { value } => {
                if let Some(v) = value { walk_expr(generator, v); }
            }
            Statement::Block { statements } => {
                for s in statements { walk(generator, s); }
            }
            _ => {}
        }
    }
    fn walk_expr(generator: &mut CCodeGenerator, e: &Expr) {
        match e {
            Expr::Literal(Literal::String(s)) => {
                if !generator.str_consts.contains_key(s) {
                    let name = format!("str_const_{}", generator.str_counter);
                    generator.str_counter += 1;
                    generator.str_consts.insert(s.clone(), name);
                }
            }
            Expr::Binary { left, right, .. } => {
                walk_expr(generator, left);
                walk_expr(generator, right);
            }
            Expr::Unary { operand, .. } => walk_expr(generator, operand),
            Expr::Call { arguments, .. } => {
                for a in arguments { walk_expr(generator, a); }
            }
            _ => {}
        }
    }
    for s in &prog.statements {
        walk(self, s);
    }
}
fn emit_string_consts(&mut self) -> Result<(), CCodeGenError> {
    let str_consts: Vec<(String, String)> = self.str_consts.iter()
        .map(|(lit, name)| (lit.clone(), name.clone()))
        .collect();
   
    for (lit, name) in str_consts {
        let escaped: String = lit
            .chars()
            .flat_map(|c| match c {
                '"' => "\\\"".chars().collect::<Vec<_>>(),
                '\\' => "\\\\".chars().collect::<Vec<_>>(),
                '\n' => "\\n".chars().collect::<Vec<_>>(),
                '\r' => "\\r".chars().collect::<Vec<_>>(),
                '\t' => "\\t".chars().collect::<Vec<_>>(),
                c => c.to_string().chars().collect::<Vec<_>>(),
            })
            .collect();
        self.line(&format!("static const char {name}[] = \"{escaped}\";"));
    }
    if !self.str_consts.is_empty() {
        self.empty();
    }
    Ok(())
}
fn extract_function_return_types(&mut self, prog: &Program) {
    let mut new_function_return_types = HashMap::new();
    for stmt in &prog.statements {
        if let Statement::FunctionDeclaration { name, return_type, body, .. } = stmt {
            let mut c_return_type = if name == "main" {
                "int".to_string()
            } else {
                self.type_to_c(return_type.as_ref().unwrap_or(&Type::Void))
            };

            // If the return type is void or unspecified, try to infer from return statements
            if c_return_type == "void" && name != "main" {
                // Walk the body to find a return with a value and infer its type
                fn infer_from_body(codegenr: &mut CCodeGenerator, stmt: &Statement) -> Option<String> {
                    match stmt {
                        Statement::Return { value } => {
                            if let Some(v) = value {
                                Some(codegenr.infer_type(v))
                            } else {
                                None
                            }
                        }
                        Statement::If { then_branch, else_branch, .. } => {
                            infer_from_body(codegenr, then_branch.as_ref())
                                .or_else(|| else_branch.as_ref().and_then(|e| infer_from_body(codegenr, e.as_ref())))
                        }
                        Statement::While { body, .. } => infer_from_body(codegenr, body.as_ref()),
                        Statement::Block { statements } => {
                            for s in statements {
                                if let Some(t) = infer_from_body(codegenr, s) { return Some(t); }
                            }
                            None
                        }
                        _ => None,
                    }
                }

                for s in body {
                    if let Some(t) = infer_from_body(self, s) {
                        c_return_type = t;
                        break;
                    }
                }
            }

            new_function_return_types.insert(name.clone(), c_return_type);
        }
    }
    self.function_return_types = Rc::new(new_function_return_types);
}
// --------------------------------------------------------------------- //
// Forward declarations
// --------------------------------------------------------------------- //
fn emit_forward_decls(&mut self, prog: &Program) -> Result<(), CCodeGenError> {
    for stmt in &prog.statements {
        if let Statement::FunctionDeclaration { name, parameters, return_type, .. } = stmt {
            // Prefer inferred return types from map; fall back to declared type or void
            let ret: String = if name == "main" {
                "int".to_string()
            } else {
                self.function_return_types
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| self.type_to_c(return_type.as_ref().unwrap_or(&Type::Void)))
            };
            self.write(&ret);
            self.write(" ");
            self.write(name);
            self.write("(");
            let mut first = true;
            for p in parameters {
                if !first { self.write(", "); }
                self.write(&self.type_to_c(&p.param_type));
                self.write(" ");
                self.write(&p.name);
                first = false;
            }
            if parameters.is_empty() {
                self.write("void");
            }
            self.line(");");
        }
    }
    if prog.statements.iter().any(|s| matches!(s, Statement::FunctionDeclaration { .. })) {
        self.empty();
    }
    Ok(())
}
// --------------------------------------------------------------------- //
// Function bodies
// --------------------------------------------------------------------- //
fn emit_functions(&mut self, prog: &Program) -> Result<(), CCodeGenError> {
    for stmt in &prog.statements {
        if let Statement::FunctionDeclaration { .. } = stmt {
            self.emit_function(stmt)?;
            self.empty();
        }
    }
    Ok(())
}
fn emit_function(&mut self, stmt: &Statement) -> Result<(), CCodeGenError> {
    let Statement::FunctionDeclaration { name, parameters, body, return_type, .. } = stmt else {
        return Err(CCodeGenError::Unsupported("not a function".into()));
    };
    // Prefer inferred return types from map; fall back to declared type or void
    let ret: String = if name == "main" {
        "int".to_string()
    } else {
        self.function_return_types
            .get(name)
            .cloned()
            .unwrap_or_else(|| self.type_to_c(return_type.as_ref().unwrap_or(&Type::Void)))
    };
    self.write(&ret);
    self.write(" ");
    self.write(name);
    self.write("(");
    let mut first = true;
    for p in parameters {
        if !first { self.write(", "); }
        let ty = self.type_to_c(&p.param_type);
        self.write(&ty);
        self.write(" ");
        self.write(&p.name);
        self.vars.insert(p.name.clone(), ty);
        first = false;
    }
    if parameters.is_empty() {
        self.write("void");
    }
    self.write(") ");
    self.block(|generator| {
                for s in body {
                    generator.emit_stmt(s).unwrap();
                }
                if name == "main" && !body.iter().any(|s| matches!(s, Statement::Return { .. })) {
                    generator.line("return 0;");
                }
            });
    Ok(())
}
fn emit_stmt(&mut self, stmt: &Statement) -> Result<(), CCodeGenError> {
    match stmt {
        Statement::Expression(e) => {
            let code = self.emit_expr(e)?;
            self.line(&format!("{code};"));
        }
        Statement::LetDeclaration { name, initializer, var_type, .. } => {
            // Use declared type if available, otherwise infer from initializer or default to int
            let ty = if let Some(declared_type) = var_type {
                self.type_to_c(declared_type)
            } else if let Some(init) = initializer {
                self.infer_type(init)
            } else {
                "int".to_string()
            };
           
            // Store the element type for arrays, not the full array type
            let var_type_to_store = if let Some(Type::Array(element_type, _)) = var_type {
                self.type_to_c(element_type)
            } else {
                ty.clone()
            };
            self.vars.insert(name.clone(), var_type_to_store);
            if let Some(init) = initializer {
                let init_code = self.emit_expr(init)?;
                
                // Handle array types specially - in C, arrays are declared as "type name[size1][size2]..."
            if let Some(array_type) = var_type {
                let c_decl = self.array_type_to_c_decl(array_type, name);
                self.line(&format!("{c_decl} = {init_code};"));
            } else {
                self.line(&format!("{ty} {name} = {init_code};"));
            }
            } else {
                // Handle array types specially - in C, arrays are declared as "type name[size1][size2]..."
                if let Some(array_type) = var_type {
                    let c_decl = self.array_type_to_c_decl(array_type, name);
                    self.line(&format!("{c_decl};"));
                } else {
                    self.line(&format!("{ty} {name};"));
                }
            }
        }
        Statement::If { condition, then_branch, else_branch } => {
            let cond = self.emit_expr(condition)?;
            self.write(&format!("if ({cond}) "));
            self.block(|generator| { generator.emit_stmt(then_branch).unwrap(); });
            if let Some(els) = else_branch {
                self.write(" else ");
                self.block(|generator| { generator.emit_stmt(els).unwrap(); });
            }
        }
        Statement::While { condition, body } => {
            let cond = self.emit_expr(condition)?;
            self.write(&format!("while ({cond}) "));
            self.block(|generator| { generator.emit_stmt(body).unwrap(); });
        }
        Statement::Return { value } => {
            if let Some(v) = value {
                let vcode = self.emit_expr(v)?;
                self.line(&format!("return {vcode};"));
            } else {
                self.line("return;");
            }
        }
        Statement::Block { statements } => {
            self.block(|generator| {
                for s in statements {
                    generator.emit_stmt(s).unwrap();
                }
            });
        }
        Statement::Break => self.line("break;"),
        Statement::Continue => self.line("continue;"),
        _ => return Err(CCodeGenError::Unsupported(format!("stmt {:?}", stmt))),
    }
    Ok(())
}
fn emit_expr(&mut self, e: &Expr) -> Result<String, CCodeGenError> {
    Ok(match e {
        Expr::Literal(l) => self.emit_lit(l)?,
        Expr::Variable(n) => n.clone(),
        Expr::Binary { left, right, operator, .. } => {
            let l = self.emit_expr(left)?;
            let r = self.emit_expr(right)?;
            format!("({l} {} {r})", self.binop(operator))
        }
        Expr::Unary { operand, operator, .. } => {
            let inner = self.emit_expr(operand)?;
            format!("({}{})", self.unop(operator), inner)
        }
        Expr::Call { callee, arguments, .. } => {
            let fname = if let Expr::Variable(v) = callee.as_ref() {
                v.clone()
            } else {
                return Err(CCodeGenError::Unsupported("complex callee".into()));
            };
            // built-ins
            match fname.as_str() {
                "print" | "println" => {
                    if arguments.len() != 1 { return Err(CCodeGenError::Unsupported("print arity".into())); }
                    let arg = &arguments[0];
                    let arg_code = self.emit_expr(arg)?;
                    let (fmt, val) = self.print_fmt(arg, &arg_code)?;
                    let call = if fname == "println" {
                        format!("printf(\"{fmt}\\n\", {val})")
                    } else {
                        format!("printf(\"{fmt}\", {val})")
                    };
                    return Ok(call);
                }
                "str" => return Ok(format!("int_to_str({})", self.emit_expr(&arguments[0])?)),
                "int" => {
                    let a = self.emit_expr(&arguments[0])?;
                    if a.contains("int_to_str") || a.contains("float_to_str") {
                        return Ok(format!("atoi({a})"));
                    } else {
                        return Ok(format!("((int){a})"));
                    }
                }
                "float" => {
                    let a = self.emit_expr(&arguments[0])?;
                    if a.contains("int_to_str") || a.contains("float_to_str") {
                        return Ok(format!("atof({a})"));
                    } else {
                        return Ok(format!("((double){a})"));
                    }
                }
                "abs" => return Ok(format!("abs({})", self.emit_expr(&arguments[0])?)),
                "abs_float" => return Ok(format!("fabs({})", self.emit_expr(&arguments[0])?)),
                _ => {}
            }
            let args: Vec<_> = arguments.iter().map(|a| self.emit_expr(a)).collect::<Result<_, _>>()?;
            format!("{}({})", fname, args.join(", "))
        }
        Expr::Assign { name, value } => {
            let v = self.emit_expr(value)?;
            self.vars.insert(name.clone(), "int".into());
            format!("({name} = {v})")
        }
        Expr::AssignIndex { sequence, index, value } => {
            // Handle array assignment: arr[i] = value
            let seq_code = self.emit_expr(sequence)?;
            let idx_code = self.emit_expr(index)?;
            let val_code = self.emit_expr(value)?;
            format!("({seq_code}[{idx_code}] = {val_code})")
        }
        Expr::Index { sequence, index } => {
            let seq_code = self.emit_expr(sequence)?;
            let idx_code = self.emit_expr(index)?;
            format!("({seq_code}[{idx_code}])")
        }
        Expr::ArrayLiteral { elements } => {
            self.emit_array_literal(elements)?
        }
        _ => return Err(CCodeGenError::Unsupported(format!("expr {:?}", e))),
    })
}

fn emit_array_literal(&mut self, elements: &[Expr]) -> Result<String, CCodeGenError> {
    if elements.is_empty() {
        return Ok("NULL".to_string());
    }
    
    let mut element_codes = Vec::new();
    for element in elements {
        // Recursively handle nested array literals
        if let Expr::ArrayLiteral { elements: nested_elements } = element {
            let nested_code = self.emit_array_literal(nested_elements)?;
            element_codes.push(nested_code);
        } else {
            element_codes.push(self.emit_expr(element)?);
        }
    }
    
    // Generate proper C array initialization syntax with nested braces
    Ok(format!("{{{}}}", element_codes.join(", ")))
}

fn emit_lit(&self, l: &Literal) -> Result<String, CCodeGenError> {
    Ok(match l {
        Literal::Integer(i) => i.to_string(),
        Literal::I8(i) => i.to_string(),
        Literal::I16(i) => i.to_string(),
        Literal::I32(i) => i.to_string(),
        Literal::I64(i) => i.to_string(),
        Literal::ISize(i) => i.to_string(),
        Literal::U8(i) => i.to_string(),
        Literal::U16(i) => i.to_string(),
        Literal::U32(i) => i.to_string(),
        Literal::U64(i) => i.to_string(),
        Literal::USize(i) => i.to_string(),
        Literal::Float(f) => f.to_string(),
        Literal::String(s) => self.str_consts.get(s).cloned()
            .ok_or_else(|| CCodeGenError::Unsupported("string not collected".into()))?,
        Literal::Boolean(b) => (if *b { "1" } else { "0" }).to_string(),
        Literal::Null => "NULL".to_string(),
    })
}
// --------------------------------------------------------------------- //
// Helpers
// --------------------------------------------------------------------- //
fn type_to_c(&self, t: &Type) -> String {
    match t {
        Type::Integer => "int".into(),
        Type::I8 => "int8_t".into(),
        Type::I16 => "int16_t".into(),
        Type::I32 => "int32_t".into(),
        Type::I64 => "int64_t".into(),
        Type::ISize => "intptr_t".into(),
        Type::U8 => "uint8_t".into(),
        Type::U16 => "uint16_t".into(),
        Type::U32 => "uint32_t".into(),
        Type::U64 => "uint64_t".into(),
        Type::USize => "size_t".into(),
        Type::F32 => "float".into(),
        Type::F64 => "double".into(),
        Type::Float => "double".into(),
        Type::String => "const char*".into(),
        Type::Boolean => "int".into(),
        Type::Void => "void".into(),
        Type::Array(inner, _) => self.type_to_c(inner),
        Type::Function { .. } => "void*".into(),
    }
}

fn array_type_to_c_decl(&self, t: &Type, name: &str) -> String {
    match t {
        Type::Array(inner, size) => {
            let base_type = self.array_type_to_c_decl(inner, name);
            format!("{}[{}]", base_type, size)
        }
        _ => {
            let base_type = self.type_to_c(t);
            format!("{} {}", base_type, name)
        }
    }
}
fn binop(&self, op: &BinaryOperator) -> &'static str {
    match op {
        BinaryOperator::Plus => "+",
        BinaryOperator::Minus => "-",
        BinaryOperator::Star => "*",
        BinaryOperator::Slash => "/",
        BinaryOperator::Percent => "%",
        BinaryOperator::EqualEqual => "==",
        BinaryOperator::NotEqual => "!=",
        BinaryOperator::Less => "<",
        BinaryOperator::LessEqual => "<=",
        BinaryOperator::Greater => ">",
        BinaryOperator::GreaterEqual => ">=",
        BinaryOperator::And => "&&",
        BinaryOperator::Or => "||",
    }
}
fn unop(&self, op: &UnaryOperator) -> &'static str {
    match op { UnaryOperator::Negate => "-", UnaryOperator::Not => "!", }
}
fn infer_type(&self, e: &Expr) -> String {
    match e {
        Expr::Literal(Literal::String(_)) => "char*".into(),
        Expr::Literal(Literal::Float(_)) => "double".into(),
        Expr::Literal(Literal::Boolean(_)) => "int".into(),
        Expr::Literal(Literal::Integer(_)) => "int".into(),
        Expr::Literal(Literal::I8(_)) => "int8_t".into(),
        Expr::Literal(Literal::I16(_)) => "int16_t".into(),
        Expr::Literal(Literal::I32(_)) => "int32_t".into(),
        Expr::Literal(Literal::I64(_)) => "int64_t".into(),
        Expr::Literal(Literal::ISize(_)) => "intptr_t".into(),
        Expr::Literal(Literal::U8(_)) => "uint8_t".into(),
        Expr::Literal(Literal::U16(_)) => "uint16_t".into(),
        Expr::Literal(Literal::U32(_)) => "uint32_t".into(),
        Expr::Literal(Literal::U64(_)) => "uint64_t".into(),
        Expr::Literal(Literal::USize(_)) => "size_t".into(),
        Expr::Call { callee, .. } => {
            if let Expr::Variable(func_name) = callee.as_ref() {
                // Check if this is a built-in function with known return type
                match func_name.as_str() {
                    "str" => "char*".into(),
                    "int" => "int".into(),
                    "float" => "double".into(),
                    "abs" => "int".into(),
                    "abs_float" => "double".into(),
                    _ => (*self.function_return_types).get(func_name)
                        .map(|s| s.as_str())
                        .unwrap_or("int")
                        .to_string(),
                }
            } else {
                "int".into()
            }
        },
        Expr::ArrayLiteral { elements } => {
            if elements.is_empty() {
                "void*".into()
            } else {
                self.infer_type(&elements[0])
            }
        },
        Expr::Index { sequence, index: _ } => {
            // For array indexing expressions, infer the type of the sequence
            // If the sequence is a variable, look up its type in the vars map
            if let Expr::Variable(var_name) = sequence.as_ref() {
                let full_type = self.vars.get(var_name).map(|s| s.as_str()).unwrap_or("int");
                
                // Extract element type from array types (e.g., "double[3]" -> "double")
                if let Some(open_bracket) = full_type.find('[') {
                    full_type[..open_bracket].to_string()
                } else {
                    full_type.into()
                }
            } else {
                // For other sequence types, recursively infer the type
                self.infer_type(sequence)
            }
        },
        Expr::Variable(var_name) => {
            self.vars.get(var_name).map(|s| s.as_str()).unwrap_or("int").into()
        },
        _ => "int".into(),
    }
}
fn print_fmt(&self, e: &Expr, code: &str) -> Result<(String, String), CCodeGenError> {
    Ok(match e {
        Expr::Literal(Literal::Integer(_)) => ("%d".into(), code.into()),
        Expr::Literal(Literal::Float(_)) => ("%f".into(), code.into()),
        Expr::Literal(Literal::Boolean(b)) => ("%s".into(), format!("\"{}\"", if *b { "true" } else { "false" })),
        Expr::Literal(Literal::String(_)) => ("%s".into(), code.into()),
        Expr::Literal(Literal::Null) => ("%s".into(), "\"null\"".into()),
        Expr::Variable(v) => {
            let ty = self.vars.get(v).map(|s| s.as_str()).unwrap_or("int");
            match ty {
                "int" => ("%d".into(), code.into()),
                "float" => ("%f".into(), code.into()),
                "double" => ("%f".into(), code.into()),
                "char*" => ("%s".into(), code.into()),
                _ => ("%s".into(), code.into()),
            }
        }
        Expr::Index { sequence, index: _ } => {
            // For array indexing expressions, infer the type of the sequence
            let ty = self.infer_type(sequence);
            match ty.as_str() {
                "int" => ("%d".into(), code.into()),
                "float" => ("%f".into(), code.into()),
                "double" => ("%f".into(), code.into()),
                "char*" => ("%s".into(), code.into()),
                _ => ("%s".into(), code.into()),
            }
        }
        _ => ("%s".into(), code.into()),
    })
}}

