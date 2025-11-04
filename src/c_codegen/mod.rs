// src/c_codegen.rs
use crate::ast::*;
use codemap::CodeMap;
use std::fmt::Write;
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
        };
        generator.line("#include <stdio.h>");
        generator.line("#include <string.h>");
        generator.line("#include <stdlib.h>");
        generator.line("#include <math.h>");
        generator.empty();
        generator
    }

    // --------------------------------------------------------------------- //
    // Public API
    // --------------------------------------------------------------------- //
    pub fn generate_program(mut self, prog: &Program) -> Result<String, CCodeGenError> {
        self.collect_strings(prog);
        self.emit_string_consts()?;
        self.emit_forward_decls(prog)?;
        self.emit_functions(prog)?;
        Ok(self.buf)
    }

    // --------------------------------------------------------------------- //
    // Tiny helpers – no raw C
    // --------------------------------------------------------------------- //
    fn line(&mut self, s: &str) {
        writeln!(self.buf, "{}{}", "    ".repeat(self.indent), s).unwrap();
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

    // --------------------------------------------------------------------- //
    // Forward declarations
    // --------------------------------------------------------------------- //
    fn emit_forward_decls(&mut self, prog: &Program) -> Result<(), CCodeGenError> {
        for stmt in &prog.statements {
            if let Statement::FunctionDeclaration { name, parameters, return_type, .. } = stmt {
                let ret = if name == "main" { "int" } else { &self.type_to_c(return_type.as_ref().unwrap_or(&Type::Void)) };
                self.write(ret);
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
        let ret = if name == "main" { "int" } else { &self.type_to_c(return_type.as_ref().unwrap_or(&Type::Void)) };
        self.write(ret);
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
            Statement::LetDeclaration { name, initializer, .. } => {
                let ty = initializer
                    .as_ref()
                    .map(|e| self.infer_type(e))
                    .unwrap_or("int".to_string());
                self.vars.insert(name.clone(), ty.clone());
                if let Some(init) = initializer {
                    let init_code = self.emit_expr(init)?;
                    self.line(&format!("{ty} {name} = {init_code};"));
                } else {
                    self.line(&format!("{ty} {name};"));
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
            _ => return Err(CCodeGenError::Unsupported(format!("expr {:?}", e))),
        })
    }

    fn emit_lit(&self, l: &Literal) -> Result<String, CCodeGenError> {
        Ok(match l {
            Literal::Integer(i) => i.to_string(),
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
            Type::Float => "double".into(),
            Type::String => "const char*".into(),
            Type::Boolean => "int".into(),
            Type::Void => "void".into(),
            Type::Array(_) | Type::Function { .. } => "void*".into(),
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
                    "double" => ("%f".into(), code.into()),
                    "char*" => ("%s".into(), code.into()),
                    _ => ("%s".into(), code.into()),
                }
            }
            _ => ("%s".into(), code.into()),
        })
    }
}