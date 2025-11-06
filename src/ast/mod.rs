use std::fmt;

#[derive(Debug, Clone, serde::Serialize)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, serde::Serialize)]
pub enum Statement {
    Expression(Expr),
    LetDeclaration {
        name: String,
        initializer: Option<Expr>,
        var_type: Option<Type>,
        is_exported: bool,
    },
    FunctionDeclaration {
        name: String,
        parameters: Vec<Parameter>,
        body: Vec<Statement>,
        return_type: Option<Type>,
        is_exported: bool,
    },
    Block {
        statements: Vec<Statement>,
    },
    If {
        condition: Box<Expr>,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    While {
        condition: Box<Expr>,
        body: Box<Statement>,
    },
    Return {
        value: Option<Box<Expr>>,
    },
    Break,
    Continue,
    Import {
        module: String,
        alias: Option<String>,
    },
    ImportFrom {
        module: String,
        items: Vec<(String, Option<String>)>,
    },
    AssignMain {
        function_name: String,
    },
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
pub enum Type {
    Integer,
    I8,
    I16,
    I32,
    I64,
    ISize,
    U8,
    U16,
    U32,
    U64,
    USize,
    F32,
    F64,
    Float,
    Boolean,
    String,
    Array(Box<Type>, usize),
    Function { params: Vec<Type>, return_type: Box<Type> },
    Void,
}

#[derive(Debug, Clone, serde::Serialize)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    Binary {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Function {
        parameters: Vec<Parameter>,
        body: Vec<Statement>,
        return_type: Option<Type>,
    },
    Get {
        object: Box<Expr>,
        name: String,
    },
    Set {
        object: Box<Expr>,
        name: String,
        value: Box<Expr>,
    },
    Index {
        sequence: Box<Expr>,
        index: Box<Expr>,
    },
    Assign {
        name: String,
        value: Box<Expr>,
    },
    AssignIndex {
        sequence: Box<Expr>,
        index: Box<Expr>,
        value: Box<Expr>,
    },
    ArrayLiteral {
        elements: Vec<Expr>,
    },
}

#[derive(Debug, Clone, serde::Serialize)]
pub enum Literal {
    Integer(i64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    ISize(isize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    USize(usize),
    Float(f64),
    Boolean(bool),
    String(String),
    Null,
}

#[derive(Debug, Clone, serde::Serialize)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    EqualEqual,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
}

#[derive(Debug, Clone, serde::Serialize)]
pub enum UnaryOperator {
    Negate,
    Not,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Integer => write!(f, "int"),
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::ISize => write!(f, "isize"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::USize => write!(f, "usize"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            // Legacy alias retained for backward compatibility; treat as f64
            Type::Float => write!(f, "f64"),
            Type::Boolean => write!(f, "bool"),
            Type::String => write!(f, "string"),
            Type::Array(inner, size) => write!(f, "[{}; {}]", inner, size),
            Type::Function { params, return_type } => {
                let param_types: Vec<String> = params.iter().map(|p| format!("{}", p)).collect();
                write!(f, "fn({}) -> {}", param_types.join(", "), return_type)
            }
            Type::Void => write!(f, "void"),
        }
    }
}