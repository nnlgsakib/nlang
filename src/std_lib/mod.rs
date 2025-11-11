//! Standard library for Nlang
//! This module contains the built-in functions and types available in Nlang

pub mod functions;
pub mod types;

use crate::ast::{Expr, Type};
use self::functions::*;
use self::types::{BuiltInFunction, BuiltInMethod, BuiltInType};

pub struct StdLib {
    pub functions: Vec<BuiltInFunction>,
    pub types: Vec<BuiltInType>,
}

impl StdLib {
    pub fn new() -> Self {
        Self {
            functions: vec![
                // I/O Functions
                BuiltInFunction {
                    name: "print".to_string(),
                    parameters: vec![],
                    return_type: Type::Void,
                    implementation: builtin_print,
                },
                BuiltInFunction {
                    name: "println".to_string(),
                    parameters: vec![],
                    return_type: Type::Void,
                    implementation: builtin_println,
                },
                BuiltInFunction {
                    name: "input".to_string(),
                    parameters: vec![],
                    return_type: Type::String,
                    implementation: builtin_input,
                },
                BuiltInFunction {
                    name: "input".to_string(),
                    parameters: vec![Type::String],
                    return_type: Type::String,
                    implementation: builtin_input,
                },
                
                // String Functions
                BuiltInFunction {
                    name: "len".to_string(),
                    parameters: vec![Type::String],
                    return_type: Type::Integer,
                    implementation: builtin_len_string,
                },
                BuiltInFunction {
                    name: "len".to_string(),
                    parameters: vec![Type::Array(Box::new(Type::Void), 0)], // Placeholder for any array
                    return_type: Type::Integer,
                    implementation: builtin_len_array,
                },
                
                // Type Conversion Functions
                BuiltInFunction {
                    name: "int".to_string(),
                    parameters: vec![Type::String],
                    return_type: Type::Integer,
                    implementation: builtin_int,
                },
                BuiltInFunction {
                    name: "int".to_string(),
                    parameters: vec![Type::Float],
                    return_type: Type::Integer,
                    implementation: builtin_int_from_float,
                },
                BuiltInFunction {
                    name: "int".to_string(),
                    parameters: vec![Type::F32],
                    return_type: Type::Integer,
                    implementation: builtin_int_from_float,
                },
                BuiltInFunction {
                    name: "int".to_string(),
                    parameters: vec![Type::F64],
                    return_type: Type::Integer,
                    implementation: builtin_int_from_float,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::Integer],
                    return_type: Type::String,
                    implementation: builtin_str,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::I8],
                    return_type: Type::String,
                    implementation: builtin_str_i8,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::I16],
                    return_type: Type::String,
                    implementation: builtin_str_i16,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::I32],
                    return_type: Type::String,
                    implementation: builtin_str_i32,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::I64],
                    return_type: Type::String,
                    implementation: builtin_str_i64,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::ISize],
                    return_type: Type::String,
                    implementation: builtin_str_isize,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::U8],
                    return_type: Type::String,
                    implementation: builtin_str_u8,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::U16],
                    return_type: Type::String,
                    implementation: builtin_str_u16,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::U32],
                    return_type: Type::String,
                    implementation: builtin_str_u32,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::U64],
                    return_type: Type::String,
                    implementation: builtin_str_u64,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::USize],
                    return_type: Type::String,
                    implementation: builtin_str_usize,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::Float],
                    return_type: Type::String,
                    implementation: builtin_str_from_float,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::F32],
                    return_type: Type::String,
                    implementation: builtin_str_from_float,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::F64],
                    return_type: Type::String,
                    implementation: builtin_str_from_float,
                },
                BuiltInFunction {
                    name: "str".to_string(),
                    parameters: vec![Type::Boolean],
                    return_type: Type::String,
                    implementation: builtin_str_from_bool,
                },
                BuiltInFunction {
                    name: "float".to_string(),
                    parameters: vec![Type::String],
                    return_type: Type::F64,
                    implementation: builtin_float,
                },
                
                // Mathematical Functions
                BuiltInFunction {
                    name: "abs".to_string(),
                    parameters: vec![Type::Integer],
                    return_type: Type::Integer,
                    implementation: builtin_abs,
                },
                BuiltInFunction {
                    name: "abs_float".to_string(),
                    parameters: vec![Type::Float],
                    return_type: Type::F64,
                    implementation: builtin_abs_float,
                },
                BuiltInFunction {
                    name: "abs_float".to_string(),
                    parameters: vec![Type::F32],
                    return_type: Type::F64,
                    implementation: builtin_abs_float,
                },
                BuiltInFunction {
                    name: "abs_float".to_string(),
                    parameters: vec![Type::F64],
                    return_type: Type::F64,
                    implementation: builtin_abs_float,
                },
                BuiltInFunction {
                    name: "max".to_string(),
                    parameters: vec![Type::Integer, Type::Integer],
                    return_type: Type::Integer,
                    implementation: builtin_max,
                },
                BuiltInFunction {
                    name: "min".to_string(),
                    parameters: vec![Type::Integer, Type::Integer],
                    return_type: Type::Integer,
                    implementation: builtin_min,
                },
                BuiltInFunction {
                    name: "pow".to_string(),
                    parameters: vec![Type::Integer, Type::Integer],
                    return_type: Type::Integer,
                    implementation: builtin_pow,
                },
                
                // Boolean Functions
                BuiltInFunction {
                    name: "bool".to_string(),
                    parameters: vec![Type::Integer],
                    return_type: Type::Boolean,
                    implementation: builtin_bool,
                },
            ],
            types: vec![
                BuiltInType {
                    name: "list".to_string(),
                    methods: vec![
                        BuiltInMethod {
                            name: "append".to_string(),
                            parameters: vec![Type::Integer], // Placeholder type
                            return_type: Type::Void,
                        },
                        BuiltInMethod {
                            name: "len".to_string(),
                            parameters: vec![],
                            return_type: Type::Integer,
                        },
                        BuiltInMethod {
                            name: "pop".to_string(),
                            parameters: vec![],
                            return_type: Type::Integer, // Placeholder
                        },
                        BuiltInMethod {
                            name: "clear".to_string(),
                            parameters: vec![],
                            return_type: Type::Void,
                        },
                    ],
                },
                BuiltInType {
                    name: "string".to_string(),
                    methods: vec![
                        BuiltInMethod {
                            name: "len".to_string(),
                            parameters: vec![],
                            return_type: Type::Integer,
                        },
                        BuiltInMethod {
                            name: "upper".to_string(),
                            parameters: vec![],
                            return_type: Type::String,
                        },
                        BuiltInMethod {
                            name: "lower".to_string(),
                            parameters: vec![],
                            return_type: Type::String,
                        },
                        BuiltInMethod {
                            name: "trim".to_string(),
                            parameters: vec![],
                            return_type: Type::String,
                        },
                        BuiltInMethod {
                            name: "contains".to_string(),
                            parameters: vec![Type::String],
                            return_type: Type::Boolean,
                        },
                    ],
                },
            ],
        }
    }
    
    /// Check if a function name is a built-in function
    pub fn is_builtin_function(&self, name: &str) -> bool {
        self.functions.iter().any(|f| f.name == name)
    }
    
    /// Get a built-in function by name
    pub fn get_builtin_function(&self, name: &str) -> Option<&BuiltInFunction> {
        self.functions.iter().find(|f| f.name == name)
    }
    
    /// Get a built-in function by name and parameter types (for overloading)
    pub fn get_builtin_function_by_signature(&self, name: &str, param_types: &[Type]) -> Option<&BuiltInFunction> {
        self.functions.iter().find(|f| f.name == name && f.parameters == param_types)
    }
    
    /// Check if a type name is a built-in type
    pub fn is_builtin_type(&self, name: &str) -> bool {
        self.types.iter().any(|t| t.name == name)
    }
    
    /// Execute a built-in function
    pub fn execute_builtin(&self, name: &str, args: &[Expr]) -> Result<Expr, String> {
        if let Some(func) = self.get_builtin_function(name) {
            (func.implementation)(args)
        } else {
            Err(format!("Unknown built-in function: {}", name))
        }
    }
}

impl Default for StdLib {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Literal};

    #[test]
    fn test_std_lib_creation() {
        let std_lib = StdLib::new();
        assert!(!std_lib.functions.is_empty());
        assert!(std_lib.is_builtin_function("print"));
        assert!(std_lib.is_builtin_function("println"));
        assert!(std_lib.is_builtin_function("input"));
        assert!(std_lib.is_builtin_function("len"));
        assert!(std_lib.is_builtin_function("abs"));
        assert!(std_lib.is_builtin_function("max"));
        assert!(std_lib.is_builtin_function("min"));
    }
    
    #[test]
    fn test_builtin_functions() {
        let std_lib = StdLib::new();
        assert!(std_lib.get_builtin_function("print").is_some());
        assert!(std_lib.get_builtin_function("nonexistent").is_none());
    }
    
    #[test]
    fn test_string_length() {
        let args = vec![Expr::Literal(Literal::String("hello".to_string()))];
        let result = builtin_len_string(&args).unwrap();
        
        match result {
            Expr::Literal(Literal::Integer(5)) => {},
            _ => panic!("Expected integer 5"),
        }
    }
    
    #[test]
    fn test_int_conversion() {
        let args = vec![Expr::Literal(Literal::String("42".to_string()))];
        let result = builtin_int(&args).unwrap();
        
        match result {
            Expr::Literal(Literal::Integer(42)) => {},
            _ => panic!("Expected integer 42"),
        }
    }
    
    #[test]
    fn test_mathematical_functions() {
        // Test abs
        let args = vec![Expr::Literal(Literal::Integer(-5))];
        let result = builtin_abs(&args).unwrap();
        assert!(matches!(result, Expr::Literal(Literal::Integer(5))));
        
        // Test max
        let args = vec![
            Expr::Literal(Literal::Integer(3)),
            Expr::Literal(Literal::Integer(7))
        ];
        let result = builtin_max(&args).unwrap();
        assert!(matches!(result, Expr::Literal(Literal::Integer(7))));
        
        // Test min
        let args = vec![
            Expr::Literal(Literal::Integer(3)),
            Expr::Literal(Literal::Integer(7))
        ];
        let result = builtin_min(&args).unwrap();
        assert!(matches!(result, Expr::Literal(Literal::Integer(3))));
    }
}
