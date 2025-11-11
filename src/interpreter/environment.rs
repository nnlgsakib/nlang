use std::collections::HashMap;
use super::value::{Value, Function};
use super::error::InterpreterError;

#[derive(Clone)]
pub struct Environment {
    pub variables: HashMap<String, Value>,
    pub functions: HashMap<String, Function>,
}

impl Environment {
    pub fn new() -> Self {
        let mut env = Environment {
            variables: HashMap::new(),
            functions: HashMap::new(),
        };
        
        // Add built-in variables
        env.variables.insert("PI".to_string(), Value::Float(std::f64::consts::PI));
        
        env
    }
    
    pub fn define_variable(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }
    
    pub fn get_variable(&self, name: &str) -> Result<Value, InterpreterError> {
        self.variables.get(name)
            .cloned()
            .ok_or_else(|| InterpreterError::VariableNotFound { name: name.to_string() })
    }
    
    pub fn set_variable(&mut self, name: String, value: Value) -> Result<(), InterpreterError> {
        if self.variables.contains_key(&name) {
            self.variables.insert(name, value);
            Ok(())
        } else {
            Err(InterpreterError::VariableNotFound { name })
        }
    }
    
    pub fn define_function(&mut self, func: Function) {
        self.functions.insert(func.name.clone(), func);
    }
    
    pub fn get_function(&self, name: &str) -> Result<&Function, InterpreterError> {
        self.functions.get(name)
            .ok_or_else(|| InterpreterError::FunctionNotFound { name: name.to_string() })
    }
}
