#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::semantic::type_inference::TypeInferenceEngine;
    use crate::semantic::analyzer::SemanticAnalyzer;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn create_test_engine() -> TypeInferenceEngine {
        TypeInferenceEngine::new()
    }

    #[test]
    fn test_literal_type_inference() {
        let engine = create_test_engine();

        // Test integer literals
        assert_eq!(
            engine.infer_expression_type(&Expr::Literal(Literal::Integer(42))),
            Type::Integer
        );
        assert_eq!(
            engine.infer_expression_type(&Expr::Literal(Literal::I8(8))),
            Type::I8
        );
        assert_eq!(
            engine.infer_expression_type(&Expr::Literal(Literal::F64(3.14))),
            Type::F64
        );

        // Test other literals
        assert_eq!(
            engine.infer_expression_type(&Expr::Literal(Literal::Boolean(true))),
            Type::Boolean
        );
        assert_eq!(
            engine.infer_expression_type(&Expr::Literal(Literal::String("hello".to_string()))),
            Type::String
        );
        assert_eq!(
            engine.infer_expression_type(&Expr::Literal(Literal::Null)),
            Type::Void
        );
    }

    #[test]
    fn test_arithmetic_type_inference() {
        let engine = create_test_engine();

        // Integer arithmetic
        let int_plus = Expr::Binary {
            left: Box::new(Expr::Literal(Literal::Integer(5))),
            operator: BinaryOperator::Plus,
            right: Box::new(Expr::Literal(Literal::Integer(3))),
        };
        assert_eq!(engine.infer_expression_type(&int_plus), Type::Integer);

        // Float arithmetic
        let float_plus = Expr::Binary {
            left: Box::new(Expr::Literal(Literal::F64(2.5))),
            operator: BinaryOperator::Plus,
            right: Box::new(Expr::Literal(Literal::F64(1.5))),
        };
        assert_eq!(engine.infer_expression_type(&float_plus), Type::F64);

        // Mixed arithmetic (should promote to float)
        let mixed = Expr::Binary {
            left: Box::new(Expr::Literal(Literal::Integer(5))),
            operator: BinaryOperator::Plus,
            right: Box::new(Expr::Literal(Literal::F64(3.2))),
        };
        assert_eq!(engine.infer_expression_type(&mixed), Type::F64);
    }

    #[test]
    fn test_comparison_type_inference() {
        let engine = create_test_engine();

        // Comparisons always return boolean
        let comparison = Expr::Binary {
            left: Box::new(Expr::Literal(Literal::Integer(5))),
            operator: BinaryOperator::Less,
            right: Box::new(Expr::Literal(Literal::Integer(10))),
        };
        assert_eq!(engine.infer_expression_type(&comparison), Type::Boolean);

        // Cross-type comparisons
        let cross_comparison = Expr::Binary {
            left: Box::new(Expr::Literal(Literal::Integer(5))),
            operator: BinaryOperator::EqualEqual,
            right: Box::new(Expr::Literal(Literal::F64(5.0))),
        };
        assert_eq!(engine.infer_expression_type(&cross_comparison), Type::Boolean);
    }

    #[test]
    fn test_array_type_inference() {
        let engine = create_test_engine();

        // Homogeneous integer array
        let int_array = Expr::ArrayLiteral {
            elements: vec![
                Expr::Literal(Literal::Integer(1)),
                Expr::Literal(Literal::Integer(2)),
                Expr::Literal(Literal::Integer(3)),
            ],
        };
        assert_eq!(
            engine.infer_expression_type(&int_array),
            Type::Array(Box::new(Type::Integer), 3)
        );

        // Homogeneous string array
        let string_array = Expr::ArrayLiteral {
            elements: vec![
                Expr::Literal(Literal::String("a".to_string())),
                Expr::Literal(Literal::String("b".to_string())),
            ],
        };
        assert_eq!(
            engine.infer_expression_type(&string_array),
            Type::Array(Box::new(Type::String), 2)
        );

        // Empty array (unknown element type)
        let empty_array = Expr::ArrayLiteral { elements: vec![] };
        assert_eq!(
            engine.infer_expression_type(&empty_array),
            Type::Array(Box::new(Type::Unknown), 0)
        );
    }

    #[test]
    fn test_tuple_type_inference() {
        let engine = create_test_engine();

        let tuple = Expr::Tuple {
            elements: vec![
                Expr::Literal(Literal::Integer(42)),
                Expr::Literal(Literal::String("hello".to_string())),
                Expr::Literal(Literal::Boolean(true)),
            ],
        };
        assert_eq!(
            engine.infer_expression_type(&tuple),
            Type::Tuple(vec![Type::Integer, Type::String, Type::Boolean])
        );
    }

    #[test]
    fn test_function_call_type_inference() {
        let engine = create_test_engine();

        // Built-in function calls
        let len_call = Expr::Call {
            callee: Box::new(Expr::Variable("len".to_string())),
            arguments: vec![Expr::Literal(Literal::String("hello".to_string()))],
        };
        assert_eq!(engine.infer_expression_type(&len_call), Type::Integer);

        let int_call = Expr::Call {
            callee: Box::new(Expr::Variable("int".to_string())),
            arguments: vec![Expr::Literal(Literal::String("42".to_string()))],
        };
        assert_eq!(engine.infer_expression_type(&int_call), Type::Integer);

        let print_call = Expr::Call {
            callee: Box::new(Expr::Variable("println".to_string())),
            arguments: vec![Expr::Literal(Literal::String("hello".to_string()))],
        };
        assert_eq!(engine.infer_expression_type(&print_call), Type::Void);
    }

    #[test]
    fn test_parameter_type_inference_from_usage() {
        let mut engine = create_test_engine();

        // Simulate parameter usage: `param + 5`
        let usage = Expr::Binary {
            left: Box::new(Expr::Variable("param".to_string())),
            operator: BinaryOperator::Plus,
            right: Box::new(Expr::Literal(Literal::Integer(5))),
        };

        let inferred_type = engine.infer_parameter_type("param", &[usage]);
        assert_eq!(inferred_type, Some(Type::Integer));

        // Simulate parameter usage: `param * 3.14`
        let float_usage = Expr::Binary {
            left: Box::new(Expr::Variable("value".to_string())),
            operator: BinaryOperator::Star,
            right: Box::new(Expr::Literal(Literal::F64(3.14))),
        };

        let inferred_float_type = engine.infer_parameter_type("value", &[float_usage]);
        assert_eq!(inferred_float_type, Some(Type::F64));
    }

    #[test]
    fn test_multiple_usage_parameter_inference() {
        let mut engine = create_test_engine();

        // Multiple usages that should all infer the same type
        let usage1 = Expr::Binary {
            left: Box::new(Expr::Variable("x".to_string())),
            operator: BinaryOperator::Plus,
            right: Box::new(Expr::Literal(Literal::Integer(1))),
        };

        let usage2 = Expr::Binary {
            left: Box::new(Expr::Variable("x".to_string())),
            operator: BinaryOperator::Minus,
            right: Box::new(Expr::Literal(Literal::Integer(2))),
        };

        let inferred_type = engine.infer_parameter_type("x", &[usage1, usage2]);
        assert_eq!(inferred_type, Some(Type::Integer));
    }

    #[test]
    fn test_type_unification() {
        let engine = create_test_engine();

        // Same types should unify
        assert_eq!(
            engine.unify_types(&[Type::Integer, Type::Integer]),
            Some(Type::Integer)
        );

        // Different numeric types should promote
        assert_eq!(
            engine.unify_types(&[Type::Integer, Type::F64]),
            Some(Type::F64)
        );

        // String types should unify
        assert_eq!(
            engine.unify_types(&[Type::String, Type::String]),
            Some(Type::String)
        );

        // Incompatible types should not unify
        assert_eq!(
            engine.unify_types(&[Type::Integer, Type::String]),
            None
        );

        // Multiple types should find common type
        assert_eq!(
            engine.unify_types(&[Type::I8, Type::I16, Type::Integer]),
            Some(Type::Integer)
        );
    }

    #[test]
    fn test_function_return_type_inference() {
        let mut engine = create_test_engine();

        // Function with explicit integer return
        let body_with_return = vec![
            Statement::Return {
                value: Some(Box::new(Expr::Literal(Literal::Integer(42)))),
            }
        ];
        assert_eq!(
            engine.infer_function_return_type(&body_with_return),
            Some(Type::Integer)
        );

        // Function with explicit void return
        let body_with_void_return = vec![Statement::Return { value: None }];
        assert_eq!(
            engine.infer_function_return_type(&body_with_void_return),
            Some(Type::Void)
        );

        // Function with implicit return (last expression)
        let body_with_implicit_return = vec![
            Statement::Expression(Expr::Literal(Literal::String("hello".to_string())))
        ];
        assert_eq!(
            engine.infer_function_return_type(&body_with_implicit_return),
            Some(Type::String)
        );

        // Function with no return (void)
        let empty_body: Vec<Statement> = vec![];
        assert_eq!(
            engine.infer_function_return_type(&empty_body),
            Some(Type::Void)
        );
    }

    #[test]
    fn test_if_expression_type_inference() {
        let mut engine = create_test_engine();

        let if_expr = Expr::IfExpression {
            condition: Box::new(Expr::Literal(Literal::Boolean(true))),
            then_branch: Box::new(Expr::Literal(Literal::Integer(42))),
            else_branch: Box::new(Expr::Literal(Literal::Integer(24))),
        };

        // Should return the unified type of then and else branches
        assert_eq!(engine.infer_expression_type(&if_expr), Type::Integer);
    }

    #[test]
    fn test_complex_expression_inference() {
        let engine = create_test_engine();

        // Complex nested expression: (a + b) * c
        let complex_expr = Expr::Binary {
            left: Box::new(Expr::Binary {
                left: Box::new(Expr::Variable("a".to_string())),
                operator: BinaryOperator::Plus,
                right: Box::new(Expr::Variable("b".to_string())),
            }),
            operator: BinaryOperator::Star,
            right: Box::new(Expr::Variable("c".to_string())),
        };

        // Should be unknown since variables aren't typed yet
        assert_eq!(engine.infer_expression_type(&complex_expr), Type::Unknown);
    }

    #[test]
    fn test_builtin_function_inference() {
        let mut engine = create_test_engine();

        // Set up some variable types to test function argument inference
        engine.set_variable_type("name".to_string(), Type::String);
        engine.set_variable_type("length".to_string(), Type::Integer);

        // Test len() function with string
        let len_str_call = Expr::Call {
            callee: Box::new(Expr::Variable("len".to_string())),
            arguments: vec![Expr::Variable("name".to_string())],
        };
        assert_eq!(engine.infer_expression_type(&len_str_call), Type::Integer);

        // Test str() function
        let str_call = Expr::Call {
            callee: Box::new(Expr::Variable("str".to_string())),
            arguments: vec![Expr::Variable("length".to_string())],
        };
        assert_eq!(engine.infer_expression_type(&str_call), Type::String);

        // Test math functions
        let abs_call = Expr::Call {
            callee: Box::new(Expr::Variable("abs".to_string())),
            arguments: vec![Expr::Literal(Literal::Integer(-5))],
        };
        assert_eq!(engine.infer_expression_type(&abs_call), Type::Integer);

        let abs_float_call = Expr::Call {
            callee: Box::new(Expr::Variable("abs_float".to_string())),
            arguments: vec![Expr::Literal(Literal::F64(-5.5))],
        };
        assert_eq!(engine.infer_expression_type(&abs_float_call), Type::F64);
    }

    #[test]
    fn test_string_operation_inference() {
        let engine = create_test_engine();

        // String concatenation
        let concat = Expr::Binary {
            left: Box::new(Expr::Literal(Literal::String("hello".to_string()))),
            operator: BinaryOperator::Plus,
            right: Box::new(Expr::Literal(Literal::String(" world".to_string()))),
        };
        assert_eq!(engine.infer_expression_type(&concat), Type::String);
    }

    #[test]
    fn test_unary_operation_inference() {
        let engine = create_test_engine();

        // Negation
        let negate = Expr::Unary {
            operator: UnaryOperator::Negate,
            operand: Box::new(Expr::Literal(Literal::Integer(5))),
        };
        assert_eq!(engine.infer_expression_type(&negate), Type::Integer);

        // Logical not
        let not = Expr::Unary {
            operator: UnaryOperator::Not,
            operand: Box::new(Expr::Literal(Literal::Boolean(true))),
        };
        assert_eq!(engine.infer_expression_type(&not), Type::Boolean);
    }

    #[test]
    fn test_end_to_end_inference() {
        let code = r#"
def add(a, b) {
    return a + b;
}

def main() {
    store result = add(5, 3);
    println(result);
}
"#;

        let lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().expect("Failed to parse");

        let mut analyzer = SemanticAnalyzer::new_with_file_path(None);
        let result = analyzer.analyze_program(ast, true);

        assert!(result.is_ok(), "Semantic analysis should succeed with type inference");
    }
}