use miette::Diagnostic;

use thiserror::Error;

use crate::{ast::BinaryOpType, source_info::Span, types::MonoType};

#[derive(Diagnostic, Error, Clone, Debug)]
pub enum SemanticError {
    #[error("Unknown analysis error")]
    UnknownError,

    #[error("Undeclared variable")]
    #[diagnostic(help("Declare a variable before using it (e.g. `let {name} = 1`)"))]
    UndeclaredVariable {
        name: String,

        #[label = "Undeclared variable `{name}`"]
        span: Span,
    },

    #[error("Unknown type `{name}`")]
    UndefinedType {
        name: String,

        #[label = "the type `{name}` is nowhere to be found"]
        span: Span,
    },

    #[error("Function `{name}` already exists")]
    #[diagnostic(help("All functions names must be unique across the program"))]
    FunctionAlreadyDeclared {
        name: String,

        #[label("the function `{name}` was first declared here")]
        first_declaration: Span,

        #[label = "and then declared again here"]
        span: Span,
    },

    #[error("Invalid type annotation `{name}`")]
    InvalidTypeAnnotation {
        name: String,

        #[label = "invalid type annotation `{name}`"]
        span: Span,
    },

    #[error("Variable `{name}` already defined")]
    VariableAlreadyDeclared {
        name: String,

        #[label("the variable `{name}` was first declared here")]
        first_declaration: Span,

        #[label = "and then declared again here"]
        span: Span,
    },

    #[error("Argument `{name}` already declared")]
    #[diagnostic(help("Function arguments must have unique names"))]
    ArgumentAlreadyDefined {
        name: String,

        #[label("the argument `{name}` was first declared here")]
        first_declaration: Span,

        #[label = "and then declared again here"]
        span: Span,
    },

    #[error("Mismatched types")]
    MismatchedTypes {
        lhs: MonoType,
        rhs: MonoType,

        #[label("the type `{lhs}`")]
        lhs_span: Span,

        #[label("cannot be equated with type `{rhs}`")]
        rhs_span: Span,
    },

    #[error("Mismatched types")]
    ExpectedType {
        expected: MonoType,
        found: MonoType,

        #[label("expected type `{expected}`, found `{found}`")]
        span: Span,
    },

    #[error("If condition invalid type")]
    #[diagnostic(help("The condition of an `if` statement must be a `Bool`"))]
    IfConditionTypeMismatch {
        ty: MonoType,

        #[label("in this `if` statement")]
        if_span: Span,

        #[label("the condition must be a `Bool`. Found `{ty}`")]
        span: Span,
    },

    #[error("Expected `{func_ty}` in return statement. Found `{return_ty}`")]
    #[diagnostic(help("The type returned must match the function's return type"))]
    ReturnTypeMismatch {
        func_ty: MonoType,

        #[label("the return type of the function")]
        func_return_span: Span,

        return_ty: MonoType,

        #[label("does not match this return type")]
        return_span: Span,
    },

    #[error("Left hand side `{lhs_type}` does not match right hand side `{rhs_type}`")]
    #[diagnostic(help(
        "The left and right hand sides of a binary operation must have the same type"
    ))]
    InvalidBinaryOperation {
        op: BinaryOpType,
        lhs_type: MonoType,
        rhs_type: MonoType,

        #[label("`{op}` expression")]
        bin_span: Span,

        #[label("this is of type `{lhs_type}`")]
        lhs_span: Span,

        #[label("this is of type `{rhs_type}`")]
        rhs_span: Span,
    },

    #[error("Invalid function call")]
    #[diagnostic(help(
        "The left hand side of a function call must be an identifier to a function"
    ))]
    InvalidFunctionCall {
        #[label("this is not an identifier to a function")]
        span: Span,
    },

    #[error("Functions have different number of arguments")]
    IncorrectNumberOfArguments {
        expected: usize,
        found: usize,

        #[label("this function has {expected} parameters")]
        func_decl: Span,

        #[label("found {found} arguments")]
        found_span: Span,
    },

    #[error("{op} operations are only valid for `Int` or `Float` types. We found `{ty}`")]
    InvalidBinaryExpressionTypes {
        ty: MonoType,

        op: BinaryOpType,

        #[label("found `{ty}`. expected `Int` or `Float`")]
        span: Span,
    },

    #[error("Return statements can only be used inside functions")]
    ReturnOutsideOfFunction {
        #[label("return statements can only be used inside functions")]
        span: Span,
    },
}

pub type SemanticResult<T> = Result<T, SemanticError>;
