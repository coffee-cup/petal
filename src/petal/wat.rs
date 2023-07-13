use crate::petal::{
    ir::{IRFunction, IRFunctionSignature, IRProgram},
    types::MonoType,
};

#[derive(PartialEq, Clone, Debug)]
pub enum WatValueType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(PartialEq, Clone, Debug)]
pub enum WatValue {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(PartialEq, Clone, Debug)]
pub enum WatInstruction {
    // Numbers https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/Numeric
    Const(WatValue),
    Equal(WatValueType),
    NotEqual(WatValueType),
    GreaterThan(WatValueType),
    GreaterOrEqual(WatValueType),
    LessThan(WatValueType),
    LessOrEqual(WatValueType),
    Add(WatValueType),
    Sub(WatValueType),
    Mult(WatValueType),
    Div(WatValueType),
    // Conversion
    Extend, // i32 -> i64
    Wrap,   // i64 -> i32
    Truncate(WatValueType, WatValueType),
    // Floating
    Min(WatValueType),
    Max(WatValueType),
    Ceil(WatValueType),
    Floor(WatValueType),
    Nearest(WatValueType),
    // Variables https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/Variables
    Local(String, WatValueType),
    GetLocal(String),
    SetLocal(String),
    // Functions
    Call(String, usize),
    // Control flow
    If(Vec<WatInstruction>, Vec<WatInstruction>),
    Drop,
}

#[derive(Clone, Debug)]
pub struct WatFunction {
    pub signature: WatFunctionSignature,
    pub locals: Vec<WatLocal>,
    pub instructions: Vec<WatInstruction>,
}

#[derive(Clone, Debug)]
pub struct WatParam {
    pub name: String,
    pub ty: WatValueType,
}

#[derive(Clone, Debug)]
pub struct WatLocal {
    pub name: String,
    pub ty: WatValueType,
}

#[derive(Clone, Debug)]
pub struct WatFunctionSignature {
    pub name: String,
    pub return_ty: Option<WatValueType>,
    pub is_exported: bool,
    pub params: Vec<WatParam>,
}

#[derive(Clone, Debug)]
pub struct WatModule {
    pub functions: Vec<WatFunction>,
}

pub struct WatGeneration {}
