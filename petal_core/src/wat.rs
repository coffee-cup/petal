use std::fmt::Display;

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
    Return,
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
    pub main_func: String,
}

impl Display for WatValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use WatValueType::*;

        match self {
            I32 => write!(f, "i32"),
            I64 => write!(f, "i64"),
            F32 => write!(f, "f32"),
            F64 => write!(f, "f64"),
        }
    }
}

impl Display for WatValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use WatValue::*;

        match self {
            I32(i) => write!(f, "{}", i),
            I64(i) => write!(f, "{}", i),
            F32(i) => write!(f, "{}", i),
            F64(i) => write!(f, "{}", i),
        }
    }
}

impl Display for WatInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use WatInstruction::*;

        match self {
            Const(value) => match value {
                WatValue::I32(value) => write!(f, "i32.const {}", value),
                WatValue::I64(value) => write!(f, "i64.const {}", value),
                WatValue::F32(value) => write!(f, "f32.const {}", value),
                WatValue::F64(value) => write!(f, "f64.const {}", value),
            },
            Equal(ty) => write!(f, "eq.{}", ty),
            NotEqual(ty) => write!(f, "ne.{}", ty),

            GetLocal(name) => write!(f, "local.get ${}", name),
            SetLocal(name) => write!(f, "local.set ${}", name),

            Add(ty) => write!(f, "{}.add", ty),
            Sub(ty) => write!(f, "{}.sub", ty),
            Mult(ty) => write!(f, "{}.mul", ty),
            Div(ty) => write!(f, "{}.div", ty),

            Return => write!(f, "return"),
            Drop => write!(f, "drop"),

            v => todo!("implement fmt for {:?}", v),
        }
    }
}

impl Display for WatFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params = self
            .signature
            .params
            .iter()
            .map(|p| format!("(param ${}: {})", p.name, p.ty))
            .collect::<Vec<_>>()
            .join("\n");

        let locals = self
            .locals
            .iter()
            .map(|l| format!("(local ${} {})", l.name, l.ty))
            .collect::<Vec<_>>()
            .join("\n");

        let body = self
            .instructions
            .iter()
            .map(|i| format!("  {}", i))
            .collect::<Vec<_>>()
            .join("\n");

        write!(
            f,
            "(func ${name} {export} {params} {return_ty}
  {locals}

{body}

)",
            name = self.signature.name,
            export = if self.signature.is_exported {
                format!("(export \"{}\")", self.signature.name)
            } else {
                String::new()
            },
            params = params,
            return_ty = match &self.signature.return_ty {
                Some(ty) => format!("(result {})", ty),
                None => String::new(),
            },
            locals = locals,
            body = body,
        )
    }
}

impl Display for WatModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let funcs = self
            .functions
            .iter()
            .map(|f| f.to_string())
            .collect::<Vec<_>>()
            .join("\n\n");

        write!(
            f,
            "(module

{funcs}
    
)",
            funcs = funcs,
        )
    }
}
