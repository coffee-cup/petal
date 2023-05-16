use wast::kw::then;

use super::{
    ast::{Expr, FuncDecl, Program, Stmt},
    token::TokenType,
};

type TT = TokenType;

const MAIN_FUNCTION_NAME: &str = "_start";

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

type Chunk = Vec<WatInstruction>;

#[derive(Clone, Debug)]
pub struct WatModule {
    pub funcs: Vec<WatFunction>,
}

impl WatModule {
    pub fn new() -> Self {
        Self { funcs: Vec::new() }
    }

    pub fn add_function(&mut self, func: WatFunction) {
        self.funcs.push(func);
    }
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
    pub params: Vec<WatParam>,
    pub return_ty: Option<WatValueType>,
    pub is_exported: bool,
}

#[derive(Clone, Debug)]
pub struct WatFunction {
    pub name: String,
    pub signature: WatFunctionSignature,
    pub locals: Vec<WatLocal>,
    pub body: Chunk,
}

trait ToWatInstructions {
    fn to_ir_chunk(&self) -> Chunk;
}

trait InstructionStackCount {
    fn stack_count(&self) -> i32;
}

trait HasLocals {
    fn locals(&self) -> Vec<WatLocal>;
}

macro_rules! instrs {
    ($($vec:expr),+ $(,)?) => {{
        let mut result = Vec::new();
        $(
            result.extend($vec.iter().cloned());
        )+
        result
    }};
}

pub struct IRGenerator {}

impl IRGenerator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn generate_ir_from_program(&mut self, program: &Program) -> WatModule {
        let mut ir_module = WatModule::new();

        for stmt in &program.statements {
            match stmt {
                Stmt::Func(func) => {
                    let ir_func = self.generate_function(func);
                    ir_module.add_function(ir_func);
                }
                _ => {}
            }
        }

        let main_func = self.generate_main_function(program);
        ir_module.add_function(main_func);

        ir_module
    }

    fn generate_function(&mut self, func: &FuncDecl) -> WatFunction {
        let mut chunk = Chunk::new();
        let mut locals = Vec::new();

        let params = func
            .args
            .iter()
            .map(|arg| WatParam {
                name: arg.name.clone(),
                ty: WatValueType::F64,
            })
            .collect();

        for stmt in &func.body.statements {
            match stmt {
                Stmt::Func { .. } => panic!("Functions cannot appear inside functions"),
                stmt => {
                    locals.extend(stmt.locals());
                    chunk.extend(stmt.to_ir_chunk())
                }
            }
        }

        let return_ty = Some(WatValueType::F64);

        self.drop_chunk_stack(&mut chunk, return_ty.is_some());

        let signature = WatFunctionSignature {
            params,
            return_ty,
            is_exported: func.is_exported,
        };

        WatFunction {
            name: func.name.clone(),
            signature,
            locals,
            body: chunk,
        }
    }

    fn generate_main_function(&mut self, program: &Program) -> WatFunction {
        let mut chunk = Chunk::new();
        let mut locals = Vec::new();

        for stmt in &program.statements {
            match stmt {
                // Functions are handled above
                Stmt::Func { .. } => {}
                stmt => {
                    locals.extend(stmt.locals());
                    chunk.extend(stmt.to_ir_chunk())
                }
            }
        }

        let return_ty = Some(WatValueType::F64);

        self.drop_chunk_stack(&mut chunk, return_ty.is_some());

        let signature = WatFunctionSignature {
            params: Vec::new(),
            return_ty,
            is_exported: true,
        };

        WatFunction {
            name: String::from(MAIN_FUNCTION_NAME),
            signature,
            locals,
            body: chunk,
        }
    }

    fn drop_chunk_stack(&self, chunk: &mut Chunk, returns_value: bool) {
        let mut size_to_drop = chunk.iter().fold(0, |acc, instr| acc + instr.stack_count());
        if returns_value {
            size_to_drop -= 1;
        }

        for _ in 0..size_to_drop {
            chunk.push(WatInstruction::Drop);
        }
    }
}

impl ToWatInstructions for Stmt {
    fn to_ir_chunk(&self) -> Chunk {
        match self {
            Stmt::ExprStmt { expr, .. } => expr.to_ir_chunk(),
            Stmt::Let { name, init, .. } => {
                let mut chunk = init.to_ir_chunk();
                chunk.push(WatInstruction::SetLocal(name.clone()));
                chunk
            }
            Stmt::IfStmt {
                condition,
                then_block,
                else_block,
                ..
            } => {
                let mut chunk = condition.to_ir_chunk();
                chunk.extend(vec![WatInstruction::Truncate(
                    WatValueType::F64,
                    WatValueType::I32,
                )]);

                let then_block = then_block.to_ir_chunk();
                let else_block = else_block
                    .as_ref()
                    .map(|e| e.to_ir_chunk())
                    .unwrap_or_else(Vec::new);

                chunk.push(WatInstruction::If(then_block, else_block));
                chunk
            }
            _ => todo!("to_ir_chunk for {:?}", self),
        }
    }
}

impl ToWatInstructions for Expr {
    fn to_ir_chunk(&self) -> Chunk {
        match self {
            Expr::Number { value, .. } => vec![WatInstruction::Const(WatValue::F64(*value))],
            Expr::BinaryOp {
                left, op, right, ..
            } => {
                let left = left.to_ir_chunk();
                let right = right.to_ir_chunk();

                match op.token_type {
                    TT::Plus => instrs!(left, right, vec![WatInstruction::Add(WatValueType::F64)]),
                    TT::Minus => instrs!(left, right, vec![WatInstruction::Sub(WatValueType::F64)]),
                    TT::Star => {
                        instrs!(left, right, vec![WatInstruction::Mult(WatValueType::F64)])
                    }
                    TT::Slash => instrs!(left, right, vec![WatInstruction::Div(WatValueType::F64)]),

                    _ => todo!("to_ir_chunk for {:?}", self),
                }
            }
            Expr::Ident { name, .. } => vec![WatInstruction::GetLocal(name.clone())],
            Expr::Call { callee, args, .. } => {
                let mut chunk = Chunk::new();

                let name = match *callee.clone() {
                    Expr::Ident { name, .. } => name,
                    _ => panic!("Callee must be an identifier"),
                };

                for arg in args {
                    chunk.extend(arg.to_ir_chunk());
                }

                chunk.push(WatInstruction::Call(name, args.len()));

                chunk
            }
            _ => todo!("to_ir_chunk for {:?}", self),
        }
    }
}

impl HasLocals for Stmt {
    fn locals(&self) -> Vec<WatLocal> {
        match self {
            Stmt::Let { name, .. } => vec![WatLocal {
                name: name.clone(),
                ty: WatValueType::F64,
            }],
            Stmt::IfStmt {
                then_block,
                else_block,
                ..
            } => {
                let mut locals = Vec::new();
                locals.extend(then_block.locals());
                locals.extend(
                    else_block
                        .as_ref()
                        .map(|s| s.locals())
                        .unwrap_or_else(Vec::new),
                );
                locals
            }
            _ => Vec::new(),
        }
    }
}

impl InstructionStackCount for WatInstruction {
    fn stack_count(&self) -> i32 {
        use WatInstruction::*;

        match self {
            Const(_) => 1,
            Add(_) => -1,
            Sub(_) => -1,
            Mult(_) => -1,
            Div(_) => -1,
            GetLocal(_) => 1,
            Drop => -1,
            GetLocal(_) => 0,
            SetLocal(_) => -1,

            Min(_) => -1,
            Max(_) => -1,
            Ceil(_) => -1,
            Floor(_) => -1,
            Nearest(_) => -1,

            Extend => -1,
            Wrap => -1,
            Truncate(_, _) => -1,

            // TODO: Need to check if a value is returned
            Call(_, arity) => -(*arity as i32) + 1,

            _ => 0,
            // _ => todo!("stack_count for {:?}", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::petal::{lexer::Lexer, parser::Parser, precedence::Precedence};

    use super::*;
    use WatInstruction::*;
    use WatValueType::*;

    fn parse_expr(s: &str) -> Expr {
        let mut lexer = Lexer::new(s);
        let mut parser = Parser::new(&mut lexer);
        parser.parse_expression(Precedence::Lowest).unwrap()
    }

    #[test]
    fn test_translate_expr() {
        assert_eq!(
            parse_expr("1").to_ir_chunk(),
            vec![Const(WatValue::F64(1.0))]
        );

        assert_eq!(
            parse_expr("1 + 2 * 3 - 4 / 5").to_ir_chunk(),
            vec![
                Const(WatValue::F64(1.0)),
                Const(WatValue::F64(2.0)),
                Const(WatValue::F64(3.0)),
                Mult(F64),
                Add(F64),
                Const(WatValue::F64(4.0)),
                Const(WatValue::F64(5.0)),
                Div(F64),
                Sub(F64)
            ]
        );
    }

    #[test]
    fn test_stack_count() {
        let instrs = vec![
            Const(WatValue::F64(1.0)),
            Const(WatValue::F64(2.0)),
            Const(WatValue::F64(3.0)),
            Mult(F64),
            Add(F64),
            Const(WatValue::F64(4.0)),
            Const(WatValue::F64(5.0)),
            Div(F64),
            Sub(F64),
        ];

        assert_eq!(
            instrs
                .iter()
                .fold(0, |acc, instr| acc + instr.stack_count()),
            1
        );
    }
}
