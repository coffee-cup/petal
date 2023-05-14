use super::{
    ast::{Expr, Program, Stmt},
    token::{TokenType},
};

type TT = TokenType;

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
    // Variables https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/Variables
    GetLocal(String),
    Drop,
}

type Chunk = Vec<WatInstruction>;

pub struct IRModule {
    pub funcs: Vec<IRFunction>,
}

impl IRModule {
    pub fn new() -> Self {
        Self { funcs: Vec::new() }
    }

    pub fn add_function(&mut self, func: IRFunction) {
        self.funcs.push(func);
    }
}

pub struct IRParam {
    pub name: String,
    pub ty: WatValueType,
}

pub struct IRFunction {
    pub name: String,
    pub params: Vec<IRParam>,
    pub body: Chunk,
}

trait ToWatInstructions {
    fn to_ir_chunk(&self) -> Chunk;
}

pub trait InstructionStackCount {
    fn stack_count(&self) -> i32;
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

    pub fn generate_ir_from_program(&mut self, program: &Program) -> IRModule {
        let mut ir_module = IRModule::new();

        let main_func = self.generate_main_function(program);
        ir_module.add_function(main_func);

        ir_module
    }

    fn generate_main_function(&mut self, program: &Program) -> IRFunction {
        let mut chunk = Chunk::new();

        for stmt in &program.statements {
            match stmt {
                Stmt::Func { .. } => todo!("program to ir",),
                stmt => {
                    chunk.extend(stmt.to_ir_chunk());
                }
            }
        }

        self.drop_chunk_stack(&mut chunk);

        

        IRFunction {
            name: String::from("petal_main"),
            params: Vec::new(),
            body: chunk,
        }
    }

    fn drop_chunk_stack(&self, chunk: &mut Chunk) {
        let stack_size = chunk.iter().fold(0, |acc, instr| acc + instr.stack_count());

        for _ in 0..stack_size {
            chunk.push(WatInstruction::Drop);
        }
    }
}

// impl Program {
//     pub fn to_ir(&self) -> IRModule {
//         let mut main_chunk = Chunk::new();
//         let mut funcs: Vec<IRFunction> = Vec::new();

//         for stmt in &self.statements {
//             match stmt {
//                 Stmt::Func { .. } => todo!("program to ir",),
//                 stmt => main_chunk.extend(stmt.to_ir_chunk()),
//             }
//         }

//         let main_func = IRFunction {
//             name: String::from("petal_main"),
//             params: Vec::new(),
//             body: main_chunk,
//         };

//         funcs.push(main_func);

//         let module = IRModule { funcs };

//         module
//     }
// }

// impl ToWatInstructions for Program {
//     fn to_ir_chunk(&self) -> Chunk {
//         let mut main_chunk = Chunk::new();

//         for stmt in &self.statements {
//             match stmt {
//                 Stmt::Fun { .. } => todo!("to_ir_chunk for {:?}", stmt),
//                 stmt => main_chunk.extend(stmt.to_ir_chunk()),
//             }
//         }

//         result
//     }
// }

impl ToWatInstructions for Stmt {
    fn to_ir_chunk(&self) -> Chunk {
        match self {
            Stmt::ExprStmt { expr, .. } => expr.to_ir_chunk(),
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
            _ => todo!("to_ir_chunk for {:?}", self),
        }
    }
}

impl InstructionStackCount for WatInstruction {
    fn stack_count(&self) -> i32 {
        match self {
            WatInstruction::Const(_) => 1,
            WatInstruction::Add(_) => -1,
            WatInstruction::Sub(_) => -1,
            WatInstruction::Mult(_) => -1,
            WatInstruction::Div(_) => -1,
            WatInstruction::GetLocal(_) => 1,
            WatInstruction::Drop => -1,
            _ => todo!("stack_count for {:?}", self),
        }
    }
}

impl InstructionStackCount for Stmt {
    fn stack_count(&self) -> i32 {
        match self {
            Stmt::ExprStmt { expr, .. } => expr.stack_count(),
            _ => todo!("stack_count for {:?}", self),
        }
    }
}

impl InstructionStackCount for Expr {
    fn stack_count(&self) -> i32 {
        match self {
            Expr::Number { .. } => 1,
            Expr::BinaryOp { .. } => -1,
            Expr::Ident { .. } => 1,
            _ => todo!("stack_count for {:?}", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::petal::{
        lexer::Lexer,
        parser::Parser,
        precedence::Precedence,
    };

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
