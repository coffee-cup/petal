use super::{
    ast::Expr,
    token::{Token, TokenType},
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
}

pub trait ToWatInstructions {
    fn to_ir_chunk(&self) -> Vec<WatInstruction>;
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

impl ToWatInstructions for Expr {
    fn to_ir_chunk(&self) -> Vec<WatInstruction> {
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

#[cfg(test)]
mod tests {
    use crate::petal::{
        lexer::Lexer,
        parser::Parser,
        positions::{Pos, Span},
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
}
