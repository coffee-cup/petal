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
    Equal(WatValueType, Box<WatInstruction>, Box<WatInstruction>),
    NotEqual(WatValueType, Box<WatInstruction>, Box<WatInstruction>),
    GreaterThan(WatValueType, Box<WatInstruction>, Box<WatInstruction>),
    GreaterOrEqual(WatValueType, Box<WatInstruction>, Box<WatInstruction>),
    LessThan(WatValueType, Box<WatInstruction>, Box<WatInstruction>),
    LessOrEqual(WatValueType, Box<WatInstruction>, Box<WatInstruction>),
    Add(WatValueType, Box<WatInstruction>, Box<WatInstruction>),
    Sub(WatValueType, Box<WatInstruction>, Box<WatInstruction>),
    Mult(WatValueType, Box<WatInstruction>, Box<WatInstruction>),
    Div(WatValueType, Box<WatInstruction>, Box<WatInstruction>),
    // Variables https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/Variables
    GetLocal(String),
}

pub trait ToWatInstructions {
    fn to_ir_chunk(&self) -> Vec<WatInstruction>;
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

                let left = left.get(0).unwrap();
                let right = right.get(0).unwrap();

                match op.token_type {
                    TT::Plus => vec![WatInstruction::Add(
                        WatValueType::F64,
                        Box::new(left.clone()),
                        Box::new(right.clone()),
                    )],
                    TT::Minus => vec![WatInstruction::Sub(
                        WatValueType::F64,
                        Box::new(left.clone()),
                        Box::new(right.clone()),
                    )],
                    TT::Star => vec![WatInstruction::Mult(
                        WatValueType::F64,
                        Box::new(left.clone()),
                        Box::new(right.clone()),
                    )],
                    TT::Slash => vec![WatInstruction::Div(
                        WatValueType::F64,
                        Box::new(left.clone()),
                        Box::new(right.clone()),
                    )],
                    TT::EqualEqual => vec![WatInstruction::Equal(
                        WatValueType::F64,
                        Box::new(left.clone()),
                        Box::new(right.clone()),
                    )],
                    TT::BangEqual => vec![WatInstruction::NotEqual(
                        WatValueType::F64,
                        Box::new(left.clone()),
                        Box::new(right.clone()),
                    )],
                    TT::Greater => vec![WatInstruction::GreaterThan(
                        WatValueType::F64,
                        Box::new(left.clone()),
                        Box::new(right.clone()),
                    )],
                    TT::GreaterEqual => vec![WatInstruction::GreaterOrEqual(
                        WatValueType::F64,
                        Box::new(left.clone()),
                        Box::new(right.clone()),
                    )],
                    TT::Less => vec![WatInstruction::LessThan(
                        WatValueType::F64,
                        Box::new(left.clone()),
                        Box::new(right.clone()),
                    )],
                    TT::LessEqual => vec![WatInstruction::LessOrEqual(
                        WatValueType::F64,
                        Box::new(left.clone()),
                        Box::new(right.clone()),
                    )],
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
            vec![Sub(
                F64,
                Box::new(Add(
                    F64,
                    Box::new(Const(WatValue::F64(1.0))),
                    Box::new(Mult(
                        F64,
                        Box::new(Const(WatValue::F64(2.0))),
                        Box::new(Const(WatValue::F64(3.0)))
                    ))
                )),
                Box::new(Div(
                    F64,
                    Box::new(Const(WatValue::F64(4.0))),
                    Box::new(Const(WatValue::F64(5.0)))
                ))
            )]
        );
    }
}
