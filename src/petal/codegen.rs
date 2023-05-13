use crate::petal::ir::WatValue;

use super::ir::{IRFunction, WatInstruction, WatValueType};

pub struct Codegen {}

impl Codegen {
    pub fn new() -> Self {
        Codegen {}
    }

    pub fn generate_module(&mut self) -> String {
        String::from(
            "
        (module
            (import \"host\" \"log\" (func $log (param i32)))

            (func (export \"hello\")
                i32.const 11
                call $log
            )
        )",
        )
    }
}

pub trait ToWat {
    fn to_wat(&self) -> String;
}

impl ToWat for IRFunction {
    fn to_wat(&self) -> String {
        let params = self
            .params
            .iter()
            .map(|p| format!("(param ${} {})", p.name, p.ty.to_wat()))
            .collect::<Vec<_>>()
            .join(" ");

        let body = self
            .body
            .iter()
            .map(|i| format!("  {}", i.to_wat()))
            .collect::<Vec<_>>()
            .join("\n");

        format!(
            "(func ${} {}
{}
)",
            self.name, params, body
        )
    }
}

impl ToWat for WatValueType {
    fn to_wat(&self) -> String {
        match self {
            WatValueType::I32 => String::from("i32"),
            WatValueType::I64 => String::from("i64"),
            WatValueType::F32 => String::from("f32"),
            WatValueType::F64 => String::from("f64"),
        }
    }
}

impl ToWat for WatInstruction {
    fn to_wat(&self) -> String {
        use WatInstruction::*;

        match self {
            Const(wat_value) => match wat_value {
                WatValue::I32(value) => format!("i32.const {}", value),
                WatValue::I64(value) => format!("i64.const {}", value),
                WatValue::F32(value) => format!("f32.const {}", value),
                WatValue::F64(value) => format!("f64.const {}", value),
            },

            Add(wat_type) => format!("{}.add", wat_type.to_wat(),),
            Sub(wat_type) => format!("{}.sub", wat_type.to_wat(),),
            Mult(wat_type) => format!("{}.mul", wat_type.to_wat(),),
            Div(wat_type) => format!("{}.div", wat_type.to_wat(),),

            GetLocal(name) => format!("get_local ${}", name),

            _ => todo!("Implement IRToWat for WatInstruction"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::petal::{
        ir::ToWatInstructions,
        lexer::Lexer,
        parser::Parser,
        positions::{Pos, Span},
        precedence::Precedence,
    };

    use super::*;
    use WatInstruction::*;

    fn to_wat(s: &str) -> String {
        let mut lexer = Lexer::new(s);
        let mut parser = Parser::new(&mut lexer);
        let expr = parser.parse_expression(Precedence::Lowest).unwrap();
        let instructions = expr.to_ir_chunk();
        instructions
            .iter()
            .map(|i| i.to_wat())
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn to_vec(instrs: Vec<&str>) -> String {
        instrs
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    }

    #[test]
    fn test_codegen_expr() {
        assert_eq!(to_wat("1"), "f64.const 1".to_string());
        assert_eq!(
            to_wat("1 + 2"),
            to_vec(vec!["f64.const 1", "f64.const 2", "f64.add"])
        );
        assert_eq!(
            to_wat("1 + 2 * 3 - 4 / 5"),
            to_vec(vec![
                "f64.const 1",
                "f64.const 2",
                "f64.const 3",
                "f64.mul",
                "f64.add",
                "f64.const 4",
                "f64.const 5",
                "f64.div",
                "f64.sub"
            ])
        );

        assert_eq!(to_wat("a"), "(get_local $a)".to_string());
    }
}
