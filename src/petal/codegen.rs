use crate::petal::ir::WatValue;

use super::ir::{IRFunction, IRModule, WatInstruction, WatValueType};

pub trait ToWat {
    fn to_wat(&self) -> String;
}

impl ToWat for IRModule {
    fn to_wat(&self) -> String {
        let funcs = self
            .funcs
            .iter()
            .map(|f| f.to_wat())
            .collect::<Vec<_>>()
            .join("\n");

        format!(
            "(module

{}

)",
            funcs
        )
    }
}

impl ToWat for IRFunction {
    fn to_wat(&self) -> String {
        let params = self
            .params
            .iter()
            .map(|p| format!("(param ${} {})", p.name, p.ty.to_wat()))
            .collect::<Vec<_>>()
            .join(" ");

        let name = if self.is_exported {
            format!("(export \"{}\")", self.name)
        } else {
            format!("{}", self.name)
        };

        let return_ty = match &self.return_ty {
            Some(ty) => format!("(result {})", ty.to_wat()),
            None => String::new(),
        };

        let locals = self
            .locals
            .iter()
            .map(|l| format!("(local ${} {})", l.name, l.ty.to_wat()))
            .collect::<Vec<_>>()
            .join("\n");

        let body = self
            .body
            .iter()
            .map(|i| format!("  {}", i.to_wat()))
            .collect::<Vec<_>>()
            .join("\n");

        format!(
            "(func {name} {params} {return_ty}
{locals}
{body}
)",
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

            GetLocal(name) => format!("local.get ${}", name),
            SetLocal(name) => format!("local.set ${}", name),

            Drop => String::from("drop"),

            _ => todo!("Implement IRToWat for WatInstruction"),
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use crate::petal::{
//         ir::ToWatInstructions,
//         lexer::Lexer,
//         parser::Parser,
//         positions::{Pos, Span},
//         precedence::Precedence,
//     };

//     use super::*;
//     use WatInstruction::*;

//     fn to_wat(s: &str) -> String {
//         let mut lexer = Lexer::new(s);
//         let mut parser = Parser::new(&mut lexer);
//         let expr = parser.parse_expression(Precedence::Lowest).unwrap();
//         let instructions = expr.to_ir_chunk();
//         instructions
//             .iter()
//             .map(|i| i.to_wat())
//             .collect::<Vec<_>>()
//             .join(" ")
//     }

//     fn to_vec(instrs: Vec<&str>) -> String {
//         instrs
//             .iter()
//             .map(|s| s.to_string())
//             .collect::<Vec<_>>()
//             .join(" ")
//     }

//     #[test]
//     fn test_codegen_expr() {
//         assert_eq!(to_wat("1"), "f64.const 1".to_string());
//         assert_eq!(
//             to_wat("1 + 2"),
//             to_vec(vec!["f64.const 1", "f64.const 2", "f64.add"])
//         );
//         assert_eq!(
//             to_wat("1 + 2 * 3 - 4 / 5"),
//             to_vec(vec![
//                 "f64.const 1",
//                 "f64.const 2",
//                 "f64.const 3",
//                 "f64.mul",
//                 "f64.add",
//                 "f64.const 4",
//                 "f64.const 5",
//                 "f64.div",
//                 "f64.sub"
//             ])
//         );

//         assert_eq!(to_wat("a"), "(get_local $a)".to_string());
//     }
// }
