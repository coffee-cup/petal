use wast::kw::r#else;

use crate::petal::wat::WatValue;

use super::wat::{WatFunction, WatInstruction, WatModule, WatValueType};

pub trait ToWat {
    fn to_wat(&self) -> String;
}

impl ToWat for WatModule {
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

impl ToWat for WatFunction {
    fn to_wat(&self) -> String {
        let signature = &self.signature;
        let params = signature
            .params
            .iter()
            .map(|p| format!("(param ${} {})", p.name, p.ty.to_wat()))
            .collect::<Vec<_>>()
            .join(" ");

        let name = format!("${}", self.name);

        let export = if signature.is_exported {
            format!("(export \"{}\")", self.name)
        } else {
            String::new()
        };

        let return_ty = match &signature.return_ty {
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
            "(func {name} {export} {params} {return_ty}
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

            Min(wat_type) => format!("{}.min", wat_type.to_wat(),),
            Max(wat_type) => format!("{}.max", wat_type.to_wat(),),
            Ceil(wat_type) => format!("{}.ceil", wat_type.to_wat(),),
            Floor(wat_type) => format!("{}.floor", wat_type.to_wat(),),
            Nearest(wat_type) => format!("{}.nearest", wat_type.to_wat(),),

            Extend => format!("i64.extend_i32_s"),
            Wrap => format!("i32.wrap_i64"),
            Truncate(from_type, to_type) => {
                format!("{}.trunc_{}_s", to_type.to_wat(), from_type.to_wat(),)
            }

            GetLocal(name) => format!("local.get ${}", name),
            SetLocal(name) => format!("local.set ${}", name),

            Call(name, _) => format!("call ${}", name),

            If(then_block, else_block) => format!(
                "(if 
                (then
                    {}
                )
                (else
                    {}
                )
            )",
                then_block
                    .iter()
                    .map(|instr| instr.to_wat())
                    .collect::<Vec<_>>()
                    .join("\n"),
                else_block
                    .iter()
                    .map(|instr| instr.to_wat())
                    .collect::<Vec<_>>()
                    .join("\n"),
            ),

            Drop => String::from("drop"),

            _ => todo!("Implement IRToWat for {:?}", self),
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
