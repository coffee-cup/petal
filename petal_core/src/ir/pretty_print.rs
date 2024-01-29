use std::fmt::Display;

use super::*;

pub struct IRPrettyPrinter {
    indent: usize,
    output: String,
}

impl IRPrettyPrinter {
    pub fn new() -> Self {
        Self {
            indent: 0,
            output: "".into(),
        }
    }

    pub fn print_program(&mut self, program: &IRProgram) -> String {
        self.output = String::new();

        program.imports.iter().for_each(|import| {
            self.print_import(import);
        });

        self.output.push('\n');

        program.functions.iter().for_each(|func| {
            self.print_function(func);
        });

        self.output.clone()
    }

    pub fn print_import(&mut self, import: &IRImport) {
        self.output(format!("import {}", import.signature));
    }

    pub fn print_function(&mut self, func: &IRFunction) {
        if func.is_exported {
            self.output.push_str("export ");
        }
        self.output
            .push_str(format!("fn {} {{\n", func.signature).as_str());

        self.indent += 1;

        for local in func.locals.iter() {
            self.output(local.to_string());
        }
        if !func.locals.is_empty() {
            self.output.push('\n');
        }

        self.print_statement(&func.body);

        self.indent -= 1;

        self.output.push_str("}\n\n");
    }

    fn print_statement(&mut self, stmt: &IRStatement) {
        match stmt {
            IRStatement::Let { name, ty, init } => {
                self.output(format!("let {}: {} = {};", name, ty, init));
            }

            IRStatement::Block { statements } => {
                for block_stmt in statements.iter() {
                    self.print_statement(block_stmt);
                }
            }

            IRStatement::If {
                condition,
                then_block,
                else_block,
            } => {
                self.output(format!("if {} {{", condition));

                self.indent += 1;
                self.print_statement(then_block);
                self.indent -= 1;

                if let Some(else_block) = else_block {
                    self.output("} else {".to_string());

                    self.indent += 1;
                    self.print_statement(else_block);
                    self.indent -= 1;

                    self.output("}\n".to_string());
                } else {
                    self.output("}\n".to_string());
                }
            }

            IRStatement::While {
                condition,
                body,
                uid,
            } => {
                self.output(format!("while#{uid} {condition} {{"));

                self.indent += 1;
                self.print_statement(body);
                self.indent -= 1;

                self.output("}\n".to_string());
            }

            IRStatement::Return { expr } => {
                if let Some(expr) = expr {
                    self.output(format!("return {}", expr))
                } else {
                    self.output("return".to_string())
                }
            }

            IRStatement::Expr(e) => {
                self.output(e.to_string());
            }
        };
    }

    fn output(&mut self, s: String) {
        self.output_indent();
        self.output.push_str(&s);
        self.output.push('\n');
    }

    fn output_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("  ");
        }
    }
}

impl Display for IRLocal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "local {}: {};", self.name, self.ty)
    }
}

impl Display for IRParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl Display for IRFunctionSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params_str = self
            .params
            .iter()
            .map(|param| format!("{}", param))
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "{}({}): {}", self.name, params_str, self.return_type)
    }
}

impl Display for IRExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IRExpression::IntLiteral(n) => write!(f, "{}", n),
            IRExpression::FloatLiteral(n) => write!(f, "{}", n),
            IRExpression::StringLiteral(s) => write!(f, "\"{}\"", s),
            IRExpression::BoolLiteral(b) => write!(f, "{}", b),
            IRExpression::PrefixOp {
                op,
                right: expr,
                ty,
            } => write!(f, "({}{}):{}", op, expr, ty),
            IRExpression::BinOp {
                op,
                left: lhs,
                right: rhs,
                ty,
            } => write!(f, "({} {} {}):{}", lhs, op, rhs, ty),
            IRExpression::Ident { name, .. } => write!(f, "{}", name),
            IRExpression::Call { name, args, ty: _ } => {
                let args_str = args
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{}({})", name, args_str)
            }
            IRExpression::Assign { name, expr, .. } => {
                write!(f, "{} = {}", name, expr)
            }
        }
    }
}

impl Display for IRUnOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IRUnOpType::Neg => write!(f, "-"),
            IRUnOpType::Not => write!(f, "!"),
        }
    }
}

impl Display for IRBinOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IRBinOpType::Add => write!(f, "+"),
            IRBinOpType::Sub => write!(f, "-"),
            IRBinOpType::Mul => write!(f, "*"),
            IRBinOpType::Div => write!(f, "/"),
            IRBinOpType::Mod => write!(f, "%"),
            IRBinOpType::Eq => write!(f, "=="),
            IRBinOpType::Neq => write!(f, "!="),
            IRBinOpType::Lt => write!(f, "<"),
            IRBinOpType::Gt => write!(f, ">"),
            IRBinOpType::Leq => write!(f, "<="),
            IRBinOpType::Geq => write!(f, ">="),
            IRBinOpType::And => write!(f, "&&"),
            IRBinOpType::Or => write!(f, "||"),
        }
    }
}
