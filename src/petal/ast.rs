use super::{
    positions::{HasSpan, Span},
    token::Token,
};

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Number {
        value: f64,
        span: Span,
    },
    PrefixOp {
        op: Token,
        right: Box<Expr>,
        span: Span,
    },
    BinaryOp {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
        span: Span,
    },
}

impl HasSpan for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Number { span, .. } => span.clone(),
            Expr::PrefixOp { span, .. } => span.clone(),
            Expr::BinaryOp { span, .. } => span.clone(),
        }
    }
}
