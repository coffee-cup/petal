use super::{
    positions::{HasSpan, Span},
    token::Token,
};

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    // 1
    Number {
        value: f64,
        span: Span,
    },

    // "hello"
    String {
        value: String,
        span: Span,
    },

    // foo
    Ident {
        name: String,
        span: Span,
    },

    // -1
    PrefixOp {
        op: Token,
        right: Box<Expr>,
        span: Span,
    },

    // 1 + 2
    BinaryOp {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
        span: Span,
    },

    // 1!
    PostfixOp {
        op: Token,
        left: Box<Expr>,
        span: Span,
    },

    // a ? b : c
    Conditional {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
        span: Span,
    },

    // # comment
    Comment {
        value: String,
        span: Span,
    },
}

impl HasSpan for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Number { span, .. } => span.clone(),
            Expr::PrefixOp { span, .. } => span.clone(),
            Expr::BinaryOp { span, .. } => span.clone(),
            Expr::PostfixOp { span, .. } => span.clone(),
            Expr::Conditional { span, .. } => span.clone(),
            Expr::Ident { span, .. } => span.clone(),
            Expr::String { span, .. } => span.clone(),
            Expr::Comment { span, .. } => span.clone(),
        }
    }
}
