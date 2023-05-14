use super::{
    positions::{HasSpan, Span},
    token::Token,
};

#[derive(PartialEq, Clone, Debug)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Decl {
    // fn foo(a, b) { ... }
    // Fn {
    //     name: String,
    //     args: Vec<String>,
    //     body: Vec<Stmt>,
    //     span: Span,
    // },

    // struct Foo { ... }
    // Struct {
    //     name: String,
    //     fields: Vec<Decl>,
    //     span: Span,
    // },
}

#[derive(PartialEq, Clone, Debug)]
pub struct FuncDecl {
    pub name: String,
    pub args: Vec<String>,
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Func(FuncDecl),

    // let a = 1
    Let {
        name: String,
        init: Expr,
        span: Span,
    },

    IfStmt {
        condition: Expr,
        then_block: Box<Stmt>,
        else_block: Option<Box<Stmt>>,
        span: Span,
    },

    ExprStmt {
        expr: Box<Expr>,
        span: Span,
    },
}

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

    // foo(1, 2, 3)
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
}

impl HasSpan for Stmt {
    fn span(&self) -> Span {
        match self {
            Stmt::Func(decl) => decl.span.clone(),
            Stmt::Let { span, .. } => span.clone(),
            Stmt::IfStmt { span, .. } => span.clone(),
            Stmt::ExprStmt { span, .. } => span.clone(),
        }
    }
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
            Expr::Call { span, .. } => span.clone(),
        }
    }
}
