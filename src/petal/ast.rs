use super::{
    positions::{HasSpan, Span},
    token::Token,
};

#[derive(PartialEq, Clone, Debug)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct TypeAnnotation {
    pub name: String,
    pub span: Span,
}

#[derive(PartialEq, Clone, Debug)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<StructField>,
    pub span: Span,
}

#[derive(PartialEq, Clone, Debug)]
pub struct StructField {
    pub name: String,
    pub ty: TypeAnnotation,
    pub span: Span,
}

#[derive(PartialEq, Clone, Debug)]
pub struct FuncArg {
    pub name: String,
    pub span: Span,
    pub ty: TypeAnnotation,
}

#[derive(PartialEq, Clone, Debug)]
pub struct FuncDecl {
    pub name: String,
    pub is_exported: bool,
    pub type_params: Vec<TypeAnnotation>,
    pub args: Vec<FuncArg>,
    pub return_ty: Option<TypeAnnotation>,
    pub body: Block,
    pub span: Span,
}

#[derive(PartialEq, Clone, Debug)]
pub struct LetDecl {
    pub name: String,
    pub ty: Option<TypeAnnotation>,
    pub init: Expr,
    pub span: Span,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub span: Span,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    // struct Foo { ... }
    Struct(StructDecl),

    // fn foo(a, b) { ... }
    Func(FuncDecl),

    // let a = 1
    Let(LetDecl),

    // if a { ... } else { ... }
    IfStmt {
        condition: Expr,
        then_block: Block,
        else_block: Option<Block>,
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
    Integer {
        value: i64,
        span: Span,
    },

    // 1.1
    Float {
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

    // foo(1, 2, 3)
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },

    // # this is a comment
    Comment {
        value: String,
        span: Span,
    },
}

impl HasSpan for Stmt {
    fn span(&self) -> Span {
        match self {
            Stmt::Struct(decl) => decl.span.clone(),
            Stmt::Func(decl) => decl.span.clone(),
            Stmt::Let(decl) => decl.span.clone(),
            Stmt::IfStmt { span, .. } => span.clone(),
            Stmt::ExprStmt { span, .. } => span.clone(),
        }
    }
}

impl HasSpan for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Integer { span, .. } => span.clone(),
            Expr::Float { span, .. } => span.clone(),
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

impl HasSpan for Block {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl HasSpan for TypeAnnotation {
    fn span(&self) -> Span {
        self.span.clone()
    }
}
