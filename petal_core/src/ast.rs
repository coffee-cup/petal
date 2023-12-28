use std::fmt::Display;

use slotmap::{new_key_type, SlotMap};

use crate::types::MonoType;

use super::{source_info::Span, token::Token};

new_key_type! {pub struct ExprId;}
pub type ExpressionPool = SlotMap<ExprId, ExprNode>;

new_key_type! {pub struct StmtId;}
pub type StatementPool = SlotMap<StmtId, StmtNode>;

new_key_type! {pub struct IdentId;}
pub type IdentifierPool = SlotMap<IdentId, Identifier>;

#[derive(Clone, Debug)]
pub struct AstPool {
    pub statements: StatementPool,
    pub expressions: ExpressionPool,
    pub identifiers: IdentifierPool,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub ast: AstPool,
    pub functions: Vec<FuncDecl>,
    pub structs: Vec<StructDecl>,
    pub main_stmts: Vec<StmtId>,
}

impl Program {
    pub fn new() -> Self {
        let ast = AstPool {
            statements: StatementPool::with_key(),
            expressions: ExpressionPool::with_key(),
            identifiers: IdentifierPool::with_key(),
        };

        Self {
            ast,
            functions: Vec::new(),
            structs: Vec::new(),
            main_stmts: Vec::new(),
        }
    }

    pub fn get_ident_name(&self, ident: IdentId) -> String {
        self.ast.identifiers[ident].name.clone()
    }

    pub fn span_for_ident(&self, ident: IdentId) -> Span {
        self.ast.identifiers[ident].span.clone()
    }

    pub fn add_function(&mut self, func: FuncDecl) {
        self.functions.push(func);
    }

    pub fn add_struct(&mut self, struct_decl: StructDecl) {
        self.structs.push(struct_decl);
    }

    pub fn new_statement(&mut self, stmt: Stmt, span: Span) -> StmtId {
        self.ast.statements.insert(StmtNode { stmt, span })
    }

    pub fn new_expression(&mut self, expr: Expr, span: Span) -> ExprId {
        self.ast.expressions.insert(ExprNode { expr, span })
    }

    pub fn new_identifier(&mut self, name: String, span: Span) -> IdentId {
        self.ast.identifiers.insert(Identifier {
            name,
            span,
            symbol_id: None,
        })
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
    pub symbol_id: Option<usize>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct TypeAnnotation {
    pub name: String,
    pub span: Span,
}

#[derive(PartialEq, Clone, Debug)]
pub struct StructDecl {
    pub ident: IdentId,
    pub fields: Vec<StructField>,
    pub span: Span,
}

/// struct Foo { ... }
#[derive(PartialEq, Clone, Debug)]
pub struct StructField {
    pub ident: IdentId,
    pub ty: TypeAnnotation,
    pub span: Span,
}

#[derive(PartialEq, Clone, Debug)]
pub struct FuncArg {
    pub ident: IdentId,
    pub ty: TypeAnnotation,
    pub span: Span,
}

/// fn foo(a, b) { ... }
#[derive(PartialEq, Clone, Debug)]
pub struct FuncDecl {
    pub ident: IdentId,
    pub is_exported: bool,
    pub type_params: Vec<TypeAnnotation>,
    pub args: Vec<FuncArg>,
    pub return_ty: Option<TypeAnnotation>,
    pub body: StmtId,
    pub span: Span,
}

#[derive(PartialEq, Clone, Debug)]
pub struct LetDecl {
    pub ident: IdentId,
    pub ty: Option<TypeAnnotation>,
    pub init: ExprId,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Block {
    pub statements: Vec<StmtId>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ExprNode {
    pub expr: Expr,
    pub span: Span,
}

#[derive(PartialEq, Clone, Debug)]
pub struct StmtNode {
    pub stmt: Stmt,
    pub span: Span,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    // let a = 1
    Let(LetDecl),

    // if a { ... } else { ... }
    IfStmt {
        condition: ExprId,
        then_block: StmtId,
        else_block: Option<StmtId>,
    },

    // return 1
    Return(Option<ExprId>),

    BlockStmt(Block),

    ExprStmt(ExprId),

    // # this is a comment
    Comment(String),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    // 1
    Integer(i64),

    // 1.1
    Float(f64),

    // "hello"
    String(String),

    // true
    Bool(bool),

    // foo
    Ident(IdentId),

    // -1
    PrefixOp {
        op: PrefixOp,
        right: ExprId,
    },

    // 1 + 2
    BinaryOp {
        op: BinaryOp,
        left: ExprId,
        right: ExprId,
    },

    // 1!
    PostfixOp {
        op: Token,
        left: ExprId,
    },

    // foo(1, 2, 3)
    Call {
        callee: ExprId,
        args: Vec<ExprId>,
    },
}

#[derive(PartialEq, Clone, Debug)]
pub enum PrefixOpType {
    Neg,
    Not,
}

#[derive(PartialEq, Clone, Debug)]
pub struct PrefixOp {
    pub prefix_type: PrefixOpType,
    pub span: Span,
}

#[derive(PartialEq, Clone, Debug)]
pub enum BinaryOpType {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equality,
    Inequality,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(PartialEq, Clone, Debug)]
pub struct BinaryOp {
    pub binary_type: BinaryOpType,
    pub span: Span,
}

impl BinaryOpType {
    pub fn supported_operand_types(&self) -> Vec<MonoType> {
        match self {
            BinaryOpType::Add
            | BinaryOpType::Subtract
            | BinaryOpType::Multiply
            | BinaryOpType::Divide => vec![MonoType::int(), MonoType::float()],
            BinaryOpType::Equality
            | BinaryOpType::Inequality
            | BinaryOpType::LessThan
            | BinaryOpType::LessThanOrEqual
            | BinaryOpType::GreaterThan
            | BinaryOpType::GreaterThanOrEqual => {
                vec![MonoType::int(), MonoType::float(), MonoType::bool()]
            }
        }
    }
}

impl Display for BinaryOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOpType::Add => write!(f, "+"),
            BinaryOpType::Subtract => write!(f, "-"),
            BinaryOpType::Multiply => write!(f, "*"),
            BinaryOpType::Divide => write!(f, "/"),
            BinaryOpType::Equality => write!(f, "=="),
            BinaryOpType::Inequality => write!(f, "!="),
            BinaryOpType::LessThan => write!(f, "<"),
            BinaryOpType::LessThanOrEqual => write!(f, "<="),
            BinaryOpType::GreaterThan => write!(f, ">"),
            BinaryOpType::GreaterThanOrEqual => write!(f, ">="),
        }
    }
}

// impl HasSpan for Stmt {
//     fn span(&self) -> Span {
//         match self {
//             Stmt::Struct(decl) => decl.span.clone(),
//             Stmt::Func(decl) => decl.span.clone(),
//             Stmt::Let(decl) => decl.span.clone(),
//             Stmt::IfStmt { span, .. } => span.clone(),
//             Stmt::ExprStmt { span, .. } => span.clone(),
//         }
//     }
// }

// impl HasSpan for Expr {
//     fn span(&self) -> Span {
//         match self {
//             Expr::Integer { span, .. } => span.clone(),
//             Expr::Float { span, .. } => span.clone(),
//             Expr::PrefixOp { span, .. } => span.clone(),
//             Expr::BinaryOp { span, .. } => span.clone(),
//             Expr::PostfixOp { span, .. } => span.clone(),
//             Expr::Conditional { span, .. } => span.clone(),
//             Expr::Ident(id) => id.span(),
//             Expr::String { span, .. } => span.clone(),
//             Expr::Comment { span, .. } => span.clone(),
//             Expr::Call { span, .. } => span.clone(),
//         }
//     }
// }

// impl HasSpan for Block {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl HasSpan for TypeAnnotation {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl HasSpan for FuncDecl {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl HasSpan for Identifier {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }
