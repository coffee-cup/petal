use std::fmt::Display;

use super::{
    ast::{FuncDecl, TypeAnnotation},
    positions::Span,
};

pub type TyVar = String;

/// Monomorphic type
#[derive(PartialEq, Clone, Debug)]
pub enum MonoType {
    /// Type variable that can be substituted with any type
    Variable(TyVar),

    Struct(StructType),

    // /// Integer (`1`, `2`, `3`, ...)
    // Int,

    // /// Float (`1.0`, `2.1`, `3.14`, ...)
    // Float,

    // /// Boolean (`true`, `false`)
    // Bool,

    // /// String (`"hello"`, `"world"`, ...)
    // String,
    /// Function type (`(Int, String) -> Bool`)
    FunApp(FunctionAppType),
}

/// Function
///
/// e.g. (Int, String) -> Bool
#[derive(PartialEq, Clone, Debug)]
pub struct FunctionAppType {
    pub params: Vec<MonoType>,
    pub return_ty: Box<MonoType>,
}

/// Structs and basic types
///
/// e.g. `Int`, `String`, `Bool`, `Point`, `Vec`, ...
#[derive(PartialEq, Clone, Debug)]
pub struct StructType {
    pub name: String,
    pub params: Vec<MonoType>,
}

/// Polymorphic type
#[derive(PartialEq, Clone, Debug)]
pub enum PolyType {
    Mono(MonoType),
    Quantifier(TypeQuantifier),
}

/// Type quantifier
///
/// e.g. forall a, b. (a, b) -> (b, a)
#[derive(PartialEq, Clone, Debug)]
pub struct TypeQuantifier {
    /// The type variables that are quantified in the polytype
    ///
    /// e.g. `a` and `b` in `forall a, b. (a, b) -> (b, a)`
    pub quantifiers: Vec<TyVar>,

    /// The type that is quantified over
    ///
    /// e.g. `(a, b) -> (b, a)` in `forall a, b. (a, b) -> (b, a)`
    pub ty: MonoType,
}

impl MonoType {
    pub fn unit() -> MonoType {
        MonoType::Struct(StructType {
            name: "Unit".into(),
            params: Vec::new(),
        })
    }

    pub fn int() -> MonoType {
        MonoType::Struct(StructType {
            name: "Int".into(),
            params: Vec::new(),
        })
    }

    pub fn float() -> MonoType {
        MonoType::Struct(StructType {
            name: "Float".into(),
            params: Vec::new(),
        })
    }

    pub fn bool() -> MonoType {
        MonoType::Struct(StructType {
            name: "Bool".into(),
            params: Vec::new(),
        })
    }

    pub fn string() -> MonoType {
        MonoType::Struct(StructType {
            name: "String".into(),
            params: Vec::new(),
        })
    }
}

impl Display for MonoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MonoType::Variable(name) => write!(f, "{}", name),
            // MonoType::Int => write!(f, "Int"),
            // MonoType::Float => write!(f, "Float"),
            // MonoType::Bool => write!(f, "Bool"),
            // MonoType::String => write!(f, "String"),
            MonoType::Struct(struct_ty) => {
                write!(
                    f,
                    "{}{}",
                    struct_ty.name,
                    if struct_ty.params.is_empty() {
                        "".to_string()
                    } else {
                        format!(
                            "<{}>",
                            struct_ty
                                .params
                                .iter()
                                .map(|ty| format!("{}", ty))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    }
                )
            }
            MonoType::FunApp(function_app) => {
                write!(
                    f,
                    "(({}) -> {})",
                    function_app
                        .params
                        .iter()
                        .map(|ty| format!("{}", ty))
                        .collect::<Vec<_>>()
                        .join(", "),
                    function_app.return_ty
                )
            }
        }
    }
}

impl Display for PolyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PolyType::Mono(ty) => write!(f, "{}", ty),
            PolyType::Quantifier(quantifier) => {
                write!(
                    f,
                    "forall {}. {}",
                    quantifier
                        .quantifiers
                        .iter()
                        .map(|ty| format!("t{}", ty))
                        .collect::<Vec<_>>()
                        .join(", "),
                    quantifier.ty
                )
            }
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Type {
    Void,

    TyVar(usize),

    // Basic type
    // Int
    Simple(String),

    // Function type
    // A -> B
    Function {
        params: Vec<Type>,
        return_ty: Box<Type>,
    },
}
