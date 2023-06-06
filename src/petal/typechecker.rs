use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
    unreachable,
};

use thiserror::Error;

use super::{
    ast::{Expr, FuncDecl, Program, Stmt, TypeAnnotation},
    positions::Span,
    types::*,
};

#[derive(Error, Clone, Debug)]
pub enum TypecheckingErrorKind {
    #[error("Unknown error")]
    Unknown,
}

#[derive(Debug, Clone)]
pub struct TypecheckingError {
    pub kind: TypecheckingErrorKind,
    pub span: Option<Span>,
}

type TypecheckingResult<T> = Result<T, TypecheckingError>;

#[derive(Clone, Debug)]
struct TypeVarGen {
    counter: usize,
}

impl TypeVarGen {
    fn new() -> Self {
        Self { counter: 0 }
    }

    fn next(&mut self) -> TyVar {
        let var = format!("t{}", self.counter);
        self.counter += 1;
        var
    }
}

/// Tracks mapping from type variables to types
#[derive(PartialEq, Clone, Debug)]
pub struct Substitution {
    map: HashMap<TyVar, MonoType>,
}

impl Substitution {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    /// Get a type from the substitution
    pub fn get(&self, var: &TyVar) -> Option<MonoType> {
        self.map.get(var).cloned()
    }

    /// Insert a new type variable and its type into the substitution
    pub fn insert(&mut self, var: TyVar, ty: MonoType) {
        self.map.insert(var, ty);
    }

    /// Remove a type variable from the substitution
    pub fn remove(&mut self, var: &TyVar) {
        self.map.remove(var);
    }

    /// Combine two substitutions into one (order matters!)
    /// If the same type variable is present in both substitutions, the one in `self` will be used
    /// s2.combine(s1) = s1(s2)
    pub fn combine(&self, other: Self) -> Self {
        let mut res = self.clone();
        for (key, value) in other.map {
            res.insert(key.clone(), value.apply(self))
        }

        res
    }
}

trait Types {
    /// Returns a list of all free type variables in the type
    fn free_variables(&self) -> BTreeSet<TyVar>;

    /// Applies the substitution to the type and returns a new type
    fn apply(&self, sub: &Substitution) -> Self;
}

impl Types for MonoType {
    fn free_variables(&self) -> BTreeSet<TyVar> {
        use MonoType::*;

        match self {
            Variable(var) => [var.clone()].into(),
            Struct(StructType { params, .. }) => params
                .iter()
                .flat_map(|param| param.free_variables())
                .collect(),
            FunApp(FunctionAppType { params, return_ty }) => {
                let mut vars = return_ty.free_variables();
                for param in params {
                    vars.extend(param.free_variables());
                }
                vars
            }
        }
    }

    fn apply(&self, sub: &Substitution) -> Self {
        todo!()
    }
}

impl Types for PolyType {
    fn free_variables(&self) -> BTreeSet<TyVar> {
        match self {
            PolyType::Mono(ty) => ty.free_variables(),
            PolyType::Quantifier(TypeQuantifier { quantifiers, ty }) => {
                let mut vars = ty.free_variables();
                for q in quantifiers {
                    vars.remove(q);
                }
                vars
            }
        }
    }

    fn apply(&self, sub: &Substitution) -> Self {
        todo!()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct TypeContext {
    /// Maps type names to their definitions
    types: BTreeMap<TyVar, PolyType>,
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            types: BTreeMap::new(),
        }
    }

    pub fn insert(&mut self, name: TyVar, ty: PolyType) {
        self.types.insert(name, ty);
    }

    pub fn insert_mono(&mut self, name: TyVar, ty: MonoType) {
        self.types.insert(name, PolyType::Mono(ty));
    }

    pub fn get(&self, name: &TyVar) -> Option<PolyType> {
        self.types.get(name).cloned()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Constraint {
    lhs: MonoType,
    rhs: MonoType,
}

impl Constraint {
    pub fn new(lhs: MonoType, rhs: MonoType) -> Self {
        Self { lhs, rhs }
    }
}

pub struct Typechecker {
    ty_gen: TypeVarGen,
    constraints: Vec<Constraint>,
}

impl Typechecker {
    pub fn new() -> Self {
        Self {
            ty_gen: TypeVarGen::new(),
            constraints: Vec::new(),
        }
    }

    pub fn associate_types(&mut self, lhs: MonoType, rhs: MonoType) {
        self.constraints.push(Constraint::new(lhs, rhs))
    }

    pub fn gen_type_var(&mut self) -> MonoType {
        let t = self.ty_gen.next();
        MonoType::Variable(t)
    }
}

impl Display for TypeContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (name, ty) in &self.types {
            writeln!(f, "{}: {}", name, ty)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use MonoType::*;
    use PolyType::*;

    #[test]
    fn test_ty_var_gen() {
        let mut gen = TypeVarGen::new();
        assert_eq!(gen.next(), "t0");
        assert_eq!(gen.next(), "t1");
        assert_eq!(gen.next(), "t2");
    }

    #[test]
    fn test_free_variables() {
        // Monotypes
        assert_eq!(MonoType::int().free_variables(), BTreeSet::new());
        assert_eq!(MonoType::bool().free_variables(), BTreeSet::new());
        assert_eq!(
            FunApp(FunctionAppType {
                params: vec![MonoType::int(), Variable("a".into())],
                return_ty: Box::new(Variable("b".into()))
            })
            .free_variables(),
            ["a".into(), "b".into()].into()
        );

        // Polytypes
        assert_eq!(
            Quantifier(TypeQuantifier {
                quantifiers: vec!["a".into()],
                ty: Variable("a".into())
            })
            .free_variables(),
            BTreeSet::new()
        );
        assert_eq!(
            Quantifier(TypeQuantifier {
                quantifiers: vec!["a".into()],
                ty: Variable("b".into())
            })
            .free_variables(),
            ["b".into()].into()
        );
    }

    #[test]
    fn test_substitution_combine() {
        use MonoType::*;

        let mut sub1 = Substitution::new();
        sub1.insert("x".to_string(), Variable("y".to_string()));
        sub1.insert("a".to_string(), MonoType::int());

        let mut sub2 = Substitution::new();
        sub2.insert(
            "z".into(),
            FunApp(FunctionAppType {
                params: vec![MonoType::bool()],
                return_ty: Box::new(Variable("x".into())),
            }),
        );
        sub2.insert("a".to_string(), MonoType::bool());

        let mut sub3 = Substitution::new();
        sub3.insert("x".to_string(), Variable("y".to_string()));
        sub3.insert(
            "z".into(),
            FunApp(FunctionAppType {
                params: vec![MonoType::bool()],
                return_ty: Box::new(Variable("y".into())),
            }),
        );
        sub3.insert("a".to_string(), MonoType::bool());

        assert_eq!(sub1.combine(sub2), sub3);
    }
}

// ---

// Statements have no type, only expressions do
// However, walking the program and statements is necessary to build the type environment
// We can generate constraints using this type environment by walking all expressions
//
// We will also likely need a TypedExpr type, which is the same as Expr but with a type annotation
// Not sure how to represent this in the top-level AST. Maybe we can just have a TypedProgram type
//
// We could also convert to an HIR at the same time as typechecking
