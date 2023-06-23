use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
};

use thiserror::Error;

use super::{
    ast::{ExprId, IdentId, StmtId},
    types::*,
};

#[derive(Clone, Debug)]
pub struct TypeVarGen {
    counter: usize,
}

impl TypeVarGen {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    pub fn gen_var(&mut self) -> MonoType {
        MonoType::Variable(self.next())
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

pub trait Types {
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
        use MonoType::*;

        match self {
            Variable(var) => sub.get(var).unwrap_or_else(|| self.clone()),
            Struct(decl) => {
                let params = decl.params.iter().map(|param| param.apply(sub)).collect();
                Struct(StructType {
                    params,
                    ..decl.clone()
                })
            }
            FunApp(decl) => {
                let params = decl.params.iter().map(|param| param.apply(sub)).collect();
                let return_ty = decl.return_ty.apply(sub);

                FunApp(FunctionAppType {
                    params,
                    return_ty: Box::new(return_ty),
                })
            }
        }
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
        match self {
            PolyType::Mono(ty) => PolyType::Mono(ty.apply(sub)),
            PolyType::Quantifier(forall) => {
                let mut sub = sub.clone();
                for q in &forall.quantifiers {
                    sub.remove(q);
                }
                PolyType::Quantifier(TypeQuantifier {
                    quantifiers: forall.quantifiers.clone(),
                    ty: forall.ty.apply(&sub),
                })
            }
        }
    }
}

impl Types for TypeContext {
    fn free_variables(&self) -> BTreeSet<TyVar> {
        self.types
            .values()
            .flat_map(|ty| ty.free_variables())
            .collect()
    }

    fn apply(&self, sub: &Substitution) -> Self {
        let mut types = BTreeMap::new();
        for (key, value) in self.types.iter() {
            types.insert(key.clone(), value.apply(sub));
        }

        Self { types }
    }
}

impl MonoType {
    // Quantify all free variables in the type
    pub fn generalise(&self, ctx: &mut TypeContext) -> PolyType {
        let quantifiers = self
            .free_variables()
            .difference(&ctx.free_variables())
            .cloned()
            .collect::<Vec<_>>();

        if quantifiers.is_empty() {
            return PolyType::Mono(self.clone());
        }

        PolyType::Quantifier(TypeQuantifier {
            quantifiers,
            ty: self.clone(),
        })
    }
}

impl PolyType {
    // Replace all the forall quantifiers with type variables
    pub fn instantiate(&self, ty_gen: &mut TypeVarGen) -> MonoType {
        match self {
            PolyType::Mono(ty) => ty.clone(),
            PolyType::Quantifier(forall) => {
                let mut sub = Substitution::new();
                for q in &forall.quantifiers {
                    sub.insert(q.clone(), MonoType::Variable(ty_gen.next()));
                }
                forall.ty.apply(&sub)
            }
        }
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

#[derive(Clone, Debug)]
pub struct MonoTypeData {
    pub ty: MonoType,
    pub expr_id: Option<ExprId>,
    pub ident_id: Option<IdentId>,
    pub parent_stmt_id: Option<StmtId>,
}

impl MonoTypeData {
    pub fn new(ty: MonoType) -> Self {
        Self {
            ty,
            expr_id: None,
            ident_id: None,
            parent_stmt_id: None,
        }
    }

    pub fn with_expr(mut self, expr_id: ExprId) -> Self {
        self.expr_id = Some(expr_id);
        self
    }

    pub fn with_ident(mut self, ident_id: IdentId) -> Self {
        self.ident_id = Some(ident_id);
        self
    }

    pub fn with_parent_stmt(mut self, stmt_id: StmtId) -> Self {
        self.parent_stmt_id = Some(stmt_id);
        self
    }
}

impl Types for MonoTypeData {
    fn free_variables(&self) -> BTreeSet<TyVar> {
        self.ty.free_variables()
    }

    fn apply(&self, sub: &Substitution) -> Self {
        Self {
            ty: self.ty.apply(sub),
            ..self.clone()
        }
    }
}

impl From<MonoType> for MonoTypeData {
    fn from(ty: MonoType) -> Self {
        Self::new(ty)
    }
}

#[derive(Clone, Debug)]
pub enum Constraint {
    /// lhs = rhs
    Equal {
        lhs: MonoTypeData,
        rhs: MonoTypeData,
    },
}

impl Constraint {
    pub fn equal(lhs: MonoTypeData, rhs: MonoTypeData) -> Self {
        Self::Equal { lhs, rhs }
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

    // pub fn solve_constraints(&self) -> TypeResult<Substitution> {
    //     let mut sub = Substitution::new();

    //     for constraint in &self.constraints {
    //         match constraint {
    //             Constraint::Equal { lhs, rhs, origin } => {
    //                 println!("Unifying {} and {}", lhs.apply(&sub), rhs.apply(&sub));

    //                 let sub2 =
    //                     self.unify_equality_constraint(lhs.apply(&sub), rhs.apply(&sub), origin)?;
    //                 println!("Substitution: {:?}", sub2);
    //                 sub = sub.combine(sub2);
    //             }
    //             Constraint::Oneof(_lhs, _tys) => todo!(),
    //         }
    //     }

    //     Ok(sub)
    // }

    // pub fn unify_equality_constraint(
    //     &self,
    //     lhs: MonoType,
    //     rhs: MonoType,
    // ) -> TypeResult<Substitution> {
    //     use MonoType::*;

    //     let sub = match (&lhs, &rhs) {
    //         (Variable(v1), Variable(v2)) => {
    //             if v1 == v2 {
    //                 Substitution::new()
    //             } else {
    //                 let mut sub = Substitution::new();
    //                 sub.insert(v1.clone(), Variable(v2.clone()));
    //                 sub
    //             }
    //         }
    //         (Variable(v), ty) | (ty, Variable(v)) => {
    //             if occurs_check(ty, &Variable(v.clone())) {
    //                 panic!("Infinite type");
    //             } else {
    //                 let mut sub = Substitution::new();
    //                 sub.insert(v.clone(), ty.clone());
    //                 sub
    //             }
    //         }
    //         (FunApp(f1), FunApp(f2)) => {
    //             if f1.params.len() != f2.params.len() {
    //                 panic!(
    //                     "Functions have different number of arguments: {} and {}",
    //                     f1.params.len(),
    //                     f2.params.len()
    //                 );
    //             }

    //             let mut sub = Substitution::new();
    //             for (a, b) in f1.params.iter().zip(f2.params.iter()) {
    //                 sub =
    //                     sub.combine(self.unify_equality_constraint(a.apply(&sub), b.apply(&sub))?);
    //             }

    //             sub = sub.combine(self.unify_equality_constraint(
    //                 f1.return_ty.apply(&sub),
    //                 f2.return_ty.apply(&sub),
    //             )?);
    //             sub
    //         }
    //         (Struct(t1), Struct(t2)) => {
    //             if t1.name != t2.name {
    //                 println!("{:?}", self.type_spans);
    //                 return Err(TypecheckingError::new(
    //                     TypecheckingErrorKind::MismatchedTypes(lhs, rhs),
    //                 ));
    //             }

    //             let mut sub = Substitution::new();
    //             for (a, b) in t1.params.iter().zip(t2.params.iter()) {
    //                 sub =
    //                     sub.combine(self.unify_equality_constraint(a.apply(&sub), b.apply(&sub))?);
    //             }

    //             sub
    //         }
    //         (v1, v2) => {
    //             if v1 != v2 {
    //                 panic!("Types {:?} and {:?} do not unify", v1, v2)
    //             }
    //             Substitution::new()
    //         }
    //     };

    //     Ok(sub)
    // }

    // pub fn associate_types(&mut self, lhs: MonoType, rhs: MonoType) {
    //     self.constraints.push(Constraint::equal(lhs, rhs));
    // }

    pub fn instantiate(&mut self, poly: PolyType) -> MonoType {
        poly.instantiate(&mut self.ty_gen)
    }

    pub fn gen_type_var(&mut self) -> MonoType {
        let t = self.ty_gen.next();

        MonoType::Variable(t)
    }

    pub fn print_constraints(&self) {
        for constraint in &self.constraints {
            println!("{}", constraint);
        }
    }
}

/// Returns true if the right type variable occurs in the left type
/// or if the left and right types are equal
pub fn occurs_check(left: &MonoType, right: &MonoType) -> bool {
    use MonoType::*;

    match left {
        FunApp(FunctionAppType { params, return_ty }) => {
            params.iter().any(|param| occurs_check(param, right)) || occurs_check(return_ty, right)
        }
        Struct(StructType { params, .. }) => params.iter().any(|param| occurs_check(param, right)),
        _ => left == right,
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

impl Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constraint::Equal { lhs, rhs, .. } => write!(f, "{} ~ {}", lhs.ty, rhs.ty),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use MonoType::*;
    use PolyType::*;

    // #[test]
    // fn test_ty_var_gen() {
    //     let mut gen = TypeVarGen::new();
    //     assert_eq!(gen.next(), "t0");
    //     assert_eq!(gen.next(), "t1");
    //     assert_eq!(gen.next(), "t2");
    // }

    // #[test]
    // fn test_free_variables() {
    //     // Monotypes
    //     assert_eq!(MonoType::int().free_variables(), BTreeSet::new());
    //     assert_eq!(MonoType::bool().free_variables(), BTreeSet::new());
    //     assert_eq!(
    //         FunApp(FunctionAppType {
    //             params: vec![MonoType::int(), Variable("a".into())],
    //             return_ty: Box::new(Variable("b".into()))
    //         })
    //         .free_variables(),
    //         ["a".into(), "b".into()].into()
    //     );

    //     // Polytypes
    //     assert_eq!(
    //         Quantifier(TypeQuantifier {
    //             quantifiers: vec!["a".into()],
    //             ty: Variable("a".into())
    //         })
    //         .free_variables(),
    //         BTreeSet::new()
    //     );
    //     assert_eq!(
    //         Quantifier(TypeQuantifier {
    //             quantifiers: vec!["a".into()],
    //             ty: Variable("b".into())
    //         })
    //         .free_variables(),
    //         ["b".into()].into()
    //     );
    // }

    // #[test]
    // fn test_substitution_combine() {
    //     use MonoType::*;

    //     let mut sub1 = Substitution::new();
    //     sub1.insert("x".to_string(), Variable("y".to_string()));
    //     sub1.insert("a".to_string(), MonoType::int());

    //     let mut sub2 = Substitution::new();
    //     sub2.insert(
    //         "z".into(),
    //         FunApp(FunctionAppType {
    //             params: vec![MonoType::bool()],
    //             return_ty: Box::new(Variable("x".into())),
    //         }),
    //     );
    //     sub2.insert("a".to_string(), MonoType::bool());

    //     let mut sub3 = Substitution::new();
    //     sub3.insert("x".to_string(), Variable("y".to_string()));
    //     sub3.insert(
    //         "z".into(),
    //         FunApp(FunctionAppType {
    //             params: vec![MonoType::bool()],
    //             return_ty: Box::new(Variable("y".into())),
    //         }),
    //     );
    //     sub3.insert("a".to_string(), MonoType::bool());

    //     assert_eq!(sub1.combine(sub2), sub3);
    // }
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
