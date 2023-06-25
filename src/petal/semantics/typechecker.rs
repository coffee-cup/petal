use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
};

use crate::petal::{
    ast::{Expr, ExprId, ExprNode, IdentId, Stmt, StmtId, StmtNode},
    source_info::Span,
    types::{FunctionAppType, MonoType, PolyType, StructType, TyVar, TypeQuantifier},
};

use super::{
    context::SemanticContext,
    errors::{SemanticError, SemanticResult},
};

/// Generates fresh type variables
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
    /// Quantify all free variables in the type
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
    /// Replace all the forall quantifiers with type variables
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

/// Container to associate a type with an expression/identifier/parent
/// This is mainly used for error reporting
#[derive(Clone, Debug)]
pub struct MonoTypeData {
    pub ty: MonoType,
    pub expr_id: Option<ExprId>,
    pub ident_id: Option<IdentId>,
    pub parent_stmt_id: Option<StmtId>,
    pub parent_expr_id: Option<ExprId>,
}

impl MonoTypeData {
    pub fn new(ty: MonoType) -> Self {
        Self {
            ty,
            expr_id: None,
            ident_id: None,
            parent_stmt_id: None,
            parent_expr_id: None,
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

    pub fn with_parent_expr(mut self, expr_id: ExprId) -> Self {
        self.parent_expr_id = Some(expr_id);
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

/// A constraint between two types
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

impl<'a> SemanticContext<'a> {
    pub fn solve_constraints(&mut self) -> SemanticResult<()> {
        let mut sub = Substitution::new();

        for constraint in self.type_constraints.clone() {
            match constraint {
                Constraint::Equal { lhs, rhs } => {
                    let sub2 = self.unify_constraint(lhs.apply(&sub), rhs.apply(&sub))?;
                    sub = sub.combine(sub2);
                }
            }
        }

        self.apply_substition_to_symbol_table(&sub);

        Ok(())
    }

    /// Apply a substitution to the symbol table
    fn apply_substition_to_symbol_table(&mut self, sub: &Substitution) {
        for (_, sym) in self.symbol_table.symbols.iter_mut() {
            if let Some(ty) = &mut sym.ty {
                *ty = ty.apply(sub);
            }
        }
    }

    /// Unify two types
    /// Returns a substitution that can be applied to both types to make them equal
    /// If the types cannot be unified, an error is returned
    fn unify_constraint(
        &mut self,
        lhs_data: MonoTypeData,
        rhs_data: MonoTypeData,
    ) -> SemanticResult<Substitution> {
        use MonoType::*;

        let sub = match (&lhs_data.ty, &rhs_data.ty) {
            (Variable(v1), Variable(v2)) => {
                if v1 == v2 {
                    Substitution::new()
                } else {
                    let mut sub = Substitution::new();
                    sub.insert(v1.clone(), Variable(v2.clone()));
                    sub
                }
            }
            (Variable(v), ty) | (ty, Variable(v)) => {
                if occurs_check(ty, &Variable(v.clone())) {
                    panic!("Infinite type")
                } else {
                    let mut sub = Substitution::new();
                    sub.insert(v.clone(), ty.clone());
                    sub
                }
            }
            (FunApp(f1), FunApp(f2)) => {
                if f1.params.len() != f2.params.len() {
                    unreachable!(
                        "Function param length should be checked during constraint gathering"
                    );
                }

                let mut sub = Substitution::new();

                // Unify the argument types
                for (a, b) in f1.params.iter().zip(f2.params.iter()) {
                    sub = sub.combine(self.unify_constraint(
                        MonoTypeData::new(a.apply(&sub)),
                        MonoTypeData::new(b.apply(&sub)),
                    )?);
                }

                // Unify the return types
                sub = sub.combine(self.unify_constraint(
                    MonoTypeData::new(f1.return_ty.apply(&sub)),
                    MonoTypeData::new(f2.return_ty.apply(&sub)),
                )?);

                sub
            }

            (t1 @ Struct(s1), t2 @ Struct(s2)) => {
                if s1.name != s2.name {
                    println!("s1 = {}, s2 = {}", s1.name, s2.name);

                    return Err(self.mismatch_type_error(t1, t2, &lhs_data, &rhs_data));
                }

                let mut sub = Substitution::new();
                for (a, b) in s1.params.iter().zip(s2.params.iter()) {
                    // TODO: Create a MonoTypeData for each param
                    // For this, I need to associate the param with an identifier node

                    sub = sub.combine(self.unify_constraint(
                        MonoTypeData::new(a.apply(&sub)),
                        MonoTypeData::new(b.apply(&sub)),
                    )?);
                }

                sub
            }

            (t1, t2) => {
                if t1 != t2 {
                    return Err(self.mismatch_type_error(t1, t2, &lhs_data, &rhs_data));
                }

                Substitution::new()
            }
        };

        Ok(sub)
    }

    /// Return an error for mismatched types
    /// This will use the most specific error possible for the given lhs and rhs types
    fn mismatch_type_error(
        &self,
        t1: &MonoType,
        t2: &MonoType,
        lhs_data: &MonoTypeData,
        rhs_data: &MonoTypeData,
    ) -> SemanticError {
        // If condition type mismatch
        if let Some(
            n @ StmtNode {
                stmt: Stmt::IfStmt { .. },
                ..
            },
        ) = lhs_data
            .parent_stmt_id
            .map(|stmt_id| &self.program.ast.statements[stmt_id])
        {
            return SemanticError::IfConditionTypeMismatch {
                ty: t1.clone(),
                span: self.span_for_monotype_data(lhs_data).unwrap_or_default(),
                if_span: n.span.start().span_from_length(1),
            };
        }

        // Binary operand type mismatch
        if let Some(
            _n @ ExprNode {
                expr: Expr::BinaryOp { op, .. },
                ..
            },
        ) = lhs_data
            .parent_expr_id
            .map(|expr_id| &self.program.ast.expressions[expr_id])
        {
            return SemanticError::InvalidBinaryOperation {
                bin_span: op.span.clone(),
                op: op.binary_type.clone(),
                lhs_type: t1.clone(),
                rhs_type: t2.clone(),
                lhs_span: self.span_for_monotype_data(lhs_data).unwrap_or_default(),
                rhs_span: self.span_for_monotype_data(rhs_data).unwrap_or_default(),
            };
        }

        match (
            self.span_for_monotype_data(lhs_data),
            self.span_for_monotype_data(rhs_data),
        ) {
            (Some(lhs_span), Some(rhs_span)) => {
                // If we can, show the span of both types
                SemanticError::MismatchedTypes {
                    lhs: t1.clone(),
                    rhs: t2.clone(),
                    lhs_span,
                    rhs_span,
                }
            }
            (lhs_span, rhs_span) => SemanticError::ExpectedType {
                expected: t2.clone(),
                found: t1.clone(),
                span: lhs_span.or(rhs_span).unwrap_or_default(),
            },
        }
    }

    /// Returns the span for the given monotype data
    fn span_for_monotype_data(&self, ty_data: &MonoTypeData) -> Option<Span> {
        if let Some(expr_id) = ty_data.expr_id {
            return Some(self.program.ast.expressions[expr_id].span.clone());
        } else if let Some(ident_id) = ty_data.ident_id {
            return Some(self.program.ast.identifiers[ident_id].span.clone());
        }

        None
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
            ["a", "b"]
                .iter()
                .map(|s| s.to_string())
                .collect::<BTreeSet<_>>()
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
