use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use thiserror::Error;

use super::{
    ast::{Program, TypeAnnotation},
    positions::{HasSpan, Span},
    types::{FunctionAppType, MonoType, PolyType, StructType},
};

#[derive(Error, Clone, Debug)]
pub enum AnalysisErrorKind {
    #[error("Undeclared variable {0}")]
    UndeclaredVariable(String),

    #[error("Undefined type {0}")]
    UndefinedType(String),
}

#[derive(Clone, Debug)]
pub struct AnalysisError {
    pub kind: AnalysisErrorKind,
    pub span: Option<Span>,
}

impl AnalysisError {
    pub fn new(kind: AnalysisErrorKind) -> Self {
        Self { kind, span: None }
    }

    pub fn with_span(&self, span: Span) -> Self {
        Self {
            span: Some(span),
            ..self.clone()
        }
    }
}

type AnalysisResult<T> = Result<T, AnalysisError>;

macro_rules! err {
    ($kind:expr) => {
        Err(AnalysisError::new($kind))
    };
    ($kind:expr, $span:expr) => {
        Err(AnalysisError::new($kind).with_span($span))
    };
}

type SymbolId = usize;

#[derive(Clone, Debug)]
struct SymbolTable {
    /// Maps symbol IDs to their types
    symbols: HashMap<SymbolId, PolyType>,

    /// Maps variables names in a scope to their symbol IDs
    scopes: Vec<HashMap<String, SymbolId>>,

    counter: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            scopes: vec![HashMap::new()],
            counter: 0,
        }
    }

    pub fn insert_mono(&mut self, name: String, ty: MonoType) {
        self.insert(name, PolyType::Mono(ty));
    }

    pub fn insert(&mut self, name: String, ty: PolyType) {
        let id = self.gen_id();
        self.symbols.insert(id, ty);
        self.scopes.last_mut().unwrap().insert(name, id);
    }

    pub fn get(&mut self, name: &String) -> Option<(PolyType, SymbolId)> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.get(name) {
                let ty = self
                    .symbols
                    .get(id)
                    .expect(format!("Symbol ID {} not found in table", id).as_str());

                return Some((ty.clone(), *id));
            }
        }

        None
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn leave_scope(&mut self) {
        self.scopes.pop();
    }

    fn gen_id(&mut self) -> SymbolId {
        let id = self.counter;
        self.counter += 1;
        id
    }
}

pub struct TypeTable(HashMap<String, MonoType>);

pub struct Analysis {
    symbol_table: SymbolTable,
    type_table: TypeTable,
}

impl Analysis {
    pub fn new() -> Self {
        let symbol_table = SymbolTable::new();
        let mut type_table: HashMap<String, MonoType> = HashMap::new();

        // Load global types
        type_table.insert("Int".into(), MonoType::int());
        type_table.insert("Float".into(), MonoType::float());
        type_table.insert("Bool".into(), MonoType::bool());
        type_table.insert("String".into(), MonoType::string());

        Self {
            symbol_table,
            type_table: TypeTable(type_table),
        }
    }

    pub fn analysis_program(&mut self, program: &Program) -> AnalysisResult<()> {
        self.load_function_declarations(program)?;

        println!("\n=== Types:\n{}", self.type_table);
        println!("=== Symbols:\n{}", self.symbol_table);

        Ok(())
    }

    /// Generate types for all the top-level function declarations and load into the symbol table
    fn load_function_declarations(&mut self, program: &Program) -> AnalysisResult<()> {
        for func in program.functions() {
            // Get the type of the arguments
            let mut param_tys = Vec::new();
            for param in &func.args {
                let ty = self.type_for_annotation(&param.ty)?;
                param_tys.push(ty);
            }

            // Get the return type
            let return_ty = match &func.return_ty {
                Some(annotation) => {
                    let ty = self.type_for_annotation(annotation)?;
                    ty
                }
                None => MonoType::unit(),
            };

            let func_ty = FunctionAppType {
                params: param_tys,
                return_ty: Box::new(return_ty),
            };

            self.symbol_table
                .insert(func.name.clone(), PolyType::Mono(MonoType::FunApp(func_ty)));
        }

        Ok(())
    }

    fn type_for_annotation(&mut self, annotation: &TypeAnnotation) -> AnalysisResult<MonoType> {
        match self.type_table.0.get(&annotation.name) {
            Some(ty) => Ok(ty.clone()),
            None => err!(
                AnalysisErrorKind::UndefinedType(annotation.name.clone(),),
                annotation.span()
            ),
        }
        // let (ty, id) = match self.symbol_table.get(&annotation.name) {
        //     Some((ty, id)) => ((ty, id)),
        //     None => {
        //         return err!(
        //             AnalysisErrorKind::UndefinedType(annotation.name.clone(),),
        //             annotation.span()
        //         )
        //     }
        // };

        // match ty {
        //     PolyType::Mono(ty) => Ok((ty.clone(), id)),
        //     PolyType::Quantifier(_) => panic!("Type annotation should not be a quantifier"),
        // }
    }
}

impl Display for TypeTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (name, ty) in &self.0 {
            writeln!(f, "{}: {}", name, ty)?;
        }

        Ok(())
    }
}

impl Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for scope in self.scopes.iter().rev() {
            for (name, id) in scope {
                let ty = self
                    .symbols
                    .get(id)
                    .expect(format!("Symbol ID {} not found in table", id).as_str());

                writeln!(f, "{name}@{id}: {ty}")?;
            }
        }

        Ok(())
    }
}
