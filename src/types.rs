use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use libloading::Library;

#[derive(Clone, Debug)]
pub enum RuntimeValue {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Function(FunctionDefinition),
    Module(ModuleInstance),
    NativeModule(NativeModuleInstance),
    Array(Arc<Mutex<Vec<RuntimeValue>>>),
    HashMap(Arc<Mutex<HashMap<String, RuntimeValue>>>),
    Null,
}

unsafe impl Send for RuntimeValue {}
unsafe impl Sync for RuntimeValue {}

impl PartialEq for RuntimeValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => a == b,
            (RuntimeValue::Float(a), RuntimeValue::Float(b)) => (a - b).abs() < f64::EPSILON,
            (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => (*a as f64 - b).abs() < f64::EPSILON,
            (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => (a - *b as f64).abs() < f64::EPSILON,
            (RuntimeValue::String(a), RuntimeValue::String(b)) => a == b,
            (RuntimeValue::Boolean(a), RuntimeValue::Boolean(b)) => a == b,
            (RuntimeValue::Null, RuntimeValue::Null) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDefinition {
    pub parameters: Vec<String>,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub struct ModuleInstance {
    pub module_name: String,
    pub environment: ExecutionEnvironment,
}

#[derive(Clone, Debug)]
pub struct ExecutionEnvironment {
    pub variables: HashMap<String, RuntimeValue>,
}

impl ExecutionEnvironment {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn lookup(&self, key: &str) -> Option<RuntimeValue> {
        self.variables.get(key).cloned()
    }

    pub fn define(&mut self, key: String, value: RuntimeValue) {
        self.variables.insert(key, value);
    }

    pub fn set(&mut self, key: &str, value: RuntimeValue) -> Result<(), String> {
        if self.variables.contains_key(key) {
            self.variables.insert(key.to_string(), value);
            Ok(())
        } else {
            Err(format!("Variable '{}' not defined", key))
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExecSignal {
    None,
    Return(RuntimeValue),
    Break,
    Continue,
    Exit(i32),
}

#[derive(Clone, Debug)]
pub enum Expression {
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    Variable(String),
    BinaryOperation(Box<Expression>, BinaryOp, Box<Expression>),
    ComparisonOperation(Box<Expression>, ComparisonOp, Box<Expression>),
    LogicalOperation(Box<Expression>, LogicalOp, Box<Expression>),
    FunctionCall(Box<Expression>, Vec<Expression>),
    MemberAccess(Box<Expression>, String),
    ArrayLiteral(Vec<Expression>),
    IndexAccess(Box<Expression>, Box<Expression>),
    UnaryOperation(UnaryOp, Box<Expression>),
    TernaryOperation(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Clone, Copy, Debug)]
pub enum ComparisonOp {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Clone, Copy, Debug)]
pub enum LogicalOp {
    And,
    Or,
}

#[derive(Clone, Debug)]
pub enum Statement {
    VariableDeclaration(String, Expression),
    Assignment(String, Expression),
    IndexAssignment(String, Expression, Expression),
    PrintStatement(Expression),
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
    FunctionDeclaration(String, Vec<String>, Vec<Statement>),
    ImportStatement(String),
    IfStatement(Expression, Vec<Statement>, Option<Vec<Statement>>),
    WhileLoop(Expression, Vec<Statement>),
    ForLoop(String, Expression, Vec<Statement>),
    BreakStatement,
    ContinueStatement,
    ExitStatement(Expression),
    AssertStatement(Expression, Option<String>),
    EmptyStatement,
}

#[derive(Clone, Debug)]
pub struct PackageManifest {
    pub package_name: String,
    pub version_string: String,
    pub entry_point: String,
    pub package_type: String,
    pub native_library: String,
    pub dependencies: Vec<Dependency>,
}

#[derive(Clone, Debug)]
pub struct Dependency {
    pub name: String,
    pub version_constraint: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct NativeFunction {
    pub name_pointer: *const u8,
    pub name_length: usize,
    pub function_pointer: extern "C" fn(argc: usize, argv_ptr: *const NativeArgument) -> NativeReturn,
}

unsafe impl Send for NativeFunction {}
unsafe impl Sync for NativeFunction {}

#[repr(C)]
pub struct NativeArgument {
    pub argument_type: u8,
    pub integer_value: i64,
    pub string_pointer: *const u8,
    pub string_length: usize,
}

#[repr(C)]
pub struct NativeReturn {
    pub return_type: u8,
    pub integer_value: i64,
    pub string_pointer: *const u8,
    pub string_length: usize,
}

#[repr(C)]
pub struct NativeModuleApi {
    pub function_count: usize,
    pub functions_pointer: *const NativeFunction,
}

#[derive(Clone, Debug)]
pub struct NativeModuleInstance {
    pub module_name: String,
    pub library_handle: Arc<Library>,
    pub exported_functions: HashMap<String, NativeFunction>,
}

unsafe impl Send for NativeModuleInstance {}
unsafe impl Sync for NativeModuleInstance {}