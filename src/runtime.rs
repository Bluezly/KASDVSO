use crate::types::*;
use crate::parser::*;
use crate::package_manager::{parse_import_spec, resolve_package_version};
use crate::native_bridge::*;
use std::fs;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use std::cell::RefCell;
use std::path::PathBuf;

thread_local! {
    static SCRIPT_DIR: RefCell<PathBuf> = RefCell::new(PathBuf::from("."));
}

pub async fn execute_script(script_file: &Path) -> Result<(), String> {
    let script_dir = script_file
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();

    SCRIPT_DIR.with(|d| *d.borrow_mut() = script_dir.clone());

    let source_code = fs::read_to_string(script_file).map_err(|e| e.to_string())?;
    let cleaned_source = source_code.trim_start_matches('\u{feff}').to_string();
    let parsed_program = parse_kas_program(&cleaned_source)?;
    let mut execution_environment = ExecutionEnvironment::new();

    execution_environment.define(
        "__file__".to_string(),
        RuntimeValue::String(script_file.to_string_lossy().to_string()),
    );
    execution_environment.define(
        "__dir__".to_string(),
        RuntimeValue::String(script_dir.to_string_lossy().to_string()),
    );

    match run_program(&parsed_program, &mut execution_environment).await? {
        ExecSignal::Exit(code) => std::process::exit(code),
        _ => {}
    }

    Ok(())
}


pub async fn run_program(
    statements: &[Statement],
    environment: &mut ExecutionEnvironment,
) -> Result<ExecSignal, String> {
    for statement in statements {
        let signal = Box::pin(execute_statement(statement, environment)).await?;
        match signal {
            ExecSignal::None => continue,
            _ => return Ok(signal),
        }
    }
    Ok(ExecSignal::None)
}

pub async fn execute_statement(
    statement: &Statement,
    environment: &mut ExecutionEnvironment,
) -> Result<ExecSignal, String> {
    match statement {
        Statement::EmptyStatement => Ok(ExecSignal::None),
        Statement::VariableDeclaration(variable_name, expression) => {
            let computed_value = Box::pin(evaluate_expression(expression, environment)).await?;
            environment.define(variable_name.clone(), computed_value);
            Ok(ExecSignal::None)
        }
        Statement::Assignment(variable_name, expression) => {
            let computed_value = Box::pin(evaluate_expression(expression, environment)).await?;
            environment.set(variable_name, computed_value)?;
            Ok(ExecSignal::None)
        }
        Statement::IndexAssignment(variable_name, index_expr, value_expr) => {
            let index_value = Box::pin(evaluate_expression(index_expr, environment)).await?;
            let new_value = Box::pin(evaluate_expression(value_expr, environment)).await?;
            
            let container = environment.lookup(variable_name)
                .ok_or_else(|| format!("Variable '{}' not defined", variable_name))?;
            
            match container {
                RuntimeValue::Array(arr) => {
                    if let RuntimeValue::Integer(idx) = index_value {
                        let mut arr_lock = arr.lock().unwrap();
                        let idx = idx as usize;
                        if idx < arr_lock.len() {
                            arr_lock[idx] = new_value;
                            Ok(ExecSignal::None)
                        } else {
                            Err("Array index out of bounds".into())
                        }
                    } else {
                        Err("Array index must be an integer".into())
                    }
                }
                RuntimeValue::HashMap(map) => {
                    if let RuntimeValue::String(key) = index_value {
                        let mut map_lock = map.lock().unwrap();
                        map_lock.insert(key, new_value);
                        Ok(ExecSignal::None)
                    } else {
                        Err("HashMap key must be a string".into())
                    }
                }
                _ => Err("Can only index arrays and hashmaps".into())
            }
        }
        Statement::PrintStatement(expression) => {
            let value_to_print = Box::pin(evaluate_expression(expression, environment)).await?;
            println!("{}", format_value(&value_to_print));
            Ok(ExecSignal::None)
        }
        Statement::ReturnStatement(expression) => {
            let return_value = Box::pin(evaluate_expression(expression, environment)).await?;
            Ok(ExecSignal::Return(return_value))
        }
        Statement::ExpressionStatement(expression) => {
            let _ = Box::pin(evaluate_expression(expression, environment)).await?;
            Ok(ExecSignal::None)
        }
        Statement::FunctionDeclaration(function_name, parameters, body) => {
            environment.define(
                function_name.clone(),
                RuntimeValue::Function(FunctionDefinition {
                    parameters: parameters.clone(),
                    body: body.clone(),
                }),
            );
            Ok(ExecSignal::None)
        }
       Statement::ImportStatement(import_specification) => {
    let raw = import_specification.trim();

    let spec = raw.trim_matches('"').trim_matches('\'');

    let looks_like_path =
        spec.ends_with(".kas") || spec.contains('/') || spec.contains('\\') || spec.starts_with('.');

    if looks_like_path {
        let full_path = SCRIPT_DIR.with(|d| d.borrow().join(spec));

        if !full_path.exists() {
            return Err(format!("Local import not found: {}", full_path.display()));
        }

        let module_name = full_path
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or("Invalid module filename")?
            .to_string();

        let kas_module = Box::pin(load_kas_module_from_file(&module_name, &full_path)).await?;
        environment.define(module_name, RuntimeValue::Module(kas_module));

        return Ok(ExecSignal::None);
    }

    let (package_name, version_constraint) = parse_import_spec(import_specification);

    if let Some(builtin_module) = crate::native_std::get_builtin_native_module(&package_name) {
        environment.define(package_name.clone(), RuntimeValue::NativeModule(builtin_module));
        return Ok(ExecSignal::None);
    }

    let (package_directory, manifest) = resolve_package_version(&package_name, &version_constraint)?;

    for dep in &manifest.dependencies {
        if let Some(builtin_dep) = crate::native_std::get_builtin_native_module(&dep.name) {
            environment.define(dep.name.clone(), RuntimeValue::NativeModule(builtin_dep));
            continue;
        }

        let (dep_dir, dep_manifest) = resolve_package_version(&dep.name, &dep.version_constraint)?;
        if dep_manifest.package_type == "rust" {
            let native_module = load_native_module(&dep.name, &dep_dir, &dep_manifest)?;
            environment.define(dep.name.clone(), RuntimeValue::NativeModule(native_module));
        } else {
            let kas_module = Box::pin(load_kas_module(&dep.name, &dep_dir, &dep_manifest)).await?;
            environment.define(dep.name.clone(), RuntimeValue::Module(kas_module));
        }
    }

    if manifest.package_type == "rust" {
        let native_module = load_native_module(&package_name, &package_directory, &manifest)?;
        environment.define(package_name.clone(), RuntimeValue::NativeModule(native_module));
    } else {
        let kas_module = Box::pin(load_kas_module(&package_name, &package_directory, &manifest)).await?;
        environment.define(package_name.clone(), RuntimeValue::Module(kas_module));
    }

    Ok(ExecSignal::None)
}
        Statement::IfStatement(condition, then_block, else_block) => {
            let condition_value = Box::pin(evaluate_expression(condition, environment)).await?;
            let is_truthy = is_value_truthy(&condition_value);
            
            if is_truthy {
                for stmt in then_block {
                    match Box::pin(execute_statement(stmt, environment)).await? {
                        ExecSignal::None => continue,
                        signal => return Ok(signal),
                    }
                }
            } else if let Some(else_stmts) = else_block {
                for stmt in else_stmts {
                    match Box::pin(execute_statement(stmt, environment)).await? {
                        ExecSignal::None => continue,
                        signal => return Ok(signal),
                    }
                }
            }
            Ok(ExecSignal::None)
        }
        Statement::WhileLoop(condition, body) => {
            loop {
                let condition_value = Box::pin(evaluate_expression(condition, environment)).await?;
                if !is_value_truthy(&condition_value) {
                    break;
                }
                
                for stmt in body {
                    match Box::pin(execute_statement(stmt, environment)).await? {
                        ExecSignal::None => continue,
                        ExecSignal::Break => return Ok(ExecSignal::None),
                        ExecSignal::Continue => break,
                        signal => return Ok(signal),
                    }
                }
            }
            Ok(ExecSignal::None)
        }
        Statement::ForLoop(iterator_var, iterable_expr, body) => {
            let iterable = Box::pin(evaluate_expression(iterable_expr, environment)).await?;
            
            match iterable {
                RuntimeValue::Array(arr) => {
                    let items: Vec<RuntimeValue> = {
                        let arr_lock = arr.lock().unwrap();
                        arr_lock.clone()
                    };
                    
                    for item in items {
                        environment.define(iterator_var.clone(), item);
                        for stmt in body {
                            match Box::pin(execute_statement(stmt, environment)).await? {
                                ExecSignal::None => continue,
                                ExecSignal::Break => return Ok(ExecSignal::None),
                                ExecSignal::Continue => break,
                                signal => return Ok(signal),
                            }
                        }
                    }
                }
                _ => return Err("For loop requires an iterable (array)".into())
            }
            Ok(ExecSignal::None)
        }
        Statement::BreakStatement => Ok(ExecSignal::Break),
        Statement::ContinueStatement => Ok(ExecSignal::Continue),
        Statement::ExitStatement(code_expr) => {
            let code_value = Box::pin(evaluate_expression(code_expr, environment)).await?;
            let exit_code = match code_value {
                RuntimeValue::Integer(code) => code as i32,
                _ => return Err("Exit code must be an integer".into()),
            };
            Ok(ExecSignal::Exit(exit_code))
        }
        Statement::AssertStatement(condition_expr, message) => {
            let condition_value = Box::pin(evaluate_expression(condition_expr, environment)).await?;
            if !is_value_truthy(&condition_value) {
                let error_message = message
                    .as_ref()
                    .map(|m| m.clone())
                    .unwrap_or_else(|| "Assertion failed".to_string());
                return Err(error_message);
            }
            Ok(ExecSignal::None)
        }
    }
}

pub async fn load_kas_module(
    module_name: &str,
    package_directory: &Path,
    manifest: &PackageManifest,
) -> Result<ModuleInstance, String> {
    let entry_file = package_directory.join(&manifest.entry_point);
    let source_code = fs::read_to_string(&entry_file)
        .map_err(|e| format!("Failed to read entry file: {e} ({})", entry_file.display()))?;
    let cleaned_source = source_code.trim_start_matches('\u{feff}').to_string();
    let parsed_program = parse_kas_program(&cleaned_source)?;
    let mut module_environment = ExecutionEnvironment::new();
    
    match Box::pin(run_program(&parsed_program, &mut module_environment)).await? {
        ExecSignal::Exit(code) => std::process::exit(code),
        _ => {}
    }

    Ok(ModuleInstance {
        module_name: module_name.to_string(),
        environment: module_environment,
    })
}

pub async fn load_kas_module_from_file(
    module_name: &str,
    file_path: &Path,
) -> Result<ModuleInstance, String> {
    let source_code = fs::read_to_string(file_path)
        .map_err(|e| format!("Failed to read module file: {e} ({})", file_path.display()))?;

    let cleaned_source = source_code.trim_start_matches('\u{feff}').to_string();
    let parsed_program = parse_kas_program(&cleaned_source)?;

    let mut module_environment = ExecutionEnvironment::new();

    let dir = file_path.parent().unwrap_or_else(|| Path::new("."));
    module_environment.define(
        "__file__".to_string(),
        RuntimeValue::String(file_path.to_string_lossy().to_string()),
    );
    module_environment.define(
        "__dir__".to_string(),
        RuntimeValue::String(dir.to_string_lossy().to_string()),
    );

    match Box::pin(run_program(&parsed_program, &mut module_environment)).await? {
        ExecSignal::Exit(code) => std::process::exit(code),
        _ => {}
    }

    Ok(ModuleInstance {
        module_name: module_name.to_string(),
        environment: module_environment,
    })
}


pub async fn evaluate_expression(
    expression: &Expression,
    environment: &mut ExecutionEnvironment,
) -> Result<RuntimeValue, String> {
    match expression {
        Expression::IntegerLiteral(number) => Ok(RuntimeValue::Integer(*number)),
        Expression::FloatLiteral(number) => Ok(RuntimeValue::Float(*number)),
        Expression::StringLiteral(text) => Ok(RuntimeValue::String(text.clone())),
        Expression::BooleanLiteral(b) => Ok(RuntimeValue::Boolean(*b)),
        Expression::Variable(name) => environment
            .lookup(name)
            .ok_or_else(|| format!("Variable '{}' is not defined", name)),
        Expression::ArrayLiteral(elements) => {
            let mut arr = Vec::new();
            for elem_expr in elements {
                arr.push(Box::pin(evaluate_expression(elem_expr, environment)).await?);
            }
            Ok(RuntimeValue::Array(Arc::new(Mutex::new(arr))))
        }
        Expression::UnaryOperation(op, operand_expr) => {
            let operand = Box::pin(evaluate_expression(operand_expr, environment)).await?;
            match op {
                UnaryOp::Negate => {
                    match operand {
                        RuntimeValue::Integer(i) => Ok(RuntimeValue::Integer(-i)),
                        RuntimeValue::Float(f) => Ok(RuntimeValue::Float(-f)),
                        _ => Err("Cannot negate non-numeric value".into()),
                    }
                }
                UnaryOp::Not => {
                    Ok(RuntimeValue::Boolean(!is_value_truthy(&operand)))
                }
            }
        }
        Expression::TernaryOperation(condition_expr, true_expr, false_expr) => {
            let condition = Box::pin(evaluate_expression(condition_expr, environment)).await?;
            if is_value_truthy(&condition) {
                Box::pin(evaluate_expression(true_expr, environment)).await
            } else {
                Box::pin(evaluate_expression(false_expr, environment)).await
            }
        }
        Expression::IndexAccess(container_expr, index_expr) => {
            let container = Box::pin(evaluate_expression(container_expr, environment)).await?;
            let index = Box::pin(evaluate_expression(index_expr, environment)).await?;
            
            match container {
                RuntimeValue::Array(arr) => {
                    if let RuntimeValue::Integer(idx) = index {
                        let arr_lock = arr.lock().unwrap();
                        let idx = if idx < 0 {
                            (arr_lock.len() as i64 + idx) as usize
                        } else {
                            idx as usize
                        };
                        if idx < arr_lock.len() {
                            Ok(arr_lock[idx].clone())
                        } else {
                            Err("Array index out of bounds".into())
                        }
                    } else {
                        Err("Array index must be an integer".into())
                    }
                }
                RuntimeValue::HashMap(map) => {
                    if let RuntimeValue::String(key) = index {
                        let map_lock = map.lock().unwrap();
                        Ok(map_lock.get(&key).cloned().unwrap_or(RuntimeValue::Null))
                    } else {
                        Err("HashMap key must be a string".into())
                    }
                }
                RuntimeValue::String(s) => {
                    if let RuntimeValue::Integer(idx) = index {
                        let chars: Vec<char> = s.chars().collect();
                        let idx = if idx < 0 {
                            (chars.len() as i64 + idx) as usize
                        } else {
                            idx as usize
                        };
                        if idx < chars.len() {
                            Ok(RuntimeValue::String(chars[idx].to_string()))
                        } else {
                            Err("String index out of bounds".into())
                        }
                    } else {
                        Err("String index must be an integer".into())
                    }
                }
                _ => Err("Can only index arrays, hashmaps, and strings".into())
            }
        }
        Expression::MemberAccess(object_expression, field_name) => {
            let object_value = Box::pin(evaluate_expression(object_expression, environment)).await?;
            match object_value {
                RuntimeValue::Module(module) => module
                    .environment
                    .lookup(field_name)
                    .ok_or_else(|| format!("Module '{}' does not have member '{}'", module.module_name, field_name)),
                RuntimeValue::NativeModule(native_module) => {
                    if native_module.exported_functions.contains_key(field_name) {
                        Ok(RuntimeValue::String(format!(
                            "__native__::{}::{}",
                            native_module.module_name, field_name
                        )))
                    } else {
                        Err(format!(
                            "Native module '{}' does not have function '{}'",
                            native_module.module_name, field_name
                        ))
                    }
                }
                RuntimeValue::Array(arr) => {
                    match field_name.as_str() {
                        "length" | "len" => {
                            let arr_lock = arr.lock().unwrap();
                            Ok(RuntimeValue::Integer(arr_lock.len() as i64))
                        }
                        "push" | "get" | "set" | "pop" | "shift" | "unshift" => {
                            Ok(RuntimeValue::String(format!("__array__::{}", field_name)))
                        }
                        _ => Err(format!("Array does not have method '{}'", field_name))
                    }
                }
                RuntimeValue::HashMap(map) => {
                    match field_name.as_str() {
                        "size" | "len" => {
                            let map_lock = map.lock().unwrap();
                            Ok(RuntimeValue::Integer(map_lock.len() as i64))
                        }
                        "get" | "set" | "keys" | "values" | "has" | "remove" | "clear" => {
                            Ok(RuntimeValue::String(format!("__hashmap__::{}", field_name)))
                        }
                        _ => Err(format!("HashMap does not have method '{}'", field_name))
                    }
                }
                RuntimeValue::String(s) => {
                    match field_name.as_str() {
                        "length" | "len" => {
                            Ok(RuntimeValue::Integer(s.len() as i64))
                        }
                        "toUpperCase" | "toLowerCase" | "trim" | "split" | "replace" | "charAt" | "indexOf" => {
                            Ok(RuntimeValue::String(format!("__string__::{}", field_name)))
                        }
                        _ => Err(format!("String does not have method '{}'", field_name))
                    }
                }
                _ => Err("Member access requires a module, array, hashmap, or string".into()),
            }
        }
        Expression::BinaryOperation(left_expr, operator, right_expr) => {
            let left_value = Box::pin(evaluate_expression(left_expr, environment)).await?;
            let right_value = Box::pin(evaluate_expression(right_expr, environment)).await?;
            perform_binary_operation(&left_value, *operator, &right_value)
        }
        Expression::ComparisonOperation(left_expr, operator, right_expr) => {
            let left_value = Box::pin(evaluate_expression(left_expr, environment)).await?;
            let right_value = Box::pin(evaluate_expression(right_expr, environment)).await?;
            perform_comparison_operation(&left_value, *operator, &right_value)
        }
        Expression::LogicalOperation(left_expr, operator, right_expr) => {
            let left_value = Box::pin(evaluate_expression(left_expr, environment)).await?;
            
            match operator {
                LogicalOp::And => {
                    if !is_value_truthy(&left_value) {
                        Ok(RuntimeValue::Boolean(false))
                    } else {
                        let right_value = Box::pin(evaluate_expression(right_expr, environment)).await?;
                        Ok(RuntimeValue::Boolean(is_value_truthy(&right_value)))
                    }
                }
                LogicalOp::Or => {
                    if is_value_truthy(&left_value) {
                        Ok(RuntimeValue::Boolean(true))
                    } else {
                        let right_value = Box::pin(evaluate_expression(right_expr, environment)).await?;
                        Ok(RuntimeValue::Boolean(is_value_truthy(&right_value)))
                    }
                }
            }
        }
        Expression::FunctionCall(callee_expression, argument_expressions) => {
            if let Expression::Variable(function_name) = &**callee_expression {
                match function_name.as_str() {
                    "net_get" => {
                        if argument_expressions.len() != 1 {
                            return Err("net_get requires exactly one argument: url".into());
                        }
                        let url_value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                        let url_string = convert_to_string(&url_value)?;
                        return Ok(RuntimeValue::String(perform_http_get(&url_string).await?));
                    }
                    "net_post" => {
                        if argument_expressions.len() != 2 {
                            return Err("net_post requires exactly two arguments: url and body".into());
                        }
                        let url_value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                        let body_value = Box::pin(evaluate_expression(&argument_expressions[1], environment)).await?;
                        let url_string = convert_to_string(&url_value)?;
                        let body_string = convert_to_string(&body_value)?;
                        return Ok(RuntimeValue::String(perform_http_post(&url_string, &body_string).await?));
                    }
                    "net_put" => {
                        if argument_expressions.len() != 2 {
                            return Err("net_put requires exactly two arguments: url and body".into());
                        }
                        let url_value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                        let body_value = Box::pin(evaluate_expression(&argument_expressions[1], environment)).await?;
                        let url_string = convert_to_string(&url_value)?;
                        let body_string = convert_to_string(&body_value)?;
                        return Ok(RuntimeValue::String(perform_http_put(&url_string, &body_string).await?));
                    }
                    "net_delete" => {
                        if argument_expressions.len() != 1 {
                            return Err("net_delete requires exactly one argument: url".into());
                        }
                        let url_value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                        let url_string = convert_to_string(&url_value)?;
                        return Ok(RuntimeValue::String(perform_http_delete(&url_string).await?));
                    }
                    "array" => {
                        return Ok(RuntimeValue::Array(Arc::new(Mutex::new(Vec::new()))));
                    }
                    "hashmap" => {
                        return Ok(RuntimeValue::HashMap(Arc::new(Mutex::new(HashMap::new()))));
                    }
                    "range" => {
                        if argument_expressions.len() < 2 || argument_expressions.len() > 3 {
                            return Err("range requires 2-3 arguments: start, end, [step]".into());
                        }
                        let start = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                        let end = Box::pin(evaluate_expression(&argument_expressions[1], environment)).await?;
                        let step = if argument_expressions.len() == 3 {
                            Box::pin(evaluate_expression(&argument_expressions[2], environment)).await?
                        } else {
                            RuntimeValue::Integer(1)
                        };
                        
                        if let (RuntimeValue::Integer(s), RuntimeValue::Integer(e), RuntimeValue::Integer(st)) = (start, end, step) {
                            let mut arr = Vec::new();
                            if st > 0 {
                                let mut i = s;
                                while i < e {
                                    arr.push(RuntimeValue::Integer(i));
                                    i += st;
                                }
                            } else if st < 0 {
                                let mut i = s;
                                while i > e {
                                    arr.push(RuntimeValue::Integer(i));
                                    i += st;
                                }
                            }
                            return Ok(RuntimeValue::Array(Arc::new(Mutex::new(arr))));
                        }
                        return Err("range arguments must be integers".into());
                    }
                    "len" => {
                        if argument_expressions.len() != 1 {
                            return Err("len requires one argument".into());
                        }
                        let value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                        match value {
                            RuntimeValue::Array(arr) => {
                                let arr_lock = arr.lock().unwrap();
                                return Ok(RuntimeValue::Integer(arr_lock.len() as i64));
                            }
                            RuntimeValue::HashMap(map) => {
                                let map_lock = map.lock().unwrap();
                                return Ok(RuntimeValue::Integer(map_lock.len() as i64));
                            }
                            RuntimeValue::String(s) => {
                                return Ok(RuntimeValue::Integer(s.len() as i64));
                            }
                            _ => return Err("len() requires array, hashmap, or string".into())
                        }
                    }
                    "type_of" => {
                        if argument_expressions.len() != 1 {
                            return Err("type_of requires one argument".into());
                        }
                        let value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                        let type_name = match value {
                            RuntimeValue::Integer(_) => "integer",
                            RuntimeValue::Float(_) => "float",
                            RuntimeValue::String(_) => "string",
                            RuntimeValue::Boolean(_) => "boolean",
                            RuntimeValue::Array(_) => "array",
                            RuntimeValue::HashMap(_) => "hashmap",
                            RuntimeValue::Function(_) => "function",
                            RuntimeValue::Module(_) => "module",
                            RuntimeValue::NativeModule(_) => "native_module",
                            RuntimeValue::Null => "null",
                        };
                        return Ok(RuntimeValue::String(type_name.to_string()));
                    }
                    "str" => {
                        if argument_expressions.len() != 1 {
                            return Err("str requires one argument".into());
                        }
                        let value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                        return Ok(RuntimeValue::String(format_value(&value)));
                    }
                    "int" => {
                        if argument_expressions.len() != 1 {
                            return Err("int requires one argument".into());
                        }
                        let value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                        match value {
                            RuntimeValue::Integer(i) => return Ok(RuntimeValue::Integer(i)),
                            RuntimeValue::Float(f) => return Ok(RuntimeValue::Integer(f as i64)),
                            RuntimeValue::Boolean(b) => return Ok(RuntimeValue::Integer(if b { 1 } else { 0 })),
                            RuntimeValue::String(s) => {
                                if let Ok(i) = s.parse::<i64>() {
                                    return Ok(RuntimeValue::Integer(i));
                                }
                                return Err("Cannot convert string to integer".into());
                            }
                            _ => return Err("Cannot convert to integer".into())
                        }
                    }
                    "float" => {
                        if argument_expressions.len() != 1 {
                            return Err("float requires one argument".into());
                        }
                        let value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                        match value {
                            RuntimeValue::Integer(i) => return Ok(RuntimeValue::Float(i as f64)),
                            RuntimeValue::Float(f) => return Ok(RuntimeValue::Float(f)),
                            RuntimeValue::Boolean(b) => return Ok(RuntimeValue::Float(if b { 1.0 } else { 0.0 })),
                            RuntimeValue::String(s) => {
                                if let Ok(f) = s.parse::<f64>() {
                                    return Ok(RuntimeValue::Float(f));
                                }
                                return Err("Cannot convert string to float".into());
                            }
                            _ => return Err("Cannot convert to float".into())
                        }
                    }
                    "println" => {
                        if argument_expressions.is_empty() {
                            println!();
                            return Ok(RuntimeValue::Null);
                        }
                        let value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                        println!("{}", format_value(&value));
                        return Ok(RuntimeValue::Null);
                    }
                    "time_now" => {
                        return Ok(RuntimeValue::Integer(crate::get_current_timestamp() as i64));
                    }
                    "sleep" => {
                        if argument_expressions.len() != 1 {
                            return Err("sleep requires one argument: milliseconds".into());
                        }
                        let ms_value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                        if let RuntimeValue::Integer(ms) = ms_value {
                            tokio::time::sleep(tokio::time::Duration::from_millis(ms as u64)).await;
                            return Ok(RuntimeValue::Null);
                        }
                        return Err("sleep argument must be an integer".into());
                    }
                    _ => {}
                }
            }

            if let Expression::MemberAccess(obj_expr, method_name) = &**callee_expression {
                let object = Box::pin(evaluate_expression(obj_expr, environment)).await?;
                
                match object {
                    RuntimeValue::Array(arr) => {
                        match method_name.as_str() {
                            "push" => {
                                if argument_expressions.len() != 1 {
                                    return Err("Array.push requires one argument".into());
                                }
                                let value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                                let mut arr_lock = arr.lock().unwrap();
                                arr_lock.push(value);
                                return Ok(RuntimeValue::Null);
                            }
                            "pop" => {
                                if !argument_expressions.is_empty() {
                                    return Err("Array.pop takes no arguments".into());
                                }
                                let mut arr_lock = arr.lock().unwrap();
                                return Ok(arr_lock.pop().unwrap_or(RuntimeValue::Null));
                            }
                            "shift" => {
                                if !argument_expressions.is_empty() {
                                    return Err("Array.shift takes no arguments".into());
                                }
                                let mut arr_lock = arr.lock().unwrap();
                                if arr_lock.is_empty() {
                                    return Ok(RuntimeValue::Null);
                                }
                                return Ok(arr_lock.remove(0));
                            }
                            "unshift" => {
                                if argument_expressions.len() != 1 {
                                    return Err("Array.unshift requires one argument".into());
                                }
                                let value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                                let mut arr_lock = arr.lock().unwrap();
                                arr_lock.insert(0, value);
                                return Ok(RuntimeValue::Null);
                            }
                            "get" => {
                                if argument_expressions.len() != 1 {
                                    return Err("Array.get requires one argument".into());
                                }
                                let index = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                                if let RuntimeValue::Integer(idx) = index {
                                    let arr_lock = arr.lock().unwrap();
                                    let idx = if idx < 0 {
                                        (arr_lock.len() as i64 + idx) as usize
                                    } else {
                                        idx as usize
                                    };
                                    if idx < arr_lock.len() {
                                        return Ok(arr_lock[idx].clone());
                                    }
                                    return Err("Array index out of bounds".into());
                                }
                                return Err("Array index must be an integer".into());
                            }
                            "set" => {
                                if argument_expressions.len() != 2 {
                                    return Err("Array.set requires two arguments".into());
                                }
                                let index = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                                let value = Box::pin(evaluate_expression(&argument_expressions[1], environment)).await?;
                                if let RuntimeValue::Integer(idx) = index {
                                    let mut arr_lock = arr.lock().unwrap();
                                    let idx = if idx < 0 {
                                        (arr_lock.len() as i64 + idx) as usize
                                    } else {
                                        idx as usize
                                    };
                                    if idx < arr_lock.len() {
                                        arr_lock[idx] = value;
                                        return Ok(RuntimeValue::Null);
                                    }
                                    return Err("Array index out of bounds".into());
                                }
                                return Err("Array index must be an integer".into());
                            }
                            "len" | "length" => {
                                if !argument_expressions.is_empty() {
                                    return Err("Array.len takes no arguments".into());
                                }
                                let arr_lock = arr.lock().unwrap();
                                return Ok(RuntimeValue::Integer(arr_lock.len() as i64));
                            }
                            _ => return Err(format!("Array does not have method '{}'", method_name))
                        }
                    }
                    RuntimeValue::HashMap(map) => {
                        match method_name.as_str() {
                            "get" => {
                                if argument_expressions.len() != 1 {
                                    return Err("HashMap.get requires one argument".into());
                                }
                                let key = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                                if let RuntimeValue::String(k) = key {
                                    let map_lock = map.lock().unwrap();
                                    return Ok(map_lock.get(&k).cloned().unwrap_or(RuntimeValue::Null));
                                }
                                return Err("HashMap key must be a string".into());
                            }
                            "set" => {
                                if argument_expressions.len() != 2 {
                                    return Err("HashMap.set requires two arguments".into());
                                }
                                let key = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                                let value = Box::pin(evaluate_expression(&argument_expressions[1], environment)).await?;
                                if let RuntimeValue::String(k) = key {
                                    let mut map_lock = map.lock().unwrap();
                                    map_lock.insert(k, value);
                                    return Ok(RuntimeValue::Null);
                                }
                                return Err("HashMap key must be a string".into());
                            }
                            "remove" => {
                                if argument_expressions.len() != 1 {
                                    return Err("HashMap.remove requires one argument".into());
                                }
                                let key = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                                if let RuntimeValue::String(k) = key {
                                    let mut map_lock = map.lock().unwrap();
                                    return Ok(map_lock.remove(&k).unwrap_or(RuntimeValue::Null));
                                }
                                return Err("HashMap key must be a string".into());
                            }
                            "clear" => {
                                if !argument_expressions.is_empty() {
                                    return Err("HashMap.clear takes no arguments".into());
                                }
                                let mut map_lock = map.lock().unwrap();
                                map_lock.clear();
                                return Ok(RuntimeValue::Null);
                            }
                            "keys" => {
                                if !argument_expressions.is_empty() {
                                    return Err("HashMap.keys takes no arguments".into());
                                }
                                let map_lock = map.lock().unwrap();
                                let keys: Vec<RuntimeValue> = map_lock.keys()
                                    .map(|k| RuntimeValue::String(k.clone()))
                                    .collect();
                                return Ok(RuntimeValue::Array(Arc::new(Mutex::new(keys))));
                            }
                            "values" => {
                                if !argument_expressions.is_empty() {
                                    return Err("HashMap.values takes no arguments".into());
                                }
                                let map_lock = map.lock().unwrap();
                                let values: Vec<RuntimeValue> = map_lock.values().cloned().collect();
                                return Ok(RuntimeValue::Array(Arc::new(Mutex::new(values))));
                            }
                            "has" => {
                                if argument_expressions.len() != 1 {
                                    return Err("HashMap.has requires one argument".into());
                                }
                                let key = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                                if let RuntimeValue::String(k) = key {
                                    let map_lock = map.lock().unwrap();
                                    return Ok(RuntimeValue::Boolean(map_lock.contains_key(&k)));
                                }
                                return Err("HashMap key must be a string".into());
                            }
                            "len" | "size" => {
                                if !argument_expressions.is_empty() {
                                    return Err("HashMap.len takes no arguments".into());
                                }
                                let map_lock = map.lock().unwrap();
                                return Ok(RuntimeValue::Integer(map_lock.len() as i64));
                            }
                            _ => return Err(format!("HashMap does not have method '{}'", method_name))
                        }
                    }
                    RuntimeValue::String(s) => {
                        match method_name.as_str() {
                            "toUpperCase" => {
                                if !argument_expressions.is_empty() {
                                    return Err("String.toUpperCase takes no arguments".into());
                                }
                                return Ok(RuntimeValue::String(s.to_uppercase()));
                            }
                            "toLowerCase" => {
                                if !argument_expressions.is_empty() {
                                    return Err("String.toLowerCase takes no arguments".into());
                                }
                                return Ok(RuntimeValue::String(s.to_lowercase()));
                            }
                            "trim" => {
                                if !argument_expressions.is_empty() {
                                    return Err("String.trim takes no arguments".into());
                                }
                                return Ok(RuntimeValue::String(s.trim().to_string()));
                            }
                            "split" => {
                                if argument_expressions.len() != 1 {
                                    return Err("String.split requires one argument".into());
                                }
                                let delimiter_value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                                if let RuntimeValue::String(delimiter) = delimiter_value {
                                    let parts: Vec<RuntimeValue> = s.split(&delimiter)
                                        .map(|part| RuntimeValue::String(part.to_string()))
                                        .collect();
                                    return Ok(RuntimeValue::Array(Arc::new(Mutex::new(parts))));
                                }
                                return Err("String.split delimiter must be a string".into());
                            }
                            "replace" => {
                                if argument_expressions.len() != 2 {
                                    return Err("String.replace requires two arguments".into());
                                }
                                let old_value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                                let new_value = Box::pin(evaluate_expression(&argument_expressions[1], environment)).await?;
                                if let (RuntimeValue::String(old), RuntimeValue::String(new)) = (old_value, new_value) {
                                    return Ok(RuntimeValue::String(s.replace(&old, &new)));
                                }
                                return Err("String.replace arguments must be strings".into());
                            }
                            "charAt" => {
                                if argument_expressions.len() != 1 {
                                    return Err("String.charAt requires one argument".into());
                                }
                                let index_value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                                if let RuntimeValue::Integer(idx) = index_value {
                                    let chars: Vec<char> = s.chars().collect();
                                    let idx = if idx < 0 {
                                        (chars.len() as i64 + idx) as usize
                                    } else {
                                        idx as usize
                                    };
                                    if idx < chars.len() {
                                        return Ok(RuntimeValue::String(chars[idx].to_string()));
                                    }
                                    return Ok(RuntimeValue::String("".to_string()));
                                }
                                return Err("String.charAt index must be an integer".into());
                            }
                            "indexOf" => {
                                if argument_expressions.len() != 1 {
                                    return Err("String.indexOf requires one argument".into());
                                }
                                let substr_value = Box::pin(evaluate_expression(&argument_expressions[0], environment)).await?;
                                if let RuntimeValue::String(substr) = substr_value {
                                    if let Some(pos) = s.find(&substr) {
                                        return Ok(RuntimeValue::Integer(pos as i64));
                                    }
                                    return Ok(RuntimeValue::Integer(-1));
                                }
                                return Err("String.indexOf argument must be a string".into());
                            }
                            _ => return Err(format!("String does not have method '{}'", method_name))
                        }
                    }
                    _ => {}
                }
            }

            let function_value = Box::pin(evaluate_expression(callee_expression, environment)).await?;
            let mut argument_values = Vec::new();
            for argument_expr in argument_expressions {
                argument_values.push(Box::pin(evaluate_expression(argument_expr, environment)).await?);
            }
            Box::pin(invoke_callable(&function_value, &argument_values, environment)).await
        }
    }
}

fn is_value_truthy(value: &RuntimeValue) -> bool {
    match value {
        RuntimeValue::Boolean(b) => *b,
        RuntimeValue::Integer(i) => *i != 0,
        RuntimeValue::Float(f) => *f != 0.0,
        RuntimeValue::String(s) => !s.is_empty(),
        RuntimeValue::Null => false,
        _ => true,
    }
}

pub fn format_value(value: &RuntimeValue) -> String {
    match value {
        RuntimeValue::Integer(number) => number.to_string(),
        RuntimeValue::Float(number) => {
            if number.fract() == 0.0 {
                format!("{:.1}", number)
            } else {
                number.to_string()
            }
        },
        RuntimeValue::String(text) => text.clone(),
        RuntimeValue::Boolean(b) => b.to_string(),
        RuntimeValue::Function(_) => "<function>".into(),
        RuntimeValue::Module(module) => format!("<module:{}>", module.module_name),
        RuntimeValue::NativeModule(module) => format!("<native_module:{}>", module.module_name),
        RuntimeValue::Array(arr) => {
            let arr_lock = arr.lock().unwrap();
            let items: Vec<String> = arr_lock.iter().map(format_value).collect();
            format!("[{}]", items.join(", "))
        }
        RuntimeValue::HashMap(map) => {
            let map_lock = map.lock().unwrap();
            let items: Vec<String> = map_lock.iter()
                .map(|(k, v)| format!("{}: {}", k, format_value(v)))
                .collect();
            format!("{{{}}}", items.join(", "))
        }
        RuntimeValue::Null => "null".into(),
    }
}

async fn invoke_callable(
    callable: &RuntimeValue,
    arguments: &[RuntimeValue],
    environment: &mut ExecutionEnvironment,
) -> Result<RuntimeValue, String> {
    if let RuntimeValue::String(token) = callable {
        if token.starts_with("__native__::") {
            return invoke_native_function(token, arguments, environment).await;
        }
    }

    match callable {
        RuntimeValue::Function(function_definition) => {
            if arguments.len() != function_definition.parameters.len() {
                return Err(format!(
                    "Function expects {} arguments but got {}",
                    function_definition.parameters.len(),
                    arguments.len()
                ));
            }

            let mut local_environment = ExecutionEnvironment::new();
            for (key, value) in environment.variables.iter() {
                local_environment.variables.insert(key.clone(), value.clone());
            }

            for (parameter_name, argument_value) in function_definition
                .parameters
                .iter()
                .zip(arguments.iter())
            {
                local_environment.define(parameter_name.clone(), argument_value.clone());
            }

            match Box::pin(run_program(&function_definition.body, &mut local_environment)).await? {
                ExecSignal::Return(value) => Ok(value),
                ExecSignal::None => Ok(RuntimeValue::Null),
                _ => Ok(RuntimeValue::Null),
            }
        }
        _ => Err("Cannot invoke a non-function value".into()),
    }
}

fn convert_to_string(value: &RuntimeValue) -> Result<String, String> {
    match value {
        RuntimeValue::String(text) => Ok(text.clone()),
        RuntimeValue::Integer(number) => Ok(number.to_string()),
        RuntimeValue::Float(number) => Ok(number.to_string()),
        RuntimeValue::Boolean(b) => Ok(b.to_string()),
        _ => Ok(format_value(value)),
    }
}

fn perform_binary_operation(
    left: &RuntimeValue,
    operator: BinaryOp,
    right: &RuntimeValue,
) -> Result<RuntimeValue, String> {
    match (left, right) {
        (RuntimeValue::Integer(x), RuntimeValue::Integer(y)) => {
            Ok(RuntimeValue::Integer(match operator {
                BinaryOp::Add => x + y,
                BinaryOp::Subtract => x - y,
                BinaryOp::Multiply => x * y,
                BinaryOp::Divide => {
                    if *y == 0 {
                        return Err("Division by zero is not allowed".into());
                    }
                    x / y
                }
                BinaryOp::Modulo => {
                    if *y == 0 {
                        return Err("Modulo by zero is not allowed".into());
                    }
                    x % y
                }
                BinaryOp::Power => {
                    if *y < 0 {
                        return Ok(RuntimeValue::Float((*x as f64).powf(*y as f64)));
                    }
                    let mut result = 1i64;
                    let mut base = *x;
                    let mut exp = *y;
                    while exp > 0 {
                        if exp % 2 == 1 {
                            result = result.saturating_mul(base);
                        }
                        base = base.saturating_mul(base);
                        exp /= 2;
                    }
                    result
                }
            }))
        }
        (RuntimeValue::Float(x), RuntimeValue::Float(y)) => {
            Ok(RuntimeValue::Float(match operator {
                BinaryOp::Add => x + y,
                BinaryOp::Subtract => x - y,
                BinaryOp::Multiply => x * y,
                BinaryOp::Divide => {
                    if *y == 0.0 {
                        return Err("Division by zero is not allowed".into());
                    }
                    x / y
                }
                BinaryOp::Modulo => x % y,
                BinaryOp::Power => x.powf(*y),
            }))
        }
        (RuntimeValue::Integer(x), RuntimeValue::Float(y)) => {
            let x = *x as f64;
            Ok(RuntimeValue::Float(match operator {
                BinaryOp::Add => x + y,
                BinaryOp::Subtract => x - y,
                BinaryOp::Multiply => x * y,
                BinaryOp::Divide => {
                    if *y == 0.0 {
                        return Err("Division by zero is not allowed".into());
                    }
                    x / y
                }
                BinaryOp::Modulo => x % y,
                BinaryOp::Power => x.powf(*y),
            }))
        }
        (RuntimeValue::Float(x), RuntimeValue::Integer(y)) => {
            let y = *y as f64;
            Ok(RuntimeValue::Float(match operator {
                BinaryOp::Add => x + y,
                BinaryOp::Subtract => x - y,
                BinaryOp::Multiply => x * y,
                BinaryOp::Divide => {
                    if y == 0.0 {
                        return Err("Division by zero is not allowed".into());
                    }
                    x / y
                }
                BinaryOp::Modulo => x % y,
                BinaryOp::Power => x.powf(y),
            }))
        }
        (RuntimeValue::String(x), RuntimeValue::String(y)) if matches!(operator, BinaryOp::Add) => {
            Ok(RuntimeValue::String(format!("{x}{y}")))
        }
        (RuntimeValue::String(x), other) if matches!(operator, BinaryOp::Add) => {
            Ok(RuntimeValue::String(format!("{x}{}", format_value(other))))
        }
        (other, RuntimeValue::String(y)) if matches!(operator, BinaryOp::Add) => {
            Ok(RuntimeValue::String(format!("{}{y}", format_value(other))))
        }
        (RuntimeValue::String(x), RuntimeValue::Integer(y)) if matches!(operator, BinaryOp::Multiply) => {
            if *y < 0 {
                return Err("Cannot multiply string by negative number".into());
            }
            Ok(RuntimeValue::String(x.repeat(*y as usize)))
        }
        (RuntimeValue::Integer(x), RuntimeValue::String(y)) if matches!(operator, BinaryOp::Multiply) => {
            if *x < 0 {
                return Err("Cannot multiply string by negative number".into());
            }
            Ok(RuntimeValue::String(y.repeat(*x as usize)))
        }
        _ => Err("Type mismatch in binary operation".into()),
    }
}

fn perform_comparison_operation(
    left: &RuntimeValue,
    operator: ComparisonOp,
    right: &RuntimeValue,
) -> Result<RuntimeValue, String> {
    let result = match (left, right) {
        (RuntimeValue::Integer(x), RuntimeValue::Integer(y)) => {
            match operator {
                ComparisonOp::Equal => x == y,
                ComparisonOp::NotEqual => x != y,
                ComparisonOp::LessThan => x < y,
                ComparisonOp::LessThanOrEqual => x <= y,
                ComparisonOp::GreaterThan => x > y,
                ComparisonOp::GreaterThanOrEqual => x >= y,
            }
        }
        (RuntimeValue::Float(x), RuntimeValue::Float(y)) => {
            match operator {
                ComparisonOp::Equal => (x - y).abs() < f64::EPSILON,
                ComparisonOp::NotEqual => (x - y).abs() >= f64::EPSILON,
                ComparisonOp::LessThan => x < y,
                ComparisonOp::LessThanOrEqual => x <= y,
                ComparisonOp::GreaterThan => x > y,
                ComparisonOp::GreaterThanOrEqual => x >= y,
            }
        }
        (RuntimeValue::Integer(x), RuntimeValue::Float(y)) => {
            let x = *x as f64;
            match operator {
                ComparisonOp::Equal => (x - y).abs() < f64::EPSILON,
                ComparisonOp::NotEqual => (x - y).abs() >= f64::EPSILON,
                ComparisonOp::LessThan => x < *y,
                ComparisonOp::LessThanOrEqual => x <= *y,
                ComparisonOp::GreaterThan => x > *y,
                ComparisonOp::GreaterThanOrEqual => x >= *y,
            }
        }
        (RuntimeValue::Float(x), RuntimeValue::Integer(y)) => {
            let y = *y as f64;
            match operator {
                ComparisonOp::Equal => (x - y).abs() < f64::EPSILON,
                ComparisonOp::NotEqual => (x - y).abs() >= f64::EPSILON,
                ComparisonOp::LessThan => *x < y,
                ComparisonOp::LessThanOrEqual => *x <= y,
                ComparisonOp::GreaterThan => *x > y,
                ComparisonOp::GreaterThanOrEqual => *x >= y,
            }
        }
        (RuntimeValue::String(x), RuntimeValue::String(y)) => {
            match operator {
                ComparisonOp::Equal => x == y,
                ComparisonOp::NotEqual => x != y,
                ComparisonOp::LessThan => x < y,
                ComparisonOp::LessThanOrEqual => x <= y,
                ComparisonOp::GreaterThan => x > y,
                ComparisonOp::GreaterThanOrEqual => x >= y,
            }
        }
        (RuntimeValue::Boolean(x), RuntimeValue::Boolean(y)) => {
            match operator {
                ComparisonOp::Equal => x == y,
                ComparisonOp::NotEqual => x != y,
                _ => return Err("Cannot compare booleans with < or >".into())
            }
        }
        _ => {
            match operator {
                ComparisonOp::Equal => left == right,
                ComparisonOp::NotEqual => left != right,
                _ => return Err("Type mismatch in comparison".into())
            }
        }
    };
    Ok(RuntimeValue::Boolean(result))
}

async fn perform_http_get(url: &str) -> Result<String, String> {
    let response = reqwest::get(url).await.map_err(|e| e.to_string())?;
    let response_body = response.text().await.map_err(|e| e.to_string())?;
    Ok(response_body)
}

async fn perform_http_post(url: &str, body: &str) -> Result<String, String> {
    let client = reqwest::Client::new();
    let response = client
        .post(url)
        .body(body.to_string())
        .send()
        .await
        .map_err(|e| e.to_string())?;
    let response_text = response.text().await.map_err(|e| e.to_string())?;
    Ok(response_text)
}

async fn perform_http_put(url: &str, body: &str) -> Result<String, String> {
    let client = reqwest::Client::new();
    let response = client
        .put(url)
        .body(body.to_string())
        .send()
        .await
        .map_err(|e| e.to_string())?;
    let response_text = response.text().await.map_err(|e| e.to_string())?;
    Ok(response_text)
}

async fn perform_http_delete(url: &str) -> Result<String, String> {
    let client = reqwest::Client::new();
    let response = client
        .delete(url)
        .send()
        .await
        .map_err(|e| e.to_string())?;
    let response_text = response.text().await.map_err(|e| e.to_string())?;
    Ok(response_text)
}