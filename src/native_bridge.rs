use crate::types::*;
use libloading::{Library, Symbol};
use std::sync::Arc;
use std::collections::HashMap;
use std::path::Path;

#[no_mangle]
pub extern "C" fn kas_alloc(size: usize) -> *mut u8 {
    let layout = std::alloc::Layout::from_size_align(size, 1).unwrap();
    unsafe { std::alloc::alloc(layout) }
}

#[no_mangle]
pub extern "C" fn kas_free(ptr: *mut u8, size: usize) {
    if !ptr.is_null() {
        let layout = std::alloc::Layout::from_size_align(size, 1).unwrap();
        unsafe { std::alloc::dealloc(ptr, layout) }
    }
}

pub fn load_native_module(
    module_name: &str,
    package_directory: &Path,
    manifest: &PackageManifest,
) -> Result<NativeModuleInstance, String> {
    let library_path = package_directory.join(&manifest.native_library);

    if manifest.native_library.is_empty() {
        return Err("Rust package requires 'native' field in kas.man".into());
    }

    if !library_path.exists() {
        return Err(format!("Native library not found: {}", library_path.display()));
    }

    unsafe {
        let library = Library::new(&library_path).map_err(|e| e.to_string())?;
        let library_handle = Arc::new(library);

        let initialization_function: Symbol<unsafe extern "C" fn() -> *const NativeModuleApi> = library_handle
            .get(b"kas_module_init")
            .map_err(|e| format!("Function 'kas_module_init' not found: {e}"))?;

        let api_pointer = initialization_function();
        if api_pointer.is_null() {
            return Err("Module initialization returned null".into());
        }

        let api_structure = &*api_pointer;
        let mut exported_functions = HashMap::new();
        let functions_slice = std::slice::from_raw_parts(
            api_structure.functions_pointer,
            api_structure.function_count
        );

        for function_definition in functions_slice {
            let name_bytes = std::slice::from_raw_parts(
                function_definition.name_pointer,
                function_definition.name_length
            );
            let function_name = String::from_utf8_lossy(name_bytes).to_string();
            exported_functions.insert(
                function_name,
                NativeFunction {
                    name_pointer: function_definition.name_pointer,
                    name_length: function_definition.name_length,
                    function_pointer: function_definition.function_pointer,
                },
            );
        }

        Ok(NativeModuleInstance {
            module_name: module_name.to_string(),
            library_handle,
            exported_functions,
        })
    }
}

pub async fn invoke_native_function(
    function_token: &str,
    arguments: &[RuntimeValue],
    environment: &mut ExecutionEnvironment,
) -> Result<RuntimeValue, String> {
let token_parts: Vec<&str> = function_token.split("::").collect();

if token_parts.len() != 3 || token_parts[0] != "__native__" {
    return Err(format!("Invalid native function token format: {}", function_token));
}

let module_name = token_parts[1];
let function_name = token_parts[2];

    let native_module = match environment.lookup(module_name) {
        Some(RuntimeValue::NativeModule(module)) => module,
        _ => return Err("Native module not found in environment".into()),
    };

    let function_definition = native_module
        .exported_functions
        .get(function_name)
        .ok_or("Native function not found in module")?;

    let mut native_arguments: Vec<NativeArgument> = Vec::new();
    let mut string_storage: Vec<Vec<u8>> = Vec::new();

    for argument in arguments {
        match argument {
            RuntimeValue::Integer(integer_value) => {
                native_arguments.push(NativeArgument {
                    argument_type: 0,
                    integer_value: *integer_value,
                    string_pointer: std::ptr::null(),
                    string_length: 0,
                });
            }
            RuntimeValue::String(string_value) => {
                string_storage.push(string_value.as_bytes().to_vec());
                let stored_bytes = string_storage.last().unwrap();
                native_arguments.push(NativeArgument {
                    argument_type: 1,
                    integer_value: 0,
                    string_pointer: stored_bytes.as_ptr(),
                    string_length: stored_bytes.len(),
                });
            }
            RuntimeValue::Float(float_value) => {
                native_arguments.push(NativeArgument {
                    argument_type: 2,
                    integer_value: float_value.to_bits() as i64,
                    string_pointer: std::ptr::null(),
                    string_length: 0,
                });
            }
            _ => return Err("Native functions only accept integer, float, or string arguments".into()),
        }
    }

    let return_value = (function_definition.function_pointer)(
        native_arguments.len(),
        native_arguments.as_ptr()
    );

    unsafe {
        match return_value.return_type {
            0 => Ok(RuntimeValue::Integer(return_value.integer_value)),
            1 => {
                if return_value.string_pointer.is_null() {
                    return Ok(RuntimeValue::String(String::new()));
                }
                let string_bytes = std::slice::from_raw_parts(
                    return_value.string_pointer,
                    return_value.string_length
                );
                Ok(RuntimeValue::String(
                    String::from_utf8_lossy(string_bytes).to_string()
                ))
            }
            2 => {
                let float_value = f64::from_bits(return_value.integer_value as u64);
                Ok(RuntimeValue::Float(float_value))
            }
            _ => Err("Invalid return type from native function".into()),
        }
    }
}