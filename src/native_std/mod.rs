pub mod ws;

use crate::types::NativeModuleInstance;

pub fn get_builtin_native_module(name: &str) -> Option<NativeModuleInstance> {
    match name {
        "ws"   => Some(ws::create_ws_module()),
        _ => None,
    }
}
