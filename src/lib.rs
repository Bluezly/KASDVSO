pub mod types;
pub mod parser;
pub mod runtime;
pub mod package_manager;
pub mod native_bridge;
pub mod std_lib;
pub mod native_std;

pub use runtime::execute_script;
pub use types::*;
pub use package_manager::init_standard_library;

use std::time::{SystemTime, UNIX_EPOCH};

pub fn display_help() {
    println!(
        "{}",
        r#"
KASDVSO Language Runtime v2.1
A modern async scripting environment with offline package management, 
HTTPS support, version constraints, and native Rust module integration.

Available Commands:
  kas run <script.kas>          Execute a KAS script file (async)
  kas add <package_path>        Add a package from local directory to cache
  kas pack <pkg_dir> <out.pkg>  Create a distributable package file
  kas install <package.kaspkg>  Install a package from .kaspkg file
  kas cache                     Show cache directory location
  kas init-std                  Initialize standard library

Package Structure:
  example_package/
    kas.man              Package manifest with metadata
    src/main.kas         Main entry point
    kas.deps             Dependencies file (optional)

Language Syntax Examples:
  let result = 10
  print result
  
  fn calculate_sum(x, y) { 
    return x + y 
  }
  
  if x > 5 {
    print "greater"
  } else {
    print "less or equal"
  }
  
  while x < 10 {
    x = x + 1
  }
  
  for i in range(0, 10) {
    print i
    if i == 5 {
      break
    }
  }
  
  import math_lib@^1.2
  print math_lib.add(5, 3)
  
  let arr = array()
  arr.push(10)
  arr.push(20)
  print arr.len()
  print arr.get(0)
  
  let map = hashmap()
  map.set("key", "value")
  print map.get("key")
  print map.keys()

Built-in Functions:
  print, println
  net_get, net_post (async)
  array, hashmap
  range, len
  type_of, str, int, float
  exit, assert
  time_now, sleep
"#
    );
}

pub fn show_error(message: &str) {
    eprintln!("{message}");
}

pub fn get_current_timestamp() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis()
}