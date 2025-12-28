use kasdvso::{execute_script, package_manager::*, display_help, show_error};
use std::env;
use std::path::Path;

#[cfg(windows)]
fn set_utf8_console() {
    use windows_sys::Win32::System::Console::SetConsoleOutputCP;
    unsafe {
        SetConsoleOutputCP(65001); 
    }
}

#[tokio::main]
async fn main() {
    #[cfg(target_os = "windows")]
    set_utf8_console();

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        display_help();
        return;
    }

    let command = args[1].as_str();
    let execution_result: Result<(), String> = match command {
        "run" => {
            if args.len() < 3 {
                show_error("Usage: kas run <file.kas>");
                return;
            }
            execute_script(Path::new(&args[2])).await
        }
        "add" => {
            if args.len() < 3 {
                show_error("Usage: kas add <path_to_package_directory>");
                return;
            }
            add_package(Path::new(&args[2]))
        }
        "pack" => {
            if args.len() < 4 {
                show_error("Usage: kas pack <path_to_package_directory> <output.kaspkg>");
                return;
            }
            pack_module(Path::new(&args[2]), Path::new(&args[3]))
        }
        "install" => {
            if args.len() < 3 {
                show_error("Usage: kas install <file.kaspkg>");
                return;
            }
            install_package(Path::new(&args[2]))
        }
        "cache" => show_cache_location(),
        "init-std" => init_standard_library(),
        _ => {
            show_error("Unknown command. Available: run, add, pack, install, cache, init-std");
            Ok(())
        }
    };

    if let Err(error_message) = execution_result {
        eprintln!("[KASDVSO Error] {error_message}");
        std::process::exit(1);
    }
}