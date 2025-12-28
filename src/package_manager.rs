use crate::types::*;
use crate::std_lib::*;
use std::{
    collections::HashMap,
    env,
    fs,
    io::{Read, Write},
    path::{Path, PathBuf},
};

pub fn get_cache_directory() -> Result<PathBuf, String> {
    let mut cache_path = env::current_dir().map_err(|e| e.to_string())?;
    cache_path.push(".kasdvso_cache");
    fs::create_dir_all(&cache_path).map_err(|e| e.to_string())?;
    Ok(cache_path)
}

pub fn get_packages_directory() -> Result<PathBuf, String> {
    let mut packages_path = get_cache_directory()?;
    packages_path.push("pkgs");
    fs::create_dir_all(&packages_path).map_err(|e| e.to_string())?;
    Ok(packages_path)
}

pub fn get_std_directory() -> Result<PathBuf, String> {
    let mut std_path = get_cache_directory()?;
    std_path.push("std");
    fs::create_dir_all(&std_path).map_err(|e| e.to_string())?;
    Ok(std_path)
}

pub fn show_cache_location() -> Result<(), String> {
    println!("{}", get_cache_directory()?.display());
    Ok(())
}

pub fn init_standard_library() -> Result<(), String> {
    let std_path = get_std_directory()?;
    
    let std_packages = vec![
        ("array", "1.0.0"),
        ("string", "1.0.0"),
        ("math", "1.0.0"),
        ("fs", "1.0.0"),
        ("json", "1.0.0"),
        ("http", "1.0.0"),
        ("time", "1.0.0"),
    ];

    for (name, version) in std_packages {
        create_std_package(&std_path, name, version)?;
    }

    println!("[KASDVSO] Standard library initialized at {}", std_path.display());
    Ok(())
}

fn create_std_package(std_path: &Path, name: &str, version: &str) -> Result<(), String> {
    let pkg_path = std_path.join(name).join(version);
    fs::create_dir_all(&pkg_path).map_err(|e| e.to_string())?;
    
    let manifest = format!("name={}\nversion={}\nentry=main.kas\ntype=kas\n", name, version);
    fs::write(pkg_path.join("kas.man"), manifest).map_err(|e| e.to_string())?;
    
    let code = match name {
        "array" => get_array_std_code(),
        "string" => get_string_std_code(),
        "math" => get_math_std_code(),
        "fs" => get_fs_std_code(),
        "json" => get_json_std_code(),
        "http" => get_http_std_code(),
        "time" => get_time_std_code(),
        _ => return Err(format!("Unknown standard package: {}", name)),
    };
    
    fs::write(pkg_path.join("main.kas"), code).map_err(|e| e.to_string())?;
    Ok(())
}

pub fn parse_manifest_file(package_directory: &Path) -> Result<PackageManifest, String> {
    let manifest_path = package_directory.join("kas.man");
    let manifest_content = fs::read_to_string(&manifest_path)
        .map_err(|e| format!("Failed to read kas.man: {e} ({})", manifest_path.display()))?;

    let mut properties = HashMap::<String, String>::new();
    for line in manifest_content.lines() {
        let trimmed_line = line.trim();
        if trimmed_line.is_empty() || trimmed_line.starts_with('#') {
            continue;
        }
        let Some((key, value)) = trimmed_line.split_once('=') else {
            continue;
        };
        properties.insert(key.trim().to_string(), value.trim().to_string());
    }

    let package_name = properties
        .get("name")
        .cloned()
        .ok_or("Package name not found in kas.man")?;
    let version_string = properties
        .get("version")
        .cloned()
        .unwrap_or_else(|| "0.0.0".to_string());
    let entry_point = properties
        .get("entry")
        .cloned()
        .unwrap_or_else(|| "src/main.kas".to_string());
    let package_type = properties
        .get("type")
        .cloned()
        .unwrap_or_else(|| "kas".to_string());
    let native_library = properties.get("native").cloned().unwrap_or_default();

    let mut dependencies = Vec::new();
    let deps_path = package_directory.join("kas.deps");
    if deps_path.exists() {
        let deps_content = fs::read_to_string(&deps_path).map_err(|e| e.to_string())?;
        for line in deps_content.lines() {
            let trimmed = line.trim();
            if trimmed.is_empty() || trimmed.starts_with('#') {
                continue;
            }
            let (name, constraint) = parse_import_spec(trimmed);
            dependencies.push(Dependency {
                name,
                version_constraint: constraint,
            });
        }
    }

    Ok(PackageManifest {
        package_name,
        version_string,
        entry_point,
        package_type,
        native_library,
        dependencies,
    })
}

pub fn get_installation_path(manifest: &PackageManifest) -> Result<PathBuf, String> {
    let mut installation_path = get_packages_directory()?;
    installation_path.push(&manifest.package_name);
    installation_path.push(&manifest.version_string);
    Ok(installation_path)
}

pub fn copy_directory_recursive(source: &Path, destination: &Path) -> Result<(), String> {
    fs::create_dir_all(destination).map_err(|e| e.to_string())?;
    for entry in fs::read_dir(source).map_err(|e| e.to_string())? {
        let entry = entry.map_err(|e| e.to_string())?;
        let file_type = entry.file_type().map_err(|e| e.to_string())?;
        let source_path = entry.path();
        let destination_path = destination.join(entry.file_name());

        if file_type.is_dir() {
            copy_directory_recursive(&source_path, &destination_path)?;
        } else if file_type.is_file() {
            fs::create_dir_all(destination_path.parent().unwrap()).map_err(|e| e.to_string())?;
            fs::copy(&source_path, &destination_path).map_err(|e| e.to_string())?;
        }
    }
    Ok(())
}

pub fn add_package(package_directory: &Path) -> Result<(), String> {
    if !package_directory.is_dir() {
        return Err("The specified path is not a directory".into());
    }

    let manifest = parse_manifest_file(package_directory)?;
    let install_destination = get_installation_path(&manifest)?;

    if install_destination.exists() {
        fs::remove_dir_all(&install_destination).map_err(|e| e.to_string())?;
    }

    copy_directory_recursive(package_directory, &install_destination)?;
    println!(
        "[KASDVSO] Successfully added package {}@{} to {}",
        manifest.package_name,
        manifest.version_string,
        install_destination.display()
    );
    Ok(())
}

pub fn pack_module(package_directory: &Path, output_file: &Path) -> Result<(), String> {
    if !package_directory.is_dir() {
        return Err("Package source is not a directory".into());
    }

    let _manifest = parse_manifest_file(package_directory)?;
    let mut collected_files: Vec<(String, Vec<u8>)> = Vec::new();
    gather_files(package_directory, package_directory, &mut collected_files)?;

    let mut output_stream = fs::File::create(output_file).map_err(|e| e.to_string())?;
    output_stream.write_all(b"KASPKG1\n").map_err(|e| e.to_string())?;
    write_uint32(&mut output_stream, collected_files.len() as u32)?;

    for (relative_path, file_content) in collected_files {
        write_uint32(&mut output_stream, relative_path.as_bytes().len() as u32)?;
        output_stream
            .write_all(relative_path.as_bytes())
            .map_err(|e| e.to_string())?;
        write_uint64(&mut output_stream, file_content.len() as u64)?;
        output_stream.write_all(&file_content).map_err(|e| e.to_string())?;
    }

    println!("[KASDVSO] Package created successfully: {}", output_file.display());
    Ok(())
}

pub fn install_package(package_file: &Path) -> Result<(), String> {
    let mut file_stream = fs::File::open(package_file).map_err(|e| e.to_string())?;
    let mut magic_bytes = [0u8; 8];
    file_stream.read_exact(&mut magic_bytes).map_err(|e| e.to_string())?;

    if &magic_bytes != b"KASPKG1\n" {
        return Err("Invalid package file format".into());
    }

    let file_count = read_uint32(&mut file_stream)? as usize;

    let mut temporary_directory = get_cache_directory()?;
    temporary_directory.push(format!("_tmp_{}", crate::get_current_timestamp()));
    fs::create_dir_all(&temporary_directory).map_err(|e| e.to_string())?;

    for _ in 0..file_count {
        let path_length = read_uint32(&mut file_stream)? as usize;
        let mut path_buffer = vec![0u8; path_length];
        file_stream.read_exact(&mut path_buffer).map_err(|e| e.to_string())?;
        let relative_path = String::from_utf8(path_buffer)
            .map_err(|_| "Package contains invalid UTF-8 path")?;

        let content_length = read_uint64(&mut file_stream)? as usize;
        let mut content_buffer = vec![0u8; content_length];
        file_stream.read_exact(&mut content_buffer).map_err(|e| e.to_string())?;

        let destination_file = temporary_directory.join(relative_path);
        if let Some(parent_dir) = destination_file.parent() {
            fs::create_dir_all(parent_dir).map_err(|e| e.to_string())?;
        }
        fs::write(&destination_file, content_buffer).map_err(|e| e.to_string())?;
    }

    let manifest = parse_manifest_file(&temporary_directory)?;
    let final_installation = get_installation_path(&manifest)?;

    if final_installation.exists() {
        fs::remove_dir_all(&final_installation).map_err(|e| e.to_string())?;
    }
    fs::create_dir_all(final_installation.parent().unwrap()).map_err(|e| e.to_string())?;

    fs::rename(&temporary_directory, &final_installation).or_else(|_| {
        copy_directory_recursive(&temporary_directory, &final_installation)?;
        fs::remove_dir_all(&temporary_directory).map_err(|e| e.to_string())?;
        Ok::<(), String>(())
    })?;

    println!(
        "[KASDVSO] Package installed: {}@{} at {}",
        manifest.package_name,
        manifest.version_string,
        final_installation.display()
    );
    Ok(())
}

fn gather_files(
    root_directory: &Path,
    current_directory: &Path,
    output: &mut Vec<(String, Vec<u8>)>,
) -> Result<(), String> {
    for entry in fs::read_dir(current_directory).map_err(|e| e.to_string())? {
        let entry = entry.map_err(|e| e.to_string())?;
        let entry_path = entry.path();
        let file_type = entry.file_type().map_err(|e| e.to_string())?;

        if file_type.is_dir() {
            gather_files(root_directory, &entry_path, output)?;
        } else if file_type.is_file() {
            let relative_path = entry_path
                .strip_prefix(root_directory)
                .map_err(|e| e.to_string())?
                .to_string_lossy()
                .to_string();
            let mut file_buffer = Vec::new();
            fs::File::open(&entry_path)
                .map_err(|e| e.to_string())?
                .read_to_end(&mut file_buffer)
                .map_err(|e| e.to_string())?;
            output.push((relative_path, file_buffer));
        }
    }
    Ok(())
}

fn write_uint32<W: Write>(writer: &mut W, value: u32) -> Result<(), String> {
    writer.write_all(&value.to_le_bytes()).map_err(|e| e.to_string())
}

fn write_uint64<W: Write>(writer: &mut W, value: u64) -> Result<(), String> {
    writer.write_all(&value.to_le_bytes()).map_err(|e| e.to_string())
}

fn read_uint32<R: Read>(reader: &mut R) -> Result<u32, String> {
    let mut buffer = [0u8; 4];
    reader.read_exact(&mut buffer).map_err(|e| e.to_string())?;
    Ok(u32::from_le_bytes(buffer))
}

fn read_uint64<R: Read>(reader: &mut R) -> Result<u64, String> {
    let mut buffer = [0u8; 8];
    reader.read_exact(&mut buffer).map_err(|e| e.to_string())?;
    Ok(u64::from_le_bytes(buffer))
}

pub fn parse_version(version_string: &str) -> Option<Version> {
    let components: Vec<&str> = version_string.split('.').collect();
    let major = components.get(0)?.parse().ok()?;
    let minor = components.get(1).unwrap_or(&"0").parse().ok()?;
    let patch = components.get(2).unwrap_or(&"0").parse().ok()?;
    Some(Version { major, minor, patch })
}

pub fn satisfies_constraint(version: Version, constraint: &str) -> bool {
    if constraint.is_empty() {
        return true;
    }
    
    if constraint.starts_with('^') {
        let base_version = parse_version(constraint.trim_start_matches('^'));
        if let Some(base) = base_version {
            let upper_bound = if base.major > 0 {
                Version {
                    major: base.major + 1,
                    minor: 0,
                    patch: 0,
                }
            } else {
                Version {
                    major: 0,
                    minor: base.minor + 1,
                    patch: 0,
                }
            };
            return version >= base && version < upper_bound;
        }
    } else if constraint.starts_with('~') {
        let base_version = parse_version(constraint.trim_start_matches('~'));
        if let Some(base) = base_version {
            let upper_bound = Version {
                major: base.major,
                minor: base.minor + 1,
                patch: 0,
            };
            return version >= base && version < upper_bound;
        }
    } else {
        let exact_version = parse_version(constraint);
        if let Some(exact) = exact_version {
            return version == exact;
        }
    }
    
    false
}

pub fn resolve_package_version(
    package_name: &str,
    version_constraint: &str,
) -> Result<(PathBuf, PackageManifest), String> {
    let std_path = get_std_directory()?;
    let std_package = std_path.join(package_name);
    if std_package.exists() {
        let mut available_versions: Vec<(Version, PathBuf)> = vec![];
        for entry in fs::read_dir(&std_package).map_err(|e| e.to_string())? {
            let version_path = entry.map_err(|e| e.to_string())?.path();
            if !version_path.is_dir() {
                continue;
            }
            let version_string = version_path.file_name().unwrap().to_string_lossy().to_string();
            if let Some(parsed_version) = parse_version(&version_string) {
                if satisfies_constraint(parsed_version, version_constraint) {
                    available_versions.push((parsed_version, version_path));
                }
            }
        }
        if !available_versions.is_empty() {
            available_versions.sort_by_key(|(version, _)| *version);
            let (_, latest_path) = available_versions.last().unwrap().clone();
            let manifest = parse_manifest_file(&latest_path)?;
            return Ok((latest_path, manifest));
        }
    }

    let packages_root = get_packages_directory()?;
    let package_directory = packages_root.join(package_name);

    if !package_directory.exists() {
        return Err(format!(
            "Package '{}' not found. Please install it using 'kas add' or 'kas install'",
            package_name
        ));
    }

    let mut available_versions: Vec<(Version, PathBuf)> = vec![];
    for entry in fs::read_dir(&package_directory).map_err(|e| e.to_string())? {
        let version_path = entry.map_err(|e| e.to_string())?.path();
        if !version_path.is_dir() {
            continue;
        }
        let version_string = version_path.file_name().unwrap().to_string_lossy().to_string();
        if let Some(parsed_version) = parse_version(&version_string) {
            if satisfies_constraint(parsed_version, version_constraint) {
                available_versions.push((parsed_version, version_path));
            }
        }
    }

    if available_versions.is_empty() {
        return Err(format!(
            "No compatible versions found for package '{}' with constraint '{}'",
            package_name, version_constraint
        ));
    }

    available_versions.sort_by_key(|(version, _)| *version);
    let (_, latest_path) = available_versions.last().unwrap().clone();
    let manifest = parse_manifest_file(&latest_path)?;
    Ok((latest_path, manifest))
}

pub fn parse_import_spec(specification: &str) -> (String, String) {
    if let Some((package_part, version_part)) = specification.split_once('@') {
        (package_part.trim().to_string(), version_part.trim().to_string())
    } else {
        (specification.trim().to_string(), "".into())
    }
}