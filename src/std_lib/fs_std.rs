pub fn get_fs_std_code() -> &'static str {
    r#"
fn read(path) {
    return fs_read(path)
}

fn write(path, content) {
    return fs_write(path, content)
}

fn exists(path) {
    return fs_exists(path)
}

fn listDir(path) {
    return fs_list_dir(path)
}

fn createDir(path) {
    return fs_create_dir(path)
}

fn deleteFile(path) {
    return fs_delete_file(path)
}

fn deleteDir(path) {
    return fs_delete_dir(path)
}

fn copy(src, dest) {
    return fs_copy(src, dest)
}

fn move(src, dest) {
    return fs_move(src, dest)
}

fn isFile(path) {
    return fs_is_file(path)
}

fn isDir(path) {
    return fs_is_dir(path)
}

fn size(path) {
    return fs_size(path)
}

fn modified(path) {
    return fs_modified(path)
}

fn extension(path) {
    return fs_extension(path)
}

fn basename(path) {
    return fs_basename(path)
}

fn dirname(path) {
    return fs_dirname(path)
}
"#
}