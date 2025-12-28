pub fn get_string_std_code() -> &'static str {
    r#"
fn upper(s) {
    return str_upper(s)
}

fn lower(s) {
    return str_lower(s)
}

fn split(s, delimiter) {
    return str_split(s, delimiter)
}

fn join(arr, separator) {
    return str_join(arr, separator)
}

fn trim(s) {
    return str_trim(s)
}

fn replace(s, old, new) {
    return str_replace(s, old, new)
}

fn charAt(s, index) {
    return str_char_at(s, index)
}

fn indexOf(s, substr) {
    return str_index_of(s, substr)
}

fn substring(s, start, end) {
    return str_substring(s, start, end)
}

fn startsWith(s, prefix) {
    return str_starts_with(s, prefix)
}

fn endsWith(s, suffix) {
    return str_ends_with(s, suffix)
}

fn repeat(s, count) {
    let result = ""
    let i = 0
    while i < count {
        result = result + s
        i = i + 1
    }
    return result
}

fn reverse(s) {
    return str_reverse(s)
}

fn isEmpty(s) {
    return len(s) == 0
}

fn contains(s, substr) {
    return indexOf(s, substr) >= 0
}
"#
}