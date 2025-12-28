pub fn get_json_std_code() -> &'static str {
    r#"
fn parse(json_str) {
    return json_parse(json_str)
}

fn stringify(obj) {
    return json_stringify(obj)
}

fn prettyStringify(obj, indent) {
    return json_pretty_stringify(obj, indent)
}

fn isValid(json_str) {
    return json_is_valid(json_str)
}

fn get(obj, key) {
    return json_get(obj, key)
}

fn set(obj, key, value) {
    return json_set(obj, key, value)
}

fn keys(obj) {
    return json_keys(obj)
}

fn values(obj) {
    return json_values(obj)
}

fn merge(obj1, obj2) {
    return json_merge(obj1, obj2)
}

fn clone(obj) {
    return parse(stringify(obj))
}
"#
}