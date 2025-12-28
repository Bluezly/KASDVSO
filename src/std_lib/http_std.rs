pub fn get_http_std_code() -> &'static str {
    r#"
fn get(url) {
    return net_get(url)
}

fn post(url, body) {
    return net_post(url, body)
}

fn put(url, body) {
    return net_put(url, body)
}

fn delete(url) {
    return net_delete(url)
}

fn getJson(url) {
    let response = get(url)
    return json_parse(response)
}

fn postJson(url, data) {
    let body = json_stringify(data)
    return post(url, body)
}

fn download(url, filepath) {
    let content = get(url)
    return fs_write(filepath, content)
}

fn upload(url, filepath) {
    let content = fs_read(filepath)
    return post(url, content)
}
"#
}