pub fn get_math_std_code() -> &'static str {
    r#"
fn abs(x) {
    if x < 0 {
        return 0 - x
    }
    return x
}

fn min(a, b) {
    if a < b {
        return a
    }
    return b
}

fn max(a, b) {
    if a > b {
        return a
    }
    return b
}

fn pow(base, exp) {
    let result = 1.0
    let i = 0
    while i < exp {
        result = result * base
        i = i + 1
    }
    return result
}

fn sqrt(x) {
    return math_sqrt(x)
}

fn sin(x) {
    return math_sin(x)
}

fn cos(x) {
    return math_cos(x)
}

fn tan(x) {
    return math_tan(x)
}

fn floor(x) {
    return math_floor(x)
}

fn ceil(x) {
    return math_ceil(x)
}

fn round(x) {
    return math_round(x)
}

fn random() {
    return math_random()
}

fn pi() {
    return 3.141592653589793
}

fn e() {
    return 2.718281828459045
}

fn clamp(value, min_val, max_val) {
    return min(max(value, min_val), max_val)
}

fn lerp(start, end, t) {
    return start + (end - start) * t
}

fn sign(x) {
    if x > 0 {
        return 1
    } else if x < 0 {
        return -1
    } else {
        return 0
    }
}

fn isEven(x) {
    return x % 2 == 0
}

fn isOdd(x) {
    return x % 2 != 0
}
"#
}