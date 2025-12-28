pub fn get_array_std_code() -> &'static str {
    r#"
fn filter(arr, predicate) {
    let result = array()
    let i = 0
    while i < arr.len() {
        let item = arr.get(i)
        if predicate(item) {
            result.push(item)
        }
        i = i + 1
    }
    return result
}

fn map(arr, mapper) {
    let result = array()
    let i = 0
    while i < arr.len() {
        result.push(mapper(arr.get(i)))
        i = i + 1
    }
    return result
}

fn reduce(arr, reducer, initial) {
    let acc = initial
    let i = 0
    while i < arr.len() {
        acc = reducer(acc, arr.get(i))
        i = i + 1
    }
    return acc
}

fn contains(arr, value) {
    let i = 0
    while i < arr.len() {
        if arr.get(i) == value {
            return true
        }
        i = i + 1
    }
    return false
}

fn reverse(arr) {
    let result = array()
    let i = arr.len() - 1
    while i >= 0 {
        result.push(arr.get(i))
        i = i - 1
    }
    return result
}

fn sort(arr) {
    let len = arr.len()
    let i = 0
    while i < len {
        let j = i + 1
        while j < len {
            if arr.get(i) > arr.get(j) {
                let temp = arr.get(i)
                arr.set(i, arr.get(j))
                arr.set(j, temp)
            }
            j = j + 1
        }
        i = i + 1
    }
    return arr
}

fn find(arr, predicate) {
    let i = 0
    while i < arr.len() {
        let item = arr.get(i)
        if predicate(item) {
            return item
        }
        i = i + 1
    }
    return null
}

fn indexOf(arr, value) {
    let i = 0
    while i < arr.len() {
        if arr.get(i) == value {
            return i
        }
        i = i + 1
    }
    return -1
}

fn slice(arr, start, end) {
    let result = array()
    let i = start
    while i < end && i < arr.len() {
        result.push(arr.get(i))
        i = i + 1
    }
    return result
}

fn concat(arr1, arr2) {
    let result = array()
    let i = 0
    while i < arr1.len() {
        result.push(arr1.get(i))
        i = i + 1
    }
    let j = 0
    while j < arr2.len() {
        result.push(arr2.get(j))
        j = j + 1
    }
    return result
}
"#
}