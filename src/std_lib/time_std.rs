pub fn get_time_std_code() -> &'static str {
    r#"
fn now() {
    return time_now()
}

fn sleep(ms) {
    return time_sleep(ms)
}

fn format(timestamp, pattern) {
    return time_format(timestamp, pattern)
}

fn parse(time_str, pattern) {
    return time_parse(time_str, pattern)
}

fn year(timestamp) {
    return time_year(timestamp)
}

fn month(timestamp) {
    return time_month(timestamp)
}

fn day(timestamp) {
    return time_day(timestamp)
}

fn hour(timestamp) {
    return time_hour(timestamp)
}

fn minute(timestamp) {
    return time_minute(timestamp)
}

fn second(timestamp) {
    return time_second(timestamp)
}

fn add(timestamp, amount, unit) {
    return time_add(timestamp, amount, unit)
}

fn diff(time1, time2, unit) {
    return time_diff(time1, time2, unit)
}

fn isLeapYear(year) {
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
}

fn daysInMonth(year, month) {
    if month == 2 {
        if isLeapYear(year) {
            return 29
        } else {
            return 28
        }
    } else if month == 4 || month == 6 || month == 9 || month == 11 {
        return 30
    } else {
        return 31
    }
}
"#
}