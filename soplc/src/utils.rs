#[macro_export]
macro_rules! USE {
    ($arg:expr) => {
        drop($arg);
    }
}
#[macro_export]
macro_rules! time_func {
    ($func:expr $(, $arg:expr)*) => {{
        let start = Instant::now();
        let result = $func($($arg),*);
        let elapsed = start.elapsed();
        println!("Elapsed time for {}: {:?}", stringify!($func),elapsed);
        result
    }};
    ($func:expr) => {{
        let start = Instant::now();
        let result = $func();
        let elapsed = start.elapsed();
        println!("Elapsed time for {}: {:?}", stringify!($func),elapsed);
        result
    }};
    ($name:expr, $obj:expr, $($args:expr),*) => {{
        let start = std::time::Instant::now();
        let result = $obj.$name($($args),*);
        let elapsed = start.elapsed();
        println!("Elapsed time for {}.{}: {:?}", stringify!($obj), stringify!($name), elapsed);
        result
    }};
    ($name:expr, $obj:expr) => {{
        let start = std::time::Instant::now();
        let result = $obj.$name();
        let elapsed = start.elapsed();
        println!("Elapsed time for {}.{}: {:?}", stringify!($obj), stringify!($name), elapsed);
        result
    }};
}
#[macro_export]
macro_rules! run_func {
    ($func:expr $(, $arg:expr)*) => {{
        let result = $func($($arg),*);
        result
    }};
    ($func:expr) => {{
        let result = $func();
        result
    }};
}
#[macro_export]
macro_rules! time_method {
    ($name:expr, $obj:ident, $($args:expr),*) => {{
        let start = std::time::Instant::now();
        let result = $name.$obj($($args),*);
        let elapsed = start.elapsed();
        println!("Elapsed time for {}.{}: {:?}", stringify!($name), stringify!($obj), elapsed);
        result
    }};
    ($name:expr, $obj:ident) => {{
        let start = std::time::Instant::now();
        let result = $name.$obj();
        let elapsed = start.elapsed();
        println!("Elapsed time for {}.{}: {:?}", stringify!($name), stringify!($obj), elapsed);
        result
    }};
}
#[macro_export]
macro_rules! com_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(C) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
    });
}
#[macro_export]
macro_rules! com_error {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(C) [ERROR] {}: {}", $location.loc_display(), message);
        exit(1);
    });
}
#[macro_export]
macro_rules! com_info {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(C) [INFO] {}: {}", $location.loc_display(), message);
    });
}
#[macro_export]
macro_rules! com_warn {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(C) [WARN] {}: {}", $location.loc_display(), message);
    });
}
#[macro_export]
macro_rules! com_assert {
    ($location:expr, $condition:expr, $($arg:tt)*) => ({
        if !($condition) {
            let message = format!($($arg)*);
            eprintln!("(C) [ERROR] {}: {}", $location.loc_display(), message);
            
            exit(1);
        }
    });
}
#[macro_export]
macro_rules! typ_error {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(T) [ERROR] {}: {}", $token.loc_display(), message);
        exit(1);
    });
}
#[macro_export]
macro_rules! typ_assert {
    ($token:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(T) [ERROR] {}: {}", $token.loc_display(), message);
            exit(1);
        }
    });
}
#[macro_export]
macro_rules! typ_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(T) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
    });
}
#[macro_export]
macro_rules! typ_info {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(T) [INFO] {}: {}", $token.loc_display(), message);
    });
}
#[macro_export]
macro_rules! typ_warn {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(T) [WARN] {}: {}", $token.loc_display(), message);
    });
}


pub fn unescape(stri: &String) -> String {
    let mut out = String::new();
    let mut chars = stri.chars().into_iter();
    while let Some(chr) = chars.next(){
        if chr == '\\' {
            let nc = &chars.next();
            assert!(nc.is_some(),"Error: could not unescape string, invalid string escape symbol");
            let nc = nc.unwrap();
            match nc {
                'n' => {
                    out.push_str("\n");
                }
                'r' => {
                    out.push_str("\r");
                }
                't' => {
                    out.push_str("\t");
                }
                _ => {
                    out.push(nc);
                }
            }

        }
        else {
            out.push(chr)
        }
    }
    out
}


