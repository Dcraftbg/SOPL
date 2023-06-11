#![allow(redundant_semicolons)]
#[macro_export]
macro_rules! cfor {
    ($init:stmt; $cond:expr; $post:stmt; $($body:tt)*) => {{
        $init
        while $cond {
            $($body)*;
            $post
        }
    }};
    ($cond:expr; $post:stmt; $($body:tt)*) => {{
        while $cond {
            $($body)*;
            $post
        }
    }};
}