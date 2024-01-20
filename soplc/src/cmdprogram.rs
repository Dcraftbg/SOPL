#[derive(PartialEq, Debug, Clone)]
pub enum OptimizationMode {
    RELEASE,
    DEBUG
}

#[derive(Debug, Clone)]
pub struct CmdProgram {
    pub path: String,
    pub opath: String,
    pub target: String,
    pub should_build: bool,
    pub should_run: bool,
    pub use_type_checking: bool,
    pub print_unused_warns  : bool,
    // Sub sets
    pub print_unused_funcs  : bool,
    pub print_unused_externs: bool,
    pub print_unused_strings: bool,
    pub remove_unused_functions: bool,
    pub in_mode: OptimizationMode,
}
impl CmdProgram {
    pub fn new() -> Self {
        Self { path: String::new(), opath: String::new(), should_build: false, should_run: false, target: "nasm_x86_64".to_string(), in_mode: OptimizationMode::DEBUG, use_type_checking: true, print_unused_warns: false,  remove_unused_functions: false, print_unused_funcs: false, print_unused_externs: false, print_unused_strings: false}
    }
}


