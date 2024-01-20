#[derive(Debug,Clone, PartialEq)]
pub struct ArcCustomOps {
    pub on_overflow_stack: bool,
    pub shadow_space: usize
}
impl ArcCustomOps {
    pub fn new() -> Self {
        Self {on_overflow_stack: true, shadow_space: 0 }
    }
}
#[derive(Debug,Clone, PartialEq)]
pub enum ArcPassType {
    NONE,
    PUSHALL,
    CUSTOM(ArcCustomOps)
}
impl ArcPassType {
    pub fn custom_get(&self) -> Option<&ArcCustomOps> {
        match self {
            Self::NONE => None,
            Self::PUSHALL => None,
            Self::CUSTOM(c) => Some(c),
        }
    }
    pub fn custom_unwrap(&self) -> &ArcCustomOps {
        match self {
            Self::NONE                     => panic!("Unexpected custom_unwarp on non custom_unwarp value"),
            Self::PUSHALL                  => panic!("Unexpected custom_unwarp on non custom_unwarp value"),
            Self::CUSTOM(c) => c,
        }
    }
}
#[derive(Debug,Clone)]
pub struct ArcOps {
    pub argumentPassing: ArcPassType,
}
impl ArcOps {
    pub fn new() -> Self {
        Self {  argumentPassing: ArcPassType::NONE}
    }

}
#[derive(Debug,Clone)]
pub struct ArcFlags {
    pub nasm: Vec<String>
}
impl ArcFlags {
    pub fn new() -> Self {
        Self { nasm: Vec::new() }
    }
}
#[derive(Debug,Clone)]
pub struct Architecture {
    pub bits: u32,
    pub platform: String,
    pub options: ArcOps,
    pub func_prefix: String,
    pub cextern_prefix: String,
    pub obj_extension: String,
    pub flags: ArcFlags
}
impl Architecture {
    pub fn new() -> Self {
        Self { bits: 32, platform: String::new(),options: ArcOps::new(), func_prefix: String::new(), cextern_prefix: String::new(), obj_extension: "o".to_owned(), flags: ArcFlags::new() }
    }
}
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
    pub architecture: Architecture,
}
impl CmdProgram {
    pub fn new() -> Self {
        Self { path: String::new(), opath: String::new(), should_build: false, should_run: false, target: "nasm_x86_64".to_string(), in_mode: OptimizationMode::DEBUG, use_type_checking: true, print_unused_warns: false,  remove_unused_functions: false, print_unused_funcs: false, print_unused_externs: false, print_unused_strings: false, architecture: Architecture::new() }
    }
}


