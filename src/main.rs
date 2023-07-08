#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(unreachable_patterns)]
#![allow(unreachable_code)]

use core::{num, panic};
use std::{env, process::{exit, Command, Stdio}, path::{Path, PathBuf}, ffi::OsStr, str::{FromStr, Chars}, collections::{HashMap, HashSet}, hash::Hash, fs::{File, self}, io::{Read, Write, self}, fmt::{format, Display}, borrow::{BorrowMut, Borrow}, clone, time::{SystemTime, Instant}, rc::Rc, iter::Peekable, cell::{RefCell, Ref, RefMut}, ops::{Deref, DerefMut}, vec, sync::Arc, os, f32::consts::E, any::Any};
use linked_hash_map::LinkedHashMap;
use serde_json::Value;
mod cfor;



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

macro_rules! par_error {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(P) [ERROR] {}: {}", $token.loc_display(), message);
        exit(1);
    });
}
macro_rules! par_assert {
    ($token:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(P) [ERROR] {}: {}", $token.loc_display(), message);
            exit(1);
        }
    });
}
macro_rules! par_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(P) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
    });
}
macro_rules! par_info {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(P) [INFO] {}: {}", $token.loc_display(), message);
    });
}
macro_rules! par_warn {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(P) [WARN] {}: {}", $token.loc_display(), message);
    });
}

macro_rules! com_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(C) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
    });
}
macro_rules! com_error {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(C) [ERROR] {}: {}", $location.loc_display(), message);
        exit(1);
    });
}
macro_rules! com_info {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(C) [INFO] {}: {}", $location.loc_display(), message);
    });
}
macro_rules! com_warn {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(C) [WARN] {}: {}", $location.loc_display(), message);
    });
}
macro_rules! com_assert {
    ($location:expr, $condition:expr, $($arg:tt)*) => ({
        if !($condition) {
            let message = format!($($arg)*);
            eprintln!("(C) [ERROR] {}: {}", $location.loc_display(), message);
            
            exit(1);
        }
    });
}

macro_rules! typ_error {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(T) [ERROR] {}: {}", $token.loc_display(), message);
        exit(1);
    });
}
macro_rules! typ_assert {
    ($token:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(T) [ERROR] {}: {}", $token.loc_display(), message);
            exit(1);
        }
    });
}
macro_rules! typ_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(T) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
    });
}
macro_rules! typ_info {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(T) [INFO] {}: {}", $token.loc_display(), message);
    });
}
macro_rules! typ_warn {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(T) [WARN] {}: {}", $token.loc_display(), message);
    });
}

macro_rules! lex_error {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(L) [ERROR] {}: {}", $token.loc_display(), message);
        exit(1);
    });
}
macro_rules! lex_assert {
    ($token:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(L) [ERROR] {}: {}", $token.loc_display(), message);
            exit(1);
        }
    });
}
macro_rules! lex_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(L) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
    });
}
macro_rules! lex_info {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(L) [INFO] {}: {}", $token.loc_display(), message);
    });
}
macro_rules! lex_warn {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(L) [WARN] {}: {}", $token.loc_display(), message);
    });
}

fn is_symbolic(c: char) -> bool {
    c.is_alphabetic() || c.is_alphanumeric() || c == '_' || c=='-'
}
fn unescape(stri: &String) -> String {
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

#[derive(PartialEq, Debug, Clone)]
enum OptimizationMode {
    RELEASE,
    DEBUG
}
#[derive(Debug,Clone, PartialEq)]
struct ArcCustomOps {
    // None means they are via the stack
    nums_ptrs: Option<Vec<Register>>,
    floats: Option<Vec<Register>>,
    returns: Option<Vec<Register>>,
    on_overflow_stack: bool,
    shadow_space: usize
}
impl ArcCustomOps {
    fn new() -> Self {
        Self {nums_ptrs: None, floats: None, returns: None, on_overflow_stack: true, shadow_space: 0 }
    }
}
#[derive(Debug,Clone, PartialEq)]
enum ArcPassType {
    NONE,
    PUSHALL,
    CUSTOM(ArcCustomOps)
}
impl ArcPassType {
    fn custom_get(&self) -> Option<&ArcCustomOps> {
        match self {
            Self::NONE => None,
            Self::PUSHALL => None,
            Self::CUSTOM(c) => Some(c),
        }
    }
    fn custom_unwrap(&self) -> &ArcCustomOps {
        match self {
            Self::NONE                     => panic!("Unexpected custom_unwarp on non custom_unwarp value"),
            Self::PUSHALL                  => panic!("Unexpected custom_unwarp on non custom_unwarp value"),
            Self::CUSTOM(c) => c,
        }
    }
}
#[derive(Debug,Clone)]
struct ArcOps {
    argumentPassing: ArcPassType,
}
impl ArcOps {
    fn new() -> Self {
        Self {  argumentPassing: ArcPassType::NONE}
    }

}
#[derive(Debug,Clone)]
struct ArcFlags {
    nasm: Vec<String>
}
impl ArcFlags {
    fn new() -> Self {
        Self { nasm: Vec::new() }
    }
}
#[derive(Debug,Clone)]
struct Architecture {
    bits: u32,
    platform: String,
    options: ArcOps,
    func_prefix: String,
    cextern_prefix: String,
    obj_extension: String,
    flags: ArcFlags
}
impl Architecture {
    fn new() -> Self {
        Self { bits: 32, platform: String::new(),options: ArcOps::new(), func_prefix: String::new(), cextern_prefix: String::new(), obj_extension: "o".to_owned(), flags: ArcFlags::new() }
    }
}
#[derive(Debug, Clone)]
struct CmdProgram {
    path: String,
    opath: String,
    target: String,
    should_build: bool,
    should_run: bool,
    use_type_checking: bool,
    print_unused_warns  : bool,
    // Sub sets
    print_unused_funcs  : bool,
    print_unused_externs: bool,
    print_unused_strings: bool,


    remove_unused_functions: bool,
    in_mode: OptimizationMode,
    architecture: Architecture,
}
impl CmdProgram {
    fn stack_ptr(&self) -> Register {
        if self.architecture.bits == 64 {
            Register::RSP
        }
        else {
            Register::ESP
        }
    }
    fn new() -> Self {
        Self { path: String::new(), opath: String::new(), should_build: false, should_run: false, target: "nasm_x86_64".to_string(), in_mode: OptimizationMode::DEBUG, use_type_checking: true, print_unused_warns: false,  remove_unused_functions: false, print_unused_funcs: false, print_unused_externs: false, print_unused_strings: false, architecture: Architecture::new() }
    }
}
#[repr(u32)]
#[derive(Clone, Copy,Debug,PartialEq )]

enum IntrinsicType {
    Extern = 0,
    DLL_IMPORT,
    DLL_EXPORT,
    Func,
    Let,
    CONSTANT,
    OPENPAREN,
    CLOSEPAREN,
    DOUBLE_COLIN,
    COMA,
    DOTCOMA,
    OPENCURLY,
    CLOSECURLY,
    OPENSQUARE,
    CLOSESQUARE,
    INTERRUPT,
    SYSCALL,

    // REGISTER MATH
    // ADD,
    // SUB,
    // MUL,
    // DIV,
    GOTO,
    MAKELABEL,
    RET,
    INCLUDE,
    IF,
    ELSE,
    WHILE,   
    CAST,
    THREEDOTS,
    // BOOLEAN OPERATIONS
    // EQUALS,
    // MORETHAN,
    // LESSTHAN,
    // MORETHANEQ,
    // LESSTHANEQ,
    // NOTEQUALS,
}
#[derive(Debug,Clone,PartialEq)]
enum PtrTyp {
    VOID,
    TYP(Box<VarType>),
}
impl PtrTyp {
    fn size(&self, program: &CmdProgram) -> usize {
        match self {
            Self::VOID => (program.architecture.bits / 8) as usize,
            Self::TYP(typ) => typ.get_size(program)
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
struct Ptr {
    typ: PtrTyp,
    inner_ref: usize,
}
impl Ptr {
    fn deref(&self) -> VarType {
        if self.inner_ref > 0 {
            VarType::PTR(Ptr {typ: self.typ.clone(), inner_ref: self.inner_ref-1 })
        }
        else {
            match &self.typ {
                PtrTyp::VOID => panic!("Cannot deref void pointer"),
                PtrTyp::TYP(t) => *(*t).clone(),
            }
        }
    }
    fn ref_to(typ: VarType) -> Self {
        if typ.is_ptr() {
            match typ.clone() {
                VarType::PTR(mut p) => {
                    p.inner_ref += 1;
                    p
                }
                _ => panic!("Unreachable")
            }
        }
        else {
            Self{inner_ref: 0, typ: PtrTyp::TYP(Box::new(typ))}
        }
    }
}
impl PtrTyp {
    fn to_string(&self) -> String {
        match self {
            Self::TYP(typ) => {
                typ.to_string(false)
            }
            Self::VOID => "void".to_owned()
        }
    }
}

impl IntrinsicType {
    fn to_string(&self,isplural: bool) -> String{
        match self {
            IntrinsicType::Extern => {
                if isplural {"Externals".to_string()} else {"External".to_string()}
            }
            IntrinsicType::Func => {
                if isplural {"Funcs".to_string()} else {"Func".to_string()}
            }
            IntrinsicType::OPENPAREN => {
                if isplural {"OpenParens".to_string()} else {"OpenParen".to_string()}
            }
            IntrinsicType::CLOSEPAREN => {
                if isplural {"CloseParens".to_string()} else {"CloseParen".to_string()}
            }
            IntrinsicType::DOUBLE_COLIN => {
                if isplural {"Double Colins".to_string()} else {"Double Colin".to_string()}
            }
            IntrinsicType::COMA => {
                if isplural {"Comas".to_string()} else {"Coma".to_string()}
            }
            IntrinsicType::OPENCURLY => {
                if isplural {"Open Curly".to_string()} else {"Open Curly".to_string()}
            }
            IntrinsicType::CLOSECURLY => {
                if isplural {"Close Curly".to_string()} else {"Close Curly".to_string()}
            }

            IntrinsicType::RET => {
                if isplural {"Ret".to_string()} else {"Ret".to_string()}
            },
            IntrinsicType::INCLUDE => {
                if isplural {"Includes".to_string()} else {"Include".to_string()}
            },
            IntrinsicType::IF => {
                if isplural {"Ifs".to_string()} else {"If".to_string()}
            },

            IntrinsicType::CONSTANT => {
                if isplural {"Constants".to_string()} else {"Constant".to_string()}
            },
            IntrinsicType::DOTCOMA => {
                if isplural {"Dotcomas".to_string()} else {"Dotcoma".to_string()}
            },
            IntrinsicType::ELSE => {
                if isplural {"Else".to_string()} else {"Else".to_string()}
            },
            IntrinsicType::Let => {
                if isplural {"Let".to_string()} else {"Let".to_string()}
            },
            IntrinsicType::INTERRUPT => {
                if isplural {"Interrupts".to_string()} else {"Interrupt".to_string()}
            },
           
            IntrinsicType::CAST => {
                if isplural {"Casts".to_string()} else {"Cast".to_string()}
            },
            IntrinsicType::DLL_IMPORT => {
                if isplural {"Dll Imports".to_string()} else {"Dll Import".to_string().to_string()}
            },
            IntrinsicType::DLL_EXPORT => {
                if isplural {"Dll Exports".to_string()} else {"Dll Export".to_string().to_string()}
            },
            IntrinsicType::WHILE => {
                if isplural {"While".to_string()} else {"While".to_string().to_string()}
            },
            IntrinsicType::SYSCALL => {
                if isplural {"Syscall".to_string()} else {"Syscall".to_string().to_string()}
            },
            IntrinsicType::THREEDOTS => {
                if isplural {"Threedots".to_string()} else {"Threedots".to_string().to_string()}
            },
            IntrinsicType::GOTO => {
                if isplural {"Gotos".to_string()} else {"Goto".to_string().to_string()}
            },
            IntrinsicType::MAKELABEL   => {
                if isplural {"Make labels".to_string()} else {"Make label".to_string()}
            },
            IntrinsicType::OPENSQUARE  => {
                if isplural {"Open square".to_string()} else {"Open square".to_string()}
            },
            IntrinsicType::CLOSESQUARE => {
                if isplural {"Close square".to_string()} else {"Close square".to_string()}
            }
        }
    }
}
#[derive(Debug,PartialEq,Clone)]
#[repr(u8)]
enum SetOp {
    SET,
    PLUSSET,
    MINUSSET,
    MULSET,
    DIVSET,
}

impl SetOp {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "="  => Some(Self::SET),
            "+=" => Some(Self::PLUSSET),
            "-=" => Some(Self::MINUSSET),
            "*=" => Some(Self::MULSET),
            "/=" => Some(Self::DIVSET),
            _ => None
        }
    }
    fn to_string(&self) -> &str {
        match self {
            Self::SET    => "=",
            Self::PLUSSET => "+=",
            Self::MINUSSET => "+=",
            Self::MULSET => "*=",
            Self::DIVSET => "/="
        }
    }
}
#[derive(Debug,PartialEq,Clone)]
#[repr(u8)]
enum Op {
    NONE = 0,
    PLUS,
    MINUS,
    DIV,
    STAR,
    BAND,
    REMAINDER,
    EQ,
    NEQ,
    GT,
    GTEQ,
    LT,
    LTEQ,
    NOT,

    //Binary Ops
    SRIGHT,
    SLEFT,
    BNOT,
    BOR,
}
impl Op {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "+" => Some(Op::PLUS),
            "-" => Some(Op::MINUS),
            "/" => Some(Op::DIV),
            "%" => Some(Op::REMAINDER),
            "*" => Some(Op::STAR),
            "=="=> Some(Op::EQ),
            "!="=> Some(Op::NEQ),
            ">" => Some(Op::GT),
            ">="=> Some(Op::GTEQ),
            "<" => Some(Op::LT),
            "<="=> Some(Op::LTEQ),
            "!" => Some(Op::NOT),
            "&" => Some(Op::BAND),
            ">>"=> Some(Op::SRIGHT),
            "<<"=> Some(Op::SLEFT),
            "|" => Some(Op::BOR),
            "~" => Some(Op::BNOT),
            _ => None
        }
    }
    fn to_string(&self) -> &str {
        match self {
            Op::NONE  => "None",
            Op::PLUS  => "+" ,
            Op::MINUS => "-" ,
            Op::DIV   => "/" ,
            Op::STAR   => "*" ,
            Op::EQ    => "==",
            Op::NEQ   => "!=",
            Op::GT    => ">" ,
            Op::GTEQ  => ">=",
            Op::LT    => "<" ,
            Op::LTEQ  => "<=",
            Op::NOT   => "!" ,
            Op::REMAINDER => "%",
            Op::BAND => "&",
            Op::BOR => "|",
            Op::BNOT => "~",
            Op::SRIGHT => ">>",
            Op::SLEFT => "<<",
        }
    }
    fn get_priority(&self, currentETree: &ExprTree) -> usize {
        match self {
            Self::BAND | Self::BNOT | Self::BOR | Self::SRIGHT | Self::SLEFT       => 6,
            Self::NOT =>  6,
            Self::EQ   | Self::NEQ | Self::GT | Self::LT | Self::GTEQ | Self::LTEQ => 2,
            Self::PLUS | Self::MINUS                                               => 3,
            Self::STAR                                                             => {
                
                if currentETree.left.is_none() {2} else {4}},
            Self::DIV | Self::REMAINDER                                            => 4,
            Self::NONE  => panic!("This should not occur"),
        }
    }
    fn is_boolean(&self) -> bool {
        match self {
            Self::EQ   | Self::NEQ | Self::GT | Self::LT | Self::GTEQ | Self::LTEQ => true,
            _ => false
        }
    }
}
#[derive(Debug, Clone)]
enum Expression {
    val(OfP),
    expr(Box<ExprTree>)
}
impl Expression {
    //fn jump(&self, label: String) -> 
    fn unwrap_val(&self) -> &OfP {
        if let Expression::val(v) = self {
            v
        }
        else {
            panic!("ERROR: Cannot unwrap on non val type! {:?}",self);
        }
    }
    fn unwrap_expr(&self) -> &Box<ExprTree> {
        if let Expression::expr(v) = self {
            v
        }
        else {
            panic!("ERROR: Cannot unwrap on non expr type!");
        }
    }
    fn is_expr(&self) -> bool {
        match self {
            Self::expr(_) => true,
            _ => false
        }
    }
    fn is_ofp(&self) -> bool {
        match self {
            Self::val(_) => true,
            _ => false
        }
    }
    fn result_of_c(&self,program: &CmdProgram,build: &BuildProgram, local_vars: &Vec<HashMap<String,LocalVariable>>, loc: &ProgramLocation) -> Option<VarType>  {
        match self {
            Self::val(v) => v.var_type(build, local_vars),
            Self::expr(s) => {
                if let Some(v1) = &s.left {
                    let res1 = v1.result_of_c(program, build, local_vars,loc);
                    if let Some(v2) = &s.right {
                        let res2 = v2.result_of_c(program, build, local_vars,loc);
                        if res1.is_some() && res2.is_some() && res1.as_ref().unwrap().weak_eq(&res2.as_ref().unwrap()) {
                            res1
                        }
                        else if res1.is_none() {
                            res2
                        }
                        else if self.is_expr() && (self.unwrap_expr().op == Op::PLUS || self.unwrap_expr().op == Op::MINUS) && res1.as_ref().unwrap().is_ptr() && res2.as_ref().unwrap().is_numeric() {
                            res1
                        }
                        else {
                            panic!("Unreachable")
                        }
                    }
                    else {
                        res1
                    }
                }
                else if let Some(v2) = &s.right {
                    if s.op == Op::STAR {
                        let res = v2.result_of_c(program, build, local_vars,loc);
                        let res = res.unwrap();
                        com_assert!(loc,res.is_some_ptr(),"Error: Cannot dereference void pointer!");
                        res.get_ptr_val()
                    }
                    else if s.op == Op::BAND {
                        com_assert!(loc,v2.is_ofp(),"Cannot reference expression!");
                        let ofp = v2.unwrap_val();
                        match ofp {
                            OfP::LOCALVAR(v) => {
                                let vp = &get_local_build(local_vars, v).unwrap().typ;
                                match vp {
                                    VarType::PTR(val) => {
                                        Some(VarType::PTR(Ptr { typ: val.typ.clone(), inner_ref: val.inner_ref+1 }))
                                    },
                                    _ => Some(VarType::PTR(Ptr { typ: PtrTyp::TYP(Box::new(vp.clone())), inner_ref: 0 }))
                                }

                            },
                            _ => {
                                typ_error!(loc,"Error: Cannot reference anything other than localvariable!");
                            }
                        }
                    }
                    else{
                        v2.result_of_c(program, build, local_vars,loc)
                    }
                }
                else {
                    panic!("Unreachable")
                }
            }
        }
    }
    fn result_of(&self,program: &CmdProgram, build: &BuildProgram, local_vars: &Vec<Locals>, buffers: &Vec<BuildBuf>, loc: &ProgramLocation) -> Option<VarType> {
        match self {
            Self::val(v) => v.var_type_t(build, local_vars,buffers),
            Self::expr(s) => {
                if let Some(v1) = &s.left {
                    let res1 = v1.result_of(program, build, local_vars,buffers,loc);
                    if let Some(v2) = &s.right {
                        let res2 = v2.result_of(program, build, local_vars,buffers,loc);
                        if res1.is_some() && res2.is_some() && res1.as_ref().unwrap().weak_eq(&res2.as_ref().unwrap()) {
                            res1
                        }
                        else if res1.is_none() {
                            res2
                        }
                        else if self.is_expr() && (self.unwrap_expr().op == Op::PLUS || self.unwrap_expr().op == Op::MINUS) && res1.as_ref().unwrap().is_ptr() && res2.as_ref().unwrap().is_numeric() {
                            res1
                        }
                        else if self.is_expr() && self.unwrap_expr().op.is_boolean() && res1.as_ref().unwrap().is_ptr() && res2.as_ref().unwrap().is_numeric() {
                            Some(VarType::BOOLEAN)
                        }
                        else {
                            println!("res1: {:?}",res1);
                            println!("res2: {:?}",res2);
                            println!("op: {:?}",s.op);
                            println!("(self.unwrap_expr().op == Op::PLUS || self.unwrap_expr().op == Op::MINUS): {}",(self.unwrap_expr().op == Op::PLUS || self.unwrap_expr().op == Op::MINUS));
                            println!("res1.as_ref().unwrap().is_ptr(): {}",res1.as_ref().unwrap().is_ptr());
                            println!("res2.as_ref().unwrap().is_numeric(): {}",res2.as_ref().unwrap().is_numeric());
                            println!("{:?}.weak_eq({:?}) = {}",res1,res2,res1.as_ref().unwrap().weak_eq(&res2.as_ref().unwrap()));
                            panic!("Unreachable at {}",loc.loc_display())
                        }
                    }
                    else {
                        res1
                    }
                }
                else if let Some(v2) = &s.right {
                    if s.op == Op::STAR {
                        let res = v2.result_of(program, build, local_vars,buffers,loc);
                        let res = res.unwrap();
                        typ_assert!(loc,res.is_some_ptr(),"Error: Cannot dereference void pointer!");
                        res.get_ptr_val()
                    }
                    else if s.op == Op::BAND {
                        typ_assert!(loc,v2.is_ofp(),"Cannot reference expression!");
                        let ofp = v2.unwrap_val();
                        match ofp {
                            OfP::LOCALVAR(v) => {
                                let vp = get_local(local_vars, v).unwrap();
                                match vp {
                                    VarType::PTR(val) => {
                                        Some(VarType::PTR(Ptr { typ: val.typ.clone(), inner_ref: val.inner_ref+1 }))
                                    },
                                    _ => Some(VarType::PTR(Ptr { typ: PtrTyp::TYP(Box::new(vp.clone())), inner_ref: 0 }))
                                }

                            },
                            _ => {
                                typ_error!(loc,"Error: Cannot reference anything other than localvariable!");
                            }
                        }
                    }
                    else{
                        v2.result_of(program, build, local_vars,buffers,loc)
                    }
                }
                else {
                    panic!("Unreachable")
                }
            }
        }
        
    }
}
impl Expression {
    fn LEIRnasm(&self,regs: Vec<Register>,f: &mut File, program: &CmdProgram,build: &BuildProgram,local_vars: &Vec<HashMap<String, LocalVariable>>, buffers: &Vec<BuildBuf>,stack_size: usize,loc: &ProgramLocation) -> std::io::Result<Vec<Register>>{
        match self {
            Expression::val(v) => {
               v.LOIRGNasm(regs, f, program, build, local_vars,buffers, stack_size, loc)
            },
            Expression::expr(e) => {
                
                e.eval_nasm(regs,f,program,build,local_vars,buffers,stack_size,loc)
            },
        }
    }
}
#[derive(Debug, Clone)]
struct ExprTree {
    left:  Option<Expression>,
    right: Option<Expression>,
    op: Op
}
impl ExprTree {
    fn eval_const(&self) -> Option<RawConstValue> {
        match self.op {
            Op::NONE      => panic!("this should be unreachable"),
            Op::PLUS      => {todo!("Op::PLUS")},
            Op::MINUS     => {todo!("Op::MINUS")},
            Op::DIV       => {todo!("Op::DIV")},
            Op::STAR      => {todo!("Op::STAR")},
            Op::BAND      => {todo!("Op::BAND")},
            Op::REMAINDER => {todo!("Op::REMAINDER")},
            Op::EQ        => {todo!("Op::EQ")},
            Op::NEQ       => {todo!("Op::NEQ")},
            Op::GT        => {todo!("Op::GT")},
            Op::GTEQ      => {todo!("Op::GTEQ")},
            Op::LT        => {todo!("Op::LT")},
            Op::LTEQ      => {todo!("Op::LTEQ")},
            Op::NOT       => {todo!("Op::NOT")},
            Op::SRIGHT => todo!(),
            Op::SLEFT => todo!(),
            Op::BNOT => todo!(),
            Op::BOR => todo!(),
        }

    }
    fn eval_nasm(&self,regs: Vec<Register>,f: &mut File,program: &CmdProgram,build: &BuildProgram,local_vars: &Vec<HashMap<String, LocalVariable>>, buffers: &Vec<BuildBuf>,stack_size: usize,loc: &ProgramLocation) -> std::io::Result<Vec<Register>>{
        #[allow(unused_assignments)]
        let mut o: Vec<Register> = Vec::new();
        match self.op {
            Op::NONE  => com_error!(loc,"Error: Cannot evaluate NONE operation!"),
            Op::PLUS  => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::PLUS.to_string());
                let right       = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::PLUS.to_string());
                
                let res_right = right.result_of_c(program, build, local_vars, loc).unwrap();
                let res_left = left.result_of_c(program, build, local_vars, loc).unwrap();

                if res_right.get_size(program) > res_left.get_size(program) {
                    writeln!(f, "   xor {}, {}",regs[1],regs[1])?;
                }
                else if res_right.get_size(program) < res_left.get_size(program) {
                    writeln!(f, "   xor {}, {}",regs[0],regs[0])?;
                }

                match left {
                    Expression::expr(left_expr) => {
                        match right {
                            Expression::expr(right_expr) => {
                                let mut left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                let mut right_oregs = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                if left_oregs[0].size() > right_oregs[0].size() {
                                    right_oregs[0] = right_oregs[0].to_byte_size(left_oregs[0].size());
                                }
                                else if left_oregs[0].size() < right_oregs[0].size() {
                                    left_oregs[0] = left_oregs[0].to_byte_size(right_oregs[0].size());
                                }
                                writeln!(f, "   add {}, {}",left_oregs[0],right_oregs[0])?;
                                o = left_oregs;
                            },
                            Expression::val(right_val) => {
                                match right_val {
                                    OfP::CONST(v) => {
                                        let left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   add {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut right = right_val.LOIRGNasm(vec![regs[1]], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if right[0].size() > left_oregs[0].size() {
                                            left_oregs[0] = left_oregs[0].to_byte_size(right[0].size());
                                        }
                                        else if right[0].size() < left_oregs[0].size() {
                                            right[0] = right[0].to_byte_size(left_oregs[0].size());
                                        }
                                        writeln!(f,"   add {}, {}",left_oregs[0],right[0])?;
                                        o = left_oregs;
                                    }
                                }
                            },
                        }
                    }
                    Expression::val(left_val) => {
                        match right {
                            Expression::expr(right_expr) => {
                                match left_val {
                                    OfP::CONST(v) => {
                                        let right_oregs = right_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   add {}, {}",right_oregs[0],v.get_num_data())?;
                                        o = right_oregs;
                                    }

                                    _ => {
                                        let mut right_oregs = right_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut left = left_val.LOIRGNasm(vec![regs[1].clone()], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if left[0].size() > right_oregs[0].size() {
                                            right_oregs[0] = right_oregs[0].to_byte_size(left[0].size());
                                        }
                                        else if left[0].size() < right_oregs[0].size() {
                                            left[0] = left[0].to_byte_size(right_oregs[0].size());
                                        }
                                        writeln!(f,"   add {}, {}",right_oregs[0],left[0])?;
                                        o = right_oregs;
                                    }
                                }
                            },
                            Expression::val(right_val) => {
                                match right_val {
                                    OfP::CONST(v) => {
                                        let left_oregs = left_val.LOIRGNasm(regs, f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   add {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_val.LOIRGNasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut right = right_val.LOIRGNasm(vec![regs[1].clone()], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if right[0].size() > left_oregs[0].size() {
                                            left_oregs[0] = left_oregs[0].to_byte_size(right[0].size());
                                        }
                                        else if right[0].size() < left_oregs[0].size() {
                                            right[0] = right[0].to_byte_size(left_oregs[0].size());
                                        }
                                        writeln!(f,"   add {}, {}",left_oregs[0],right[0])?;
                                        o = left_oregs;
                                    }
                                }
                            },
                        }
                    }
                }
            },
            Op::MINUS => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::MINUS.to_string());
                let right       = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::MINUS.to_string());
                
                let res_right = right.result_of_c(program, build, local_vars, loc).unwrap();
                let res_left = left.result_of_c(program, build, local_vars, loc).unwrap();

                if res_right.get_size(program) > res_left.get_size(program) {
                    writeln!(f, "   xor {}, {}",regs[1],regs[1])?;
                }
                else if res_right.get_size(program) < res_left.get_size(program) {
                    writeln!(f, "   xor {}, {}",regs[0],regs[0])?;
                }

                match left {
                    Expression::expr(left_expr) => {
                        match right {
                            Expression::expr(right_expr) => {
                                let mut left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                let mut right_oregs = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                if left_oregs[0].size() > right_oregs[0].size() {
                                    right_oregs[0] = right_oregs[0].to_byte_size(left_oregs[0].size());
                                }
                                else if left_oregs[0].size() < right_oregs[0].size() {
                                    left_oregs[0] = left_oregs[0].to_byte_size(right_oregs[0].size());
                                }
                                writeln!(f, "   sub {}, {}",left_oregs[0],right_oregs[0])?;
                                o = left_oregs;
                            },
                            Expression::val(right_val) => {
                                match right_val {
                                    OfP::CONST(v) => {
                                        let left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   sub {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut right = right_val.LOIRGNasm(vec![regs[1]], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if right[0].size() > left_oregs[0].size() {
                                            left_oregs[0] = left_oregs[0].to_byte_size(right[0].size());
                                        }
                                        else if right[0].size() < left_oregs[0].size() {
                                            right[0] = right[0].to_byte_size(left_oregs[0].size());
                                        }
                                        writeln!(f,"   sub {}, {}",left_oregs[0],right[0])?;
                                        o = left_oregs;
                                    }
                                }
                            },
                        }
                    }
                    Expression::val(left_val) => {
                        match right {
                            Expression::expr(right_expr) => {
                                match left_val {
                                    OfP::CONST(v) => {
                                        let right_oregs = right_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   sub {}, {}",right_oregs[0],v.get_num_data())?;
                                        o = right_oregs;
                                    }

                                    _ => {
                                        let mut right_oregs = right_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut left = left_val.LOIRGNasm(vec![regs[1].clone()], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if left[0].size() > right_oregs[0].size() {
                                            right_oregs[0] = right_oregs[0].to_byte_size(left[0].size());
                                        }
                                        else if left[0].size() < right_oregs[0].size() {
                                            left[0] = left[0].to_byte_size(right_oregs[0].size());
                                        }
                                        writeln!(f,"   sub {}, {}",right_oregs[0],left[0])?;
                                        o = right_oregs;
                                    }
                                }
                            },
                            Expression::val(right_val) => {
                                match right_val {
                                    OfP::CONST(v) => {
                                        let left_oregs = left_val.LOIRGNasm(regs, f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   sub {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_val.LOIRGNasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut right = right_val.LOIRGNasm(vec![regs[1].clone()], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if right[0].size() > left_oregs[0].size() {
                                            left_oregs[0] = left_oregs[0].to_byte_size(right[0].size());
                                        }
                                        else if right[0].size() < left_oregs[0].size() {
                                            right[0] = right[0].to_byte_size(left_oregs[0].size());
                                        }
                                        writeln!(f,"   sub {}, {}",left_oregs[0],right[0])?;
                                        o = left_oregs;
                                    }
                                }
                            },
                        }
                    }
                }
            },
            Op::DIV   => {
                com_assert!(loc,regs[0].to_byte_size(8) == Register::RAX,"Error: Cannot do division with output register different from RAX");
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::DIV.to_string());
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::DIV.to_string());
                //if left.is_ofp() 
                match right {
                    Expression::expr(right_expr) => {
                        let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                        com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                        let rightregs1 = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                        com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                        if leftregs1[0].to_byte_size(8) != Register::RAX {
                            writeln!(f, "   mov {}, {}",Register::RAX.to_byte_size(leftregs1[0].size()),leftregs1[0])?;
                        }
                        writeln!(f,"   cqo")?;
                        writeln!(f, "   idiv {}",rightregs1[0])?;
                        o = vec![Register::RAX.to_byte_size(leftregs1[0].size())];
                    }
                    Expression::val(right_val) => {
                        let left_oregs = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                        com_assert!(loc,left_oregs.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                        if left_oregs[0].to_byte_size(8) != Register::RAX {
                            writeln!(f, "   mov {}, {}",Register::RAX.to_byte_size(left_oregs[0].size()),left_oregs[0])?;
                        }
                        match right_val {
                            OfP::GLOBALVAR(v) => {
                                writeln!(f,"   cqo")?;
                                writeln!(f,"   idiv {} [_GLOBL_{}]",size_to_nasm_type(left_oregs[0].size()),v)?;
                            }
                            OfP::LOCALVAR(v) => {
                                let ov = get_local_build(local_vars,v).unwrap();
                                writeln!(f,"   cqo")?;
                                if stack_size-ov.operand > 0 {
                                    writeln!(f,"   idiv {} [{}+{}]",size_to_nasm_type(ov.typ.get_size(program)),program.stack_ptr(),stack_size-ov.operand)?;
                                }
                                else {
                                    writeln!(f,"   idiv {} [{}]",size_to_nasm_type(ov.typ.get_size(program)),program.stack_ptr())?;
                                }
                                
                            }
                            _ => {
                                let right = right_val.LOIRGNasm(vec![regs[1].clone()], f, program, build, local_vars, buffers, stack_size, loc)?;
                                writeln!(f,"   cqo")?;
                                writeln!(f,"   idiv {}",right[0],)?;
                            }
                        }
                        o = vec![Register::RAX.to_byte_size(left_oregs[0].size())];
                    }
                }
                
                
            },
            Op::REMAINDER => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::DIV.to_string());
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::DIV.to_string());
                match right {
                    Expression::expr(right_expr) => {
                        let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                        com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                        let rightregs1 = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                        com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                        if leftregs1[0].to_byte_size(8) != Register::RDX {
                            writeln!(f, "   mov {}, {}",Register::RDX.to_byte_size(leftregs1[0].size()),leftregs1[0])?;
                        }
                        writeln!(f,"   cqo")?;
                        writeln!(f, "   idiv {}",rightregs1[0])?;
                        o = vec![Register::RDX.to_byte_size(leftregs1[0].size())];
                    }
                    Expression::val(right_val) => {
                        let left_oregs = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                        com_assert!(loc,left_oregs.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                        if left_oregs[0].to_byte_size(8) != Register::RDX {
                            writeln!(f, "   mov {}, {}",Register::RDX.to_byte_size(left_oregs[0].size()),left_oregs[0])?;
                        }
                        match right_val {
                            OfP::GLOBALVAR(v) => {
                                writeln!(f,"   cqo")?;
                                writeln!(f,"   idiv {} [_GLOBL_{}]",size_to_nasm_type(left_oregs[0].size()),v)?;
                            }
                            OfP::LOCALVAR(v) => {
                                let ov = get_local_build(local_vars,v).unwrap();
                                writeln!(f,"   cqo")?;
                                if stack_size-ov.operand > 0 {
                                    writeln!(f,"   idiv {} [{}+{}]",size_to_nasm_type(ov.typ.get_size(program)),program.stack_ptr(),stack_size-ov.operand)?;
                                }
                                else {
                                    writeln!(f,"   idiv {} [{}]",size_to_nasm_type(ov.typ.get_size(program)),program.stack_ptr())?;
                                }
                                
                            }
                            _ => {
                                let right = right_val.LOIRGNasm(vec![regs[1].clone()], f, program, build, local_vars, buffers, stack_size, loc)?;
                                writeln!(f,"   cqo")?;
                                writeln!(f,"   idiv {}",right[0])?;
                            }
                        }
                        o = vec![Register::RDX.to_byte_size(left_oregs[0].size())];
                    }
                }

            },
            Op::STAR   => {
                if let Some(left) = self.left.as_ref() {
                    com_assert!(loc,regs[0].to_byte_size(8) == Register::RAX,"Error: Cannot do multiplication with output register different from RAX");
                    let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::DIV.to_string());
                    match right {
                        Expression::expr(right_expr) => {
                            let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                            com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                            let rightregs1 = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                            com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                            if leftregs1[0].to_byte_size(8) != Register::RAX {
                                writeln!(f, "   mov {}, {}",Register::RAX.to_byte_size(leftregs1[0].size()),leftregs1[0])?;
                            }
                            writeln!(f,"   cqo")?;
                            writeln!(f, "   imul {}",rightregs1[0])?;
                            o = vec![Register::RAX.to_byte_size(leftregs1[0].size())];
                        }
                        Expression::val(right_val) => {
                            let left_oregs = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                            com_assert!(loc,left_oregs.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                            if left_oregs[0].to_byte_size(8) != Register::RAX {
                                writeln!(f, "   mov {}, {}",Register::RAX.to_byte_size(left_oregs[0].size()),left_oregs[0])?;
                            }
                            match right_val {
                                OfP::GLOBALVAR(v) => {
                                    writeln!(f,"   cqo")?;
                                    writeln!(f,"   imul {} [_GLOBL_{}]",size_to_nasm_type(left_oregs[0].size()),v)?;
                                }
                                OfP::LOCALVAR(v) => {
                                    let ov = get_local_build(local_vars,v).unwrap();
                                    writeln!(f,"   cqo")?;
                                    if stack_size-ov.operand > 0 {
                                        writeln!(f,"   imul {} [{}+{}]",size_to_nasm_type(ov.typ.get_size(program)),program.stack_ptr(),stack_size-ov.operand)?;
                                    }
                                    else {
                                        writeln!(f,"   imul {} [{}]",size_to_nasm_type(ov.typ.get_size(program)),program.stack_ptr())?;
                                    }
                                    
                                }
                                _ => {
                                    let right = right_val.LOIRGNasm(vec![regs[1].clone()], f, program, build, local_vars, buffers, stack_size, loc)?;
                                    writeln!(f,"   cqo")?;
                                    writeln!(f,"   imul {}",right[0],)?;
                                }
                            }
                            o = vec![Register::RAX.to_byte_size(left_oregs[0].size())];
                        }
                    }
                }
                else {
                    let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without right parameter",Op::STAR.to_string());
                    let rightregs1 = right.LEIRnasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                    com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                    let rego = &rightregs1[0];
                    let resof = right.result_of_c(program, build, local_vars,loc).unwrap();
                    com_assert!(loc, resof.is_some_ptr(), "Error: Cannot dereference void pointer!");
                    let resof = resof.get_ptr_val().unwrap();
                    writeln!(f, "   mov {}, {} [{}]",rego.to_byte_size(resof.get_size(program)).to_string(),size_to_nasm_type(resof.get_size(program)),rego.to_string())?;
                    o = vec![rego.to_byte_size(resof.get_size(program))];
                }
            },
            Op::EQ    => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::EQ   .to_string());
                let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::EQ   .to_string());
                let rightregs1 = right.LEIRnasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                writeln!(f, "   cmp {}, {}",leftregs1[0].to_string(),rightregs1[0].to_string())?;
                writeln!(f, "   sete {}",leftregs1[0].to_byte_size(1).to_string())?;
                o = vec![leftregs1[0].to_byte_size(1)];
            },
            Op::NEQ   => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::NEQ  .to_string());
                let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::NEQ  .to_string());
                let rightregs1 = right.LEIRnasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                writeln!(f, "   cmp {}, {}",leftregs1[0].to_string(),rightregs1[0].to_string())?;
                writeln!(f, "   setne {}",leftregs1[0].to_byte_size(1).to_string())?;
                o = vec![leftregs1[0].to_byte_size(1)];
            },
            Op::GT    => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::GT   .to_string());
                let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::GT   .to_string());
                let rightregs1 = right.LEIRnasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                writeln!(f, "   cmp {}, {}",leftregs1[0].to_string(),rightregs1[0].to_string())?;
                writeln!(f, "   setg {}",leftregs1[0].to_byte_size(1).to_string())?;
                o = vec![leftregs1[0].to_byte_size(1)];

            },
            Op::GTEQ  => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::GTEQ .to_string());
                let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::GTEQ .to_string());
                let rightregs1 = right.LEIRnasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                writeln!(f, "   cmp {}, {}",leftregs1[0].to_string(),rightregs1[0].to_string())?;
                writeln!(f, "   setge {}",leftregs1[0].to_byte_size(1).to_string())?;
                o = vec![leftregs1[0].to_byte_size(1)];

            },
            Op::LT    => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::LT   .to_string());
                let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::LT   .to_string());
                let rightregs1 = right.LEIRnasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                writeln!(f, "   cmp {}, {}",leftregs1[0].to_string(),rightregs1[0].to_string())?;
                writeln!(f, "   setl {}",leftregs1[0].to_byte_size(1).to_string())?;
                o = vec![leftregs1[0].to_byte_size(1)];

            },
            Op::LTEQ  => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::LTEQ .to_string());
                let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::LTEQ .to_string());
                let rightregs1 = right.LEIRnasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                writeln!(f, "   cmp {}, {}",leftregs1[0].to_string(),rightregs1[0].to_string())?;
                writeln!(f, "   setle {}",leftregs1[0].to_byte_size(1).to_string())?;
                o = vec![leftregs1[0].to_byte_size(1)];

            },
            Op::NOT   => {
                todo!("Not is not yet implemented");
                

            },
            Op::BAND => {
                if let Some(_) = self.left.as_ref() {
                    todo!()
                }
                else {
                    let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::BAND.to_string());
                    com_assert!(loc,right.is_ofp(), "Error: Expected right to be ofp but found something else!");
                    let rightofp = right.unwrap_val();
                    match rightofp {
                        OfP::LOCALVAR(v) => {
                            let v = get_local_build(local_vars, v).unwrap();
                            writeln!(f, "   mov {}, {}",regs[0].to_string(),program.stack_ptr())?;
                            writeln!(f, "   add {}, {}",regs[0].to_string(),stack_size-v.operand)?;
                            o = vec![regs[0].clone()]
                        }
                        _ => com_error!(loc, "Error: Cannot get location of ofp")
                    }
                }
            },
            Op::SRIGHT => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::SRIGHT.to_string());
                let right       = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::SRIGHT.to_string());
                
                let res_right = right.result_of_c(program, build, local_vars, loc).unwrap();
                let res_left = left.result_of_c(program, build, local_vars, loc).unwrap();

                if res_right.get_size(program) > res_left.get_size(program) {
                    writeln!(f, "   xor {}, {}",regs[1],regs[1])?;
                }
                else if res_right.get_size(program) < res_left.get_size(program) {
                    writeln!(f, "   xor {}, {}",regs[0],regs[0])?;
                }

                match left {
                    Expression::expr(left_expr) => {
                        match right {
                            Expression::expr(right_expr) => {
                                let mut left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                let mut right_oregs = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                if left_oregs[0].size() > right_oregs[0].size() {
                                    right_oregs[0] = right_oregs[0].to_byte_size(left_oregs[0].size());
                                }
                                else if left_oregs[0].size() < right_oregs[0].size() {
                                    left_oregs[0] = left_oregs[0].to_byte_size(right_oregs[0].size());
                                }
                                writeln!(f, "   shr {}, {}",left_oregs[0],right_oregs[0])?;
                                o = left_oregs;
                            },
                            Expression::val(right_val) => {
                                match right_val {
                                    OfP::CONST(v) => {
                                        let left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   shr {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut right = right_val.LOIRGNasm(vec![regs[1]], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if right[0].size() > left_oregs[0].size() {
                                            left_oregs[0] = left_oregs[0].to_byte_size(right[0].size());
                                        }
                                        else if right[0].size() < left_oregs[0].size() {
                                            right[0] = right[0].to_byte_size(left_oregs[0].size());
                                        }
                                        writeln!(f,"   shr {}, {}",left_oregs[0],right[0])?;
                                        o = left_oregs;
                                    }
                                }
                            },
                        }
                    }
                    Expression::val(left_val) => {
                        match right {
                            Expression::expr(right_expr) => {
                                match left_val {
                                    OfP::CONST(v) => {
                                        let right_oregs = right_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   shr {}, {}",right_oregs[0],v.get_num_data())?;
                                        o = right_oregs;
                                    }

                                    _ => {
                                        let mut right_oregs = right_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut left = left_val.LOIRGNasm(vec![regs[1].clone()], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if left[0].size() > right_oregs[0].size() {
                                            right_oregs[0] = right_oregs[0].to_byte_size(left[0].size());
                                        }
                                        else if left[0].size() < right_oregs[0].size() {
                                            left[0] = left[0].to_byte_size(right_oregs[0].size());
                                        }
                                        writeln!(f,"   shr {}, {}",right_oregs[0],left[0])?;
                                        o = right_oregs;
                                    }
                                }
                            },
                            Expression::val(right_val) => {
                                match right_val {
                                    OfP::CONST(v) => {
                                        let left_oregs = left_val.LOIRGNasm(regs, f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   shr {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_val.LOIRGNasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut right = right_val.LOIRGNasm(vec![regs[1].clone()], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if right[0].size() > left_oregs[0].size() {
                                            left_oregs[0] = left_oregs[0].to_byte_size(right[0].size());
                                        }
                                        else if right[0].size() < left_oregs[0].size() {
                                            right[0] = right[0].to_byte_size(left_oregs[0].size());
                                        }
                                        writeln!(f,"   shr {}, {}",left_oregs[0],right[0])?;
                                        o = left_oregs;
                                    }
                                }
                            },
                        }
                    }
                }
            },
            Op::SLEFT => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::SLEFT.to_string());
                let right       = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::SLEFT.to_string());
                
                let res_right = right.result_of_c(program, build, local_vars, loc).unwrap();
                let res_left = left.result_of_c(program, build, local_vars, loc).unwrap();

                if res_right.get_size(program) > res_left.get_size(program) {
                    writeln!(f, "   xor {}, {}",regs[1],regs[1])?;
                }
                else if res_right.get_size(program) < res_left.get_size(program) {
                    writeln!(f, "   xor {}, {}",regs[0],regs[0])?;
                }

                match left {
                    Expression::expr(left_expr) => {
                        match right {
                            Expression::expr(right_expr) => {
                                let mut left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                let mut right_oregs = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                if left_oregs[0].size() > right_oregs[0].size() {
                                    right_oregs[0] = right_oregs[0].to_byte_size(left_oregs[0].size());
                                }
                                else if left_oregs[0].size() < right_oregs[0].size() {
                                    left_oregs[0] = left_oregs[0].to_byte_size(right_oregs[0].size());
                                }
                                writeln!(f, "   shl {}, {}",left_oregs[0],right_oregs[0])?;
                                o = left_oregs;
                            },
                            Expression::val(right_val) => {
                                match right_val {
                                    OfP::CONST(v) => {
                                        let left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   shl {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut right = right_val.LOIRGNasm(vec![regs[1]], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if right[0].size() > left_oregs[0].size() {
                                            left_oregs[0] = left_oregs[0].to_byte_size(right[0].size());
                                        }
                                        else if right[0].size() < left_oregs[0].size() {
                                            right[0] = right[0].to_byte_size(left_oregs[0].size());
                                        }
                                        writeln!(f,"   shl {}, {}",left_oregs[0],right[0])?;
                                        o = left_oregs;
                                    }
                                }
                            },
                        }
                    }
                    Expression::val(left_val) => {
                        match right {
                            Expression::expr(right_expr) => {
                                match left_val {
                                    OfP::CONST(v) => {
                                        let right_oregs = right_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   shl {}, {}",right_oregs[0],v.get_num_data())?;
                                        o = right_oregs;
                                    }

                                    _ => {
                                        let mut right_oregs = right_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut left = left_val.LOIRGNasm(vec![regs[1].clone()], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if left[0].size() > right_oregs[0].size() {
                                            right_oregs[0] = right_oregs[0].to_byte_size(left[0].size());
                                        }
                                        else if left[0].size() < right_oregs[0].size() {
                                            left[0] = left[0].to_byte_size(right_oregs[0].size());
                                        }
                                        writeln!(f,"   shl {}, {}",right_oregs[0],left[0])?;
                                        o = right_oregs;
                                    }
                                }
                            },
                            Expression::val(right_val) => {
                                match right_val {
                                    OfP::CONST(v) => {
                                        let left_oregs = left_val.LOIRGNasm(regs, f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   shl {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_val.LOIRGNasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut right = right_val.LOIRGNasm(vec![regs[1].clone()], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if right[0].size() > left_oregs[0].size() {
                                            left_oregs[0] = left_oregs[0].to_byte_size(right[0].size());
                                        }
                                        else if right[0].size() < left_oregs[0].size() {
                                            right[0] = right[0].to_byte_size(left_oregs[0].size());
                                        }
                                        writeln!(f,"   shl {}, {}",left_oregs[0],right[0])?;
                                        o = left_oregs;
                                    }
                                }
                            },
                        }
                    }
                }
            },
            Op::BNOT => todo!(),
            Op::BOR => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::BOR.to_string());
                let right       = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::BOR.to_string());
                
                let res_right = right.result_of_c(program, build, local_vars, loc).unwrap();
                let res_left = left.result_of_c(program, build, local_vars, loc).unwrap();

                if res_right.get_size(program) > res_left.get_size(program) {
                    writeln!(f, "   xor {}, {}",regs[1],regs[1])?;
                }
                else if res_right.get_size(program) < res_left.get_size(program) {
                    writeln!(f, "   xor {}, {}",regs[0],regs[0])?;
                }

                match left {
                    Expression::expr(left_expr) => {
                        match right {
                            Expression::expr(right_expr) => {
                                let mut left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                let mut right_oregs = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                if left_oregs[0].size() > right_oregs[0].size() {
                                    right_oregs[0] = right_oregs[0].to_byte_size(left_oregs[0].size());
                                }
                                else if left_oregs[0].size() < right_oregs[0].size() {
                                    left_oregs[0] = left_oregs[0].to_byte_size(right_oregs[0].size());
                                }
                                writeln!(f, "   or {}, {}",left_oregs[0],right_oregs[0])?;
                                o = left_oregs;
                            },
                            Expression::val(right_val) => {
                                match right_val {
                                    OfP::CONST(v) => {
                                        let left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   or {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut right = right_val.LOIRGNasm(vec![regs[1]], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if right[0].size() > left_oregs[0].size() {
                                            left_oregs[0] = left_oregs[0].to_byte_size(right[0].size());
                                        }
                                        else if right[0].size() < left_oregs[0].size() {
                                            right[0] = right[0].to_byte_size(left_oregs[0].size());
                                        }
                                        writeln!(f,"   or {}, {}",left_oregs[0],right[0])?;
                                        o = left_oregs;
                                    }
                                }
                            },
                        }
                    }
                    Expression::val(left_val) => {
                        match right {
                            Expression::expr(right_expr) => {
                                match left_val {
                                    OfP::CONST(v) => {
                                        let right_oregs = right_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   or {}, {}",right_oregs[0],v.get_num_data())?;
                                        o = right_oregs;
                                    }

                                    _ => {
                                        let mut right_oregs = right_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut left = left_val.LOIRGNasm(vec![regs[1].clone()], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if left[0].size() > right_oregs[0].size() {
                                            right_oregs[0] = right_oregs[0].to_byte_size(left[0].size());
                                        }
                                        else if left[0].size() < right_oregs[0].size() {
                                            left[0] = left[0].to_byte_size(right_oregs[0].size());
                                        }
                                        writeln!(f,"   or {}, {}",right_oregs[0],left[0])?;
                                        o = right_oregs;
                                    }
                                }
                            },
                            Expression::val(right_val) => {
                                match right_val {
                                    OfP::CONST(v) => {
                                        let left_oregs = left_val.LOIRGNasm(regs, f, program, build, local_vars, buffers, stack_size, loc)?;
                                        writeln!(f,"   or {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_val.LOIRGNasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc)?;
                                        let mut right = right_val.LOIRGNasm(vec![regs[1].clone()], f, program, build, local_vars, buffers, stack_size, loc)?;
                                        if right[0].size() > left_oregs[0].size() {
                                            left_oregs[0] = left_oregs[0].to_byte_size(right[0].size());
                                        }
                                        else if right[0].size() < left_oregs[0].size() {
                                            right[0] = right[0].to_byte_size(left_oregs[0].size());
                                        }
                                        writeln!(f,"   or {}, {}",left_oregs[0],right[0])?;
                                        o = left_oregs;
                                    }
                                }
                            },
                        }
                    }
                }
            },

        }
        Ok(o)
        //todo!("ERROR: this should stop here")
    }
    fn new() -> Self {
        Self { left: None, right: None, op: Op::NONE }
    }
}
fn tokens_to_expression(body: &[Token],build: &mut BuildProgram, program: &CmdProgram,locals: &Vec<Locals>, buffers: &mut Vec<BuildBuf>) -> Expression {
    
    let mut bracketcount = 0;
    if body.len() == 0 {
        panic!("Error: Cannot evaluate empty body");
    }
    if body.len() == 1 {
        return Expression::val(par_expect!(&body[0],OfP::from_token(&body[0], build, program, locals),"Error: Expected Ofp but found {}",&body[0].typ.to_string(false)));
    }
    if let TokenType::Function(f) = &body[0].typ {
        let mut i2 = 0;
        let cbody = {
            while i2 < body.len() && body[i2].typ != TokenType::IntrinsicType(IntrinsicType::CLOSEPAREN){
                i2+=1
            }
            let res = &body[1..i2];
            res
        };
        if body.len() == i2+1 {
            return Expression::val(OfP::RESULT(f.clone(), parse_argument_contract_from_body(cbody, build, program,locals, &body[0].location)));
        }
    }
    if let TokenType::WordType(f) = &body[0].typ {
        if build.externals.contains_key(f) {
            let mut i2 = 0;
            let cbody = {
                while i2 < body.len() && body[i2].typ != TokenType::IntrinsicType(IntrinsicType::CLOSEPAREN){
                    i2+=1
                }
                let res = &body[1..i2];
                res
            };
            if body.len() == i2+1 {
                return Expression::val(OfP::RESULT(f.clone(), parse_argument_contract_from_body(cbody, build, program, locals, &body[0].location)));
            }
        }
    }
    if let TokenType::IntrinsicType(i) = &body[0].typ {
        let mut i2 = 0;
        if *i == IntrinsicType::OPENSQUARE {
            {
                while i2 < body.len() && body[i2].typ != TokenType::IntrinsicType(IntrinsicType::CLOSESQUARE){
                    i2+=1
                }
                let res = &body[1..i2];
                res
            };
            if body.len() == i2+1 {
                let ntok = &body[1];
                if let TokenType::Definition(buftyp) = &ntok.typ {
                    let ntok = &body[2];
                    par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::COMA), "Error: Expected coma after defintion in buffer!");    
                    let ntok = &body[3];
                    let size = par_expect!(ntok, ntok.typ.unwrap_numeric(build), "Error: Expected numeric value after coma to indicate the size of the buffer but found {}",ntok.typ);
                    par_assert!(ntok, size > 0, "Error: Cannot have buffer of size lower than or equal to 0");
                    let res = buffers.len();
                    buffers.push(BuildBuf::from_parse_buf(buftyp.clone(),size as usize));
                    return Expression::val(OfP::BUFFER(res));
                }
                else {
                    par_error!(ntok, "Error: Expected VarType for buffer but found {}",ntok.typ);
                }
                
                //return Expression::val(OfP::BUFFER(()))
            }
        };  

    }
    let mut currentETree: ExprTree = ExprTree::new();
    let mut i: usize = 0;

    
    while i < body.len() {
        let token = &body[i];
    
        i += 1;
    
        if let Some(mut val) = OfP::from_token(token, build, program, locals) {
            if currentETree.op == Op::NONE && currentETree.left.is_some() && currentETree.right.is_none() && match &val {OfP::CONST(c) => {match c { RawConstValueType::INT(i) => *i < 0, RawConstValueType::LONG(i) => *i < 0, _ => false}},_ => false}{
                currentETree.op = Op::MINUS;
                match val {
                    OfP::CONST(c) => {
                        match c {
                            RawConstValueType::INT(v) => {
                                val = OfP::CONST(RawConstValueType::INT(v.abs()))
                            }
                            RawConstValueType::LONG(v) => {
                                val = OfP::CONST(RawConstValueType::LONG(v.abs()))
                            }
                            _ => panic!("Unreachable")
                        }
                    }
                    _ => panic!("Unreachable!")
                }
            }
            par_assert!(token,currentETree.left.is_none() || (currentETree.op != Op::NONE && currentETree.right.is_none()),"Error: Cannot have multiple Ofp values consecutively!");
            if currentETree.left.is_none() {
                currentETree.left = Some(Expression::val(val))
            }
            else if currentETree.right.is_none() && currentETree.op != Op::NONE {
                currentETree.right = Some(Expression::val(val))
            }
            else {
                par_error!(token, "Error: Cannot assign value of an expression where both are the sides are fufilled or an operator was not provided before the right side!");
            }
        }
        else if let TokenType::Function(f) = &token.typ {

            par_assert!(token,currentETree.left.is_none() || (currentETree.op != Op::NONE && currentETree.right.is_none()),"Error: Cannot have multiple Ofp values consecutively!");
            let cbody = {
                let mut i2 = i;
                while i2 < body.len() && body[i2].typ != TokenType::IntrinsicType(IntrinsicType::CLOSEPAREN){
                    i2+=1
                }
                let res = &body[i..i2];
                i += i2;
                res
            };

            if currentETree.left.is_none() {
                currentETree.left = Some(Expression::val(OfP::RESULT(f.to_owned(), parse_argument_contract_from_body(cbody, build, program, locals, &token.location))))
            }
            else if currentETree.right.is_none() && currentETree.op != Op::NONE {
                currentETree.right = Some(Expression::val(OfP::RESULT(f.to_owned(), parse_argument_contract_from_body(cbody, build,program, locals, &token.location))))
            }
            else {
                par_error!(token, "Error: Cannot assign value of an expression where both are the sides are fufilled or an operator was not provided before the right side!");
            }
        }
        else if let TokenType::Operation(op) = &token.typ {
            if *op == Op::NOT || *op == Op::BAND {
                par_assert!(token, currentETree.left.is_none(), "Error: Cannot have NOT with a left side already defined!");
            }
            else if *op == Op::STAR {//this means its a deref
            }
            else if currentETree.left.is_none() && currentETree.op == Op::STAR {
                //println!("currentETree before: {:?}",currentETree);
                let mut tmp_eTree = ExprTree::new();
                tmp_eTree.left = Some(Expression::expr(Box::new(currentETree)));
                tmp_eTree.op = op.clone();
                //currentETree = tmp_eTree;
                todo!("{}: pls just use a temporary value here! this shit is absolutely broken rn",token.loc_display());
                //println!("currentETree after: {:?}",currentETree);
            }
            else {
                //println!("currentETree: {:?}",currentETree);
                par_assert!(token, currentETree.left.is_some(), "Error: Cannot have an Operator different from NOT or STAR without a left side! Found op: {}",op.to_string());
            }
            currentETree.op = op.clone();
            let mut opo = &Op::NONE;
            let priority = op.get_priority(&currentETree);
            let mut i2 = i;
            if body.len() < i+1 {
                continue;
            }
            let mut tmp_eTree: ExprTree = ExprTree::new();
            while i2 < body.len() {
                let exprTok = &body[i2];
                if let Some(val) = OfP::from_token(exprTok, build, program, locals) {
                    if tmp_eTree.left.is_none() {
                        tmp_eTree.left = Some(Expression::val(val))
                    }
                    else if tmp_eTree.right.is_none() && currentETree.op != Op::NONE {
                        tmp_eTree.right = Some(Expression::val(val))
                    }
                    else {
                        tmp_eTree = ExprTree::new();
                        tmp_eTree.left = Some(Expression::val(val))
                    }
                }
                else if let TokenType::Operation(op2) = &exprTok.typ {
                    if op2.get_priority(&tmp_eTree) <= priority {
                        opo = op2;
                        break;
                    }
                }
                else if let TokenType::IntrinsicType(t) = &exprTok.typ {
                    par_assert!(exprTok, *t == IntrinsicType::OPENPAREN, "Unexpected intrinsic {}", t.to_string(false));
                    let org_count = bracketcount;
                    bracketcount += 1;
                    while bracketcount > org_count {
                        i2 += 1;
                        if let Some(tok) = body.get(i2) {
                            match tok.typ {
                                TokenType::IntrinsicType(it) => {
                                    match it {
                                        IntrinsicType::OPENPAREN => bracketcount += 1,
                                        IntrinsicType::CLOSEPAREN => bracketcount -= 1,
                                        _ =>{}
                                    }
                                }
                                _ => {}
                            }
                        }
                        else {
                            par_error!(exprTok, "Open paren opened here but never closed!");
                        }
                    }

                }
                else {
                    par_error!(exprTok, "Error: unknown token type in expression: {}",exprTok.typ.to_string(false))
                }
                i2 += 1;
            }
            currentETree.right = Some(tokens_to_expression(&body[i..i2],build,program,locals,buffers));
            if i2 == body.len() {}
            else {
                let buf = Expression::expr(Box::new(currentETree));
                currentETree = ExprTree::new();
                currentETree.left = Some(buf);
                currentETree.op = opo.to_owned();
            }
            //println!("currentETree after everything: {:?}",currentETree);
            i = i2
        }
        else if let TokenType::WordType(f) = &body[0].typ {
            if build.externals.contains_key(f) {

                par_assert!(token,currentETree.left.is_none() || (currentETree.op != Op::NONE && currentETree.right.is_none()),"Error: Cannot have multiple Ofp values consecutively!");
                let cbody = {
                    let mut i2 = i;
                    while i2 < body.len() && body[i2].typ != TokenType::IntrinsicType(IntrinsicType::CLOSEPAREN){
                        i2+=1
                    }
                    let res = &body[i..i2];
                    i += i2;
                    res
                };

                if currentETree.left.is_none() {
                    currentETree.left = Some(Expression::val(OfP::RESULT(f.to_owned(), parse_argument_contract_from_body(cbody, build, program,locals, &token.location))))
                }
                else if currentETree.right.is_none() && currentETree.op != Op::NONE {
                    currentETree.right = Some(Expression::val(OfP::RESULT(f.to_owned(), parse_argument_contract_from_body(cbody, build,program, locals, &token.location))))
                }
                else {
                    par_error!(token, "Error: Cannot assign value of an expression where both are the sides are fufilled or an operator was not provided before the right side!");
                }
            }
            else {
                par_error!(token, "Error: Unexpected word {}",f);
            }
        }
        else if let TokenType::IntrinsicType(t) = &body[0].typ {
            match t {
                IntrinsicType::OPENPAREN => {
                    let org_count = bracketcount;
                    bracketcount += 1;
                    let index_from = i;
                    i += 1;
                    while bracketcount > org_count {
                        if let Some(tok) = body.get(i) {
                            match tok.typ {
                                TokenType::IntrinsicType(it) => {
                                    match it {
                                        IntrinsicType::OPENPAREN => bracketcount += 1,
                                        IntrinsicType::CLOSEPAREN => bracketcount -= 1,
                                        _ =>{}
                                    }
                                }
                                _ => {}
                            }
                        }
                        else {
                            par_error!(token, "Open paren opened here but never closed!");
                        }
                        i += 1;
                    }
                    let expr = tokens_to_expression(&body[index_from..i-1], build, program, locals,buffers);
                    
                    // if currentETree.left.is_none()  && currentETree.right.is_none() && currentETree.op == Op::NONE && expr.is_expr(){
                    //     currentETree = *(*expr.unwrap_expr()).clone();
                    // }
                    //else
                    if currentETree.left.is_none() && currentETree.op == Op::NONE{
                        currentETree.left = Some(expr);
                    }
                    else if currentETree.right.is_none() && currentETree.op != Op::NONE {
                        currentETree.right = Some(expr);
                    }
                    else {
                        par_error!(token, "This should be unreachable!");
                    }
                }
                _ => par_error!(token, "Error: Unexpected intrinsic type: {} in expression!",t.to_string(false))

            }
        }
        else {
            par_error!(token, "Error: unknown token type in expression: {}",token.typ.to_string(false))
        }
    }
    if currentETree.right.is_none() && currentETree.op == Op::NONE && currentETree.left.is_some() {
        return currentETree.left.unwrap()
    }
    Expression::expr(Box::new(currentETree))
}
#[derive(Debug,PartialEq,Clone)]
enum TokenType {
    WordType      (String),
    Register      (Register),
    IntrinsicType (IntrinsicType),
    Definition    (VarType),
    Function      (String),
    StringType    (String),
    CStringType   (String),
    CharType      (char),
    Number8       (i8),
    Number16      (i16),
    Number32      (i32),
    Number64      (i64),
    Operation     (Op),
    SETOperation  (SetOp)
}
impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_string(false))
    }
}

impl TokenType {
    fn unwrap_numeric(&self, build: &BuildProgram) -> Option<i64> {
        match self {
            TokenType::Number8(data)   => Some(data.clone() as i64),
            TokenType::Number16(data) => Some(data.clone() as i64),
            TokenType::Number32(data) => Some(data.clone() as i64),
            TokenType::Number64(data) => Some(data.clone() as i64),
            TokenType::WordType(data) => {
                let t = build.constdefs.get(data)?;
                match &t.typ {
                    RawConstValueType::INT(data)=> Some(data.clone() as i64),
                    &RawConstValueType::LONG(data)    => Some(data.clone() as i64),
                    _ => None
                }
            }
            _ => None
        }
    }
    fn unwrap_setop(&self) -> &SetOp {
        match self {
            Self::SETOperation(op) => op,
            _ => panic!("Unreachable")
        }
    }
    fn is_setop(&self) -> bool {
        match self {
            Self::SETOperation(_) => true,
            _ => false
        }
    }
    fn to_string(&self,isplural:bool) -> String {
        match self {
            TokenType::WordType(word) => {
                if isplural {format!("Words").to_string()} else {format!("Word {}",word).to_string()}
            },
            TokenType::IntrinsicType(typ) => {
                if isplural {format!("Intrinsics").to_string()} else {format!("Intrinsic {}",typ.to_string(false)).to_string()}
            }
            TokenType::StringType(st) => {
                if isplural {format!("String").to_string()} else {format!("String {}",st).to_string()}
            }
            TokenType::CharType(chr) => {
                if isplural {"Chars".to_string()} else {format!("Char {}",chr).to_string()}
            }
            TokenType::Number8(num) => {
                if isplural {"Numbers(8)".to_string()} else {format!("Number(8) {}",num).to_string()}
            }
            TokenType::Number16(num) => {
                if isplural {"Numbers(16)".to_string()} else {format!("Number(16) {}",num).to_string()}
            }
            TokenType::Number32(num) => {
                if isplural {"Numbers(32)".to_string()} else {format!("Number(32) {}",num).to_string()}
            }
            TokenType::Number64(num) => {
                if isplural {"Numbers(64)".to_string()} else {format!("Number(64) {}",num).to_string()}
            }
            TokenType::Definition(typ) => {
                if isplural {"Definitions".to_string()} else {format!("Definition {}",typ.to_string(false)).to_string()}
            }
            TokenType::CStringType(st) => {
                if isplural {format!("CString").to_string()} else {format!("CString {}",st).to_string()}
            },
            TokenType::Function(name) => {
                if isplural {format!("Functions").to_string()} else {format!("Function {}",name).to_string()}
            },
            TokenType::Register(reg) => {
                if isplural {format!("Registers").to_string()} else {format!("Register {}",reg.to_string()).to_string()}
            },
            TokenType::Operation(op) => {
                if isplural {format!("Operations").to_string()} else {format!("Operation {}",op.to_string()).to_string()}
            },
            TokenType::SETOperation(sop) => {
                if isplural {format!("Set Operations").to_string()} else {format!("Set Operation {}",sop.to_string()).to_string()}
            },

        }
    }
}
#[derive(Clone,Debug, PartialEq)]
struct ProgramLocation {
    file: Rc<String>,
    linenumber: i32,
    character:  i32,
}
impl ProgramLocation {
    fn loc_display(&self) -> String{
        format!("{}:{}:{}",self.file,self.linenumber,self.character)
    }
}
#[derive(Debug, Clone)]
struct Token {
    typ: TokenType,
    location: ProgramLocation
}
impl Token {
    fn loc_display(&self) -> String {
        self.location.loc_display()
    }
    fn is_word(&self) -> bool {
        match self.typ {
            TokenType::WordType(_) => true,
            _ => false
        }
    }
    fn unwrap_word(&self) -> Option<&String> {
        match &self.typ {
            TokenType::WordType(data) => Some(data),
            _ => None
        }
    }
    fn unwrap_string(&self) -> Option<&String> {
        match &self.typ {
            TokenType::StringType(data) | TokenType::CStringType(data) => Some(data),
            _ => None
        }
    }
}
struct Lexer<'a> {
    source: Vec<char>,
    cursor: usize,
    currentLocation: ProgramLocation,
    Intrinsics: &'a HashMap<String,IntrinsicType>,
    Definitions: &'a HashMap<String,VarType>,
    CurrentFuncs: HashSet<String>
}

impl<'a> Lexer<'a> {
    fn is_newline(&mut self) -> bool {
        if let Some(c) = self.cchar_s() {
            c=='\n' || c=='\r'
        }
        else {
            true
        }

    }
    fn trim_left(&mut self) -> bool {
        if !self.is_not_empty(){
            return false;
        }
        while self.is_not_empty() && self.source.get(self.cursor).unwrap().is_whitespace() {
            if self.source.get(self.cursor).unwrap() == &'\n' {
                self.currentLocation.linenumber += 1;
                self.currentLocation.character = 0;
            }
            if self.cursor >= self.source.len(){
                break;
            }
            self.cursor += 1;
            self.currentLocation.character+=1;
        }
        true
    }
    fn is_not_empty(&self) -> bool {
        self.cursor < self.source.len()
    }
    fn is_not_empty_offset(&self, offset: usize) -> bool {
        self.cursor+offset < self.source.len()
    }
    fn new(source: &'a String, Intrinsics: &'a HashMap<String, IntrinsicType>, Definitions: &'a HashMap<String,VarType>, CurrentFuncs: HashSet<String>) -> Self {
        Self {
            source: source.chars().collect(),
            cursor: 0,
            currentLocation: ProgramLocation { file: Rc::new(String::new()), linenumber: 1, character: 0 },
            Intrinsics,
            Definitions,
            CurrentFuncs,
        }
    }
    fn drop_char(&mut self){
        if self.is_not_empty() {
            let c = self.cchar_s();
            if let Some(c) = c {
                if c == '\n' {
                    self.currentLocation.linenumber += 1;
                    self.currentLocation.character = 0;
                } else {
                    self.currentLocation.character += 1;
                }
                self.cursor += 1;
            }
        }
    }
    fn drop_line(&mut self) {
        while self.is_not_empty() && self.source.get(self.cursor).unwrap() != &'\n' {
            self.cursor += 1;
        }
        self.cursor += 1;
        self.currentLocation.character = 0;
        self.currentLocation.linenumber += 1;
    }
    fn loc_display(&self) -> String {
        self.currentLocation.loc_display()
    }
    fn pop_symbol(&mut self) -> Option<String> {
        let mut o = String::new();
        let mut c = self.cchar_s()?;
        if !c.is_alphabetic() && c != '_'{
            return None;
        }
        while self.is_not_empty() && c.is_alphanumeric() || c=='_'{
            c = self.cchar_s()?;
            self.cursor += 1;
            o.push(c);
        }
        o.pop();
        self.cursor -= 1;
        self.currentLocation.character += o.len() as i32;
        Some(o)
    }
    fn cchar(&mut self) -> char {
        self.source.get(self.cursor).unwrap().clone()
    }
    fn cchar_s(&mut self) -> Option<char> {
        self.source.get(self.cursor).clone().copied()
    }
    fn cchar_offset(&mut self, offset: usize) -> Option<char> {
        if self.is_not_empty_offset(offset) {
            return Some(self.source.get(self.cursor+offset).unwrap().clone())
        }
        None
    }



    fn next_line(&mut self) -> Option<Vec<Token>> {
        self.trim_left();
        let mut o: Vec<Token> = Vec::new();
        while let Some(t) = self.next(){
            o.push(t);
            if self.source[self.cursor]  == '\n' {
                break;
            }
        }
        Some(o)
    }
}
impl Iterator for Lexer<'_> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.trim_left();
        let mut outstr: String = Default::default();
        if self.is_not_empty() {
            let mut c = self.cchar();
            match c {
                '"' => {
                    let mut shouldIgnoreNext: bool = false;
                    self.cursor += 1;
                    self.currentLocation.character += 1;
                    c = self.cchar();
                    while self.is_not_empty() && (shouldIgnoreNext || c != '\"'){
                        c = self.cchar_s()?;
                        if shouldIgnoreNext {
                            shouldIgnoreNext = false
                        }
                        if c == '\\' {
                            shouldIgnoreNext = true
                        }
                        outstr.push(c);
                        self.cursor+=1;
                    }
                    outstr.pop();
                    self.currentLocation.character += 2+outstr.len() as i32;
                    if self.is_not_empty() {
                        let outstr = unescape(&outstr);
                        if self.cchar() == 'c' {
                            self.cursor += 1;
                            self.currentLocation.character += 1;
                            return Some(Token { typ: TokenType::CStringType(outstr), location: self.currentLocation.clone() });
                        }
                        else if self.cchar() == 's' {
                            self.cursor += 1;
                            self.currentLocation.character += 1;
                            return Some(Token { typ: TokenType::StringType(outstr), location: self.currentLocation.clone() });
                        }
                        else {
                            return Some(Token { typ: TokenType::CStringType(outstr), location: self.currentLocation.clone() });
                        }
                    }
                }
                '\'' => {
                    let mut c_char = self.cchar_s()?;
                    self.cursor += 1;
                    self.currentLocation.character += 1;
                    if c_char == '\\'{
                        c_char = match self.cchar_s()? {
                            'r' => {
                                '\r'
                            }
                            't' => {
                                '\t'
                            }
                            'n' => {
                                '\n'
                            }
                            v => {
                              v
                            }
                        };
                        self.cursor += 1;
                        self.currentLocation.character += 1;
                    }
                    assert_eq!(self.cchar_s()?, '\'', "Error:{}: Unclosed char token! Expected ' but found {}",self.currentLocation.loc_display(),self.cchar_s()?);
                    self.cursor += 1;
                    self.currentLocation.character += 1;
                    return Some(Token { typ: TokenType::CharType(c_char), location: self.currentLocation.clone() });
                }
                '/' => {
                    self.cursor += 1;
                    self.currentLocation.character += 1;
                    if let Some(nc) = self.cchar_s() {
                        if nc == '/' {
                            self.drop_line();
                            return self.next();
                        }
                        else if nc == '=' {
                            self.cursor += 1;
                            self.currentLocation.character += 1;
                            return Some(Token { typ: TokenType::SETOperation(SetOp::DIVSET), location: self.currentLocation.clone() });
                        }
                        else {
                            return Some(Token { typ: TokenType::Operation(Op::DIV), location: self.currentLocation.clone() });
                        }
                    }
                    else {
                        panic!("Error: Abruptly ran out of chars!");
                    }

                }
                '@' => {
                    self.cursor += 1;
                    self.currentLocation.character += 1;
                    let outstr = "@".to_owned()+&self.pop_symbol()?;
                    if let Some(Intr) = self.Intrinsics.get(&outstr) {
                        return Some(Token { typ: TokenType::IntrinsicType(Intr.clone()), location: self.currentLocation.clone() })
                    }
                    else {
                        lex_error!(self, "Error: Expected Intrinsic but found {}. Cannot have @ in variable names, function names etc.",outstr)
                    }
                }
                _    => {
                    let _orgcursor = self.cursor;
                    if !self.is_not_empty() {
                        return None;
                    }
                    if c.is_alphabetic() || c == '_' {
                        let outstr = self.pop_symbol()?;
                        if let Some(o) = self.Intrinsics.get(&outstr) {
                            return Some(Token { typ: TokenType::IntrinsicType(o.clone()), location: self.currentLocation.clone() });
                        }
                        else if let Some(o) = self.Definitions.get(&outstr){
                            return Some(Token { typ: TokenType::Definition(o.clone()), location: self.currentLocation.clone() });
                        }
                        else if let Some(reg) = Register::from_string(&outstr) {
                            return Some(Token { typ: TokenType::Register(reg), location: self.currentLocation.clone() })
                        }
                        else if let Some(op) = Op::from_str(&outstr) {
                            return Some(Token { typ: TokenType::Operation(op), location: self.currentLocation.clone() })
                        }
                        else if let Some(op) = SetOp::from_str(&outstr) {
                            return Some(Token { typ: TokenType::SETOperation(op), location: self.currentLocation.clone() })
                        }
                        else if self.CurrentFuncs.contains(&outstr) {
                            return Some(Token { typ: TokenType::Function(outstr), location: self.currentLocation.clone() })
                        }
                        else{
                            return Some(Token { typ: TokenType::WordType(outstr), location: self.currentLocation.clone() });
                        }
                    }
                    else if (c == '-' && self.cchar_offset(1)?.is_numeric()) || c.is_numeric(){
                        if c=='0' && self.cchar_offset(1).is_some() && self.cchar_offset(1).unwrap() == 'x' {
                            self.cursor += 2;
                            self.currentLocation.character += 2;
                            while self.is_not_empty() && c.is_ascii_hexdigit() {
                                c = self.cchar_s()?;
                                if c == '_' {

                                    self.cursor+=1;
                                    c = self.cchar_s()?;
                                    continue;
                                }
                                self.cursor += 1;
                                outstr.push(c);
                            }
                            outstr.pop();
                            self.cursor -= 1;
                            self.currentLocation.character += outstr.len() as i32;
                            if let Some(nc) = self.cchar_s() {
                                match nc {
                                    'l' =>  {
                                        self.cursor += 1;
                                        self.currentLocation.character += 1;
                                        if let Ok(val) =  i64::from_str_radix(&outstr, 16) {
                                            return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number64(val)});
                                        }
                                        else {
                                            todo!("Error message for this")
                                        }
                                    }
                                    'i' => {
                                        self.cursor += 1;
                                        self.currentLocation.character += 1;
                                        if let Ok(val) =  i32::from_str_radix(&outstr, 16) {
                                            return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number32(val)});
                                        }
                                        else {
                                            todo!("Error message for this")
                                        }
                                    }
                                    's' => {
                                        self.cursor += 1;
                                        self.currentLocation.character += 1;
                                        if let Ok(val) =  i16::from_str_radix(&outstr, 16) {
                                            return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number16(val)});
                                        }
                                        else {
                                            todo!("Error message for this")
                                        }
                                    }
                                    'c' => {
                                        self.cursor += 1;
                                        self.currentLocation.character += 1;
                                        if let Ok(val) =  i8::from_str_radix(&outstr, 16) {
                                            return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number8(val)});
                                        }
                                        else {
                                            todo!("Error message for this")
                                        }
                                    }
                                    _ => {
                                        if let Ok(val) = i32::from_str_radix(&outstr, 16) {
                                            return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number32(val)});
                                        }
                                        else {
                                            todo!("Error message for this: {}: \"{}\"",self.currentLocation.loc_display(),outstr);
                                        }
                                    }
                                }
                                
                                
                            }
                            else {
                                if let Ok(val) =i32::from_str_radix(&outstr, 16) {
                                    return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number32(val)});
                                }
                                else if let Ok(val) = i64::from_str_radix(&outstr, 16) {
                                    return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number64(val)});
                                }
                                else {
                                    panic!("Unknown number combo: {}",outstr);
                                }
                            }
                        }
                        if c=='-' {
                            outstr.push(c);
                            self.cursor += 1;
                            self.currentLocation.character += 1;
                            c = self.cchar_s()?;
                        }
                        while self.is_not_empty() && c.is_numeric() {
                            c = self.cchar_s()?;
                            if c == '_' {
                                self.cursor+=1;
                                c = self.cchar_s()?;
                                continue;
                            }
                            self.cursor += 1;
                            outstr.push(c);
                        }
                        outstr.pop();
                        self.cursor -= 1;
                        self.currentLocation.character += outstr.len() as i32;
                        if let Some(nc) = self.cchar_s() {
                            match nc {
                                'l' => {
                                    self.cursor += 1;
                                    self.currentLocation.character += 1;
                                    if let Ok(val) = outstr.parse::<i64>() {
                                        return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number64(val)});
                                    }
                                    else {
                                        todo!("Error message for this")
                                    }
                                }
                                'i' => {
                                    self.cursor += 1;
                                    self.currentLocation.character += 1;
                                    if let Ok(val) = outstr.parse::<i32>() {
                                        return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number32(val)});
                                    }
                                    else {
                                        todo!("Error message for this")
                                    }
                                }
                                's' => {
                                    self.cursor += 1;
                                    self.currentLocation.character += 1;
                                    if let Ok(val) = outstr.parse::<i16>() {
                                        return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number16(val)});
                                    }
                                    else {
                                        todo!("Error message for this")
                                    }
                                }
                                'c' => {
                                    self.cursor += 1;
                                    self.currentLocation.character += 1;
                                    if let Ok(val) = outstr.parse::<i8>() {
                                        return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number8(val)});
                                    }
                                    else {
                                        todo!("Error message for this")
                                    }
                                }
                                _ => {
                                    if let Ok(val) = outstr.parse::<i32>() {
                                        return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number32(val)});
                                    }
                                    else {
                                        todo!("Error message for this: {}: \"{}\"",self.currentLocation.loc_display(),outstr);
                                    }
                                }
                            }
                        }
                        else {
                            if let Ok(val) = outstr.parse::<i32>() {
                                return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number32(val)});
                            }
                            else if let Ok(val) = outstr.parse::<i64>() {
                                return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number64(val)});
                            }
                            else {
                                panic!("Unknown number combo: {}",outstr);
                            }
                        }
                    }
                    else if c == ';' || c==')' || c=='(' || c=='{' || c=='}' || c=='[' || c==']' || c == ',' {
                        self.cursor += 1;
                        self.currentLocation.character += 1;
                        return Some(Token {typ: TokenType::IntrinsicType(self.Intrinsics.get(&c.to_string()).expect(&format!("Unhandled intrinsic :( {}",c)).clone()), location: self.currentLocation.clone()});
                    }
                    else if c == '*' && self.cchar_offset(1).is_some(){
                        let mut already_alphabetic = false;
                        while self.is_not_empty() && (c == '*'  && !already_alphabetic) || c.is_alphabetic(){
                            c = self.cchar_s()?;
                            if c=='=' && outstr=="*" {
                                outstr+="= ";
                                self.cursor+=2;
                                break;
                            }
                            self.cursor += 1;
                            outstr.push(c);
                            if c.is_alphabetic() {
                                already_alphabetic = true
                            }
                        }
                        outstr.pop();
                        self.cursor -= 1;
                        self.currentLocation.character += outstr.len() as i32;
                        if outstr == "*" {
                            return Some(Token { typ: TokenType::Operation(Op::STAR), location: self.currentLocation.clone() });
                        }
                        else if outstr=="*=" {
                            return Some(Token { typ: TokenType::SETOperation(SetOp::MULSET), location: self.currentLocation.clone() });
                        }
                        else {
                            let osize = outstr.chars().take_while(|&c| c == '*').count();
                            let otyp = &outstr[osize..];
                            if let Some(def) = self.Definitions.get(otyp) {
                                return Some(Token { typ: TokenType::Definition(VarType::PTR(Ptr{typ: PtrTyp::TYP(Box::new(def.clone())), inner_ref: osize-1})), location: self.currentLocation.clone() });
                            } else if otyp == "void" {
                                return Some(Token { typ: TokenType::Definition(VarType::PTR(Ptr{typ: PtrTyp::VOID, inner_ref: osize-1})), location: self.currentLocation.clone() });
                            }
                            else {
                                self.cursor -= outstr.len()-1;
                                self.currentLocation.character -= (outstr.len()-1) as i32;
                                
                                return Some(Token { typ: TokenType::Operation(Op::STAR), location: self.currentLocation.clone() });
                            }
                        }
                    }
                    else {
                        while self.is_not_empty() && !c.is_alphabetic() && !c.is_numeric() && !c.is_whitespace() && c != ';' && c!=')' && c!='(' && c!='{' && c!='}' && c!='[' && c!=']'{
                            c = self.cchar_s()?;
                            self.cursor += 1;
                            outstr.push(c);
                        }
                        outstr.pop();
                        self.cursor -= 1;
                        self.currentLocation.character += outstr.len() as i32;

                        if let Some(intrinsic) = self.Intrinsics.get(&outstr) {
                            return Some(Token { typ: TokenType::IntrinsicType(intrinsic.clone()), location: self.currentLocation.clone() })
                        }
                        else if let Some(op) = Op::from_str(&outstr) {
                            return Some(Token { typ: TokenType::Operation(op), location: self.currentLocation.clone() })
                        }
                        else if let Some(op) = SetOp::from_str(&outstr) {
                            return Some(Token { typ: TokenType::SETOperation(op), location: self.currentLocation.clone() })
                        }
                        else if self.CurrentFuncs.contains(&outstr) {
                            return Some(Token { typ: TokenType::Function(outstr), location: self.currentLocation.clone() })
                        }
                        else {
                            return  Some(Token { typ: TokenType::WordType(outstr), location: self.currentLocation.clone() });
                        }
                    }
                }
            }
        }

        None

    }
}
#[derive(Debug,Clone)]
enum ExternalType {
    RawExternal,
    CExternal
}
#[derive(Debug,Clone)]
struct External {
    typ: ExternalType,
    loc: ProgramLocation,
    contract: Option<AnyContract>
}
impl ExternalType {
    fn prefix(&self, program: &CmdProgram) -> String {
        match self {
        ExternalType::RawExternal => String::new(),
            ExternalType::CExternal => program.architecture.cextern_prefix.clone(),
            _ => String::new()
        }
    }
    fn suffix(&self) -> String {
        match self {
            ExternalType::RawExternal => String::new(),
            ExternalType::CExternal   => String::new(),
            _ => String::new()
        }
    }
    fn to_string(&self) -> String{
        match self {
            ExternalType::RawExternal => "RawExternal".to_string(),
            ExternalType::CExternal => "CExternal".to_string()
        }
    }
}
#[repr(u32)]
#[derive(Clone, Copy,Debug, PartialEq)]
enum Register {
    // 64 bit
    RAX,
    RBX,
    RCX,
    RDX,
    RSP,
    RBP,
    RSI,
    RDI,
    // 32 bit
    EAX,
    EBX,
    ECX,
    EDX,
    ESP,
    EBP,
    ESI,
    EDI,
    // 16 bit
    AX,
    BX,
    CX,
    DX,
    SP,
    BP,
    SI,
    DI,
    // 8 bit low
    AL,
    BL,
    CL,
    DL,
    SIL,
    DIL,
    // 8 bit high
    AH,
    BH,
    CH,
    DH,



    // General Purpose registers
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    // General Purpose registers 32
    R8D,
    R9D,
    R10D,
    R11D,
    R12D,
    R13D,
    R14D,
    R15D,
    // General Purpose registers 16
    R8W,
    R9W,
    R10W,
    R11W,
    R12W,
    R13W,
    R14W,
    R15W,
    // General Purpose registers 8
    R8B,
    R9B,
    R10B,
    R11B,
    R12B,
    R13B,
    R14B,
    R15B,

    //Floating point 32 arithmetics
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,

}
impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_string())
    }
}
impl Register {
    fn to_string(&self) -> String {
        match self {
            Register::RAX => "rax".to_string(),
            Register::RBX => "rbx".to_string(),
            Register::RCX => "rcx".to_string(),
            Register::RDX => "rdx".to_string(),
            Register::RSP => "rsp".to_string(),
            Register::RBP => "rbp".to_string(),
            Register::RSI => "rsi".to_string(),
            Register::RDI => "rdi".to_string(),
            Register::EAX => "eax".to_string(),
            Register::EBX => "ebx".to_string(),
            Register::ECX => "ecx".to_string(),
            Register::EDX => "edx".to_string(),
            Register::ESP => "esp".to_string(),
            Register::EBP => "ebp".to_string(),
            Register::AX  => "ax".to_string(),
            Register::BX  => "bx".to_string(),
            Register::CX  => "cx".to_string(),
            Register::DX  => "dx".to_string(),
            Register::SP  => "sp".to_string(),
            Register::BP  => "bp".to_string(),
            Register::AL  => "al".to_string(),
            Register::BL  => "bl".to_string(),
            Register::CL  => "cl".to_string(),
            Register::DL  => "dl".to_string(),
            Register::AH  => "ah".to_string(),
            Register::BH  => "bh".to_string(),
            Register::CH  => "ch".to_string(),
            Register::DH  => "dh".to_string(),
            Register::R8  => "r8".to_string(),
            Register::R9  => "r9".to_string(),
            Register::R10 => "r10".to_string(),
            Register::R11 => "r11".to_string(),
            Register::R12 => "r12".to_string(),
            Register::R13 => "r13".to_string(),
            Register::R14 => "r14".to_string(),
            Register::R15 => "r15".to_string(),
            Register::XMM0 => "xmm0".to_string(),
            Register::XMM1 => "xmm1".to_string(),
            Register::XMM2 => "xmm2".to_string(),
            Register::XMM3 => "xmm3".to_string(),
            Register::XMM4 => "xmm4".to_string(),
            Register::XMM5 => "xmm5".to_string(),
            Register::XMM6 => "xmm6".to_string(),
            Register::XMM7 => "xmm7".to_string(),
            Register::ESI => "esi".to_string(),
            Register::EDI => "edi".to_string(),
            Register::SI  => "si".to_string(),
            Register::DI  => "di".to_string(),
            Register::SIL => "sil".to_string(),
            Register::DIL => "dil".to_string(),

            Register::R8D  => "r8d".to_string(),
            Register::R9D  => "r9d".to_string(),
            Register::R10D => "r10d".to_string(),
            Register::R11D => "r11d".to_string(),
            Register::R12D => "r12d".to_string(),
            Register::R13D => "r13d".to_string(),
            Register::R14D => "r14d".to_string(),
            Register::R15D => "r15d".to_string(),
            Register::R8W  => "r8w".to_string(),
            Register::R9W  => "r9w".to_string(),
            Register::R10W => "r10w".to_string(),
            Register::R11W => "r11w".to_string(),
            Register::R12W => "r12w".to_string(),
            Register::R13W => "r13w".to_string(),
            Register::R14W => "r14w".to_string(),
            Register::R15W => "r15w".to_string(),
            Register::R8B  => "r8b".to_string(),
            Register::R9B  => "r9b".to_string(),
            Register::R10B => "r10b".to_string(),
            Register::R11B => "r11b".to_string(),
            Register::R12B => "r12b".to_string(),
            Register::R13B => "r13b".to_string(),
            Register::R14B => "r14b".to_string(),
            Register::R15B => "r15b".to_string(),
        }
    }
    fn from_string(stri: &String) -> Option<Self> {
        match stri.as_str() {
             "RAX"  | "rax"  => Some(Register::RAX),
             "RBX"  | "rbx"  => Some(Register::RBX),
             "RCX"  | "rcx"  => Some(Register::RCX),
             "RDX"  | "rdx"  => Some(Register::RDX),
             "RSP"  | "rsp"  => Some(Register::RSP),
             "RBP"  | "rbp"  => Some(Register::RBP),
             "EAX"  | "eax"  => Some(Register::EAX),
             "EBX"  | "ebx"  => Some(Register::EBX),
             "ECX"  | "ecx"  => Some(Register::ECX),
             "EDX"  | "edx"  => Some(Register::EDX),
             "ESP"  | "esp"  => Some(Register::ESP),
             "EBP"  | "ebp"  => Some(Register::EBP),
             "AX"   | "ax"   => Some(Register::AX ),
             "BX"   | "bx"   => Some(Register::BX ),
             "CX"   | "cx"   => Some(Register::CX ),
             "DX"   | "dx"   => Some(Register::DX ),
             "SP"   | "sp"   => Some(Register::SP ),
             "BP"   | "bp"   => Some(Register::BP ),
             "AL"   | "al"   => Some(Register::AL ),
             "BL"   | "bl"   => Some(Register::BL ),
             "CL"   | "cl"   => Some(Register::CL ),
             "DL"   | "dl"   => Some(Register::DL ),
             "AH"   | "ah"   => Some(Register::AH ),
             "BH"   | "bh"   => Some(Register::BH ),
             "CH"   | "ch"   => Some(Register::CH ),
             "DH"   | "dh"   => Some(Register::DH ),
             "R8"   | "r8"   => Some(Register::R8),
             "R9"   | "r9"   => Some(Register::R9),
             "R10"  | "r10"  => Some(Register::R10),
             "R11"  | "r11"  => Some(Register::R11),
             "R12"  | "r12"  => Some(Register::R12),
             "R13"  | "r13"  => Some(Register::R13),
             "R14"  | "r14"  => Some(Register::R14),
             "R15"  | "r15"  => Some(Register::R15),
             "XMM0" | "XMM0" => Some(Register::XMM0),
             "XMM1" | "XMM1" => Some(Register::XMM1),
             "XMM2" | "XMM2" => Some(Register::XMM2),
             "XMM3" | "XMM3" => Some(Register::XMM3),
             "RSI" | "rsi" => Some(Register::RSI),
             "RDI" | "rdi" => Some(Register::RDI),

             "R8D" | "r8d" => Some(Register::R8D),

            _ => None
        }
        //TODO ^ Add the rest of the registers as strings
    }
    fn size(&self) -> usize {
        match self {
            Register::RAX  => 8,
            Register::RBX  => 8,
            Register::RCX  => 8,
            Register::RDX  => 8,
            Register::RSP  => 8,
            Register::RBP  => 8,
            Register::RSI  => 8,
            Register::RDI  => 8,
            Register::EAX  => 4,
            Register::EBX  => 4,
            Register::ECX  => 4,
            Register::EDX  => 4,
            Register::ESP  => 4,
            Register::EBP  => 4,
            Register::ESI  => 4,
            Register::EDI  => 4,
            Register::AX   => 2,
            Register::BX   => 2,
            Register::CX   => 2,
            Register::DX   => 2,
            Register::SP   => 2,
            Register::BP   => 2,
            Register::SI   => 2,
            Register::DI   => 2,
            Register::AL   => 1,
            Register::BL   => 1,
            Register::CL   => 1,
            Register::DL   => 1,
            Register::AH   => 1,
            Register::BH   => 1,
            Register::CH   => 1,
            Register::DH   => 1,
            Register::SIL  => 1,
            Register::DIL  => 1,
            Register::RSI  => 8,
            Register::RDI  => 8,
            Register::R8   => 8,
            Register::R9   => 8,
            Register::R10  => 8,
            Register::R11  => 8,
            Register::R12  => 8,
            Register::R13  => 8,
            Register::R14  => 8,
            Register::R15  => 8,
            Register::XMM0 => 4,
            Register::XMM1 => 4,
            Register::XMM2 => 4,
            Register::XMM3 => 4,
            Register::XMM4 => 4,
            Register::XMM5 => 4,
            Register::XMM6 => 4,
            Register::XMM7 => 4,
            Register::R8D  => 4,
            Register::R9D  => 4,
            Register::R10D => 4,
            Register::R11D => 4,
            Register::R12D => 4,
            Register::R13D => 4,
            Register::R14D => 4,
            Register::R15D => 4,
            Register::R8W  => 2,
            Register::R9W  => 2,
            Register::R10W => 2,
            Register::R11W => 2,
            Register::R12W => 2,
            Register::R13W => 2,
            Register::R14W => 2,
            Register::R15W => 2,
            Register::R8B  => 1,
            Register::R9B  => 1,
            Register::R10B => 1,
            Register::R11B => 1,
            Register::R12B => 1,
            Register::R13B => 1,
            Register::R14B => 1,
            Register::R15B => 1,
        }
    }
    fn to_byte_size(&self, size: usize) -> Self {
        match size {
            8 => {
              match self {
                Register::RAX | Register::EAX  | Register::AX | Register::AH | Register::AL => Register::RAX,
                Register::RBX | Register::EBX  | Register::BX | Register::BH | Register::BL => Register::RBX,
                Register::RCX | Register::ECX  | Register::CX | Register::CH | Register::CL => Register::RCX,
                Register::RDX | Register::EDX  | Register::DX | Register::DH | Register::DL => Register::RDX,
                Register::RSI | Register::ESI | Register::SI  | Register::SIL => Register::RSI,
                Register::RDI | Register::EDI | Register::DI  | Register::DIL => Register::RDI,
                Register::RSP | Register::ESP  | Register::SP => Register::RSP,
                Register::RBP | Register::EBP  | Register::BP => Register::RBP,
                Register::R8  | Register::R8D | Register::R8W | Register::R8B => Register::R8,
                Register::R9  | Register::R9D | Register::R9W | Register::R9B => Register::R9,
                Register::R10  | Register::R10D | Register::R10W | Register::R10B => Register::R10,
                Register::R11  | Register::R11D | Register::R11W | Register::R11B => Register::R11,
                Register::R12  | Register::R12D | Register::R12W | Register::R12B => Register::R12,
                Register::R13  | Register::R13D | Register::R13W | Register::R13B => Register::R13,
                Register::R14  | Register::R14D | Register::R14W | Register::R14B => Register::R14,
                Register::R15  | Register::R15D | Register::R15W | Register::R15B => Register::R15,
                Register::XMM0 => todo!(),
                Register::XMM1 => todo!(),
                Register::XMM2 => todo!(),
                Register::XMM3 => todo!(),
                Register::XMM4 => todo!(),
                Register::XMM5 => todo!(),
                Register::XMM6 => todo!(),
                Register::XMM7 => todo!(),
              }
            },
            4 => {
                match self {
                    Register::RAX | Register::EAX  | Register::AX | Register::AH | Register::AL => Register::EAX,
                    Register::RBX | Register::EBX  | Register::BX | Register::BH | Register::BL => Register::EBX,
                    Register::RCX | Register::ECX  | Register::CX | Register::CH | Register::CL => Register::ECX,
                    Register::RDX | Register::EDX  | Register::DX | Register::DH | Register::DL => Register::EDX,
                    Register::RSI | Register::ESI | Register::SI  | Register::SIL => Register::ESI,
                    Register::RDI | Register::EDI | Register::DI  | Register::DIL => Register::EDI,
                    Register::RSP | Register::ESP  | Register::SP => Register::ESP,
                    Register::RBP | Register::EBP  | Register::BP => Register::EBP,

                    Register::R8  | Register::R8D | Register::R8W | Register::R8B => Register::R8D,
                    Register::R9  | Register::R9D | Register::R9W | Register::R9B => Register::R9D,
                    Register::R10  | Register::R10D | Register::R10W | Register::R10B => Register::R10D,
                    Register::R11  | Register::R11D | Register::R11W | Register::R11B => Register::R11D,
                    Register::R12  | Register::R12D | Register::R12W | Register::R12B => Register::R12D,
                    Register::R13  | Register::R13D | Register::R13W | Register::R13B => Register::R13D,
                    Register::R14  | Register::R14D | Register::R14W | Register::R14B => Register::R14D,
                    Register::R15  | Register::R15D | Register::R15W | Register::R15B => Register::R15D,
                    _ => todo!()
                }
            },
            2 => {
                match self {
                    Register::RAX | Register::EAX  | Register::AX | Register::AH | Register::AL => {
                        return Register::AX;
                    }
                    Register::RBX | Register::EBX  | Register::BX | Register::BH | Register::BL => {
                        return Register::BX;
                    }
                    Register::RCX | Register::ECX  | Register::CX | Register::CH | Register::CL => {
                        return Register::CX;
                    }
                    Register::RDX | Register::EDX  | Register::DX | Register::DH | Register::DL => {
                        return Register::DX;
                    }
                    Register::RSP | Register::ESP  | Register::SP => {
                        return Register::SP;
                    }
                    Register::RBP | Register::EBP  | Register::BP => {
                        return Register::BP;
                    }
                    Register::RSI | Register::ESI | Register::SI  | Register::SIL => Register::SI,

                    Register::RDI | Register::EDI | Register::DI  | Register::DIL => Register::DI,

                    Register::R8  | Register::R8D | Register::R8W | Register::R8B => Register::R8W,
                    Register::R9  | Register::R9D | Register::R9W | Register::R9B => Register::R9W,
                    Register::R10  | Register::R10D | Register::R10W | Register::R10B => Register::R10W,
                    Register::R11  | Register::R11D | Register::R11W | Register::R11B => Register::R11W,
                    Register::R12  | Register::R12D | Register::R12W | Register::R12B => Register::R12W,
                    Register::R13  | Register::R13D | Register::R13W | Register::R13B => Register::R13W,
                    Register::R14  | Register::R14D | Register::R14W | Register::R14B => Register::R14W,
                    Register::R15  | Register::R15D | Register::R15W | Register::R15B => Register::R15W,
                    _ => todo!()
                  }
            },
            1 => {
                match self {
                    Register::RAX | Register::EAX  | Register::AX | Register::AH | Register::AL => {
                        return Register::AL;
                    }
                    Register::RBX | Register::EBX  | Register::BX | Register::BH | Register::BL => {
                        return Register::BL;
                    }
                    Register::RCX | Register::ECX  | Register::CX | Register::CH | Register::CL => {
                        return Register::CL;
                    }
                    Register::RDX | Register::EDX  | Register::DX | Register::DH | Register::DL => {
                        return Register::DL;
                    }
                    Register::RSP | Register::ESP  | Register::SP => {
                        todo!("Handle rsp error case");
                    }
                    Register::RBP | Register::EBP  | Register::BP => {
                        todo!("Handle rbp error case");
                    }
                    Register::RSI | Register::ESI | Register::SI | Register::SIL  => Register::SIL,
                    Register::RDI | Register::EDI | Register::DI  | Register::DIL => Register::DIL,

                    Register::R8   | Register::R8D  | Register::R8W  | Register::R8B  => Register::R8B,
                    Register::R9   | Register::R9D  | Register::R9W  | Register::R9B  => Register::R9B,
                    Register::R10  | Register::R10D | Register::R10W | Register::R10B => Register::R10B,
                    Register::R11  | Register::R11D | Register::R11W | Register::R11B => Register::R11B,
                    Register::R12  | Register::R12D | Register::R12W | Register::R12B => Register::R12B,
                    Register::R13  | Register::R13D | Register::R13W | Register::R13B => Register::R13B,
                    Register::R14  | Register::R14D | Register::R14W | Register::R14B => Register::R14B,
                    Register::R15  | Register::R15D | Register::R15W | Register::R15B => Register::R15B,
                    _ => todo!()
                  }
            },
            _ => {
                panic!("Unexpected use case for to_bit_size!");
            }
        }
    }
    fn to_var_type(&self) -> VarType {
        match self {
            Register::RAX | Register::RBX | Register::RCX | Register::RDX | Register::RSI | Register::RDI |
            Register::R8 | Register::R9 | Register::R10 | Register::R11 | Register::R12 | Register::R13 | Register::R14 | Register::R15 => {
                VarType::LONG
            },
            Register::RSP | Register::RBP => {
                VarType::PTR(Ptr { typ: PtrTyp::VOID, inner_ref: 0 })
            },
            Register::EAX | Register::EBX | Register::ECX | Register::EDX | Register::ESI | Register::EDI |
            Register::R8D | Register::R9D | Register::R10D | Register::R11D | Register::R12D | Register::R13D | Register::R14D | Register::R15D => {
                VarType::INT
            },
            Register::ESP | Register::EBP => {
                VarType::PTR(Ptr { typ: PtrTyp::VOID, inner_ref: 0})
            }
            Register::AX |Register::BX |Register::CX |Register::DX |Register::SP |Register::BP | Register::SI | Register::DI |
            Register::R8W | Register::R9W | Register::R10W | Register::R11W | Register::R12W | Register::R13W | Register::R14W | Register::R15W => {
                VarType::SHORT
            }
            Register::AL |Register::BL |Register::CL |Register::DL |Register::AH |Register::BH |Register::CH |Register::DH | Register::DIL | Register::SIL |
            Register::R8B | Register::R9B | Register::R10B | Register::R11B | Register::R12B | Register::R13B | Register::R14B | Register::R15B => {
                VarType::CHAR
            }
            Register::R8 |Register::R9 | Register::R10  | Register::R11  | Register::R12  | Register::R13  | Register::R14  | Register::R15 => {
                VarType::LONG
            },
            Register::R8D => {
                VarType::INT
            }
            Register::XMM0 => todo!("floats"),
            Register::XMM1 => todo!("floats"),
            Register::XMM2 => todo!("floats"),
            Register::XMM3 => todo!("floats"),
            Register::XMM4 => todo!("floats"),
            Register::XMM5 => todo!("floats"),
            Register::XMM6 => todo!("floats"),
            Register::XMM7 => todo!("floats"),

        }
    }
}
fn size_to_nasm_type(size: usize) -> String {
    match size {
        1 => {
            "byte".to_string()
        }
        2 => {
            "word".to_string()
        }
        4 => {
            "dword".to_string()
        }
        8 => {
            "qword".to_string()
        }

        _ => {
            panic!("Invalid size  nasm type!");

        }
    }
}
#[derive(Debug,PartialEq,Clone)]
enum CallArgType {
    LOCALVAR(String),
    REGISTER(Register),
    CONSTANT(RawConstValueType)
}
#[derive(Debug,PartialEq,Clone)]
struct CallArg {
    typ: CallArgType,
    loc: ProgramLocation
}
impl CallArg {
    fn from_token(build: &mut BuildProgram, tok: &Token, currentLocals: &Vec<Locals>) -> Option<Self> {
        match &tok.typ {
            TokenType::WordType(word) => {
                if contains_local(currentLocals, word) {
                    return Some(Self { typ: CallArgType::LOCALVAR(word.clone()), loc: tok.location.clone() });
                }
                else if build.constdefs.contains_key(word){
                    return Some(Self { typ: CallArgType::CONSTANT(build.constdefs.get(word).unwrap().typ.clone()), loc: tok.location.clone() })
                }

                None
            },
            TokenType::StringType(val) => {
                //let uuid = build.(ProgramString { Typ: ProgramStringType::STR, Data: val.clone() });
                
                let id = build.insert_new_str(ProgramString { Typ: ProgramStringType::STR, Data: val.clone() });
                Some(Self { typ: CallArgType::CONSTANT(RawConstValueType::STR(id)), loc: tok.location.clone()})
            },
            TokenType::CStringType(val) => {
                let id = build.insert_new_str(ProgramString { Typ: ProgramStringType::CSTR, Data: val.clone() });
                Some(Self { typ: CallArgType::CONSTANT(RawConstValueType::STR(id)), loc: tok.location.clone()})
            },
            TokenType::Number32(val) => {
                Some(Self { typ: CallArgType::CONSTANT(RawConstValueType::INT(val.clone())), loc: tok.location.clone()})
            },
            TokenType::Number64(val) => {
                Some(Self { typ: CallArgType::CONSTANT(RawConstValueType::LONG(val.clone())), loc: tok.location.clone()})
            },
            TokenType::Register(reg) => {
                Some(Self { typ: CallArgType::REGISTER(reg.clone()), loc: tok.location.clone() })
            },
            _ => {
                None
            }
        }
    }
    fn match_any(_contract1: &Vec<Self>, _contract: &AnyContract) -> bool {
        true
    }
}
type CallArgs = Vec<OfP>;

#[derive(Debug, Clone)]
enum OfP {
    REGISTER (Register),
    //PARAM    (String),
    LOCALVAR (String),
    GLOBALVAR(String),
    CONST    (RawConstValueType),
    RESULT   (String,CallArgs),
    BUFFER   (usize)
    // RAW      (i64),
    // STR      (Uuid, ProgramStringType)
    // etc.
}

impl OfP {
    fn to_string(&self, build: &BuildProgram) -> String {
        match self {
            Self::REGISTER(reg)    => reg.to_string(),
            Self::LOCALVAR(v)      => format!("Local var {}",v),
            Self::BUFFER(b)        => format!("Buffer {}",b),
            Self::CONST(v)         => v.to_string(build),
            Self::GLOBALVAR(v)     => format!("Global var {}",v),
            Self::RESULT(v, _)     => format!("Result of {}",v)
        }
    }
    fn var_type_t(&self, build: &BuildProgram, local_vars: &Vec<Locals>, buffers: &Vec<BuildBuf>) -> Option<VarType> {
        
        match self {
            Self::REGISTER(reg) => Some(reg.to_var_type()),
            Self::CONST(v) => Some(v.to_type(build)[0].clone()),
            Self::LOCALVAR(v) => Some(get_local(local_vars,v).unwrap().clone()),
            Self::RESULT(f, _) => {        
                build.functions.get(f)?.contract.Outputs.get(0).cloned()
            }
            Self::BUFFER(i) => {
                Some(VarType::PTR(Ptr::ref_to(buffers[i.to_owned()].typ.clone())))

            },
            Self::GLOBALVAR(_) => todo!()
        }
    }
    fn var_type(&self, build: &BuildProgram, local_vars: &Vec<HashMap<String, LocalVariable>>) -> Option<VarType> {
        match self {
            Self::REGISTER(reg) => Some(reg.to_var_type()),
            Self::CONST(v) => Some(v.to_type(build)[0].clone()),
            Self::LOCALVAR(v) => Some(get_local_build(local_vars, v).unwrap().typ.clone()),
            Self::RESULT(f, _) => build.functions.get(f).unwrap().contract.Outputs.get(0).cloned(),
            Self::BUFFER(_) => todo!(),
            Self::GLOBALVAR(v) => Some(build.global_vars.get(v).unwrap().typ.var_type(build)),
        }
    }
    fn LOIRGNasm(&self, regs: Vec<Register>, f: &mut File, program: &CmdProgram,build: &BuildProgram, local_vars: &Vec<HashMap<String, LocalVariable>>, buffers: &Vec<BuildBuf>, stack_size: usize, loc: &ProgramLocation) -> std::io::Result<Vec<Register>>{
        let mut out: Vec<Register> = Vec::with_capacity(regs.len());
        match self {
            OfP::GLOBALVAR(v) => {
                
                let var= build.global_vars.get(v).unwrap();
                match &var.typ {
                    GlobalVarType::BUFFER(i) => {
                        let reg = regs[0].to_byte_size(8);
                        writeln!(f, "   mov {}, _GLOBL_{}",reg,i)?;
                    }
                    GlobalVarType::CONSTANT(_) => {
                        todo!("I don't know how to handle this yet");
                        //let osize = val.get_size(program);
                        //let reg = regs[0].to_byte_size(8);                        
                        //writeln!(f, "   mov {}, {} [_GLOBL_{}]",reg,size_to_nasm_type(osize),v)?;
                    }
                }
            }
            OfP::REGISTER(reg2) => {
                let reg = regs[0].to_byte_size(reg2.size());
                writeln!(f, "   mov {}, {}",reg.to_string(),reg2.to_string())?;
                out.push(reg)
            },
            OfP::LOCALVAR(val) => {
                let lvar = get_local_build(local_vars, val).unwrap();
                let osize = lvar.typ.get_size(program);
                let reg = regs[0].to_byte_size(osize);
                if stack_size-lvar.operand == 0 {
                    writeln!(f, "   mov {}, {} [{}]",reg.to_string(),size_to_nasm_type(osize),program.stack_ptr())?;
                }
                else {
                    writeln!(f, "   mov {}, {} [{}+{}]",reg.to_string(),size_to_nasm_type(osize),program.stack_ptr(),stack_size-lvar.operand)?;
                }
                out.push(reg);
            },
            OfP::CONST(val) => {
                match val {
                    RawConstValueType::CHAR(val) => {
                        let reg = regs[0].to_byte_size(1);
                        writeln!(f, "   mov {}, {}",reg.to_string(),val)?;
                        out.push(reg);
                    },
                    RawConstValueType::SHORT(val) => {
                        let reg = regs[0].to_byte_size(2);
                        writeln!(f, "   mov {}, {}",reg.to_string(),val)?;
                        out.push(reg);
                    },
                    RawConstValueType::INT(val) => {
                        let reg = regs[0].to_byte_size(4);
                        writeln!(f, "   mov {}, {}",reg.to_string(),val)?;
                        out.push(reg);
                    },
                    RawConstValueType::LONG(val) => {
                        let reg = regs[0].to_byte_size(8);
                        writeln!(f, "   mov {}, {}",reg.to_string(),val)?;
                        out.push(reg);
                    },
                    RawConstValueType::STR(index) => {
                        let str = &build.stringdefs[index.to_owned()];
                        match str.Typ {
                            ProgramStringType::STR => {
                                com_assert!(loc,regs.len() > 1, "Error: Cannot load Ofp into register!");
                                let reg1 = regs[1];
                                let reg = if program.architecture.bits == 32 {
                                    let reg = regs[0].to_byte_size(4);
                                    writeln!(f, "   mov {}, $STRING_{}_",reg.to_string(),index)?;
                                    reg
                                }
                                else {
                                    let reg = regs[0].to_byte_size(8);
                                    writeln!(f, "   lea {}, [rel $STRING_{}_]",reg.to_string(),index)?;
                                    reg
                                };
                                writeln!(f, "   mov {}, {}",reg1.to_string(),str.Data.len())?;
                                out.push(reg);
                                out.push(reg1);
                            },
                            ProgramStringType::CSTR => {
                                let reg = if program.architecture.bits == 32 {
                                    let reg = regs[0].to_byte_size(4);
                                    writeln!(f, "   mov {}, $STRING_{}_",reg.to_string(),index)?;
                                    reg
                                }
                                else {
                                    let reg = regs[0].to_byte_size(8);
                                    writeln!(f, "   lea {}, [rel $STRING_{}_]",reg.to_string(),index)?;
                                    reg
                                };
                                out.push(reg);
                            },
                        }
                    },
                    RawConstValueType::PTR(_,val) => {
                        let reg = regs[0].to_byte_size(8);
                        writeln!(f, "    mov {}, {}",reg.to_string(),val)?;
                        out.push(reg);
                    },
                }
            },
            OfP::RESULT(func, args) => {
                let sp_taken = nasm_x86_64_prep_args(program, build, f, args, stack_size, local_vars)?;
                writeln!(f, "   call {}{}",program.architecture.func_prefix,func)?;
                if sp_taken-stack_size > 0 {
                    writeln!(f, "   add {}, {}",program.stack_ptr(),sp_taken-stack_size)?
                }
                let reg = regs[0].to_byte_size(build.get_contract_of_symbol(func).unwrap().Outputs.get(0).unwrap_or(&VarType::LONG).get_size(program));
                if reg.to_byte_size(8) != Register::RAX {
                    let oreg = Register::RAX.to_byte_size(reg.size());
                    writeln!(f, "   mov {}, {}",reg.to_string(),oreg.to_string())?;
                }
                out.push(reg);
            },
            OfP::BUFFER(s) => {
                let buf: &BuildBuf = &buffers[*s];
                let reg = regs[0].to_byte_size(8);
                writeln!(f, "   mov {}, {}",reg,program.stack_ptr())?;
                if stack_size-buf.offset > 0 {
                    writeln!(f, "   add {}, {}",reg,stack_size-buf.offset)?;
                }
                out.push(reg)
            },
        }
        Ok(out)
    }
    fn from_token(token: &Token, build: &mut BuildProgram, _: &CmdProgram, locals: &Vec<Locals>) -> Option<Self> {
        match &token.typ {
            TokenType::WordType(data)  => {
                if contains_local(locals, data){
                    return Some(Self::LOCALVAR(data.clone()))
                }
                else if build.global_vars.contains_key(data) {
                    return Some(Self::GLOBALVAR(data.clone()))
                }
                else if build.constdefs.contains_key(data) {
                    return Some(Self::CONST(build.constdefs.get(data).unwrap().typ.clone()))
                }
                None
            },
            TokenType::Register(reg) => Some(Self::REGISTER(reg.clone())),
            TokenType::StringType(val) => Some(Self::CONST(RawConstValueType::STR(build.insert_new_str(ProgramString { Typ: ProgramStringType::STR, Data:  val.clone()})))),
            TokenType::CStringType(val)=> Some(Self::CONST(RawConstValueType::STR(build.insert_new_str(ProgramString { Typ: ProgramStringType::CSTR, Data: val.clone() })))),
            TokenType::Number32(val)      => Some(Self::CONST(RawConstValueType::INT(val.clone()))),
            TokenType::Number64(val)      => Some(Self::CONST(RawConstValueType::LONG(val.clone()))),
            _ => None
        }
    }

}
#[derive(Debug)]
enum Instruction {
    //PUSH    (OfP),
    DEFVAR  (String),
    MOV     (Expression, Expression),
    //POP     (OfP),
    ADDSET     (Expression, Expression),
    SUBSET     (Expression, Expression),
    MULSET     (Expression, Expression),
    DIVSET     (Expression, Expression),
    CALL    (String, CallArgs),
    CALLRAW (String, CallArgs),
    FNBEGIN (),
    RET     (Expression),
    SYSCALL,

    // CONDITIONAL_JUMP(usize),
    // JUMP(usize),
    INTERRUPT(i64),

    EXPAND_SCOPE       (NormalScope),
    EXPAND_IF_SCOPE    (NormalScope),
    EXPAND_WHILE_SCOPE (NormalScope),
    EXPAND_ELSE_SCOPE  (NormalScope),
    GOTO(String, usize),
    MAKELABEL(String)
}

#[derive(Debug,Clone,PartialEq)]
enum ProgramStringType {
    STR,
    CSTR
}
impl ProgramStringType {
    fn sizeof(&self, bits: u32) -> usize {
        match self {
            Self::CSTR => if bits == 64 {8} else {4}
            Self::STR => if bits == 64 {16} else {12}
        }
    }
}
#[derive(Debug,Clone,PartialEq)]
struct ProgramString {
    Typ:  ProgramStringType,
    Data: String
}
#[derive(Debug,Clone)]
struct LocalVariable {
    typ: VarType,
    operand: usize
}
type Locals = LinkedHashMap<String, VarType>; //The issue is here because we use a normal hashmap instead of a linked one
#[derive(Debug)]
struct Function {
    contract: FunctionContract,
    locals: Locals,
    location: ProgramLocation,
    body: Vec<(ProgramLocation,Instruction)>,
    buffers: Vec<BuildBuf>,
}
impl Function {
    fn loc_display(&self) -> String {
        self.location.loc_display()
    }
}


#[derive(Debug, PartialEq, Clone)]
enum ConstValueType {
    BOOLEAN(bool),
    CHAR(i8),
    SHORT(i16),
    INT(i32),
    LONG(i64),
    STR(String, ProgramStringType),
    PTR(Ptr, i64),
}

#[derive(Debug,PartialEq,Clone)]
struct ConstValue {
    typ: ConstValueType,
    loc: ProgramLocation
}
impl ConstValueType {
    fn unwrap_int_data(&self) -> Option<i64> {
        match self {
            Self::INT(d) => Some(d.clone() as i64),
            Self::LONG(d) => Some(d.clone() as i64),
            Self::SHORT(d) => Some(d.clone() as i64),
            Self::CHAR(d) => Some(d.clone() as i64),
            Self::BOOLEAN(d) => Some(d.clone() as i64),
            _ => None
        }
    }
    fn unwrap_str_data(&self) -> Option<&String>  {
        match self {
            ConstValueType::STR(d, _) => Some(d),
            _ => None
        }
    }
    fn is_int(&self) -> bool {
        match self {
            Self::INT(_) | Self::LONG(_) | Self::SHORT(_) | Self::CHAR(_) | Self::BOOLEAN(_) | Self::PTR(_,_) => true,
            _ => false
        }
    }
    fn weak_cast(&self, typ: &VarType) -> Option<ConstValueType> {
        match self {
            ConstValueType::INT(data) => {
                match typ {
                    VarType::CHAR           => Some(ConstValueType::CHAR(data.clone() as i8)),
                    VarType::SHORT          => Some(ConstValueType::SHORT(data.clone() as i16)),
                    VarType::BOOLEAN        => Some(ConstValueType::BOOLEAN(*data != 0)),
                    VarType::INT            => Some(self.clone()),
                    VarType::LONG           => Some(ConstValueType::LONG(data.clone() as i64)),
                    VarType::PTR(typ) => Some(ConstValueType::PTR(typ.clone(), data.clone() as i64)),
                    VarType::CUSTOM(_) => None,
                }
            },
            ConstValueType::LONG(data) => {
                match typ {
                    VarType::CHAR => None,
                    VarType::SHORT => None,
                    VarType::BOOLEAN => None,
                    VarType::INT => {
                        if *data <= i32::MAX as i64 {
                            Some(ConstValueType::INT(data.clone() as i32))
                        }
                        else {
                            None
                        }
                    },
                    VarType::LONG => Some(self.clone()),
                    VarType::PTR(typ) => Some(ConstValueType::PTR(typ.clone(), data.clone() as i64)),
                    VarType::CUSTOM(_) => None,
                }
            },
            ConstValueType::STR(_, _) => None,
            ConstValueType::PTR(_, _) => {None}
            ConstValueType::BOOLEAN(_) => todo!(),
            ConstValueType::CHAR(_) => todo!(),
            ConstValueType::SHORT(_) => todo!(),
        }
    }
    fn to_var_type(&self) -> Option<VarType> {
        match self {
            ConstValueType::INT(_) => Some(VarType::INT),
            ConstValueType::LONG(_) => Some(VarType::LONG),
            ConstValueType::STR(_, typ) => if *typ == ProgramStringType::CSTR { Some(VarType::PTR(Ptr{ typ: PtrTyp::TYP(Box::new(VarType::CHAR)), inner_ref: 0})) } else { None },
            ConstValueType::PTR(typ, _) => Some(VarType::PTR(typ.clone())),
            ConstValueType::BOOLEAN(_) => Some(VarType::BOOLEAN),
            ConstValueType::CHAR(_)    => Some(VarType::CHAR),
            ConstValueType::SHORT(_)   => Some(VarType::SHORT),
        }
    }
    fn is_eq_vartype(&self, vartyp: &VarType) -> bool {
        match self {
            ConstValueType::INT(_)            => vartyp.weak_eq(&VarType::INT),
            ConstValueType::LONG(_)           => vartyp.weak_eq(&VarType::LONG),
            ConstValueType::STR(_, typ) => if *typ == ProgramStringType::CSTR { vartyp.weak_eq(&VarType::PTR(Ptr {typ: PtrTyp::TYP(Box::new(VarType::CHAR)), inner_ref: 0})) } else {false},
            ConstValueType::PTR(typ, _) => vartyp.weak_eq(&VarType::PTR(typ.clone())),
            ConstValueType::BOOLEAN(_)        => vartyp.weak_eq(&VarType::BOOLEAN),
            ConstValueType::CHAR(_)           => vartyp.weak_eq(&VarType::CHAR),
            ConstValueType::SHORT(_)          => vartyp.weak_eq(&VarType::SHORT),
        }
    }
    fn mul(&self, Other: &ConstValueType) -> Result<ConstValueType,String> {
        match self {
            ConstValueType::INT(val) => {
                if let Some(v) = Other.unwrap_int_data() {
                    if v <= i32::MAX as i64 && v >= i32::MIN as i64 {
                        return Ok(ConstValueType::INT(*val*v as i32));
                    }
                    else {
                        return Err("Error: Value of operation overflows i32 limit!".to_string());
                    }
                }
                else {
                    return Err("Error: Unexpected - operation on int and string".to_string());
                }
            }
            ConstValueType::LONG(val) => {
                if let Some(v) = Other.unwrap_int_data() {
                    return Ok(ConstValueType::LONG(*val*v));
                }
                else {
                    return Err("Error: Unexpected - operation on int and string".to_string());
                }
            }
            ConstValueType::STR(_, _) => {
                match Other {
                    _ => {
                        return Err("Error: Cannot do * operation on a string".to_string());
                    }
                }
            },
            ConstValueType::PTR(_,_) => {todo!("ConstValueType::PTR")}
            ConstValueType::BOOLEAN(_) => todo!(),
            ConstValueType::CHAR(val) => {
                if let Some(v) = Other.unwrap_int_data() {
                    if v <= i8::MAX as i64 && v >= i8::MIN as i64 {
                        return Ok(ConstValueType::CHAR(*val*v as i8));
                    }
                    else {
                        return Err("Error: Value of operation overflows i32 limit!".to_string());
                    }
                }
                else {
                    return Err("Error: Unexpected - operation on int and string".to_string());
                }
            },
            ConstValueType::SHORT(val) => {
                if let Some(v) = Other.unwrap_int_data() {
                    if v <= i16::MAX as i64 && v >= i16::MIN as i64 {
                        return Ok(ConstValueType::SHORT(*val*v as i16));
                    }
                    else {
                        return Err("Error: Value of operation overflows i32 limit!".to_string());
                    }
                }
                else {
                    return Err("Error: Unexpected - operation on int and string".to_string());
                }
            },
        }
    }
    fn sub(&self, Other: &ConstValueType) -> Result<ConstValueType,String> {
        match self {
            ConstValueType::INT(val) => {
                match Other {
                    ConstValueType::INT(nval) => {
                        return Ok(ConstValueType::INT(val-nval));
                    }
                    ConstValueType::LONG(nval) => {
                        return Ok(ConstValueType::LONG((*val as i64)-nval));
                    }
                    ConstValueType::STR(_nval, _) => {
                        return Err("Error: Unexpected - operation on int and string".to_string());
                    },
                    ConstValueType::PTR(_,_) => { todo!("ConstValueType::PTR")}
                    ConstValueType::BOOLEAN(_) => todo!(),
                    ConstValueType::CHAR(_) => todo!(),
                    ConstValueType::SHORT(_) => todo!(),

                }
            }
            ConstValueType::LONG(val) => {
                match Other {
                    ConstValueType::INT(nval) => {
                        return Ok(ConstValueType::LONG(val-(*nval as i64)));
                    }
                    ConstValueType::LONG(nval) => {
                        return Ok(ConstValueType::LONG(val-nval));
                    }
                    ConstValueType::STR(_nval, _) => {
                        return Err("Error: Unexpected - operation on long and string".to_string());
                    },
                    ConstValueType::PTR(_,_) => { todo!("ConstValueType::PTR")}
                    ConstValueType::BOOLEAN(_) => todo!(),
                    ConstValueType::CHAR(_) => todo!(),
                    ConstValueType::SHORT(_) => todo!(),

                }
            }
            ConstValueType::STR(_, _) => {
                match Other {
                    _ => {
                        return Err("Error: Cannot do - operation on a string".to_string());
                    }
                }
            },
            ConstValueType::PTR(_,_) => { todo!("ConstValueType::PTR")}
            ConstValueType::BOOLEAN(_) => todo!(),
            ConstValueType::CHAR(_) => todo!(),
            ConstValueType::SHORT(_) => todo!(),

        }
    }
    fn add(&self, Other: &ConstValueType) -> Result<ConstValueType,String> {
        match self {
            ConstValueType::INT(val) => {
                match Other {
                    ConstValueType::INT(nval) => {
                        return Ok(ConstValueType::INT(val+nval));
                    }
                    ConstValueType::LONG(nval) => {
                        return Ok(ConstValueType::LONG((*val as i64)+nval));
                    }
                    ConstValueType::STR(nval, typ) => {
                        return Ok(ConstValueType::STR(val.to_string()+nval, typ.clone()));
                    },
                    ConstValueType::PTR(_,_) => { todo!("ConstValueType::PTR")}
                    ConstValueType::BOOLEAN(_) => todo!(),
                    ConstValueType::CHAR(_) => todo!(),
                    ConstValueType::SHORT(_) => todo!(),

                }
            }
            ConstValueType::LONG(val) => {
                match Other {
                    ConstValueType::INT(nval) => {
                        return Ok(ConstValueType::LONG(val+(*nval as i64)));
                    }
                    ConstValueType::LONG(nval) => {
                        return Ok(ConstValueType::LONG(val+nval));
                    }
                    ConstValueType::STR(nval, typ) => {
                        return Ok(ConstValueType::STR(val.to_string()+nval, typ.clone()));
                    },
                    ConstValueType::PTR(_,_) => { todo!("ConstValueType::PTR")}
                    ConstValueType::BOOLEAN(_) => todo!(),
                    ConstValueType::CHAR(_) => todo!(),
                    ConstValueType::SHORT(_) => todo!(),

                }
            }
            ConstValueType::STR(val,typ) => {
                match Other {
                    ConstValueType::INT(nval) => {
                        return Ok(ConstValueType::STR(val.clone()+&nval.to_string(), typ.clone()));
                    }
                    ConstValueType::LONG(nval) => {
                        return Ok(ConstValueType::STR(val.clone()+&nval.to_string(), typ.clone()));
                    }
                    ConstValueType::STR(nval,_) => {
                        let out = val.clone()+nval;
                        return Ok(ConstValueType::STR(out,typ.clone()));
                    },
                    ConstValueType::PTR(_,_) => { todo!("ConstValueType::PTR")}
                    ConstValueType::BOOLEAN(_) => todo!(),
                    ConstValueType::CHAR(_) => todo!(),
                    ConstValueType::SHORT(_) => todo!(),

                }
            },
            ConstValueType::PTR(_,_) => { todo!("ConstValueType::PTR")}
            ConstValueType::BOOLEAN(_) => todo!(),
            ConstValueType::CHAR(_) => todo!(),
            ConstValueType::SHORT(_) => todo!(),

        }
    }
}

#[derive(Debug, Clone,PartialEq)]
enum RawConstValueType{
    CHAR(i8),
    SHORT(i16),
    INT(i32),
    LONG(i64),
    STR(usize),
    PTR(Ptr, i64)
}
impl RawConstValueType {
    fn to_string(&self, build: &BuildProgram) -> String {
        match self {
            Self::SHORT(v)  => format!("{}",v),
            Self::CHAR(v)    => format!("{}",v),
            Self::INT(v)    => format!("{}",v),
            Self::LONG(v)   => format!("{}",v),
            Self::STR(v)  => format!("\"{}\"",build.stringdefs[v.to_owned()].Data),
            Self::PTR(_, p) => format!("ptr {}",p)
        }
    }
    fn to_type(&self, build: &BuildProgram) -> Vec<VarType> {
        match self {
            RawConstValueType::CHAR(_) => {
                vec![VarType::CHAR]
            }
            RawConstValueType::SHORT(_) => {
                vec![VarType::SHORT]
            }
            RawConstValueType::INT(_) => {
                vec![VarType::INT]
            },
            RawConstValueType::LONG(_) => {
                vec![VarType::LONG]
            },
            RawConstValueType::STR(index) => {
                let val = &build.stringdefs[index.to_owned()];
                match val.Typ {
                    ProgramStringType::STR =>  vec![VarType::PTR(Ptr { typ: PtrTyp::TYP(Box::new(VarType::CHAR)), inner_ref: 0}), VarType::LONG],
                    ProgramStringType::CSTR => vec![VarType::PTR(Ptr { typ: PtrTyp::TYP(Box::new(VarType::CHAR)), inner_ref: 0})],
                }
            },
            RawConstValueType::PTR(typ, _) => {
                vec![VarType::PTR(typ.clone())]
            },
        }
    }
    fn get_num_data(&self) -> i64 {
        match self {
            RawConstValueType::LONG(val) => val.clone(),
            RawConstValueType::INT(val) => val.clone() as i64,
            RawConstValueType::SHORT(val) => val.clone() as i64,
            RawConstValueType::CHAR(val) => val.clone() as i64,
            RawConstValueType::STR(_) => todo!(),
            RawConstValueType::PTR(_,val) => val.clone(),
        }
    }
    fn LRNasm(&self, f: &mut File, build: &BuildProgram, program: &CmdProgram, iregs: &Vec<Register>) -> std::io::Result<Vec<Register>> {
        let o: Vec<Register>;
        match self {
            RawConstValueType::SHORT(val) => {
                let oreg = iregs[0].to_byte_size(2);
                writeln!(f, "   mov {}, {}",oreg.to_string(),val)?;
                o = vec![oreg]
            }
            RawConstValueType::CHAR(val) => {
                let oreg = iregs[0].to_byte_size(1);
                writeln!(f, "   mov {}, {}",oreg.to_string(),val)?;
                o = vec![oreg]
            }
            RawConstValueType::INT(val) => {
                let oreg = iregs[0].to_byte_size(4);
                writeln!(f, "   mov {}, {}",oreg.to_string(),val)?;
                o = vec![oreg]
            },
            RawConstValueType::LONG(val) => {
                let oreg = iregs[0].to_byte_size(8);
                writeln!(f, "   mov {}, {}",oreg.to_string(),val)?;
                o = vec![oreg]
            },
            RawConstValueType::STR(index) => {
                let sstr = &build.stringdefs[index.to_owned()];
                match sstr.Typ {
                    ProgramStringType::STR  => {
                        o = if program.architecture.bits == 32 {
                            let reg = iregs[0].to_byte_size(4);
                            let reg2 = iregs[1].to_byte_size(4);
                            writeln!(f, "   mov {}, $STRING_{}_",reg.to_string(),index)?;
                            writeln!(f, "   mov {}, {}",reg2,sstr.Data.len())?;
                            vec![reg,reg2]
                        }
                        else {
                            let reg = iregs[0].to_byte_size(8);
                            let reg2 = iregs[1].to_byte_size(8);
                            writeln!(f, "   lea {}, [rel $STRING_{}_]",reg.to_string(),index)?;
                            writeln!(f, "   mov {}, {}",reg2,sstr.Data.len())?;
                            vec![reg,reg2]
                        };
                        
                    },
                    ProgramStringType::CSTR => {
                        let oreg = if program.architecture.bits == 32 {
                            let reg = iregs[0].to_byte_size(4);
                            writeln!(f, "   mov {}, $STRING_{}_",reg.to_string(),index)?;
                            reg
                        }
                        else {
                            let reg = iregs[0].to_byte_size(8);
                            writeln!(f, "   lea {}, [rel $STRING_{}_]",reg.to_string(),index)?;
                            reg
                        };
                        o = vec![oreg]
                    },
                }
            },
            RawConstValueType::PTR(_, val) => {
                let oreg = iregs[0].to_byte_size(8);
                writeln!(f, "   mov {}, {}",oreg.to_string(),val)?;
                o = vec![oreg]
            },
        }
        Ok(o)
    }
}
#[derive(Debug, Clone,PartialEq)]
struct RawConstValue {
    typ: RawConstValueType,
    loc: ProgramLocation,
}
type RawConstants = HashMap<String, RawConstValue>;
#[derive(Debug)]
struct DLL_import {
    from: String,
    contract: AnyContract
}
#[derive(Debug)]
struct DLL_export {
    contract: AnyContract
}
#[derive(Debug, Clone)]
struct BuildBuf {
    typ: VarType,
    size: usize,
    offset: usize,
}
impl BuildBuf {
    fn from_typ(typ: VarType) -> Self {
        Self { typ, size: 0, offset: 0 }
    }
    fn from_parse_buf(typ: VarType, size: usize) -> Self {
        Self { typ, size, offset: 0 }
    }
}
#[derive(Debug)]
enum GlobalVarType {
    BUFFER(usize),
    CONSTANT(RawConstValueType)
}
impl GlobalVarType {
    fn var_type(&self, build: &BuildProgram) -> VarType {
        match self {
            Self::BUFFER(b) => {
                VarType::PTR(Ptr::ref_to(build.buffers[b.to_owned()].typ.clone()))
            }
            Self::CONSTANT(c) => {
                c.to_type(build)[0].clone()
            }
        }
    }
}
#[derive(Debug)]
struct GlobalVar {
    typ: GlobalVarType,
    loc: ProgramLocation
}
#[derive(Debug)]
struct BuildProgram {
    externals:    HashMap<String,External>,
    functions:    HashMap<String, Function>,
    stringdefs:   Vec<ProgramString>,
    constdefs:    HashMap<String, RawConstValue>,
    dll_imports:  HashMap<String, DLL_import>,
    dll_exports:  HashMap<String, DLL_export>,
    global_vars:  HashMap<String, GlobalVar>,
    buffers:      Vec<BuildBuf>,
    stringoffset: usize,
}
impl BuildProgram {
    fn insert_new_str(&mut self, str: ProgramString) -> usize {
        let id = self.stringdefs.len()+self.stringoffset;
        self.stringdefs.push(str);
        id
    }
    fn contains_symbol(&self, str: &String) -> bool {
        self.constdefs.contains_key(str) || self.dll_imports.contains_key(str) || self.externals.contains_key(str) || self.functions.contains_key(str) || self.global_vars.contains_key(str)
    }
    fn get_location_of_symbol(&self, str: &String) -> Option<ProgramLocation> {
        if let Some(ext) = self.externals.get(str) {
            return Some(ext.loc.clone())
        }
        else if let Some(func) = self.functions.get(str) {
            return Some(func.location.clone())
        }
        else if let Some(var) = self.global_vars.get(str) {
            return Some(var.loc.clone())
        }
        else if let Some(cons) = self.constdefs.get(str) {
            return Some(cons.loc.clone())
        }
        None
    }
    fn get_contract_of_symbol(&self, str: &String) -> Option<AnyContract> {
        if let Some(ext) = self.externals.get(str) {
            return ext.contract.clone()
        }
        else if let Some(dll_import) = self.dll_imports.get(str) {
            return Some(dll_import.contract.clone())
        }
        else if let Some(func) = self.functions.get(str) {
            return Some(func.contract.to_any_contract())
        }
        None
    }
    fn new() -> Self {
        Self { externals: HashMap::new(), functions: HashMap::new(),stringdefs: Vec::new(), constdefs: HashMap::new(), dll_imports: HashMap::new(), dll_exports: HashMap::new(),
            global_vars: HashMap::new(), 
            buffers: Vec::new(),
            stringoffset: 0
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum VarType {
    CHAR,
    SHORT,
    BOOLEAN,
    INT,
    LONG,
    PTR(Ptr),
    CUSTOM(usize),
}
impl VarType {
    fn is_numeric(&self) -> bool {
        match self {
            Self::PTR(_) => true,
            Self::CHAR | Self::SHORT  | Self::INT | Self::LONG => true,
            _ => false
        }
    }
    fn is_ptr(&self) -> bool {
        match self {
            Self::PTR(_) => true,
            _ => false
        }
    }
    fn get_ptr_val(&self) -> Option<VarType> {
        match self {
            Self::PTR(p) => {
                if p.typ == PtrTyp::VOID && p.inner_ref == 0 {
                    None
                }
                else {
                    if p.inner_ref > 0 {
                        Some(VarType::PTR(Ptr { inner_ref: p.inner_ref-1, typ: p.typ.clone()}))
                    }
                    else {
                        match &p.typ {
                            PtrTyp::TYP(t) => Some(*t.clone()),
                            _ => panic!("Unreachable"),

                        }
                    }
                }
            }
            _ => None
        }
    }
    fn is_some_ptr(&self) -> bool {
        match self {
            Self::PTR(p) => {
                if p.typ == PtrTyp::VOID && p.inner_ref == 0 {
                    false
                }
                else {
                    true
                }
            }
            _ => false
        }
    }
    fn to_string(&self, isplural: bool) -> String {
        match self {
            VarType::CHAR => {
                if isplural {"chars".to_string()} else {"char".to_string()}
            },
            VarType::SHORT => {
                if isplural {"shorts".to_string()} else {"short".to_string()}
            }
            VarType::BOOLEAN => {
                if isplural {"booleans".to_string()} else {"bool".to_string()}
            },
            VarType::INT => {
                if isplural {"ints".to_string()} else {"int".to_string()}
            }
            VarType::LONG => {
                if isplural {"longs".to_string()} else {"long".to_string()}
            }
            VarType::PTR(typ) => {
                if isplural {"pointers".to_string()} else {format!("pointer<{}{}>","*".repeat(typ.inner_ref+1),typ.typ.to_string()).to_string()}
            }
            VarType::CUSTOM(_) => {
                todo!("Implement custom")
            }
        }
    }
    fn get_size(&self, program: &CmdProgram) -> usize{
        match self {
            VarType::CHAR    => 1,
            VarType::SHORT   => 2,
            VarType::BOOLEAN => 1,
            VarType::INT     => 4,
            VarType::LONG    => 8,
            VarType::PTR(_) => {
                match program.architecture.bits{
                    32 => 4,
                    64 => 8,
                    _    => 4
                }
            },
            VarType::CUSTOM(_) => todo!(),
        }
    }
    fn weak_eq(&self, other: &Self) -> bool {
        match self {
            Self::PTR(typ) => {
                match other {
                    VarType::PTR(otyp) => {
                        if (typ.typ == PtrTyp::VOID) && typ.inner_ref == otyp.inner_ref { true }
                        else {typ==otyp && typ.inner_ref == otyp.inner_ref}
                    },
                    _ => false
                }
            }
            Self::INT | Self::LONG | Self::SHORT | Self::CHAR | Self::BOOLEAN => {
                match other {
                    Self::INT | Self::LONG | Self::SHORT | Self::CHAR | Self::BOOLEAN  => true,
                    _ => false
                }
            }
            _ => self == other
        }
    }
}
type ScopeBody = Vec<(ProgramLocation,Instruction)>;
type ContractInputs = LinkedHashMap<String, VarType>;


#[derive(Debug, Clone)]
struct ContractInputPool {
    body: Vec<VarType>,
    is_dynamic: bool,
    dynamic_type: Option<VarType>
}
impl ContractInputPool {
    fn len(&self) -> usize {
        self.body.len()
    }
    fn push(&mut self, val: VarType) {
        self.body.push(val)
    }
    fn pop(&mut self) -> Option<VarType> {
        self.body.pop()
    }
    fn reverse(&mut self) {
        self.body.reverse()
    }
    fn new() -> Self {
        Self { body: vec![], is_dynamic: false, dynamic_type: None}
    }
}

#[derive(Debug, Clone)]
struct FunctionContract {
    Inputs: ContractInputs,
    Outputs: Vec<VarType>
}
impl FunctionContract {
    fn to_any_contract(&self) -> AnyContract {
        AnyContract { InputPool: {
            let mut o: Vec<VarType> = Vec::with_capacity(self.Inputs.len());
            for (_,val) in self.Inputs.iter() {
                o.push(val.clone());
            }
            ContractInputPool { body: o, is_dynamic: false, dynamic_type: None}
        }, Outputs: self.Outputs.clone() }
    }
}
#[derive(Debug, Clone)]
struct AnyContract {
    InputPool: ContractInputPool,
    Outputs: Vec<VarType>
}
#[derive(Debug)]
enum NormalScopeType {
    IF(Expression),
    ELSE,
    WHILE(Expression),
    EMPTY
}
impl NormalScopeType {
    fn unwrap_expr(&self) -> &Expression {
        match self {
            NormalScopeType::IF(c) => c,
            NormalScopeType::ELSE => panic!("This should be unreachable"),
            NormalScopeType::WHILE(c) => c,
            NormalScopeType::EMPTY => panic!("This should be unreachable"),
        }
    }
}

#[derive(Debug)]
struct NormalScope {
    typ: NormalScopeType,
    body: ScopeBody,
    buffers: Vec<BuildBuf>,
    locals: Locals
}

#[derive(Debug)]
struct FunctionScope<'a> {
    func: &'a mut Function
}
#[derive(Debug)]
enum ScopeType {
    FUNCTION(Function, String),
    NORMAL(NormalScope),
}
#[derive(Debug)]
struct Scope {
    typ: ScopeType,
    hasBeenOpened: bool,
}

impl ScopeType {
    fn body_unwrap(&self) -> Option<&ScopeBody> {
        match self {
            Self::FUNCTION(func,_) => {
                Some(&func.body)
            },
            Self::NORMAL(scope) => {
                Some(&scope.body)
            },
        }
    }
    fn body_unwrap_mut(&mut self) -> Option<&mut ScopeBody> {
        match self {
            Self::FUNCTION(func,_) => {
                Some(&mut func.body)
            },
            Self::NORMAL(scope) => {
                Some(&mut scope.body)
            },
        }
    }
    fn locals_unwrap(&self) -> Option<&Locals> {
        match self {
            Self::FUNCTION(func,_) => {
                Some(&func.locals)
            },
            Self::NORMAL(_) => {
                None
            },
        }
    }
    fn locals_unwrap_mut(&mut self) -> Option<&mut Locals> {
        match self {
            Self::FUNCTION(func,_) => {
                Some(&mut func.locals)
            },
            Self::NORMAL(_) => {
                None
            },
        }
    }
    fn contract_unwrap(&self) -> Option<&FunctionContract> {
        match self {
            Self::FUNCTION(func,_) => {
                Some(&func.contract)
            },
            Self::NORMAL(_) => {
                None
            },
        }
    }
    fn buffers_unwrap_mut(&mut self) -> Option<&mut Vec<BuildBuf>> {
        match self {
            Self::FUNCTION(func,_) => {
                Some(&mut func.buffers)
            },
            Self::NORMAL(s) => {
                Some(&mut s.buffers)
            },
        }
    }

    fn body_is_some(&self) -> bool {
        match self {
            Self::FUNCTION(_,_) => true,
            Self::NORMAL(_) => true,
        }
    }
    fn locals_is_some(&self) -> bool {
        match self {
            Self::FUNCTION(_,_) => true,
            Self::NORMAL(_) => false,
        }
    }
    fn contract_is_some(&self) -> bool {
        match self {
            Self::FUNCTION(_,_) => true,
            Self::NORMAL(_) => false,
        }
    }
    fn to_string(&self, isplural: bool) -> &str {
        match self {
            ScopeType::FUNCTION(_,_)                 => {
                if isplural {"functions"} else {"function"}
            }
            ScopeType::NORMAL(normal)=> {
                match normal.typ {
                    NormalScopeType::IF(_)    => if isplural {"ifs"}     else {"if"},
                    NormalScopeType::ELSE     => if isplural {"elses"}   else {"else"},
                    NormalScopeType::EMPTY    => if isplural {"empties"} else {"empty"},
                    NormalScopeType::WHILE(_) => if isplural {"while"}else {"while"},
                }
            },
        }
    }
}
impl Scope {
    fn body_unwrap(&self)         -> Option<&ScopeBody>                    {if self.hasBeenOpened { self.typ.body_unwrap()} else { None }}
    fn body_unwrap_mut(&mut self) -> Option<&mut ScopeBody>                {if self.hasBeenOpened { self.typ.body_unwrap_mut()} else { None }}
    fn locals_unwrap(&self)          -> Option<& Locals>                   {self.typ.locals_unwrap()}
    fn locals_unwrap_mut(&mut self)  -> Option<&mut Locals>                {self.typ.locals_unwrap_mut()}
    fn contract_unwrap(&self)        -> Option<&FunctionContract>          {self.typ.contract_unwrap()}
    fn body_is_some(&self)           -> bool                               {self.typ.body_is_some()}
    fn locals_is_some(&self)         -> bool                               {self.typ.locals_is_some()}
    fn contract_is_some(&self)       -> bool                               {self.typ.contract_is_some()}
}

fn eval_const_def(lexer: &mut Lexer, build: &mut BuildProgram, until: TokenType) -> ConstValue {
    let mut varStack: Vec<ConstValueType> = vec![];
    let orgLoc = lexer.currentLocation.clone();
    while let Some(token) = lexer.next() {
        if token.typ == until {
            break;
        }
        match token.typ {
            TokenType::Operation(ref op) => {
                match op {
                    Op::PLUS => {
                        let valTwo = par_expect!(token.location,varStack.pop(),"Stack underflow in constant definition");
                        let valOne = par_expect!(token.location,varStack.pop(),"Stack underflow in constant definition");
                        varStack.push(par_expect!(token.location,valOne.add(&valTwo),"Error: Failed to add the constant values together"));
                    }
                    Op::MINUS => {
                        let valTwo = par_expect!(token.location,varStack.pop(),"Stack underflow in constant definition");
                        let valOne = par_expect!(token.location,varStack.pop(),"Stack underflow in constant definition");
                        varStack.push(par_expect!(token.location,valOne.sub(&valTwo),"Error: Failed to add the constant values together"));
                    }
                    Op::STAR => {
                        let valTwo = par_expect!(token.location,varStack.pop(),"Stack underflow in constant definition");
                        let valOne = par_expect!(token.location,varStack.pop(),"Stack underflow in constant definition");
                        varStack.push(par_expect!(token.location,valOne.mul(&valTwo),"Error: Failed to add the constant values together"));
                    }
                    _ => {
                        par_error!(token, "Error: Unexpected op {} in constant definition!",op.to_string());
                    }
                }
            }
            TokenType::IntrinsicType(typ) => {
                match typ {
                    IntrinsicType::CAST => {
                        let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for constant definition cast!");
                        par_assert!(ntok, ntok.typ == TokenType::Operation(Op::LT),"Error: unexpected token type {} in cast! INVALID SYNTAX",ntok.typ.to_string(false));
                        let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for constant definition cast!");
                        let typ = if let TokenType::Definition(typ) = ntok.typ { typ } else {par_error!(ntok, "Error: expected definition but found {}",ntok.typ.to_string(false))};
                        let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for constant definition cast!");
                        par_assert!(ntok, ntok.typ == TokenType::Operation(Op::GT),"Error: unexpected token type {} in cast! INVALID SYNTAX",ntok.typ.to_string(false));
                        let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for constant definition cast!");
                        par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::OPENPAREN),"Error: unexpected token type {} in cast! INVALID SYNTAX",ntok.typ.to_string(false));
                        let val = eval_const_def(lexer, build,TokenType::IntrinsicType(IntrinsicType::CLOSEPAREN));
                        let val = par_expect!(val.loc,val.typ.weak_cast(&typ),"Error: Cannot cast to type {}",typ.to_string(false));
                        varStack.push(val);
                    }
                    _ => {
                        par_error!(token, "Unexpected Intrinsic: {}\nConstant definitions can only work with ADD,SUB,MUL intrinsics ",typ.to_string(false));
                    }
                }
            }
            TokenType::WordType(word) => {
                let def = par_expect!(token.location, build.constdefs.get(&word), "Could not find constant definition {}. You can only have constants inside other constant definitions!",word);
                match def.typ {
                    RawConstValueType::CHAR(val) => {
                        varStack.push(ConstValueType::CHAR(val));
                    }
                    RawConstValueType::SHORT(val) => {
                        varStack.push(ConstValueType::SHORT(val));
                    }
                    RawConstValueType::INT(val)  => {
                        varStack.push(ConstValueType::INT(val));
                    }
                    RawConstValueType::LONG(val) => {
                        varStack.push(ConstValueType::LONG(val));
                    }
                    RawConstValueType::STR(index)       => {
                        let d = &build.stringdefs[index];
                        varStack.push(ConstValueType::STR(d.Data.clone(),d.Typ.clone()));
                    }
                    RawConstValueType::PTR(ref ptr, val) => {
                        varStack.push(ConstValueType::PTR(ptr.clone(), val));
                    }
                }
            }
            TokenType::StringType(word) => {
                varStack.push(ConstValueType::STR(word, ProgramStringType::STR))
            }
            TokenType::Number32(num) => {
                varStack.push(ConstValueType::INT(num));
            }
            TokenType::Number64(num) => {
                varStack.push(ConstValueType::LONG(num));
            }
            TokenType::Number16(num) => {
                varStack.push(ConstValueType::SHORT(num));
            }
            TokenType::Number8(num) => {
                varStack.push(ConstValueType::CHAR(num));
            }
            TokenType::CStringType(word) => {
                varStack.push(ConstValueType::STR(word, ProgramStringType::CSTR))
            }

            _ => {
                par_error!(token,"Unexpected token type in const declaration {}",token.typ.to_string(false));
            }
        }
    }

    par_assert!(lexer.currentLocation,varStack.len() == 1,"Error: Lazy constant stack handling! You need to correctly handle your constants");
    let top = varStack.pop().unwrap();

    ConstValue {typ: top, loc: orgLoc}
}
fn parse_argument_contract_from_body(body: &[Token], build: &mut BuildProgram,program: &CmdProgram,currentLocals: &Vec<Locals>,loc: &ProgramLocation) -> CallArgs {
    let mut out: CallArgs = CallArgs::new();
    let mut expectNextSY = false;
    let mut lexer = body.iter();
    par_assert!(loc, par_expect!(loc,lexer.next(), "Error: abruptly ran out of tokens in argument contract definition").typ == TokenType::IntrinsicType(IntrinsicType::OPENPAREN), "Error: argument contract must begin with (");
    while let Some(token) = lexer.next() {
        match token.typ {
            TokenType::IntrinsicType(Typ) => {
                match Typ {
                    IntrinsicType::COMA => {
                        par_assert!(token.location,expectNextSY, "undefined coma placed inside argument contract! Comas only seperate Input parameters");
                        expectNextSY = false;
                    }
                    IntrinsicType::CLOSEPAREN => {
                        return out
                    },
                    other => par_error!(token, "Unexpected intrinsic in argument contract! {}",other.to_string(false))
                }
            }
            _ => {
                out.push(par_expect!(token, OfP::from_token(&token,build,program, currentLocals),"Unexpected Token Type in argument Contract. Expected Definition but found: {}",token.typ.to_string(false)));
                expectNextSY = true;
            }
        }
    }
    out
}
fn parse_argument_contract(lexer: &mut Lexer, build: &mut BuildProgram, program: &CmdProgram, currentLocals: &Vec<Locals>) -> CallArgs {
    let mut out: CallArgs = CallArgs::new();
    let mut expectNextSY = false;
    par_assert!(lexer.currentLocation, par_expect!(lexer.currentLocation,lexer.next(), "Error: abruptly ran out of tokens in argument contract definition").typ == TokenType::IntrinsicType(IntrinsicType::OPENPAREN), "Error: argument contract must begin with (");
    while let Some(token) = lexer.next() {
        match token.typ {
            TokenType::IntrinsicType(Typ) => {
                match Typ {
                    IntrinsicType::COMA => {
                        par_assert!(token.location,expectNextSY, "undefined coma placed inside argument contract! Comas only seperate Input parameters");
                        expectNextSY = false;
                    }
                    IntrinsicType::CLOSEPAREN => {
                        return out
                    },
                    other => par_error!(token, "Unexpected intrinsic in argument contract! {}",other.to_string(false))
                }
            }
            _ => {
                out.push(par_expect!(token, OfP::from_token(&token,build,program, currentLocals),"Unexpected Token Type in argument Contract. Expected Definition but found: {}",token.typ.to_string(false)));
                expectNextSY = true;
            }
        }
    }
    out
}
/*
[NOTE] For future me using this function:
IT DOES NOT CONSUME THE FIRST (
*/
fn parse_any_contract(lexer: &mut Lexer) -> AnyContract {
    let mut out = AnyContract { InputPool: ContractInputPool::new(), Outputs: vec![] };
    let mut expectNextSY = false;
    let mut is_input = true;
    while let Some(token) = lexer.next() {
        match token.typ {
            TokenType::IntrinsicType(Typ) => {
                match Typ {
                    IntrinsicType::COMA => {
                        assert!(expectNextSY, "undefined coma placed inside any contract! Comas only seperate Input or Output parameters");
                        expectNextSY = false;
                    }
                    IntrinsicType::CLOSEPAREN => return out,
                    IntrinsicType::DOUBLE_COLIN => {
                        par_assert!(token,is_input, "multiple double colin seperators found in any contract!");
                        is_input = false;
                        expectNextSY = false;
                    },
                    IntrinsicType::THREEDOTS => {
                        let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for constant definition cast!");
                        par_assert!(ntok, ntok.typ == TokenType::Operation(Op::LT),"Error: unexpected token type {} in cast! INVALID SYNTAX",ntok.typ.to_string(false));
                        let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for constant definition cast!");
                        if let TokenType::WordType(w) = &ntok.typ {
                            par_assert!(ntok, w == "Any","Error: Expected any found {}!",w);
                            out.InputPool.is_dynamic = true;
                            out.InputPool.dynamic_type = None
                        }
                        else if let TokenType::Definition(typ) = ntok.typ {
                            out.InputPool.is_dynamic = true;
                            out.InputPool.dynamic_type = Some(typ)
                        }
                        let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for constant definition cast!");
                        par_assert!(ntok, ntok.typ == TokenType::Operation(Op::GT),"Error: unexpected token type {} in cast! INVALID SYNTAX",ntok.typ.to_string(false));

                    }

                    other => par_error!(token, "Unexpected intrinsic in any contract! {}",other.to_string(false))
                }
            }
            TokenType::Definition(Def) => {
                expectNextSY = true;
                if is_input {
                    out.InputPool.push(Def);
                }
                else {
                    out.Outputs.push(Def)
                }
            }
            _ => {
                par_error!(token, "Unexpected Token Type in any Contract. Expected Definition but found: {}",token.typ.to_string(false))
            }
        }
    }

    out
}
fn parse_function_contract(lexer: &mut Lexer) -> FunctionContract {
    let mut out = FunctionContract {Inputs: LinkedHashMap::new(), Outputs: vec![]};
    let mut is_input = true;
    let first = lexer.next();
    let first = par_expect!(lexer.currentLocation,first,"abruptly ran out of tokens in function contract");
    let mut expectNextSY = false;
    match first.typ {
        TokenType::IntrinsicType(ref typ) => {
            match typ {
                IntrinsicType::OPENPAREN => {},
                Other => {
                    par_error!(first,"INVALID TOKEN FOR PARSING, Expected an Open paren but found other {}",Other.to_string(false));
                }
            }
        }
        _ => {par_error!(first, "INVALID TOKEN FOR PARSING, Expected an Open paren intrinsic but found: {}",first.typ.to_string(false));}
    }
    while let Some(token) = lexer.next() {
        match token.typ {
            TokenType::IntrinsicType(Typ) => {
                match Typ {
                    IntrinsicType::COMA => {
                        par_assert!(token, expectNextSY, "undefined coma placed inside function contract! Comas only seperate Input or Output parameters");
                        expectNextSY = false;
                    }
                    IntrinsicType::CLOSEPAREN => return out,
                    IntrinsicType::DOUBLE_COLIN => {
                        par_assert!(token,is_input, "multiple double colin seperators found in function contract!");
                        is_input = false;
                        expectNextSY = false;
                    },
                    other => par_error!(token, "Unexpected intrinsic in function contract! {}",other.to_string(false))
                }
            }
            TokenType::WordType(Word) => {
                par_assert!(lexer.currentLocation, is_input, "Error: Unknown Word in parameter/output definitions {}",Word);
                let f = par_expect!(lexer.currentLocation,lexer.next(),"Error: Unknown word! Word has to be given a type but instead abruptly ran out of tokens");
                par_assert!(f, f.typ == TokenType::IntrinsicType(IntrinsicType::DOUBLE_COLIN), "Error: Expected : after parameter name!");
                let f = par_expect!(lexer.currentLocation,lexer.next(),"Error: Unknown word! Word has to be given a type but instead abruptly ran out of tokens");
                match f.typ {
                    TokenType::Definition(Def) => {
                        expectNextSY = true;
                        out.Inputs.insert(Word, Def);
                    }
                    _ => {
                        par_error!(f, "Error: Unexpected token type in parameter definition");
                    }
                }
            }
            TokenType::Definition(Def) => {
                expectNextSY = true;
                out.Outputs.push(Def)
            }
            _ => {
                par_error!(token, "Unexpected Token Type in Function Contract. Expected Definition but found: {}",token.typ.to_string(false))
            }
        }
    }
    out
}
type ScopeStack = Vec<Scope>;
fn getTop(scopeStack: &ScopeStack) -> Option<&Scope> {
    scopeStack.get(scopeStack.len()-1)
}
fn getTopMut(scopeStack: &mut ScopeStack) -> Option<&mut Scope> {
    let len = scopeStack.len();
    if len > 0 {
        scopeStack.get_mut(len-1)
    }
    else{
        None
    }
}
fn get_local<'a>(currentLocals: &'a Vec<Locals>, name: &String) -> Option<&'a VarType> {
    for e in currentLocals.iter().rev() {
        if let Some(v) = e.get(name) {
            return Some(v);
        }
    }
    None
}

fn contains_local<'a>(currentLocals: &'a Vec<Locals>, name: &String) -> bool {
    for e in currentLocals.iter().rev() {
        if e.contains_key(name) {
            return true
        }
    }
    false
}
fn parse_token_to_build_inst(token: Token,lexer: &mut Lexer, program: &mut CmdProgram, build: &mut BuildProgram, scopeStack: &mut ScopeStack, currentLocals: &mut Vec<Locals>, currentLabels: &mut HashSet<String>, expectedLabels: &mut HashMap<String, Vec<(ProgramLocation,*mut Instruction)>>, include_folders: &HashSet<PathBuf>){
    match token.typ {
        TokenType::WordType(ref word) => {
            par_assert!(token,scopeStack.len() > 0, "Undefined Word Call outside of entry point! '{}'",word);
            let currentScope = getTopMut(scopeStack).unwrap();
            par_assert!(token,currentScope.body_is_some(), "Error: can not insert word operation at the top level of a {} as it does not support instructions",currentScope.typ.to_string(false));
            if build.externals.contains_key(word) {
                let contract = parse_argument_contract(lexer, build, program, currentLocals);
                let body = currentScope.body_unwrap_mut().unwrap();
                body.push((token.location.clone(),Instruction::CALLRAW(word.clone(), contract)));
                return;
            } else if build.dll_imports.contains_key(word) {
                let contract = parse_argument_contract(lexer, build, program,currentLocals);
                let body = currentScope.body_unwrap_mut().unwrap();
                body.push((token.location.clone(),Instruction::CALLRAW(word.clone(), contract)));
                return;
            }
            let ofp1 = par_expect!(token, OfP::from_token(&token, build, program,currentLocals), "Unknown word type: {}!",word);
            let Op = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected variable operation or another variable!");
            
            if let TokenType::SETOperation(op) = Op.typ {
                let expr_body: Vec<Token> = lexer.map_while(|t| if t.typ != TokenType::IntrinsicType(IntrinsicType::DOTCOMA) {Some(t)} else { None }).collect();
                let expr = tokens_to_expression(&expr_body, build, program, &currentLocals,par_expect!(lexer.currentLocation, currentScope.typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"));
                let body = currentScope.body_unwrap_mut().unwrap();
                match &op {
                    SetOp::SET      => {
                        body.push((token.location.clone(),Instruction::MOV(Expression::val(ofp1), expr)))
                    },
                    SetOp::PLUSSET  => {
                        body.push((token.location.clone(),Instruction::ADDSET(Expression::val(ofp1), expr)))
                    },
                    SetOp::MINUSSET => {
                        body.push((token.location.clone(),Instruction::SUBSET(Expression::val(ofp1), expr)))
                    },
                    SetOp::MULSET   => {
                        body.push((token.location.clone(),Instruction::MULSET(Expression::val(ofp1), expr)))
                    },
                    SetOp::DIVSET   => {
                        body.push((token.location.clone(),Instruction::DIVSET(Expression::val(ofp1), expr)))
                    },
                }
            }
            else {
                par_error!(Op,"Error: Unexpected token {} after ofp!",Op.typ.to_string(false))
            }
        }
        TokenType::IntrinsicType(Type) => {
            match Type {
                IntrinsicType::Extern => {
                    let externType = lexer.next();
                    let externType = par_expect!(lexer.currentLocation,externType,"Error: Unexpected abtrupt end of tokens in extern");
                    match externType.typ {
                        TokenType::WordType(Word) => {
                            par_assert!(lexer.currentLocation, !build.contains_symbol(&Word), "Error: Redifinition of existing symbol {}",Word);
                            let mut contract: Option<AnyContract> = None;

                            if let Some(tok) = lexer.peekable().peek() {
                                if tok.typ == TokenType::IntrinsicType(IntrinsicType::OPENPAREN) {
                                    let tok = tok.clone();
                                    contract = Some(parse_any_contract(lexer));
                                    par_assert!(tok, tok.typ==TokenType::IntrinsicType(IntrinsicType::DOTCOMA),"Error: Expected dotcoma at the end of extern definition!");
                                }
                                else{
                                    par_assert!(tok, tok.typ==TokenType::IntrinsicType(IntrinsicType::DOTCOMA),"Error: Expected dotcoma at the end of extern definition!");
                                }
                            }
                            build.externals.insert(Word,External { typ: ExternalType::RawExternal, loc: externType.location.clone(), contract});
                        }
                        TokenType::StringType(Type) | TokenType::CStringType(Type) => {
                            match Type.as_str() {
                                "C" => {
                                    let externWord = lexer.next();
                                    let externWord = externWord.expect("Error: C type extern defined but stream of tokens abruptly ended!");
                                    match externWord.typ {
                                        TokenType::WordType(Word) => {
                                            par_assert!(lexer.currentLocation, !build.contains_symbol(&Word), "Error: Redifinition of existing symbol {}",Word);
                                            let mut contract: Option<AnyContract> = None;
                                            if let Some(tok) = lexer.peekable().peek() {
                                                if tok.typ == TokenType::IntrinsicType(IntrinsicType::OPENPAREN) {
                                                    contract = Some(parse_any_contract(lexer));
                                                    if !lexer.is_newline() {
                                                        let ntc = par_expect!(lexer.currentLocation,lexer.next(),"Error: stream of tokens abruptly ended in extern definition");
                                                        par_assert!(ntc, ntc.typ==TokenType::IntrinsicType(IntrinsicType::DOTCOMA),"Error: Expected dotcoma at the end of extern definition! But found {}",ntc.typ.to_string(false));
                                                    }
                                                }
                                                else {
                                                    par_assert!(tok, tok.typ==TokenType::IntrinsicType(IntrinsicType::DOTCOMA),"Error: Expected dotcoma at the end of extern definition!");
                                                }
                                            }
                                            build.externals.insert(Word,External { typ: ExternalType::CExternal, loc: externWord.location.clone(), contract});

                                        }
                                        TokenType::StringType(Word) => {
                                            par_error!(token, "Error: Expected type Word but found String \"{}\"",Word)
                                        }
                                        Other => {
                                            par_error!(token, "Unexpected token type \"{}\", (Expected Word)",Other.to_string(false));
                                        }
                                    }
                                }
                                typ => {
                                    par_error!(token, "Unexpected behaviour! Unexpected type: {}",typ)
                                }
                            }
                        },
                        other => {
                            par_error!(token,"Unexpected behaviour! expected type Word or String but found {}",other.to_string(false));
                        }

                    }
                }
                IntrinsicType::Func => {

                    let funcName: Option<Token> = lexer.next();
                    let funcName: Token = par_expect!(lexer.currentLocation,funcName,"Unexpected abtrupt end of tokens in func");
                    match funcName.typ {
                        TokenType::WordType(Word) => {
                            par_assert!(lexer.currentLocation, !build.contains_symbol(&Word), "Error: Redifinition of existing symbol {}",Word);
                            par_assert!(token,build.functions.get(&Word).is_none(),"Multiply defined symbols {}!",Word);
                            let contract = parse_function_contract(lexer);
                            lexer.CurrentFuncs.insert(Word.clone());
                            let mut locals: Locals = LinkedHashMap::with_capacity(contract.Inputs.len());
                            for (inn,inp) in contract.Inputs.iter() {
                                locals.insert(inn.clone(), inp.clone());
                            }
                            scopeStack.push(Scope { typ: ScopeType::FUNCTION(Function { contract, body:  vec![(token.location.clone(),Instruction::FNBEGIN())], location: token.location.clone(), locals: Locals::new(), buffers: Vec::new() }, Word), hasBeenOpened: false });
                            currentLocals.push(locals);
                            build.functions.reserve(1);
                        }
                        Other => par_error!(token,"Unexpected behaviour! Expected type Word but found {}",Other.to_string(false))
                    }

                }
                IntrinsicType::OPENPAREN    => par_error!(token, "Error: Unexpected intrinsic type openparen!"),
                IntrinsicType::CLOSEPAREN   => par_error!(token, "Error: Unexpected intrinsic type closeparen!"),
                IntrinsicType::DOUBLE_COLIN => par_error!(token, "Error: Unexpected intrinsic double colin!"),
                IntrinsicType::COMA         => par_error!(token, "Error: Unexpected intrinsic type coma!"),
                IntrinsicType::OPENCURLY => {
                    let ln = scopeStack.len();
                    if ln != 0 {
                        let s = scopeStack.last_mut().unwrap();
                        if s.hasBeenOpened {
                            currentLocals.push(Locals::new());
                            scopeStack.push(Scope { typ: ScopeType::NORMAL(NormalScope { typ: NormalScopeType::EMPTY, body: vec![], locals: Locals::new(), buffers: Vec::new() }), hasBeenOpened: true });
                            return;
                        }
                        par_assert!(token,!s.hasBeenOpened, "Scope already opened! {:?}",scopeStack);
                        s.hasBeenOpened = true;

                        match &s.typ {
                            ScopeType::FUNCTION(_, _) => {}
                            ScopeType::NORMAL(normal) => {
                                
                                currentLocals.push(Locals::new());
                                match normal.typ {
                                    NormalScopeType::IF(_) => {
                                       par_assert!(token, ln > 1, "Error: Alone if outside of any scope is not allowed!");
                                       let prev = scopeStack.get_mut(ln-2).unwrap();
                                       par_assert!(token, prev.body_is_some(), "Error: if can not be declared inside of scope of {} as they do not allow instructions!",prev.typ.to_string(true));
                                    }
                                    NormalScopeType::ELSE | NormalScopeType::EMPTY => {}
                                    NormalScopeType::WHILE(_) => {
                                        par_assert!(token, ln > 1, "Error: Alone if outside of any scope is not allowed!");
                                        let prev = scopeStack.get_mut(ln-2).unwrap();
                                        par_assert!(token, prev.body_is_some(), "Error: if can not be declared inside of scope of {} as they do not allow instructions!",prev.typ.to_string(true));
                                    },
                                }
                            }
                        }
                    }
                    else {

                        scopeStack.push(Scope { typ: ScopeType::NORMAL(NormalScope { typ: NormalScopeType::EMPTY, body: vec![], locals: Locals::new(), buffers: Vec::new()}), hasBeenOpened: true});
                    }
                }
                IntrinsicType::CLOSECURLY => {
                    if let Some(sc) = scopeStack.pop() {
                        par_assert!(token,sc.hasBeenOpened, "Error: scope closed but never opened!");
                        match sc.typ {
                            ScopeType::FUNCTION(mut func, name) => {
                                
                                for (label, locs) in expectedLabels.iter() {
                                    for (_,goto) in locs.iter() {
                                        let t = *goto;
                                
                                        unsafe { *t = Instruction::GOTO(label.clone(), currentLocals.len()) };
                                
                                    }
                                    if currentLabels.contains(label) { continue;}
                                    eprintln!("Error: Label to goto not defined! ({})",label);
                                    for (loc, _) in locs {
                                        eprintln!("{}: defined goto here",loc.loc_display());
                                    }
                                    exit(1);
                                }
                                expectedLabels.clear();
                                currentLabels.clear();
                                func.locals = currentLocals.pop().unwrap();
                                build.functions.insert(name, func);
                            },
                            ScopeType::NORMAL(mut normal) => {
                                
                                normal.locals = currentLocals.pop().unwrap();
                                match normal.typ {
                                    NormalScopeType::IF(_) => {
                                        let currentScope = getTopMut(scopeStack).unwrap();
                                        let body = par_expect!(token,currentScope.body_unwrap_mut(), "Error: Can not close if, because it is inside a {} which doesn't support instructions!",currentScope.typ.to_string(false));
                                        body.push((token.location.clone(),Instruction::EXPAND_IF_SCOPE(normal)))
                                    },
                                    NormalScopeType::EMPTY => {
                                        let currentScope = getTopMut(scopeStack).unwrap();
                                        let body = par_expect!(token,currentScope.body_unwrap_mut(), "Error: Can not close if, because it is inside a {} which doesn't support instructions!",currentScope.typ.to_string(false));
                                        body.push((token.location.clone(),Instruction::EXPAND_SCOPE(normal)));
                                    }
                                    NormalScopeType::ELSE => {
                                        let currentScope = getTopMut(scopeStack).unwrap();

                                        let body = par_expect!(token,currentScope.body_unwrap_mut(), "Error: Can not close if, because it is inside a {} which doesn't support instructions!",currentScope.typ.to_string(false));
                                        body.push((token.location.clone(),Instruction::EXPAND_ELSE_SCOPE(normal)));
                                    },
                                    NormalScopeType::WHILE(_) => {
                                        let currentScope = getTopMut(scopeStack).unwrap();
                                        let body = par_expect!(token,currentScope.body_unwrap_mut(), "Error: Can not close if, because it is inside a {} which doesn't support instructions!",currentScope.typ.to_string(false));
                                        body.push((token.location.clone(),Instruction::EXPAND_WHILE_SCOPE(normal)))
                                    },
                                }
                            }
                        }

                    }
                    else {
                        par_error!(token, "Scope closed but never opened!!!");
                    }
                }
                IntrinsicType::RET => {
                    par_assert!(token, scopeStack.len()> 0 && getTopMut(scopeStack).unwrap().body_is_some(), "Error: Unexpected return intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(scopeStack).unwrap().typ.to_string(false));
                    let result_body: Vec<Token> = lexer.map_while(|t| {
                        if t.typ != TokenType::IntrinsicType(IntrinsicType::DOTCOMA) {
                            Some(t)
                        }
                        else {
                            None
                        }
                    }).collect();
                    let res = tokens_to_expression(&result_body, build, program, &currentLocals,par_expect!(lexer.currentLocation, getTopMut(scopeStack).unwrap().typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"));
                    getTopMut(scopeStack).unwrap().body_unwrap_mut().unwrap().push((token.location.clone(),Instruction::RET(res)));
                },
                IntrinsicType::SYSCALL => {
                    par_assert!(token, scopeStack.len()> 0 && getTopMut(scopeStack).unwrap().body_is_some(), "Error: Unexpected return intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(scopeStack).unwrap().typ.to_string(false));
                    let body = getTopMut(scopeStack).unwrap().body_unwrap_mut().unwrap();
                    body.push((token.location.clone(),Instruction::SYSCALL));
                },
                IntrinsicType::INCLUDE => {
                    let includeName = par_expect!(lexer.currentLocation,lexer.next(),"Error: abruptly ran out of tokens");
                    match includeName.typ {
                        TokenType::StringType(path) | TokenType::CStringType(path) => {
                            let p = PathBuf::from(&program.path);
                            let p = p.parent().unwrap();
                            let include_p = PathBuf::from(&path);
                            let mut opath=  p.join(&include_p).to_str().unwrap().replace("\\", "/");
                            
                            for p_path in include_folders.iter() {
                                let p_buf = PathBuf::from(p_path.to_str().unwrap().to_owned()+"/"+path.as_str());
                                if p_buf.exists() {
                                    opath=p_buf.to_string_lossy().to_owned().to_string();
                                    break;
                                }
                            }
                            
                            //let info = par_expect!(token,fs::read_to_string(opath),"Error: could not open file: {}",String::from(p.join(&include_p).to_str().unwrap()).replace("\\", "/"));
                            let info = par_expect!(token, fs::read_to_string(&opath), "Error: could not read from file {}!",opath);
                            let mut lf = Lexer::new(&info,lexer.Intrinsics,lexer.Definitions,HashSet::new());
                            lf.currentLocation.file = Rc::new(String::from(p.join(&include_p).to_str().unwrap().replace("\\", "/")));
                            let mut nprogram = program.clone();
                            nprogram.path = String::from(p.join(&include_p).to_str().unwrap().replace("\\", "/"));
                            let build2 = parse_tokens_to_build(&mut lf, &mut nprogram,include_folders,build.stringoffset+build.stringdefs.len());
                            build.externals.extend(build2.externals);
                            build.stringdefs.extend(build2.stringdefs);
                            // for (strdefId,_strdef) in build2.stringdefs.iter() {
                            //     let orgstrdefId  = strdefId.clone();
                            //     let mut strdefId = strdefId.clone();
                            //     let isContaining = build.stringdefs.contains_key(&strdefId);
                            //     while build.stringdefs.contains_key(&strdefId) || build2.stringdefs.contains_key(&strdefId) {
                            //         strdefId = Uuid::new_v4();
                            //     }
                            //     if isContaining {
                            //         for (_, cn_cn) in build2.constdefs.iter_mut() {
                            //             match cn_cn.typ {
                            //                 _ => {}
                            //                 _ => {}
                            //                 RawConstValueType::STR(ref mut val) => {
                            //                     if &orgstrdefId == val {
                            //                         *val = strdefId
                            //                     }
                            //                 },
                            //             }
                            //         }
                            //     }
                            // }
                            //build.stringdefs.extend(build2.stringdefs);
                            for (fn_name,fn_fn) in build2.functions {
                                let _loc = fn_fn.location.clone();
                                match build.functions.insert(fn_name.clone(), fn_fn) {
                                    Some(_Other) => {
                                        par_error!(_loc, "Error: multiply defined symbols {}",fn_name);
                                    },
                                    None => {},
                                }
                            }
                            lexer.CurrentFuncs.extend(lf.CurrentFuncs);
                            build.constdefs.reserve(build2.constdefs.len());
                            for (cn_name,cn_val) in build2.constdefs{
                                let loc = cn_val.loc.clone();
                                match build.constdefs.insert(cn_name.clone(),cn_val.clone()) {
                                    Some(_Other) => {
                                        if _Other.typ == cn_val.typ {}
                                        else {
                                            par_error!(loc,"Error: multiply defined symbols {}",cn_name);
                                        }
                                    },
                                    None => {},
                                }
                            }
                        }
                        _ => {
                            par_error!(includeName, "Expected token type String but found {}",includeName.typ.to_string(false));
                        }
                    }
                },

                IntrinsicType::CONSTANT => {
                    let first = lexer.next();
                    let first = par_expect!(lexer.currentLocation,first,"abruptly ran out of tokens in constant name definition");
                    let name: String = match first.typ {
                        TokenType::WordType(ref word) => {
                            par_assert!(first, !lexer.CurrentFuncs.contains(word), "Error: multiple constant symbol definitions! Cannot define a constant that already exists as a function!\nFunction defined at: {}",build.functions.get(word).unwrap().location.loc_display());
                            word.to_string()
                        }
                        _ => {
                            par_error!(first, "Unexpected token type! Expected word but found {}",first.typ.to_string(false));
                        }
                    };
                    par_assert!(first, !build.contains_symbol(&name) || build.constdefs.contains_key(&name), "Error: Cannot define constant {} as that would redefine a symbol of a different type!",name);
                    let first = lexer.next();
                    let first = par_expect!(lexer.currentLocation,first,"abruptly ran out of tokens in constant name definition");
                    let mut expect_type: Option<VarType> = None;
                    match first.typ {
                        TokenType::SETOperation(ref typ) => {
                            par_assert!(first,*typ == SetOp::SET,"Error: Unexpected operation type: {}",typ.to_string());
                        }
                        TokenType::IntrinsicType(typ) => {
                            match typ {
                                IntrinsicType::DOUBLE_COLIN => {
                                    let typ = par_expect!(lexer.currentLocation,lexer.next(), "Error: abruptly ran out of tokens in constant type definition");
                                    if let TokenType::Definition(d) = typ.typ {
                                        expect_type = Some(d);
                                        let typ = par_expect!(lexer.currentLocation,lexer.next(), "Error: abruptly ran out of tokens in constant type definition");
                                        par_assert!(typ, typ.typ == TokenType::SETOperation(SetOp::SET), "Error: unexpected token type {} after constant definition! Expected =",typ.typ.to_string(false));
                                    }
                                    else {
                                        par_error!(typ, "Error: Expected definition but found {}!",typ.typ.to_string(false));
                                    }
                                }
                                _ => {
                                    par_error!(first, "Error: expected = but found {}",typ.to_string(false));
                                }
                            }
                        }
                        _ => {
                            par_error!(first, "Unexpected token type! Expected intrinsic but found {}",first.typ.to_string(false));
                        }
                    }

                    let val = eval_const_def(lexer,build,TokenType::IntrinsicType(IntrinsicType::DOTCOMA));
                    if let Some(expect_type) = expect_type {
                        let tvt = val.typ.to_var_type();
                        par_assert!(lexer.currentLocation, val.typ.is_eq_vartype(&expect_type), "Error: Non matching variable types!\nExpected: {}\nFound: {}",expect_type.to_string(false),if let Some(tvt) = tvt {tvt.to_string(false)} else {"None".to_string()});
                    }
                    let result = match val.typ {
                        ConstValueType::INT(rval) => {
                           RawConstValue {typ: RawConstValueType::INT(rval), loc: val.loc}
                        }
                        ConstValueType::LONG(rval) => {
                            RawConstValue {typ: RawConstValueType::LONG(rval), loc: val.loc}
                        }
                        ConstValueType::STR(rval, typ) => {
                            RawConstValue {typ: RawConstValueType::STR(build.insert_new_str(ProgramString {Data: rval, Typ: typ})), loc: val.loc}
                        }
                        ConstValueType::PTR(typ,v) => {
                            RawConstValue {typ: RawConstValueType::PTR(typ, v), loc: val.loc}
                        }
                        ConstValueType::BOOLEAN(_) => todo!(),
                        ConstValueType::CHAR(rval)     => RawConstValue {typ: RawConstValueType::CHAR(rval), loc: val.loc},
                        ConstValueType::SHORT(rval)   => RawConstValue {typ: RawConstValueType::SHORT(rval), loc: val.loc},

                    };
                    if let Some(constdef) = build.constdefs.get(&name) {
                        let i1 = constdef.typ.to_type(&build);
                        let i2 = result.typ.to_type(&build);
                        par_assert!(result.loc, i1 == i2,"Error: Constant value defined at {}, Redined with a different type at {}!\nOriginal = {:?}\nRedefinition = {:?}",constdef.loc.loc_display(),result.loc.loc_display(),i1,i2);
                    }
                    build.constdefs.insert(name, result);

                },
                IntrinsicType::DOTCOMA => {},

                IntrinsicType::Let => {
                    if scopeStack.len() == 0 {
                        let nametok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for Let");
                        let name = par_expect!(lexer.currentLocation, nametok.unwrap_word(), "Error: Expected word after let but found {}",nametok.typ.to_string(false));
                        par_assert!(nametok, !build.contains_symbol(name), "Error: Redefinition of symbol {} is not allowed! {}",name, {
                            if let Some(loc) = build.get_location_of_symbol(name) {
                                format!("Symbol already defined {} here",loc.loc_display())
                            }
                            else {
                                "".to_owned()
                            }
                        });
                        let typ = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for let type");
                        par_assert!(typ,typ.typ==TokenType::IntrinsicType(IntrinsicType::DOUBLE_COLIN), "Error: You probably forgot to put a : after the name ({})!",name);
                        let typ = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for let type");
                        match &typ.typ {
                            TokenType::IntrinsicType(t) => {
                                match t {
                                    IntrinsicType::OPENSQUARE => {
                                        let ntok = par_expect!(lexer,lexer.next(),"Error: Ran out of tokens for global buffer initialization");
                                        if let TokenType::Definition(buftyp) = &ntok.typ {
                                            let ntok = par_expect!(lexer,lexer.next(),"Error: Ran out of tokens for global buffer initialization");
                                            par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::COMA), "Error: Expected coma after defintion in buffer!");    
                                            let ntok = par_expect!(lexer,lexer.next(),"Error: Ran out of tokens for global buffer initialization");
                                            let size = par_expect!(ntok, ntok.typ.unwrap_numeric(build), "Error: Expected numeric value after coma to indicate the size of the buffer but found {}",ntok.typ);
                                            par_assert!(ntok, size > 0, "Error: Cannot have buffer of size lower than or equal to 0");
                                            let res = build.buffers.len();
                                            build.global_vars.insert(name.clone(), GlobalVar { typ: GlobalVarType::BUFFER(res), loc: typ.location });
                                            build.buffers.push(BuildBuf::from_parse_buf(buftyp.to_owned(), size as usize));
                                            let ntok = par_expect!(lexer,lexer.next(),"Error: Ran out of tokens for global buffer initialization");
                                            par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::CLOSESQUARE), "Error: Expected close square bracket but found {}",ntok.typ);
                                        }
                                        else {
                                            par_error!(ntok, "Error: Expected VarType for buffer but found {}",ntok.typ);
                                        }
                                    }
                                    _ => {
                                        par_error!(typ, "Error: Unexpected intrinsic type: {}",t.to_string(false))
                                    }
                                }
                            }

                            TokenType::Definition(_def) => {
                                let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for Let set instruction");
                                if ntok.typ==TokenType::IntrinsicType(IntrinsicType::DOTCOMA) { todo!("Handle dotcoma for uninitialized globalvars")}
                                par_assert!(ntok,ntok.typ==TokenType::SETOperation(SetOp::SET),"Error: Expected set or dotcoma but found something else!");
                                let t = par_expect!(lexer,lexer.next(),"Error: Ran out of tokens for global initialization");
                                todo!("Finish other global vars\nt: {:?}",t);
                            }
                            _ => {
                                par_error!(typ, "Error: unexpected token type in let definition. Expected VarType but found {}",typ.typ.to_string(false))
                            }
                        }
                    }
                    else {
                        let nametok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for Let");
                        let loc = nametok.location.clone();
                        match nametok.typ {
                            TokenType::WordType(ref name) => {
                                par_assert!(loc, !build.contains_symbol(&name), "Error: Redifinition of existing symbol {}",name);
                                par_assert!(loc, scopeStack.len() > 0, "Error: Cannot have variables outside of scopes");
                                par_assert!(token,  getTopMut(scopeStack).unwrap().body_is_some(), "Error: Unexpected multiply intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(scopeStack).unwrap().typ.to_string(false));
                                let currentScope = getTopMut(scopeStack).unwrap();
                                let typ = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for let type");
                                par_assert!(typ,typ.typ==TokenType::IntrinsicType(IntrinsicType::DOUBLE_COLIN), "Error: You probably forgot to put a : after the name!");
                                let typ = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for let type");
                                match typ.typ {
                                    TokenType::Definition(def) => {
                                        currentLocals.last_mut().unwrap().insert(name.clone(), def);
                                        currentScope.body_unwrap_mut().unwrap().push((lexer.currentLocation.clone(),Instruction::DEFVAR(name.clone())))
                                    }
                                    TokenType::IntrinsicType(ref t) => {
                                        match t {
                                            IntrinsicType::OPENSQUARE => {
                                                let ntok = par_expect!(lexer,lexer.next(),"Error: Ran out of tokens for global buffer initialization");
                                                if let TokenType::Definition(buftyp) = &ntok.typ {
                                                    let ntok = par_expect!(lexer,lexer.next(),"Error: Ran out of tokens for global buffer initialization");
                                                    par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::COMA), "Error: Expected coma after defintion in buffer!");    
                                                    let ntok = par_expect!(lexer,lexer.next(),"Error: Ran out of tokens for global buffer initialization");
                                                    let size = par_expect!(ntok, ntok.typ.unwrap_numeric(build), "Error: Expected numeric value after coma to indicate the size of the buffer but found {}",ntok.typ);
                                                    par_assert!(ntok, size > 0, "Error: Cannot have buffer of size lower than or equal to 0");
                                                    let res = build.buffers.len();
                                                    
                                                    currentLocals.last_mut().unwrap().insert(name.clone(), VarType::PTR(Ptr::ref_to(buftyp.clone()))); // TODO: Implement VarType::BUFFER instead of all of this bullshit
                                                    currentScope.body_unwrap_mut().unwrap().push((lexer.currentLocation.clone(),Instruction::MOV(Expression::val(OfP::LOCALVAR(name.clone())),Expression::val(OfP::BUFFER(res)))));
                                                    currentScope.typ.buffers_unwrap_mut().unwrap().push(BuildBuf::from_parse_buf(buftyp.to_owned(), size as usize));
                                                    
                                                    let ntok = par_expect!(lexer,lexer.next(),"Error: Ran out of tokens for global buffer initialization");
                                                    par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::CLOSESQUARE), "Error: Expected close square bracket but found {}",ntok.typ);
                                                }
                                                else {
                                                    par_error!(ntok, "Error: Expected VarType for buffer but found {}",ntok.typ);
                                                }
                                            }
                                            _ => par_error!(typ, "Error: unexpected token type in let definition. Expected VarType but found {}",t.to_string(false))
                                        }
                                    }
                                    _ => {
                                        par_error!(typ, "Error: unexpected token type in let definition. Expected VarType but found {}",typ.typ.to_string(false))
                                    }
                                }
                                
                            },
                            _ => {
                                par_error!(nametok, "Unexpected name type, Expected Word but found {}", nametok.typ.to_string(false))
                            }
                        }
                        if !lexer.is_newline() {
                            let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for Let set instruction");
                            if ntok.typ==TokenType::IntrinsicType(IntrinsicType::DOTCOMA) { return }
                            par_assert!(ntok,ntok.typ==TokenType::SETOperation(SetOp::SET),"Error: Expected set or dotcoma but found something else!");
                            let body: Vec<Token> = lexer.map_while(|t| {
                                if t.typ == TokenType::IntrinsicType(IntrinsicType::DOTCOMA) {
                                    None
                                }
                                else{
                                    Some(t)
                                }
                            }).collect();
                            let currentScope = getTopMut(scopeStack).unwrap();
                            let res = tokens_to_expression(&body, build, program, &currentLocals,par_expect!(lexer.currentLocation, currentScope.typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"));
                            currentScope.body_unwrap_mut().unwrap().push((lexer.currentLocation.clone(),Instruction::MOV(Expression::val(OfP::LOCALVAR(nametok.unwrap_word().unwrap().clone())),res)))
                        }
                    }
                },
                IntrinsicType::INTERRUPT => {
                    par_assert!(token, scopeStack.len() > 0, "Error: Unexpected interrupt intrinsic outside of scope!");
                    par_assert!(token, getTopMut(scopeStack).unwrap().body_is_some(), "Error: Scopes of type {} do not support instructions!",getTopMut(scopeStack).unwrap().typ.to_string(false));
                    let body = getTopMut(scopeStack).unwrap().body_unwrap_mut().unwrap();
                    let lexerNext = par_expect!(lexer.currentLocation,lexer.next(),"Stream of tokens ended abruptly at INTERRUPT call");
                    match lexerNext.typ {
                        TokenType::Number32(val) => {
                            body.push((lexer.currentLocation.clone(),Instruction::INTERRUPT(val as i64)));
                        }
                        TokenType::Number64(val) => {
                            body.push((lexer.currentLocation.clone(),Instruction::INTERRUPT(val)));
                        }
                        TokenType::WordType(ref data) => {
                            if let Some(cons) = build.constdefs.get(data) {
                                body.push((lexer.currentLocation.clone(),Instruction::INTERRUPT(cons.typ.get_num_data())));
                            }
                            else {
                                par_error!(lexerNext, "Unexpected word type for INTERRUPT, {}",lexerNext.typ.to_string(false))
                            }
                        }
                        _ => {
                            par_error!(lexerNext, "Unexpected token type for INTERRUPT, {}",lexerNext.typ.to_string(false))
                        }
                    }
                },
                IntrinsicType::CAST => todo!(),
                IntrinsicType::DLL_IMPORT => {
                    let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: Abruptly ran out of tokens for DLL_IMPORT");
                    let file_from = par_expect!(ntok, ntok.unwrap_string(), "Error: Expected token string but found {}",ntok.typ.to_string(false));
                    let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: Abruptly ran out of tokens for DLL_IMPORT");
                    let symbol_name = par_expect!(ntok,ntok.unwrap_word(), "Error: Expected token word but found {}",ntok.typ.to_string(false));
                    par_assert!(ntok, !build.contains_symbol(symbol_name), "Error: Trying to overwrite already existing symbol! {}",symbol_name);
                    let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: Abruptly ran out of tokens for DLL_IMPORT");
                    par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::OPENPAREN), "Error: Unexpected symbol {} in DLL_IMPORT! {}",ntok.typ.to_string(false),symbol_name);
                    let symbol_contract = parse_any_contract(lexer);
                    build.dll_imports.insert(symbol_name.clone(), DLL_import { from: file_from.clone(), contract: symbol_contract });
                },
                IntrinsicType::DLL_EXPORT => {
                    let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: Abruptly ran out of tokens for DLL_EXPORT");
                    let symbol_name = par_expect!(ntok,ntok.unwrap_word(), "Error: Expected token word but found {}",ntok.typ.to_string(false));
                    let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: Abruptly ran out of tokens for DLL_EXPORT");
                    par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::OPENPAREN), "Error: Unexpected symbol {} in DLL_EXPORT! {}",ntok.typ.to_string(false),symbol_name);
                    let symbol_contract = parse_any_contract(lexer);
                    build.dll_exports.insert(symbol_name.clone(), DLL_export { contract: symbol_contract });
                },

                IntrinsicType::WHILE => {
                    let condition: Vec<Token> = lexer.map_while(|t| {
                        if t.typ == TokenType::IntrinsicType(IntrinsicType::OPENCURLY) {
                            None
                        }
                        else{
                            Some(t)
                        }
                    }).collect();
                    let condi = tokens_to_expression(&condition, build, program, &currentLocals, par_expect!(lexer.currentLocation, getTopMut(scopeStack).unwrap().typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"));
                    scopeStack.push(Scope { typ: ScopeType::NORMAL(NormalScope { typ: NormalScopeType::WHILE(condi), body: vec![], locals: Locals::new(), buffers: Vec::new()}), hasBeenOpened: true});
                    currentLocals.push(Locals::new());
                },
                IntrinsicType::IF => {
                    let condition: Vec<Token> = lexer.map_while(|t| {
                        if t.typ == TokenType::IntrinsicType(IntrinsicType::OPENCURLY) {
                            None
                        }
                        else{
                            Some(t)
                        }
                    }).collect();
                    let condi = tokens_to_expression(&condition, build, program, &currentLocals, par_expect!(lexer.currentLocation, getTopMut(scopeStack).unwrap().typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"));
                    scopeStack.push(Scope { typ: ScopeType::NORMAL(NormalScope { typ: NormalScopeType::IF(condi), body: vec![], locals: Locals::new(), buffers: Vec::new()}), hasBeenOpened: true});
                    currentLocals.push(Locals::new());

                },
                IntrinsicType::ELSE => {
                    scopeStack.push(Scope { typ: ScopeType::NORMAL(NormalScope { typ: NormalScopeType::ELSE, body: vec![], locals: Locals::new(), buffers: Vec::new() }), hasBeenOpened: false})
                },
                IntrinsicType::THREEDOTS => todo!(),

                IntrinsicType::GOTO => {
                    par_assert!(token, scopeStack.len() > 0, "Unexpected goto intrinsic outside of scope!");
                    par_assert!(token, getTopMut(scopeStack).unwrap().body_is_some(), "Error: Scopes of type {} do not support instructions!",getTopMut(scopeStack).unwrap().typ.to_string(false));
                    let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: Abruptly ran out of tokens for GOTO");
                    par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::OPENPAREN), "Error: Unexpected symbol {} in GOTO! Expected open paren followed by a string (name of the label) and then a closing paren",ntok.typ.to_string(false));
                    let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: Abruptly ran out of tokens for GOTO");
                    let symbol_name = par_expect!(ntok,ntok.unwrap_string(), "Error: Expected token string but found {}",ntok.typ.to_string(false));
                    let body = getTopMut(scopeStack).unwrap().body_unwrap_mut().unwrap();
                    body.push((ntok.location.clone(), Instruction::GOTO(symbol_name.to_owned(), 0)));
                    let len = body.len();
                    if let Some(label) = expectedLabels.get_mut(symbol_name) {
                        let (_,r) = &mut body[len-1];
                        label.push((ntok.location.clone(),r))
                    }
                    else {
                        let (_,r) = &mut body[len-1];
                        expectedLabels.insert(symbol_name.clone(), vec![(ntok.location.clone(),r)]);
                    }
                    let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: Abruptly ran out of tokens for GOTO");
                    par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::CLOSEPAREN), "Error: Unexpected symbol {} in GOTO! Expected open paren followed by a string (name of the label) and then a closing paren",ntok.typ.to_string(false));
                    
                },
                IntrinsicType::MAKELABEL => {
                    par_assert!(token, scopeStack.len() > 0, "Unexpected makelabel intrinsic outside of scope!");
                    par_assert!(token, getTopMut(scopeStack).unwrap().body_is_some(), "Error: Scopes of type {} do not support instructions!",getTopMut(scopeStack).unwrap().typ.to_string(false));
                    let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: Abruptly ran out of tokens for MAKELABEL");
                    par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::OPENPAREN), "Error: Unexpected symbol {} in MAKELABEL! Expected open paren followed by a string (name of the label) and then a closing paren",ntok.typ.to_string(false));
                    let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: Abruptly ran out of tokens for MAKELABEL");
                    let symbol_name = par_expect!(ntok,ntok.unwrap_string(), "Error: Expected token string but found {}",ntok.typ.to_string(false));
                    currentLabels.insert(symbol_name.to_owned());
                    let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: Abruptly ran out of tokens for MAKELABEL");
                    par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::CLOSEPAREN), "Error: Unexpected symbol {} in MAKELABEL! Expected open paren followed by a string (name of the label) and then a closing paren",ntok.typ.to_string(false));
                    let body = getTopMut(scopeStack).unwrap().body_unwrap_mut().unwrap();
                    body.push((ntok.location.clone(), Instruction::MAKELABEL(symbol_name.to_owned())))
                },
                IntrinsicType::OPENSQUARE => todo!(),
                IntrinsicType::CLOSESQUARE => todo!(),

            }
        }
        TokenType::StringType(_) => {
            par_error!(lexer.currentLocation, "Error: Unexpected Token type String!");
        }
        TokenType::CStringType(_) => {
            par_error!(lexer.currentLocation, "Error: Unexpected Token type CString!");
        }

        TokenType::CharType(_) => {
            todo!("{}: Unexpected char! Chars",token.loc_display())

        }
        TokenType::Number32(_) => {
            par_error!(lexer.currentLocation, "Error: Unexpected Token type Integer!");
        }
        TokenType::Number64(_) => {
            par_error!(lexer.currentLocation, "Error: Unexpected Token type Long!");
        }
        TokenType::Definition(_) => todo!("{}",lexer.currentLocation.loc_display()),
        TokenType::CStringType(_) => todo!(),
        TokenType::Function(name) => {
            let args = parse_argument_contract(lexer, build, &program, currentLocals);
            let body = getTopMut(scopeStack).unwrap().body_unwrap_mut().unwrap();
            body.push((token.location.clone(),Instruction::CALL(name, args)));

        },
        TokenType::Register(reg) => {
            let currentScope = getTopMut(scopeStack).unwrap();
            let regOp = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected register operation or another register!");
            match regOp.typ {
                TokenType::SETOperation(ref op) => {
                    match op {
                        SetOp::SET => {
                            let token = par_expect!(lexer.currentLocation,lexer.next(),"abruptly ran out of tokens");
                            match token.typ {
                                TokenType::Number32(data) => {
                                    if reg.size() >= 4 {
                                        let body = currentScope.body_unwrap_mut().unwrap();
                                        body.push((token.location,Instruction::MOV(Expression::val(OfP::REGISTER(reg)), Expression::val(OfP::CONST(RawConstValueType::INT(data))))))
                                    }
                                }
                                TokenType::Number64(data) => {
                                    if reg.size() >= 8 {
                                        let body = currentScope.body_unwrap_mut().unwrap();
                                        body.push((token.location,Instruction::MOV(Expression::val(OfP::REGISTER(reg)), Expression::val(OfP::CONST(RawConstValueType::LONG(data))))));
                                    }
                                }
                                TokenType::Register(reg2) => {
                                    currentScope.body_unwrap_mut().unwrap().push((token.location.clone(),Instruction::MOV(Expression::val(OfP::REGISTER(reg)), Expression::val(OfP::REGISTER(reg2)))));

                                }
                                TokenType::WordType(ref data) => {
                                    if currentScope.contract_is_some() && currentScope.contract_unwrap().unwrap().Inputs.contains_key(data) {
                                        let contract = currentScope.contract_unwrap().unwrap();
                                        par_assert!(token, contract.Inputs.contains_key(data), "Error: Unexpected word for register: '{}'",data);
                                        currentScope.body_unwrap_mut().unwrap().push((token.location.clone(), Instruction::MOV(Expression::val(OfP::REGISTER(reg)), Expression::val(OfP::LOCALVAR(data.to_owned())))))
                                    }
                                    else if let Some(cons) = build.constdefs.get(data) {
                                        currentScope.body_unwrap_mut().unwrap().push((token.location.clone(), Instruction::MOV(Expression::val(OfP::REGISTER(reg)), Expression::val(OfP::CONST(cons.typ.clone())))))
                                    }
                                    else {
                                        par_error!(token,"Unexpected Type for Mov Intrinsic. Expected Number32/Number64 but found {}",token.typ.to_string(false))
                                    }
                                }
                                _ => par_error!(token,"Unexpected Type for Mov Intrinsic. Expected Number32/Number64 but found {}",token.typ.to_string(false))
                            }
                        }
                        _ => {
                            par_error!(regOp,"Error: Unexepected operation {}",op.to_string());
                        }
                    }
                }
                TokenType::Register(reg2) => {
                    par_assert!(token,reg.size()==reg2.size(),"Gotten two differently sized registers to one op!");
                    let regOp = lexer.next().expect(&format!("(P) [ERROR] {}:{}:{}: Unexpected register operation!",token.location.clone().file,&token.location.clone().linenumber,&token.location.clone().character));
                    let body = currentScope.body_unwrap_mut().unwrap();
                    match regOp.typ {
                        TokenType::SETOperation(typ) => {
                            match typ {
                                SetOp::PLUSSET => {
                                    body.push((token.location.clone(),Instruction::ADDSET(Expression::val(OfP::REGISTER(reg)), Expression::val(OfP::REGISTER(reg2)))))
                                }
                                SetOp::MINUSSET => {
                                    body.push((token.location.clone(),Instruction::SUBSET(Expression::val(OfP::REGISTER(reg)), Expression::val(OfP::REGISTER(reg2)))))
                                }
                                SetOp::MULSET => {
                                    body.push((token.location.clone(),Instruction::MULSET(Expression::val(OfP::REGISTER(reg)), Expression::val(OfP::REGISTER(reg2)))))
                                }
                                SetOp::DIVSET => {
                                    body.push((token.location.clone(),Instruction::DIVSET(Expression::val(OfP::REGISTER(reg)), Expression::val(OfP::REGISTER(reg2)))))
                                }
                                SetOp::SET => {
                                    body.push((token.location.clone(),Instruction::MOV(Expression::val(OfP::REGISTER(reg)), Expression::val(OfP::REGISTER(reg2)))))
                                }
                                other => par_error!(token,"Unexpected Operation! Expected Register Operation but found {}",other.to_string())
                            }
                        }
                        other => {
                            par_error!(token, "Unexpected token type: Expected Op but found {}",other.to_string(false));
                        }
                    }
                }
                typ => {
                    par_error!(token,"Unexpected register operation! Expected Op or another Register but found {}",typ.to_string(false));
                }
            }
        },
        TokenType::Operation(ref op) => {
            par_assert!(token, *op == Op::STAR, "Error: Cannot have op without a star!");
            let mut setop: Option<SetOp> = None;
            let mut body: Vec<Token> = vec![token.clone()];
            body.extend(lexer.map_while(|t| {
                if t.typ.is_setop() {
                    setop = Some(t.typ.unwrap_setop().clone());
                    None
                }
                else {
                    Some(t)
                }
            }));
            let setop = par_expect!(lexer.currentLocation, setop, "Error: Expected op but found nothing!");
            
            let currentScope = getTopMut(scopeStack).unwrap();
            let expr_body: Vec<Token> = lexer.map_while(|t| if t.typ != TokenType::IntrinsicType(IntrinsicType::DOTCOMA) {Some(t)} else { None }).collect();
            let expr = tokens_to_expression(&expr_body, build, program, &currentLocals, par_expect!(lexer.currentLocation, currentScope.typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"));
            let expr_2 = tokens_to_expression(&body, build, program, &currentLocals,par_expect!(lexer.currentLocation, currentScope.typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"));
            match &setop {
                
                SetOp::SET      => {
                    currentScope.body_unwrap_mut().unwrap().push((token.location.clone(),Instruction::MOV(expr_2,expr)))
                },
                SetOp::PLUSSET  => {
                    currentScope.body_unwrap_mut().unwrap().push((token.location.clone(),Instruction::ADDSET(expr_2, expr)))
                },
                SetOp::MINUSSET => {
                    currentScope.body_unwrap_mut().unwrap().push((token.location.clone(),Instruction::SUBSET(expr_2, expr)))
                },
                SetOp::MULSET   => {
                    currentScope.body_unwrap_mut().unwrap().push((token.location.clone(),Instruction::MULSET(expr_2, expr)))
                },
                SetOp::DIVSET   => {
                    currentScope.body_unwrap_mut().unwrap().push((token.location.clone(),Instruction::DIVSET(expr_2, expr)))
                },
            }
        },
        TokenType::SETOperation(_) => todo!(),
        TokenType::Number8(_) => todo!(),
        TokenType::Number16(_) => todo!(),
    }

}
fn parse_tokens_to_build(lexer: &mut Lexer, program: &mut CmdProgram, include_folders: &HashSet<PathBuf>, string_offset: usize) -> BuildProgram {
    let mut build: BuildProgram = BuildProgram::new();
    build.stringoffset = string_offset;
    let mut scopeStack: ScopeStack = vec![];
    let mut currentLocals: Vec<Locals> = Vec::new();
    let mut currentLabels: HashSet<String> = HashSet::new();
    let mut expectedLabels: HashMap<String,Vec<(ProgramLocation,*mut Instruction)>> = HashMap::new();
    while let Some(token) = lexer.next() {
        parse_token_to_build_inst(token, lexer, program, &mut build, &mut scopeStack, &mut currentLocals, &mut currentLabels, &mut expectedLabels,include_folders);
    }
    
    build
}

struct optim_ops {
    usedStrings: HashMap<usize, String>,
    usedExterns: HashSet<String>,
    usedFuncs: HashSet<String>,
}
impl optim_ops {
    fn new() -> Self{
        Self { usedStrings: HashMap::new(), usedExterns: HashSet::new(), usedFuncs: HashSet::new() }
    }
}
fn optimization_ops_scope(build: &BuildProgram, program: &CmdProgram, scope: TCScopeType, out: &mut optim_ops,fn_name: String) {
    for (_,op) in scope.get_body(build).iter() {
        match op {
            Instruction::CALLRAW(r, args) => {
                for arg in args {
                    match arg {
                        OfP::CONST(val) => {
                            match val {
                                RawConstValueType::STR(id) => {
                                    out.usedStrings.insert(id.clone(), fn_name.clone());
                                },
                                _ => {}
                            }
                        },

                        _ => {}
                    }
                }
                out.usedExterns.insert(r.clone());
            }
            Instruction::MOV(_, v2) => {
                if v2.is_ofp() {
                    match v2.unwrap_val() {
                        OfP::CONST(val) => {
                            match val {
                                RawConstValueType::STR(uuid) => {
                                    out.usedStrings.insert(uuid.clone(), fn_name.clone());
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
            }
            Instruction::CALLRAW(r, args) => {
                for arg in args {
                    match arg {
                        OfP::CONST(val) => {
                            match val {
                                RawConstValueType::STR(uuid) => {
                                    out.usedStrings.insert(uuid.clone(), fn_name.clone());
                                },
                                _ => {}
                            }
                        },

                        _ => {}
                    }
                }
                out.usedExterns.insert(r.clone());
            }
            Instruction::CALL(r,args) => {
                for arg in args {
                    match arg {
                        OfP::CONST(val) => {
                            match val {
                                RawConstValueType::STR(uuid) => {
                                    out.usedStrings.insert(uuid.clone(), fn_name.clone());
                                },
                                _ => {}
                            }
                        },

                        _ => {}
                    }
                }

                out.usedFuncs.insert(r.clone());
            }
            Instruction::EXPAND_SCOPE(s) | Instruction::EXPAND_IF_SCOPE(s) | Instruction::EXPAND_ELSE_SCOPE(s) => {
                optimization_ops_scope(build, program, TCScopeType::NORMAL(&s), out, fn_name.clone())
            }

            _ => {}
        }
    }
}
fn optimization_ops(build: &mut BuildProgram, program: &CmdProgram) -> optim_ops{
    match program.in_mode {
        OptimizationMode::RELEASE => {
            let mut out = optim_ops::new();

            for fn_name in build.functions.keys() {
                optimization_ops_scope(build, program, TCScopeType::FUNCTION(fn_name.clone()), &mut out, fn_name.clone())
            }
            out
        },
        OptimizationMode::DEBUG => optim_ops { usedStrings: HashMap::new(), usedExterns: HashSet::new(), usedFuncs: HashSet::new() },
    }
}
fn nasm_x86_64_prep_args(program: &CmdProgram, build: &BuildProgram, f: &mut File, contract: &Vec<OfP>, mut stack_size: usize, local_vars: &Vec<HashMap<String, LocalVariable>>) -> io::Result<usize> {
    let org_stack_size = stack_size;
    let mut int_passed_count: usize = 0;
    let mut offset: usize=0;
    let shadow_space = if let ArcPassType::CUSTOM(arc) = &program.architecture.options.argumentPassing { arc.shadow_space } else { 0 };
    if program.architecture.options.argumentPassing != ArcPassType::PUSHALL {
        for iargs in contract.iter().rev() {
            if program.architecture.options.argumentPassing.custom_unwrap().nums_ptrs.is_some() && int_passed_count >= program.architecture.options.argumentPassing.custom_unwrap().nums_ptrs.as_ref().unwrap().len() {
                offset += iargs.var_type(build, local_vars).unwrap().get_size(program);
            }
            match iargs.var_type(build, local_vars).unwrap()  {
                VarType::CHAR    | VarType::SHORT   | VarType::BOOLEAN | VarType::INT     | VarType::LONG    | VarType::PTR(_)  => int_passed_count+=1,
                VarType::CUSTOM(_) => {},
            }
        }
    }
    let additional_space: usize = offset;
    for arg in contract.iter().rev() {
        match arg {
            OfP::GLOBALVAR(v) => {
                let var1 = build.global_vars.get(v).expect("Unknown local variable parameter");
                let oreg = match &var1.typ {
                    GlobalVarType::BUFFER(i) => {
                        let oreg = Register::RAX.to_byte_size((program.architecture.bits/8) as usize);
                        writeln!(f, "   mov {}, _GLOBL_{}",oreg,i)?;
                        oreg
                    }
                    GlobalVarType::CONSTANT(val) => {
                        let oreg = Register::RAX.to_byte_size(val.to_type(build)[0].get_size(program));
                        writeln!(f,"   mov {}, {} [_GLOBL_{}]",oreg,size_to_nasm_type(oreg.size()),v)?;
                        oreg
                    },
                };
                
                if program.architecture.options.argumentPassing == ArcPassType::PUSHALL{
                    stack_size += oreg.size();
                    writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(),stack_size-org_stack_size, oreg.to_string())?
                }
                else {
                    let custompassing = program.architecture.options.argumentPassing.custom_unwrap();
                    if let Some(nptrs) = custompassing.nums_ptrs.as_ref() {
                        if let Some(oreg2) = nptrs.get(int_passed_count-1) {
                            writeln!(f, "   mov {}, {}",oreg2.to_byte_size(oreg.size()).to_string(), oreg.to_string())?;
                        }
                        else {
                            stack_size += oreg.size();
                            if additional_space-offset+oreg.size() < shadow_space {
                                writeln!(f, "   mov {} [{}+{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), shadow_space-(additional_space-offset+oreg.size()), oreg.to_string())?;
                            }
                            else if additional_space-offset+oreg.size() == shadow_space {
                                writeln!(f, "   mov {} [{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), oreg)?
                            }
                            else {
                                writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), additional_space-offset+oreg.size()-shadow_space, oreg.to_string())?;
                            }
                            offset-=oreg.size();
                            
                        }
                        int_passed_count-=1;
                    }
                    else {
                        stack_size += oreg.size();
                        writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), offset, oreg.to_string())?;
                        offset-=oreg.size();
                        
                    }
                }
            }
            OfP::LOCALVAR(v) => {
                let var1 = get_local_build(local_vars, v).expect("Unknown local variable parameter");
                let oreg = Register::RAX.to_byte_size(var1.typ.get_size(program));
                if stack_size-var1.operand == 0 {
                    writeln!(f, "   mov {}, {} [{}]",oreg.to_string(),size_to_nasm_type(oreg.size()),program.stack_ptr())?;
                }
                else {
                    writeln!(f, "   mov {}, {} [{}+{}]",oreg.to_string(),size_to_nasm_type(oreg.size()),program.stack_ptr(),stack_size-var1.operand)?;
                }
                if program.architecture.options.argumentPassing == ArcPassType::PUSHALL{
                    stack_size += oreg.size();
                    writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(),stack_size-org_stack_size, oreg.to_string())?
                }
                else {
                    let custompassing = program.architecture.options.argumentPassing.custom_unwrap();
                    if let Some(nptrs) = custompassing.nums_ptrs.as_ref() {
                        if let Some(oreg2) = nptrs.get(int_passed_count-1) {
                            writeln!(f, "   mov {}, {}",oreg2.to_byte_size(oreg.size()).to_string(), oreg.to_string())?;
                        }
                        else {
                            stack_size += oreg.size();
                            if additional_space-offset+oreg.size() < shadow_space {
                                writeln!(f, "   mov {} [{}+{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), shadow_space-(additional_space-offset+oreg.size()), oreg.to_string())?;
                            }
                            else if additional_space-offset+oreg.size() == shadow_space {
                                writeln!(f, "   mov {} [{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), oreg)?
                            }
                            else {
                                writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), additional_space-offset+oreg.size()-shadow_space, oreg.to_string())?;
                            }
                            offset-=oreg.size();
                            
                        }
                        int_passed_count-=1;
                    }
                    else {
                        stack_size += oreg.size();
                        writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(),offset, oreg.to_string())?;
                        offset-=oreg.size();
                        
                    }
                }
            }
            OfP::CONST(val)  => {
                let oregs = val.LRNasm(f, build, program,&vec![Register::RAX, Register::RBX])?;
                if program.architecture.options.argumentPassing == ArcPassType::PUSHALL ||  program.architecture.options.argumentPassing.custom_get().is_none() ||  program.architecture.options.argumentPassing.custom_unwrap().nums_ptrs.is_none(){
                    for oreg in oregs {
                        stack_size += oreg.size();
                        writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), offset, oreg.to_string())?;
                        offset-=oreg.size();
                    }
                }
                else {
                    let custompassing = program.architecture.options.argumentPassing.custom_unwrap();
                    if let Some(nptrs) = custompassing.nums_ptrs.as_ref() {
                        for oreg in oregs {
                            if let Some(oreg2) = nptrs.get(int_passed_count-1) {
                                writeln!(f, "   mov {}, {}",oreg2.to_byte_size(oreg.size()).to_string(), oreg.to_string())?;
                            }
                            else {
                                stack_size += oreg.size();
                                if additional_space-offset+oreg.size() < shadow_space {
                                    writeln!(f, "   mov {} [{}+{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), shadow_space-(additional_space-offset+oreg.size()), oreg.to_string())?;
                                }
                                else if additional_space-offset+oreg.size() == shadow_space {
                                    writeln!(f, "   mov {} [{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), oreg)?
                                }
                                else {
                                    writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), additional_space-offset+oreg.size()-shadow_space, oreg.to_string())?;
                                }
                                offset-=oreg.size();
                            }
                            int_passed_count-=1;
                        }
                    }
                    else {
                        panic!("Unreachable")
                    }
                }
                //println!("stack_size after = {}",stack_size);
            }
            _ => todo!(),
        }
    }
    
    //println!("Final stack_size={}",stack_size);
    //if program.architecture.bits == 64 {
    //    stack_size += 8-stack_size%8;
    //}
    //println!("Final stack_size={}",stack_size);
    if stack_size-org_stack_size > 0 {
        writeln!(f, "   sub {}, {}",program.stack_ptr(),stack_size-org_stack_size)?; //TODO: Setup stackframe only at the start of the scope
    }
    Ok(stack_size)
}
fn nasm_x86_64_load_args(f: &mut File, scope: &TCScopeType, build: &BuildProgram, program: &CmdProgram) -> io::Result<usize> {
    let mut shadow_space = 0;
    if let Some(ops) = program.architecture.options.argumentPassing.custom_get() {
        if ops.shadow_space > 0 {
            shadow_space = ops.shadow_space;
        }
    }
    let mut offset: usize = 0;
    if program.architecture.options.argumentPassing == ArcPassType::PUSHALL {        
        todo!("Im sure there is a bug to do here with x86 assembly and passing parameters and offsets having to be 8 aligned");
        
        for (_, iarg) in scope.get_contract(build).unwrap().Inputs.iter().rev() {
            
            offset+=iarg.get_size(program);
        }
        for (_, iarg) in scope.get_contract(build).unwrap().Inputs.iter().rev() {
            let osize = iarg.get_size(program);
            let reg = Register::RAX.to_byte_size(osize);
            if offset+shadow_space > 0 {
                writeln!(f, "   mov {}, {} [{}+{}]",reg.to_string(), size_to_nasm_type(osize),program.stack_ptr(),offset+shadow_space)?;
            }
            writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(osize),program.stack_ptr(),offset,reg.to_string())?;
            offset-=osize
        };
    }
    else {
        let custom_build = program.architecture.options.argumentPassing.custom_unwrap();

        let mut offset_of_ins: usize = 0;
        let mut int_ptr_count: usize = 0;

        for (_, iarg) in scope.get_contract(build).unwrap().Inputs.iter().rev() {
            if offset%8+iarg.get_size(program) > 8 {
                offset+=8-offset%8;
            }
            offset+=iarg.get_size(program);
            if program.architecture.options.argumentPassing.custom_unwrap().nums_ptrs.is_some() && int_ptr_count >= program.architecture.options.argumentPassing.custom_unwrap().nums_ptrs.as_ref().unwrap().len() {
                if offset_of_ins%8+iarg.get_size(program) > 8 {
                    offset_of_ins+=8-offset_of_ins%8;
                }
                offset_of_ins += iarg.get_size(program);
            }
            match iarg {
                VarType::CHAR    | VarType::SHORT   | VarType::BOOLEAN | VarType::INT     | VarType::LONG    | VarType::PTR(_)  => int_ptr_count+=1,
                VarType::CUSTOM(_) => {},
            }
        }
        if offset%8 > 0 {
            offset+=offset%8;
        }
        if offset_of_ins%8 > 0 {
            offset_of_ins+=offset_of_ins%8;
        }
        for (
            _, iarg) in scope.get_contract(build).unwrap().Inputs.iter().rev() {
            if custom_build.nums_ptrs.is_some() && custom_build.nums_ptrs.as_ref().unwrap().len() > int_ptr_count-1 {
                let osize = iarg.get_size(program);
                let ireg = &custom_build.nums_ptrs.as_ref().unwrap()[int_ptr_count-1].to_byte_size(osize);
                writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(osize), program.stack_ptr(), offset, ireg.to_string())?;
                if (offset_of_ins-osize)%8+osize > 8 {
                    offset_of_ins-=8-(offset_of_ins-osize)%8;
                }
                offset-=osize;
                int_ptr_count-=1
            }
            else {
                let osize = iarg.get_size(program);
                let reg = Register::RAX.to_byte_size(osize);
                writeln!(f, "   mov {}, {} [{}+{}]",reg.to_string(), size_to_nasm_type(osize),program.stack_ptr(),offset_of_ins+shadow_space+osize)?;
                writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(osize),program.stack_ptr(),offset,reg.to_string())?;
                if (offset_of_ins-osize)%8+osize > 8 {
                    offset_of_ins-=8-(offset_of_ins-osize)%8;
                }
                offset_of_ins-=osize;
                if offset%8-osize > 8 {
                    offset-=8-offset%8;
                }
                offset-=osize;
                int_ptr_count-=1
            }
        };
    }
    Ok(offset)
}
fn get_local_build<'a>(currentLocals: &'a Vec<HashMap<String, LocalVariable>>, name: &String) -> Option<&'a LocalVariable> {
    for e in currentLocals {
        if let Some(v) = e.get(name) {
            return Some(v);
        }
    }
    None
}

fn nasm_x86_64_handle_scope(f: &mut File, build: &BuildProgram, program: &CmdProgram, scope: TCScopeType, local_vars: &mut Vec<HashMap<String, LocalVariable>>, stack_sizes: &mut Vec<usize>, mut stack_size: usize, func_stack_begin: usize, inst_count: &mut usize) -> io::Result<()> {
    //println!("Gotten stack_size: {} ",stack_size);
    let shadow_space: usize = if let ArcPassType::CUSTOM(argpassing) = &program.architecture.options.argumentPassing {
        argpassing.shadow_space
    } else { 0 };
    *inst_count += 1;
    let expect_loc_t = LinkedHashMap::new();
    let contract_loc_t = FunctionContract { Inputs: LinkedHashMap::new(), Outputs: Vec::new()};
    let expect_locals = scope.get_locals(build).unwrap_or(&expect_loc_t);
    let contract = scope.get_contract(build).unwrap_or(&contract_loc_t);
    let stack_size_org: usize = stack_size;
    local_vars.reserve(contract.Inputs.len()+expect_locals.len());
    if scope.has_contract() {
        let _ = nasm_x86_64_load_args(f, &scope, build, program)?;
    }
    for (name, val) in expect_locals.iter() {
        if stack_size%8+val.get_size(program)>8 {
           // println!("Adding padding...");
            stack_size+=8-stack_size%8;
        }
        stack_size += val.get_size(program);
        let res = local_vars.len()-1;
        local_vars[res].insert(name.clone(), LocalVariable { typ: val.clone(), operand: stack_size });
    }
    
    let mut bufs = scope.get_buffers(build).clone();
    for buf in bufs.iter_mut() {
        stack_size += buf.size*buf.typ.get_size(program);
        buf.offset = stack_size;
    }
    if stack_size%8 > 0 {
        stack_size+=8-stack_size%8;
    }
    stack_size+=shadow_space;
    //println!("stack_size after: {}",stack_size);
    //println!("stack_size%8: {}",stack_size%8);
    let dif = stack_size-stack_size_org;
    
    if dif > 0 {
        writeln!(f, "   sub {}, {}",program.stack_ptr(),dif)?;
    }
    for (i,(loc,inst)) in scope.get_body(build).iter().enumerate() {
        match inst {
            Instruction::MOV(Op, Op2) => {

                if let Expression::val(Op) = Op {
                    match Op {
                        OfP::REGISTER(Reg1) => {
                            let oreg2 = Register::RBX.to_byte_size(Reg1.size());
                            //TODO: This may break
                            Op2.LEIRnasm(vec![*Reg1,oreg2], f, program, build,&local_vars, &bufs,stack_size, loc)?;
                        }
                        OfP::LOCALVAR(varOrg) => {
                            let var = com_expect!(loc,get_local_build(&local_vars,varOrg),"Error: Unknown variable found during compilation {}",varOrg);
                            let oreg  = Register::RAX.to_byte_size(var.typ.get_size(program));
                            let oreg2 = Register::RBX.to_byte_size(var.typ.get_size(program));
                            let oregs = Op2.LEIRnasm(vec![oreg,oreg2,Register::RCX.to_byte_size(var.typ.get_size(program))], f, program,build, &local_vars, &bufs,stack_size, loc)?;
                            if stack_size-var.operand == 0 {
                                writeln!(f, "   mov {} [{}], {}",size_to_nasm_type(var.typ.get_size(program)),program.stack_ptr(), oregs[0].to_byte_size(var.typ.get_size(program)).to_string())?;
                            }
                            else {
                                writeln!(f, "   mov {} [{}+{}], {}",size_to_nasm_type(var.typ.get_size(program)),program.stack_ptr(),stack_size-var.operand, oregs[0].to_byte_size(var.typ.get_size(program)).to_string())?;
                            }
                        }
                        _ => {
                            todo!("Unsupported");
                        }
                    }
                }
                else {
                    let expr = Op.unwrap_expr();
                    com_assert!(loc, expr.op == Op::STAR, "Error: Expected op STAR but found {}",expr.op.to_string());
                    let res_right = expr.right.as_ref().unwrap().result_of_c(program, build, &local_vars, loc).unwrap();
                    
                    com_assert!(loc, res_right.is_some_ptr(), "Error: Cannot dereference void pointer");
                    let oregs = expr.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX.to_byte_size(res_right.get_size(program)),Register::RBX.to_byte_size(res_right.get_size(program)),Register::RCX.to_byte_size(res_right.get_size(program))], f, program, build, &local_vars, &bufs,stack_size, loc)?;
                    let res_deref_val = res_right.get_ptr_val().unwrap();
                    writeln!(f, "   mov rsi, {}",oregs[0].to_string())?;
                    let oregs2 = Op2.LEIRnasm(vec![Register::RAX.to_byte_size(res_deref_val.get_size(program)),Register::RBX.to_byte_size(res_deref_val.get_size(program)),Register::RCX.to_byte_size(res_deref_val.get_size(program))], f, program, build, &local_vars, &bufs,stack_size, loc)?;
                    writeln!(f, "   mov {} [rsi], {}",size_to_nasm_type(res_deref_val.get_size(program)),oregs2[0].to_byte_size(res_deref_val.get_size(program)).to_string())?;
                }
            }
            Instruction::CALLRAW(Word, contract) => {
                let sp_taken = nasm_x86_64_prep_args(program, build, f, contract, stack_size, &local_vars)?;
                let rax = Register::RAX.to_byte_size((program.architecture.bits/8) as usize);
                writeln!(f, "   xor {}, {}",rax,rax)?;
                if let Some(external) = build.externals.get(Word) {
                    writeln!(f, "   call {}{}{}",external.typ.prefix(program),Word,external.typ.suffix())?;
                }
                else {
                    writeln!(f, "   call {}",Word)?;
                }
                
                if sp_taken-stack_size > 0 {
                    writeln!(f, "   add {}, {}",program.stack_ptr(),sp_taken-stack_size)?;
                }
            }
            Instruction::ADDSET(op1, op2) => {
                let op1 = op1.unwrap_val();
                match op1 {
                    OfP::REGISTER(reg1) => {
                        let reg2 = Register::RAX.to_byte_size(reg1.size());
                        let reg3 = Register::RBX.to_byte_size(reg1.size());
                        op2.LEIRnasm(vec![reg2,reg3], f, program,build, &local_vars, &bufs,stack_size, loc)?;
                        writeln!(f, "   add {}, {}",reg1.to_string(), reg2.to_string())?;
                    }
                    OfP::LOCALVAR(var1) => {
                        let var1 = com_expect!(loc,get_local_build(&local_vars,var1),"Error: Unknown variable found during compilation {}",var1);
                        let reg1 = Register::RAX.to_byte_size(var1.typ.get_size(program));
                        let reg2 = Register::RBX.to_byte_size(reg1.size());
                        op2.LEIRnasm(vec![reg1,reg2], f, program,build, &local_vars, &bufs,stack_size, loc)?;
                        if stack_size-var1.operand == 0 {
                            writeln!(f, "   add {} [{}], {}",size_to_nasm_type(var1.typ.get_size(program)),program.stack_ptr(),reg1.to_string())?;
                        }
                        else {
                            writeln!(f, "   add {} [{}+{}], {}",size_to_nasm_type(var1.typ.get_size(program)),program.stack_ptr(),stack_size-var1.operand,reg1.to_string())?;
                        }
                    },
                    OfP::CONST(_) => todo!(),
                    OfP::RESULT(_, _) => todo!(),
                    _ => todo!()
                }
            }
            Instruction::SUBSET(op1, op2) => {
                let op1 = op1.unwrap_val();
                match op1 {
                    OfP::REGISTER(reg1) => {
                        let reg2 = Register::RAX.to_byte_size(reg1.size());
                        let reg3 = Register::RBX.to_byte_size(reg1.size());
                        op2.LEIRnasm(vec![reg2,reg3], f, program,build, &local_vars, &bufs,stack_size, loc)?;
                        writeln!(f, "   sub {}, {}",reg1.to_string(), reg2.to_string())?;
                    }
                    OfP::LOCALVAR(var1) => {
                        let var1 = com_expect!(loc,get_local_build(&local_vars,var1),"Error: Unknown variable found during compilation {}",var1);
                        let reg1 = Register::RAX.to_byte_size(var1.typ.get_size(program));
                        let reg2 = Register::RBX.to_byte_size(reg1.size());
                        op2.LEIRnasm(vec![reg1,reg2], f, program,build, &local_vars, &bufs,stack_size, loc)?;
                        if stack_size-var1.operand == 0 {
                            writeln!(f, "   sub {} [{}], {}",size_to_nasm_type(var1.typ.get_size(program)),program.stack_ptr(),reg1.to_string())?;
                        }
                        else {
                            writeln!(f, "   sub {} [{}+{}], {}",size_to_nasm_type(var1.typ.get_size(program)),program.stack_ptr(),stack_size-var1.operand,reg1.to_string())?;
                        }
                    },
                    OfP::CONST(_) => todo!(),
                    OfP::RESULT(_, _) => todo!(),
                    _ => todo!()
                }
            }
            Instruction::MULSET(op1, op2) => {
                let op1 = op1.unwrap_val();
                match op1 {
                    OfP::REGISTER(reg1) => {
                        let reg2 = Register::RAX.to_byte_size(reg1.size());
                        let res = op2.LEIRnasm(vec![reg2], f, program,build, &local_vars, &bufs,stack_size, loc)?;
                        if res[0].to_byte_size(8) != Register::RAX {
                            writeln!(f, "   mov {}, {}",Register::RAX.to_byte_size(res[0].size()),res[0])?
                        }
                        writeln!(f, "   cqo")?;
                        writeln!(f, "   imul {}",reg1.to_string())?;
                    }
                    OfP::LOCALVAR(var1) => {
                        let var1 = com_expect!(loc,get_local_build(&local_vars, var1),"Error: Unknown variable found during compilation {}",var1);
                        let reg1 = Register::RAX.to_byte_size(var1.typ.get_size(program));
                        let res = op2.LEIRnasm(vec![reg1], f, program,build, &local_vars, &bufs,stack_size, loc)?;
                        if res[0].to_byte_size(8) != Register::RAX {
                            writeln!(f, "   mov {}, {}",Register::RAX.to_byte_size(res[0].size()),res[0])?
                        }
                        if stack_size-var1.operand == 0 {
                            writeln!(f, "   cqo")?;
                            writeln!(f, "   imul {} [{}]",size_to_nasm_type(var1.typ.get_size(program)),program.stack_ptr())?;
                            writeln!(f, "   mov {} [{}], {}",size_to_nasm_type(var1.typ.get_size(program)),program.stack_ptr(),Register::RAX.to_byte_size(var1.typ.get_size(program)))?;
                        }
                        else {
                            writeln!(f, "   cqo")?;
                            writeln!(f, "   imul {} [{}+{}]",size_to_nasm_type(var1.typ.get_size(program)),program.stack_ptr(),stack_size-var1.operand)?;
                            writeln!(f, "   mov {} [{}+{}], {}",size_to_nasm_type(var1.typ.get_size(program)),program.stack_ptr(),stack_size-var1.operand,Register::RAX.to_byte_size(var1.typ.get_size(program)))?;
                        }
                        
                    },
                    _ => todo!()
                }

            }
            Instruction::DIVSET(_, _) => {
                todo!("Divset is not yet implemented!");
            }
            Instruction::CALL(Word,args) => {
                let sp_taken = nasm_x86_64_prep_args(program, build, f, args, stack_size, &local_vars)?;
                let rax = Register::RAX.to_byte_size((program.architecture.bits/8) as usize);
                writeln!(f, "   xor {}, {}",rax,rax)?;
                if let Some(external) = build.externals.get(Word) {
                    writeln!(f, "   call {}{}{}",external.typ.prefix(program),Word,external.typ.suffix())?;
                }
                else {
                    writeln!(f, "   call {}",Word)?;
                }
                if sp_taken-stack_size > 0 {
                    writeln!(f, "   add {}, {}",program.stack_ptr(),sp_taken-stack_size)?;
                }
            }
            Instruction::FNBEGIN() => {


            }
            Instruction::RET(expr) => {
                let dif = if scope.is_normal() {stack_size-func_stack_begin} else {stack_size-stack_size_org};
                let _regs = expr.LEIRnasm(vec![Register::RAX,Register::RBX,Register::RCX], f, program, build, &local_vars, &bufs,stack_size, loc)?;
                let oreg = Register::RAX.to_byte_size(_regs[0].size());
                if _regs[0] != oreg {
                    writeln!(f, "   mov {}, {}",oreg.to_string(),_regs[0].to_string())?;
                }
                if dif > 0 {
                    writeln!(f, "   add {}, {}",program.stack_ptr(),dif)?;
                }
                
                writeln!(f, "   ret")?;
            }
            Instruction::DEFVAR(_) => {},
            Instruction::INTERRUPT(val) => {
                writeln!(f, "   int 0x{:x}",val)?;
            },
            Instruction::EXPAND_SCOPE(s) => {
                local_vars.push(HashMap::new());
                stack_sizes.push(stack_size);
                writeln!(f, "   add {}, {}",program.stack_ptr(),shadow_space)?;
                stack_size-=shadow_space;
                nasm_x86_64_handle_scope(f, build, program, TCScopeType::NORMAL(&s),local_vars,stack_sizes,stack_size,func_stack_begin,inst_count)?;
                
                stack_size+=shadow_space;
                writeln!(f, "   sub {}, {}",program.stack_ptr(),shadow_space)?;
                
            }
            Instruction::EXPAND_IF_SCOPE(s)   => {
                let binst = inst_count.clone();
                let condition = match s.typ {
                    NormalScopeType::IF(ref c) => c,
                    _ => panic!("Unreachable")
                };
                if condition.is_ofp() {
                    let val = condition.unwrap_val();
                    match val {
                        OfP::REGISTER(reg) => {
                            let reg = reg.to_byte_size(1);
                            writeln!(f, "   cmp {}, 0",reg.to_string())?;
                        },
                        OfP::LOCALVAR(v) => {
                            let var = get_local_build(&local_vars,v).unwrap();
                            com_assert!(loc,var.typ.weak_eq(&VarType::BOOLEAN),"Error: Expected boolean but found {}",var.typ.to_string(false));
                            if stack_size-var.operand == 0 {
                                writeln!(f, "   cmp {} [{}], 0",size_to_nasm_type(var.typ.get_size(program)),program.stack_ptr())?;
                            }
                            else {
                                writeln!(f, "   cmp {} [{}+{}], 0",size_to_nasm_type(var.typ.get_size(program)),program.stack_ptr(),stack_size-var.operand)?;
                            }
                        },
                        OfP::CONST(val) => {
                            writeln!(f, "   mov rax, {}",val.get_num_data())?;
                            writeln!(f, "   cmp rax, 0")?;
                        },
                        OfP::RESULT(func,args) => {
                            let sp_taken = nasm_x86_64_prep_args(program, build, f, args, stack_size, &local_vars)?;
                            writeln!(f, "   call {}{}",program.architecture.func_prefix,func)?;
                            if sp_taken-stack_size > 0 {
                                writeln!(f, "   add {}, {}",program.stack_ptr(),sp_taken-stack_size)?
                            }
                            writeln!(f, "   cmp {}, 0",Register::AL.to_string())?;
                        },
                        _ => todo!()
                    }
                    writeln!(f, "   jnz .IF_SCOPE_{}",binst)?;
                }
                else {
                    let val = condition.unwrap_expr();
                    com_assert!(loc,val.op.is_boolean(),"Error: Condition MUST be of type boolean but encountered op {}",val.op.to_string());
                    match val.op {
                        Op::EQ   => {
                            let oreg1 = val.left.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            let tmp = Register::RSI.to_byte_size(oreg1[0].size());
                            writeln!(f, "   mov {}, {}",tmp.to_string(),oreg1[0].to_string())?;
                            let oreg2 = val.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;

                            writeln!(f, "   cmp {}, {}",tmp.to_string(),oreg2[0].to_byte_size(tmp.size()).to_string())?;
                            writeln!(f, "   jz .IF_SCOPE_{}",binst)?;
                        },
                        Op::NEQ  => {
                            let oreg1 = val.left.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            let tmp = Register::RSI.to_byte_size(oreg1[0].size());
                            writeln!(f, "   mov {}, {}",tmp.to_string(),oreg1[0].to_string())?;
                            let oreg2 = val.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            writeln!(f, "   cmp {}, {}",tmp.to_string(),oreg2[0].to_byte_size(tmp.size()).to_string())?;
                            writeln!(f, "   jnz .IF_SCOPE_{}",binst)?;
                        },
                        Op::GT   => {
                            let oreg1 = val.left.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            let tmp = Register::RSI.to_byte_size(oreg1[0].size());
                            writeln!(f, "   mov {}, {}",tmp.to_string(),oreg1[0].to_string())?;
                            let oreg2 = val.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            writeln!(f, "   cmp {}, {}",tmp.to_string(),oreg2[0].to_byte_size(tmp.size()).to_string())?;
                            writeln!(f, "   jg .IF_SCOPE_{}",binst)?;
                        },
                        Op::GTEQ => {
                            let oreg1 = val.left.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            let tmp = Register::RSI.to_byte_size(oreg1[0].size());
                            writeln!(f, "   mov {}, {}",tmp.to_string(),oreg1[0].to_string())?;
                            let oreg2 = val.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            writeln!(f, "   cmp {}, {}",tmp.to_string(),oreg2[0].to_byte_size(tmp.size()).to_string())?;
                            writeln!(f, "   jge .IF_SCOPE_{}",binst)?;
                        },
                        Op::LT   => {
                            let oreg1 = val.left.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            let tmp = Register::RSI.to_byte_size(oreg1[0].size());
                            writeln!(f, "   mov {}, {}",tmp.to_string(),oreg1[0].to_string())?;
                            let oreg2 = val.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            writeln!(f, "   cmp {}, {}",tmp.to_string(),oreg2[0].to_byte_size(tmp.size()).to_string())?;
                            writeln!(f, "   jl .IF_SCOPE_{}",binst)?;
                        },
                        Op::LTEQ => {
                            let oreg1 = val.left.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            let tmp = Register::RSI.to_byte_size(oreg1[0].size());
                            writeln!(f, "   mov {}, {}",tmp.to_string(),oreg1[0].to_string())?;
                            let oreg2 = val.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            writeln!(f, "   cmp {}, {}",tmp.to_string(),oreg2[0].to_byte_size(tmp.size()).to_string())?;
                            writeln!(f, "   jle .IF_SCOPE_{}",binst)?;
                        },
                        Op::NOT  => todo!(),
                        _ => panic!("Unreachable")
                    }
                }
                if let Some((_,elses)) = scope.get_body(build).get(i+1) {
                    match elses {
                        Instruction::EXPAND_ELSE_SCOPE(elses) => {
                            local_vars.push(HashMap::new());
                            writeln!(f, "   add {}, {}",program.stack_ptr(),shadow_space)?;
                            stack_size-=shadow_space;
                            stack_sizes.push(stack_size);
                            nasm_x86_64_handle_scope(f, build, program, TCScopeType::NORMAL(&elses), local_vars, stack_sizes,stack_size,func_stack_begin,inst_count)?;
                            
                            stack_size+=shadow_space;
                            writeln!(f, "   sub {}, {}",program.stack_ptr(),shadow_space)?;
                        }
                        _ => {}
                    }
                }


                writeln!(f, "   jmp .IF_SCOPE_END_{}",binst)?;
                writeln!(f, "   .IF_SCOPE_{}:",binst)?;
                *inst_count += 1;
                local_vars.push(HashMap::new());
                writeln!(f, "   add {}, {}",program.stack_ptr(),shadow_space)?;
                stack_size-=shadow_space;
                stack_sizes.push(stack_size);
                nasm_x86_64_handle_scope(f, build, program, TCScopeType::NORMAL(&s), local_vars, stack_sizes,stack_size,func_stack_begin,inst_count)?;
                
                stack_size+=shadow_space;
                writeln!(f, "   sub {}, {}",program.stack_ptr(),shadow_space)?;
                *inst_count -= 1;

                writeln!(f, "   .IF_SCOPE_END_{}:",binst)?;
                
            },
            Instruction::EXPAND_WHILE_SCOPE(s) => {
                writeln!(f, "   .WHILE_SCOPE_{}:",inst_count)?;
                let condition = match s.typ {
                    NormalScopeType::WHILE(ref c) => c,
                    _ => panic!("Unreachable")
                };
                if condition.is_ofp() {
                    let val = condition.unwrap_val();

                    match val {
                        OfP::REGISTER(reg) => {
                            let reg = reg.to_byte_size(1);
                            writeln!(f, "   cmp {}, 0",reg.to_string())?;
                        },
                        OfP::LOCALVAR(v) => {
                            let var = get_local_build(&local_vars, v).unwrap();
                            com_assert!(loc,var.typ.weak_eq(&VarType::BOOLEAN),"Error: Expected boolean but found {}",var.typ.to_string(false));
                            if stack_size-var.operand == 0 {
                                writeln!(f, "   cmp {} [{}], 0",size_to_nasm_type(var.typ.get_size(program)),program.stack_ptr())?;
                            }
                            else {
                                writeln!(f, "   cmp {} [{}+{}], 0",size_to_nasm_type(var.typ.get_size(program)),program.stack_ptr(),stack_size-var.operand)?;
                            }
                        },
                        OfP::CONST(val) => {
                            writeln!(f, "   mov rax, {}",val.get_num_data())?;
                            writeln!(f, "   cmp rax, 0")?;
                        },
                        OfP::RESULT(func, args) => {
                            let sp_taken = nasm_x86_64_prep_args(program, build, f, args, stack_size, &local_vars)?;
                            writeln!(f, "   call {}{}",program.architecture.func_prefix,func)?;
                            if sp_taken-stack_size > 0 {
                                writeln!(f, "   add {}, {}",program.stack_ptr(),sp_taken-stack_size)?
                            }
                            writeln!(f, "   cmp {}, 0",Register::AL.to_string())?;
                        },
                        _ => todo!()
                    }
                    writeln!(f, "   jz .WHILE_SCOPE_END_{}",inst_count)?;
                }
                else {
                    let val = condition.unwrap_expr();
                    com_assert!(loc,val.op.is_boolean(),"Error: Condition MUST be of type boolean but encountered op {}",val.op.to_string());
                    match val.op {
                        Op::EQ   => {
                            let oreg1 = val.left.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            let tmp = Register::RSI.to_byte_size(oreg1[0].size());
                            writeln!(f, "   mov {}, {}",tmp.to_string(),oreg1[0].to_string())?;
                            let oreg2 = val.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            writeln!(f, "   cmp {}, {}",tmp.to_string(),oreg2[0].to_byte_size(tmp.size()).to_string())?;
                            writeln!(f, "   jnz .WHILE_SCOPE_END_{}",inst_count)?;
                        },
                        Op::NEQ  => {
                            let oreg1 = val.left.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            let tmp = Register::RSI.to_byte_size(oreg1[0].size());
                            
                            writeln!(f, "   mov {}, {}",tmp.to_string(),oreg1[0].to_string())?;

                            let oreg2 = val.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            writeln!(f, "   cmp {}, {}",tmp.to_string(),oreg2[0].to_byte_size(tmp.size()).to_string())?;
                            writeln!(f, "   jz .WHILE_SCOPE_END_{}",inst_count)?;
                        },
                        Op::GT   => {
                            let oreg1 = val.left.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            let tmp = Register::RSI.to_byte_size(oreg1[0].size());
                            writeln!(f, "   mov {}, {}",tmp.to_string(),oreg1[0].to_string())?;
                            let oreg2 = val.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            writeln!(f, "   cmp {}, {}",tmp.to_string(),oreg2[0].to_byte_size(tmp.size()).to_string())?;
                            writeln!(f, "   jle .WHILE_SCOPE_END_{}",inst_count)?;
                        },
                        Op::GTEQ => {
                            let oreg1 = val.left.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            let tmp = Register::RSI.to_byte_size(oreg1[0].size());
                            writeln!(f, "   mov {}, {}",tmp.to_string(),oreg1[0].to_string())?;
                            let oreg2 = val.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            writeln!(f, "   cmp {}, {}",tmp.to_string(),oreg2[0].to_byte_size(tmp.size()).to_string())?;
                            writeln!(f, "   jl .WHILE_SCOPE_END_{}",inst_count)?;
                        },
                        Op::LT   => {
                            let oreg1 = val.left.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            let tmp = Register::RSI.to_byte_size(oreg1[0].size());
                            writeln!(f, "   mov {}, {}",tmp.to_string(),oreg1[0].to_string())?;
                            let oreg2 = val.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            writeln!(f, "   cmp {}, {}",tmp.to_string(),oreg2[0].to_byte_size(tmp.size()).to_string())?;
                            writeln!(f, "   jge .WHILE_SCOPE_END_{}",inst_count)?;
                        },
                        Op::LTEQ => {
                            let oreg1 = val.left.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            let tmp = Register::RSI.to_byte_size(oreg1[0].size());
                            writeln!(f, "   mov {}, {}",tmp.to_string(),oreg1[0].to_string())?;
                            let oreg2 = val.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX, Register::RBX, Register::RCX], f, program, build, &local_vars, &bufs, stack_size, loc)?;
                            writeln!(f, "   cmp {}, {}",tmp.to_string(),oreg2[0].to_byte_size(tmp.size()).to_string())?;
                            writeln!(f, "   jg .WHILE_SCOPE_END_{}",inst_count)?;
                        },
                        Op::NOT  => todo!(),
                        _ => panic!("Unreachable")
                    }
                }

                
                let binst = inst_count.clone();
                local_vars.push(HashMap::new());
                writeln!(f, "   add {}, {}",program.stack_ptr(),shadow_space)?;
                stack_size-=shadow_space;
                stack_sizes.push(stack_size);
                nasm_x86_64_handle_scope(f, build, program, TCScopeType::NORMAL(&s), local_vars, stack_sizes,stack_size,func_stack_begin,inst_count)?;
                
                stack_size+=shadow_space;
                writeln!(f, "   sub {}, {}",program.stack_ptr(),shadow_space)?;
                writeln!(f, "   jmp .WHILE_SCOPE_{}" ,binst)?;
                writeln!(f, "   .WHILE_SCOPE_END_{}:",binst)?;

            }
            Instruction::EXPAND_ELSE_SCOPE(_) => {
                
            },
            Instruction::SYSCALL => {
                writeln!(f, "   syscall")?;
            }
            Instruction::MAKELABEL(lname) => {
                write!(f, "   .LABEL_")?;
                for chr in lname.bytes() {
                    write!(f, "{}_",chr)?;
                }
                writeln!(f, ":\n")?;
            }
            Instruction::GOTO(lname, s) => {
                let mut total_drop: usize = 0;
                for s in &stack_sizes[s.to_owned()..] {
                    //println!("Passing by vars: {:?}",vars);
                    total_drop += s;
                }
                if total_drop > 0 {
                    writeln!(f, "   add {}, {}",program.stack_ptr(),total_drop)?;
                }
                
                write!(f, "   jmp .LABEL_")?;
                for chr in lname.bytes() {
                    write!(f, "{}_",chr)?;
                }
                writeln!(f)?;
            }
            _ => todo!("Re-enable these:")
            
        }
        *inst_count += 1;
    }
    if !scope.is_func() {
        let dif = stack_size-stack_size_org;
        if dif != 0 {
            writeln!(f, "   add {}, {}",program.stack_ptr(),dif)?;
        }
    }
    else if scope.unwrap_func() != "main" {
        let dif = stack_size-stack_size_org;
        if dif != 0 {
            writeln!(f, "   add {}, {}",program.stack_ptr(),dif)?;
        }
        writeln!(f, "   ret")?;
    }
    local_vars.pop();
    stack_sizes.pop();
    Ok(())
}
fn to_nasm_x86_64(build: &mut BuildProgram, program: &CmdProgram) -> io::Result<()>{
    let optimization = optimization_ops(build, program);
    let mut f = File::create(&program.opath).expect(&format!("Error: could not open output file {}",program.opath.as_str()));
    if program.architecture.bits == 64 {
        writeln!(&mut f,"BITS 64")?;
        writeln!(&mut f,"default rel")?;
    }
    writeln!(&mut f, "section .bss")?;
    for (i,buf) in build.buffers.iter().enumerate() {
        writeln!(&mut f, "   _GLOBL_{}: resb {}",i,buf.size*buf.typ.get_size(program))?;
    }
    
    writeln!(&mut f, "section .data")?;


    for (id,stridef) in build.stringdefs.iter().enumerate(){
        if program.in_mode == OptimizationMode::DEBUG || (optimization.usedStrings.contains_key(&id) && (optimization.usedFuncs.contains(optimization.usedStrings.get(&id).unwrap()) || !program.remove_unused_functions || optimization.usedStrings.get(&id).expect(&format!("Could not find: {}",&id)) == "main")) {
            write!(&mut f, "   $STRING_{}_: db ",id)?;
            for chr in stridef.Data.chars() {
                write!(&mut f, "{}, ",(chr as u8))?;
            }
            if program.in_mode == OptimizationMode::DEBUG {
                writeln!(&mut f, "0    ; {}",stridef.Data.escape_default())?;
            }
            else {
                writeln!(&mut f, "0")?;
            }
            match stridef.Typ {
                ProgramStringType::STR  => {},
                ProgramStringType::CSTR => {},
            }
        }
        else if program.print_unused_warns || program.print_unused_strings {
            println!("[NOTE] Unused string:   <{}> \"{}\" - This is probably due to a constant definition that was never used or a function that may have used that strings, that got cut off from the final build",id, stridef.Data.escape_default());
            for (cd_name, cd) in build.constdefs.iter() {
                match cd.typ {
                    RawConstValueType::STR(id2) => {
                        if id == id2 {
                            println!("       ^ Found matching constant definition: \"{}\" at {}",cd_name,cd.loc.loc_display());
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    for (name,var) in build.global_vars.iter() {
        write!(&mut f, "   _GLOBL_{}: ",name)?;
        match &var.typ {
            //GlobalVarType::BUFFER(i) => writeln!(&mut f, "_GLOBLBUF_{}",i)?,
            GlobalVarType::CONSTANT(val) => {
                match val {
                    RawConstValueType::CHAR(v) => writeln!(&mut f, "db {}",v)?,
                    RawConstValueType::SHORT(v) => writeln!(&mut f, "dw {}",v)?,
                    RawConstValueType::INT(v) => writeln!(&mut f, "dd {}",v)?,
                    RawConstValueType::LONG(v) => writeln!(&mut f, "dq {}",v)?,
                    RawConstValueType::STR(UUID) => writeln!(&mut f, "$STRING_{}",UUID.to_string().replace("-", ""))?,
                    RawConstValueType::PTR(_, t) => {
                        if program.architecture.bits == 32 {
                            writeln!(&mut f, "dd {}",t)?;
                        }
                        else if program.architecture.bits == 64 {
                            writeln!(&mut f, "dq {}",t)?;
                        }
                        else {
                            panic!("Unreachable")
                        }
                    },
                }
            },
            _ => {}
        }
    }
    for (dll_import_name,_) in build.dll_imports.iter() {
        writeln!(&mut f, "extern {}",dll_import_name)?;
        
    }
    for (dll_export_name,_dll_export) in build.dll_exports.iter() {
        writeln!(&mut f, "global {}",dll_export_name)?;
    }
    for function_name in build.functions.keys() {
        if function_name == "main" {
            writeln!(&mut f,"global {}{}",program.architecture.func_prefix,function_name)?;
        }
        else {
            writeln!(&mut f,"global {}{}",program.architecture.func_prefix,function_name)?;
            if program.in_mode != OptimizationMode::DEBUG && program.remove_unused_functions && !optimization.usedFuncs.contains(function_name) && program.print_unused_warns && program.print_unused_funcs {
                println!("[NOTE] {}: Unused function: \"{}\"", build.functions.get(function_name).unwrap().location.loc_display(),function_name);
            }
        }
    }
    for (Word,exter) in build.externals.iter() {
        match exter.typ {
            ExternalType::CExternal| ExternalType::RawExternal => {
    
                if program.in_mode == OptimizationMode::DEBUG || optimization.usedExterns.contains(Word) {

                    writeln!(&mut f,"  extern {}{}{}",exter.typ.prefix(&program),Word,exter.typ.suffix())?;
                }
                else if program.print_unused_warns || program.print_unused_externs {
                    println!("[NOTE] {}: Unused external: \"{}\"",exter.loc.loc_display(),Word);
                }
            },
        }
    }
    
    writeln!(&mut f, "section .text")?;

    for (function_name,function) in build.functions.iter() {
        if program.in_mode != OptimizationMode::DEBUG && program.remove_unused_functions && !optimization.usedFuncs.contains(function_name) && function_name != "main" {
            continue;
        }

        if function_name == "main" {
            writeln!(&mut f, "{}{}:",program.architecture.func_prefix,function_name)?;
        }
        else {
            writeln!(&mut f, "{}{}:",program.architecture.func_prefix,function_name)?;
        }
        let mut inst_count = 0;
        let mut stack_sizes: Vec<usize> = Vec::new();
        nasm_x86_64_handle_scope(&mut f, build, program, TCScopeType::FUNCTION(function_name.clone()),&mut vec![HashMap::new()],&mut stack_sizes,0,0,&mut inst_count)?;
        if function_name == "main" {

            let shadow_space: usize = if let ArcPassType::CUSTOM(argpassing) = &program.architecture.options.argumentPassing {
                argpassing.shadow_space
            } else { 0 };
            let mut argsize: usize = 0;
            for (_,local) in function.locals.iter() {
                if argsize%8+local.get_size(program) > 8 {
                    argsize += 8-argsize%8;
                }
                argsize += local.get_size(program);
            }
            for buf in function.buffers.iter() {
                argsize += buf.size*buf.typ.get_size(program);
            }
            if argsize%8 > 0 {
                argsize += 8-argsize%8;
            }
            argsize+=shadow_space;
            if argsize > 0 {
                writeln!(&mut f, "   add {}, {}",program.stack_ptr(),argsize)?;
            }
            let rax = Register::RAX.to_byte_size((program.architecture.bits/8) as usize);
            writeln!(f, "   xor {}, {}",rax,rax)?;
            writeln!(&mut f, "   ret")?;
        }
    }
    
    Ok(())

}

#[derive(Debug)]
enum TCScopeType<'a> {
    FUNCTION(String),
    NORMAL(&'a NormalScope)
}
impl TCScopeType<'_> {
    fn is_func(&self) -> bool {
        match self {
            Self::FUNCTION(_) => true,
            _ => false
        }
    }
    fn is_normal(&self) -> bool {
        match self {
            Self::NORMAL(_) => true,
            _ => false
        }
    }
    fn has_contract(&self) -> bool {
        match self {
            TCScopeType::FUNCTION(_) => true,
            TCScopeType::NORMAL(_) => false,
        }
    }
    fn get_body<'a>(&'a self, build: &'a BuildProgram) -> &ScopeBody {
        match self {
            Self::FUNCTION(name) => {
                &build.functions.get(name).as_ref().unwrap().body
            }
            Self::NORMAL(s) => {
                &s.body
            }
        }
    }
    fn get_locals<'a>(&'a self, build: &'a BuildProgram) -> Option<&Locals> {
        match self {
            Self::FUNCTION(name) => {
                Some(&build.functions.get(name).unwrap().locals)
            }
            Self::NORMAL(s) => {
                Some(&s.locals)
            }
        }
    }
    fn get_locals_mut<'a>(&'a self, build: &'a mut BuildProgram) -> Option<&mut Locals> {
        match self {
            Self::FUNCTION(name) => {
                Some(&mut build.functions.get_mut(name).unwrap().locals)
            }
            Self::NORMAL(_) => {
                None
            }
        }
    }
    fn get_contract<'a>(&'a self, build: &'a BuildProgram) -> Option<&FunctionContract> {
        match self {
            Self::FUNCTION(name) => {
                Some(&build.functions.get(name).unwrap().contract)
            }
            Self::NORMAL(_) => {
                None
            }
        }
    }
    fn get_location<'a>(&'a self, build: &'a BuildProgram) -> Option<&ProgramLocation> {
        match self {
            Self::FUNCTION(name) => {
                Some(&build.functions.get(name).unwrap().location)
            }
            Self::NORMAL(_) => {
                None
            }
        }
    }
    fn unwrap_func(&self) -> String {
        match self {
            Self::FUNCTION(name) => {
                name.clone()
            }
            Self::NORMAL(_) => {
                panic!("Cannot call unwrap_func on non function scope!");
            }
        }
    }
    fn get_buffers<'a>(&'a self, build: &'a BuildProgram) -> &Vec<BuildBuf> {
        match self {
            TCScopeType::FUNCTION(name) => &build.functions.get(name).unwrap().buffers,
            TCScopeType::NORMAL(s) => &s.buffers,
        }
    }
}
fn type_check_scope(build: &BuildProgram, program: &CmdProgram, scope: TCScopeType, currentLocals: &mut Vec<Locals>) -> bool {
    if let Some(locals) = scope.get_locals(build) {
        currentLocals.push(locals.clone());
    }
    let mut hasFoundRet = false;    

    for (loc, instruction) in scope.get_body(build).iter() {
        match instruction {
            Instruction::DEFVAR(_)           => {},
            
            Instruction::CALLRAW(name,contract)        => {
                let mut externContract = build.get_contract_of_symbol(name).unwrap_or(AnyContract { InputPool: ContractInputPool::new(), Outputs: vec![] }).clone();//build.externals.get(name).unwrap().contract.as_ref().unwrap_or(&AnyContract { InputPool: vec![], Outputs: vec![] }).clone();
                externContract.InputPool.reverse();
                typ_assert!(loc, externContract.InputPool.len() == contract.len() || externContract.InputPool.is_dynamic, "Error: Expected: {} amount of arguments but found {}",externContract.InputPool.len(), contract.len());
                for arg in contract {
                    match arg {
                        OfP::LOCALVAR(name) => {
                            let etyp = if externContract.InputPool.len() > 0 {
                                Some(typ_expect!(loc, externContract.InputPool.pop(), "Error: Additional arguments provided for external that doesn't take in any more arguments!"))
                            } else if externContract.InputPool.is_dynamic {
                                externContract.InputPool.dynamic_type.clone()
                            }
                            else {
                                typ_error!(loc, "Error: Unreachable");
                            };
                            let local = get_local(currentLocals, name).unwrap();
                            if let Some(etyp) = etyp {
                                typ_assert!(loc,etyp.weak_eq(&local),"Error: Incompatible types for contract\nExpected: {}\nFound: ({}) {}",etyp.to_string(false),name,local.to_string(false));
                            }
                        },
                        OfP::GLOBALVAR(name) => {
                            let etyp = if externContract.InputPool.len() > 0 {
                                Some(typ_expect!(loc, externContract.InputPool.pop(), "Error: Additional arguments provided for external that doesn't take in any more arguments!"))
                            } else if externContract.InputPool.is_dynamic {
                                externContract.InputPool.dynamic_type.clone()
                            }
                            else {
                                typ_error!(loc, "Error: Unreachable");
                            };
                            let var = match &build.global_vars.get(name).unwrap().typ {
                                GlobalVarType::BUFFER(b) => {
                                    VarType::PTR(Ptr::ref_to(build.buffers[*b].typ.clone()))
                                }
                                _ => todo!()
                            };
                            if let Some(etyp) = etyp {
                                typ_assert!(loc,etyp.weak_eq(&var),"Error: Incompatible types for contract\nExpected: {}\nFound: ({}) {}",etyp.to_string(false),name,var.to_string(false));
                            }
                        }
                        OfP::REGISTER(_) => todo!("Registers are still yet unhandled!"),
                        OfP::CONST(Const) => {
                            let typs = Const.to_type(build);
                            for typ in typs {
                                let etyp = if externContract.InputPool.len() > 0 {
                                    Some(typ_expect!(loc, externContract.InputPool.pop(), "Error: Additional arguments provided for external that doesn't take in any more arguments!"))
                                } else if externContract.InputPool.is_dynamic {
                                    externContract.InputPool.dynamic_type.clone()
                                }
                                else {
                                    typ_error!(loc, "Error: Unreachable");
                                };
                                if let Some(etyp) = etyp {
                                    typ_assert!(loc,etyp.weak_eq(&typ),"Error: Incompatible types for contract\nExpected: {}\nFound: {}",etyp.to_string(false),typ.to_string(false));
                                }
                            }
                        },
                        OfP::RESULT(_, _) => todo!(),
                        _ => todo!()
                    }
                }
            },
            Instruction::MOV(o, e)           => {
                let ot = o.result_of(program,build, &currentLocals,scope.get_buffers(build),loc);
                let et = e.result_of(program, build, &currentLocals,scope.get_buffers(build),loc);
                typ_assert!(loc,ot.is_some() && et.is_some(), "Error: Expected result but found nothing!");
                typ_assert!(loc,ot.as_ref().unwrap().weak_eq(&et.as_ref().unwrap()) || ot.as_ref().unwrap().is_ptr() && ot.as_ref().unwrap().is_numeric(), "Error: Types did not match Expected: {} but found {}",ot.unwrap().to_string(false),et.unwrap().to_string(false));
            },
            Instruction::ADDSET(o, e)           => {
                let ot = o.result_of(program,build, &currentLocals,scope.get_buffers(build),loc);
                let et = e.result_of(program, build, &currentLocals,scope.get_buffers(build),loc);
                typ_assert!(loc,ot.is_some() && et.is_some(), "Error: Expected result but found nothing!");
                typ_assert!(loc,ot.as_ref().unwrap().weak_eq(&et.as_ref().unwrap()) || ot.as_ref().unwrap().is_ptr() && ot.as_ref().unwrap().is_numeric(), "Error: Types did not match Expected: {} but found {}",ot.unwrap().to_string(false),et.unwrap().to_string(false));
            },
            Instruction::SUBSET(o, e)           => {
                let ot = o.result_of(program,build, currentLocals,scope.get_buffers(build),loc);
                let et = e.result_of(program, build, currentLocals,scope.get_buffers(build),loc);
                typ_assert!(loc,ot.is_some() && et.is_some(), "Error: Expected result but found nothing!");
                typ_assert!(loc,ot.as_ref().unwrap().weak_eq(&et.as_ref().unwrap()), "Error: Types did not match Expected: {} but found {}",ot.unwrap().to_string(false),et.unwrap().to_string(false));
            },
            Instruction::MULSET(o, e)           => {
                let ot = o.result_of(program,build, currentLocals,scope.get_buffers(build),loc);
                let et = e.result_of(program, build, currentLocals,scope.get_buffers(build),loc);
                typ_assert!(loc,ot.is_some() && et.is_some(), "Error: Expected result but found nothing!");
                typ_assert!(loc,ot.as_ref().unwrap().weak_eq(&et.as_ref().unwrap()), "Error: Types did not match Expected: {} but found {}",ot.unwrap().to_string(false),et.unwrap().to_string(false));
            },
            Instruction::DIVSET(o, e)           => {
                let ot = o.result_of(program,build, currentLocals,scope.get_buffers(build),loc);
                let et = e.result_of(program, build, currentLocals,scope.get_buffers(build),loc);
                typ_assert!(loc,ot.is_some() && et.is_some(), "Error: Expected result but found nothing!");
                typ_assert!(loc,ot.as_ref().unwrap().weak_eq(&et.as_ref().unwrap()), "Error: Types did not match Expected: {} but found {}",ot.unwrap().to_string(false),et.unwrap().to_string(false));
            },

            Instruction::CALL(funcn, args)             => {
                let function = typ_expect!(loc, build.functions.get(funcn), "Error: unknown function call to {}, Function may not exist!",funcn);
                let mut functionIP: Vec<(&String, &VarType)> = function.contract.Inputs.iter().collect();
                
                //println!("functionIP: {:?}\nargs: {:?}",functionIP,args);
                for arg in args.iter() {
                    match arg {
                        OfP::LOCALVAR(name) => {
                            let (_,etyp) = typ_expect!(loc, if functionIP.len() > 0 { Some(functionIP.remove(0)) } else { None }, "Error: Additional arguments provided for external that doesn't take in any more arguments!");
                            let local = get_local(currentLocals, &name).unwrap();
                            //println!("arg: {:?}\netyp: {:?}",local,etyp);
                            typ_assert!(loc,etyp.weak_eq(&local),"Error: Incompatible types for contract\nExpected: {}\nFound: ({}) {}",etyp.to_string(false),name,local.to_string(false));
                        },
                        OfP::GLOBALVAR(name) => {
                            let (_,etyp) = typ_expect!(loc, if functionIP.len() > 0 { Some(functionIP.remove(0)) } else { None }, "Error: Additional arguments provided for external that doesn't take in any more arguments!");
                            let var = match &build.global_vars.get(name).unwrap().typ {
                                GlobalVarType::BUFFER(b) => {
                                    VarType::PTR(Ptr::ref_to(build.buffers[*b].typ.clone()))
                                }
                                _ => todo!()
                            };
                            typ_assert!(loc,etyp.weak_eq(&var),"Error: Incompatible types for contract\nExpected: {}\nFound: ({}) {}",etyp.to_string(false),name,var.to_string(false));
                        }
                        OfP::REGISTER(_) => todo!("Registers are still yet unhandled!"),
                        OfP::CONST(Const) => {
                            let typs = Const.to_type(build);
                            for typ in typs {
                                let (_,etyp) = typ_expect!(loc, if functionIP.len() > 0 { Some(functionIP.remove(0)) } else { None }, "Error: Additional arguments provided for external that doesn't take in any more arguments!\nExpected: Nothing\nFound: {}\n",typ.to_string(false));
                                typ_assert!(loc,etyp.weak_eq(&typ),"Error: Incompatible types for contract\nExpected: {}\nFound: {}",etyp.to_string(false),typ.to_string(false));
                            }
                        },
                        _ => todo!()
                    }
                }
                if functionIP.len() > 0 {
                    let functionIP = function.contract.Inputs.clone();
                    typ_error!(loc, "Error: NON-matching function arguments (function defined here: {}). Expected: \n{}\nBut found: \n{}",function.loc_display(),
                    {  
                        let mut o: String = String::new();
                        for (v,t) in functionIP.iter() {
                            o += &format!("   {} ({})\n",t.to_string(false),v);
                        }
                        &o[..o.len()-1].to_owned()
                    },
                    {
                        let mut o: String = String::new();
                        for arg in args.iter() {
                            o += &format!("   {} ({})\n",arg.var_type_t(build, currentLocals,scope.get_buffers(build)).unwrap().to_string(false),arg.to_string(build))
                        }
                        &o[..o.len()-1].to_owned()
                    });
                }
            },
            Instruction::FNBEGIN()           => {},
            Instruction::RET(_) => {hasFoundRet=true}, // TODO: typecheck expression
            Instruction::INTERRUPT(_)        => {},
            Instruction::EXPAND_SCOPE(s)       => if type_check_scope(build, program, TCScopeType::NORMAL(&s), currentLocals) { hasFoundRet = true},
            Instruction::EXPAND_IF_SCOPE(s)    => {
                let res = s.typ.unwrap_expr().result_of(program, build, &currentLocals,scope.get_buffers(build),loc);
                typ_assert!(loc,res.is_some() && res.as_ref().unwrap().weak_eq(&VarType::BOOLEAN), "Error: Expected result of expression to be boolean but found: {}",if let Some(res) = res { res.to_string(false)} else { "None".to_owned()});
                if type_check_scope(build, program, TCScopeType::NORMAL(&s), currentLocals) { hasFoundRet = true}
            }
            Instruction::EXPAND_ELSE_SCOPE(s)  => if type_check_scope(build, program, TCScopeType::NORMAL(&s), currentLocals) { hasFoundRet = true},
            Instruction::EXPAND_WHILE_SCOPE(s) => {
                let res = s.typ.unwrap_expr().result_of(program, build, &currentLocals,scope.get_buffers(build),loc);
                typ_assert!(loc,res.is_some() && res.as_ref().unwrap().weak_eq(&VarType::BOOLEAN), "Error: Expected result of expression to be boolean but found: {}",if let Some(res) = res { res.to_string(false)} else { "None".to_owned()});
                if type_check_scope(build, program, TCScopeType::NORMAL(&s), currentLocals) { hasFoundRet = true}
            }

            Instruction::SYSCALL              => {}
            Instruction::MAKELABEL(_) => {},
            Instruction::GOTO(_,_) => {},
            _ => todo!("Unreachable")
        }
    }
    if scope.is_func() {
        if !scope.get_contract(build).unwrap().Outputs.is_empty() {
            typ_assert!(scope.get_location(build).unwrap(),hasFoundRet,"Error: Expected return value but function didn't return anything!");
        }
    }
    hasFoundRet
}
fn type_check_build(build: &mut BuildProgram, program: &CmdProgram) {

    for name in build.functions.keys() {
        let mut currentLocals:Vec<Locals>  = Vec::new();
        type_check_scope(build, program,TCScopeType::FUNCTION(name.clone()),&mut currentLocals);
    }
}



fn usage(program: &String) {
    println!("--------------------------------------------");
    println!("{} [flags]",program);
    println!("     Currently supported targets: ");
    list_targets(9);
    println!("     flags: ");
    println!("         -t (target)                                     -> compiles to the given target (default is nasm_x86_64)");
    println!("         -o (output path)                                -> outputs to that file (example: hello.asm in nasm_x86_64 mode). If the output path is not specified it defaults to the modes default (for nasm_x86_64 thats a.asm)");
    println!("         (NOT RECOMMENDED - use lighthouse instead) -r   -> runs the program for you if the option is available for that language mode (for example in nasm_x86_64 it calls nasm with gcc to link it to an executeable)");
    println!("         (NOT RECOMMENDED - use lighthouse instead) -b   -> builds the program for you if the option is available for that language mode");
    println!("         -i (path to directory to shortcut)              -> Adds a shortcut when including (if it finds the file it automatically expands the path) so you could do -i libs/libc and then just do include \"stdio.spl\" instead of the whole ordeal you had before");
    println!("         -release                                        -> builds the program in release mode");
    println!("         -ntc                                            -> (NoTypeChecking) Disable type checking");
    println!("         -warn (all, funcs, externs, strings)            -> Enable unused warns for parameter");
    println!("         -ruf                                            -> Remove unused functions");
    println!("         -arc (builtin arc)                              -> builds for a builtin architecture");
    println!("         -arc - (path to custom arc)                     -> builds for a custom architecture following the syntax described in ./examples/arcs");
    println!("         -usage                                          -> Show this page");
    println!("--------------------------------------------");
}
fn dump_tokens(lexer: &mut Lexer) {
    while let Some(token) = lexer.next() {
        println!("Token: {:?}",token);
    }
}
fn val_arr_to_reg_arr(list: &Vec<Value>) -> Option<Vec<Register>> {
    let mut out: Vec<Register> = Vec::with_capacity(list.len());
    for val in list {
        out.push(Register::from_string(&val.as_str()?.to_owned())?);
    }
    Some(out)
}
fn json_to_arc(json: &serde_json::Map<String, Value>) -> Architecture {
    let ops = json.get("options").expect("Expected options but found nothing!").as_object().expect("Ops must be an object!");
    let argpassing = ops.get("argumentPassing").expect("Expected argumentPassing but found nothing!");
    let oarc: Architecture = Architecture {
        bits: json.get("bits").expect("Error: expected value bits but found nothing!").as_u64().expect("Error: expected value of bits to be u64 but found string!") as u32,
        platform: json.get("os").expect("Error: expected value os but found nothing!").as_str().expect("os must be a string!").to_owned(),
        options: {
            if argpassing.is_string() && argpassing.as_str().unwrap() == "PUSHALL" {
                ArcOps { argumentPassing: ArcPassType::PUSHALL}
            }
            else if let Value::Object(argpassing) = argpassing {

                ArcOps { argumentPassing: ArcPassType::CUSTOM(ArcCustomOps {
                    nums_ptrs: Some(val_arr_to_reg_arr(argpassing.get("num_ptrs").expect("Error: expected num_ptrs but found nothing!").as_array().expect("Error: Value of num_ptrs must be array")).expect("Error: unknown syntax or register inside num_ptrs")),
                    floats: Some(val_arr_to_reg_arr(argpassing.get("floats").expect("Error: expected floats but found nothing!").as_array().expect("Error: Value of floats must be array")).expect("Error: unknown syntax or register inside floats")),
                    returns: Some(val_arr_to_reg_arr(argpassing.get("returns").expect("Error: expected returns but found nothing!").as_array().expect("Error: Value of returns must be array")).expect("Error: unknown syntax or register inside returns")),
                    on_overflow_stack: argpassing.get("on_overflow_stack").expect("Expected on_overflow_stack but found nothing!").as_bool().expect("Value of on_overflow_stack must be boolean!"),
                    shadow_space: argpassing.get("shadow_space").expect("Expected shadow_space but found nothing!").as_u64().expect("Value of shadow_space must be u64!") as usize }) }
            }
            else {
                todo!()
            }
        },
        func_prefix: json.get("func_prefix").expect("Error: expected func_prefix but found nothing!").as_str().expect("Value of func_prefix must be string").to_string(),
        cextern_prefix: json.get("cextern_prefix").expect("Error: expected cextern_prefix but found nothing!").as_str().expect("Value of cextern_prefix must be string").to_string(),
        obj_extension: json.get("obj-extension").expect("Error: expected obj_extension but found nothing!").as_str().expect("Value of obj_extension must be string").to_string(),
        flags: {
            let mut o: ArcFlags = ArcFlags { nasm: vec![] };
            let flagsObj = json.get("flags").expect("Error: expected flags but found nothing!").as_object().expect("Error: Value of flags must be an object!");
            o.nasm = {
                let mut o: Vec<String> = Vec::new();
                let nasm_flags = flagsObj.get("nasm").expect("Error: expected nasm flags but found nothing!").as_array().expect("Error: Value of nasm flags must be an array");
                for val in nasm_flags {
                    o.push(val.as_str().expect("Value of flag in nasm must be a string!").to_owned());
                }
                o
            };
            o
        },
    };
    oarc
}
fn list_targets(indent: usize){
    let indent = " ".repeat(indent);
    println!("{}- nasm_x86_64",indent);
}
fn main() {
    #[cfg(not(debug_assertions))]
    std::panic::set_hook(Box::new(|panic_info| {

        if let Some(e) = panic_info.payload().downcast_ref::<String>() {
            eprintln!("{}",e);
            exit(1);
        }
        else if let Some(e) = panic_info.payload().downcast_ref::<&str>() {
            eprintln!("{}",e);
            exit(1);
        }
        else {
            eprintln!("Error occured at {:?}",panic_info.location());
            exit(1);
        }
    }));
    let mut args: Vec<_> = env::args().collect();
    let program_n = args.remove(0);
    
    let mut program = CmdProgram::new();
    
    program.opath = Path::new(&program.path).with_extension(".asm").to_str().unwrap().to_string();
    let mut Architectures: HashMap<String, Architecture> = HashMap::new();
    use Register::*;
    Architectures.insert("windows_x86_64".to_owned(), Architecture { bits: 64, platform: "windows".to_string(), cextern_prefix: "".to_owned(),  func_prefix:"".to_owned(),  options: ArcOps { argumentPassing: ArcPassType::CUSTOM(ArcCustomOps { nums_ptrs: Some(vec![RCX, RDX,R8,R9]), floats: Some(vec![XMM0,XMM1,XMM2,XMM3]), returns: Some(vec![RAX]), on_overflow_stack: true, shadow_space: 32}) }        , obj_extension: "obj".to_owned(), flags: ArcFlags { nasm: vec!["-f".to_string(),"win64".to_string()] }});
    Architectures.insert("windows_x86".to_owned(),    Architecture { bits: 32, platform: "windows".to_string(), cextern_prefix: "_".to_owned(), func_prefix:"_".to_owned(), options: ArcOps { argumentPassing: ArcPassType::PUSHALL }                                                                                                                                                                            , obj_extension: "obj".to_owned(), flags: ArcFlags { nasm: vec!["-f".to_string(),"win32".to_string()] }});
    Architectures.insert("linux_x86_64".to_owned(),   Architecture { bits: 64, platform: "linux".to_string()  , cextern_prefix: "".to_owned(),  func_prefix:"".to_owned(),  options: ArcOps { argumentPassing: ArcPassType::CUSTOM(ArcCustomOps { nums_ptrs: Some(vec![RDI, RSI,RDX,RCX,R8,R9]), floats: Some(vec![XMM0,XMM1,XMM2,XMM3]), returns: Some(vec![RAX]), on_overflow_stack: true, shadow_space: 0 }) }, obj_extension: "o".to_owned()  , flags: ArcFlags { nasm: vec!["-f".to_string(),"elf64".to_string()] }});
    Architectures.insert("linux_x86".to_owned(),      Architecture { bits: 32, platform: "linux".to_string()  , cextern_prefix: "_".to_owned(), func_prefix:"_".to_owned(), options: ArcOps { argumentPassing: ArcPassType::PUSHALL }                                                                                                                                                                            , obj_extension: "o".to_owned()  , flags: ArcFlags { nasm: vec!["-f".to_string(),"elf64".to_string()] }});
    //short calls
    Architectures.insert("win_x86_64".to_owned(), Architectures.get("windows_x86_64").unwrap().clone());
    Architectures.insert("win_x86".to_owned(), Architectures.get("windows_x86").unwrap().clone());
    if let Some(arc) = Architectures.get(&("".to_owned()+env::consts::OS+"_"+env::consts::ARCH)) {
        program.architecture = arc.clone();
    }
    else {
        println!("[NOTE] No architecture found for {}_{}! Please specify the output architecture!",env::consts::OS,env::consts::ARCH);
    }
    let mut include_folders:HashSet<PathBuf>  = HashSet::new();
    cfor!(let mut i: usize = 0; i < args.len(); i+=1; {
        let flag = args.get(i).unwrap();
        match flag.as_str() {
            "-o" => {
                program.opath = args.get(i+1).unwrap_or_else(|| {
                    eprintln!("Error: Could not get output path for -o flag!");
                    exit(1);
                }).clone();
                i += 1;
            },
            "-b" => {
                program.should_build = true
            }
            "-r" => {
                program.should_build = true;
                program.should_run = true

            }
            "-release" => {
                program.in_mode = OptimizationMode::RELEASE
            }
            "-ntc" => {
                program.use_type_checking = false
            }
            "-t" => {
                program.target = args.get(i+1).expect("Error: Expected target but found nothing!").to_owned();
                if program.target == "list" {
                    list_targets(0);
                    exit(0);
                }
                i+=1;
            }
            "-warn" => {
                let typ = args.get(i+1).expect("Error: Expected `all, funcs,externs,strings`");
                match typ.as_str() {
                    "all" => {
                        program.print_unused_warns = true
                    }
                    "funcs" => {
                        program.print_unused_funcs = true
                    }
                    "externs" => {
                        program.print_unused_externs = true
                    }
                    "strings" => {
                        program.print_unused_strings = true
                    }
                    _ => {
                        eprintln!("Unexpected parameter: {}, Expected: `all,funcs,externs,strings`",typ)
                    }
                }
                i += 1;
            }
            "-ruf" => {
                program.remove_unused_functions = true;
            }
            "-usage" => {
                usage(&program_n);
                exit(0);
            }
            "-i" => {
                while i < args.len() && args[i].ends_with(",") {
                    include_folders.insert(PathBuf::from(&args[i][..args[i].len()-2]));
                    i += 1;
                }
                if args.len() == i {
                    panic!("Last element of -i should not end on ,");
                }
                i+=1;
                include_folders.insert(PathBuf::from(&args[i]));
            }
            "-arc" => {
                let val = args.get(i+1).expect("Error: Unexpected built-in target or path to json");
                if val == "-" {
                    let path = Path::new(args.get(i+2).expect("Error: Path not specified for -arc"));
                    let ext = path.extension().expect(&format!("Error: Path provided doesn't have an extension! Path: '{:?}'",path));
                    let ext = ext.to_str().unwrap();
                    assert!(ext=="json","Error: only accepting arc files with the json format!");
                    assert!(path.is_file() && path.exists(), "Error: path provided '{:?}' is not a file or does not exist!",path);
                    let f = fs::read_to_string(path).expect("Error: file does not exist or can't be opened!");
                    let arc: Value = serde_json::from_str(&f).expect("Contents of file not a json!");
                    let arc = arc.as_object().unwrap();
                    program.architecture = json_to_arc(arc);
                    i+=2;
                }
                else {
                    assert!(Architectures.contains_key(val),"Error: Unknown Architecture: {}. It is probably not built in!",val);
                    program.architecture = Architectures.get(val).unwrap().clone();
                    i+=1;
                }
            }
            flag => {
                if program.path.is_empty() && &flag[0..1] != "-" {
                    program.path = flag.to_string();
                }
                else {
                    eprintln!("Error: undefined flag: {flag}");
                    usage(&program_n);
                    exit(1);
                }
            }
        }
    });
    match program.target.as_str() {
        "nasm_x86_64" => {}
        _ => {
            eprintln!("Undefined target: {}\nSee supported targets by doing -t list",program.target);
            exit(1);
        }
    }
    let mut Intrinsics: HashMap<String,IntrinsicType> = HashMap::new();
    Intrinsics.insert("extern".to_string(), IntrinsicType::Extern);
    Intrinsics.insert("dll_import".to_string(), IntrinsicType::DLL_IMPORT);
    Intrinsics.insert("dll_export".to_string(), IntrinsicType::DLL_EXPORT);
    Intrinsics.insert("let".to_string(), IntrinsicType::Let);
    Intrinsics.insert("func".to_string()  , IntrinsicType::Func);
    Intrinsics.insert("include".to_string(), IntrinsicType::INCLUDE);
    Intrinsics.insert("const".to_string(), IntrinsicType::CONSTANT);
    Intrinsics.insert("interrupt".to_string(), IntrinsicType::INTERRUPT);
    Intrinsics.insert("(".to_string(),IntrinsicType::OPENPAREN);
    Intrinsics.insert(")".to_string(),IntrinsicType::CLOSEPAREN);
    Intrinsics.insert("{".to_string(),IntrinsicType::OPENCURLY);
    Intrinsics.insert("}".to_string(),IntrinsicType::CLOSECURLY);
    Intrinsics.insert("[".to_string(),IntrinsicType::OPENSQUARE);
    Intrinsics.insert("]".to_string(),IntrinsicType::CLOSESQUARE);
    Intrinsics.insert(":".to_string(),IntrinsicType::DOUBLE_COLIN);
    Intrinsics.insert(",".to_string(),IntrinsicType::COMA);
    Intrinsics.insert(";".to_string(),IntrinsicType::DOTCOMA);
    Intrinsics.insert("return".to_string(),IntrinsicType::RET);
    Intrinsics.insert("if".to_string(), IntrinsicType::IF);
    Intrinsics.insert("else".to_string(), IntrinsicType::ELSE);
    Intrinsics.insert("while".to_string(), IntrinsicType::WHILE);
    Intrinsics.insert("cast".to_string(), IntrinsicType::CAST);
    Intrinsics.insert("syscall".to_string(), IntrinsicType::SYSCALL);
    Intrinsics.insert("...".to_string(), IntrinsicType::THREEDOTS);
    Intrinsics.insert("@goto".to_string(), IntrinsicType::GOTO);
    Intrinsics.insert("@makelabel".to_string(), IntrinsicType::MAKELABEL);

    
    let mut Definitions: HashMap<String,VarType> = HashMap::new();
    Definitions.insert("int".to_string(), VarType::INT);
    Definitions.insert("char".to_string(), VarType::CHAR);
    Definitions.insert("long".to_string(), VarType::LONG);
    Definitions.insert("bool".to_string(), VarType::BOOLEAN);
    Definitions.insert("ptr".to_string(), VarType::PTR(Ptr{ typ: PtrTyp::VOID, inner_ref: 0}));
    Definitions.insert("short".to_string(), VarType::SHORT);
    Definitions.insert("size_t".to_string(), if program.architecture.bits == 64 { VarType::LONG } else { VarType::INT});
    
    if program.path.is_empty() {
        println!("Error: Unspecified input file!");
        usage(&program_n);
        exit(1);
    }
    let info = fs::read_to_string(&program.path);
    if let Err(info) = info {
        eprintln!("Error: Could not read file \"{}\"!",program.path);
        eprintln!("{}",info.to_string());
        exit(1);
    }
    let oinfo = info.unwrap();
    let mut lexer = Lexer::new(&oinfo, & Intrinsics, &Definitions, HashSet::new());
    lexer.currentLocation.file = Rc::new(program.path.clone());
    let mut build = parse_tokens_to_build(&mut lexer, &mut program, &include_folders,0);
    if program.use_type_checking {
        type_check_build(&mut build, &program);
    }
    match program.target.as_str() {
        "nasm_x86_64" => {
            to_nasm_x86_64(&mut build, &program).expect("Could not build to nasm_x86_64");
            if program.should_build {
                println!("-------------");
                let mut args = program.architecture.flags.nasm.clone();
                args.append(&mut vec![program.opath.as_str().to_owned()]);
                println!("   * nasm {}",args.join(" "));
                let nasm = Command::new("nasm").args(args).output().expect("Could not build nasm!");
                let v = Path::new(program.opath.as_str()).with_extension(&program.architecture.obj_extension);
                let v2 = Path::new(program.opath.as_str()).with_extension("");
                let args = [v.to_str().unwrap(),"-o",v2.to_str().unwrap(), if program.architecture.bits==64 { "-m64"} else {"-m32"}];
                println!("   * gcc -m64 {}",args.join(" "));
                let gcc  = Command::new("gcc").args(args).output().expect("Could not build gcc!");
                if !nasm.status.success() {
                    println!("--------------");
                    println!("Nasm: \n{:?}\n-----------",nasm);
                    println!("--------------");
                    exit(nasm.status.code().unwrap_or(0));
                }
                else if !gcc.status.success() {
                    println!("--------------");
                    println!("Gcc:  \n{:?}",gcc);
                    println!("--------------");
                    exit(nasm.status.code().unwrap_or(0));
                }
                else {
                    println!("--------------");
                    println!("   - Finished build successfully");
                    println!("--------------");
                }
            }
            if program.should_run {
                let exe_path = Path::new(program.opath.as_str()).with_extension("");
                println!("   * {}", exe_path.to_str().unwrap());
                println!("--------------");
                let mut prog = Command::new(exe_path.to_str().unwrap()).stdout(Stdio::inherit()).stdin(Stdio::inherit()).stderr(Stdio::inherit()).spawn().unwrap();
                let status = prog.wait().unwrap();
                println!("--------------");
                println!("Program exited with: {}",status.code().unwrap());
                println!("--------------");
            }
        }
        _ => {
            
            eprintln!("Target {} is either unsupported or a target is not provided!\n",program.target)
        }
    }

}





/*
- [x] TODO: No point in keeping params and localvars seperate once we set LOCALVAR onto the stack instead of the CALLSTACK
- [x] TODO: remove warn_rax_usage
- [x] TODO: Fix checking for UUID overloading in includes
- [x] TODO: implement macros for assert, expect etc. for type checking
- [x] TODO: implement pointers for the lexer (like *char *int **void etc.)
- [x] TODO: Add dynamic linking with dlls with dll_import dll_export
- [x] TODO: Implement the rest of the floating point arithmetic registers
- [x] TODO: Implement local variables for normal scopes
- [x] TODO: implement booleans
- [x] TODO: Implement if statements as well as else statements
- [x] TODO: Implement conditions and 'evaluate_condition'
- [x] TODO: Add expressions like a+b*c etc.
- [x] TODO: Remove -callstack
- [x] TODO: Fix returning from functions
- [x] TODO: Add 'result' as a part of OfP for calling the function and getting its result
- [x] TODO: Add more examples like OpenGL examples, native Windows examples with linking to kernel.dll etc.
- [x] TODO: Implement while loops
- [x] TODO: Push to master
- [x] TODO: Fix something like this (which currently compiles but nasm or any other assembler doesn't allow it since its invalid assembly):
func a() {
    @goto("b");
}
func b(){
    @makelabel("b");
}
- [x] TODO: Remove some dependencies like UUID since we don't exactly need it (also bench mark it to see the improvement in speed! - didn't do it :( )
- [x] TODO: Update README.md flags



- [ ] TODO: Implement || and && boolean logic
- [ ] TODO: Implement function overloading
- [ ] TODO: Make it so that get_body returns None if scope has not been opened yet
- [ ] TODO: Update all of readme and add more documentation
- [ ] TODO: Add more useful examples
- [ ] TODO: Add some quality of life things such as __FILE__ __LINE__
*/
