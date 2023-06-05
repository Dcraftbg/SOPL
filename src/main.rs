#![allow(non_snake_case)] 
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(unreachable_patterns)]

use core::{num, panic};
use std::{env, process::{exit, Command, Stdio}, path::{Path, PathBuf}, ffi::OsStr, str::{FromStr, Chars}, collections::{HashMap, HashSet}, hash::Hash, fs::{File, self}, io::{Read, Write, self}, fmt::format, borrow::{BorrowMut, Borrow}, clone, time::{SystemTime, Instant}, rc::Rc, iter::Peekable, cell::{RefCell, Ref, RefMut}, ops::{Deref, DerefMut}, vec, sync::Arc, os, f32::consts::E, any::Any};
use linked_hash_map::LinkedHashMap;
use serde_json::Value;
use uuid::Uuid; 




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
    call_stack_size: usize,
}
impl CmdProgram {
    fn new() -> Self {
        Self { path: String::new(), opath: String::new(), should_build: false, should_run: false, target: "nasm_x86_64".to_string(), call_stack_size: 64000, in_mode: OptimizationMode::DEBUG, use_type_checking: true, print_unused_warns: false,  remove_unused_functions: false, print_unused_funcs: false, print_unused_externs: false, print_unused_strings: false, architecture: Architecture::new() }
    }
}
#[repr(u32)]
#[derive(Clone, Copy,Debug,PartialEq )]

enum IntrinsicType {
    Extern = 0,
    DLL_IMPORT,
    DLL_EXPORT,
    Func,
    RS,
    Let,
    CONSTANT,
    OPENPAREN,
    CLOSEPAREN,
    DOUBLE_COLIN,
    COMA,
    DOTCOMA,
    OPENCURLY,
    CLOSECURLY,
    OPENANGLE,
    CLOSEANGLE,
    INTERRUPT,
    // REGISTER OPERATIONS
    POP,
    PUSH,
    SET,

    // REGISTER MATH
    ADD,
    SUB,
    MUL,
    DIV,
    RET,
    INCLUDE,
    IF,
    ELSE,
    
    TOP,
    CAST,
    // BOOLEAN OPERATIONS
    EQUALS,
    MORETHAN,
    LESSTHAN,
    MORETHANEQ,
    LESSTHANEQ,
    NOTEQUALS,
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
            IntrinsicType::POP => {
                if isplural {"Pop".to_string()} else {"Pop".to_string()}
            }
            IntrinsicType::PUSH => {
                if isplural {"Push".to_string()} else {"Push".to_string()}
            }
            IntrinsicType::SET => {
                if isplural {"Move".to_string()} else {"Move".to_string()}
            }
            IntrinsicType::ADD => {
                if isplural {"Add".to_string()} else {"Add".to_string()}
            },
            IntrinsicType::SUB => {
                if isplural {"Sub".to_string()} else {"Sub".to_string()}
            }
            IntrinsicType::MUL => {
                if isplural {"Mul".to_string()} else {"Mul".to_string()}
            }
            IntrinsicType::RET => {
                if isplural {"Ret".to_string()} else {"Ret".to_string()}
            },
            IntrinsicType::INCLUDE => {
                if isplural {"Includes".to_string()} else {"Include".to_string()}
            }
            IntrinsicType::IF => {
                if isplural {"Ifs".to_string()} else {"If".to_string()}
            }
            
            IntrinsicType::CONSTANT => {
                if isplural {"Constants".to_string()} else {"Constant".to_string()}
            },
            IntrinsicType::DOTCOMA => {
                if isplural {"Dotcomas".to_string()} else {"Dotcoma".to_string()}
            },
            IntrinsicType::ELSE => {
                if isplural {"Else".to_string()} else {"Else".to_string()}
            },
            IntrinsicType::DIV => {
                if isplural {"Div".to_string()} else {"Div".to_string()}
            },
            IntrinsicType::Let => {
                if isplural {"Let".to_string()} else {"Let".to_string()}
            },
            IntrinsicType::INTERRUPT => {
                if isplural {"Interrupts".to_string()} else {"Interrupt".to_string()}
            },
            IntrinsicType::RS => {
                if isplural {"Return Stacks".to_string()} else {"Return Stack".to_string()}
            },
            IntrinsicType::TOP => {
                if isplural {"Tops".to_string()} else {"Top".to_string()}
            },
            IntrinsicType::CAST => {
                if isplural {"Casts".to_string()} else {"Cast".to_string()}
            },
            IntrinsicType::OPENANGLE => {
                if isplural {"Open angle brackets".to_string()} else {"Open angle bracket".to_string().to_string()}
            },
            IntrinsicType::CLOSEANGLE => {
                if isplural {"Close angle brackets".to_string()} else {"Close angle bracket".to_string().to_string()}
            },
            IntrinsicType::DLL_IMPORT => {
                if isplural {"Dll Imports".to_string()} else {"Dll Import".to_string().to_string()}
            },
            IntrinsicType::DLL_EXPORT => {
                if isplural {"Dll Exports".to_string()} else {"Dll Export".to_string().to_string()}
            },
            IntrinsicType::MORETHAN   => if isplural {"Morethan   ".to_string()} else {"Morethan".to_string()},
            IntrinsicType::LESSTHAN   => if isplural {"Lessthan   ".to_string()} else {"Lessthan".to_string()},
            IntrinsicType::MORETHANEQ => if isplural {"Morethaneq ".to_string()} else {"Morethaneq".to_string()},
            IntrinsicType::LESSTHANEQ => if isplural {"Lessthaneq ".to_string()} else {"Lessthaneq".to_string()},
            IntrinsicType::NOTEQUALS  => if isplural {"Notequals  ".to_string()} else {"Notequals".to_string()},
            IntrinsicType::EQUALS     => if isplural {"Equals".to_string()     } else {"Equal".to_string()},
        }
    }
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
    Number32      (i32),
    Number64      (i64)
}

impl TokenType {
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
            }

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
            TokenType::StringType(data) => Some(data),
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
    fn pop_symbol(&mut self) -> Option<String> {
        let mut o = String::new();
        let mut c = self.cchar_s()?;
        if !c.is_alphabetic() && c != '_' {
            return None;
        }
        while self.is_not_empty() && c.is_alphanumeric() || c=='_' || c=='-' {
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
                        else {
                            return Some(Token { typ: TokenType::StringType(outstr), location: self.currentLocation.clone() });
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
                        else {
                            return Some(Token { typ: TokenType::IntrinsicType(IntrinsicType::DIV), location: self.currentLocation.clone() });
                        }
                    }
                    else {
                        panic!("Error: Abruptly ran out of chars!");
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
                                if nc == 'l' {
                                    self.cursor += 1;
                                    self.currentLocation.character += 1;
                                    if let Ok(val) =  i64::from_str_radix(&outstr, 16) {
                                        return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number64(val)});
                                    }
                                    else {
                                        todo!("Error message for this")
                                    }
                                }
                                else {
                                    if let Ok(val) = i32::from_str_radix(&outstr, 16) {
                                        return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number32(val)});
                                    }   
                                    else {
                                        todo!("Error message for this: {}: \"{}\"",self.currentLocation.loc_display(),outstr);
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
                            if nc == 'l' {
                                self.cursor += 1;
                                self.currentLocation.character += 1;
                                if let Ok(val) = outstr.parse::<i64>() {
                                    return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number64(val)});
                                }
                                else {
                                    todo!("Error message for this")
                                }
                            }
                            else {
                                if let Ok(val) = outstr.parse::<i32>() {
                                    return Some(Token {location: self.currentLocation.clone(), typ: TokenType::Number32(val)});
                                }   
                                else {
                                    todo!("Error message for this: {}: \"{}\"",self.currentLocation.loc_display(),outstr);
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
                        return Some(Token {typ: TokenType::IntrinsicType(self.Intrinsics.get(&c.to_string()).expect("Unhandled intrinsic :(").clone()), location: self.currentLocation.clone()});
                    }
                    else if c == '*' && self.cchar_offset(1).is_some(){
                        let mut already_alphabetic = false;
                        while self.is_not_empty() && (c == '*' && !already_alphabetic) || c.is_alphabetic() {
                            c = self.cchar_s()?;
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
                            return Some(Token { typ: TokenType::IntrinsicType(IntrinsicType::MUL), location: self.currentLocation.clone() });
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
                                //TODO: Maybe a*b could be an issue V Fix this
                                todo!("Handle situation of unknown pointer! Make it return a Mul and reset back the cursor!");
                                
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
    
    //Floating point 32 arithmetics
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    R8D,
    
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
            Register::R8D => "r8d".to_string(),
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
             "R8D" | "r8d" => Some(Register::R8D),
             "RSI" | "rsi" => Some(Register::RSI),
             "RDI" | "rdi" => Some(Register::RDI),
             
            _ => None 
        }
    }
    fn size(&self) -> usize {
        match self {
            Register::RAX => 8,
            Register::RBX => 8,
            Register::RCX => 8,
            Register::RDX => 8,
            Register::RSP => 8,
            Register::RBP => 8,
            Register::RSI => 8,
            Register::RDI => 8,
            Register::EAX => 4,
            Register::EBX => 4,
            Register::ECX => 4,
            Register::EDX => 4,
            Register::ESP => 4,
            Register::EBP => 4,
            Register::ESI => 4,
            Register::EDI => 4,
            Register::AX  => 2,
            Register::BX  => 2,
            Register::CX  => 2,
            Register::DX  => 2,
            Register::SP  => 2,
            Register::BP  => 2,
            Register::SI  => 2,
            Register::DI  => 2,
            Register::AL  => 1,
            Register::BL  => 1,
            Register::CL  => 1,
            Register::DL  => 1,
            Register::AH  => 1,
            Register::BH  => 1,
            Register::CH  => 1,
            Register::DH  => 1,
            Register::SIL => 1,
            Register::DIL => 1,
            Register::R8 => 8,
            Register::R8D => 4,
            Register::RSI => 8,
            Register::RDI => 8,
            Register::R9  => 8,
            Register::R10 => 8,
            Register::R11 => 8,
            Register::R12 => 8,
            Register::R13 => 8,
            Register::R14 => 8,
            Register::R15 => 8,
            Register::XMM0 => 4,
            Register::XMM1 => 4,
            Register::XMM2 => 4,
            Register::XMM3 => 4,
            Register::XMM4 => 4,
            Register::XMM5 => 4,
            Register::XMM6 => 4,
            Register::XMM7 => 4,
        }
    }
    fn to_byte_size(&self, size: usize) -> Self {
        match size {
            8 => {
              match self {
                Register::RAX | Register::EAX  | Register::AX | Register::AH | Register::AL => {
                    return Register::RAX;
                } 
                Register::RBX | Register::EBX  | Register::BX | Register::BH | Register::BL => {
                    return Register::RBX;
                }
                Register::RCX | Register::ECX  | Register::CX | Register::CH | Register::CL => {
                    return Register::RCX;
                }
                Register::RDX | Register::EDX  | Register::DX | Register::DH | Register::DL => {
                    return Register::RDX;
                }
                Register::RSP | Register::ESP  | Register::SP => return Register::RSP,
                Register::RBP | Register::EBP  | Register::BP => Register::RBP,
                Register::RSI | Register::ESI | Register::SI  | Register::SIL => Register::RSI,
                Register::RDI | Register::EDI | Register::DI  | Register::DIL => Register::RDI,
                Register::R8  => todo!(),
                Register::R8D => todo!(),
                Register::R9 => todo!(),
                Register::R10 => todo!(),
                Register::R11 => todo!(),
                Register::R12 => todo!(),
                Register::R13 => todo!(),
                Register::R14 => todo!(),
                Register::R15 => todo!(),
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
                    Register::RAX | Register::EAX  | Register::AX | Register::AH | Register::AL => {
                        return Register::EAX;
                    } 
                    Register::RBX | Register::EBX  | Register::BX | Register::BH | Register::BL => {
                        return Register::EBX;
                    }
                    Register::RCX | Register::ECX  | Register::CX | Register::CH | Register::CL => {
                        return Register::ECX;
                    }
                    Register::RDX | Register::EDX  | Register::DX | Register::DH | Register::DL => {
                        return Register::EDX;
                    }
                    Register::RSP | Register::ESP  | Register::SP => {
                        return Register::ESP;
                    }
                    Register::RBP | Register::EBP  | Register::BP => {
                        return Register::EBP;
                    }
                    Register::RSI | Register::ESI | Register::SI  | Register::SIL => Register::ESI,

                    Register::RDI | Register::EDI | Register::DI  | Register::DIL => Register::EDI,

                    Register::R8 => todo!(),
                    Register::R8D => todo!(),
                    Register::R9 => todo!(),
                    Register::R10 => todo!(),
                    Register::R11 => todo!(),
                    Register::R12 => todo!(),
                    Register::R13 => todo!(),
                    Register::R14 => todo!(),
                    Register::R15 => todo!(),
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

                    Register::R8 => todo!(),
                    Register::R8D => todo!(),
                    Register::R9 => todo!(),
                    Register::R10 => todo!(),
                    Register::R11 => todo!(),
                    Register::R12 => todo!(),
                    Register::R13 => todo!(),
                    Register::R14 => todo!(),
                    Register::R15 => todo!(),
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

                    Register::R8 => todo!(),
                    Register::R8D => todo!(),
                    Register::R9 => todo!(),
                    Register::R10 => todo!(),
                    Register::R11 => todo!(),
                    Register::R12 => todo!(),
                    Register::R13 => todo!(),
                    Register::R14 => todo!(),
                    Register::R15 => todo!(),
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
            _ => {
                panic!("Unexpected use case for to_bit_size!");
            }
        }
    }
    fn to_var_type(&self) -> VarType {
        match self {
            Register::RAX | Register::RBX | Register::RCX | Register::RDX | Register::RSI | Register::RDI => {
                VarType::LONG
            },
            Register::RSP | Register::RBP => {
                VarType::PTR(Ptr { typ: PtrTyp::VOID, inner_ref: 0 })
            },
            Register::EAX | Register::EBX | Register::ECX | Register::EDX | Register::ESI | Register::EDI => {
                VarType::INT
            },
            Register::ESP | Register::EBP => {
                VarType::PTR(Ptr { typ: PtrTyp::VOID, inner_ref: 0})
            }
            Register::AX |Register::BX |Register::CX |Register::DX |Register::SP |Register::BP | Register::SI | Register::DI => {
                VarType::SHORT
            }
            Register::AL |Register::BL |Register::CL |Register::DL |Register::AH |Register::BH |Register::CH |Register::DH | Register::DIL | Register::SIL => {
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
                let uuid = build.insert_unique_str(ProgramString { Typ: ProgramStringType::STR, Data: val.clone() });
                Some(Self { typ: CallArgType::CONSTANT(RawConstValueType::STR(uuid)), loc: tok.location.clone()})
            },
            TokenType::CStringType(val) => {
                let uuid = build.insert_unique_str(ProgramString { Typ: ProgramStringType::CSTR, Data: val.clone() });
                Some(Self { typ: CallArgType::CONSTANT(RawConstValueType::STR(uuid)), loc: tok.location.clone()})
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
type CallArgs = Vec<CallArg>;
// TODO: Introduce this for ADD, SUB, MUL, DIV
#[derive(Debug)]
enum OfP {
    REGISTER (Register),
    //PARAM    (String),
    LOCALVAR (String),
    CONST    (RawConstValueType)
    // RAW      (i64),
    // STR      (Uuid, ProgramStringType)
    // etc.
}
impl OfP { 
    fn LOIRGNasm(&self, regs: Vec<Register>, f: &mut File, program: &CmdProgram,build: &BuildProgram, local_vars: &HashMap<String, LocalVariable>, stack_size: usize, loc: &ProgramLocation) -> std::io::Result<Vec<Register>>{
        let mut out: Vec<Register> = Vec::with_capacity(regs.len());
        match self {
            OfP::REGISTER(reg2) => {
                let reg = &regs[0].to_byte_size(reg2.size());
                writeln!(f, "   mov {}, {}",reg.to_string(),reg2.to_string())?;
                out.push(reg.clone());
            },
            // OfP::PARAM(val) => {
            //     todo!("this");
            //     // let (offset,osize) = func.contract.get_offset_of_param(val,stack_size,&func.contract,program);
            //     // let reg = &regs[0].to_byte_size(osize);
            //     // out.push(reg.clone());
            //     // if offset > 0 {
            //     //     writeln!(f, "   mov {}, {} [rsp+{}]",reg.to_string(),size_to_nasm_type(osize),offset)?;
            //     // }
            //     // else {
            //     //     writeln!(f, "   mov {}, {} [rsp]",reg.to_string(),size_to_nasm_type(osize))?;
            //     // }
            // },
            OfP::LOCALVAR(val) => {
                //println!("Got to here!");
                let lvar = local_vars.get(val).unwrap();
                let osize = lvar.typ.get_size(program);
                let reg = &regs[0].to_byte_size(osize);
                if stack_size-lvar.operand == 0 {
                    writeln!(f, "   mov {}, {} [rsp]",reg.to_string(),size_to_nasm_type(osize))?;
                }
                else {
                    writeln!(f, "   mov {}, {} [rsp+{}]",reg.to_string(),size_to_nasm_type(osize),stack_size-lvar.operand)?;
                }
                out.push(reg.clone());
            },
            OfP::CONST(val) => {
                match val {
                    RawConstValueType::INT(val) => {
                        let reg = &regs[0].to_byte_size(4);
                        writeln!(f, "   mov {}, {}",reg.to_string(),val)?;
                        out.push(reg.clone());
                    },
                    RawConstValueType::LONG(val) => {
                        let reg = &regs[0].to_byte_size(8);
                        writeln!(f, "   mov {}, {}",reg.to_string(),val)?;
                        out.push(reg.clone());
                    },
                    RawConstValueType::STR(val) => {
                        let str = build.stringdefs.get(val).unwrap();
                        match str.Typ {
                            ProgramStringType::STR => {
                                com_assert!(loc,regs.len() > 1, "Error: Cannot load Ofp into register!");
                                let reg = &regs[0];
                                let reg1 = &regs[1];
                                writeln!(f, "   lea {}, [rel _STRING_{}_]",reg.to_string(),val.to_string().replace("-", ""))?;
                                writeln!(f, "   mov {}, {}",reg1.to_string(),build.stringdefs.get(val).unwrap().Data.len())?;
                                out.push(reg.clone()); 
                                out.push(reg1.clone()); 
                            },
                            ProgramStringType::CSTR => {
                                let reg = &regs[0];
                                writeln!(f, "   lea {}, [rel _STRING_{}_]",reg.to_string(),val.to_string().replace("-", ""))?;
                                out.push(reg.clone()); 
                            },
                        }
                    },
                    RawConstValueType::PTR(_,val) => {
                        let reg = &regs[0].to_byte_size(8);
                        writeln!(f, "    mov {}, {}",reg.to_string(),val)?;
                        out.push(reg.clone());
                    },
                }
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
                else if build.constdefs.contains_key(data) {
                    return Some(Self::CONST(build.constdefs.get(data).unwrap().typ.clone()))
                }
                None
            },
            TokenType::Register(reg) => Some(Self::REGISTER(reg.clone())),
            TokenType::StringType(val) => Some(Self::CONST(RawConstValueType::STR(build.insert_unique_str(ProgramString { Typ: ProgramStringType::STR, Data:  val.clone()})))),
            TokenType::CStringType(val)=> Some(Self::CONST(RawConstValueType::STR(build.insert_unique_str(ProgramString { Typ: ProgramStringType::CSTR, Data: val.clone() })))),
            TokenType::Number32(val)      => Some(Self::CONST(RawConstValueType::INT(val.clone()))),
            TokenType::Number64(val)      => Some(Self::CONST(RawConstValueType::LONG(val.clone()))),
            _ => None
        }
    }

}
#[derive(Debug)]
enum Instruction {
    RSPUSH  (OfP),
    //PUSH    (OfP),
    DEFVAR  (String),
    MOV     (OfP, OfP),
    //POP     (OfP),
    CALLRAW (String, CallArgs),
    ADD     (OfP, OfP),
    SUB     (OfP, OfP),
    MUL     (OfP, OfP),
    DIV     (OfP, OfP),
    CALL    (String, CallArgs),
    FNBEGIN (),
    RET     (),
    SCOPEBEGIN,
    SCOPEEND,
    
    // CONDITIONAL_JUMP(usize),
    // JUMP(usize),
    INTERRUPT(i64),
    
    EXPAND_SCOPE(NormalScope),
    EXPAND_IF_SCOPE(NormalScope),
    EXPAND_ELSE_SCOPE(NormalScope),

    EQUALS  (OfP, OfP),
    MORETHAN  (OfP, OfP),
    LESSTHAN  (OfP, OfP),
    MORETHANEQUALS  (OfP, OfP),
    LESSTHANEQUALS  (OfP, OfP),
    NOTEQUALS  (OfP, OfP),
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
}


#[derive(Debug, PartialEq, Clone)]
enum ConstValueType {
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
    fn weak_cast(&self, typ: &VarType) -> Option<ConstValueType> {
        match self {
            ConstValueType::INT(data) => {
                match typ {
                    VarType::CHAR => None,
                    VarType::SHORT => None,
                    VarType::BOOLEAN => None,
                    VarType::INT => Some(self.clone()),
                    VarType::LONG => Some(ConstValueType::LONG(data.clone() as i64)),
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
        }
    }
    fn to_var_type(&self) -> Option<VarType> {
        match self {
            ConstValueType::INT(_) => Some(VarType::INT),
            ConstValueType::LONG(_) => Some(VarType::LONG),
            ConstValueType::STR(_, typ) => if *typ == ProgramStringType::CSTR { Some(VarType::PTR(Ptr{ typ: PtrTyp::TYP(Box::new(VarType::CHAR)), inner_ref: 0})) } else { None },
            ConstValueType::PTR(typ, _) => Some(VarType::PTR(typ.clone())),
        }
    }
    fn is_eq_vartype(&self, vartyp: &VarType) -> bool {
        match self {
            ConstValueType::INT(_) => vartyp.weak_eq(&VarType::INT),
            ConstValueType::LONG(_) => vartyp.weak_eq(&VarType::LONG),
            ConstValueType::STR(_, typ) => if *typ == ProgramStringType::CSTR { vartyp.weak_eq(&VarType::PTR(Ptr {typ: PtrTyp::TYP(Box::new(VarType::CHAR)), inner_ref: 0})) } else {false},
            ConstValueType::PTR(typ, _) => vartyp.weak_eq(&VarType::PTR(typ.clone())),
        }
    }
    fn mul(&self, Other: &ConstValueType) -> Result<ConstValueType,String> {
        match self {
            ConstValueType::INT(val) => {
                match Other {
                    ConstValueType::INT(nval) => {
                        return Ok(ConstValueType::INT(val*nval));
                    }
                    ConstValueType::LONG(nval) => {
                        return Ok(ConstValueType::LONG((*val as i64)*nval));
                    }
                    ConstValueType::STR(_nval, _) => {
                        return Err("Error: Unexpected - operation on int and string".to_string());
                    },
                    ConstValueType::PTR(_,_) => { todo!("ConstValueType::PTR")}
                }
            }
            ConstValueType::LONG(val) => {
                match Other {
                    ConstValueType::INT(nval) => {
                        return Ok(ConstValueType::LONG(val*(*nval as i64)));
                    }
                    ConstValueType::LONG(nval) => {
                        return Ok(ConstValueType::LONG(val*nval));
                    }
                    ConstValueType::STR(_nval, _) => {
                        return Err("Error: Unexpected * operation on long and string".to_string());
                    },
                    ConstValueType::PTR(_,_) => { todo!("ConstValueType::PTR")}
 
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
                     
                }
            },
            ConstValueType::PTR(_,_) => { todo!("ConstValueType::PTR")}
             
        }
    }
}

#[derive(Debug, Clone,PartialEq)]
enum RawConstValueType{
    INT(i32),
    LONG(i64),
    STR(Uuid),
    PTR(Ptr, i64)
}
impl RawConstValueType {
    fn to_type(&self, build: &BuildProgram) -> Vec<VarType> {
        match self {
            RawConstValueType::INT(_) => {
                vec![VarType::INT]
            },
            RawConstValueType::LONG(_) => {
                vec![VarType::LONG]
            },
            RawConstValueType::STR(UUID) => {
                let val = build.stringdefs.get(UUID).unwrap();
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
            RawConstValueType::INT(val) => val.clone() as i64,
            RawConstValueType::LONG(val) => val.clone(),
            RawConstValueType::STR(_) => todo!(),
            RawConstValueType::PTR(_,val) => val.clone(),
        }
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
#[derive(Debug)]
struct BuildProgram {
    externals:    HashMap<String,External>,
    functions:    HashMap<String, Function>,
    stringdefs:   HashMap<Uuid,ProgramString>,
    constdefs:    HashMap<String, RawConstValue>,
    dll_imports:  HashMap<String, DLL_import>,
    dll_exports:  HashMap<String, DLL_export>
}
impl BuildProgram {
    fn insert_unique_str(&mut self, str: ProgramString) -> Uuid {
        let mut UUID: Uuid = Uuid::new_v4();
        while self.stringdefs.contains_key(&UUID) {
            UUID = Uuid::new_v4();
        }
        self.stringdefs.insert(UUID.clone(), str);
        UUID
    }
    fn contains_symbol(&self, str: &String) -> bool {
        self.constdefs.contains_key(str) || self.dll_imports.contains_key(str) || self.externals.contains_key(str) || self.functions.contains_key(str)
    }
    fn get_contract_of_symbol(&self, str: &String) -> Option<&AnyContract> {
        if let Some(ext) = self.externals.get(str) {
            return ext.contract.as_ref()
        }
        else if let Some(dll_import) = self.dll_imports.get(str) {
            return Some(&dll_import.contract)
        }
        None
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
    CUSTOM(Uuid)   
}
impl VarType {
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
        //println!("Comparing: {} and {}",self.to_string(false),other.to_string(false));
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
            Self::INT | Self::LONG=> {
                match other {
                    Self::INT | Self::LONG | Self::SHORT => true,
                    _ => false
                }
            }
            _ => self == other
        }
    }
}
type ScopeBody = Vec<(ProgramLocation,Instruction)>;
type ContractInputs = LinkedHashMap<String, VarType>;
type ContractInputPool = Vec<VarType>;
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
            o
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
    IF,
    ELSE,
    EMPTY
}

#[derive(Debug)]
struct NormalScope {
    typ: NormalScopeType,
    body: ScopeBody,
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
                    NormalScopeType::IF =>    if isplural {"ifs"}     else {"if"},
                    NormalScopeType::ELSE =>  if isplural {"elses"}   else {"else"},
                    NormalScopeType::EMPTY => if isplural {"empties"} else {"empty"},
                }
            },
        }
    }
}
impl Scope {
    fn body_unwrap(&self)         -> Option<&ScopeBody>                    {self.typ.body_unwrap()}
    fn body_unwrap_mut(&mut self) -> Option<&mut ScopeBody>                {self.typ.body_unwrap_mut()}
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
            TokenType::IntrinsicType(typ) => {
                match typ {
                    IntrinsicType::ADD => {
                        let valTwo = par_expect!(token.location,varStack.pop(),"Stack underflow in constant definition");
                        let valOne = par_expect!(token.location,varStack.pop(),"Stack underflow in constant definition");
                        varStack.push(par_expect!(token.location,valOne.add(&valTwo),"Error: Failed to add the constant values together"));
                    }
                    IntrinsicType::SUB => {
                        let valTwo = par_expect!(token.location,varStack.pop(),"Stack underflow in constant definition");
                        let valOne = par_expect!(token.location,varStack.pop(),"Stack underflow in constant definition");
                        varStack.push(par_expect!(token.location,valOne.sub(&valTwo),"Error: Failed to add the constant values together"));
                    }
                    IntrinsicType::MUL => {
                        let valTwo = par_expect!(token.location,varStack.pop(),"Stack underflow in constant definition");
                        let valOne = par_expect!(token.location,varStack.pop(),"Stack underflow in constant definition");
                        varStack.push(par_expect!(token.location,valOne.mul(&valTwo),"Error: Failed to add the constant values together"));
                    }
                    IntrinsicType::CAST => {
                        let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for constant definition cast!");
                        par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::OPENANGLE),"Error: unexpected token type {} in cast! INVALID SYNTAX",ntok.typ.to_string(false));
                        let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for constant definition cast!");
                        let typ = if let TokenType::Definition(typ) = ntok.typ { typ } else {par_error!(ntok, "Error: expected definition but found {}",ntok.typ.to_string(false))};
                        let ntok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for constant definition cast!");
                        par_assert!(ntok, ntok.typ == TokenType::IntrinsicType(IntrinsicType::CLOSEANGLE),"Error: unexpected token type {} in cast! INVALID SYNTAX",ntok.typ.to_string(false));
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
                    RawConstValueType::INT(val)  => {
                        varStack.push(ConstValueType::INT(val));
                    }
                    RawConstValueType::LONG(val) => {
                        varStack.push(ConstValueType::LONG(val));
                    }
                    RawConstValueType::STR(ref UUID)       => {
                        let d = build.stringdefs.get(UUID).unwrap();
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
fn parse_argument_contract(lexer: &mut Lexer, build: &mut BuildProgram, currentLocals: &Vec<Locals>) -> CallArgs {
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
                        return out},
                    IntrinsicType::TOP => {
                        expectNextSY = true;
                        out.push(par_expect!(token, CallArg::from_token(build,&token, currentLocals),"Unexpected Token Type in argument Contract. Expected Definition but found: {}",token.typ.to_string(false)));
                    }
                    other => par_error!(token, "Unexpected intrinsic in argument contract! {}",other.to_string(false))
                }
            }
            _ => {
                out.push(par_expect!(token, CallArg::from_token(build,&token, currentLocals),"Unexpected Token Type in argument Contract. Expected Definition but found: {}",token.typ.to_string(false)));
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
    let mut out = AnyContract { InputPool: vec![], Outputs: vec![] };
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
    scopeStack.get_mut(len-1)
}
fn get_local<'a>(currentLocals: &'a Vec<Locals>, name: &String) -> Option<&'a VarType> {
    for e in currentLocals {
        if let Some(v) = e.get(name) {
            return Some(v);
        }
    }
    None
}
fn contains_local<'a>(currentLocals: &'a Vec<Locals>, name: &String) -> bool {
    for e in currentLocals {
        if e.contains_key(name) {
            return true   
        }
    }
    false
}
fn parse_tokens_to_build(lexer: &mut Lexer, program: &mut CmdProgram) -> BuildProgram {
    let mut build: BuildProgram = BuildProgram { externals: HashMap::new(), functions: HashMap::new(),stringdefs: HashMap::new(), constdefs: HashMap::new(), dll_imports: HashMap::new(), dll_exports: HashMap::new() };
    let mut scopeStack: ScopeStack = vec![];
    let mut currentLocals: Vec<Locals> = Vec::new();
    while let Some(token) = lexer.next() {
        match token.typ {
            TokenType::WordType(ref word) => {
                par_assert!(token,scopeStack.len() > 0, "Undefined Word Call outside of entry point! '{}'",word);
                let currentScope = getTopMut(&mut scopeStack).unwrap();
                par_assert!(token,currentScope.body_is_some(), "Error: can not insert word operation at the top level of a {} as it does not support instructions",currentScope.typ.to_string(false));
                if build.externals.contains_key(word) {
                    let contract = parse_argument_contract(lexer, &mut build, &currentLocals);
                    let body = currentScope.body_unwrap_mut().unwrap();
                    body.push((token.location.clone(),Instruction::CALLRAW(word.clone(), contract)));
                    continue;
                } else if build.dll_imports.contains_key(word) {
                    let contract = parse_argument_contract(lexer, &mut build, &currentLocals);
                    let body = currentScope.body_unwrap_mut().unwrap();
                    body.push((token.location.clone(),Instruction::CALLRAW(word.clone(), contract)));
                    continue;
                }
                let ofp1 = par_expect!(token, OfP::from_token(&token, &mut build, program,&currentLocals), "Unknown word type: {}!",word);
                let Op = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected variable operation or another variable!");
                match &Op.typ {
                    
                    TokenType::IntrinsicType(typ) => {
                        match typ {
                            IntrinsicType::SET => {
                                let ofp2_t = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected variable operation or another variable!");
                                let ofp2 = par_expect!(token, OfP::from_token(&ofp2_t, &mut build, program,&currentLocals), "Unknown word type: {}!",ofp2_t.typ.to_string(false));
                                currentScope.body_unwrap_mut().unwrap().push((Op.location.clone(), Instruction::MOV(ofp1, ofp2)))
                            },
                            IntrinsicType::EQUALS => {
                                let ofp2_t = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected variable operation or another variable!");
                                let ofp2 = par_expect!(token, OfP::from_token(&ofp2_t, &mut build, program,&currentLocals), "Unknown word type: {}!",ofp2_t.typ.to_string(false));
                                currentScope.body_unwrap_mut().unwrap().push((Op.location.clone(), Instruction::EQUALS(ofp1, ofp2)))
                            }
                            IntrinsicType::NOTEQUALS => {
                                let ofp2_t = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected variable operation or another variable!");
                                let ofp2 = par_expect!(token, OfP::from_token(&ofp2_t, &mut build, program,&currentLocals), "Unknown word type: {}!",ofp2_t.typ.to_string(false));
                                currentScope.body_unwrap_mut().unwrap().push((Op.location.clone(), Instruction::NOTEQUALS(ofp1, ofp2)))
                            }
                            
                            IntrinsicType::LESSTHAN => {
                                let ofp2_t = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected variable operation or another variable!");
                                let ofp2 = par_expect!(token, OfP::from_token(&ofp2_t, &mut build, program,&currentLocals), "Unknown word type: {}!",ofp2_t.typ.to_string(false));
                                currentScope.body_unwrap_mut().unwrap().push((Op.location.clone(), Instruction::LESSTHAN(ofp1, ofp2)))
                            }
                            IntrinsicType::MORETHAN => {
                                let ofp2_t = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected variable operation or another variable!");
                                let ofp2 = par_expect!(token, OfP::from_token(&ofp2_t, &mut build, program,&currentLocals), "Unknown word type: {}!",ofp2_t.typ.to_string(false));
                                currentScope.body_unwrap_mut().unwrap().push((Op.location.clone(), Instruction::MORETHAN(ofp1, ofp2)))
                            }
                            IntrinsicType::LESSTHANEQ => {
                                let ofp2_t = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected variable operation or another variable!");
                                let ofp2 = par_expect!(token, OfP::from_token(&ofp2_t, &mut build, program,&currentLocals), "Unknown word type: {}!",ofp2_t.typ.to_string(false));
                                currentScope.body_unwrap_mut().unwrap().push((Op.location.clone(), Instruction::LESSTHANEQUALS(ofp1, ofp2)))
                            }
                            IntrinsicType::MORETHANEQ => {
                                let ofp2_t = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected variable operation or another variable!");
                                let ofp2 = par_expect!(token, OfP::from_token(&ofp2_t, &mut build, program,&currentLocals), "Unknown word type: {}!",ofp2_t.typ.to_string(false));
                                currentScope.body_unwrap_mut().unwrap().push((Op.location.clone(), Instruction::MORETHANEQUALS(ofp1, ofp2)))
                            }
                            
                            
                            _ => {
                                par_error!(Op, "Unexpected intrinsic {} after ofp",typ.to_string(false))
                            }
                        }
                    },
                    _ => {
                        let ofp2 = par_expect!(token, OfP::from_token(&Op, &mut build, program,&currentLocals), "Unexpected token type: {} after ofp!",Op.typ.to_string(false));
                        let Op = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected variable operation or another variable!");
                        match &Op.typ {
                            TokenType::IntrinsicType(op) => {
                                match op {
                                    IntrinsicType::ADD => currentScope.body_unwrap_mut().unwrap().push((Op.location.clone(), Instruction::ADD(ofp1,ofp2))),
                                    IntrinsicType::SUB => currentScope.body_unwrap_mut().unwrap().push((Op.location.clone(), Instruction::SUB(ofp1,ofp2))),
                                    IntrinsicType::MUL => currentScope.body_unwrap_mut().unwrap().push((Op.location.clone(), Instruction::MUL(ofp1,ofp2))),
                                    IntrinsicType::DIV => currentScope.body_unwrap_mut().unwrap().push((Op.location.clone(), Instruction::DIV(ofp1,ofp2))),
                                    IntrinsicType::EQUALS  => currentScope.body_unwrap_mut().unwrap().push((Op.location.clone(), Instruction::EQUALS(ofp1,ofp2))),
                                    _ => {
                                        par_error!(Op, "Unexpected intrinsic {} after ofp",Op.typ.to_string(false))
                                    }
                                }
                            },
                            _ => {
                                par_error!(Op, "Unexpected token type {} after ofp",Op.typ.to_string(false))
                            }
                        }
                    }
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
                            TokenType::StringType(Type) => {
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
                                                        let ntc = par_expect!(lexer.currentLocation,lexer.next(),"Error: stream of tokens abruptly ended in extern definition");
                                                        par_assert!(ntc, ntc.typ==TokenType::IntrinsicType(IntrinsicType::DOTCOMA),"Error: Expected dotcoma at the end of extern definition! But found {}",ntc.typ.to_string(false));
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
                                scopeStack.push(Scope { typ: ScopeType::FUNCTION(Function { contract, body:  vec![(token.location.clone(),Instruction::FNBEGIN())], location: token.location.clone(), locals: Locals::new() }, Word), hasBeenOpened: false });
                                currentLocals.push(locals);
                                build.functions.reserve(1);
                            }
                            Other => par_error!(token,"Unexpected behaviour! Expected type Word but found {}",Other.to_string(false))
                        }

                    }
                    IntrinsicType::OPENPAREN => todo!("loc: {}",token.location.loc_display()),
                    IntrinsicType::CLOSEPAREN => todo!(),
                    IntrinsicType::DOUBLE_COLIN => todo!("Context {:#?}",build),
                    IntrinsicType::COMA => todo!(),
                    IntrinsicType::OPENCURLY => {
                        let ln = scopeStack.len();
                        if ln != 0 {
                            let s = scopeStack.last_mut().unwrap();
                            if s.hasBeenOpened {
                                //println!("CurrentLocals: {:#?}",currentLocals);
                                currentLocals.push(Locals::new());
                                //println!("CurrentLocals: {:#?}",currentLocals);
                                scopeStack.push(Scope { typ: ScopeType::NORMAL(NormalScope { typ: NormalScopeType::EMPTY, body: vec![], locals: Locals::new()}), hasBeenOpened: true });
                                continue;
                            }
                            par_assert!(token,!s.hasBeenOpened, "Scope already opened! {:?}",scopeStack);
                            s.hasBeenOpened = true;
                            
                            match &s.typ {
                                ScopeType::FUNCTION(_, _) => {}
                                ScopeType::NORMAL(normal) => {
                                    //println!("CurrentLocals: {:#?}",currentLocals);
                                    currentLocals.push(Locals::new());
                                    match normal.typ {
                                        NormalScopeType::IF => {
                                           par_assert!(token, ln > 1, "Error: Alone if outside of any scope is not allowed!");
                                           let prev = scopeStack.get_mut(ln-2).unwrap();
                                           par_assert!(token, prev.body_is_some(), "Error: if can not be declared inside of scope of {} as they do not allow instructions!",prev.typ.to_string(true));
                                        }
                                        NormalScopeType::ELSE | NormalScopeType::EMPTY => {}
                                    }
                                }
                            }
                        }
                        else {
                            
                            scopeStack.push(Scope { typ: ScopeType::NORMAL(NormalScope { typ: NormalScopeType::EMPTY, body: vec![], locals: Locals::new()}), hasBeenOpened: true});
                        }
                    }
                    IntrinsicType::CLOSECURLY => {
                        if let Some(sc) = scopeStack.pop() {
                            par_assert!(token,sc.hasBeenOpened, "Error: scope closed but never opened!");
                            match sc.typ {
                                ScopeType::FUNCTION(mut func, name) => {
                                    //println!("CurrentLocals function: {:#?}",currentLocals);
                                    func.body.push((token.location.clone(),Instruction::SCOPEEND));
                                    func.locals = currentLocals.pop().unwrap();
                                    build.functions.insert(name, func);
                                },
                                ScopeType::NORMAL(mut normal) => {
                                    //println!("CurrentLocals at Normal: {:#?}",currentLocals);
                                    normal.locals = currentLocals.pop().unwrap();
                                    match normal.typ {
                                        NormalScopeType::IF => {
                                            let currentScope = getTopMut(&mut scopeStack).unwrap();
                                            let body = par_expect!(token,currentScope.body_unwrap_mut(), "Error: Can not close if, because it is inside a {} which doesn't support instructions!",currentScope.typ.to_string(false));
                                            body.push((token.location.clone(),Instruction::EXPAND_IF_SCOPE(normal)))
                                        },
                                        NormalScopeType::EMPTY => {
                                            let currentScope = getTopMut(&mut scopeStack).unwrap();
                                            let body = par_expect!(token,currentScope.body_unwrap_mut(), "Error: Can not close if, because it is inside a {} which doesn't support instructions!",currentScope.typ.to_string(false));
                                            body.push((token.location.clone(),Instruction::EXPAND_SCOPE(normal)));
                                        }
                                        NormalScopeType::ELSE => {
                                            let currentScope = getTopMut(&mut scopeStack).unwrap();

                                            let body = par_expect!(token,currentScope.body_unwrap_mut(), "Error: Can not close if, because it is inside a {} which doesn't support instructions!",currentScope.typ.to_string(false));
                                            body.push((token.location.clone(),Instruction::EXPAND_ELSE_SCOPE(normal)));
                                        },
                                    }
                                }
                            }
                            
                        }
                        else {
                            par_error!(token, "Scope closed but never opened!!!");
                        }
                    }
                    IntrinsicType::ADD | IntrinsicType::SET  | IntrinsicType::PUSH | IntrinsicType::SUB | IntrinsicType::MUL | IntrinsicType::POP => {
                        par_error!(token,"Unexpected token {}",Type.to_string(false));   
                    }
                    IntrinsicType::RET => {
                        par_assert!(token, scopeStack.len()> 0 && getTopMut(&mut scopeStack).unwrap().body_is_some(), "Error: Unexpected return intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(&mut scopeStack).unwrap().typ.to_string(false));
                        let body = getTopMut(&mut scopeStack).unwrap().body_unwrap_mut().unwrap();
                        body.push((token.location.clone(),Instruction::RET()));
                    },
                    IntrinsicType::INCLUDE => {
                        let includeName = par_expect!(lexer.currentLocation,lexer.next(),"Error: abruptly ran out of tokens");
                        match includeName.typ {
                            TokenType::StringType(path) => {
                                let p = PathBuf::from(&program.path);
                                let p = p.parent().unwrap();
                                let include_p  = PathBuf::from(path);
                                let info = fs::read_to_string(String::from(p.join(&include_p).to_str().unwrap().replace("\\", "/"))).expect(&format!("Error: could not open file: {}",String::from(p.join(&include_p).to_str().unwrap()).replace("\\", "/")));
                                let mut lf = Lexer::new(&info,lexer.Intrinsics,lexer.Definitions,HashSet::new());
                                lf.currentLocation.file = Rc::new(String::from(p.join(&include_p).to_str().unwrap().replace("\\", "/")));
                                let mut nprogram = program.clone();
                                nprogram.path = String::from(p.join(&include_p).to_str().unwrap().replace("\\", "/"));
                                let mut build2 = parse_tokens_to_build(&mut lf, &mut nprogram);
                                build.externals.extend(build2.externals);
                                for (strdefId,_strdef) in build2.stringdefs.iter() {
                                    let orgstrdefId  = strdefId.clone();
                                    let mut strdefId = strdefId.clone();
                                    let isContaining = build.stringdefs.contains_key(&strdefId);
                                    while build.stringdefs.contains_key(&strdefId) || build2.stringdefs.contains_key(&strdefId) {
                                        strdefId = Uuid::new_v4();
                                    }
                                    if isContaining {
                                        for (_, cn_cn) in build2.constdefs.iter_mut() {
                                            match cn_cn.typ {
                                                _ => {}
                                                _ => {}
                                                RawConstValueType::STR(ref mut val) => {
                                                    if &orgstrdefId == val {
                                                        *val = strdefId
                                                    }
                                                },
                                            }
                                        }
                                    }
                                }
                                build.stringdefs.extend(build2.stringdefs);
                                for (fn_name,fn_fn) in build2.functions {
                                    let _loc = fn_fn.location.clone();
                                    match build.functions.insert(fn_name.clone(), fn_fn) {
                                        Some(_Other) => {
                                            par_error!(_loc, "Error: mulitply defined symbols {}",fn_name);
                                        },
                                        None => {},
                                    }
                                }
                                build.constdefs.reserve(build2.constdefs.len());
                                for (cn_name,cn_val) in build2.constdefs{
                                    let loc = cn_val.loc.clone();
                                    match build.constdefs.insert(cn_name.clone(),cn_val) {
                                        Some(_Other) => {
                                            par_error!(loc,"Error: mulitply defined symbols {}",cn_name);
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
                    IntrinsicType::IF => {
                        scopeStack.push(Scope { typ: ScopeType::NORMAL(NormalScope { typ: NormalScopeType::IF, body: vec![], locals: Locals::new()}), hasBeenOpened: false })
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
                            TokenType::IntrinsicType(typ) => {
                                match typ {
                                    IntrinsicType::SET => {}
                                    IntrinsicType::DOUBLE_COLIN => {
                                        let typ = par_expect!(lexer.currentLocation,lexer.next(), "Error: abruptly ran out of tokens in constant type definition");
                                        if let TokenType::Definition(d) = typ.typ {
                                            expect_type = Some(d);
                                            let typ = par_expect!(lexer.currentLocation,lexer.next(), "Error: abruptly ran out of tokens in constant type definition");
                                            par_assert!(typ, typ.typ == TokenType::IntrinsicType(IntrinsicType::SET), "Error: unexpected token type {} after constant definition! Expected =",typ.typ.to_string(false));
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
                        
                        let val = eval_const_def(lexer,&mut build,TokenType::IntrinsicType(IntrinsicType::DOTCOMA));
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
                                let mut UUID = Uuid::new_v4();
                                while build.stringdefs.contains_key(&UUID) {
                                    UUID = Uuid::new_v4();
                                }
                                build.stringdefs.insert(UUID,ProgramString {Data: rval, Typ: typ});
                                RawConstValue {typ: RawConstValueType::STR(UUID), loc: val.loc}
                            }
                            ConstValueType::PTR(typ,v) => { 
                                RawConstValue {typ: RawConstValueType::PTR(typ, v), loc: val.loc}
                            }
 
                        };
                        if let Some(constdef) = build.constdefs.get(&name) {
                            let i1 = constdef.typ.to_type(&build);
                            let i2 = result.typ.to_type(&build);
                            par_assert!(result.loc, i1 == i2,"Error: Constant value defined at {}, Redined with a different type at {}!\nOriginal = {:?}\nRedefinition = {:?}",constdef.loc.loc_display(),result.loc.loc_display(),i1,i2);
                        }
                        build.constdefs.insert(name, result);
                        
                    },
                    IntrinsicType::DOTCOMA => {},
                    IntrinsicType::ELSE => todo!(),
                    IntrinsicType::DIV => todo!("{:#?} at {}",build,lexer.currentLocation.loc_display()),
                    IntrinsicType::Let => {
                        
                        let nametok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for Let");
                        let loc = nametok.location.clone();
                        match nametok.typ {
                            TokenType::WordType(name) => {
                                par_assert!(loc, !build.contains_symbol(&name), "Error: Redifinition of existing symbol {}",name);
                                par_assert!(token, scopeStack.len() > 0 && getTopMut(&mut scopeStack).unwrap().body_is_some(), "Error: Unexpected multiply intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(&mut scopeStack).unwrap().typ.to_string(false));
                                let currentScope = getTopMut(&mut scopeStack).unwrap();
                                let typ = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for let type");                                
                                par_assert!(typ,typ.typ==TokenType::IntrinsicType(IntrinsicType::DOUBLE_COLIN), "Error: You probably forgot to put a : after the name!");
                                let typ = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for let type");
                                match typ.typ {
                                    TokenType::Definition(def) => {
                                        currentLocals.last_mut().unwrap().insert(name.clone(), def);
                                        currentScope.body_unwrap_mut().unwrap().push((lexer.currentLocation.clone(),Instruction::DEFVAR(name)))
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
                    },
                    IntrinsicType::INTERRUPT => {
                        
                        par_assert!(token, scopeStack.len() > 0 && getTopMut(&mut scopeStack).unwrap().body_is_some(), "Error: Unexpected interrupt intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(&mut scopeStack).unwrap().typ.to_string(false));
                        let body = getTopMut(&mut scopeStack).unwrap().body_unwrap_mut().unwrap();
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
                    IntrinsicType::RS => {
                        par_assert!(token, scopeStack.len() > 0 && getTopMut(&mut scopeStack).unwrap().body_is_some(), "Error: Unexpected rs intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(&mut scopeStack).unwrap().typ.to_string(false));
                        let body = getTopMut(&mut scopeStack).unwrap().body_unwrap_mut().unwrap();
                        
                        let lexerNext = par_expect!(lexer.currentLocation,lexer.next(),"Stream of tokens ended abruptly at RS call");
                        match lexerNext.typ {
                            TokenType::Register(reg) => {
                                let optyp = par_expect!(lexer.currentLocation,lexer.next(),"Stream of tokens ended abruptly at RS push call");
                                match optyp.typ {
                                    TokenType::IntrinsicType(typ) => {
                                    match typ {
                                        IntrinsicType::PUSH => {
                                            
                                            body.push((lexer.currentLocation.clone(), Instruction::RSPUSH(OfP::REGISTER(reg))))            
                                        }
                                        _ => {
                                            par_error!(lexerNext.location, "Error: Unexpected Intrinsic Type: {}",typ.to_string(false))
                                        }
                                    }
                                    }
                                    _ => {
                                        par_error!(lexerNext.location, "Error: Expected Intrinsic but found {}",lexerNext.typ.to_string(false))
                                    }
                                }
                                
                            }
                            _ => {
                                par_error!(lexerNext.location, "Error: Expected Register but found {}",lexerNext.typ.to_string(false))                
                            }
                            
                        }
                    },
                    IntrinsicType::TOP => todo!(),
                    IntrinsicType::CAST => todo!(),
                    IntrinsicType::OPENANGLE => todo!(),
                    IntrinsicType::CLOSEANGLE => todo!(),
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
                    IntrinsicType::MORETHAN => todo!(),
                    IntrinsicType::LESSTHAN => todo!(),
                    IntrinsicType::MORETHANEQ => todo!(),
                    IntrinsicType::LESSTHANEQ => todo!(),
                    IntrinsicType::NOTEQUALS => todo!(),
                    IntrinsicType::EQUALS => todo!(),
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
            TokenType::Definition(_) => todo!(),
            TokenType::CStringType(_) => todo!(),
            TokenType::Function(name) => {
                let args = parse_argument_contract(lexer, &mut build, &currentLocals);
                let body = getTopMut(&mut scopeStack).unwrap().body_unwrap_mut().unwrap();
                body.push((token.location.clone(),Instruction::CALL(name, args)));
                
            },
            TokenType::Register(reg) => {
                let currentScope = getTopMut(&mut scopeStack).unwrap();
                let regOp = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected register operation or another register!");
                match regOp.typ {
                    TokenType::IntrinsicType(typ) => {
                        match typ {
                            IntrinsicType::SET => {
                                let token = par_expect!(lexer.currentLocation,lexer.next(),"abruptly ran out of tokens");
                                match token.typ {
                                    TokenType::Number32(data) => {
                                        if reg.size() >= 4 {
                                            let body = currentScope.body_unwrap_mut().unwrap();
                                            body.push((token.location,Instruction::MOV(OfP::REGISTER(reg), OfP::CONST(RawConstValueType::INT(data)))))
                                        }
                                    }
                                    TokenType::Number64(data) => {
                                        if reg.size() >= 8 {
                                            let body = currentScope.body_unwrap_mut().unwrap();
                                            body.push((token.location,Instruction::MOV(OfP::REGISTER(reg), OfP::CONST(RawConstValueType::LONG(data)))));
                                        }
                                    }
                                    TokenType::Register(reg2) => {
                                        currentScope.body_unwrap_mut().unwrap().push((token.location.clone(),Instruction::MOV(OfP::REGISTER(reg), OfP::REGISTER(reg2))));

                                    }
                                    TokenType::WordType(ref data) => {
                                        if currentScope.contract_is_some() && currentScope.contract_unwrap().unwrap().Inputs.contains_key(data) {
                                            let contract = currentScope.contract_unwrap().unwrap();
                                            par_assert!(token, contract.Inputs.contains_key(data), "Error: Unexpected word for register: '{}'",data);
                                            currentScope.body_unwrap_mut().unwrap().push((token.location.clone(), Instruction::MOV(OfP::REGISTER(reg), OfP::LOCALVAR(data.to_owned()))))
                                        }
                                        else if let Some(cons) = build.constdefs.get(data) {
                                            currentScope.body_unwrap_mut().unwrap().push((token.location.clone(), Instruction::MOV(OfP::REGISTER(reg), OfP::CONST(cons.typ.clone()))))
                                        }
                                        else {
                                            par_error!(token,"Unexpected Type for Mov Intrinsic. Expected Number32/Number64 but found {}",token.typ.to_string(false))
                                        }
                                    }
                                    _ => par_error!(token,"Unexpected Type for Mov Intrinsic. Expected Number32/Number64 but found {}",token.typ.to_string(false))
                                }
                            }
                            Other => {
                                par_error!(token,"Unexpected Intrinsic Type: {}, Registers can only perform register operations \"pop\" \"push\" \"mov\" ",Other.to_string(false))
                            }
                        }
                    },
                    TokenType::Register(reg2) => {
                        par_assert!(token,reg.size()==reg2.size(),"Gotten two differently sized registers to one op!");
                        let regOp = lexer.next().expect(&format!("(P) [ERROR] {}:{}:{}: Unexpected register operation!",token.location.clone().file,&token.location.clone().linenumber,&token.location.clone().character));
                        let body = currentScope.body_unwrap_mut().unwrap();
                        match regOp.typ {
                            TokenType::IntrinsicType(typ) => {
                                
                                match typ {
                                    IntrinsicType::ADD => {
                                        body.push((token.location.clone(),Instruction::ADD(OfP::REGISTER(reg), OfP::REGISTER(reg2))))
                                    }
                                    IntrinsicType::SUB => {
                                        body.push((token.location.clone(),Instruction::SUB(OfP::REGISTER(reg), OfP::REGISTER(reg2))))
                                    }
                                    IntrinsicType::MUL => {
                                        body.push((token.location.clone(),Instruction::MUL(OfP::REGISTER(reg), OfP::REGISTER(reg2))))
                                    }
                                    IntrinsicType::EQUALS => {
                                        body.push((token.location.clone(),Instruction::EQUALS(OfP::REGISTER(reg), OfP::REGISTER(reg2))))
                                    }
                                    IntrinsicType::DIV => {
                                        body.push((token.location.clone(),Instruction::DIV(OfP::REGISTER(reg), OfP::REGISTER(reg2))))
                                    }
                                    IntrinsicType::SET => {
                                        body.push((token.location.clone(),Instruction::MOV(OfP::REGISTER(reg), OfP::REGISTER(reg2))))
                                    }
                                    other => par_error!(token,"Unexpected Intrinsic! Expected Register Intrinsic but found {}",other.to_string(false))
                                }
                            }
                            other => {
                                par_error!(token, "Unexpected token type: Expected Intrinsic but found {}",other.to_string(false));
                            }
                        }
                    }
                    typ => {
                        par_error!(token,"Unexpected register operation! Expected Intrinsic or another Register but found {}",typ.to_string(false));
                    }
                }
            },
        }
    }
    for (fn_name, fn_fn)in build.functions.iter_mut() {
        if fn_name != "main" && fn_fn.location.file == lexer.currentLocation.file {
            fn_fn.body.push((fn_fn.location.clone(),Instruction::RET()))
        }
    }
    build
}

struct optim_ops {
    usedStrings: HashMap<Uuid, String>,
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
                    match &arg.typ {
                        CallArgType::CONSTANT(val) => {
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
            Instruction::MOV(_, v2) => {
                match v2 {
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
            Instruction::CALL(r,args) => {
                for arg in args {
                    match &arg.typ {
                        CallArgType::CONSTANT(val) => {
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
            Instruction::EXPAND_SCOPE(s) => {
                optimization_ops_scope(build, program, TCScopeType::NORMAL(s), out, fn_name.clone())
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
fn nasm_x86_64_prep_args(program: &CmdProgram, build: &BuildProgram, f: &mut File,mut _econtract: AnyContract,contract: &Vec<CallArg>, stack_size: &mut usize, _: ProgramLocation, local_vars: &HashMap<String, LocalVariable>) -> io::Result<()>{
    let mut shadow_space = 0;
    if let Some(ops) = program.architecture.options.argumentPassing.custom_get() {
        if ops.shadow_space > 0 {
            shadow_space = ops.shadow_space;
            writeln!(f, "   sub rsp, {}",ops.shadow_space)?;
        }
    }
    let dcontract = contract.clone();
    #[allow(unused_variables)]
    let mut stack_space_taken: usize = 0;
    let mut int_ptr_count:  usize = 0;
    let mut _float_count:    usize = 0;
    for arg in dcontract {
        match arg.typ {
            CallArgType::LOCALVAR(name) => {
                let var1 = local_vars.get(&name).expect("Unknown local variable parameter");
                let oreg = Register::RAX.to_byte_size(var1.typ.get_size(program));
                if *stack_size-var1.operand == 0 {
                    writeln!(f, "   mov {}, [rsp+{}]",oreg.to_string(),shadow_space)?;
                }
                else {
                    writeln!(f, "   mov {}, [rsp+{}]",oreg.to_string(),*stack_size-var1.operand+shadow_space)?;
                }
                let osize = var1.typ.get_size(program);
                if program.architecture.options.argumentPassing == ArcPassType::PUSHALL {
                    //TODO: todo!("This is probably broken btw");
                    stack_space_taken += osize;
                    *stack_size += osize;
                    let oreg = Register::RAX.to_byte_size(osize);
                    writeln!(f, "   sub rsp, {}",osize)?;
                    writeln!(f, "   mov {} [rsp], {}",size_to_nasm_type(osize), oreg.to_string())?;
                }
                else {
                    match &program.architecture.options.argumentPassing {
                        ArcPassType::CUSTOM(ops) => {
                            if let Some(numptrs) = &ops.nums_ptrs {
                                if int_ptr_count > numptrs.len() && ops.on_overflow_stack{
                                    stack_space_taken += osize;
                                    *stack_size += osize;
                                    let oreg = Register::RAX.to_byte_size(osize);
                                    writeln!(f, "   sub rsp, {}",osize)?;    
                                    writeln!(f, "   mov {} [rsp], {}",size_to_nasm_type(osize),oreg.to_string())?;
                                }
                                else {                                                    
                                    let ireg = numptrs.get(int_ptr_count).unwrap();
                                    writeln!(f, "   mov {}, rax",ireg.to_string())?;    
                                }
                            }
                            else {
                                stack_space_taken += osize;
                                *stack_size += osize;
                                writeln!(f, "   sub rsp, {}",osize)?;    
                                writeln!(f, "   mov qword [rsp], rax")?;
                            }
                        }
                        _ => todo!("Unhandled")
                    }
                }
                int_ptr_count += 1;
            },
            CallArgType::REGISTER(_) => {
                todo!("Registers are disabled!");
            },
            CallArgType::CONSTANT(val) => {
                match val {
                    RawConstValueType::INT(val) => {
                        if program.architecture.options.argumentPassing == ArcPassType::PUSHALL {
                            
                            let osize = 4;
                            stack_space_taken += osize;
                            *stack_size += osize;
                            writeln!(f, "   sub rsp, {}",osize)?;
                            writeln!(f, "   mov eax, {}",val)?;
                            writeln!(f, "   mov dword [rsp], eax")?;
                            int_ptr_count += 1;
                        }
                        else {
                            let ops = program.architecture.options.argumentPassing.custom_get().unwrap();
                            if let Some(num_ptrs) = &ops.nums_ptrs {
                                let ireg = num_ptrs.get(int_ptr_count).unwrap();
                                writeln!(f, "   mov {}, {}",ireg.to_string(), val)?;    
                            }
                            else {
                                let osize = 4;
                                stack_space_taken += osize;
                                *stack_size += osize;
                                writeln!(f, "   sub rsp, {}",osize)?;
                                writeln!(f, "   mov eax, {}",val)?;
                                writeln!(f, "   mov dword [rsp], eax")?;
                            }
                            int_ptr_count += 1;
                        }
                    },
                    RawConstValueType::LONG(val) => {
                        let osize = 8;
                        if program.architecture.options.argumentPassing == ArcPassType::PUSHALL {
                            stack_space_taken += osize;
                            *stack_size += osize;
                            writeln!(f, "   sub rsp, {}",osize)?;
                            writeln!(f, "   mov rax, {}",val)?;
                            writeln!(f, "   mov qword [rsp], rax")?;
                            int_ptr_count += 1;
                        }
                        else {
                            let ops = program.architecture.options.argumentPassing.custom_get().unwrap();
                            if let Some(num_ptrs) = &ops.nums_ptrs {
                                let ireg = num_ptrs.get(int_ptr_count).unwrap();
                                
                                writeln!(f, "   mov {}, {}",ireg.to_string(), val)?;    
                            }
                            else {
                                stack_space_taken += osize;
                                *stack_size += osize;
                                writeln!(f, "   sub rsp, {}",osize)?;
                                writeln!(f, "   mov rax, {}",val)?;
                                writeln!(f, "   mov qword [rsp], rax")?;
                            }
                            int_ptr_count += 1;
                        }
                    },
                    RawConstValueType::STR(UUID) => {
                        let osize: usize = build.stringdefs.get(&UUID).unwrap().Typ.sizeof(program.architecture.bits);
                        if program.architecture.options.argumentPassing == ArcPassType::PUSHALL {
                            stack_space_taken += osize;
                            *stack_size += osize;
                            let typ = &build.stringdefs.get(&UUID).unwrap().Typ;
                            match typ {
                                ProgramStringType::STR => {
                                    match program.architecture.bits {
                                        64 => {
                                            writeln!(f, "   sub rsp, 12")?;
                                            writeln!(f, "   mov qword [rsp+8], _STRING_{}_",UUID.to_string().replace("-", ""))?;
                                            *stack_size += 12
                                        }
                                        32 | _ => {
                                            writeln!(f, "   sub rsp, 16")?;
                                            writeln!(f, "   mov dword [rsp+8], _STRING_{}_",UUID.to_string().replace("-", ""))?;
                                            *stack_size += 16
                                        }
                                    }
                                    writeln!(f, "   mov qword [rsp], {}",build.stringdefs.get(&UUID).unwrap().Data.len())?;
                                    
                                },
                                ProgramStringType::CSTR => {
                                    match program.architecture.bits {
                                        64 => {
                                            writeln!(f, "   sub rsp, 8")?;
                                            writeln!(f, "   mov qword [rsp], _STRING_{}_",UUID.to_string().replace("-", ""))?;
                                            *stack_size += 8
                                        }
                                        32 | _ => {
                                            writeln!(f, "   sub rsp, 4")?;
                                            writeln!(f, "   mov dword [rsp], _STRING_{}_",UUID.to_string().replace("-", ""))?;
                                            *stack_size += 4
                                        }
                                    }
                                },
                            }
                            int_ptr_count += 1;
                        }
                        else {
                            let ops = program.architecture.options.argumentPassing.custom_get().unwrap();
                            if let Some(num_ptrs) = &ops.nums_ptrs {
                                let ireg = num_ptrs.get(int_ptr_count).unwrap();
                                let s = build.stringdefs.get(&UUID).unwrap();
                                match s.Typ {
                                    ProgramStringType::STR => {
                                        writeln!(f, "   lea {}, [rel _STRING_{}_]",ireg.to_string(), UUID.to_string().replace("-", ""))?;    
                                        let ireg = num_ptrs.get(int_ptr_count+1).unwrap();
                                        writeln!(f, "   mov {}, {}",ireg.to_string(), s.Data.len())?;    
                                        int_ptr_count += 1;
                                    }
                                    ProgramStringType::CSTR => {
                                        writeln!(f, "   lea {}, [rel _STRING_{}_]",ireg.to_string(), UUID.to_string().replace("-", ""))?;    
                                    }
                                }
                            }
                            else {
                                stack_space_taken += osize;
                                *stack_size += osize;
                                writeln!(f, "   sub rsp, {}",osize)?;
                                writeln!(f, "   lea rax, [rel _STRING_{}_]",UUID.to_string().replace("-", ""))?;
                                writeln!(f, "   mov {} [rsp], rax",size_to_nasm_type(osize))?;
                                int_ptr_count += 1;
                            }
                            int_ptr_count += 1;
                        }
                    },
                    RawConstValueType::PTR(_,val) => {                        
                        let osize = (program.architecture.bits/8) as usize;
                        if program.architecture.options.argumentPassing == ArcPassType::PUSHALL {
                            stack_space_taken += osize;
                            *stack_size += osize;
                            writeln!(f, "   sub rsp, {}",osize)?;
                            writeln!(f, "   mov rax, {}",val)?;
                            writeln!(f, "   mov qword [rsp], rax")?;
                            int_ptr_count += 1;
                        }
                        else {
                            let ops = program.architecture.options.argumentPassing.custom_get().unwrap();
                            if let Some(num_ptrs) = &ops.nums_ptrs {
                                let ireg = num_ptrs.get(int_ptr_count).unwrap();
                                writeln!(f, "   mov {}, {}",ireg.to_string(), val)?;    
                            }
                            else {
                                stack_space_taken += osize;
                                *stack_size += osize;
                                writeln!(f, "   sub rsp, {}",osize)?;
                                writeln!(f, "   mov rax, {}",val)?;
                                writeln!(f, "   mov qword [rsp], rax")?;
                            }
                            int_ptr_count += 1;
                        }
                    },
                }

            },
        }
    }
    Ok(())
}
fn nasm_x86_64_load_args(f: &mut File, scope: &TCScopeType, build: &BuildProgram, program: &CmdProgram) -> io::Result<usize> {
    let mut offset = 0;
    if program.architecture.options.argumentPassing == ArcPassType::PUSHALL {                    
        for (_,iarg) in scope.get_contract(build).unwrap().Inputs.iter() {
            let osize = iarg.get_size(program);
            let reg = Register::RAX.to_byte_size(osize);
            if offset+osize > 0 {
                writeln!(f, "   mov {}, {} [rsp+{}]",reg.to_string(),size_to_nasm_type(osize),offset)?;
                writeln!(f, "   mov {} [rsp-{}], {}",size_to_nasm_type(osize),offset+osize,reg.to_string())?;
            }
            else {
                writeln!(f, "   mov {}, {} [rsp]",reg.to_string(),size_to_nasm_type(osize))?;
                writeln!(f, "   mov {} [rsp-{}], {}",size_to_nasm_type(osize),osize,reg.to_string())?;
            }
            offset += osize;
        }
        todo!("Fix this^");
    }
    else {
        let mut int_ptr_args: usize = 0;
        let argsPassing = program.architecture.options.argumentPassing.custom_get().unwrap();
        let mut offset_from_sbegin: usize = 0;
        for (_,iarg) in scope.get_contract(build).unwrap().Inputs.iter() {
            let osize = iarg.get_size(program);
            if let Some(reg) = argsPassing.nums_ptrs.as_ref().unwrap_or(&Vec::new()).get(int_ptr_args){
                let reg = reg.to_byte_size(osize);
                if offset+osize > 0 { 
                    writeln!(f, "   mov {} [rsp-{}], {}",size_to_nasm_type(osize),offset+osize,reg.to_string())?;
                }
                else {
                    writeln!(f, "   mov {} [rsp-{}], {}",size_to_nasm_type(osize),osize,reg.to_string())?;
                }
                offset += reg.size();
            }
            else {
                let reg = Register::RAX.to_byte_size(osize);
                if offset > 0 {
                    if offset_from_sbegin > 0 {
                        writeln!(f, "   mov {}, {} [rsp+{}]",reg.to_string(),size_to_nasm_type(osize),offset_from_sbegin)?;
                    }
                    else {
                        writeln!(f, "   mov {}, {} [rsp]",reg.to_string(),size_to_nasm_type(osize))?;
                    }
                    writeln!(f, "   mov {} [rsp-{}], {}",size_to_nasm_type(osize),offset,reg.to_string())?;
                }
                else {
                    if offset_from_sbegin > 0 {
                        writeln!(f, "   mov {}, {} [rsp+{}]",reg.to_string(),size_to_nasm_type(osize),offset_from_sbegin)?;
                    }
                    else {
                        writeln!(f, "   mov {}, {} [rsp]",reg.to_string(),size_to_nasm_type(osize))?;
                    }
                    writeln!(f, "   mov {} [rsp], {}",size_to_nasm_type(osize),reg.to_string())?;
                }
                offset_from_sbegin+=osize;
                offset+=osize;
            }
            int_ptr_args+=1
        }
        // if offset > 0 {
        //     writeln!(f, "   sub rsp, {}",offset)?;
        // }
    }
    Ok(offset)
}
fn nasm_x86_64_handle_scope(f: &mut File, build: &BuildProgram, program: &CmdProgram, scope: TCScopeType, mut local_vars: HashMap<String, LocalVariable>, mut stack_size: usize) -> io::Result<()> {
    let expect_loc_t = LinkedHashMap::new();
    let contract_loc_t = FunctionContract { Inputs: LinkedHashMap::new(), Outputs: Vec::new()};
    let expect_locals = scope.get_locals(build).unwrap_or(&expect_loc_t);
    let contract = scope.get_contract(build).unwrap_or(&contract_loc_t);
    let stack_size_org: usize = stack_size;
    local_vars.reserve(contract.Inputs.len()+expect_locals.len());
    if scope.has_contract() {
        nasm_x86_64_load_args(f, &scope, build, program)?;
    }
    for (name, val) in expect_locals.iter() {
        stack_size += val.get_size(program);
    
        local_vars.insert(name.clone(), LocalVariable { typ: val.clone(), operand: stack_size });
    }
    let dif = stack_size-stack_size_org;
    let additional = dif%8;
    let dif = dif + additional;
    stack_size+=additional;
    if dif > 0 {
        writeln!(f, "   sub rsp, {}",dif)?;
    }
    for (loc,inst) in scope.get_body(build) {
        match inst {
            Instruction::MOV(Op, Op2) => {
                match Op {
                    OfP::REGISTER(Reg1) => {
                        match Op2 {
                            OfP::REGISTER(Reg2) => {
                                writeln!(f, "   mov {}, {}",Reg1.to_string(), Reg2.to_string())?;
                            },
                            OfP::LOCALVAR(_) => todo!(),
                            OfP::CONST(val) => {
                                match val {
                                    RawConstValueType::INT(Data) => {
                                        writeln!(f, "   mov {}, {}",Reg1.to_string(), Data)?;
                                    },
                                    RawConstValueType::LONG(Data) => {
                                        writeln!(f, "   mov {}, {}",Reg1.to_string(), Data)?;
                                    },
                                    RawConstValueType::STR(_) => todo!(),
                                    RawConstValueType::PTR(_,_) => todo!(),
                                }
                            }
                            // OfP::PARAM(data) => {
                            //     //let i = com_expect!(loc, scope.get_contract(build).unwrap().Inputs.get(data),"Error: Unexpected variable {}",data).clone();
                            //     let osize = local_vars.get(data).unwrap().operand;
                            //     let offset = stack_size - osize;
                            //     if offset > 0 {
                            //         writeln!(f, "   mov {}, {} [rsp+{}]",Reg1.to_string(),size_to_nasm_type(Reg1.size()),offset)?;
                            //     }
                            //     else {
                            //         writeln!(f, "   mov {}, {} [rsp]",Reg1.to_string(),size_to_nasm_type(Reg1.size()))?;
                            //     }
                            // },
                        }
                    }
                    OfP::LOCALVAR(varOrg) => {
                        match Op2 {
                            OfP::REGISTER(val) => {
                                let var = com_expect!(loc,local_vars.get(varOrg),"Error: Unknown variable found during compilation {}",varOrg);
                                let osize = var.typ.get_size(program);
                                if osize <= 8  {
                                    if stack_size-var.operand == 0 {
                                        writeln!(f, "   mov {} [rsp], {}",size_to_nasm_type(var.typ.get_size(program)), val.to_string())?;
                                    }
                                    else {
                                        writeln!(f, "   mov {} [rsp+{}], {}",size_to_nasm_type(var.typ.get_size(program)),stack_size-var.operand, val.to_string())?;
                                    }
                                }
                                else {
                                    todo!()
                                }

                            },
                            OfP::LOCALVAR(varSet) => {
                                let var1 = com_expect!(loc,local_vars.get(varOrg),"Error: Unknown variable found during compilation {}",varOrg);
                                let var2 = com_expect!(loc,local_vars.get(varSet),"Error: Unknown variable found during compilation {}",varOrg);
                                com_assert!(loc, var1.typ.get_size(program) == var2.typ.get_size(program), "Error: Can not set differently sized variabled together");
                                let osize = var1.typ.get_size(program);
                                let oreg = Register::RAX.to_byte_size(osize);
                                if osize <= 8  {
                                    if stack_size-var2.operand == 0 {
                                        writeln!(f, "   mov {}, {} [rsp]", oreg.to_string(),size_to_nasm_type(var1.typ.get_size(program)))?;
                                    }
                                    else {
                                        writeln!(f, "   mov {}, {} [rsp+{}]",oreg.to_string(),size_to_nasm_type(var1.typ.get_size(program)),stack_size-var2.operand)?;
                                    }
                                }
                                else {
                                    todo!()
                                }
                                if osize <= 8  {
                                    if stack_size-var1.operand == 0 {
                                        writeln!(f, "   mov {} [rsp], {}",size_to_nasm_type(var1.typ.get_size(program)), oreg.to_string())?;
                                    }
                                    else {
                                        writeln!(f, "   mov {} [rsp+{}], {}",size_to_nasm_type(var1.typ.get_size(program)),stack_size-var1.operand, oreg.to_string())?;
                                    }
                                }
                                else {
                                    todo!()
                                }
                            },
                            OfP::CONST(val) => {
                                match val {
                                    RawConstValueType::INT(val) => {
                                        let var = com_expect!(loc,local_vars.get(varOrg),"Error: Unknown variable found during compilation {}",varOrg);
                                        if var.typ.get_size(program) <= 8  {
                                            if stack_size-var.operand == 0 {
                                                writeln!(f, "   mov {} [rsp], {}",size_to_nasm_type(var.typ.get_size(program)), val)?;
                                            }
                                            else {
                                                writeln!(f, "   mov {} [rsp+{}], {}",size_to_nasm_type(var.typ.get_size(program)),stack_size-var.operand, val)?;
                                            }
                                        }
                                        else {
                                            todo!()
                                        }
                                    },
                                    RawConstValueType::LONG(val) => {
                                        let var = com_expect!(loc,local_vars.get(varOrg),"Error: Unknown variable found during compilation {}",varOrg);
                                        if var.typ.get_size(program) <= 8  {
                                            if stack_size-var.operand == 0 {
                                                writeln!(f, "   mov {} [rsp], {}",size_to_nasm_type(var.typ.get_size(program)), val)?;
                                            }
                                            else {
                                                writeln!(f, "   mov {} [rsp+{}], {}",size_to_nasm_type(var.typ.get_size(program)),stack_size -var.operand, val)?;
                                            }
                                        }
                                        else {
                                            todo!()
                                        }
                                    },
                                    RawConstValueType::STR(UUID) => {
                                        let strdef = build.stringdefs.get(UUID).unwrap();
                                        com_assert!(loc,strdef.Typ == ProgramStringType::CSTR,"TODO: Cannot assign types of string different from cstr yet!");
                                        writeln!(f, "   lea rax, [_STRING_{}_]",UUID.to_string().replace("-", ""))?;
                                        let var = com_expect!(loc,local_vars.get(varOrg),"Error: Unknown variable found during compilation {}",varOrg);
                                        //TODO: there are a lot of problems with these hardcoded rax stuff
                                        if stack_size-var.operand == 0 {
                                            writeln!(f, "   mov {} [rsp], rax",size_to_nasm_type(var.typ.get_size(program)))?;
                                        }
                                        else {
                                            writeln!(f, "   mov {} [rsp+{}], rax",size_to_nasm_type(var.typ.get_size(program)),stack_size -var.operand)?;
                                        }
                                    },
                                    RawConstValueType::PTR(_,_) => todo!(),
                                }
                            }
                            // OfP::PARAM(param) => {
                            //     let var = com_expect!(loc,local_vars.get(varOrg),"Error: Unknown variable found during compilation {}",varOrg);
                            //     let osize = var.typ.get_size(program);
                            //     let offset = stack_size - var.operand;
                            //     let reg = Register::RAX.to_byte_size(osize);
                            //     writeln!(f, "   mov {}, {} [rsp+{}]",reg.to_string(),size_to_nasm_type(osize),offset)?;
                            //     if stack_size-var.operand == 0 {
                            //         writeln!(f, "   mov {} [rsp], {}",size_to_nasm_type(osize), reg.to_string())?;
                            //     }
                            //     else {
                            //         writeln!(f, "   mov {} [rsp+{}], {}",size_to_nasm_type(var.typ.get_size(program)),stack_size-var.operand, reg.to_string())?;
                            //     }
                            // },
                        }
                    }
                    _ => {
                        todo!("Unsupported");
                    }
                }
                
            }
            Instruction::CALLRAW(Word, contract) => {
                nasm_x86_64_prep_args(program, build, f, build.get_contract_of_symbol(Word).expect("TODO: implement rawcall without contract").clone(), contract, &mut stack_size, loc.clone(), &local_vars)?;
                writeln!(f, "   xor rax, rax")?;
                if let Some(external) = build.externals.get(Word) {
                    writeln!(f, "   call {}{}{}",external.typ.prefix(program),Word,external.typ.suffix())?;
                }
                else {
                    writeln!(f, "   call {}",Word)?;
                }
                if let Some(ops) = program.architecture.options.argumentPassing.custom_get() {
                    if ops.shadow_space > 0 {
                        writeln!(f, "   add rsp, {}",ops.shadow_space)?;
                    }
                }
            }
            Instruction::ADD(op1, op2) => {
                match op1 {
                    OfP::REGISTER(reg1) => {
                        match op2 {
                            OfP::REGISTER(reg2) => {
                                assert!(reg1.size() == reg2.size(), "Two different sized registers passed to add Instruction");
                                writeln!(f, "   add {}, {}",reg1.to_string(), reg2.to_string())?;
                            }
                            OfP::LOCALVAR(_) => todo!(),
                            OfP::CONST(val) => {
                                if val.get_num_data() != 0 {
                                    writeln!(f, "   add {}, {}",reg1.to_string(),val.get_num_data())?;
                                }
                            }
                        }
                    }
                    OfP::LOCALVAR(var1) => {
                        match op2 {
                            OfP::REGISTER(_) => todo!(),
                            OfP::LOCALVAR(var2) => {
                                com_assert!(loc, local_vars.contains_key(var1) && local_vars.contains_key(var2), "Unknown variable");
                                let nvar1 = var1;
                                let nvar2 = var2;
                                let var1 = local_vars.get(var1).unwrap();
                                let var2 = local_vars.get(var2).unwrap();
                                println!("Local_vars: {:?}\nVar1({}): {:?}\nVar2({}): {:?}",local_vars,nvar1,var1,nvar2,var2);
                                com_assert!(loc, var1.typ.get_size(program) == var2.typ.get_size(program), "Unknown variable size");
                                //let osize = var1.typ.get_size(program);
                                if stack_size-var1.operand == 0 {
                                    writeln!(f, "   mov rax, qword [rsp]")?;
                                }
                                else {
                                    writeln!(f, "   mov rax, qword [rsp+{}]",stack_size-var1.operand)?;
                                }
                                if stack_size-var2.operand == 0{
                                    writeln!(f, "   add rax, qword [rsp]")?;  
                                }
                                else {
                                    writeln!(f, "   add rax, qword [rsp+{}]", stack_size-var2.operand)?;  
                                }
                                //let osize = var2.typ.get_size(program);
                                if stack_size-var1.operand == 0 {
                                    writeln!(f, "   mov qword [rsp], rax")?;
                                }
                                else {
                                    writeln!(f, "   mov qword [rsp+{}], rax",stack_size-var1.operand)?;
                                }
                            },
                            OfP::CONST(_) => todo!(),
                        }
                    },
                    
                    OfP::CONST(_) => todo!(),
                    
                }
                
            }
            Instruction::SUB(op1, op2) => {
                match op1 {
                    OfP::REGISTER(reg1) => {
                        match op2 {
                            OfP::REGISTER(reg2) => {
                                assert!(reg1.size() == reg2.size(), "Two different sized registers passed to sub Instruction");
                                writeln!(f, "   sub {} {}, {}",size_to_nasm_type(reg1.size()),reg1.to_string(), reg2.to_string())?;
                            }
                            OfP::LOCALVAR(_) => todo!(),
                            OfP::CONST(val) => {
                                writeln!(f, "   sub {}, {}",reg1.to_string(),val.get_num_data() )?;
                            }
                  
                        }
                    }
                    OfP::LOCALVAR(_) => todo!(),
                  
                    OfP::CONST(_) => todo!(),
                }
            }
            Instruction::MUL(op1, op2) => {
                match op1 {
                    OfP::REGISTER(reg1) => {
                        match op2 {
                            OfP::REGISTER(reg2) => {
                                assert!(reg1.size() == reg2.size(), "Two different sized registers passed to mul Instruction");
                                writeln!(f, "   mul {} {}, {}",size_to_nasm_type(reg1.size()),reg1.to_string(), reg2.to_string())?;
                            }
                            OfP::LOCALVAR(_) => todo!(),
                            OfP::CONST(val) => {
                                writeln!(f, "   mul {}, {}",reg1.to_string(),val.get_num_data())?;
                            }
                        
                        }
                    }
                    OfP::LOCALVAR(_) => todo!(),
                    OfP::CONST(_) => todo!(),
                }
                
            }
            Instruction::DIV(op1, op2) => {
                match op1 {
                    OfP::REGISTER(reg1) => {
                        match op2 {
                            OfP::REGISTER(reg2) => {
                                assert!(reg1.size() == reg2.size(), "Two different sized registers passed to div Instruction");
                                writeln!(f, "   mov r9, rdx")?;
                                writeln!(f, "   xor rdx, rdx")?;
                                writeln!(f, "   cqo")?;
                                writeln!(f, "   mov rax, {}",reg1.to_string())?;
                                writeln!(f, "   idiv {}",reg2.to_string())?;
                                writeln!(f, "   mov {}, rax",reg1.to_string())?;
                                writeln!(f, "   mov {}, rdx",reg2.to_string())?;
                                writeln!(f, "   mov rdx, r9")?;
                            }
                            OfP::LOCALVAR(_) => todo!(),
                   
                            OfP::CONST(_) => todo!(),
                        }
                    }
                    OfP::LOCALVAR(_) => todo!(),
                   
                    OfP::CONST(_) => todo!(),
                }
                
            }
            Instruction::CALL(Func,args) => {
                nasm_x86_64_prep_args(program, build, f, build.functions.get(Func).unwrap().contract.to_any_contract(), args, &mut stack_size, loc.clone(), &local_vars)?;
                writeln!(f, "   call {}{}",program.architecture.func_prefix,Func)?;
                if let Some(ops) = program.architecture.options.argumentPassing.custom_get() {
                    if ops.shadow_space > 0 {
                        writeln!(f, "   add rsp, {}",ops.shadow_space)?;
                    }
                }
            }
            Instruction::FNBEGIN() => {
                
                
            }
            Instruction::RET() => {
                let dif = stack_size-stack_size_org;
                writeln!(f, "   add rsp, {}",dif)?;
                writeln!(f, "   ret")?;
            }
            //TOOD: Potentially remove these
            Instruction::SCOPEBEGIN | Instruction::SCOPEEND => {}
            Instruction::DEFVAR(_) => {
                // let var = local_vars.get_mut(name).expect("Error: unknown defvar definition in function! This is most likely due to a bug inside the compiler! Make sure to contact the developer if you encounter this!");
                // var.operand     = callstack_size as usize;
                // callstack_size += var.typ.get_size(program) as i64;

                // writeln!(f, "   mov rax, [_CALLSTACK_BUF_PTR]")?;
                // writeln!(f, "   sub rax, {}",var.typ.get_size(program))?;
                // writeln!(f, "   mov [_CALLSTACK_BUF_PTR], rax")?;
            },
            Instruction::INTERRUPT(val) => {
                writeln!(f, "   int 0x{:x}",val)?;
            },
            Instruction::RSPUSH(val) => {
                match val {
                    OfP::REGISTER(_) => {
                        // let offset = rs_stack_offset as i64+stack_size;
                        // rs_stack_offset -= reg.size();
                        // writeln!(f, "   mov {} [rsp+{}], {}",size_to_nasm_type(reg.size()),offset,reg.to_string())?;
                        todo!("Unreachable")
                    },
                   
                    OfP::LOCALVAR(_) => todo!(),
                    OfP::CONST(_) => todo!(),
                }
            },
            Instruction::EXPAND_SCOPE(s) => nasm_x86_64_handle_scope(f, build, program, TCScopeType::NORMAL(s),local_vars.clone(),stack_size)?,
            Instruction::EXPAND_IF_SCOPE(_) => todo!("EXPAND_IF_SCOPE"),
            Instruction::EXPAND_ELSE_SCOPE(_) => todo!("EXPAND_ELSE_SCOPE"),

            Instruction::MORETHAN(ofp1, ofp2) => {
                let regs = ofp1.LOIRGNasm(vec![Register::RAX], f, program, build, &local_vars, stack_size,loc)?[0];
                let regs2 = ofp2.LOIRGNasm(vec![Register::RBX], f, program, build, &local_vars, stack_size,loc)?[0];
                writeln!(f,"   cmp {}, {}",regs.to_string(),regs2.to_string())?;
                writeln!(f,"   setg al")?;
            },
            Instruction::LESSTHAN(ofp1, ofp2) => {
                let regs = ofp1.LOIRGNasm(vec![Register::RAX], f, program, build, &local_vars, stack_size,loc)?[0];
                let regs2 = ofp2.LOIRGNasm(vec![Register::RBX], f, program, build, &local_vars, stack_size,loc)?[0];
                writeln!(f,"   cmp {}, {}",regs.to_string(),regs2.to_string())?;
                writeln!(f,"   setl al")?;
            },
            Instruction::MORETHANEQUALS(ofp1, ofp2) => {
                let regs = ofp1.LOIRGNasm(vec![Register::RAX], f, program, build, &local_vars, stack_size,loc)?[0];
                let regs2 = ofp2.LOIRGNasm(vec![Register::RBX], f, program, build, &local_vars, stack_size,loc)?[0];
                writeln!(f,"   cmp {}, {}",regs.to_string(),regs2.to_string())?;
                writeln!(f,"   setge al")?;
            },
            Instruction::LESSTHANEQUALS(ofp1, ofp2) => {
                let regs = ofp1.LOIRGNasm(vec![Register::RAX], f, program, build, &local_vars, stack_size,loc)?[0];
                let regs2 = ofp2.LOIRGNasm(vec![Register::RBX], f, program, build, &local_vars, stack_size,loc)?[0];
                writeln!(f,"   cmp {}, {}",regs.to_string(),regs2.to_string())?;
                writeln!(f,"   setle al")?;
            },
            Instruction::NOTEQUALS(ofp1, ofp2) => {
                let regs = ofp1.LOIRGNasm(vec![Register::RAX], f, program, build, &local_vars, stack_size,loc)?[0];
                let regs2 = ofp2.LOIRGNasm(vec![Register::RBX], f, program, build, &local_vars, stack_size,loc)?[0];
                writeln!(f,"   cmp {}, {}",regs.to_string(),regs2.to_string())?;
                writeln!(f,"   setne al")?;
            },
            Instruction::EQUALS(ofp1, ofp2) => {
                let regs = ofp1.LOIRGNasm(vec![Register::RAX], f, program, build, &local_vars, stack_size,loc)?[0];
                let regs2 = ofp2.LOIRGNasm(vec![Register::RBX], f, program, build, &local_vars, stack_size,loc)?[0];
                writeln!(f,"   cmp {}, {}",regs.to_string(),regs2.to_string())?;
                writeln!(f,"   sete al")?;
            },
        }
    }
    if !scope.is_func() {
        // let mut varSize: usize = 0;
        // for val in scope.get_locals(build).unwrap().values() {
        //     varSize += val.get_size(program);
        // };
        // if varSize > 0 {
        //     writeln!(f, "   add rsp, {}",varSize)?;
        // }
        let dif = stack_size-stack_size_org;
        writeln!(f, "   add rsp, {}",dif)?;
    }
    Ok(())
}
fn to_nasm_x86_64(build: &mut BuildProgram, program: &CmdProgram) -> io::Result<()>{
    let optimization = optimization_ops(build, program);
    let mut f = File::create(&program.opath).expect(&format!("Error: could not open output file {}",program.opath.as_str()));
    writeln!(&mut f,"BITS 64")?;
    writeln!(&mut f,"default rel")?;
    writeln!(&mut f, "section .data")?;
    
    
    for (UUID,stridef) in build.stringdefs.iter(){                
        if program.in_mode == OptimizationMode::DEBUG || (optimization.usedStrings.contains_key(UUID) && (optimization.usedFuncs.contains(optimization.usedStrings.get(UUID).unwrap()) || !program.remove_unused_functions || optimization.usedStrings.get(UUID).expect(&format!("Could not find: {}",UUID)) == "main")) {
            write!(&mut f, "   _STRING_{}_: db ",UUID.to_string().replace("-", ""))?;
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
                ProgramStringType::STR  => {},//writeln!(&mut f, "   _LEN_STRING_{}_: dq {}",UUID.to_string().replace("-", ""),stridef.Data.len())?,
                ProgramStringType::CSTR => {},
            }
        }
        else if program.print_unused_warns && program.print_unused_strings {
            println!("[NOTE] Unused string:   <{}> \"{}\" - This is probably due to a constant definition that was never used or a function that may have used that strings, that got cut off from the final build",UUID, stridef.Data.escape_default());
            for (cd_name, cd) in build.constdefs.iter() {
                match cd.typ {
                    RawConstValueType::STR(ref id) => {
                        if id == UUID {
                            println!("       ^ Found matching constant definition: \"{}\" at {}",cd_name,cd.loc.loc_display());
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    for (dll_import_name,_) in build.dll_imports.iter() {
        writeln!(&mut f, "extern {}",dll_import_name)?;
        //writeln!(&mut f, "import {} {}",dll_import_name,dll_import.from)?;
    }
    for (dll_export_name,_dll_export) in build.dll_exports.iter() {
        writeln!(&mut f, "global {}",dll_export_name)?;
    }
    for function_name in build.functions.keys() {
        if function_name == "main" {    
            writeln!(&mut f,"global {}{}",program.architecture.func_prefix,function_name)?;
        }
        else {
            // if program.in_mode == OptimizationMode::DEBUG || !program.remove_unused_functions || optimization.usedFuncs.contains(function_name){
            //     writeln!(&mut f,"global {}{}",program.architecture.func_prefix,function_name)?;
            // }
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
                else if program.print_unused_warns && program.print_unused_externs {
                    println!("[NOTE] {}: Unused external: \"{}\"",exter.loc.loc_display(),Word);
                } 
            },
        }
    }
    writeln!(&mut f, "section .text")?;

    for (function_name,function) in build.functions.iter() {
        // TODO: implement build.architecture.entry
        if program.in_mode != OptimizationMode::DEBUG && program.remove_unused_functions && !optimization.usedFuncs.contains(function_name) && function_name != "main" {
            continue;
        }
        
        if function_name == "main" {
            writeln!(&mut f, "{}{}:",program.architecture.func_prefix,function_name)?;
            writeln!(&mut f, "   sub rsp, {}",program.architecture.bits/8)?;
        }
        else {
            writeln!(&mut f, "{}{}:",program.architecture.func_prefix,function_name)?;
        }
        nasm_x86_64_handle_scope(&mut f, build, program, TCScopeType::FUNCTION(function_name.clone()),HashMap::new(),0)?;
        if function_name == "main" {
            let mut argsize: usize = 0;
            for (_,local) in function.locals.iter() {
                argsize += local.get_size(program);
            }
            argsize += argsize%8;
            writeln!(&mut f, "   add rsp, {}",8+argsize)?;
            writeln!(&mut f, "   xor rax,rax")?;
            writeln!(&mut f, "   ret")?;
        }
    }
    Ok(())

}


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
}
fn type_check_scope(build: &BuildProgram, program: &CmdProgram, scope: TCScopeType, currentLocals: &mut Vec<Locals>) {
    let mut rs_stack: Vec<VarType> = Vec::new();
    if let Some(locals) = scope.get_locals(build) {
        currentLocals.push(locals.clone());
    }
    // TODO: Add current locals (&Vec<Locals>)
    for (loc, instruction) in scope.get_body(build).iter() {
        match instruction {
            Instruction::DEFVAR(_)           => {},
            Instruction::MOV(_, _)           => {},
            Instruction::CALLRAW(name,contract)        => {
                let mut externContract = build.get_contract_of_symbol(name).unwrap_or(&AnyContract { InputPool: vec![], Outputs: vec![] }).clone();//build.externals.get(name).unwrap().contract.as_ref().unwrap_or(&AnyContract { InputPool: vec![], Outputs: vec![] }).clone();
                externContract.InputPool.reverse();
                
                typ_assert!(loc, externContract.InputPool.len() == contract.len(), "Error: Expected: {} amount of arguments but found {}",externContract.InputPool.len(), contract.len());
                for arg in contract {
                    match &arg.typ {
                        CallArgType::LOCALVAR(name) => {
                            let etyp = typ_expect!(loc, externContract.InputPool.pop(), "Error: Additional arguments provided for external that doesn't take in any more arguments!");
                            let local = get_local(currentLocals, name).unwrap();
                            typ_assert!(loc,etyp.weak_eq(&local),"Error: Incompatible types for contract\nExpected: {}\nFound: ({}) {}",etyp.to_string(false),name,local.to_string(false));
                        },
                        CallArgType::REGISTER(_) => todo!("Registers are still yet unhandled!"),
                        CallArgType::CONSTANT(Const) => {
                            let typs = Const.to_type(build);
                            for typ in typs {
                                let etyp = typ_expect!(loc, externContract.InputPool.pop(), "Error: Additional arguments provided for external that doesn't take in any more arguments!\nExpected: Nothing\nFound: {}\n",typ.to_string(false));
                                typ_assert!(loc,etyp.weak_eq(&typ),"Error: Incompatible types for contract\nExpected: {}\nFound: {}",etyp.to_string(false),typ.to_string(false));
                            }
                        },
                    }
                }
            },
            Instruction::ADD(_, _)           => {},
            Instruction::SUB(_, _)           => {},
            Instruction::MUL(_, _)           => {},
            Instruction::DIV(_, _)           => {},
            
            Instruction::CALL(funcn, args)             => {
                let function = typ_expect!(loc, build.functions.get(funcn), "Error: unknown function call to {}, Function may not exist!",funcn);
                let mut functionIP = function.contract.Inputs.clone();
                
                for arg in args {
                    match &arg.typ {
                        CallArgType::LOCALVAR(name) => {
                            let (_,etyp) = typ_expect!(loc, functionIP.pop_back(), "Error: Additional arguments provided for external that doesn't take in any more arguments!");
                            
                            let local =get_local(currentLocals, name).unwrap();
                            typ_assert!(loc,etyp.weak_eq(&local),"Error: Incompatible types for contract\nExpected: {}\nFound: ({}) {}",etyp.to_string(false),name,local.to_string(false));
                        },
                        CallArgType::REGISTER(_) => todo!("Registers are still yet unhandled!"),
                        CallArgType::CONSTANT(Const) => {
                            let typs = Const.to_type(build);
                            for typ in typs {
                                let (_,etyp) = typ_expect!(loc, functionIP.pop_back(), "Error: Additional arguments provided for external that doesn't take in any more arguments!\nExpected: Nothing\nFound: {}\n",typ.to_string(false));
                                typ_assert!(loc,etyp.weak_eq(&typ),"Error: Incompatible types for contract\nExpected: {}\nFound: {}",etyp.to_string(false),typ.to_string(false));
                            }
                        },
                    }
                }
            },
            Instruction::FNBEGIN()           => {},
            Instruction::RET()               => {},
            Instruction::SCOPEBEGIN          => {},
            Instruction::SCOPEEND            => {},
            Instruction::INTERRUPT(_)        => {},
            Instruction::RSPUSH(typ)           => {
                match typ {
                    OfP::REGISTER(Reg) => {
                        rs_stack.push(Reg.to_var_type());
                    },
                    OfP::LOCALVAR(_) => todo!(),
                    OfP::CONST(val) => {
                        rs_stack.extend(val.to_type(build))
                    }
                }
            },
            Instruction::EXPAND_SCOPE(s)      => type_check_scope(build, program, TCScopeType::NORMAL(s), currentLocals),
            Instruction::EXPAND_IF_SCOPE(s)   => type_check_scope(build, program, TCScopeType::NORMAL(s), currentLocals),
            Instruction::EXPAND_ELSE_SCOPE(s) => type_check_scope(build, program, TCScopeType::NORMAL(s), currentLocals),
            //TODO: add typechecking for this:
            Instruction::MORETHAN(_, _)       => {},
            Instruction::LESSTHAN(_, _)       => {},
            Instruction::MORETHANEQUALS(_, _) => {},
            Instruction::LESSTHANEQUALS(_, _) => {},
            Instruction::NOTEQUALS(_, _)      => {},
            Instruction::EQUALS(_, _)         => {},
        }
    }
    if scope.is_func() {
        if rs_stack != scope.get_contract(build).unwrap().Outputs {
            typ_warn!(scope.get_location(build).unwrap(),"Error: Mismatched types for output");
            for typ in rs_stack.iter() {
                eprintln!("   {}",typ.to_string(false).to_uppercase());
            }

            eprintln!("Expected: ");
            for typ in scope.get_contract(build).unwrap().Outputs.iter() {
                eprintln!("   {}",typ.to_string(false).to_uppercase());
            }
            exit(1)
        }
    }
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
    list_targets(9);
    println!("     flags: ");
    println!("         -t (target)                         -> compiles to the given target (default is nasm_x86_64)");
    println!("         -o (output path)                    -> outputs to that file (example: hello.asm in nasm_x86_64 mode). If the output path is not specified it defaults to the modes default (for nasm_x86_64 thats a.asm)");
    println!("         -r                                  -> runs the program for you if the option is available for that language mode (for example in nasm_x86_64 it calls nasm with gcc to link it to an executeable)");
    println!("         -b                                  -> builds the program for you if the option is available for that language mode");
    println!("         -release                            -> builds the program in release mode");
    println!("         -ntc                                -> (NoTypeChecking) Disable type checking");
    println!("         -warn (all, funcs, externs, strings)-> Enable unused warns for parameter");
    println!("         -ruf                                -> Remove unused functions");
    println!("         -callstack (size)                   -> Set callstack size.\n{}\n{}",
             "                                                The name is very deceiving but callstack is now only used for locals as of 0.0.6A (checkout versions.md)",
             "                                                [NOTE] it is planned for -callstack to be deprecated for instead using the stack as a way to store variables with the new function system");
    println!("         -arc (builtin arc)                  -> builds for a builtin architecture");
    println!("         -arc - (path to custom arc)         -> builds for a custom architecture following the syntax described in ./examples/arcs");
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
    // if args.len() < 2 {
    //     usage(&program_n);
    //     exit(1);
    // }
    let mut program = CmdProgram::new();
    //program.path = args.remove(0);
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
    {
        let mut i: usize = 0;
        while i < args.len(){
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
                "-callstack" => {
                    let val = args.get(i+1).expect("Unexpected flag -callstack, Please specify a size!").parse::<usize>().expect("Value not a valid usize!");
                    program.call_stack_size = val;
                    i += 1;
                }
                "-usage" => {
                    usage(&program_n);
                    exit(0);
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
            i+=1;
        }
    }
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
    Intrinsics.insert("<".to_string(),IntrinsicType::OPENANGLE);
    Intrinsics.insert(">".to_string(),IntrinsicType::CLOSEANGLE);
    Intrinsics.insert(":".to_string(),IntrinsicType::DOUBLE_COLIN);
    Intrinsics.insert(",".to_string(),IntrinsicType::COMA);
    Intrinsics.insert(";".to_string(),IntrinsicType::DOTCOMA);
    Intrinsics.insert("=".to_string(),IntrinsicType::SET);
    Intrinsics.insert("+".to_string(),IntrinsicType::ADD);
    Intrinsics.insert("-".to_string(),IntrinsicType::SUB);
    Intrinsics.insert("*".to_string(),IntrinsicType::MUL);
    Intrinsics.insert("/".to_string(),IntrinsicType::DIV);
    Intrinsics.insert("return".to_string(),IntrinsicType::RET);
    Intrinsics.insert("if".to_string(), IntrinsicType::IF);
    Intrinsics.insert("else".to_string(), IntrinsicType::ELSE);
    Intrinsics.insert("cast".to_string(), IntrinsicType::CAST);

    Intrinsics.insert("==".to_string(),IntrinsicType::EQUALS);
    Intrinsics.insert("!=".to_string(),IntrinsicType::NOTEQUALS);
    Intrinsics.insert(">".to_string(),IntrinsicType::MORETHAN);
    Intrinsics.insert("<".to_string(),IntrinsicType::LESSTHAN);
    Intrinsics.insert("<=".to_string(),IntrinsicType::LESSTHANEQ);
    Intrinsics.insert(">=".to_string(),IntrinsicType::MORETHANEQ);
    let mut Definitions: HashMap<String,VarType> = HashMap::new();
    Definitions.insert("int".to_string(), VarType::INT);
    Definitions.insert("char".to_string(), VarType::CHAR);
    Definitions.insert("long".to_string(), VarType::LONG);
    Definitions.insert("bool".to_string(), VarType::BOOLEAN);
    Definitions.insert("ptr".to_string(), VarType::PTR(Ptr{ typ: PtrTyp::VOID, inner_ref: 0}));
    Definitions.insert("short".to_string(), VarType::SHORT);
    //assert!(!program.path.is_empty(),"Error: expected program.path but found nothing!");
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
    let mut build = parse_tokens_to_build(&mut lexer, &mut program);    
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
            // todo!("Unimplemented type {}",program.target);
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
- [ ] TODO: Implement if statements as well as else statements
- [ ] TODO: Fix returning from functions
- [ ] TODO: Add 'result' as a part of OfP for calling the function and getting its result
- [ ] TODO: Add more examples like OpenGL examples, native Windows examples with linking to kernel.dll etc.
- [ ] TODO: Add some quality of life things such as __FILE__ __LINE__

- [ ] TODO: Add expressions like a+b*c etc. 
- [ ] TODO: Add EOL (End of line) token


- [/] TODO: Make callraw use reference to UUID and name instead of raw when typechecking
- [/] TODO: Make call use reference to UUID and name instead of raw
*/