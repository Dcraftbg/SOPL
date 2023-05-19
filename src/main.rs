#![allow(non_snake_case)] 
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(unreachable_patterns)]

use std::{env, process::{exit, Command, Stdio}, path::{Path, PathBuf}, ffi::OsStr, str::{FromStr, Chars}, collections::{HashMap, HashSet}, hash::Hash, fs::{File, self}, io::{Read, Write, self}, fmt::format, borrow::{BorrowMut, Borrow}, clone, time::{SystemTime, Instant}, rc::Rc, iter::Peekable, cell::{RefCell, Ref, RefMut}, ops::{Deref, DerefMut}, vec};
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
macro_rules! lpar_error {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(P) [ERROR] {}: {}", $location.loc_display(), message);
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
macro_rules! lpar_assert {
    ($location:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(P) [ERROR] {}: {}", $location.loc_display(), message);
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
macro_rules! com_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(C) [ERROR] {}: {}", $location.loc_display(), message);
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
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(C) [ERROR] {}: {}", $location.loc_display(), message);
            exit(1);
        }
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
#[derive(Debug, Clone)]
struct CmdProgram {
    path: String,
    opath: String,
    typ: String,
    should_build: bool,
    warn_rax_usage: bool,
    use_type_checking: bool,
    print_unused_warns  : bool,
    // Sub sets
    print_unused_funcs  : bool,
    print_unused_externs: bool,
    print_unused_strings: bool,
    

    remove_unused_functions: bool,
    in_mode: OptimizationMode,
    build_architecture: String,
    call_stack_size: usize,
}
impl CmdProgram {
    fn new() -> Self {
        Self { path: String::new(), opath: String::new(), should_build: false, typ: String::new(), warn_rax_usage: true, call_stack_size: 64000, in_mode: OptimizationMode::DEBUG, use_type_checking: true, print_unused_warns: true, build_architecture: String::new(), remove_unused_functions: false, print_unused_funcs: true, print_unused_externs: true, print_unused_strings: true }
    }
}
#[repr(u32)]
#[derive(Clone, Copy,Debug,PartialEq )]

enum IntrinsicType {
    Extern = 0,
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
    // BOOLEAN OPERATIONS
    EQ,
    RET,
    INCLUDE,
    IF,
    ELSE,
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
                if isplural {"Move registers".to_string()} else {"Move register".to_string()}
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
            IntrinsicType::EQ => {
                if isplural {"Equals".to_string()} else {"Equal".to_string()}
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
        }
    }
}
// TODO: implement booleans
#[derive(Debug,PartialEq,Clone)]
enum TokenType {
    WordType      (String),
    IntrinsicType (IntrinsicType),
    Definition    (VarType),
    StringType    (String),
    CStringType   (String),
    CharType      (String),
    Number32      (i32),
    Number64      (i64)
}

impl TokenType {
    fn to_string(&self,isplural:bool) -> String{
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
        }
    }
}
#[derive(Clone,Debug)]
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
}
struct Lexer<'a> {
    source: Vec<char>,
    cursor: usize,
    currentLocation: ProgramLocation,
    Intrinsics: &'a HashMap<String,IntrinsicType>,
    Definitions: &'a HashMap<String,VarType>
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
    fn new(source: &'a String, Intrinsics: &'a HashMap<String, IntrinsicType>, Definitions: &'a HashMap<String,VarType>) -> Self {
        Self { 
            source: source.chars().collect(), 
            cursor: 0, 
            currentLocation: ProgramLocation { file: Rc::new(String::new()), linenumber: 1, character: 0 },
            Intrinsics,
            Definitions,
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
        if !c.is_alphabetic() {
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
        //println!("Source: {:?}",self.source.as_str());
        self.trim_left();
        let mut outstr: String = Default::default();
        if self.is_not_empty() {
            let mut c = self.cchar();
            //println!("Source after: {:?}",self.source.as_str());
            match c {
                '"' => {
                    let mut shouldIgnoreNext: bool = false;
                    self.cursor += 1;
                    c = self.cchar();
                    // REMEMBER TO UPDATE LOCATION CHAR AFTER
                    /*
                    shouldIgnoreNext:     c != '\"'
                    1                  0              1
                    1                  1              1
                    0                  1              1
                    0                  0              0
                    */
                    while self.is_not_empty() && (shouldIgnoreNext || c != '\"'){
                        c = self.cchar_s()?;
                        //println!("Adding: {}, shouldIgnoreNext: {}",c.escape_default(),shouldIgnoreNext);
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
                    //println!("Output string: \"{}\"",outstr);
                    //println!("Cchar: {}",self.cchar());
                    if self.is_not_empty() {
                        let outstr = unescape(&outstr);
                        //self.cursor += 1;
                        //println!("C: {}",self.cchar());
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
                    todo!("CHARS ARE NOT YET IMPLEMENTED AND SHOULDN'T BE USED!");
                    let mut shouldIgnoreNext: bool = true;
                    self.cursor += 1;
                    self.currentLocation.character += 1;
                    while self.is_not_empty() && (self.source.get(self.cursor).unwrap() != &'\'' && shouldIgnoreNext){
                        let c = self.source.get(self.cursor).unwrap();
                        if shouldIgnoreNext {
                            shouldIgnoreNext = false
                        }
                        if c == &'\\' {
                            shouldIgnoreNext = true
                        }
                        else {
                            outstr.push(self.source.get(self.cursor).unwrap().clone());
                        }
                        self.cursor+=1;
                        self.currentLocation.character += 1;
                    }
                    if outstr.len() > 1 {
                        eprintln!("Error: undefined char type! \'{}\'\n[NOTE] Consider changing to string\n",outstr);
                        exit(1);
                    }
                    return Some(Token { typ: TokenType::CharType(outstr), location: self.currentLocation.clone() });
                }
                '/' => {
                    //println!("Source: {:?}",self.source);
                    //println!("Cursor: {}",self.cursor);
                    self.cursor += 1;
                    if let Some(nc) = self.cchar_s() {
                        //println!("Nc: \'{}\'",nc);
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
                    if c.is_alphabetic() {
                        // outstr.push(c);
                        // self.cursor += 1;
                        // self.currentLocation.character += 1;
                        // while self.is_not_empty() && (self.cchar() != '(' && self.cchar() != ')' && self.cchar() != '[' && self.cchar()  != ']' && self.cchar() != ';') && !self.cchar().is_alphanumeric() && !self.cchar().is_whitespace() {
                        //     outstr.push(self.source.chars().nth(self.cursor).unwrap());
                        //     self.cursor+=1;
                        //     self.currentLocation.character += 1;
                        // }

                        let outstr = self.pop_symbol()?;
                        if let Some(o) = self.Intrinsics.get(&outstr) {
                            return Some(Token { typ: TokenType::IntrinsicType(o.clone()), location: self.currentLocation.clone() });
                        }
                        else if let Some(o) = self.Definitions.get(&outstr){                        
                                return Some(Token { typ: TokenType::Definition(o.clone()), location: self.currentLocation.clone() });
                        }
                        else{
                            return Some(Token { typ: TokenType::WordType(outstr), location: self.currentLocation.clone() });
                        }
                    }
                    else if (c == '-' && self.cchar_offset(1)?.is_numeric()) || c.is_numeric(){
                        if c=='-' {
                            outstr.push(c);
                            self.cursor += 1;
                            c = self.cchar_s()?;
                        }
                        while self.is_not_empty() && c.is_numeric() {
                            c = self.cchar_s()?;
                            //println!("l.is_numeric(): {}",'l'.is_numeric());
                            //println!("Adding char: '{}', self.is_not_empty() && c.is_numeric(): {}",c,self.is_not_empty() && c.is_numeric() );
                            if c == '_' {
                                //println!("having to do _");
                                self.cursor+=1;
                                c = self.cchar_s()?;
                                continue;
                            }
                            self.cursor += 1;
                            outstr.push(c);
                        }
                        //println!("Current outstr: \"{}\"",outstr);
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
                    else if c == ';' || c==')' || c=='(' || c=='{' || c=='}' || c=='[' || c==']' {
                        self.cursor += 1;
                        self.currentLocation.character += 1;
                        //println!("Got here!");
                        return Some(Token {typ: TokenType::IntrinsicType(self.Intrinsics.get(&c.to_string()).expect("Unhandled intrinsic :(").clone()), location: self.currentLocation.clone()});
                    }
                    else {
                        
                        while self.is_not_empty() && !c.is_alphabetic() && !c.is_numeric() && !c.is_whitespace() && c != ';' && c!=')' && c!='(' && c!='{' && c!='}' && c!='[' && c!=']'{
                            c = self.cchar_s()?;
                            self.cursor += 1;
                            outstr.push(c);
                        }
                        outstr.pop();
                        //self.cursor -= 1;
                        //println!("Debug: {}",self.cchar());
                        self.currentLocation.character += outstr.len() as i32;
                        if let Some(intrinsic) = self.Intrinsics.get(&outstr) {
                            return Some(Token { typ: TokenType::IntrinsicType(intrinsic.clone()), location: self.currentLocation.clone() })
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
#[derive(Debug)]
enum ExternalType {
    RawExternal,
    CExternal
}
#[derive(Debug)]
struct External {
    typ: ExternalType,
    loc: ProgramLocation
}
impl ExternalType {
    fn prefix(&self) -> String {
        match self {
            ExternalType::RawExternal => String::new(),
            ExternalType::CExternal => "_".to_string(),
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
    // 16 bit
    AX,
    BX,
    CX,
    DX,
    SP,
    BP,
    // 8 bit low
    AL,
    BL,
    CL,
    DL,
    // 8 bit high
    AH,
    BH,
    CH,
    DH,



    // General Purpose registers
    R8,
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
            Register::R8D => "r8d".to_string()
        }
    }
    fn from_string(stri: &String) -> Option<Self> {
        match stri.as_str() {
             "RAX" | "rax" => Some(Register::RAX), 
             "RBX" | "rbx" => Some(Register::RBX), 
             "RCX" | "rcx" => Some(Register::RCX), 
             "RDX" | "rdx" => Some(Register::RDX), 
             "RSP" | "rsp" => Some(Register::RSP), 
             "RBP" | "rbp" => Some(Register::RBP), 
             "EAX" | "eax" => Some(Register::EAX), 
             "EBX" | "ebx" => Some(Register::EBX), 
             "ECX" | "ecx" => Some(Register::ECX), 
             "EDX" | "edx" => Some(Register::EDX), 
             "ESP" | "esp" => Some(Register::ESP), 
             "EBP" | "ebp" => Some(Register::EBP), 
             "AX"  | "ax"  => Some(Register::AX ), 
             "BX"  | "bx"  => Some(Register::BX ), 
             "CX"  | "cx"  => Some(Register::CX ), 
             "DX"  | "dx"  => Some(Register::DX ), 
             "SP"  | "sp"  => Some(Register::SP ), 
             "BP"  | "bp"  => Some(Register::BP ), 
             "AL"  | "al"  => Some(Register::AL ), 
             "BL"  | "bl"  => Some(Register::BL ), 
             "CL"  | "cl"  => Some(Register::CL ), 
             "DL"  | "dl"  => Some(Register::DL ), 
             "AH"  | "ah"  => Some(Register::AH ), 
             "BH"  | "bh"  => Some(Register::BH ), 
             "CH"  | "ch"  => Some(Register::CH ), 
             "DH"  | "dh"  => Some(Register::DH ), 
             "R8"  | "r8"  => Some(Register::R8),
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
            Register::EAX => 4,
            Register::EBX => 4,
            Register::ECX => 4,
            Register::EDX => 4,
            Register::ESP => 4,
            Register::EBP => 4,
            Register::AX  => 2,
            Register::BX  => 2,
            Register::CX  => 2,
            Register::DX  => 2,
            Register::SP  => 2,
            Register::BP  => 2,
            Register::AL  => 1,
            Register::BL  => 1,
            Register::CL  => 1,
            Register::DL  => 1,
            Register::AH  => 1,
            Register::BH  => 1,
            Register::CH  => 1,
            Register::DH  => 1,
            Register::R8 => 8,
            Register::R8D => 4,
            Register::RSI => 8,
            Register::RDI => 8,
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
                Register::RSP | Register::ESP  | Register::SP => {
                    return Register::RSP;
                }
                Register::RBP | Register::EBP  | Register::BP => {
                    return Register::RBP;
                }
                Register::R8  => todo!(),
                Register::R8D => todo!(),
                Register::RSI => todo!(),
                Register::RDI => todo!(),
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
                    Register::R8 => todo!(),
                    Register::R8D => todo!(),
                    Register::RSI => todo!(),
                    Register::RDI => todo!(),
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
                    Register::R8 => todo!(),
                    Register::R8D => todo!(),
                    Register::RSI => todo!(),
                    Register::RDI => todo!(),
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
                    Register::R8 => todo!(),
                    Register::R8D => todo!(),
                    Register::RSI => todo!(),
                    Register::RDI => todo!(),
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
                //TODO: introduce VarType::PTR64
                VarType::PTR
            },
            Register::EAX | Register::EBX | Register::ECX | Register::EDX => {
                VarType::INT
            },
            Register::ESP | Register::EBP => {
                VarType::PTR
            }
            Register::AX |Register::BX |Register::CX |Register::DX |Register::SP |Register::BP => {
                VarType::SHORT
            }
            Register::AL |Register::BL |Register::CL |Register::DL |Register::AH |Register::BH |Register::CH |Register::DH => {
                VarType::CHAR
            }
            Register::R8 |Register::R8D => {
                VarType::LONG
            },
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



// TODO: Introduce this for ADD, SUB, MUL, DIV
#[derive(Debug)]
enum OfP {
    REGISTER (Register),
    PARAM    (String),
    LOCALVAR (String),
    RAW      (i64),
    STR      (Uuid, ProgramStringType)
    // etc.

}
#[derive(Debug)]
enum Instruction {
    RSPUSH  (OfP),
    PUSH    (OfP),
    DEFVAR  (String),
    MOV     (OfP, OfP),
    POP     (OfP),
    CALLRAW (String),
    ADD     (OfP, OfP),
    SUB     (OfP, OfP),
    MUL     (OfP, OfP),
    DIV     (OfP, OfP),
    EQUALS  (OfP, OfP),
    CALL    (String),
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
}

#[derive(Debug,Clone)]
enum ProgramStringType {
    STR,
    CSTR
}
#[derive(Debug,Clone)]
struct ProgramString {
    Typ:  ProgramStringType,
    Data: String
}
#[derive(Debug,Clone)]
struct LocalVariable {
    typ: VarType,
    operand: usize
}
type Locals = HashMap<String, LocalVariable>;
#[derive(Debug)]
struct Function {
    contract: FunctionContract,
    locals: Locals,
    location: ProgramLocation,
    body: Vec<(ProgramLocation,Instruction)>,
}


#[derive(Debug, Clone)]
enum ConstValueType {
    INT(i32),
    LONG(i64),
    STR(String),
}
struct ConstValue {
    typ: ConstValueType,
    loc: ProgramLocation
}
impl ConstValueType {
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
                    ConstValueType::STR(_nval) => {
                        return Err("Error: Unexpected - operation on int and string".to_string());
                    },
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
                    ConstValueType::STR(_nval) => {
                        return Err("Error: Unexpected * operation on long and string".to_string());
                    },
                }
            }
            ConstValueType::STR(_) => {
                match Other {
                    _ => {
                        return Err("Error: Cannot do * operation on a string".to_string());
                    }
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
                    ConstValueType::STR(_nval) => {
                        return Err("Error: Unexpected - operation on int and string".to_string());
                    },
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
                    ConstValueType::STR(_nval) => {
                        return Err("Error: Unexpected - operation on long and string".to_string());
                    },
                }
            }
            ConstValueType::STR(_) => {
                match Other {
                    _ => {
                        return Err("Error: Cannot do - operation on a string".to_string());
                    }
                }
            },
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
                    ConstValueType::STR(nval) => {
                        return Ok(ConstValueType::STR(val.to_string()+nval));
                    },
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
                    ConstValueType::STR(nval) => {
                        return Ok(ConstValueType::STR(val.to_string()+nval));
                    },
                }
            }
            ConstValueType::STR(val) => {
                match Other {
                    ConstValueType::INT(nval) => {
                        return Ok(ConstValueType::STR(val.clone()+&nval.to_string()));
                    }
                    ConstValueType::LONG(nval) => {
                        return Ok(ConstValueType::STR(val.clone()+&nval.to_string()));
                    }
                    ConstValueType::STR(nval) => {
                        let out = val.clone()+nval;
                        return Ok(ConstValueType::STR(out));
                    },
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
enum RawConstValueType{
    INT(i32),
    LONG(i64),
    STR(Uuid),
}
#[derive(Debug, Clone)]
struct RawConstValue {
    typ: RawConstValueType,
    loc: ProgramLocation,
}
#[derive(Debug)]
struct BuildProgram {
    externals:    HashMap<String,External>,
    functions:    HashMap<String, Function>,
    stringdefs:   HashMap<Uuid,ProgramString>,
    constdefs:    HashMap<String, RawConstValue>
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum VarType {
    CHAR,
    SHORT,
    BOOLEAN,
    INT,
    LONG,
    STR,
    PTR,
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
            VarType::STR => {
                if isplural {"strings".to_string()} else {"string".to_string()}
            }
            VarType::PTR => {
                if isplural {"pointers".to_string()} else {"pointer".to_string()}
            }
            VarType::CUSTOM(_) => {
                todo!("Implement custom")
            }
        }
    }
    fn get_size(&self, program: &CmdProgram) -> usize{
        match self {
            VarType::CHAR => 1,
            VarType::SHORT => 2,
            VarType::BOOLEAN => 1,
            VarType::INT => 4,
            VarType::LONG => 8,
            VarType::STR => 16,
            VarType::PTR => {
                match program.build_architecture.as_str() {
                    "32" => 4,
                    "64" => 8,
                    _    => 4
                }
            },
            VarType::CUSTOM(_) => todo!(),
        }
    }
}
// #[derive(Debug)]
// enum ScopeType {
//     FUNC,
//     IF,
//     EMPTY,
//     ELSE,
// }
// impl ScopeType {
//     fn to_string(&self, isplural: bool) -> &str {
//         if isplural {
//             match self {
//                 ScopeType::FUNC  => "functions",
//                 ScopeType::IF    => "ifs",
//                 ScopeType::EMPTY => "empties",
//                 ScopeType::ELSE  => "elses",
//             }
//         }
//         else {
//             match self {
//                 ScopeType::FUNC  => "function",
//                 ScopeType::IF    => "if",
//                 ScopeType::EMPTY => "empty",
//                 ScopeType::ELSE  => "else",
//             }
//         }
//     }
// }
// #[derive(Debug)]
// struct ScopeOpener {
//     cinstruct_size: usize,
//     hasBeenOpened: bool,
//     typ:   ScopeType
// }
/*
#[derive(Debug)]
struct Function {
    contract: FunctionContract,
    locals: HashMap<String,LocalVariable>,
    location: ProgramLocation,
    body: Vec<(ProgramLocation,Instruction)>,
}
*/
// #[derive(Debug)]
// struct Scope {
//     typ:  ScopeType,
//     inst: InstBodyType,
//     locals: LocalBodyType,
//     contract: Option<FunctionContract>,
//     hasBeenOpened: bool,
//     //funcs: Option<Vec<Function>>,  
// }
type ScopeBody = Vec<(ProgramLocation,Instruction)>;
#[derive(Debug, Clone)]
struct FunctionContract {
    Inputs: Vec<(String, VarType)>,
    Outputs: Vec<VarType>
}
#[derive(Debug)]
enum NormalScopeType {
    IF,
    ELSE,
    EMPTY
}
// impl NormalScopeType {
//     //TODO: Get rid of ScopeType as a whole
//     fn to_scope_type(&self) -> ScopeType {
//         match self {
//             NormalScopeType::IF => ScopeType::IF,
//             NormalScopeType::ELSE => ScopeType::ELSE,
//             NormalScopeType::EMPTY => ScopeType::EMPTY,
//         }
//     }
// }
#[derive(Debug)]
struct NormalScope {
    typ: NormalScopeType,
    body: ScopeBody,
}

#[derive(Debug)]
struct FunctionScope<'a> {
    func: &'a mut Function
    // body: &'a mut ScopeBody,
    // locals: &'a mut Locals,
    // contract: &'a mut FunctionContract
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
            //TODO: Implement local variables for normal scopes
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

fn eval_const_def(lexer: &mut Lexer, build: &mut BuildProgram) -> ConstValue {
    let mut varStack: Vec<ConstValueType> = vec![];
    let orgLoc = lexer.currentLocation.clone();
    while let Some(token) = lexer.next() {
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
                    IntrinsicType::DOTCOMA => {
                        break;
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
                        varStack.push(ConstValueType::STR(build.stringdefs.get(UUID).unwrap().Data.clone()));
                    }
                }
            }
            TokenType::StringType(word) => {
                varStack.push(ConstValueType::STR(word))
            }
            TokenType::Number32(num) => {
                varStack.push(ConstValueType::INT(num));
            }
            TokenType::Number64(num) => {
                varStack.push(ConstValueType::LONG(num));
            }
            _ => {
                par_error!(token,"Unexpected token type in const declaration {}",token.typ.to_string(false));
            }
        }
    }
    
    lpar_assert!(lexer.currentLocation,varStack.len() == 1,"Error: Lazy constant stack handling! You need to correctly handle your constants");
    ConstValue {typ: varStack.pop().unwrap(), loc: orgLoc}
}

fn parse_function_contract(lexer: &mut Lexer) -> FunctionContract {
    let mut out = FunctionContract {Inputs: vec![], Outputs: vec![]};
    let mut is_input = true;
    let first = lexer.next();
    let first = par_expect!(lexer.currentLocation,first,"abruptly ran out of tokens in function contract");
    let mut expectNextSY = false;
    match first.typ {
        TokenType::IntrinsicType(typ) => {
            match typ {
                IntrinsicType::OPENPAREN => {},
                Other => {
                    par_error!(first,"INVALID TOKEN FOR PARSING, Expected an Open paren but found other {}",Other.to_string(false));
                }
            }
        }
        Other => {assert!(false, "INVALID TOKEN FOR PARSING, Expected an Open paren intrinsic but found: {}",Other.to_string(false));}
    }
    while let Some(token) = lexer.next() {
        match token.typ {
            TokenType::IntrinsicType(Typ) => {
                match Typ {
                    IntrinsicType::COMA => {
                        assert!(expectNextSY, "undefined coma placed inside function contract! Comas only seperate Input or Output parameters");
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
                        out.Inputs.push((Word,Def))
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
fn parse_tokens_to_build(lexer: &mut Lexer, program: &mut CmdProgram) -> BuildProgram {
    let mut build: BuildProgram = BuildProgram { externals: HashMap::new(), functions: HashMap::new(),stringdefs: HashMap::new(), constdefs: HashMap::new()};
    let mut scopeStack: ScopeStack = vec![];
    let mut foundFuncs: HashSet<String> = HashSet::new();
    //let mut currentFunction: Option<String> = None;
    //let mut ctime: Instant= Instant::now();
    //let tokens: Vec<Token> = lexer.collect();
    while let Some(token) = lexer.next() {
        //time_func!()
        // let t = Instant::now();
        // println!("From last next: {:?}",t-ctime);
        // ctime = t;
        match token.typ {
            TokenType::WordType(ref word) => {
                par_assert!(token,scopeStack.len() > 0, "Undefined Word Call outside of entry point! '{}'",word);
                let currentScope = getTopMut(&mut scopeStack).unwrap();
                par_assert!(token,currentScope.body_is_some(), "Error: can not insert word operation at the top level of a {} as it does not support instructions",currentScope.typ.to_string(false));
                
                let mut isvalid = false;
                // for (name,external) in build.externals.iter() {
                //     match external.typ {
                //         ExternalType::RawExternal => {
                //             if word == name {
                //                 let body = currentScope.body_unwrap_mut().unwrap();
                //                 isvalid = true;
                //                 // TODO: There might be some optimization here to do with Strings, and instead making them references
                //                 body.push((token.location.clone(),Instruction::CALLRAW(ext.clone())));
                //                 break;
                //             }
                //         }
                //         ExternalType::CExternal => {
                //             if word == ext {
                //                 let body = currentScope.body_unwrap_mut().unwrap();
                //                 isvalid = true;
                //                 // TODO: There might be some optimization here to do with Strings, and instead making them references
                //                 body.push((token.location.clone(),Instruction::CALLRAW(external.typ.prefix().clone()+&ext+&external.typ.suffix())));
                //                 break;
                //             }
                //         }
                //     }
                // }
                if let Some(external) = build.externals.get(word) {
                    isvalid = true;
                    let body = currentScope.body_unwrap_mut().unwrap();
                    body.push((token.location.clone(),Instruction::CALLRAW(external.typ.prefix().clone()+word+&external.typ.suffix())));
                }
                if let Some(reg) = Register::from_string(&word) {
                    if !isvalid {
                        isvalid = true;
                        let regOp = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected register operation or another register!");
                        if reg == Register::RAX && program.warn_rax_usage {
                            eprintln!("(P) [WARNING] {}:{}:{}: Usage of RAX is not recommended since RAX is used for popping and might be manipulated! Consider using registers like RBX, RCX, RDX etc.", token.location.file, token.location.linenumber,token.location.character);
                            program.warn_rax_usage = false
                        }
                        match regOp.typ {
                        TokenType::IntrinsicType(typ) => {
                            match typ {
                                IntrinsicType::POP => {
                                    let body = currentScope.body_unwrap_mut().unwrap();
                                    body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(reg))));
                                }
                                IntrinsicType::PUSH => {
                                    let body = currentScope.body_unwrap_mut().unwrap();
                                    body.push((token.location.clone(),Instruction::PUSH(OfP::REGISTER(reg))));
                                }
                                IntrinsicType::SET => {
                                    let token = par_expect!(lexer.currentLocation,lexer.next(),"abruptly ran out of tokens");
                                    match token.typ {
                                        TokenType::Number32(data) => {
                                            if reg.size() >= 4 {
                                                let body = currentScope.body_unwrap_mut().unwrap();
                                                body.push((token.location,Instruction::MOV(OfP::REGISTER(reg), OfP::RAW(data as i64))))
                                            }
                                        }
                                        TokenType::Number64(data) => {
                                            if reg.size() >= 8 {
                                                let body = currentScope.body_unwrap_mut().unwrap();
                                                body.push((token.location,Instruction::MOV(OfP::REGISTER(reg), OfP::RAW(data))))
                                            }
                                        }
                                        TokenType::WordType(ref data) => {
                                            if let Some(reg2) = Register::from_string(data) {
                                                currentScope.body_unwrap_mut().unwrap().push((token.location.clone(),Instruction::MOV(OfP::REGISTER(reg), OfP::REGISTER(reg2))));
                                            }
                                            else if currentScope.contract_is_some() {
                                                let contract = currentScope.contract_unwrap().unwrap();
                                                let mut found = false;
                                                for (name,val) in contract.Inputs.iter() 
                                                {
                                                    if name == data {
                                                        par_assert!(token, val.get_size(program)==reg.size(), "Error: Can not move parameter into register of different size: Register: {} of size {}, parameter {name} of size {}",reg.to_string(),reg.size(),val.get_size(program));
                                                        currentScope.body_unwrap_mut().unwrap().push((token.location.clone(),Instruction::MOV(OfP::REGISTER(reg), OfP::PARAM(data.to_owned()))));
                                                        found = true;
                                                        break;
                                                    }
                                                }
                                                par_assert!(token, found, "Error: Unexpected word for register: '{}'",data);
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
                        TokenType::WordType(Word) => {
                            if let Some(reg2) = Register::from_string(&Word) {
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
                                            IntrinsicType::EQ => {
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
                            // else if currentScope.contract_is_some() {
                                
                            //     // if let Some(local) = currentScope.contract_unwrap().unwrap().Inputs.get{

                            //     // }
                            // }
                            else{
                                par_error!(token, "unknown word '{}' found after Register operation",Word);
                            }
                        }
                        typ => {
                            par_error!(token,"Unexpected register operation! Expected Intrinsic or another Register but found {}",typ.to_string(false));
                        }
                    }
                    }
                }
                if !isvalid && currentScope.locals_is_some() {
                    
                    isvalid = currentScope.locals_unwrap_mut().unwrap().contains_key(word);
                    
                    if isvalid {
                        let regOp = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected variable operation or another variable!");
                        
                        match regOp.typ {
                            TokenType::IntrinsicType(Typ) => {
                                match Typ {
                                    IntrinsicType::POP => {
                                        let body = currentScope.body_unwrap_mut().unwrap();
                                        // TODO: There might be some optimzation to do with references of strings instead of actual strings
                                        body.push((regOp.location,Instruction::POP(OfP::LOCALVAR(word.clone()))));
                                    },
                                    IntrinsicType::PUSH => {
                                        let body = currentScope.body_unwrap_mut().unwrap();
                                        // TODO: There might be some optimzation to do with references of strings instead of actual strings
                                        body.push((regOp.location,Instruction::PUSH(OfP::LOCALVAR(word.clone()))));
                                    },
                                    IntrinsicType::SET => {
                                        let operand = par_expect!(lexer.currentLocation,lexer.next(),"Expected another variable, a constant integer or a (string: not yet implemented)!");
                                        match operand.typ {
                                            TokenType::WordType(ref other) => {
                                                
                                                if currentScope.locals_unwrap().unwrap().contains_key(other) {
                                                    // TODO: There might be some optimzation to do with references of strings instead of actual strings
                                                    let body = currentScope.body_unwrap_mut().unwrap();
                                                    body.push((operand.location,Instruction::MOV(OfP::LOCALVAR(word.clone()), OfP::LOCALVAR(other.clone()))))
                                                }
                                                else if let Some(reg) = Register::from_string(&other) {
                                                    par_assert!(operand,reg.size() == currentScope.locals_unwrap().unwrap().get(word).unwrap().typ.get_size(program), "Register assigned to differently sized variable, Variable size: {}, Register size: {}",reg.size(),currentScope.locals_unwrap_mut().unwrap().get(word).unwrap().typ.get_size(program));
                                                    // TODO: There might be some optimzation to do with references of strings instead of actual strings
                                                    let body = currentScope.body_unwrap_mut().unwrap();
                                                    body.push((operand.location,Instruction::MOV(OfP::LOCALVAR(word.clone()), OfP::REGISTER(reg))))
                                                }
                                                else {
                                                    par_error!(operand,"Could not find variable or register {}",other);
                                                }
                                            },
                                            TokenType::StringType(_) => todo!("Implement strings with local variables"),
                                            TokenType::Number32(val) => {
                                                let body = currentScope.body_unwrap_mut().unwrap();
                                                // TODO: There might be some optimzation to do with references of strings instead of actual strings
                                                body.push((operand.location,Instruction::MOV(OfP::LOCALVAR(word.clone()), OfP::RAW(val as i64))))
                                            },
                                            TokenType::Number64(val) => {
                                                let body = currentScope.body_unwrap_mut().unwrap();
                                                // TODO: There might be some optimzation to do with references of strings instead of actual strings
                                                body.push((operand.location,Instruction::MOV(OfP::LOCALVAR(word.clone()), OfP::RAW(val))))
                                            },
                                            _ => {
                                                par_error!(operand,"Unexpected operand for = intrinsic! Expected either a constant integer, another variable or a (string: not yet implemented) but found {}",operand.typ.to_string(false))
                                            }
                                        }
                                    },
                                    _ => {
                                        par_error!(regOp,"Error: Unexpected intrinsic type for a local variable! Expected POP/PUSH/SET or another local variable but found {}",Typ.to_string(false))
                                    }
                                }
                            },
                            TokenType::WordType(Word) => {
                                if currentScope.locals_unwrap_mut().unwrap().contains_key(&Word) {
                                    
                                    let regOp = lexer.next().expect(&format!("(P) [ERROR] {}:{}:{}: Unexpected register operation!",token.location.clone().file,&token.location.clone().linenumber,&token.location.clone().character));
                                    match regOp.typ {
                                        TokenType::IntrinsicType(typ) => {
                                            
                                            match typ {
                                                IntrinsicType::ADD => {
                                                    let body = currentScope.body_unwrap_mut().unwrap();
                                                    body.push((token.location.clone(),Instruction::ADD(OfP::LOCALVAR(word.clone()), OfP::LOCALVAR(Word.clone()))))
                                                }
                                                IntrinsicType::SUB => {
                                                    let body = currentScope.body_unwrap_mut().unwrap();
                                                    body.push((token.location.clone(),Instruction::SUB(OfP::LOCALVAR(word.clone()), OfP::LOCALVAR(Word.clone()))))
                                                }
                                                IntrinsicType::MUL => {
                                                    let body = currentScope.body_unwrap_mut().unwrap();
                                                    body.push((token.location.clone(),Instruction::MUL(OfP::LOCALVAR(word.clone()), OfP::LOCALVAR(Word.clone()))))
                                                }
                                                IntrinsicType::EQ => {
                                                    let body = currentScope.body_unwrap_mut().unwrap();
                                                    body.push((token.location.clone(),Instruction::EQUALS(OfP::LOCALVAR(word.clone()), OfP::LOCALVAR(Word.clone()))))
                                                }
                                                IntrinsicType::DIV => {
                                                    let body = currentScope.body_unwrap_mut().unwrap();
                                                    body.push((token.location.clone(),Instruction::DIV(OfP::LOCALVAR(word.clone()), OfP::LOCALVAR(Word.clone()))))
                                                }
                                                other => par_error!(token, "Unexpected Intrinsic! Expected Register Intrinsic but found {}",other.to_string(false))
                                            }
                                        }
                                        other => {
                                            par_error!(token, "Unexpected token type: Expected Intrinsic but found {}",other.to_string(false));
                                        }
                                    }
                                }
                                else{
                                    par_error!(token, "unknown word {} found after Variable operation",Word);
                                }
                            }
                            _ => {
                                par_error!(regOp,"Error: Unexpected token type for a local variable! Expected Intrinsic or another local but found {}",regOp.typ.to_string(false))
                            }
                        }
                    }
                }
                if !isvalid {
                    isvalid = foundFuncs.contains(word);
                    let body = currentScope.body_unwrap_mut().unwrap();
                    if isvalid {
                        // TODO: There might be some optimzation to do with references of strings instead of actual strings
                        body.push((token.location.clone(),Instruction::CALL(word.clone())));
                    }
                }
                if !isvalid  {
                    isvalid = build.constdefs.contains_key(word);
                    if isvalid {
                        let cons = build.constdefs.get(word).unwrap();
                        match cons.typ {
                            RawConstValueType::INT(ref val) => {
                                let body = currentScope.body_unwrap_mut().unwrap();
                                body.push((token.location.clone(),Instruction::MOV(OfP::REGISTER(Register::RAX), OfP::RAW(*val as i64))));
                                body.push((token.location.clone(),Instruction::PUSH(OfP::REGISTER(Register::RAX))));
                            }
                            RawConstValueType::LONG(ref val) => {
                                let body = currentScope.body_unwrap_mut().unwrap();
                                body.push((token.location.clone(),Instruction::MOV(OfP::REGISTER(Register::RAX), OfP::RAW(*val as i64))));
                                body.push((token.location.clone(),Instruction::PUSH(OfP::REGISTER(Register::RAX))));
                            }
                            RawConstValueType::STR(val) => {
                                let body = currentScope.body_unwrap_mut().unwrap();
                                body.push((token.location.clone(),Instruction::PUSH(OfP::STR(val, ProgramStringType::STR))));
                            }
                        }
                    }
                }
                if !isvalid && currentScope.contract_is_some(){
                    //let func = build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap();
                    let contract = currentScope.contract_unwrap().unwrap();
                    for (p_name,_) in contract.Inputs.iter() {
                        if p_name == word {
                            isvalid = true;
                            let f = par_expect!(lexer.currentLocation, lexer.next(), "Error: Abruptly ran out of tokens for parameter operation");
                            match f.typ {
                                TokenType::WordType(_) => todo!(),
                                TokenType::IntrinsicType(typ) => {
                                    match typ {
                                        IntrinsicType::PUSH => {
                                            let body = currentScope.body_unwrap_mut().unwrap();
                                            body.push((lexer.currentLocation.clone(), Instruction::PUSH(OfP::PARAM(word.clone()))))
                                        },
                                        _ => {
                                            par_error!(lexer.currentLocation, "Unknown operation for parameter {}",typ.to_string(false))
                                        }
                                    }
                                },
                                TokenType::Definition(_) => todo!(),
                                TokenType::StringType(_) => todo!(),
                                TokenType::CStringType(_) => todo!(),
                                TokenType::CharType(_) => todo!(),
                                TokenType::Number32(_) => todo!(),
                                TokenType::Number64(_) => todo!(),
                            }
                            break;
                        }
                    }
                    
                }
                par_assert!(token,isvalid,"Unknown word type: {}",word);
            }
            TokenType::IntrinsicType(Type) => {
                match Type {
                    IntrinsicType::Extern => {
                        let externType = lexer.next();
                        let externType = par_expect!(lexer.currentLocation,externType,"Error: Unexpected abtrupt end of tokens in extern");
                        match externType.typ {
                            TokenType::WordType(Word) => {
                                build.externals.insert(Word,External { typ: ExternalType::RawExternal, loc: externType.location.clone()});
                            }
                            TokenType::IntrinsicType(_) => assert!(false,"Unexpected behaviour! expected type Word or String but found Intrinsic"),
                            TokenType::StringType(Type) => {
                                match Type.as_str() {
                                    "C" => {
                                        let externWord = lexer.next();
                                        let externWord = externWord.expect("Error: C type extern defined but stream of tokens abruptly ended!");
                                        match externWord.typ {
                                            TokenType::WordType(Word) => {
                                                build.externals.insert(Word,External { typ: ExternalType::CExternal, loc: externWord.location.clone()});
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
                                par_assert!(token,build.functions.get(&Word).is_none(),"Multiply defined symbols {}!",Word);
                                let contract = parse_function_contract(lexer);
                                //currentFunction = Some(Word.clone()); // Make currentFunction a shared reference with Rc<RefCell<>>
                                if Word == "main" {
                                    //build.functions.insert(Word.clone(), None); //Function { contract: contract.clone(), body:  vec![], location: token.location.clone(), locals: HashMap::new() }
                                    foundFuncs.insert(Word.clone());
                                    scopeStack.push(Scope { typ: ScopeType::FUNCTION(Function { contract: contract.clone(), body:  vec![], location: token.location.clone(), locals: HashMap::new() }, Word), hasBeenOpened: false });
                                    build.functions.reserve(1);
                                }
                                else {
                                    //build.functions.insert(Word.clone(), None);//Function { contract: contract.clone(), body:  vec![(token.location.clone(),Instruction::FNBEGIN())], location: token.location.clone(), locals:  HashMap::new() }
                                    foundFuncs.insert(Word.clone());
                                    scopeStack.push(Scope { typ: ScopeType::FUNCTION(Function { contract: contract.clone(), body:  vec![(token.location.clone(),Instruction::FNBEGIN())], location: token.location.clone(), locals: HashMap::new() }, Word), hasBeenOpened: false });
                                    build.functions.reserve(1);
                                }
                                //let v = build.functions.get_mut(&Word).unwrap();
                                //Scope {typ: ScopeType::FUNC, hasBeenOpened: false,inst: InstBodyType::Func(v.body.clone()), locals: LocalBodyType::Func(v.locals.clone()), contract: Some(contract)}
                                //scopeStack.push(Scope { typ: ScopeType::FUNCTION(FunctionScope { func: v }), hasBeenOpened: false});
                            }
                            Other => par_error!(token,"Unexpected behaviour! Expected type Word but found {}",Other.to_string(false))
                        }

                    }
                    IntrinsicType::OPENPAREN => todo!(),
                    IntrinsicType::CLOSEPAREN => todo!(),
                    IntrinsicType::DOUBLE_COLIN => todo!("Context {:#?}",build),
                    IntrinsicType::COMA => todo!(),
                    IntrinsicType::OPENCURLY => {
                        let ln = scopeStack.len();
                        if ln != 0 {
                            let s = scopeStack.get_mut(ln-1).unwrap();
                            
                            par_assert!(token,!s.hasBeenOpened, "Scope already opened! {:?}",scopeStack);
                            s.hasBeenOpened = true;
                            //let currentFunc = build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap();
                            
                            match &s.typ {
                                ScopeType::FUNCTION(_, _) => {}
                                ScopeType::NORMAL(normal) => {
                                    match normal.typ {
                                        NormalScopeType::IF => {
                                           par_assert!(token, ln > 1, "Error: Alone if outside of any scope is not allowed!");
                                           let prev = scopeStack.get_mut(ln-2).unwrap();
                                           // TODO: implement expect_mut()
                                           par_assert!(token, prev.body_is_some(), "Error: if can not be declared inside of scope of {} as they do not allow instructions!",prev.typ.to_string(true));
                                        }
                                        NormalScopeType::ELSE | NormalScopeType::EMPTY => {}
                                    }
                                }
                            }
                        }
                        else {
                            //inst: InstBodyType::Normal(vec![]), locals: LocalBodyType::None, contract: None, hasBeenOpened: true }
                            scopeStack.push(Scope { typ: ScopeType::NORMAL(NormalScope { typ: NormalScopeType::EMPTY, body: vec![] }), hasBeenOpened: true});
                        }
                    }
                    IntrinsicType::CLOSECURLY => {
                        if let Some(sc) = scopeStack.pop() {
                            par_assert!(token,sc.hasBeenOpened, "Error: scope closed but never opened!");
                            match sc.typ {
                                ScopeType::FUNCTION(mut func, name) => {
                                    // TODO: Redo system, since we know that if the scope is Function it is always going to have a body
                                    // Meaning we basically need to redo this
                                    //let body = sc.body_unwrap_mut().unwrap();
                                    func.body.push((token.location.clone(),Instruction::SCOPEEND));
                                    build.functions.insert(name, func);
                                },
                                ScopeType::NORMAL(normal) => {
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
                    IntrinsicType::POP => {
                        par_assert!(token, scopeStack.len() > 0 && getTopMut(&mut scopeStack).unwrap().body_is_some(), "Error: Unexpected pop intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(&mut scopeStack).unwrap().typ.to_string(false));
                        let body = getTopMut(&mut scopeStack).unwrap().body_unwrap_mut().unwrap();
                        body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::RAX))));
                    }
                    IntrinsicType::PUSH => todo!("{}",lexer.currentLocation.loc_display()),
                    IntrinsicType::SET => todo!("{}",lexer.currentLocation.loc_display()),
                    IntrinsicType::ADD => {
                        par_assert!(token, scopeStack.len() > 0 && getTopMut(&mut scopeStack).unwrap().body_is_some(), "Error: Unexpected add intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(&mut scopeStack).unwrap().typ.to_string(false));
                        let body = getTopMut(&mut scopeStack).unwrap().body_unwrap_mut().unwrap();
                        body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::EAX))));
                        body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::R8D))));
                        body.push((token.location.clone(),Instruction::ADD(OfP::REGISTER(Register::EAX), OfP::REGISTER(Register::R8D))))
                    }
                    IntrinsicType::SUB => {
                        par_assert!(token, scopeStack.len() > 0 && getTopMut(&mut scopeStack).unwrap().body_is_some(), "Error: Unexpected sub intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(&mut scopeStack).unwrap().typ.to_string(false));
                        let body = getTopMut(&mut scopeStack).unwrap().body_unwrap_mut().unwrap();
                        body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::EAX))));
                        body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::R8D))));
                        body.push((token.location.clone(),Instruction::SUB(OfP::REGISTER(Register::EAX), OfP::REGISTER(Register::R8D))))
                    }
                    IntrinsicType::MUL => {
                        par_assert!(token, scopeStack.len() > 0 && getTopMut(&mut scopeStack).unwrap().body_is_some(), "Error: Unexpected multiply intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(&mut scopeStack).unwrap().typ.to_string(false));
                        let body = getTopMut(&mut scopeStack).unwrap().body_unwrap_mut().unwrap();
                        body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::EAX))));
                        body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::R8D))));
                        body.push((token.location.clone(),Instruction::MUL(OfP::REGISTER(Register::EAX), OfP::REGISTER(Register::R8D))))
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
                                let mut lf = Lexer::new(&info,lexer.Intrinsics,lexer.Definitions);
                                lf.currentLocation.file = Rc::new(String::from(p.join(&include_p).to_str().unwrap().replace("\\", "/")));
                                let mut nprogram = program.clone();
                                
                                nprogram.path = String::from(p.join(&include_p).to_str().unwrap().replace("\\", "/"));
                                let mut build2 = parse_tokens_to_build(&mut lf, &mut nprogram);
                                build.externals.extend(build2.externals);
                                for (strdefId,_strdef) in build2.stringdefs.iter() {
                                    let orgstrdefId  = strdefId.clone();
                                    let mut strdefId = strdefId.clone();
                                    let isContaining = build.stringdefs.contains_key(&strdefId);
                                    while build.stringdefs.contains_key(&strdefId) {
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
                                        for (_floc, funcDef) in build2.functions.iter_mut() {
                                            for (_iloc,Inst) in funcDef.body.iter_mut() {
                                                match Inst {
                                                    Instruction::PUSH(ref mut strid) => {
                                                        match strid {
                                                            OfP::STR(strid,_) => {
                                                                if *strid == orgstrdefId {
                                                                    *strid = strdefId.clone();
                                                                }
                                                            }
                                                            _ => {}
                                                        }
                                                    }
                                                    _ => {}
                                                }
                                            }
                                        }
                                    }
                                }
                                build.stringdefs.extend(build2.stringdefs);
                                for (fn_name,fn_fn) in build2.functions {
                                    let _loc = fn_fn.location.clone();
                                    match build.functions.insert(fn_name.clone(), fn_fn) {
                                        Some(_Other) => {
                                            lpar_error!(_loc, "Error: mulitply defined symbols {}",fn_name);
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
                        scopeStack.push(Scope { typ: ScopeType::NORMAL(NormalScope { typ: NormalScopeType::IF, body: vec![] }), hasBeenOpened: false })
                        //scopeStack.push( ScopeOpener { hasBeenOpened: false, typ: ScopeType::IF, cinstruct_size: build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.len().clone() });
                    },
                    IntrinsicType::EQ => {
                        par_assert!(token, scopeStack.len() > 0 && getTopMut(&mut scopeStack).unwrap().body_is_some(), "Error: Unexpected equals intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(&mut scopeStack).unwrap().typ.to_string(false));
                        let body = getTopMut(&mut scopeStack).unwrap().body_unwrap_mut().unwrap();
                        body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::RAX))));
                        body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::R8))));
                        body.push((token.location.clone(),Instruction::EQUALS(OfP::REGISTER(Register::RAX), OfP::REGISTER(Register::R8))));
                        
                    },
                    IntrinsicType::CONSTANT => {
                        let first = lexer.next();
                        let first = par_expect!(lexer.currentLocation,first,"abruptly ran out of tokens in constant name definition");
                        let name: String = match first.typ {
                            TokenType::WordType(ref word) => {
                                par_assert!(first, !build.constdefs.contains_key(word) && !foundFuncs.contains(word), "Error: multiple constant symbol definitions");
                                word.to_string()
                            }
                            _ => {
                                par_error!(first, "Unexpected token type! Expected word but found {}",first.typ.to_string(false));
                            }
                        };
                        let first = lexer.next();
                        let first = par_expect!(lexer.currentLocation,first,"abruptly ran out of tokens in constant name definition");
                        match first.typ {
                            TokenType::IntrinsicType(typ) => {
                                match typ {
                                    IntrinsicType::SET => {}
                                    _ => {
                                        par_error!(first, "Error: expected = but found {}",typ.to_string(false));
                                    }
                                }
                            }
                            _ => {
                                par_error!(first, "Unexpected token type! Expected intrinsic but found {}",first.typ.to_string(false));
                            }
                        }
                        
                        let val = eval_const_def(lexer,&mut build);
                        build.constdefs.insert(name, match val.typ {
                            ConstValueType::INT(rval) => {
                               RawConstValue {typ: RawConstValueType::INT(rval), loc: val.loc}
                            }
                            ConstValueType::LONG(rval) => {
                                RawConstValue {typ: RawConstValueType::LONG(rval), loc: val.loc}
                            }
                            ConstValueType::STR(rval) => {
                                let mut UUID = Uuid::new_v4();
                                while build.stringdefs.contains_key(&UUID) {
                                    UUID = Uuid::new_v4();
                                }
                                build.stringdefs.insert(UUID,ProgramString {Data: rval, Typ: ProgramStringType::STR});
                                RawConstValue {typ: RawConstValueType::STR(UUID), loc: val.loc}
                                
                            }
                        });
                        
                    },
                    IntrinsicType::DOTCOMA => {},
                    IntrinsicType::ELSE => todo!(),
                    IntrinsicType::DIV => todo!("{:#?} at {}",build,lexer.currentLocation.loc_display()),
                    IntrinsicType::Let => {
                        
                        let nametok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for Let");
                        let loc = nametok.location.clone();
                        match nametok.typ {
                            TokenType::WordType(name) => {
                                par_assert!(loc, !build.constdefs.contains_key(&name) && !foundFuncs.contains(&name), "Error: multiply defined symbols");
                                //let currentFunc = build.functions.get_mut(&currentFunction.clone().expect("Todo: Global variables are not yet implemented :|")).unwrap();
                                par_assert!(token, scopeStack.len() > 0 && getTopMut(&mut scopeStack).unwrap().body_is_some(), "Error: Unexpected multiply intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(&mut scopeStack).unwrap().typ.to_string(false));
                                let currentScope = getTopMut(&mut scopeStack).unwrap();
                                //let body = currentScope.body_unwrap_mut().unwrap();
                                //let locals = currentScope.locals_unwrap_mut().unwrap();
                                let typ = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for let type");                                
                                par_assert!(typ,typ.typ==TokenType::IntrinsicType(IntrinsicType::DOUBLE_COLIN), "Error: You probably forgot to put a : after the name!");
                                let typ = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for let type");
                                match typ.typ {
                                    TokenType::Definition(def) => {
                                        currentScope.locals_unwrap_mut().unwrap().insert(name.clone(), LocalVariable { typ: def, operand: 0 });
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
                        // TODO: Fix the whole system of currentFunction as this is REALLY REALLY REALLY wasteful
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
                            _ => {
                                par_error!(lexerNext, "Unexpected token type for INTERRUPT, {}",lexerNext.typ.to_string(false))
                            }
                        }
                        
                    },
                    IntrinsicType::RS => {
                        par_assert!(token, scopeStack.len() > 0 && getTopMut(&mut scopeStack).unwrap().body_is_some(), "Error: Unexpected rs intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(&mut scopeStack).unwrap().typ.to_string(false));
                        let body = getTopMut(&mut scopeStack).unwrap().body_unwrap_mut().unwrap();
                        //                             Call here: ⌄
                        // PARAM <Reserve Space for return stack> RIP
                        let lexerNext = par_expect!(lexer.currentLocation,lexer.next(),"Stream of tokens ended abruptly at RS call");
                        match lexerNext.typ {
                            TokenType::WordType(ref Word) => {
                                let optyp = par_expect!(lexer.currentLocation,lexer.next(),"Stream of tokens ended abruptly at RS push call");
                                match optyp.typ {
                                    TokenType::IntrinsicType(typ) => {
                                    match typ {
                                        IntrinsicType::PUSH => {
                                            let Reg = par_expect!(lexer.currentLocation, Register::from_string(Word), "Error: Expected Register but found: {}",Word);
                                            body.push((lexer.currentLocation.clone(), Instruction::RSPUSH(OfP::REGISTER(Reg))))            
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
                }
            }
            TokenType::StringType(Word) => {

                par_assert!(lexer.currentLocation, scopeStack.len() > 0 && getTopMut(&mut scopeStack).unwrap().body_is_some(), "Error: Unexpected string outside of scope! Scopes of type {} do not support instructions!",getTopMut(&mut scopeStack).unwrap().typ.to_string(false));
                let body = getTopMut(&mut scopeStack).unwrap().body_unwrap_mut().unwrap();
                let mut UUID = Uuid::new_v4();
                while build.stringdefs.contains_key(&UUID) {
                    UUID = Uuid::new_v4();
                }
                build.stringdefs.insert(UUID,ProgramString {Data: Word, Typ: ProgramStringType::STR});
                body.push((token.location,Instruction::PUSH(OfP::STR(UUID,ProgramStringType::STR))));
            }
            TokenType::CStringType(Word) => {
                par_assert!(lexer.currentLocation, scopeStack.len() > 0 && getTopMut(&mut scopeStack).unwrap().body_is_some(), "Error: Unexpected cstring outside of scope! Scopes of type {} do not support instructions!",getTopMut(&mut scopeStack).unwrap().typ.to_string(false));
                let body = getTopMut(&mut scopeStack).unwrap().body_unwrap_mut().unwrap();

                
                let mut UUID = Uuid::new_v4();
                while build.stringdefs.contains_key(&UUID) {
                    UUID = Uuid::new_v4();
                }
                build.stringdefs.insert(UUID,ProgramString {Data: Word, Typ: ProgramStringType::CSTR}); 
                body.push((token.location,Instruction::PUSH(OfP::STR(UUID,ProgramStringType::CSTR))));
            }
            
            TokenType::CharType(_) => {
                todo!("{}: Unexpected char! Chars",token.loc_display())
                //par_assert!(token,currentFunction.is_some(), "Unexpected char definition outside of entry point!");
            }
            TokenType::Number32(val) => {
                let len = scopeStack.len();
                let a = &mut scopeStack;
                par_assert!(token, len > 0 && getTopMut(a).unwrap().body_is_some(), "Error: Unexpected interrupt intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(a).unwrap().typ.to_string(false));
                let body = getTopMut(a).unwrap().body_unwrap_mut().unwrap();
                // TODO: implement this
                body.push((token.location.clone(),Instruction::MOV(OfP::REGISTER(Register::EAX), OfP::RAW(val as i64))));
                body.push((token.location.clone(),Instruction::PUSH(OfP::REGISTER(Register::EAX))));
            }
            TokenType::Number64(val) => {
                let len = scopeStack.len();
                let a = &mut scopeStack;
                par_assert!(token,  len > 0 && getTopMut(a).unwrap().body_is_some(), "Error: Unexpected interrupt intrinsic outside of scope! Scopes of type {} do not support instructions!",getTopMut(a).unwrap().typ.to_string(false));
                let body = getTopMut(a).unwrap().body_unwrap_mut().unwrap();
                body.push((token.location.clone(),Instruction::PUSH(OfP::RAW(val))));
            }
            TokenType::Definition(_) => todo!(),
            TokenType::CStringType(_) => todo!(),
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
    should_use_callstack: bool,
    usedStrings: HashMap<Uuid, String>,
    usedExterns: HashSet<String>,
    usedFuncs: HashSet<String>,
}
impl optim_ops {
    fn new() -> Self{
        Self { should_use_callstack: false, usedStrings: HashMap::new(), usedExterns: HashSet::new(), usedFuncs: HashSet::new() }
    }
}
fn optimization_ops(build: &mut BuildProgram, program: &CmdProgram) -> optim_ops{
    match program.in_mode {
        OptimizationMode::RELEASE => {
            let mut out = optim_ops::new();

            for (fn_name, func) in build.functions.iter() {
                for (_,op) in func.body.iter() {
                    match op {
                        Instruction::PUSH(d) => {
                            match d {
                                OfP::STR(UUID,_) => {
                                    //TODO: Make usedStrings to be a HashMap of two references instead of the raw values so that they don't need to be cloned
                                    out.usedStrings.insert(UUID.clone(), fn_name.clone());
                                }
                                _ => {}
                            }
                        }
                        Instruction::DEFVAR(_) => {
                            out.should_use_callstack = true;
                        }
                        Instruction::CALLRAW(r) => {
                            //TODO: Make callraw use reference instead of raw
                            out.usedExterns.insert(r.clone());
                        }
                        Instruction::CALL(r) => {
                            //TODO: Make call use reference instead of raw
                            out.usedFuncs.insert(r.clone());
                        }
                        _ => {}
                    }
                }
            }
            out
        },
        OptimizationMode::DEBUG => optim_ops { should_use_callstack: true, usedStrings: HashMap::new(), usedExterns: HashSet::new(), usedFuncs: HashSet::new() },
    }
}

//fn nasm_x86_64_handle_scope(build: &mut BuildProgram, program: &CmdProgram, scope: &
fn to_nasm_x86_64(build: &mut BuildProgram, program: &CmdProgram) -> io::Result<()>{
    let optimization = optimization_ops(build, program);
    let mut f = File::create(&program.opath).expect(&format!("Error: could not open output file {}",program.opath.as_str()));
    writeln!(&mut f,"BITS 64")?;
    writeln!(&mut f, "section .data")?;
    
    
    for (UUID,stridef) in build.stringdefs.iter(){                
        if program.in_mode == OptimizationMode::DEBUG || (optimization.usedStrings.contains_key(UUID) && optimization.usedFuncs.contains(optimization.usedStrings.get(UUID).unwrap()) || optimization.usedStrings.get(UUID).unwrap() == "main") {
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
                ProgramStringType::STR  => writeln!(&mut f, "   _LEN_STRING_{}_: dq {}",UUID.to_string().replace("-", ""),stridef.Data.len())?,
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
    if optimization.should_use_callstack {
        writeln!(&mut f, "section .bss")?;
        writeln!(&mut f, "   _CALLSTACK: resb {}",program.call_stack_size)?;
        writeln!(&mut f, "   _CALLSTACK_TOP: ")?;
        writeln!(&mut f, "   _CALLSTACK_BUF_PTR: resd 1")?;
    }
    for function_name in build.functions.keys() {
        if function_name == "main" {
            writeln!(&mut f,"global _{}",function_name)?;
        }
        else {
            if program.in_mode == OptimizationMode::DEBUG || !program.remove_unused_functions || optimization.usedFuncs.contains(function_name){
                writeln!(&mut f,"global _F_{}",function_name)?;
            }
            else if program.print_unused_warns && program.print_unused_funcs {
                println!("[NOTE] {}: Unused function: \"{}\"", build.functions.get(function_name).unwrap().location.loc_display(),function_name);
            }
        }
    }
    for (Word,exter) in build.externals.iter() {
        match exter.typ {            
            ExternalType::CExternal| ExternalType::RawExternal => {
                if program.in_mode == OptimizationMode::DEBUG || optimization.usedExterns.contains(&format!("{}{}{}",exter.typ.prefix(),Word,exter.typ.suffix())) {
                    writeln!(&mut f,"  extern {}{}{}",exter.typ.prefix(),Word,exter.typ.suffix())?;
                }
                else if program.print_unused_warns && program.print_unused_externs {
                    println!("[NOTE] {}: Unused external: \"{}\"",exter.loc.loc_display(),Word);
                } 
            },
        }
    }
    writeln!(&mut f, "section .text")?;
    // TODO: introduce something like mainMEM which won't be bound to the 640 000
    let mut callstack_size: i64 = 0;
    for (function_name,function) in build.functions.iter() {
        // true                                          true                               true                                               true                              
        if program.in_mode != OptimizationMode::DEBUG && program.remove_unused_functions && !optimization.usedFuncs.contains(function_name) && function_name != "main" {
            continue;
        }
        let mut hasFoundRet = false;
        let mut stack_size: i64 = 0;
        
        if function_name == "main" {
            writeln!(&mut f, "_{}:",function_name)?;
            //writeln!(&mut f, "   push rsp")?;
            //writeln!(&mut f, "   sub rsp, 4")?;
            //writeln!(&mut f, "   mov rbp, rsp")?;
        }
        else {
            writeln!(&mut f, "_F_{}:",function_name)?;
        }
        let mut rs_stack_offset: usize = {
            let mut o: usize = 0;
            for v in function.contract.Outputs.iter() {
                o += v.get_size(program);
            }
            o
        };
        let mut local_vars = function.locals.clone();
        for (i,(_location, instruction)) in function.body.iter().enumerate(){
            if program.in_mode == OptimizationMode::DEBUG {
                writeln!(&mut f,"   ; --- {} ",i)?
            }
            match instruction {
                Instruction::PUSH(Reg) => {
                    match Reg {
                        OfP::REGISTER(Reg) => {
                            writeln!(&mut f, "   sub rsp, {}",Reg.size())?;
                            writeln!(&mut f, "   mov {} [rsp], {}",size_to_nasm_type(Reg.size()), Reg.to_string())?;
                                
                            stack_size += Reg.size() as i64
                        }
                        OfP::STR(UUID,typ) => {
                            match typ {
                                ProgramStringType::STR => {
                                    match program.build_architecture.as_str() {
                                        "64" => {
                                            writeln!(&mut f, "   sub rsp, 12")?;
                                            writeln!(&mut f, "   mov qword [rsp+8], _STRING_{}_",UUID.to_string().replace("-", ""))?;
                                            stack_size += 12
                                        }
                                        "32" | _ => {
                                            writeln!(&mut f, "   sub rsp, 16")?;
                                            writeln!(&mut f, "   mov dword [rsp+8], _STRING_{}_",UUID.to_string().replace("-", ""))?;
                                            stack_size += 16
                                        }
                                    }
                                    writeln!(&mut f, "   mov qword [rsp], {}",build.stringdefs.get(&UUID).unwrap().Data.len())?;
                                    //writeln!(&mut f, "   mov qword [rsp], _LEN_STRING_{}_",UUID.to_string().replace("-", ""))?;
                                },
                                ProgramStringType::CSTR => {
                                    match program.build_architecture.as_str() {
                                        "64" => {
                                            writeln!(&mut f, "   sub rsp, 8")?;
                                            writeln!(&mut f, "   mov qword [rsp], _STRING_{}_",UUID.to_string().replace("-", ""))?;
                                            stack_size += 8
                                        }
                                        "32" | _ => {
                                            writeln!(&mut f, "   sub rsp, 4")?;
                                            writeln!(&mut f, "   mov dword [rsp], _STRING_{}_",UUID.to_string().replace("-", ""))?;
                                            stack_size += 4
                                        }
                                    }
                                },
                            }
                        }
                        OfP::RAW(Data) => {
                            writeln!(&mut f, "   sub rsp, 8")?;
                            writeln!(&mut f, "   mov qword [rsp], {}",Data)?;
                            stack_size += 8
                        }
                        OfP::LOCALVAR(var) => {
                            let var = local_vars.get(var).expect(&format!("Error: could not find variable {}",var));
                            if callstack_size as usize-var.operand-var.typ.get_size(program) == 0 {
                                writeln!(&mut f, "   mov r10, [_CALLSTACK_BUF_PTR]")?;
                            }
                            else {
                                writeln!(&mut f, "   mov r10, [_CALLSTACK_BUF_PTR+{}]",callstack_size as usize-var.operand-var.typ.get_size(program))?;
                            }
                            stack_size += var.typ.get_size(program) as i64;
                            writeln!(&mut f, "   push r10")?;
                        }
                        OfP::PARAM(var) => {
                            for (i,(name, p)) in function.contract.Inputs.iter().enumerate() {
                                if name == var {
                                    let offset = stack_size+{
                                        let mut o: usize = 0;
                                        for (_, p) in &function.contract.Inputs[i..] {                                        
                                            o += p.get_size(program)
                                        }
                                        o
                                    } as i64+{
                                        let mut o: usize = 0;
                                        for i in function.contract.Outputs.iter() {
                                            o += i.get_size(program)
                                        }
                                        o
                                    } as i64;
                                    writeln!(&mut f, "   mov rbx, qword [rsp+{}]",offset)?;
                                    writeln!(&mut f, "   sub rsp, {}",p.get_size(program))?;
                                    writeln!(&mut f, "   mov qword [rsp], rbx")?;
                                    stack_size += p.get_size(program) as i64;
                                    break;
                                }
                            }
                        }
                        _ => {
                            todo!("Unsupported")
                        }
                    }
                    
                }
                Instruction::MOV(Op, Op2) => {
                    match Op {
                        OfP::REGISTER(Reg1) => {
                            match Op2 {
                                OfP::REGISTER(Reg2) => {
                                    writeln!(&mut f, "   mov {}, {}",Reg1.to_string(), Reg2.to_string())?;
                                },
                                OfP::LOCALVAR(_) => todo!(),
                                OfP::RAW(Data) => {
                                    writeln!(&mut f, "   mov {}, {}",Reg1.to_string(), Data)?;
                                },
                                OfP::STR(_, _) => todo!(),
                                OfP::PARAM(data) => {
                                    for (i,(name, _)) in function.contract.Inputs.iter().enumerate() {
                                        if name == data {
                                            let offset = stack_size+{
                                                let mut o: usize = 0;
                                                for (_, p) in &function.contract.Inputs[i..] {                                        
                                                    o += p.get_size(program)
                                                }
                                                o
                                            } as i64+{
                                                let mut o: usize = 0;
                                                for i in function.contract.Outputs.iter() {
                                                    o += i.get_size(program)
                                                }
                                                o
                                            } as i64;
                                            writeln!(&mut f, "   mov {}, qword [rsp+{}]",Reg1.to_string(),offset)?;
                                            break;
                                        }
                                    }
                                },
                            }
                        }
                        OfP::LOCALVAR(varOrg) => {
                            match Op2 {
                                OfP::REGISTER(val) => {
                                    let var = com_expect!(_location,local_vars.get(varOrg),"Error: Unknown variable found during compilation {}",varOrg);
                                    
                                    if var.typ.get_size(program) <= 8  {
                                        if callstack_size as usize-var.operand-var.typ.get_size(program) == 0 {
                                            writeln!(&mut f, "   mov {} [_CALLSTACK_BUF_PTR], {}",size_to_nasm_type(var.typ.get_size(program)), val.to_string())?;
                                        }
                                        else {
                                            writeln!(&mut f, "   mov {} [_CALLSTACK_BUF_PTR+{}], {}",size_to_nasm_type(var.typ.get_size(program)),callstack_size as usize-var.operand-var.typ.get_size(program), val.to_string())?;
                                        }
                                    }
                                    else {
                                        todo!()
                                    }
 
                                },
                                OfP::LOCALVAR(_) => todo!("vars"),
                                OfP::RAW(val) => {
                                    let var = com_expect!(_location,local_vars.get(varOrg),"Error: Unknown variable found during compilation {}",varOrg);
                                    
                                    if var.typ.get_size(program) <= 8  {
                                        if callstack_size as usize-var.operand-var.typ.get_size(program) == 0 {
                                            writeln!(&mut f, "   mov {} [_CALLSTACK_BUF_PTR], {}",size_to_nasm_type(var.typ.get_size(program)), val)?;
                                        }
                                        else {
                                            writeln!(&mut f, "   mov {} [_CALLSTACK_BUF_PTR+{}], {}",size_to_nasm_type(var.typ.get_size(program)),callstack_size as usize-var.operand-var.typ.get_size(program), val)?;
                                        }
                                    }
                                    else {
                                        todo!()
                                    }
                                },
                                OfP::STR(_,_) => todo!(),
                                OfP::PARAM(_) => todo!(),
                            }
                        }
                        _ => {
                            todo!("Unsupported");
                        }
                    }
                    
                }
                Instruction::POP(Reg) => {
                    match Reg {
                        OfP::REGISTER(Reg) => {
                            writeln!(&mut f, "   mov {}, {} [rsp]",Reg.to_string(),size_to_nasm_type(Reg.size()))?;
                            writeln!(&mut f, "   add rsp, {}",Reg.size())?;
                            stack_size -= Reg.size() as i64
                        }
                        _ => {
                            todo!("Unsupported")
                        }
                    }
                }
                Instruction::CALLRAW(Word) => {
                    writeln!(&mut f, "   xor rax, rax")?;
                    writeln!(&mut f, "   call {}",Word)?;
                }
                Instruction::ADD(op1, op2) => {
                    match op1 {
                        OfP::REGISTER(reg1) => {
                            match op2 {
                                OfP::REGISTER(reg2) => {
                                    assert!(reg1.size() == reg2.size(), "Two different sized registers passed to add Instruction");
                                    writeln!(&mut f, "   add {}, {}",reg1.to_string(), reg2.to_string())?;
                                }
                                OfP::LOCALVAR(_) => todo!(),
                                OfP::RAW(val) => {
                                    if val != &0 {
                                        writeln!(&mut f, "   add {}, {}",reg1.to_string(),val)?;
                                    }
                                }
                                OfP::STR(_,_) => todo!(),
                                OfP::PARAM(_) => todo!(),
                            }
                        }
                        OfP::LOCALVAR(var1) => {
                            match op2 {
                                OfP::REGISTER(_) => todo!(),
                                OfP::LOCALVAR(var2) => {
                                    com_assert!(_location, function.locals.contains_key(var1) && function.locals.contains_key(var2), "Unknown variable");
                                    let local_vars = &local_vars;
                                    let var1 = local_vars.get(var1).unwrap();
                                    let var2 = local_vars.get(var2).unwrap();
                                    com_assert!(_location, var1.typ.get_size(program) == var2.typ.get_size(program), "Unknown variable size");
                                    if callstack_size as usize-var1.operand-var1.typ.get_size(program) == 0 {
                                        writeln!(&mut f, "   mov r10, [_CALLSTACK_BUF_PTR]")?;
                                    }
                                    else {
                                        writeln!(&mut f, "   mov r10, [_CALLSTACK_BUF_PTR+{}]",callstack_size as usize-var1.operand-var1.typ.get_size(program))?;
                                    }
                                    if callstack_size as usize-var2.operand-var2.typ.get_size(program) == 0{
                                        writeln!(&mut f, "   add r10, [_CALLSTACK_BUF_PTR]")?;  
                                    }
                                    else {
                                        writeln!(&mut f, "   add r10, [_CALLSTACK_BUF_PTR+{}]", callstack_size as usize-var2.operand-var2.typ.get_size(program))?;  
                                    }
                                    if callstack_size as usize-var1.operand-var1.typ.get_size(program) == 0 {
                                        writeln!(&mut f, "   mov [_CALLSTACK_BUF_PTR], r10")?;
                                    }
                                    else {
                                        writeln!(&mut f, "   mov [_CALLSTACK_BUF_PTR+{}], r10",callstack_size as usize-var1.operand-var1.typ.get_size(program))?;
                                    }
                                },
                                OfP::RAW(_) => todo!(),
                                OfP::STR(_,_) => todo!(),
                                OfP::PARAM(_) => todo!(),
                            }
                        },
                        OfP::RAW(_) => todo!(),
                        OfP::STR(_,_) => todo!(),
                        OfP::PARAM(_) => todo!(),
                        
                    }
                    
                }
                Instruction::SUB(op1, op2) => {
                    match op1 {
                        OfP::REGISTER(reg1) => {
                            match op2 {
                                OfP::REGISTER(reg2) => {
                                    assert!(reg1.size() == reg2.size(), "Two different sized registers passed to sub Instruction");
                                    writeln!(&mut f, "   sub {} {}, {}",size_to_nasm_type(reg1.size()),reg1.to_string(), reg2.to_string())?;
                                }
                                OfP::LOCALVAR(_) => todo!(),
                                OfP::RAW(val) => {
                                    writeln!(&mut f, "   sub {}, {}",reg1.to_string(),val)?;
                                },
                                OfP::STR(_,_) => todo!(),
                                OfP::PARAM(_) => todo!(),
                            }
                        }
                        OfP::LOCALVAR(_) => todo!(),
                        OfP::RAW(_) => todo!(),
                        OfP::STR(_,_) => todo!(),
                        OfP::PARAM(_) => todo!(),
                    }
                }
                Instruction::MUL(op1, op2) => {
                    match op1 {
                        OfP::REGISTER(reg1) => {
                            match op2 {
                                OfP::REGISTER(reg2) => {
                                    assert!(reg1.size() == reg2.size(), "Two different sized registers passed to mul Instruction");
                                    writeln!(&mut f, "   mul {} {}, {}",size_to_nasm_type(reg1.size()),reg1.to_string(), reg2.to_string())?;
                                }
                                OfP::LOCALVAR(_) => todo!(),
                                OfP::RAW(val) => {
                                    writeln!(&mut f, "   mul {}, {}",reg1.to_string(),val)?;
                                },
                                OfP::STR(_,_) => todo!(),
                                OfP::PARAM(_) => todo!(),
                            }
                        }
                        OfP::LOCALVAR(_) => todo!(),
                        OfP::RAW(_) => todo!(),
                        OfP::STR(_,_) => todo!(),
                        OfP::PARAM(_) => todo!(),
                    }
                    
                }
                Instruction::DIV(op1, op2) => {
                    match op1 {
                        OfP::REGISTER(reg1) => {
                            match op2 {
                                OfP::REGISTER(reg2) => {
                                    assert!(reg1.size() == reg2.size(), "Two different sized registers passed to div Instruction");
                                    writeln!(&mut f, "   mov r9, rdx")?;
                                    writeln!(&mut f, "   xor rdx, rdx")?;
                                    writeln!(&mut f, "   cqo")?;
                                    writeln!(&mut f, "   mov rax, {}",reg1.to_string())?;
                                    writeln!(&mut f, "   idiv {}",reg2.to_string())?;
                                    writeln!(&mut f, "   mov {}, rax",reg1.to_string())?;
                                    writeln!(&mut f, "   mov {}, rdx",reg2.to_string())?;
                                    writeln!(&mut f, "   mov rdx, r9")?;
                                }
                                OfP::LOCALVAR(_) => todo!(),
                                OfP::RAW(_) => todo!(),
                                OfP::STR(_,_) => todo!(),
                                OfP::PARAM(_) => todo!(),
                            }
                        }
                        OfP::LOCALVAR(_) => todo!(),
                        OfP::RAW(_) => todo!(),
                        OfP::STR(_,_) => todo!(),
                        OfP::PARAM(_) => todo!(),
                    }
                    
                }
                Instruction::CALL(Func) => {
                    let mut o: usize = 0;
                    for i in build.functions.get(Func).unwrap().contract.Outputs.iter() {
                        o += i.get_size(program)
                    }
                    if o > 0 {
                        writeln!(&mut f, "   sub rsp, {}",o)?;
                    }
                    writeln!(&mut f, "   call _F_{}",Func)?;
                    stack_size += o as i64;

                }
                Instruction::FNBEGIN() => {
                    //writeln!(&mut f, "   push rbp")?;
                }
                Instruction::RET() => {
                    let mut functionSize: usize = 0;
                    if !hasFoundRet {
                        for val in function.locals.values() {
                            functionSize += val.typ.get_size(program);
                        };
                        if functionSize > 0 {
                            writeln!(&mut f, "   mov r10, [_CALLSTACK_BUF_PTR]")?;
                            writeln!(&mut f, "   add r10, {}",functionSize)?;
                            writeln!(&mut f, "   mov [_CALLSTACK_BUF_PTR], r10")?;
                        }
                        //writeln!(&mut f, "   pop rbp")?;
                        
                        writeln!(&mut f, "   ret")?;
                        hasFoundRet = true;
                    }
                }
                Instruction::SCOPEBEGIN => {
                    writeln!(&mut f, "   .{}_S_{}:",function_name,i)?;
                }
                Instruction::SCOPEEND => {
                    // TODO: do the same thing we did for functions but for scopes
                    writeln!(&mut f, "   .{}_S_{}:",function_name,i)?;
                }
                // Instruction::CONDITIONAL_JUMP(ni) => {
                //     let (_,prev) = function.body.get(i-1).unwrap();
                //     match prev {
                //         Instruction::EQUALS(Reg, _) => {
                //             match Reg {
                //                 OfP::REGISTER(Reg) => {
                //                     writeln!(&mut f, "   cmp {}, 1",Reg.to_byte_size(1).to_string())?;        
                //                 }
                //                 _ => todo!()
                //             }
                            
                //         }
                //         _ => {
                //             todo!("Implement parsing of if:\nif RBX {{}}\n")
                //         }
                //     }
                //     writeln!(&mut f, "   jz .{}_S_{}",function_name,ni)?;
                // }
                // Instruction::JUMP(ni) => {
                //     writeln!(&mut f, "   jmp .{}_S_{}",function_name,ni)?;
                // }
                Instruction::EQUALS(op1, op2) => {
                    match op1 {
                        OfP::REGISTER(Reg1) => {
                            match op2 {
                                OfP::REGISTER(Reg2) => {
                                    writeln!(&mut f, "   cmp  {}, {}",Reg1.to_string(),Reg2.to_string())?;
                                    writeln!(&mut f, "   sete  {}",Reg1.to_byte_size(1).to_string())?;
                                }
                                OfP::LOCALVAR(_) => todo!(),
                                OfP::RAW(_) => todo!(),
                                OfP::STR(_,_) => todo!(),
                                OfP::PARAM(_) => todo!(),
                            }
                        }
                        OfP::LOCALVAR(_) => todo!(),
                        OfP::RAW(_) => todo!(),
                        OfP::STR(_,_) => todo!(),
                        OfP::PARAM(_) => todo!(),
                    }
                    
                },
                Instruction::DEFVAR(name) => {
                    let var = local_vars.get_mut(name).expect("Error: unknown defvar definition in function! This is most likely due to a bug inside the compiler! Make sure to contact the developer if you encounter this!");
                    var.operand     = callstack_size as usize;
                    callstack_size += var.typ.get_size(program) as i64;

                    writeln!(&mut f, "   mov r10, [_CALLSTACK_BUF_PTR]")?;
                    writeln!(&mut f, "   sub r10, {}",var.typ.get_size(program))?;
                    writeln!(&mut f, "   mov [_CALLSTACK_BUF_PTR], r10")?;
                },
                Instruction::INTERRUPT(val) => {
                    writeln!(&mut f, "   int 0x{:x}",val)?;
                },
                Instruction::RSPUSH(val) => {
                    match val {
                        OfP::REGISTER(reg) => {
                            let offset = rs_stack_offset as i64+stack_size;
                            rs_stack_offset -= reg.size();
                            writeln!(&mut f, "   mov {} [rsp+{}], {}",size_to_nasm_type(reg.size()),offset,reg.to_string())?;
                        },
                        OfP::PARAM(_) => todo!(),
                        OfP::LOCALVAR(_) => todo!(),
                        OfP::RAW(_) => todo!(),
                        OfP::STR(_, _) => todo!(),
                    }
                },
                Instruction::EXPAND_SCOPE(scope) => todo!("EXPAND_SCOPE"),
                Instruction::EXPAND_IF_SCOPE(_) => todo!("EXPAND_IF_SCOPE"),
                Instruction::EXPAND_ELSE_SCOPE(_) => todo!("EXPAND_ELSE_SCOPE"),
            }
        }
        if function_name == "main" {
            //writeln!(&mut f, "   mov rsp, rbp")?;
            //writeln!(&mut f, "   pop rbp")?;
            writeln!(&mut f, "   xor rax,rax")?;
            writeln!(&mut f, "   ret")?;
        }
    }
    Ok(())

}




fn type_check_build(build: &mut BuildProgram, program: &CmdProgram) {
    // TODO: implement macros for assert, expect etc. for type checking that also print the type stack trace.
    for (name, func) in build.functions.iter() {
        let mut rs_stack: Vec<VarType> = Vec::new();
        let mut typeStack: Vec<VarType> = Vec::new();
        for (loc, instruction) in func.body.iter() {
            match instruction {
                Instruction::PUSH(ofp) => {
                    match ofp {
                        OfP::REGISTER(reg) => {
                            typeStack.push(reg.to_var_type());
                        }
                        OfP::LOCALVAR(v) => {
                            typeStack.push(func.locals.get(v).unwrap().typ.clone());
                        },
                        OfP::RAW(_) => {
                            typeStack.push(VarType::LONG);
                        },
                        OfP::STR(_, typ) => {
                            match typ {
                                ProgramStringType::STR => {
                                    typeStack.push(VarType::PTR);
                                    typeStack.push(VarType::LONG)
                                },
                                ProgramStringType::CSTR => {
                                    typeStack.push(VarType::PTR)
                                },
                            }
                        },
                        OfP::PARAM(word) => {
                            for (name,p) in func.contract.Inputs.iter() {
                                if word == name {
                                    typeStack.push(p.clone());
                                    break;
                                }
                            }
                        }
                    }
                },
                Instruction::DEFVAR(_)           => {},
                Instruction::MOV(_, _)           => {},
                Instruction::POP(ofp)              => {
                    match ofp {
                        OfP::REGISTER(reg) => {
                            let v = com_expect!(loc, typeStack.pop(), "Error: Stack underflow occured for types! Make sure everything is ok and you aren't manipulating the stack with anything");
                            com_assert!(loc, v.get_size(program) == reg.size(), "Error: Can not pop type into register as its a different size! Top is: {} bytes Register: {} is {} bytes, Typ: {:?}",v.get_size(program),reg.to_string(),reg.size(),v);
                            match reg.size() {
                                8 | 4 | 2 | 1 => {}
                                _ => {
                                    todo!("Unreachable");
                                }
                            }
                        },
                        _ => {
                            todo!("Unsupported");
                        }
                    }
                },
                Instruction::CALLRAW(_)          => {},
                Instruction::ADD(_, _)           => {},
                Instruction::SUB(_, _)           => {},
                Instruction::MUL(_, _)           => {},
                Instruction::DIV(_, _)           => {},
                Instruction::EQUALS(_, _)        => {},
                Instruction::CALL(func)             => {
                    let function =  com_expect!(loc, build.functions.get(func), "Error: unknown function call to {}, Function may not exist!",func);
                    com_assert!(loc, function.contract.Inputs.len() <= typeStack.len(), "Error: Not enough arguments for {} in {}",func,name);
                    let raw_inputs = {
                        let mut o: Vec<VarType> = Vec::with_capacity(function.contract.Inputs.len());
                        for (_, typ) in function.contract.Inputs.iter() {
                            o.push(typ.clone())
                        }
                        o
                    };
                    com_assert!(loc, typeStack.ends_with(&raw_inputs), "Error: Arguments for function don't match, function expected:\n {}, But found\n: {}",
                    {
                        
                        for (name, typ) in function.contract.Inputs.iter() {
                            eprintln!("   {} ({})",typ.to_string(false).to_uppercase(),name)
                        }
                        ""
                    },
                    {
                        for typ in typeStack.iter() {
                            eprintln!("   {}",typ.to_string(false).to_uppercase())
                        }
                        ""
                    });
                    typeStack.extend(function.contract.Outputs.clone());
                },
                Instruction::FNBEGIN()           => {},
                Instruction::RET()               => {},
                Instruction::SCOPEBEGIN          => {
                    //eprintln!("WARNING: Not implemented yet")
                    /*
                    func main() {
                        // str
                        if pop "Hello World!"c streq RAX {
                            pop 
                        }
                        else {
                            
                        }
                    }
                    */
                },
                Instruction::SCOPEEND            => {},//eprintln!("WARNING: Not implemented yet"),
                // Instruction::CONDITIONAL_JUMP(_) => todo!("Branching"),
                // Instruction::JUMP(_)             => todo!("JUMP to index"),
                Instruction::INTERRUPT(_)        => {},
                Instruction::RSPUSH(typ)           => {
                    match typ {
                        OfP::REGISTER(Reg) => {
                            rs_stack.push(Reg.to_var_type());
                        },
                        OfP::PARAM(_)    => todo!(),
                        OfP::LOCALVAR(_) => todo!(),
                        OfP::RAW(_)      => {
                            rs_stack.push(VarType::LONG);
                        },
                        OfP::STR(_, _)   => todo!(),
                    }
                },
                Instruction::EXPAND_SCOPE(_) => todo!(),
                Instruction::EXPAND_IF_SCOPE(_) => todo!(),
                Instruction::EXPAND_ELSE_SCOPE(_) => todo!(),
            }
        }
        if !typeStack.is_empty() {
            eprintln!("Error: {}: types left on the stack in function body: {}\n   Type stack trace: ",func.location.loc_display(),name);
            for typ in typeStack.iter() {
                eprintln!("   {}",typ.to_string(false).to_uppercase());
            }
            exit(1)
        }
        if rs_stack != func.contract.Outputs {
            com_warn!(func.location,"Error: Mismatched types for output");
            for typ in rs_stack.iter() {
                eprintln!("   {}",typ.to_string(false).to_uppercase());
            }

            eprintln!("Expected: ");
            for typ in func.contract.Outputs.iter() {
                eprintln!("   {}",typ.to_string(false).to_uppercase());
            }
            exit(1)
        }
    }
}



fn usage(program: &String) {
    println!("--------------------------------------------");
    println!("{} (output language) (input path) [flags]",program);
    println!("     Output Language: ");
    println!("         - nasm_x86_64");
    println!("     flags: ");
    println!("         -o (output path)                    -> outputs to that file (example: hello.asm in nasm_x86_64 mode). If the output path is not specified it defaults to the modes default (for nasm_x86_64 thats a.asm)");
    println!("         -r                                  -> builds the program for you if the option is available for that language mode (for example in nasm_x86_64 it calls nasm with gcc to link it to an executeable)");
    println!("         -noRaxWarn                          -> removes the RAX usage warning for nasm");
    println!("         -release                            -> builds the program in release mode");
    println!("         -ntc                                -> (NoTypeChecking) Disable type checking");
    println!("         -nou (all, funcs, externs, strings) -> Disable unused warns for parameter");
    println!("         -callstack (size)                   -> Set callstack size. The name is very deceiving but callstack is now only used for locals as of 0.0.6A (checkout versions.md)");
    println!("--------------------------------------------");
}
fn dump_tokens(lexer: &mut Lexer) {
    while let Some(token) = lexer.next() {
        println!("Token: {:?}",token);
    }
}
fn main() {
    let mut args: Vec<_> = env::args().collect();
    let program_n = args.remove(0);
    if args.len() < 2 {
        usage(&program_n);
        exit(1);
    }
    let mut program = CmdProgram::new();
    program.typ  = args.remove(0);
    program.path = args.remove(0);
    program.opath = "a.asm".to_string();
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
                "-r" => {
                    program.should_build = true
                }
                "-release" => {
                    program.in_mode = OptimizationMode::RELEASE
                }
                "-noRaxWarn" => {
                    program.warn_rax_usage = false
                }
                "-ntc" => {
                    program.use_type_checking = false
                }
                "-nou" => {
                    let typ = args.get(i+1).expect("Error: Expected `all, funcs,externs,strings`");
                    match typ.as_str() {
                        "all" => {
                            program.print_unused_warns = false
                        }
                        "funcs" => {
                            program.print_unused_funcs = false
                        }
                        "externs" => {
                            program.print_unused_externs = false
                        }
                        "strings" => {
                            program.print_unused_strings = false
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
                flag => {
                    eprintln!("Error: undefined flag: {flag}\nUsage: ");
                    usage(&program_n);
                    exit(1);
                }
            }
            i+=1;
        }
    }
    match program.typ.as_str() {
        "nasm_x86_64" => {}
        _ => {
            assert!(false, "Undefined type: {}",program.typ);          
        }
    }
    let mut Intrinsics: HashMap<String,IntrinsicType> = HashMap::new();
    Intrinsics.insert("extern".to_string(), IntrinsicType::Extern);
    Intrinsics.insert("rs".to_string(), IntrinsicType::RS);
    Intrinsics.insert("let".to_string(), IntrinsicType::Let);
    Intrinsics.insert("func".to_string()  , IntrinsicType::Func);
    Intrinsics.insert("include".to_string(), IntrinsicType::INCLUDE);
    Intrinsics.insert("const".to_string(), IntrinsicType::CONSTANT);
    Intrinsics.insert("interrupt".to_string(), IntrinsicType::INTERRUPT);
    Intrinsics.insert("(".to_string(),IntrinsicType::OPENPAREN);
    Intrinsics.insert(")".to_string(),IntrinsicType::CLOSEPAREN);
    Intrinsics.insert(":".to_string(),IntrinsicType::DOUBLE_COLIN);
    Intrinsics.insert(",".to_string(),IntrinsicType::COMA);
    Intrinsics.insert(";".to_string(),IntrinsicType::DOTCOMA);
    Intrinsics.insert("{".to_string(),IntrinsicType::OPENCURLY);
    Intrinsics.insert("}".to_string(),IntrinsicType::CLOSECURLY);
    Intrinsics.insert("push".to_string(),IntrinsicType::PUSH);
    Intrinsics.insert("pop".to_string(),IntrinsicType::POP);
    Intrinsics.insert("=".to_string(),IntrinsicType::SET);
    Intrinsics.insert("+".to_string(),IntrinsicType::ADD);
    Intrinsics.insert("-".to_string(),IntrinsicType::SUB);
    Intrinsics.insert("*".to_string(),IntrinsicType::MUL);
    Intrinsics.insert("/".to_string(),IntrinsicType::DIV);
    Intrinsics.insert("ret".to_string(),IntrinsicType::RET);
    Intrinsics.insert("if".to_string(), IntrinsicType::IF);
    Intrinsics.insert("else".to_string(), IntrinsicType::ELSE);
    Intrinsics.insert("==".to_string(),IntrinsicType::EQ);
    let mut Definitions: HashMap<String,VarType> = HashMap::new();
    Definitions.insert("int".to_string(), VarType::INT);
    Definitions.insert("char".to_string(), VarType::CHAR);
    Definitions.insert("long".to_string(), VarType::LONG);
    Definitions.insert("bool".to_string(), VarType::BOOLEAN);
    Definitions.insert("ptr".to_string(), VarType::PTR);
    Definitions.insert("short".to_string(), VarType::SHORT);
    Definitions.insert("str".to_string(), VarType::STR);
    let info = fs::read_to_string(&program.path).expect("Error: could not open file!");
    let mut lexer = Lexer::new(&info, & Intrinsics, &Definitions);
    // dump_tokens(&mut lexer);
    // exit(1);
    lexer.currentLocation.file = Rc::new(program.path.clone());
    let mut build = parse_tokens_to_build(&mut lexer, &mut program);
    //panic!("Build: {:#?}",build);
    if program.use_type_checking {
        type_check_build(&mut build, &program);
    }
    match program.typ.as_str() {
        "nasm_x86_64" => {
            to_nasm_x86_64(&mut build, &program).expect("Could not build to nasm_x86_64");
            if program.should_build {
                println!("-------------");
                println!("   * nasm -f elf {}",program.opath.as_str());
                let nasm = Command::new("nasm").args(["-f","elf",program.opath.as_str()]).output().expect("Could not build nasm!");
                println!("   * gcc {}",[Path::new(program.opath.as_str()).with_extension("o").to_str().unwrap(),"-o",Path::new(program.opath.as_str()).with_extension("").to_str().unwrap()].join(" "));
                let gcc  = Command::new("gcc").args([Path::new(program.opath.as_str()).with_extension("o").to_str().unwrap(),"-o",Path::new(program.opath.as_str()).with_extension("").to_str().unwrap()]).output().expect("Could not build gcc!");
                println!("   * ld {}",Path::new(program.opath.as_str()).with_extension("").to_str().unwrap());
                let _ld  = Command::new("ld".to_string()).arg(Path::new(program.opath.as_str()).with_extension("").to_str().unwrap()).output().expect("Could not build your program!");
                if !nasm.status.success() {
                    println!("--------------");
                    println!("Nasm: \n{:?}\n-----------",nasm);
                    println!("--------------");
                }
                else if !gcc.status.success() {
                    println!("--------------");
                    println!("Gcc:  \n{:?}",gcc);
                    println!("--------------");
                }
                else {
                    println!("--------------");
                    println!("   - Finished build successfully");
                    println!("--------------");
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
        }
        _ => {
            todo!("Unimplemented type {}",program.typ);
        }
    }
}
