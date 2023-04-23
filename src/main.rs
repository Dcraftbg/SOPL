#![allow(non_snake_case)] 
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(unreachable_patterns)]

use std::{env, process::{exit, Command}, path::{Path, PathBuf}, ffi::OsStr, str::FromStr, collections::{HashMap, HashSet}, hash::Hash, fs::{File, self}, io::{Read, Write, self}, fmt::format, os::windows::{process::CommandExt, self}, arch::x86_64::_mm_testz_pd, borrow::BorrowMut, clone};
use uuid::Uuid;

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
        //eprintln!("(P) [ERROR] {}:{}:{}: {}", $token.location.file, $token.location.linenumber, $token.location.character, message);
        //exit(1);
    });
}
macro_rules! com_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(C) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
        //eprintln!("(P) [ERROR] {}:{}:{}: {}", $token.location.file, $token.location.linenumber, $token.location.character, message);
        //exit(1);
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




fn unescape(stri: &String) -> String {
    let mut out = String::new();
    let mut chars = stri.chars().into_iter();
    while let Some(chr) = chars.next(){
        if chr == '\\' {
            //let nc = stri.chars().next();
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

#[derive(PartialEq)]
enum OptimizationMode {
    RELEASE,
    DEBUG
}
struct CmdProgram {
    path: String,
    opath: String,
    typ: String,
    should_build: bool,
    warn_rax_usage: bool,
    use_type_checking: bool,
    print_unused_warns: bool,
    in_mode: OptimizationMode,
    call_stack_size: usize
}
impl CmdProgram {
    fn new() -> Self {
        Self { path: String::new(), opath: String::new(), should_build: false, typ: String::new(), warn_rax_usage: true, call_stack_size: 64000, in_mode: OptimizationMode::DEBUG, use_type_checking: true, print_unused_warns: true }
    }
}
#[repr(u32)]
#[derive(Clone, Copy,Debug,PartialEq )]

enum IntrinsicType {
    Extern = 0,
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
        }
    }
}
// static mut Intrinsic: HashMap<&str,IntrinsicType> = {
//     let mut map:HashMap<&str,IntrinsicType> = HashMap::new();

//     map.insert("extern", IntrinsicType::Extern);
//     map
// };
//mut Intrinsics: HashMap<&str,IntrinsicType> = HashMap::new();
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
    file: String,
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
    source: String,
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
        while self.is_not_empty() && self.source.chars().nth(self.cursor).unwrap().is_whitespace() {
            //println!("Skipping '{}'",self.source.chars().nth(self.cursor).unwrap());
            if self.source.chars().nth(self.cursor).unwrap() == '\n' {
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
    fn new(source: String, Intrinsics: &'a HashMap<String, IntrinsicType>, Definitions: &'a HashMap<String,VarType>) -> Self {
        Self { 
            source: source.clone(), 
            cursor: 0, 
            currentLocation: ProgramLocation { file: String::new(), linenumber: 1, character: 0 },
            Intrinsics,
            Definitions,
        }
    }
}
impl Iterator for Lexer<'_> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        // while char::from_u32(*self.source.get(self.cursor as usize).unwrap_or_else(|| exit(1))).unwrap_or_else(|| exit(1)).is_whitespace() {
        //     self.cursor+=1;    
        // }
        // None
        self.trim_left();
        let mut outstr: String = Default::default();
        //println!("Next {}",self.source.chars().nth(self.cursor).unwrap());
        //if self.is_not_empty() && self.source.chars().nth(self.cursor).unwrap()
        
        if self.is_not_empty() {
            //println!("'{}'Cursor: {}, CurrentLoc.char: {}",self.source.chars().nth(self.cursor).unwrap(),self.cursor, self.currentLocation.loc_display());
            let c = self.source.chars().nth(self.cursor).unwrap();
            
            match c {
                '\"' => {
                    let mut shouldIgnoreNext: bool = true;
                    self.cursor += 1;
                    self.currentLocation.character += 1;
                    //println!("Is not empty: {}",self.is_not_empty());
                    //println!("This stuffs: {}",(self.source.chars().nth(self.cursor).unwrap() != '\"' && shouldIgnoreNext));
                    while self.is_not_empty() && if !shouldIgnoreNext {self.source.chars().nth(self.cursor).unwrap() != '\"'} else {true} {
                        let c = self.source.chars().nth(self.cursor).unwrap();
                        //println!("Parsing: {c}");
                        // if c.is_alphabetic() {
                        //     isalphabetic = true;
                        // }
                        if shouldIgnoreNext {
                            shouldIgnoreNext = false
                        }
                        if c == '\\' {
                            shouldIgnoreNext = true
                        }
                        outstr.push(self.source.chars().nth(self.cursor).unwrap());
                        self.cursor+=1;
                        self.currentLocation.character += 1;
                    }
                    //println!("Parsing string: {}",outstr);
                    if self.is_not_empty() {
                        let u_outstr = unescape(&outstr);
                        self.cursor += 1;
                        self.currentLocation.character += 1;
                        if self.source.chars().nth(self.cursor).unwrap() == 'c' {
                            self.cursor += 1;
                            self.currentLocation.character += 1;
                            return Some(Token { typ: TokenType::CStringType(u_outstr.clone()), location: self.currentLocation.clone() });
                        }
                        else {
                            return Some(Token { typ: TokenType::StringType(u_outstr.clone()), location: self.currentLocation.clone() });
                        }
                    }
                }
                '\'' => {
                    let mut shouldIgnoreNext: bool = true;
                    self.cursor += 1;
                    self.currentLocation.character += 1;
                    while self.is_not_empty() && (self.source.chars().nth(self.cursor).unwrap() != '\'' && shouldIgnoreNext){
                        let c = self.source.chars().nth(self.cursor).unwrap();
                        // if c.is_alphabetic() {
                        //     isalphabetic = true;
                        // }
                        if shouldIgnoreNext {
                            shouldIgnoreNext = false
                        }
                        if c == '\\' {
                            shouldIgnoreNext = true
                        }
                        else {
                            outstr.push(self.source.chars().nth(self.cursor).unwrap());
                        }
                        self.cursor+=1;
                        self.currentLocation.character += 1;
                    }
                    if outstr.len() > 1 {
                        eprintln!("Error: undefined char type! \'{}\'\n[NOTE] Consider changing to string\n",outstr);
                        exit(1);
                    }
                    return Some(Token { typ: TokenType::CharType(outstr.clone()), location: self.currentLocation.clone() });
                    //return Some(Token { typ: TokenType::StringType(outstr.clone()), location: self.currentLocation.clone() });
                }
                '/' => {
                    self.cursor += 1;
                    if let Some(nc) = self.source.chars().nth(self.cursor+1) {
                        if nc == '/' {
                            self.cursor += 1;
                            while self.is_not_empty() && self.source.chars().nth(self.cursor).unwrap() != '\n' {
                                //println!("Char: '{}'",self.source.chars().nth(self.cursor).unwrap());
                                self.currentLocation.character += 1;
                                self.cursor += 1;
                            }
                            self.currentLocation.character = 0;
                            return self.next();
                        }
                        else {
                            self.cursor += 1;
                            return Some(Token { typ: TokenType::IntrinsicType(IntrinsicType::DIV), location: self.currentLocation.clone() });
                        }
                    }
                    else {
                        panic!("Error: Abruptly ran out of chars!");
                    }
                    
                }
                _    => {
                    let _orgcursor = self.cursor;
                    if !self.source.chars().nth(self.cursor).unwrap().is_alphanumeric() {
                        // ->
                        // ()
                        outstr.push(c);
                        self.cursor += 1;
                        self.currentLocation.character += 1;
                        //println!("-----\nCurrent char: {} and C is {}",self.source.chars().nth(self.cursor).unwrap(), c);
                        while self.is_not_empty() && (self.source.chars().nth(self.cursor).unwrap() != '(' && self.source.chars().nth(self.cursor).unwrap() != ')' && self.source.chars().nth(self.cursor).unwrap() != '[' && self.source.chars().nth(self.cursor).unwrap() != ']') && !self.source.chars().nth(self.cursor).unwrap().is_alphanumeric() && !self.source.chars().nth(self.cursor).unwrap().is_whitespace() {
                            outstr.push(self.source.chars().nth(self.cursor).unwrap());
                            self.cursor+=1;
                            self.currentLocation.character += 1;
                        }
                        
                        //println!("------\nStr: '{}'",outstr);
                        
                        if let Some(o) = self.Intrinsics.get(&outstr) {
                            //println!("Got intrinsic: {}",o.to_string(false));
                            return Some(Token { typ: TokenType::IntrinsicType(o.clone()), location: self.currentLocation.clone() });
                        }
                        else{
                            //println!("Got word: {}",outstr);
                            return Some(Token { typ: TokenType::WordType(outstr.clone()), location: self.currentLocation.clone() });
                        }
                    }
                    while self.is_not_empty() && self.source.chars().nth(self.cursor).unwrap().is_alphanumeric(){
                        let _c = self.source.chars().nth(self.cursor).unwrap();
                        // if c.is_alphabetic() {
                        //     isalphabetic = true;
                        // }
                        outstr.push(self.source.chars().nth(self.cursor).unwrap());
                        self.cursor+=1;
                        self.currentLocation.character += 1;
                    }       
                    if c.is_alphabetic(){
                        if let Some(o) = self.Intrinsics.get(&outstr){                        
                            return Some(Token { typ: TokenType::IntrinsicType((*o).clone()), location: self.currentLocation.clone() });
                        }
                        else if let Some(o) = self.Definitions.get(&outstr){                        
                            return Some(Token { typ: TokenType::Definition((*o).clone()), location: self.currentLocation.clone() });
                        }
                        else {
                            return Some(Token { typ: TokenType::WordType(outstr), location: self.currentLocation.clone() });
                        }
                    }
                    else {
                        //println!("C: {}, Outstr: {}",c,outstr);
                        let nc = self.source.chars().nth(self.cursor-1);
                        if nc.is_some() {
                            //println!("Some data {}",nc.unwrap());
                            //println!("{}",outstr);
                            if nc.unwrap() == 'l' {
                                outstr.pop();
                                let num = outstr.parse::<i64>();
                                if num.is_ok(){
                                    return Some(Token { typ: TokenType::Number64(num.unwrap()), location: self.currentLocation.clone() });
                                }
                                else {
                                    println!("INVALID Integer for 64 (long) size!");
                                }
                            }
                        }
                        let num = outstr.parse::<i32>();
                        //println!("got here: '{}'",outstr);
                        if num.is_ok(){
                            return Some(Token { typ: TokenType::Number32(num.unwrap()), location: self.currentLocation.clone() });
                        }
                        else {
                            //println!("C: {}",c);
                            todo!()
                            //todo!("Unimplemented! for \"{}\"",outstr);
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
    RawExternal(String),
    CExternal(String)
}
#[derive(Debug)]
struct External {
    typ: ExternalType,
    loc: ProgramLocation
}
impl ExternalType {
    fn prefix(&self) -> String {
        match self {
            ExternalType::RawExternal(_) => String::new(),
            ExternalType::CExternal(_) => "_".to_string(),
            _ => String::new()
        }
    }
    fn suffix(&self) -> String {
        match self {
            ExternalType::RawExternal(_) => String::new(),
            ExternalType::CExternal(_)   => String::new(),
            _ => String::new()
        }
    }
    fn to_string(&self) -> String{
        match self {
            ExternalType::RawExternal(_) => "RawExternal".to_string(),
            ExternalType::CExternal(_) => "CExternal".to_string()
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
    LOCALVAR (String),
    RAW      (i64),
    STR      (Uuid, ProgramStringType)
    // etc.

}
#[derive(Debug)]
enum Instruction {
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
    CONDITIONAL_JUMP(usize),
    JUMP(usize),
    INTERRUPT(i64),
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
#[derive(Debug)]
struct Function {
    contract: FunctionContract,
    locals: HashMap<String,LocalVariable>,
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
                        //return Ok(ConstValueType::STR(val.to_string()+nval));
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
                        //return Ok(ConstValueType::STR(val.to_string()+nval));
                    },
                }
            }
            ConstValueType::STR(_) => {
                match Other {
                    // TODO:
                    // Implement:
                    // const HelloFive "Hello" 5 *;  // HelloHelloHelloHelloHello
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
                        //return Ok(ConstValueType::STR(val.to_string()+nval));
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
                        //return Ok(ConstValueType::STR(val.to_string()+nval));
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
    externals:    Vec<External>,
    functions:    HashMap<String, Function>,
    stringdefs:   HashMap<Uuid,ProgramString>,
    constdefs:    HashMap<String, RawConstValue>
}
// Functions: Vec<Function>

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
    fn get_size(&self) -> usize{
        match self {
            VarType::CHAR => 1,
            VarType::SHORT => 2,
            VarType::BOOLEAN => 1,
            VarType::INT => 4,
            VarType::LONG => 8,
            VarType::STR => 16,
            VarType::PTR => 8,
            VarType::CUSTOM(_) => todo!(),
        }
    }
}
#[derive(Debug)]
enum ScopeOpenerType {
    FUNC,
    IF,
    EMPTY,
    ELSE,
}
#[derive(Debug)]
struct ScopeOpener {
    cinstruct_size: usize,
    hasBeenOpened: bool,
    typ:   ScopeOpenerType
}
#[derive(Debug)]
struct FunctionContract {
    Inputs: Vec<VarType>,
    Outputs: Vec<VarType>
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
                //varStack.push(def.clone());
                match def.typ {
                    RawConstValueType::INT(val)  => {
                        varStack.push(ConstValueType::INT(val.clone()));
                    }
                    RawConstValueType::LONG(val) => {
                        varStack.push(ConstValueType::LONG(val.clone()));
                    }
                    RawConstValueType::STR(ref UUID)       => {
                        varStack.push(ConstValueType::STR(build.stringdefs.get(UUID).unwrap().Data.clone()));
                    }
                }
            }
            TokenType::StringType(word) => {
                // let mut UUID = Uuid::new_v4();
                // while build.stringdefs.insert(UUID,ProgramString {Data: word.clone(), Typ: ProgramStringType::STR}).is_some() {
                //     UUID = Uuid::new_v4();
                // }
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
    //lpar_error!(lexer.currentLocation,"Error:")
    lpar_assert!(lexer.currentLocation,varStack.len() == 1,"Error: Lazy constant stack handling! You need to correctly handle your constants");
    ConstValue {typ: varStack.pop().unwrap(), loc: orgLoc}
}
fn parse_function_contract(lexer: &mut Lexer) -> FunctionContract {
    let mut out = FunctionContract {Inputs: vec![], Outputs: vec![]};
    let mut is_input = true;
    let first = lexer.next();
    let first = par_expect!(lexer.currentLocation,first,"abruptly ran out of tokens in function contract");//first.expect("Error: abruptly ran out of tokens in function contract");
    let mut expectNextSY = false;
    //println!("Parsing function contract....");
    match first.typ {
        TokenType::IntrinsicType(typ) => {
            match typ {
                IntrinsicType::OPENPAREN => {
                    //println!("Got open paren!")
                },
                Other => {
                    par_error!(first,"INVALID TOKEN FOR PARSING, Expected an Open paren but found other {}",Other.to_string(false));
                    // TODO: add to_string for IntrinsicType
                    //assert!(false, "INVALID TOKEN FOR PARSING, Expected an Open paren but found other {}",Other.to_string(false));
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
            TokenType::Definition(Def) => {
                expectNextSY = true;    
                if is_input {out.Inputs.push(Def.clone())} else {out.Outputs.push(Def.clone())};   
            }
            _ => {
                par_error!(token, "Unexpected Token Type in Function Contract. Expected Definition but found: {}",token.typ.to_string(false))
            }
        }
    }
    out
}
fn parse_tokens_to_build(lexer: &mut Lexer, program: &mut CmdProgram) -> BuildProgram {
    let mut build: BuildProgram = BuildProgram { externals: vec![], functions: HashMap::new(),stringdefs: HashMap::new(), constdefs: HashMap::new()};
    let mut scopeStack: Vec<ScopeOpener> = vec![];
    let mut currentFunction: Option<String> = None;
    while let Some(token) = lexer.next(){
        match token.typ {
            TokenType::WordType(ref word) => {
                //HashMap<String, ExternalType>
                // TODO: Introduce some sort of hashmap for this
                par_assert!(token,currentFunction.is_some(), "Undefined Word Call outside of entry point! '{}'",word);
                let mut isvalid = false;
                for external in build.externals.iter() {
                    match external.typ {
                        ExternalType::RawExternal(ref ext) => {
                            if word == ext {
                                isvalid = true;
                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::CALLRAW(ext.clone())));
                                break;
                            }
                        }
                        ExternalType::CExternal(ref ext) => {
                            if word == ext {
                                isvalid = true;
                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::CALLRAW(external.typ.prefix().clone()+&ext.clone()+&external.typ.suffix())));
                                break;
                            }
                        }
                    }
                }
                if let Some(reg) = Register::from_string(&word) {
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
                                    build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(reg))));
                                }
                                IntrinsicType::PUSH => {
                                    build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::PUSH(OfP::REGISTER(reg))));
                                }
                                IntrinsicType::SET => {
                                    let token = par_expect!(lexer.currentLocation,lexer.next(),"abruptly ran out of tokens");//.expect(&format!("abruptly ran out of tokens"));
                                    match token.typ {
                                        TokenType::Number32(data) => {
                                            if reg.size() >= 4 {
                                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location,Instruction::MOV(OfP::REGISTER(reg), OfP::RAW(data as i64))))
                                            }
                                        }
                                        TokenType::Number64(data) => {
                                            if reg.size() >= 8 {
                                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location,Instruction::MOV(OfP::REGISTER(reg), OfP::RAW(data))))
                                            }
                                        }
                                        _ => par_error!(token,"Unexpected Type for Mov Intrinsic. Expected Number32/Number64 but found {}",token.typ.to_string(false))//token.location.par_error(&format!("Unexpected Type for Mov Intrinsic. Expected Number32/Number64 but found {}",other.to_string(false)))
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
                                match regOp.typ {
                                    TokenType::IntrinsicType(typ) => {
                                        
                                        match typ {
                                            IntrinsicType::ADD => {
                                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::ADD(OfP::REGISTER(reg), OfP::REGISTER(reg2))))
                                            }
                                            IntrinsicType::SUB => {
                                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::SUB(OfP::REGISTER(reg), OfP::REGISTER(reg2))))
                                            }
                                            IntrinsicType::MUL => {
                                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::MUL(OfP::REGISTER(reg), OfP::REGISTER(reg2))))
                                            }
                                            IntrinsicType::EQ => {
                                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::EQUALS(OfP::REGISTER(reg), OfP::REGISTER(reg2))))
                                            }
                                            IntrinsicType::DIV => {
                                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::DIV(OfP::REGISTER(reg), OfP::REGISTER(reg2))))
                                            }
                                            IntrinsicType::SET => {
                                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::MOV(OfP::REGISTER(reg), OfP::REGISTER(reg2))))
                                            }
                                            other => par_error!(token,"Unexpected Intrinsic! Expected Register Intrinsic but found {}",other.to_string(false))
                                        }
                                    }
                                    other => {
                                        par_error!(token, "Unexpected token type: Expected Intrinsic but found {}",other.to_string(false));
                                    }
                                }
                            }
                            else{
                                par_error!(token, "unknown word {} found after Register operation",Word);
                            }
                        }
                        typ => {
                            par_error!(token,"Unexpected register operation! Expected Intrinsic or another Register but found {}",typ.to_string(false));
                        }
                    }
                }
                if !isvalid {
                    let func = build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap();
                    isvalid = func.locals.contains_key(word);
                    if isvalid {
                        let regOp = par_expect!(lexer.currentLocation,lexer.next(),"Unexpected variable operation or another variable!");
                        match regOp.typ {
                            TokenType::IntrinsicType(Typ) => {
                                match Typ {
                                    IntrinsicType::POP => {
                                        func.body.push((regOp.location,Instruction::POP(OfP::LOCALVAR(word.clone()))));
                                    },
                                    IntrinsicType::PUSH => {
                                        func.body.push((regOp.location,Instruction::PUSH(OfP::LOCALVAR(word.clone()))));
                                    },
                                    IntrinsicType::SET => {
                                        let operand = par_expect!(lexer.currentLocation,lexer.next(),"Expected another variable, a constant integer or a (string: not yet implemented)!");
                                        match operand.typ {
                                            TokenType::WordType(ref other) => {
                                                //par_assert!(operand,func.locals.contains_key(&other),"Could not find variable {}",other);
                                                if func.locals.contains_key(other) {
                                                    func.body.push((operand.location,Instruction::MOV(OfP::LOCALVAR(word.clone()), OfP::LOCALVAR(other.clone()))))
                                                }
                                                else if let Some(reg) = Register::from_string(&other) {
                                                    par_assert!(operand,reg.size() == func.locals.get(word).unwrap().typ.get_size(), "Register assigned to differently sized variable, Variable size: {}, Register size: {}",reg.size(),func.locals.get(word).unwrap().typ.get_size());
                                                    func.body.push((operand.location,Instruction::MOV(OfP::LOCALVAR(word.clone()), OfP::REGISTER(reg))))
                                                }
                                                else {
                                                    par_error!(operand,"Could not find variable or register {}",other);
                                                }
                                            },
                                            TokenType::StringType(_) => todo!("Implement strings with local variables"),
                                            TokenType::Number32(val) => {
                                                func.body.push((operand.location,Instruction::MOV(OfP::LOCALVAR(word.clone()), OfP::RAW(val as i64))))
                                            },
                                            TokenType::Number64(val) => {
                                                func.body.push((operand.location,Instruction::MOV(OfP::LOCALVAR(word.clone()), OfP::RAW(val))))
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
                                if func.locals.contains_key(&Word) {
                                    //par_assert!(token,reg.size()==reg2.size(),"Gotten two differently sized registers to one op!");
                                    let regOp = lexer.next().expect(&format!("(P) [ERROR] {}:{}:{}: Unexpected register operation!",token.location.clone().file,&token.location.clone().linenumber,&token.location.clone().character));
                                    match regOp.typ {
                                        TokenType::IntrinsicType(typ) => {
                                            
                                            match typ {
                                                IntrinsicType::ADD => {
                                                    build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::ADD(OfP::LOCALVAR(word.clone()), OfP::LOCALVAR(Word.clone()))))
                                                }
                                                IntrinsicType::SUB => {
                                                    build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::SUB(OfP::LOCALVAR(word.clone()), OfP::LOCALVAR(Word.clone()))))
                                                }
                                                IntrinsicType::MUL => {
                                                    build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::MUL(OfP::LOCALVAR(word.clone()), OfP::LOCALVAR(Word.clone()))))
                                                }
                                                IntrinsicType::EQ => {
                                                    build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::EQUALS(OfP::LOCALVAR(word.clone()), OfP::LOCALVAR(Word.clone()))))
                                                }
                                                IntrinsicType::DIV => {
                                                    build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::DIV(OfP::LOCALVAR(word.clone()), OfP::LOCALVAR(Word.clone()))))
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
                    isvalid = build.functions.contains_key(word);
                    if isvalid {
                        let func = build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap();
                        func.body.push((token.location.clone(),Instruction::CALL(word.clone())));
                        //func.body.push((token.location,Instruction::FNBEGIN());
                    }
                }
                if !isvalid  {
                    isvalid = build.constdefs.contains_key(word);
                    if isvalid {
                        let func = build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap();
                        let cons = build.constdefs.get(word).unwrap();
                        match cons.typ {
                            RawConstValueType::INT(ref val) => {
                                func.body.push((token.location.clone(),Instruction::MOV(OfP::REGISTER(Register::RAX), OfP::RAW(*val as i64))));
                                func.body.push((token.location.clone(),Instruction::PUSH(OfP::REGISTER(Register::RAX))));
                            }
                            RawConstValueType::LONG(ref val) => {
                                func.body.push((token.location.clone(),Instruction::MOV(OfP::REGISTER(Register::RAX), OfP::RAW(*val as i64))));
                                func.body.push((token.location.clone(),Instruction::PUSH(OfP::REGISTER(Register::RAX))));
                            }
                            RawConstValueType::STR(ref val) => {
                                func.body.push((token.location.clone(),Instruction::PUSH(OfP::STR(val.clone(), ProgramStringType::STR))));
                            }
                        }
                    }
                }
                par_assert!(token,isvalid,"Unknown word type: {}",word);
            }
            TokenType::IntrinsicType(Type) => {
                match Type {
                    IntrinsicType::Extern => {
                        let externType = lexer.next();
                        let externType = externType.expect("Error: Unexpected abtrupt end of tokens in extern");
                        match externType.typ {
                            TokenType::WordType(Word) => {
                                build.externals.push(External { typ: ExternalType::RawExternal(Word), loc: externType.location.clone()});
                            }
                            TokenType::IntrinsicType(_) => assert!(false,"Unexpected behaviour! expected type Word or String but found Intrinsic"),
                            TokenType::StringType(Type) => {
                                match Type.as_str() {
                                    "C" => {
                                        let externWord = lexer.next();
                                        let externWord = externWord.expect("Error: C type extern defined but stream of tokens abruptly ended!");
                                        match externWord.typ {
                                            TokenType::WordType(Word) => {
                                                build.externals.push(External { typ: ExternalType::CExternal(Word), loc: externWord.location.clone()})
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
                        //println!("Got func!!!!!!");
                        let funcName = lexer.next();
                        let funcName = par_expect!(lexer.currentLocation,funcName,"Unexpected abtrupt end of tokens in func");//.expect("Error: Unexpected abtrupt end of tokens in func");
                        match funcName.typ {
                            TokenType::WordType(Word) => {
                                //println!("Func name {}",Word);
                                par_assert!(token,build.functions.get(&Word).is_none(),"Multiply defined symbols {}!",Word);
                                let _contract = parse_function_contract(lexer);
                                currentFunction = Some(Word.clone());
                                if Word != "main" {
                                    build.functions.insert(Word.clone(), Function { contract: _contract, body: vec![(token.location.clone(),Instruction::FNBEGIN())], location: token.location.clone(), locals: HashMap::new() });
                                }
                                else {
                                    build.functions.insert(Word.clone(), Function { contract: _contract, body: vec![], location: token.location.clone(), locals: HashMap::new() });
                                }
                                scopeStack.push(ScopeOpener{typ: ScopeOpenerType::FUNC, hasBeenOpened: false, cinstruct_size: 0});
                                // TODO: push func onto the scope stack
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
                            //s.hasBeenOpened = !s.hasBeenOpened;
                            par_assert!(token,!s.hasBeenOpened, "Scope already opened! {:?}",scopeStack);
                            s.hasBeenOpened = true;
                            let currentFunc = build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap();
                            
                            match s.typ {
                                ScopeOpenerType::FUNC => {}
                                ScopeOpenerType::IF => {
                                    s.cinstruct_size = currentFunc.body.len();
                                    currentFunc.body.push(( token.location.clone(),Instruction::CONDITIONAL_JUMP(currentFunc.body.len()+2) ));
                                    currentFunc.body.push(( token.location.clone(),Instruction::JUMP(0) ));
                                }
                                ScopeOpenerType::EMPTY => {}
                                ScopeOpenerType::ELSE => {},
                            }
                            currentFunc.body.push(( token.location.clone(),Instruction::SCOPEBEGIN));
                        }
                        else {
                            let currentFunc = build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap();
                            currentFunc.body.push(( token.location.clone(), Instruction::SCOPEBEGIN));
                            scopeStack.push( ScopeOpener { hasBeenOpened: true, typ: ScopeOpenerType::EMPTY, cinstruct_size: build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.len() });
                        }


                        // TODO: I don't know if I should put something here or not
                    }
                    IntrinsicType::CLOSECURLY => {
                        if let Some(sc) = scopeStack.pop(){
                            par_assert!(token,sc.hasBeenOpened, "Error: scope closed but never opened!");
                            
                            let currentFunc = build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap();
                            match sc.typ {
                                ScopeOpenerType::FUNC => {
                                    build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::SCOPEEND));
                                    currentFunction = None;
                                },
                                ScopeOpenerType::IF => {
                                    #[allow(unused_assignments)]
                                    let mut len: usize = 0;
                                    {
                                        let body = &mut currentFunc.body;
                                        
                                        //let l = currentFunc.body.get_mut(sc.cinstruct_size);
                                        //println!("{:#?}",body);
                                        if let Some(ntok) = lexer.peekable().peek() {
                                            match ntok.typ {
                                                TokenType::IntrinsicType(typ) => {
                                                    match typ {
                                                        IntrinsicType::ELSE => {
                                                            body.push((token.location.clone(),Instruction::JUMP(0)));
                                                            scopeStack.push( ScopeOpener { hasBeenOpened: false, typ: ScopeOpenerType::ELSE, cinstruct_size: body.len() });
                                                        },
                                                        _ => {}
                                                    }
                                                }
                                                _ => {}
                                            } 
                                        }
                                        len = body.len();
                                    }
                                    let body = &mut currentFunc.body;
                                    let (_, p) = par_expect!(token.location.clone(),body.get_mut(sc.cinstruct_size+1), "Error");
                                    match p {
                                        Instruction::JUMP(loc) => {
                                            *loc = len;
                                        }
                                        _ => par_error!(token,"Unexpected Instruction! This is probably due to a bug inside the program! {:?}",p)
                                    }

                                    build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::SCOPEEND))
                                },
                                ScopeOpenerType::EMPTY => {
                                    build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::SCOPEEND))
                                }
                                ScopeOpenerType::ELSE => {
                                    let body = &mut currentFunc.body;
                                    let len = body.len();
                                    //println!("[DEBUG] Len {} Trace: {:#?} Stringdefs: {:#?}",sc.cinstruct_size,body,build.stringdefs);
                                    let (_,toc) = body.get_mut(sc.cinstruct_size-1).unwrap();
                                    match toc {
                                        Instruction::JUMP(data) => {
                                            *data = len;
                                        }
                                        _ => {
                                            par_error!(token, "Invalid instruction for else token! Expected jump! This is probably due to a bug in the system {:?}",toc)
                                        }
                                    }
                                    build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::SCOPEEND))
                                },
                            }
                            
                        }
                        else {
                            par_error!(token, "Scope closed but never opened!!!");
                            //build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::SCOPEEND));
                        }
                    }
                    IntrinsicType::POP => {
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::RAX))));
                    }
                    IntrinsicType::PUSH => todo!(),
                    IntrinsicType::SET => todo!(),
                    // TODO: implement math ops
                    IntrinsicType::ADD => {
                        par_assert!(token,currentFunction.is_some(), "Unexpected ADD operation of entry point!");
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::EAX))));
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::R8D))));
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::ADD(OfP::REGISTER(Register::EAX), OfP::REGISTER(Register::R8D))))
                    }
                    IntrinsicType::SUB => {
                        par_assert!(token,currentFunction.is_some(), "Unexpected SUB operation of entry point!");
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::EAX))));
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::R8D))));
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::SUB(OfP::REGISTER(Register::EAX), OfP::REGISTER(Register::R8D))))
                    }
                    IntrinsicType::MUL => {
                        par_assert!(token,currentFunction.is_some(), "Unexpected MUL operation of entry point!");
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::EAX))));
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::R8D))));
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::MUL(OfP::REGISTER(Register::EAX), OfP::REGISTER(Register::R8D))))
                    }
                    IntrinsicType::RET => {
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::RET()));
                    },
                    IntrinsicType::INCLUDE => {
                        let includeName = par_expect!(lexer.currentLocation,lexer.next(),"Error: abruptly ran out of tokens");
                        match includeName.typ {
                            TokenType::StringType(path) => {
                                let p = PathBuf::from(&program.path);
                                let p = p.parent().unwrap();
                                let include_p  = PathBuf::from(path);
                                //println!("Path {}",String::from(p.join(&include_p).to_str().unwrap()).replace("\\", "/"));
                                let mut lf = Lexer::new(fs::read_to_string(String::from(p.join(&include_p).to_str().unwrap().replace("\\", "/"))).expect(&format!("Error: could not open file: {}",String::from(p.join(&include_p).to_str().unwrap()).replace("\\", "/"))),lexer.Intrinsics,lexer.Definitions);
                                lf.currentLocation.file = String::from(p.join(&include_p).to_str().unwrap().replace("\\", "/"));
                                let mut nprogram = CmdProgram::new();
                                nprogram.path = String::from(p.join(&include_p).to_str().unwrap().replace("\\", "/"));
                                let mut build2 = parse_tokens_to_build(&mut lf, &mut nprogram);
                                build.externals.extend(build2.externals);
                                for (strdefId,_strdef) in build2.stringdefs.iter() {
                                    let orgstrdefId  = strdefId.clone();
                                    let mut strdefId = strdefId.clone();
                                    //build.stringdefs.insert(strdefId.clone(), strdef.clone());
                                    let isContaining = build.stringdefs.contains_key(&strdefId);
                                    while build.stringdefs.contains_key(&strdefId) {
                                        strdefId = Uuid::new_v4();
                                        //let nid = Uuid::ge
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
                                                    Instruction::PUSH(strid) => {
                                                        match strid {
                                                            OfP::STR(strid,_) => {
                                                                if strid.clone() == orgstrdefId {
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
                                for (cn_name,cn_val) in build2.constdefs{
                                    match build.constdefs.insert(cn_name.clone(),cn_val.clone()) {
                                        Some(_Other) => {
                                            par_error!(cn_val.loc,"Error: mulitply defined symbols {}",cn_name);
                                        },
                                        None => {},
                                    }
                                }
                                //build.functions.extend(build2.functions)
                                //build.stringdefs.extend(build2.stringdefs);
                                
                            }
                            _ => {
                                par_error!(includeName, "Expected token type String but found {}",includeName.typ.to_string(false));
                            }
                        }
                    },
                    IntrinsicType::IF => {
                        scopeStack.push( ScopeOpener { hasBeenOpened: false, typ: ScopeOpenerType::IF, cinstruct_size: build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.len().clone() });
                    },
                    IntrinsicType::EQ => {
                        let currentFunc = build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap();
                        currentFunc.body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::RAX))));
                        currentFunc.body.push((token.location.clone(),Instruction::POP(OfP::REGISTER(Register::R8))));
                        currentFunc.body.push((token.location.clone(),Instruction::EQUALS(OfP::REGISTER(Register::RAX), OfP::REGISTER(Register::R8))));
                        
                    },
                    IntrinsicType::CONSTANT => {
                        let first = lexer.next();
                        let first = par_expect!(lexer.currentLocation,first,"abruptly ran out of tokens in constant name definition");//first.expect("Error: abruptly ran out of tokens in function contract");
                        let name: String = match first.typ {
                            TokenType::WordType(ref word) => {
                                par_assert!(first, !build.constdefs.contains_key(word) && !build.functions.contains_key(word), "Error: multiple constant symbol definitions");
                                word.to_string()
                            }
                            _ => {
                                par_error!(first, "Unexpected token type! Expected word but found {}",first.typ.to_string(false));
                            }
                        };
                        let first = lexer.next();
                        let first = par_expect!(lexer.currentLocation,first,"abruptly ran out of tokens in constant name definition");//first.expect("Error: abruptly ran out of tokens in function contract");
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
                                while build.stringdefs.insert(UUID,ProgramString {Data: rval.clone(), Typ: ProgramStringType::STR}).is_some() {
                                    UUID = Uuid::new_v4();
                                }
                                RawConstValue {typ: RawConstValueType::STR(UUID), loc: val.loc}
                                
                            }
                        });
                        
                    },
                    IntrinsicType::DOTCOMA => {},
                    IntrinsicType::ELSE => todo!(),
                    IntrinsicType::DIV => todo!("{:#?}",build),
                    IntrinsicType::Let => {
                        // let a: int = 5
                        let nametok = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for Let");

                        match nametok.typ {
                            TokenType::WordType(ref name) => {
                                par_assert!(nametok, !build.constdefs.contains_key(name) && !build.functions.contains_key(name), "Error: multiply defined symbols");
                                let currentFunc = build.functions.get_mut(&currentFunction.clone().expect("Todo: Global variables are not yet implemented :|")).unwrap();
                                let typ = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for let type");                                
                                par_assert!(typ,typ.typ==TokenType::IntrinsicType(IntrinsicType::DOUBLE_COLIN), "Error: You probably forgot to put a : after the name!");
                                let typ = par_expect!(lexer.currentLocation, lexer.next(), "Error: abruptly ran out of tokens for let type");
                                match typ.typ {
                                    TokenType::Definition(def) => {
                                        currentFunc.locals.insert(name.clone(), LocalVariable { typ: def, operand: 0 });
                                        currentFunc.body.push((lexer.currentLocation.clone(),Instruction::DEFVAR(name.clone())))
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
                        let currentFunc = build.functions.get_mut(&currentFunction.clone().expect("Error: Can not have interrupt outside of functions!")).unwrap();
                        let lexerNext = par_expect!(lexer.currentLocation,lexer.next(),"Stream of tokens ended abruptly at INTERRUPT call");
                        match lexerNext.typ {
                            TokenType::Number32(val) => {
                                currentFunc.body.push((lexer.currentLocation.clone(),Instruction::INTERRUPT(val as i64)));
                            }
                            TokenType::Number64(val) => {
                                currentFunc.body.push((lexer.currentLocation.clone(),Instruction::INTERRUPT(val)));
                            }
                            _ => {
                                par_error!(lexerNext, "Unexpected token type for INTERRUPT, {}",lexerNext.typ.to_string(false))
                            }
                        }
                        
                    },
                }
            }
            TokenType::StringType(ref Word) => {
                par_assert!(token,currentFunction.is_some(), "Unexpected string definition outside of entry point!");

                //build.stringdefs.push(ProgramString {Word.clone());
                let mut UUID = Uuid::new_v4();
                while build.stringdefs.insert(UUID,ProgramString {Data: Word.clone(), Typ: ProgramStringType::STR}).is_some() {
                    UUID = Uuid::new_v4();
                }
                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location,Instruction::PUSH(OfP::STR(UUID,ProgramStringType::STR))));
            }
            TokenType::CStringType(ref Word) => {
                par_assert!(token,currentFunction.is_some(), "Unexpected string definition outside of entry point!");

                //build.stringdefs.push(ProgramString {Word.clone());
                let mut UUID = Uuid::new_v4();
                while build.stringdefs.insert(UUID,ProgramString {Data: Word.clone(), Typ: ProgramStringType::CSTR}).is_some() {
                    UUID = Uuid::new_v4();
                }
                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location,Instruction::PUSH(OfP::STR(UUID,ProgramStringType::CSTR))));
            }
            
            TokenType::CharType(_) => {
                par_assert!(token,currentFunction.is_some(), "Unexpected char definition outside of entry point!");
            }
            TokenType::Number32(val) => {
                par_assert!(token,currentFunction.is_some(), "Unexpected number (64) definition outside of entry point!");
                // TODO: implement this
                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::MOV(OfP::REGISTER(Register::EAX), OfP::RAW(val as i64))));
                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::PUSH(OfP::REGISTER(Register::EAX))));
            }
            TokenType::Number64(val) => {
                par_assert!(token,currentFunction.is_some(), "Unexpected number (64) definition outside of entry point!");
                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::MOV(OfP::REGISTER(Register::RAX), OfP::RAW(val))));
                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::PUSH(OfP::REGISTER(Register::RAX))));
            }
            TokenType::Definition(_) => todo!(),
            TokenType::CStringType(_) => todo!(),
        }
    }
    //println!("Functions {:#?}",build.functions);
    for (fn_name, fn_fn)in build.functions.iter_mut() {
        if fn_name != "main" && fn_fn.location.file == lexer.currentLocation.file {
            //println!("-----------------\n\n\n\nAdded Ret for {} with body: {:#?}--------\n\n\n\n",fn_name,fn_fn.body);
            fn_fn.body.push((fn_fn.location.clone(),Instruction::RET()))
        }
    }
    build
}

struct optim_ops {
    should_use_callstack: bool,
    usedStrings: HashSet<Uuid>,
    usedExterns: HashSet<String>
}
impl optim_ops {
    fn new() -> Self{
        Self { should_use_callstack: false, usedStrings: HashSet::new(), usedExterns: HashSet::new() }
    }
}
fn optimization_ops(build: &mut BuildProgram, program: &CmdProgram) -> optim_ops{
    match program.in_mode {
        OptimizationMode::RELEASE => {
            let mut out = optim_ops::new();

            for (_, func) in build.functions.iter() {
                if !out.should_use_callstack {
                    for (_,op) in func.body.iter() {
                        match op {
                            Instruction::PUSH(d) => {
                                match d {
                                    OfP::STR(UUID,_) => {
                                        out.usedStrings.insert(UUID.clone());
                                    }
                                    _ => {}
                                }
                            }
                            Instruction::CALL(_) => {                                
                                out.should_use_callstack = true;
                                break
                            },
                            Instruction::DEFVAR(_) => {
                                out.should_use_callstack = true;
                                break
                            }
                            Instruction::CALLRAW(r) => {
                                out.usedExterns.insert(r.clone());
                            }
                            _ => {}
                        }
                    }
                }
                else {
                    break;
                }
            }
            out
        },
        OptimizationMode::DEBUG => optim_ops { should_use_callstack: true, usedStrings: HashSet::new(), usedExterns: HashSet::new()},
    }
}
fn to_nasm_x86_64(build: &mut BuildProgram, program: &CmdProgram) -> io::Result<()>{
    
    //
    // TODO: Finish work on used strings!
    //
    //
    //
    //
    


    
    
    let optimization = optimization_ops(build, program);
    let mut f = File::create(&program.opath).expect(&format!("Error: could not open output file {}",program.opath.as_str()));
    writeln!(&mut f,"BITS 64")?;
    writeln!(&mut f, "section .data")?;
    
    
    for (UUID,stridef) in build.stringdefs.iter(){                
        if program.in_mode == OptimizationMode::DEBUG || optimization.usedStrings.contains(UUID) {
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
        else {
            if program.print_unused_warns {
                println!("[NOTE] Unused string:   <{}> \"{}\" - This is probably due to a constant definition that was never used",UUID, stridef.Data.escape_default());
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
            writeln!(&mut f,"global F_{}",function_name)?;
        }
    }
    //writeln!(&mut f,"global _main")?;
    for exter in build.externals.iter() {
        match exter.typ {
            
            ExternalType::CExternal(ref Word) | ExternalType::RawExternal(ref Word) => {
                if program.in_mode == OptimizationMode::DEBUG || optimization.usedExterns.contains(&format!("{}{}{}",exter.typ.prefix(),Word,exter.typ.suffix())) {
                    writeln!(&mut f,"  extern {}{}{}",exter.typ.prefix(),Word,exter.typ.suffix())?;
                }
                else if program.print_unused_warns {
                    
                    println!("[NOTE] {}: Unused external: \"{}\"",exter.loc.loc_display(),Word)
                }
            },
        }
    }
    writeln!(&mut f, "section .text")?;
    // TODO: introduce something like mainMEM which won't be bound to the 640 000
    let mut callstack_size: i64 = 0;
    for (function_name,function) in build.functions.iter_mut() {
        let mut hasFoundRet = false;
        //writeln!(&mut f,"global _{}",function_name)?;
        if function_name == "main" {
            writeln!(&mut f, "_{}:",function_name)?;
            writeln!(&mut f, "   push rbp")?;
            writeln!(&mut f, "   mov rbp, rsp")?;
            if optimization.should_use_callstack {
                writeln!(&mut f, "   mov dword [_CALLSTACK_BUF_PTR], _CALLSTACK_TOP")?;
            }
        }
        else {
            writeln!(&mut f, "F_{}:",function_name)?;
        }
        
        for (i,(_location, instruction)) in function.body.iter().enumerate(){
            if program.in_mode == OptimizationMode::DEBUG {
                writeln!(&mut f,"   ; --- {} ",i)?
            }
            match instruction {
                Instruction::PUSH(Reg) => {
                    match Reg {
                        OfP::REGISTER(Reg) => {
                            if Reg.size() == 4 {
                                todo!("{:?}\n",Reg);
                            }
                            else{
                                writeln!(&mut f, "   push {} {}",size_to_nasm_type(Reg.size()),Reg.to_string())?;
                            }
                        }
                        OfP::STR(UUID,typ) => {
                            match typ {
                                ProgramStringType::STR => {
                                    writeln!(&mut f, "   push _STRING_{}_",UUID.to_string().replace("-", ""))?;
                                    writeln!(&mut f, "   push _LEN_STRING_{}_",UUID.to_string().replace("-", ""))?;
                                },
                                ProgramStringType::CSTR => {
                                    writeln!(&mut f, "   push _STRING_{}_",UUID.to_string().replace("-", ""))?;
                                },
                            }
                            
                        }
                        OfP::RAW(Data) => {
                            writeln!(&mut f, "   push {}",Data)?;
                        }
                        OfP::LOCALVAR(var) => {
                            let var = function.locals.get(var).expect(&format!("Error: could not find variable {}",var));
                            if callstack_size as usize-var.operand-var.typ.get_size() == 0 {
                                writeln!(&mut f, "   mov r10, [_CALLSTACK_BUF_PTR]")?;
                            }
                            else {
                                writeln!(&mut f, "   mov r10, [_CALLSTACK_BUF_PTR+{}]",callstack_size as usize-var.operand-var.typ.get_size())?;
                            }
                            writeln!(&mut f, "   push r10")?;
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
                            }
                        }
                        OfP::LOCALVAR(varOrg) => {
                            match Op2 {
                                OfP::REGISTER(_) => todo!(),
                                OfP::LOCALVAR(_) => todo!("vars"),
                                OfP::RAW(val) => {
                                    let var = com_expect!(_location,function.locals.get(varOrg),"Error: Unknown variable found during compilation {}",varOrg);
                                    
                                    if var.typ.get_size() <= 8  {
                                        println!("Current callstack_size: {} for {} with stack_size: {}",callstack_size,varOrg,var.operand);
                                        if callstack_size as usize-var.operand-var.typ.get_size() == 0 {
                                            writeln!(&mut f, "   mov {} [_CALLSTACK_BUF_PTR], {}",size_to_nasm_type(var.typ.get_size()), val)?;
                                        }
                                        else {
                                            writeln!(&mut f, "   mov {} [_CALLSTACK_BUF_PTR+{}], {}",size_to_nasm_type(var.typ.get_size()),callstack_size as usize-var.operand-var.typ.get_size(), val)?;
                                        }
                                    }
                                    else {
                                        todo!()
                                    }
                                },
                                OfP::STR(_,_) => todo!(),
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
                            if Reg.size() == 4 {
                                writeln!(&mut f, "   add rsp, 4")?;
                                writeln!(&mut f, "   mov dword [rsp], {}", Reg.to_string())?;
                            }
                            else{
                                writeln!(&mut f, "   pop {} {}",size_to_nasm_type(Reg.size()),Reg.to_string())?;
                            }
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
                                    writeln!(&mut f, "   add {} {}, {}", size_to_nasm_type(reg1.size()),reg1.to_string(), reg2.to_string())?;
                                }
                                OfP::LOCALVAR(_) => todo!(),
                                OfP::RAW(val) => {
                                    if val != &0 {
                                        writeln!(&mut f, "   add {}, {}",reg1.to_string(),val)?;
                                    }
                                }
                                OfP::STR(_,_) => todo!(),
                            }
                        }
                        OfP::LOCALVAR(var1) => {
                            match op2 {
                                OfP::REGISTER(_) => todo!(),
                                OfP::LOCALVAR(var2) => {
                                    com_assert!(_location, function.locals.contains_key(var1) && function.locals.contains_key(var2), "Unknown variable");
                                    let var1 = function.locals.get(var1).unwrap();
                                    let var2 = function.locals.get(var2).unwrap();
                                    com_assert!(_location, var1.typ.get_size() == var2.typ.get_size(), "Unknown variable size");
                                    if callstack_size as usize-var1.operand-var1.typ.get_size() == 0 {
                                        writeln!(&mut f, "   mov r10, [_CALLSTACK_BUF_PTR]")?;
                                    }
                                    else {
                                        writeln!(&mut f, "   mov r10, [_CALLSTACK_BUF_PTR+{}]",callstack_size as usize-var1.operand-var1.typ.get_size())?;
                                    }
                                    if callstack_size as usize-var2.operand-var2.typ.get_size() == 0{
                                        writeln!(&mut f, "   add r10, [_CALLSTACK_BUF_PTR]")?;  
                                    }
                                    else {
                                        writeln!(&mut f, "   add r10, [_CALLSTACK_BUF_PTR+{}]", callstack_size as usize-var2.operand-var2.typ.get_size())?;  
                                    }
                                    if callstack_size as usize-var1.operand-var1.typ.get_size() == 0 {
                                        writeln!(&mut f, "   mov [_CALLSTACK_BUF_PTR], r10")?;
                                    }
                                    else {
                                        writeln!(&mut f, "   mov [_CALLSTACK_BUF_PTR+{}], r10",callstack_size as usize-var1.operand-var1.typ.get_size())?;
                                    }
                                },
                                OfP::RAW(_) => todo!(),
                                OfP::STR(_,_) => todo!(),
                            }
                        },
                        OfP::RAW(_) => todo!(),
                        OfP::STR(_,_) => todo!(),
                        
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
                            }
                        }
                        OfP::LOCALVAR(_) => todo!(),
                        OfP::RAW(_) => todo!(),
                        OfP::STR(_,_) => todo!(),
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
                            }
                        }
                        OfP::LOCALVAR(_) => todo!(),
                        OfP::RAW(_) => todo!(),
                        OfP::STR(_,_) => todo!(),
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
                            }
                        }
                        OfP::LOCALVAR(_) => todo!(),
                        OfP::RAW(_) => todo!(),
                        OfP::STR(_,_) => todo!(),
                    }
                    
                }
                Instruction::CALL(Func) => {
                    writeln!(&mut f, "   mov r10, rsp")?;
                    writeln!(&mut f, "   mov rsp, [_CALLSTACK_BUF_PTR]")?;
                    writeln!(&mut f, "   call F_{}",Func)?;
                    writeln!(&mut f, "   mov [_CALLSTACK_BUF_PTR], rsp")?;
                    writeln!(&mut f, "   mov rsp, r10")?;
                    println!("Calling :D {} WHATDAFAK\n",Func);
                    callstack_size += 8;
                }
                Instruction::FNBEGIN() => {
                    writeln!(&mut f, "   mov [_CALLSTACK_BUF_PTR], rsp")?;
                    writeln!(&mut f, "   mov rsp, r10")?;
                }
                Instruction::RET() => {
                    let mut functionSize: usize = 0;
                    if !hasFoundRet {
                        for val in function.locals.values() {
                            functionSize += val.typ.get_size();
                        };
                        if functionSize > 0 {
                            writeln!(&mut f, "   mov r10, [_CALLSTACK_BUF_PTR]")?;
                            writeln!(&mut f, "   add r10, {}",functionSize)?;
                            writeln!(&mut f, "   mov [_CALLSTACK_BUF_PTR], r10")?;
                        }
                        writeln!(&mut f, "   mov r10, rsp")?;
                        writeln!(&mut f, "   mov rsp, [_CALLSTACK_BUF_PTR]")?;
                        writeln!(&mut f, "   ret")?;
                        println!("Call stack size {} in function {}, With hasFoundRet {}:\n{:#?}",callstack_size,function_name,hasFoundRet,function.body);
                        callstack_size -= 8;
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
                Instruction::CONDITIONAL_JUMP(ni) => {
                    let (_,prev) = function.body.get(i-1).unwrap();
                    match prev {
                        Instruction::EQUALS(Reg, _) => {
                            match Reg {
                                OfP::REGISTER(Reg) => {
                                    writeln!(&mut f, "   cmp {}, 1",Reg.to_byte_size(1).to_string())?;        
                                }
                                _ => todo!()
                            }
                            
                        }
                        _ => {
                            todo!("Implement parsing of if:\nif RBX {{}}\n")
                        }
                    }
                    writeln!(&mut f, "   jz .{}_S_{}",function_name,ni)?;
                }
                Instruction::JUMP(ni) => {
                    writeln!(&mut f, "   jmp .{}_S_{}",function_name,ni)?;
                }
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
                            }
                        }
                        OfP::LOCALVAR(_) => todo!(),
                        OfP::RAW(_) => todo!(),
                        OfP::STR(_,_) => todo!(),
                    }
                    
                },
                Instruction::DEFVAR(name) => {
                    let var = function.locals.get_mut(&name.clone()).expect("Error: unknown defvar definition in function! This is most likely due to a bug inside the compiler! Make sure to contact the developer if you encounter this!");
                    var.operand = callstack_size as usize;
                    callstack_size += var.typ.get_size() as i64;

                    writeln!(&mut f, "   mov r10, [_CALLSTACK_BUF_PTR]")?;
                    writeln!(&mut f, "   sub r10, {}",var.typ.get_size())?;
                    writeln!(&mut f, "   mov [_CALLSTACK_BUF_PTR], r10")?;
                },
                Instruction::INTERRUPT(val) => {
                    writeln!(&mut f, "   int 0x{:x}",val)?;
                },
            }
        }
        if function_name == "main" {
            writeln!(&mut f, "   xor rax,rax")?;
            writeln!(&mut f, "   pop rbp")?;
            writeln!(&mut f, "   ret")?;
        }
    }
    Ok(())

}




fn type_check_build(build: &mut BuildProgram) {
    // TODO: implement macros for assert, expect etc. for type checking that also print the type stack trace.
    let mut typeStack: Vec<VarType> = Vec::new();
    for (name, func) in build.functions.iter() {
        typeStack.extend(func.contract.Inputs.clone());
        for (loc, instruction) in func.body.iter() {
            match instruction {
                Instruction::PUSH(ofp) => {
                    match ofp {
                        OfP::REGISTER(reg) => {
                            match reg.size() {
                                8 => {
                                    // TODO: Implement checking for if register is rsp or rbp, (push a pointer onto the typeStack)
                                    typeStack.push(VarType::LONG)
                                }
                                4 => {
                                    typeStack.push(VarType::INT)
                                }
                                2 => {
                                    typeStack.push(VarType::SHORT)
                                }
                                1 => {
                                    typeStack.push(VarType::CHAR)
                                }
                                _ => {
                                    todo!("Unreachable");
                                }
                            }
                        }
                        OfP::LOCALVAR(_) => todo!("Type checking for local variables on the stack"),
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
                    }
                },
                Instruction::DEFVAR(_)           => {},
                Instruction::MOV(_, _)           => {},
                Instruction::POP(ofp)              => {
                    match ofp {
                        OfP::REGISTER(reg) => {
                            match reg.size() {
                                8 => {
                                    // TODO: Implement checking for if register is rsp or rbp, (push a pointer onto the typeStack)
                                    let v = com_expect!(loc, typeStack.pop(), "Error: Stack underflow occured for types! Make sure everything is ok and you aren't manipulating the stack with anything");
                                    com_assert!(loc, v.get_size() == 8, "Error: Can not pop type into register as its a different size!");
                                }
                                4 => {
                                    let v = com_expect!(loc, typeStack.pop(), "Error: Stack underflow occured for types! Make sure everything is ok and you aren't manipulating the stack with anything");
                                    com_assert!(loc, v.get_size() == 4, "Error: Can not pop type into register as its a different size!");
                                }
                                2 => {
                                    let v = com_expect!(loc, typeStack.pop(), "Error: Stack underflow occured for types! Make sure everything is ok and you aren't manipulating the stack with anything");
                                    com_assert!(loc, v.get_size() == 2, "Error: Can not pop type into register as its a different size!");
                                }
                                1 => {
                                    let v = com_expect!(loc, typeStack.pop(), "Error: Stack underflow occured for types! Make sure everything is ok and you aren't manipulating the stack with anything");
                                    com_assert!(loc, v.get_size() == 1, "Error: Can not pop type into register as its a different size!");
                                }
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
                    com_assert!(loc, typeStack.ends_with(&function.contract.Inputs), "Error: Arguments for function don't match, function expected:\n {}, But found\n: {}",
                    {
                        
                        for typ in function.contract.Inputs.iter() {
                            eprintln!("   {}",typ.to_string(false).to_uppercase())
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
                    func main(int, ptr : int) {
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
                Instruction::CONDITIONAL_JUMP(_) => todo!("Branching"),
                Instruction::JUMP(_)             => todo!("JUMP to index"),
                Instruction::INTERRUPT(_)        => {},
            }
        }
        if typeStack != func.contract.Outputs {
            eprintln!("Error: types left on the stack in function body: {}\n   Type stack trace: ",name);
            for typ in typeStack.iter() {
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
    println!("         -o (output path) -> outputs to that file (example: hello.asm in nasm_x86_64 mode). If the output path is not specified it defaults to the modes default (for nasm_x86_64 thats a.asm)");
    println!("         -r               -> builds the program for you if the option is available for that language mode (for example in nasm_x86_64 it calls nasm with gcc to link it to an executeable)");
    println!("         -noRaxWarn       -> removes the RAX usage warning for nasm");
    println!("         -release         -> builds the program in release mode");
    println!("         -ntc             -> (NoTypeChecking) Disable type checking");
    println!("         -nuw             -> No unused warn");
    println!("--------------------------------------------");
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
                // TODO: introduce a flag for call stack size
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
                "-nuw" => {
                    program.print_unused_warns = false
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

    let mut lexer = Lexer::new(info, & Intrinsics, &Definitions);
    lexer.currentLocation.file = program.path.clone();
    let mut build = parse_tokens_to_build(&mut lexer, &mut program);
    if program.use_type_checking {
        type_check_build(&mut build);
    }
    match program.typ.as_str() {
        "nasm_x86_64" => {
            to_nasm_x86_64(&mut build, &program).expect("Could not build to nasm_x86_64");
            if program.should_build {
        
                let nasm = Command::new("nasm").args(["-f","elf",program.opath.as_str()]).output().expect("Could not build nasm!");
                let gcc  = Command::new("gcc").args([Path::new(program.opath.as_str()).with_extension("o").to_str().unwrap(),"-o",Path::new(program.opath.as_str()).with_extension("").to_str().unwrap()]).output().expect("Could not build gcc!");
                let _ld     = Command::new("ld".to_string()).arg(Path::new(program.opath.as_str()).with_extension("").to_str().unwrap()).output().expect("Could not build your program!");
                if !nasm.status.success() {
                    println!("Nasm: \n{:?}\n-----------",nasm);
                }
                else if !gcc.status.success() {
                    println!("Gcc:  \n{:?}\n-----------",gcc);
                }
                else {
                    println!("Finished build successfully")
                }        
            }
        }
        _ => {
            todo!("Unimplemented type {}",program.typ);
        }
    }
}
