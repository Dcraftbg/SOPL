#![allow(non_snake_case)] 
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(unreachable_patterns)]

use std::{env, process::{exit, Command}, path::{Path, PathBuf}, ffi::OsStr, str::FromStr, collections::HashMap, hash::Hash, fs::{File, self}, io::{Read, Write, self}, fmt::format, os::windows::{process::CommandExt, self}, arch::x86_64::_mm_testz_pd};
use uuid::Uuid;

macro_rules! par_error {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(P) [ERROR] {}:{}:{}: {}", $token.location.clone().file, $token.location.clone().linenumber, $token.location.clone().character, message);
        exit(1);
    });
}
macro_rules! lpar_error {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(P) [ERROR] {}:{}:{}: {}", $location.clone().file, $location.clone().linenumber, $location.clone().character, message);
        exit(1);
    });
}

macro_rules! par_assert {
    ($token:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(P) [ERROR] {}:{}:{}: {}", $token.location.clone().file, $token.location.clone().linenumber, $token.location.clone().character, message);
            exit(1);
        }
    });
}
macro_rules! par_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(P) [ERROR] {}:{}:{}: {}", $location.file, $location.linenumber, $location.character, message);
        $expector.expect(&message)
        //eprintln!("(P) [ERROR] {}:{}:{}: {}", $token.location.file, $token.location.linenumber, $token.location.character, message);
        //exit(1);
    });
}
macro_rules! par_info {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(P) [INFO] {}:{}:{}: {}", $token.location.file, $token.location.linenumber, $token.location.character, message);
    });
}
macro_rules! par_warn {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(P) [WARN] {}:{}:{}: {}", $token.location.file, $token.location.linenumber, $token.location.character, message);
    });
}

macro_rules! com_error {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(C) [ERROR] {}:{}:{}: {}", $token.location.file, $token.location.linenumber, $token.location.character, message);
        exit(1);
    });
}
macro_rules! com_info {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(C) [INFO] {}:{}:{}: {}", $token.location.file, $token.location.linenumber, $token.location.character, message);
    });
}
macro_rules! com_assert {
    ($token:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(C) [ERROR] {}:{}:{}: {}", $token.location.file, $token.location.linenumber, $token.location.character, message);
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

struct CmdProgram {
    path: String,
    opath: String,
    typ: String,
    should_build: bool,
    warn_rax_usage: bool,
    call_stack_size: usize
}
impl CmdProgram {
    fn new() -> Self {
        Self { path: "".to_string(), opath: "".to_string(), should_build: false, typ: "".to_string(), warn_rax_usage: true, call_stack_size: 64000 }
    }
}
fn usage() {
    todo!("Unimplemented usage function!");
}
#[repr(u32)]
#[derive(Clone, Copy,Debug)]

enum IntrinsicType {
    Extern = 0,
    Func,
    OPENPAREN,
    CLOSEPAREN,
    DOUBLE_COLIN,
    COMA,
    OPENCURLY,
    CLOSECURLY,
    // REGISTER OPERATIONS
    POP,
    PUSH,
    MOV_REG,

    // REGISTER MATH
    ADD,
    SUB,
    MUL,
    RET,
    INCLUDE
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
            IntrinsicType::MOV_REG => {
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
        }
    }
}
// static mut Intrinsic: HashMap<&str,IntrinsicType> = {
//     let mut map:HashMap<&str,IntrinsicType> = HashMap::new();

//     map.insert("extern", IntrinsicType::Extern);
//     map
// };
//mut Intrinsics: HashMap<&str,IntrinsicType> = HashMap::new();
#[derive(Debug)]
struct Intrinsic {
    operand: i64,
}
#[derive(Debug)]
enum TokenType {
    WordType      (String),
    IntrinsicType (IntrinsicType, Intrinsic),
    StringType    (String),
    CharType      (String),
    Number32      (i32),
    Number64      (i64)
}
impl TokenType {
    fn to_string(&self,isplural:bool) -> String{
        match self {
            TokenType::WordType(_) => {
                if isplural {"Words".to_string()} else {"Word".to_string()}
            },
            TokenType::IntrinsicType(_, _) => {
                if isplural {"Intrinsics".to_string()} else {"Intrinsic".to_string()}
            }
            TokenType::StringType(_) => {
                if isplural {"Strings".to_string()} else {"String".to_string()}
            }
            TokenType::CharType(_) => {
                if isplural {"Chars".to_string()} else {"Char".to_string()}
            }
            TokenType::Number32(_) => {
                if isplural {"Numbers(32)".to_string()} else {"Number(32)".to_string()}
            }
            TokenType::Number64(_) => {
                if isplural {"Numbers(64)".to_string()} else {"Number(64)".to_string()}
            }
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
    fn com_error(&self, msg: &str) {
        eprintln!("(C) [ERROR] {}:{}:{}: {}",self.file,self.linenumber,self.character,msg);
        exit(1);
    }
    fn par_error(&self, msg: &str) {
        eprintln!("(P) [ERROR] {}:{}:{}: {}",self.file,self.linenumber,self.character,msg);
        exit(1);
    }
    fn par_info(&self, msg: &str) {
        println!("(P) [INFO] {}:{}:{}: {}",self.file,self.linenumber,self.character,msg);
    }
    fn com_info(&self, msg: &str) {
        println!("(C) [INFO] {}:{}:{}: {}",self.file,self.linenumber,self.character,msg);
    }
    fn assert_com(&self, condition:bool, msg: &str) {
        if !condition {
            eprintln!("(C) [ERROR] {}:{}:{}: {}",self.file,self.linenumber,self.character,msg);
            exit(1);
        }
    }
    fn assert_par(&self, condition:bool, msg: &str) {
        if !condition {
            eprintln!("(P) [ERROR] {}:{}:{}: {}",self.file,self.linenumber,self.character,msg);
            exit(1);
        }
    }
}
#[derive(Debug)]
struct Token {
    typ: TokenType,
    location: ProgramLocation
}
struct Lexer<'a> {
    source: String,
    cursor: usize,
    currentLocation: ProgramLocation,
    Intrinsics: &'a HashMap<String,IntrinsicType>
}

impl<'a> Lexer<'a> {
    fn trim_left(&mut self) -> bool {
        if self.cursor >= self.source.len(){
            return false;
        }
        while self.is_not_empty() && self.source.chars().nth(self.cursor).unwrap().is_whitespace() {
            //println!("Skipping '{}'",self.source.chars().nth(self.cursor).unwrap());
            self.cursor += 1;
            self.currentLocation.character+=1;
            if self.source.chars().nth(self.cursor).unwrap() == '\n' {
                self.currentLocation.linenumber += 1;
                self.currentLocation.character = 0;
            }
            if self.cursor >= self.source.len(){
                break;
            }
        }
        true
        
    }
    fn is_not_empty(&self) -> bool {
        self.cursor < self.source.len()
    }
    fn new(source: String, Intrinsics: &'a HashMap<String, IntrinsicType>) -> Self {
        Self { 
            source: source.clone(), 
            cursor: 0, 
            currentLocation: ProgramLocation { file: String::from(""), linenumber: 1, character: 0 },
            Intrinsics 
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
                    let u_outstr = unescape(&outstr);
                    self.cursor += 1;
                    return Some(Token { typ: TokenType::StringType(u_outstr.clone()), location: self.currentLocation.clone() });
                }
                '\'' => {
                    let mut shouldIgnoreNext: bool = true;
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
                    if let Some(nc) = self.source.chars().nth(self.cursor+1) {
                        if nc == '/' {
                            self.cursor += 1;
                            while self.is_not_empty() && self.source.chars().nth(self.cursor).unwrap() != '\n' {
                                //println!("Char: '{}'",self.source.chars().nth(self.cursor).unwrap());
                                self.cursor += 1;
                            }
                            self.currentLocation.character = 0;
                            return self.next();
                        }
                        else {
                            todo!("Check for any intrinsics thatm might include this")
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
                        //println!("-----\nCurrent char: {} and C is {}",self.source.chars().nth(self.cursor).unwrap(), c);
                        while self.is_not_empty() && (self.source.chars().nth(self.cursor).unwrap() != '(' && self.source.chars().nth(self.cursor).unwrap() != ')' && self.source.chars().nth(self.cursor).unwrap() != '[' && self.source.chars().nth(self.cursor).unwrap() != ']') && !self.source.chars().nth(self.cursor).unwrap().is_alphanumeric() && !self.source.chars().nth(self.cursor).unwrap().is_whitespace() {
                            outstr.push(self.source.chars().nth(self.cursor).unwrap());
                            self.cursor+=1;
                            self.currentLocation.character += 1;
                        }
                        
                        self.currentLocation.character += 1;
                        //println!("------\nStr: '{}'",outstr);
                        
                        if let Some(o) = self.Intrinsics.get(&outstr) {
                            //println!("Got intrinsic: {}",o.to_string(false));
                            return Some(Token { typ: TokenType::IntrinsicType(o.clone(), Intrinsic { operand: 0 }), location: self.currentLocation.clone() });
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
                        let o = self.Intrinsics.get(&outstr);
                        if o.is_none() {
                            return Some(Token { typ: TokenType::WordType(outstr), location: self.currentLocation.clone() });
                        }
                        else {
                            //TODO: implement operand
                            return Some(Token { typ: TokenType::IntrinsicType((*o.unwrap()).clone(),Intrinsic { operand: 0 }), location: self.currentLocation.clone() });
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
enum External {
    RawExternal(String),
    CExternal(String)
}
impl External {
    fn prefix(&self) -> String {
        match self {
            External::RawExternal(_) => "".to_string(),
            External::CExternal(_) => "_".to_string(),
            _ => "".to_string()
        }
    }
    fn suffix(&self) -> String {
        match self {
            External::RawExternal(_) => "".to_string(),
            External::CExternal(_) => "".to_string(),
            _ => "".to_string()
        }
    }
    fn to_string(&self) -> String{
        match self {
            External::RawExternal(_) => "RawExternal".to_string(),
            External::CExternal(_) => "CExternal".to_string()
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
            panic!("Invalid size for nasm type!");
            
        }
    }
}
#[derive(Debug)]
enum Instruction {
    PUSH    (Register),
    PUSHSTR (Uuid),
    PUSHRAW (i64),
    MOV     (Register, i64),
    MOV_REG (Register, Register),
    POP     (Register),
    CALLRAW (String),
    ADD     (Register, Register),
    SUB     (Register, Register),
    MUL     (Register, Register),
    DIV     (Register, Register),
    CALL    (String),
    FNBEGIN (),
    RET     ()
}

#[derive(Debug,Clone)]
enum ProgramStringType {
    STR ,
    CSTR
}
#[derive(Debug,Clone)]
struct ProgramString {
    Typ:  ProgramStringType,
    Data: String
}
#[derive(Debug)]
struct Function {
    contract: FunctionContract,
    location: ProgramLocation,
    body: Vec<(ProgramLocation,Instruction)>,
}
#[derive(Debug)]
struct BuildProgram {
    externals: Vec<External>,
    functions: HashMap<String, Function>,
    stringdefs:   HashMap<Uuid,ProgramString>
}
// Functions: Vec<Function>

#[derive(Clone, Copy, Debug)]
enum VarType {
    CHAR,
    SHORT,
    BOOLEAN,
    INT,
    LONG,
    STR,
    PTR,
    CUSTOM(u64)   
}
#[derive(Debug)]
struct FunctionContract {
    Inputs: Vec<VarType>,
    Outputs: Vec<VarType>
}
fn parse_function_contract(lexer: &mut Lexer, Definitions: &HashMap<String,VarType>) -> FunctionContract {
    let mut out = FunctionContract {Inputs: vec![], Outputs: vec![]};
    let mut is_input = true;
    let first = lexer.next();
    let first = par_expect!(lexer.currentLocation,first,"abruptly ran out of tokens in function contract");//first.expect("Error: abruptly ran out of tokens in function contract");
    let mut expectNextSY = false;
    //println!("Parsing function contract....");
    match first.typ {
        TokenType::IntrinsicType(typ, _) => {
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
            TokenType::IntrinsicType(Typ,_ ) => {
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
                par_assert!(token,!expectNextSY, "Definitions inside function contracts must be seperated by either , or :");
                expectNextSY = true;
                match Word {
                    Word => {
                        if let Some(var) = Definitions.get(&Word) {
                            if is_input {out.Inputs.push(var.clone())} else {out.Outputs.push(var.clone())};
                        }
                        else {
                            par_error!(token, "Unexpected Word: {} found in function contract",Word)
                        }
                    }
                }
            }
            Other => {
                par_error!(token, "Unexpected Token Type in Function Contract. Expected Word but found: {}",Other.to_string(false))
            }
        }
    }
    out
}
fn parse_tokens_to_build(lexer: &mut Lexer, _Intrinsics: &HashMap<String,IntrinsicType>, Definitions: &HashMap<String,VarType>, program: &mut CmdProgram) -> BuildProgram {
    let mut build: BuildProgram = BuildProgram { externals: vec![], functions: HashMap::new(),stringdefs: HashMap::new()};
    let mut currentFunction: Option<String> = None;
    while let Some(token) = lexer.next(){
        
        match token.typ {
            TokenType::WordType(word) => {
                //HashMap<String, ExternalType>
                // TODO: Introduce some sort of hashmap for this
                par_assert!(token,currentFunction.is_some(), "Undefined Word Call outside of entry point! '{}'",word);
                let mut isvalid = false;
                for external in build.externals.iter() {
                    match external {
                        External::RawExternal(ext) => {
                            if &word == ext {
                                isvalid = true;
                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::CALLRAW(ext.clone())));
                                break;
                            }
                        }
                        External::CExternal(ext) => {
                            if &word == ext {
                                isvalid = true;
                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::CALLRAW(external.prefix().clone()+&ext.clone()+&external.suffix())));
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
                        TokenType::IntrinsicType(typ,_) => {
                            match typ {
                                
                                IntrinsicType::POP => {
                                    build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(reg)));
                                }
                                IntrinsicType::PUSH => {
                                    build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::PUSH(reg)));
                                }
                                IntrinsicType::MOV_REG => {
                                    let token = par_expect!(lexer.currentLocation,lexer.next(),"abruptly ran out of tokens");//.expect(&format!("abruptly ran out of tokens"));
                                    match token.typ {
                                        TokenType::Number32(data) => {
                                            if reg.size() >= 4 {
                                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location,Instruction::MOV(reg, data as i64)))
                                            }
                                        }
                                        TokenType::Number64(data) => {
                                            if reg.size() >= 8 {
                                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location,Instruction::MOV(reg, data)))
                                            }
                                        }
                                        other => par_error!(token,"Unexpected Type for Mov Intrinsic. Expected Number32/Number64 but found {}",other.to_string(false))//token.location.par_error(&format!("Unexpected Type for Mov Intrinsic. Expected Number32/Number64 but found {}",other.to_string(false)))
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
                                    TokenType::IntrinsicType(typ, _) => {
                                        
                                        match typ {
                                            IntrinsicType::ADD => {
                                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::ADD(reg, reg2)))
                                            }
                                            IntrinsicType::SUB => {
                                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::SUB(reg, reg2)))
                                            }
                                            IntrinsicType::MUL => {
                                                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::MUL(reg, reg2)))
                                            }
                                            other => token.location.par_error(&format!("Unexpected Intrinsic! Expected Register Intrinsic but found {}",other.to_string(false)))//panic!("Unexpected Intrinsic! Expected Register Intrinsic but found {}",other.to_string(false))
                                        }
                                    }
                                    other => {
                                        par_error!(token, "Unexpected token type: Expected Intrinsic but found {}",other.to_string(false));
                                    }
                                }
                            }
                            else{
                                par_error!(token, "unknown word {} found after Register",Word);
                            }
                        }
                        typ => {
                            par_error!(token,"Unexpected register operation! Expected Intrinsic or another Register but found {}",typ.to_string(false));
                        }
                    }
                }
                if !isvalid {
                    isvalid = build.functions.contains_key(&word);
                    if isvalid {
                        let func = build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap();
                        func.body.push((token.location.clone(),Instruction::CALL(word.clone())));
                        //func.body.push((token.location,Instruction::FNBEGIN());
                    }
                }
                par_assert!(token,isvalid,"Unknown word type: {}",word);
            }
            TokenType::IntrinsicType(Type, _Data) => {
                match Type {
                    IntrinsicType::Extern => {
                        let externType = lexer.next();
                        let externType = externType.expect("Error: Unexpected abtrupt end of tokens in extern");
                        match externType.typ {
                            TokenType::WordType(Word) => {
                                build.externals.push(External::RawExternal(Word));
                            }
                            TokenType::IntrinsicType(_, _) => assert!(false,"Unexpected behaviour! expected type Word or String but found Intrinsic"),
                            TokenType::StringType(Type) => {
                                match Type.as_str() {
                                    "C" => {
                                        let externWord = lexer.next();
                                        let externWord = externWord.expect("Error: C type extern defined but stream of tokens abruptly ended!");
                                        match externWord.typ {
                                            TokenType::WordType(Word) => {
                                                build.externals.push(External::CExternal(Word))
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
                            TokenType::CharType(_) => par_error!(token,"Unexpected behaviour! expected type Word or String but found Char"),
                            TokenType::Number32(_) => par_error!(token,"Unexpected behaviour! expected type Word or String but found Number32"),
                            TokenType::Number64(_) => par_error!(token,"Unexpected behaviour! expected type Word or String but found Number64"),

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
                                let _contract = parse_function_contract(lexer, &Definitions);
                                currentFunction = Some(Word.clone());
                                if Word != "main" {
                                    build.functions.insert(Word.clone(), Function { contract: _contract, body: vec![(token.location.clone(),Instruction::FNBEGIN())], location: token.location.clone()});
                                }
                                else {
                                    build.functions.insert(Word.clone(), Function { contract: _contract, body: vec![], location: token.location.clone() });
                                }

                            }
                            Other => par_error!(token,"Unexpected behaviour! Expected type Word but found {}",Other.to_string(false))
                        }
                    }
                    IntrinsicType::OPENPAREN => todo!(),
                    IntrinsicType::CLOSEPAREN => todo!(),
                    IntrinsicType::DOUBLE_COLIN => todo!(),
                    IntrinsicType::COMA => todo!(),
                    IntrinsicType::OPENCURLY => {
                        // TODO: implement stack system!
                    }
                    IntrinsicType::CLOSECURLY => {
                        // TODO: implement stack system!
                    }
                    IntrinsicType::POP => {
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(Register::RAX)));
                    }
                    IntrinsicType::PUSH => todo!(),
                    IntrinsicType::MOV_REG => todo!(),
                    // TODO: implement math ops
                    IntrinsicType::ADD => {
                        par_assert!(token,currentFunction.is_some(), "Unexpected ADD operation of entry point!");
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(Register::EAX)));
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(Register::R8D)));
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::ADD(Register::EAX, Register::R8D)))
                    }
                    IntrinsicType::SUB => {
                        par_assert!(token,currentFunction.is_some(), "Unexpected SUB operation of entry point!");
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(Register::EAX)));
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(Register::R8D)));
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::SUB(Register::EAX, Register::R8D)))
                    }
                    IntrinsicType::MUL => {
                        par_assert!(token,currentFunction.is_some(), "Unexpected MUL operation of entry point!");
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(Register::EAX)));
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::POP(Register::R8D)));
                        build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::MUL(Register::EAX, Register::R8D)))
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
                                let mut lf = Lexer::new(fs::read_to_string(String::from(p.join(&include_p).to_str().unwrap().replace("\\", "/"))).expect(&format!("Error: could not open file: {}",String::from(p.join(&include_p).to_str().unwrap()).replace("\\", "/"))),_Intrinsics);
                                lf.currentLocation.file = String::from(p.join(&include_p).to_str().unwrap().replace("\\", "/"));
                                let mut nprogram = CmdProgram::new();
                                nprogram.path = String::from(p.join(&include_p).to_str().unwrap().replace("\\", "/"));
                                let mut build2 = parse_tokens_to_build(&mut lf, _Intrinsics, Definitions, &mut nprogram);
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
                                        for (_floc, funcDef) in build2.functions.iter_mut() {
                                            for (_iloc,Inst) in funcDef.body.iter_mut() {
                                                match Inst {
                                                    Instruction::PUSHSTR(strid) => {
                                                        if strid.clone() == orgstrdefId {
                                                            *strid = strdefId.clone();
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
                                //build.functions.extend(build2.functions)
                                //build.stringdefs.extend(build2.stringdefs);
                                
                            }
                            Other => {
                                par_error!(includeName, "Expected token type String but found {}",Other.to_string(false));
                            }
                        }
                    },
                }
            }
            TokenType::StringType(Word) => {
                par_assert!(token,currentFunction.is_some(), "Unexpected string definition outside of entry point!");

                //build.stringdefs.push(ProgramString {Word.clone());
                let UUID = Uuid::new_v4();
                build.stringdefs.insert(UUID,ProgramString {Data: Word.clone(), Typ: ProgramStringType::STR});
                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location,Instruction::PUSHSTR(UUID)));
            }
            TokenType::CharType(_) => {
                par_assert!(token,currentFunction.is_some(), "Unexpected char definition outside of entry point!");
            }
            TokenType::Number32(val) => {
                par_assert!(token,currentFunction.is_some(), "Unexpected number (64) definition outside of entry point!");
                // TODO: implement this
                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::MOV(Register::EAX, val as i64)));
                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::PUSH(Register::EAX)));
            }
            TokenType::Number64(val) => {
                par_assert!(token,currentFunction.is_some(), "Unexpected number (64) definition outside of entry point!");
                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::MOV(Register::RAX, val)));
                build.functions.get_mut(currentFunction.as_mut().unwrap()).unwrap().body.push((token.location.clone(),Instruction::PUSH(Register::RAX)));
            }
        }
    }
    for (fn_name, fn_fn)in build.functions.iter_mut() {
        if fn_name != "main" {
            fn_fn.body.push((fn_fn.location.clone(),Instruction::RET()))
        }
    }
    build
}
fn to_nasm_x86_64(build: &BuildProgram, program: &CmdProgram) -> io::Result<()>{
    let mut f = File::create(&program.opath).expect(&format!("Error: could not open output file {}",program.opath.as_str()));
    writeln!(&mut f,"BITS 64")?;
    for function_name in build.functions.keys() {
        writeln!(&mut f,"global _{}",function_name)?;
    }
    //writeln!(&mut f,"global _main")?;
    for exter in build.externals.iter() {
        match exter {
            External::RawExternal(Word) => {
                writeln!(&mut f,"  extern {}{}{}",exter.prefix(),Word,exter.suffix())?;
            }
            External::CExternal(Word) => {
                writeln!(&mut f,"  extern {}{}{}",exter.prefix(),Word,exter.suffix())?;
            },
        }
    }
    writeln!(&mut f, "section .text")?;
    //writeln!(&mut f, "_main:")?;
    //_STRING_(INDEX)
    for (function_name,function) in build.functions.iter() {
        //writeln!(&mut f,"global _{}",function_name)?;
        if function_name == "main" {
            writeln!(&mut f, "_{}:",function_name)?;
        }
        else {
            writeln!(&mut f, "F_{}:",function_name)?;
        }
        for (_location, instruction) in function.body.iter(){
            match instruction {
                Instruction::PUSH(Reg) => {
                    if Reg.size() == 4 {
                        //writeln!(&mut f, "   sub rsp, 4")?;
                        //writeln!(&mut f, "   mov dword [rsp], {}", Reg.to_string())?;
                        todo!("{:?}\n{:#?}",Reg,build);
                    }
                    else{
                        writeln!(&mut f, "   push {} {}",size_to_nasm_type(Reg.size()),Reg.to_string())?;
                    }
                    //writeln!(&mut f, "   push {} {}",size_to_nasm_type(Reg.size()),Reg.to_string())?;
                }
                Instruction::PUSHSTR(Index) => {
                    writeln!(&mut f, "   push _STRING_{}",Index.to_string().replace("-", ""))?;
                    writeln!(&mut f, "   push _LEN_STRING_{}",Index.to_string().replace("-", ""))?;
                }
                Instruction::PUSHRAW(Data) => {
                    writeln!(&mut f, "   push {}",Data)?;
                }
                Instruction::MOV(Reg, Data) => {
                    writeln!(&mut f, "   mov {}, {}",Reg.to_string(), Data)?;
                }
                Instruction::POP(Reg) => {
                    if Reg.size() == 4 {
                        writeln!(&mut f, "   add rsp, 4")?;
                        writeln!(&mut f, "   mov {}, dword [rsp]", Reg.to_string())?;
                    }
                    else{
                        writeln!(&mut f, "   pop {} {}",size_to_nasm_type(Reg.size()),Reg.to_string())?;
                    }
                }
                Instruction::CALLRAW(Word) => {
                    writeln!(&mut f, "   call {}",Word)?;
                }
                Instruction::MOV_REG(reg1, reg2) => {
                    writeln!(&mut f, "   mov {}, {}",reg1.to_string(), reg2.to_string())?;
                }
                Instruction::ADD(reg1, reg2) => {
                    assert!(reg1.size() == reg2.size(), "Two different sized registers passed to add Instruction");
                    writeln!(&mut f, "   add {} {}, {}", size_to_nasm_type(reg1.size()),reg1.to_string(), reg2.to_string())?;
                }
                Instruction::SUB(reg1, reg2) => {
                    assert!(reg1.size() == reg2.size(), "Two different sized registers passed to sub Instruction");
                    writeln!(&mut f, "   sub {} {}, {}",size_to_nasm_type(reg1.size()),reg1.to_string(), reg2.to_string())?;
                }
                Instruction::MUL(reg1, reg2) => {
                    assert!(reg1.size() == reg2.size(), "Two different sized registers passed to mul Instruction");
                    writeln!(&mut f, "   mul {} {}, {}",size_to_nasm_type(reg1.size()),reg1.to_string(), reg2.to_string())?;
                }
                Instruction::DIV(reg1, reg2) => {
                    assert!(reg1.size() == reg2.size(), "Two different sized registers passed to div Instruction");
                    writeln!(&mut f, "   div {}, {}",reg1.to_string(), reg2.to_string())?;
                }
                Instruction::CALL(Func) => {
                    writeln!(&mut f, "   mov rbp, rsp")?;
                    writeln!(&mut f, "   mov rsp, [_CALLSTACK_BUF_PTR]")?;
                    writeln!(&mut f, "   call F_{}",Func)?;
                    writeln!(&mut f, "   mov [_CALLSTACK_BUF_PTR], rsp")?;
                    writeln!(&mut f, "   mov rsp, rbp")?;
                }
                Instruction::FNBEGIN() => {
                    writeln!(&mut f, "   mov [_CALLSTACK_BUF_PTR], rsp")?;
                    writeln!(&mut f, "   mov rsp, rbp")?;
                }
                Instruction::RET() => {
                    writeln!(&mut f, "   mov rbp, rsp")?;
                    writeln!(&mut f, "   mov rsp, [_CALLSTACK_BUF_PTR]")?;
                    writeln!(&mut f, "   ret")?;
                }
            }
        }
        if function_name == "main" {
            writeln!(&mut f, "   xor rax,rax")?;
            writeln!(&mut f, "   jmp ENDOFCODE")?;
        }
    }

    // for instruction in build.instructions.iter(){
    //     match instruction {
    //         Instruction::PUSH(Reg) => {
    //             if Reg.size() == 4 {
    //                 //writeln!(&mut f, "   sub rsp, 4")?;
    //                 //writeln!(&mut f, "   mov dword [rsp], {}", Reg.to_string())?;
    //                 todo!()
    //             }
    //             else{
    //                 writeln!(&mut f, "   push {} {}",size_to_nasm_type(Reg.size()),Reg.to_string())?;
    //             }
    //             //writeln!(&mut f, "   push {} {}",size_to_nasm_type(Reg.size()),Reg.to_string())?;
    //         }
    //         Instruction::PUSHSTR(Index) => {
    //             writeln!(&mut f, "   push _STRING_{}",Index)?;
    //             writeln!(&mut f, "   push _LEN_STRING_{}",Index)?;
    //         }
    //         Instruction::PUSHRAW(Data) => {
    //             writeln!(&mut f, "   push {}",Data)?;
    //         }
    //         Instruction::MOV(Reg, Data) => {
    //             writeln!(&mut f, "   mov {}, {}",Reg.to_string(), Data)?;
    //         }
    //         Instruction::POP(Reg) => {
    //             if Reg.size() == 4 {
    //                 writeln!(&mut f, "   add rsp, 4")?;
    //                 writeln!(&mut f, "   mov {}, dword [rsp]", Reg.to_string())?;
    //             }
    //             else{
    //                 writeln!(&mut f, "   pop {} {}",size_to_nasm_type(Reg.size()),Reg.to_string())?;
    //             }
    //         }
    //         Instruction::CALLRAW(Word) => {
    //             writeln!(&mut f, "   call {}",Word)?;
    //         }
    //         Instruction::MOV_REG(reg1, reg2) => {
    //             writeln!(&mut f, "   mov {}, {}",reg1.to_string(), reg2.to_string())?;
    //         }
    //         Instruction::ADD(reg1, reg2) => {
    //             assert!(reg1.size() == reg2.size(), "Two different sized registers passed to add Instruction");
    //             writeln!(&mut f, "   add {} {}, {}", size_to_nasm_type(reg1.size()),reg1.to_string(), reg2.to_string())?;
    //         }
    //         Instruction::SUB(reg1, reg2) => {
    //             assert!(reg1.size() == reg2.size(), "Two different sized registers passed to sub Instruction");
    //             writeln!(&mut f, "   sub {} {}, {}",size_to_nasm_type(reg1.size()),reg1.to_string(), reg2.to_string())?;
    //         }
    //         Instruction::MUL(reg1, reg2) => {
    //             assert!(reg1.size() == reg2.size(), "Two different sized registers passed to mul Instruction");
    //             writeln!(&mut f, "   mul {} {}, {}",size_to_nasm_type(reg1.size()),reg1.to_string(), reg2.to_string())?;
    //         }
    //         Instruction::DIV(reg1, reg2) => {
    //             assert!(reg1.size() == reg2.size(), "Two different sized registers passed to div Instruction");
    //             writeln!(&mut f, "   div {}, {}",reg1.to_string(), reg2.to_string())?;
    //         }
    //         Instruction::CALL(Func) => {
    //             writeln!(&mut f, "   mov rbp, rsp")?;
    //             writeln!(&mut f, "   mov rsp, [_CALLSTACK_BUF_PTR]")?;
    //             writeln!(&mut f, "   call F_{}",Func)?;
    //         }
    //         Instruction::FNBEGIN() => {
    //             writeln!(&mut f, "   mov [_CALLSTACK_BUF_PTR], rsp")?;
    //             writeln!(&mut f, "   mov rsp, rbp")?;
    //         }
    //         Instruction::RET() => {
    //             writeln!(&mut f, "   mov rbp, rsp")?;
    //             writeln!(&mut f, "   mov rsp, _CALLSTACK")?;
    //             writeln!(&mut f, "   ret")?;
    //         }
    //     }
    // }
    writeln!(&mut f, "ENDOFCODE:")?;
    writeln!(&mut f, "section .data")?;
    writeln!(&mut f, "   _CALLSTACK_BUF_PTR: dq _CALLSTACK_TOP")?;
    for (UUID,stridef) in build.stringdefs.iter(){        
        //writeln!(&mut f, "   _STRING_{}: db \"{}\", 0",i,stridef.Data)?;
        write!(&mut f, "   _STRING_{}: db ",UUID.to_string().replace("-", ""))?;
        for chr in stridef.Data.chars() {
            write!(&mut f, "{}, ",(chr as u8))?;
        }
        writeln!(&mut f, "0    ; {}",stridef.Data)?;
        match stridef.Typ {
            ProgramStringType::STR  => writeln!(&mut f, "   _LEN_STRING_{}: dq {}",UUID.to_string().replace("-", ""),stridef.Data.len())?,
            ProgramStringType::CSTR => todo!(),
        }
    }
    writeln!(&mut f, "section .bss")?;
    writeln!(&mut f, "   _CALLSTACK: resb {}",program.call_stack_size)?;
    writeln!(&mut f, "   _CALLSTACK_TOP: ")?;
    
    //mov rbp, rsp
    //mov rsp, _CALLSTACK
    //call fib

    //BEGIN:
    //mov rsp, rbp
    //...
    //mov rbp, rsp
    //mov rsp, _CALLSTACK
    //ret
    //AFTER:
    //mov rsp, rbp
    //f.write(b"BITS 64");
    Ok(())

}




fn main() {
    let mut args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        usage();
        exit(1);
    }
    args.remove(0);
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
                "-noRaxWarn" => {

                    program.warn_rax_usage = false
                }
                flag => {
                    eprintln!("Error: undefined flag: {flag}")
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
    Intrinsics.insert("func".to_string()  , IntrinsicType::Func);
    Intrinsics.insert("(".to_string(),IntrinsicType::OPENPAREN);
    Intrinsics.insert(")".to_string(),IntrinsicType::CLOSEPAREN);
    Intrinsics.insert(":".to_string(),IntrinsicType::DOUBLE_COLIN);
    Intrinsics.insert(",".to_string(),IntrinsicType::COMA);
    Intrinsics.insert("{".to_string(),IntrinsicType::OPENCURLY);
    Intrinsics.insert("}".to_string(),IntrinsicType::CLOSECURLY);
    Intrinsics.insert("push".to_string(),IntrinsicType::PUSH);
    Intrinsics.insert("pop".to_string(),IntrinsicType::POP);
    Intrinsics.insert("=".to_string(),IntrinsicType::MOV_REG);
    Intrinsics.insert("+".to_string(),IntrinsicType::ADD);
    Intrinsics.insert("-".to_string(),IntrinsicType::SUB);
    Intrinsics.insert("*".to_string(),IntrinsicType::MUL);
    Intrinsics.insert("ret".to_string(),IntrinsicType::RET);
    Intrinsics.insert("include".to_string(), IntrinsicType::INCLUDE);
    let mut Definitions: HashMap<String,VarType> = HashMap::new();
    Definitions.insert("int".to_string(), VarType::INT);
    Definitions.insert("char".to_string(), VarType::CHAR);
    Definitions.insert("long".to_string(), VarType::LONG);
    Definitions.insert("bool".to_string(), VarType::BOOLEAN);
    Definitions.insert("ptr".to_string(), VarType::PTR);
    Definitions.insert("short".to_string(), VarType::SHORT);
    Definitions.insert("str".to_string(), VarType::STR);
    //println!("[DEBUG] Path {}",program.path);
    let info = fs::read_to_string(&program.path).expect("Error: could not open file!");

    let mut lexer = Lexer::new(info, & Intrinsics);
    lexer.currentLocation.file = program.path.clone();
    let build = parse_tokens_to_build(&mut lexer, &Intrinsics, &Definitions, &mut program);
    match program.typ.as_str() {
        "nasm_x86_64" => {
            to_nasm_x86_64(&build, &program).expect("Could not build to nasm_x86_64");
            if program.should_build {
                //println!("Building program!");
                let nasm = Command::new("nasm").args(["-f","elf",program.opath.as_str()]).output().expect("Could not build nasm!");
                let gcc  = Command::new("gcc").args([Path::new(program.opath.as_str()).with_extension("o").to_str().unwrap(),"-o",Path::new(program.opath.as_str()).with_extension("").to_str().unwrap()]).output().expect("Could not build gcc!");
                let _ld     = Command::new("ld".to_string()).arg(Path::new(program.opath.as_str()).with_extension("").to_str().unwrap()).output().expect("Could not build your program!");
                //let prg  = Command::new(Path::new(program.opath.as_str()).with_extension("").to_str().unwrap()).output().expect("Could not build your program!");
                //let prg = Command::new().args(["/K",(Path::new(program.opath.as_str()).with_extension("").to_str().unwrap().replace("/", "\\").as_str())]).output().expect("Could not build your program!");
                //let prg = Command::new(Path::new(program.opath.as_str()).with_extension("").to_str().unwrap().replace("/", "\\").as_str()).creation_flags(0x08000000).output().expect("Could not build program");
                //println!("{:?}","".to_string()+Path::new(program.opath.as_str()).with_extension("").to_str().unwrap());
                //println!("{:?}",["-f","elf",program.opath.as_str()]);
                //println!("{:?}",[Path::new(program.opath.as_str()).with_extension("o").to_str().unwrap(),"-o",Path::new(program.opath.as_str()).with_extension("").to_str().unwrap()]);
                //println!("Path: {}",Path::new(program.opath.as_str()).with_extension("").to_str().unwrap().replace("/", "\\").as_str());
                if !nasm.status.success() {
                    println!("Nasm: \n{:?}\n-----------",nasm);
                }
                else if !gcc.status.success() {
                    println!("Gcc:  \n{:?}\n-----------",gcc);
                }
                else {
                    println!("Finished build successfully")
                }
                //println!("{:?}",prg);
                //println!("{}{}",String::from_utf8_lossy(&prg.stdout),String::from_utf8_lossy(&prg.stderr));
                
            }
        }
        _ => {
            todo!("Unimplemented type {}",program.typ);
        }
    }
    
    //println!("{:#?}",build);
    // if !path.ends_with(".spl") {
    //     eprintln!("Error: invalid script extension! expected: \".spl\" but found {}",Path::new(&path).extension().and_then(OsStr::to_str).unwrap_or_else(|| "None"))
    // }
    // CALL STACK: 
    // <ptr> <ptr> <ptr> <ptr>
}
