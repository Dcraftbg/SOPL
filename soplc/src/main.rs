#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(unreachable_patterns)]
#![allow(dead_code)]
#![allow(unreachable_code)]

mod lexer;
mod parser;
mod utils;
mod cmdprogram;
mod cfor;
mod constants;
mod build_llvm;

use lexer::*;
use parser::*;
use cmdprogram::*;
use utils::*;
use constants::*;

use core::{num, panic};
use std::{env, process::{exit, Command, Stdio}, path::{Path, PathBuf}, ffi::OsStr, str::{FromStr, Chars}, collections::{HashMap, HashSet}, hash::Hash, fs::{File, self}, io::{Read, Write, self}, fmt::{format, Display}, borrow::{BorrowMut, Borrow}, clone, time::{SystemTime, Instant}, rc::Rc, iter::Peekable, cell::{RefCell, Ref, RefMut}, ops::{Deref, DerefMut}, vec, sync::Arc, os, f32::consts::E, any::Any};
use linked_hash_map::LinkedHashMap;
use serde_json::Value;


#[derive(Debug,PartialEq,Clone)]
enum CallArgType {
    LOCALVAR(String),
    CONSTANT(RawConstValueType)
}
#[derive(Debug,PartialEq,Clone)]
struct CallArg {
    pub typ: CallArgType,
    pub loc: ProgramLocation
}
#[derive(Debug)]
pub enum Instruction {
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
pub struct ProgramString {
    Typ:  ProgramStringType,
    Data: String
}
#[derive(Debug,Clone)]
pub struct LocalVariable {
    pub typ: VarType,
    pub operand: usize
}
type Locals = LinkedHashMap<String, VarType>; //The issue is here because we use a normal hashmap instead of a linked one
#[derive(Debug)]
pub struct Function {
    pub contract: FunctionContract,
    pub locals: Locals,
    pub location: ProgramLocation,
    pub body: Vec<(ProgramLocation,Instruction)>,
    pub buffers: Vec<BuildBuf>,
}
impl Function {
    fn loc_display(&self) -> String {
        self.location.loc_display()
    }
}

#[derive(Debug)]
pub struct DLL_import {
    pub from: String,
    pub contract: AnyContract
}
#[derive(Debug)]
pub struct DLL_export {
    pub contract: AnyContract
}
#[derive(Debug)]
pub enum GlobalVarType {
    BUFFER(usize),
    CONSTANT(RawConstValueType)
}
impl GlobalVarType {
    pub fn var_type(&self, build: &BuildProgram) -> VarType {
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
pub struct GlobalVar {
    pub typ: GlobalVarType,
    pub loc: ProgramLocation
}
type ScopeBody = Vec<(ProgramLocation,Instruction)>;
type ContractInputs = LinkedHashMap<String, VarType>;


#[derive(Debug, Clone, Default)]
pub struct ContractInputPool {
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
pub struct FunctionContract {
    Inputs: ContractInputs,
    Output: Option<VarType>
}
impl FunctionContract {
    fn to_any_contract(&self) -> AnyContract {
        AnyContract { InputPool: {
            let mut o: Vec<VarType> = Vec::with_capacity(self.Inputs.len());
            for (_,val) in self.Inputs.iter() {
                o.push(val.clone());
            }
            ContractInputPool { body: o, is_dynamic: false, dynamic_type: None}
        }, Output: self.Output.clone() }
    }
}
#[derive(Debug, Clone, Default)]
pub struct AnyContract {
    pub InputPool: ContractInputPool,
    pub Output: Option<VarType>
}
#[derive(Debug)]
pub enum NormalScopeType {
    IF(Expression),
    ELSE,
    WHILE(Expression),
    EMPTY
}
impl NormalScopeType {
    pub fn unwrap_expr(&self) -> &Expression {
        match self {
            NormalScopeType::IF(c) => c,
            NormalScopeType::ELSE => panic!("This should be unreachable"),
            NormalScopeType::WHILE(c) => c,
            NormalScopeType::EMPTY => panic!("This should be unreachable"),
        }
    }
}

#[derive(Debug)]
pub struct NormalScope {
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
    let mut out = AnyContract { InputPool: ContractInputPool::new(), Output: None };
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
            TokenType::Definition(ref Def) => {
                expectNextSY = true;
                if is_input {
                    out.InputPool.push(Def.clone());
                }
                else {
                    par_assert!(token, out.Output.is_none(), "Multiple return types are not allowed");
                    out.Output = Some(Def.clone());
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
    let mut out = FunctionContract {Inputs: LinkedHashMap::new(), Output: None};
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
            TokenType::Definition(ref Def) => {
                expectNextSY = true;
                par_assert!(token, out.Output.is_none(), "Multiple return types are not allowed");
                out.Output = Some(Def.clone())
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
                let expr = tokens_to_expression(&expr_body, build, program, &currentLocals,par_expect!(lexer.currentLocation, currentScope.typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"),&token.location);
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
                            let mut contract: AnyContract = AnyContract::default();

                            if let Some(tok) = lexer.peekable().peek() {
                                if tok.typ == TokenType::IntrinsicType(IntrinsicType::OPENPAREN) {
                                    let tok = tok.clone();
                                    contract = parse_any_contract(lexer);
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
                                            let mut contract: AnyContract = AnyContract::default();
                                            if let Some(tok) = lexer.peekable().peek() {
                                                if tok.typ == TokenType::IntrinsicType(IntrinsicType::OPENPAREN) {
                                                    contract = parse_any_contract(lexer);
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
                            for (inn,inp) in contract.Inputs.iter().rev() {
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
                    let res = tokens_to_expression(&result_body, build, program, &currentLocals,par_expect!(lexer.currentLocation, getTopMut(scopeStack).unwrap().typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"),&token.location);
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
                            nprogram.path = opath;
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
                            let res = tokens_to_expression(&body, build, program, &currentLocals,par_expect!(lexer.currentLocation, currentScope.typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"),&token.location);
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
                    let condi = tokens_to_expression(&condition, build, program, &currentLocals, par_expect!(lexer.currentLocation, getTopMut(scopeStack).unwrap().typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"),&token.location);
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
                    let condi = tokens_to_expression(&condition, build, program, &currentLocals, par_expect!(lexer.currentLocation, getTopMut(scopeStack).unwrap().typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"),&token.location);
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
            let expr = tokens_to_expression(&expr_body, build, program, &currentLocals, par_expect!(lexer.currentLocation, currentScope.typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"),&token.location);
            let expr_2 = tokens_to_expression(&body, build, program, &currentLocals,par_expect!(lexer.currentLocation, currentScope.typ.buffers_unwrap_mut(), "Error: Expected to find buffers but found none"),&token.location);
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
fn get_local_build<'a>(currentLocals: &'a Vec<HashMap<String, LocalVariable>>, name: &String) -> Option<&'a LocalVariable> {
    for e in currentLocals {
        if let Some(v) = e.get(name) {
            return Some(v);
        }
    }
    None
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
                let mut externContract = build.get_contract_of_symbol(name).unwrap_or(AnyContract { InputPool: ContractInputPool::new(), Output: None }).clone();//build.externals.get(name).unwrap().contract.as_ref().unwrap_or(&AnyContract { InputPool: vec![], Outputs: vec![] }).clone();
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
        if !scope.get_contract(build).unwrap().Output.is_none() {
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
    println!("         -t (target)                                     -> compiles to the given target (default is {TARGET_DEFAULT})");
    println!("         -o (output path)                                -> outputs to that file (example: hello.asm in nasm_x86_64 mode). If the output path is not specified it defaults to the modes default (for nasm_x86_64 thats a.asm)");
    println!("         -i (path to directory to shortcut)              -> Adds a shortcut when including (if it finds the file it automatically expands the path) so you could do -i libs/libc and then just do include \"stdio.spl\" instead of the whole ordeal you had before");
    println!("         -release                                        -> builds the program in release mode");
    println!("         -ntc                                            -> (NoTypeChecking) Disable type checking");
    println!("         -warn (all, funcs, externs, strings)            -> Enable unused warns for parameter");
    println!("         -ruf                                            -> Remove unused functions");
    println!("         -usage                                          -> Show this page");
    println!("--------------------------------------------");
}
fn dump_tokens(lexer: &mut Lexer) {
    while let Some(token) = lexer.next() {
        println!("Token: {:?}",token);
    }
}

type TargetBuildfn_t = fn(&CmdProgram, &BuildProgram, &str) -> io::Result<()>;
struct Target {
    build: TargetBuildfn_t,
    name: &'static str,
}
const TARGET_DEFAULT: &'static str = "llvm-native";
const TARGETS: &[Target] = &[
    Target {
        name: "llvm-native",
        build: build_llvm::build_llvm_native,
    },
];



fn list_targets(indent: usize){
    let indent = " ".repeat(indent);
    for target in TARGETS {
        println!("{}-{}", indent, target.name);
    }
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
    
    program.opath = Path::new(&program.path).with_extension(".o").to_str().unwrap().to_string();
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
    let target = TARGETS.iter().find(|t| t.name == program.target.as_str());
    if target.is_none() {
        eprintln!("Unknown target: \"{}\"", program.target);
        list_targets(1);
        exit(1);
    }
    let target = target.unwrap();
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
    
    if program.path.is_empty() {
        eprintln!("[ERROR] Unspecified input file!");
        usage(&program_n);
        exit(1);
    }
    let info = fs::read_to_string(&program.path);
    if let Err(info) = info {
        eprintln!("[ERROR] Could not read file \"{}\"!",program.path);
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
    let b = (target.build)(&program, &build, &program.opath);
    if let Err(err) = b {
        eprintln!("[ERROR] Could not compile {} for {} with out {}, because:\n{}", program.path, target.name, program.opath, err);
        exit(1);
    }
    println!("b: {:?}",b);
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


- [x] TODO: Implement || and && boolean logic
- [x] TODO: Split lexer, parser and others into separate files!
- [ ] TODO: Split x86 and x86_64 LIKE IT SHOULD BE you dingus!
- [ ] TODO: Generate AST instead the stuff its currently generating
- [ ] TODO: Implement passing for 8+ byte values
- [ ] TODO: Implement return for 8+ byte values
- [ ] TODO: Implement structs
- [ ] TODO: Implement function overloading
- [ ] TODO: Make it so that get_body returns None if scope has not been opened yet
- [ ] TODO: Update all of readme and add more documentation
- [ ] TODO: Add more useful examples
- [ ] TODO: Add some quality of life things such as __FILE__ __LINE__
*/

// What could have been kinda cool is if we had like:
/*

enum IntermediateInst {
    DEFFUNC(String),
    
}

struct IntermediateBuild {
    instructions: Vec<IntermediateInst>,
    zero_initialized_allocations: Vec<(String,usize)>,
    allocations: Vec<(String,usize)>
}

fn build_to_intermediate() -> IntermediateBuild {

}
*/
