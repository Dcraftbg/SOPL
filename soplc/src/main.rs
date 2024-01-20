#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(unreachable_patterns)]
#![allow(unreachable_code)]


mod lexer;
mod parser;
mod utils;
mod cmdprogram;
mod register;
mod nasm_x64;
mod cfor;

use register::*;
use lexer::*;
use parser::*;
use cmdprogram::*;
use utils::*;
use nasm_x64::*;


use core::{num, panic};
use std::{env, process::{exit, Command, Stdio}, path::{Path, PathBuf}, ffi::OsStr, str::{FromStr, Chars}, collections::{HashMap, HashSet}, hash::Hash, fs::{File, self}, io::{Read, Write, self}, fmt::{format, Display}, borrow::{BorrowMut, Borrow}, clone, time::{SystemTime, Instant}, rc::Rc, iter::Peekable, cell::{RefCell, Ref, RefMut}, ops::{Deref, DerefMut}, vec, sync::Arc, os, f32::consts::E, any::Any};
use linked_hash_map::LinkedHashMap;
use serde_json::Value;


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

type RawConstants = HashMap<String, RawConstValue>;
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


#[derive(Debug, Clone)]
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
pub struct AnyContract {
    pub InputPool: ContractInputPool,
    pub Outputs: Vec<VarType>
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
fn nasm_x86_64_prep_args(program: &CmdProgram, build: &BuildProgram, f: &mut File, contract: &Vec<OfP>, mut stack_size: usize, local_vars: &Vec<HashMap<String, LocalVariable>>,buffers: &Vec<BuildBuf>) -> io::Result<usize> {
    let org_stack_size = stack_size;
    let mut int_passed_count: usize = 0;
    let mut offset: usize=0;
    let shadow_space = if let ArcPassType::CUSTOM(arc) = &program.architecture.options.argumentPassing { arc.shadow_space } else { 0 };
    if program.architecture.options.argumentPassing != ArcPassType::PUSHALL {
        for iargs in contract.iter().rev() {
            if program.architecture.options.argumentPassing.custom_unwrap().nums_ptrs.is_some() && int_passed_count >= program.architecture.options.argumentPassing.custom_unwrap().nums_ptrs.as_ref().unwrap().len() {
                offset += iargs.var_type(build, local_vars,buffers).unwrap().get_size(program);
                offset += offset%8;
            }
            match iargs.var_type(build, local_vars,buffers).unwrap()  {
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
                    stack_size += stack_size%8;
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
                            stack_size += stack_size%8;
                            if additional_space+8-offset < shadow_space {
                                writeln!(f, "   mov {} [{}+{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), shadow_space-(additional_space-offset)-8, oreg.to_string())?;
                            }
                            else if additional_space+8 -offset == shadow_space {
                                writeln!(f, "   mov {} [{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), oreg)?
                            }
                            else {
                                writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), additional_space+8-offset-shadow_space, oreg.to_string())?;
                            }
                            offset-=oreg.size();
                            offset-=offset%8;
                        }
                        int_passed_count-=1;
                    }
                    else {
                        stack_size += oreg.size();
                        stack_size += stack_size%8;
                        writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), offset, oreg.to_string())?;
                        offset-=oreg.size();
                        offset-=offset%8;
                    }
                }
            }
            OfP::LOCALVAR(v) => {
                let var1 = get_local_build(local_vars, v).expect("Unknown local variable parameter");
                let oreg = Register::RAX.to_byte_size(var1.typ.get_size(program));
                if org_stack_size-var1.operand == 0 {
                    writeln!(f, "   mov {}, {} [{}]",oreg.to_string(),size_to_nasm_type(oreg.size()),program.stack_ptr())?;
                }
                else {
                    writeln!(f, "   mov {}, {} [{}+{}]",oreg.to_string(),size_to_nasm_type(oreg.size()),program.stack_ptr(),org_stack_size-var1.operand)?;
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
                            stack_size += stack_size%8;
                            if additional_space+8-offset < shadow_space {
                                writeln!(f, "   mov {} [{}+{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), shadow_space-(additional_space-offset)-8, oreg.to_string())?;
                            }
                            else if additional_space+8 -offset == shadow_space {
                                writeln!(f, "   mov {} [{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), oreg)?
                            }
                            else {
                                writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), additional_space+8-offset-shadow_space, oreg.to_string())?;
                            }
                            offset-=oreg.size();
                            offset-=offset%8;
                        }
                        int_passed_count-=1;
                    }
                    else {
                        stack_size += oreg.size();
                        stack_size += stack_size%8;
                        writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(),offset, oreg.to_string())?;
                        offset-=oreg.size();
                        offset-=offset%8;
                    }
                }
            }
            OfP::CONST(val)  => {
                let oregs = val.LRNasm(f, build, program,&vec![Register::RAX, Register::RBX])?;
                if program.architecture.options.argumentPassing == ArcPassType::PUSHALL ||  program.architecture.options.argumentPassing.custom_get().is_none() ||  program.architecture.options.argumentPassing.custom_unwrap().nums_ptrs.is_none(){
                    for oreg in oregs {
                        stack_size += oreg.size();
                        stack_size += stack_size%8;
                        writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), offset, oreg.to_string())?;
                        offset-=oreg.size();
                        offset-=offset%8;
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
                                stack_size += stack_size%8;
                                if additional_space+8-offset < shadow_space {
                                    writeln!(f, "   mov {} [{}+{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), shadow_space-(additional_space-offset)-8, oreg.to_string())?;
                                }
                                else if additional_space+8 -offset == shadow_space {
                                    writeln!(f, "   mov {} [{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), oreg)?
                                }
                                else {
                                    writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(oreg.size()), program.stack_ptr(), additional_space+8-offset-shadow_space, oreg.to_string())?;
                                }
                                offset-=oreg.size();
                                offset-=offset%8;
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
    //let mut offset: usize = 0;
    if program.architecture.options.argumentPassing == ArcPassType::PUSHALL {        
        todo!("Im sure there is a bug to do here with x86 assembly and passing parameters and offsets having to be 8 aligned");
    }
    else {
        let custom_build = program.architecture.options.argumentPassing.custom_unwrap();

        let mut offset_of_ins: usize = 0;
        let mut int_ptr_count: usize = 0;
        
        for (_, iarg) in scope.get_contract(build).unwrap().Inputs.iter().rev() {
            if program.architecture.options.argumentPassing.custom_unwrap().nums_ptrs.is_some() && int_ptr_count >= program.architecture.options.argumentPassing.custom_unwrap().nums_ptrs.as_ref().unwrap().len() {
                // if offset_of_ins%8+iarg.get_size(program) > 8 {
                //     offset_of_ins+=8-offset_of_ins%8;
                // }
                offset_of_ins += iarg.get_size(program);
                offset_of_ins += offset_of_ins%8;
            }
            match iarg {
                VarType::CHAR    | VarType::SHORT   | VarType::BOOLEAN | VarType::INT     | VarType::LONG    | VarType::PTR(_)  => int_ptr_count+=1,
                VarType::CUSTOM(_) => {},
            }
        }
        
        //if offset_of_ins%8 > 0 {
        //    offset_of_ins+=offset_of_ins%8;
        //}
        let mut offset: usize = 0;
        for (_, iarg) in scope.get_contract(build).unwrap().Inputs.iter().rev() {
            if custom_build.nums_ptrs.is_some() && custom_build.nums_ptrs.as_ref().unwrap().len() > int_ptr_count-1 {
                let osize = iarg.get_size(program);
                let ireg = &custom_build.nums_ptrs.as_ref().unwrap()[int_ptr_count-1].to_byte_size(osize);
                if offset%8+osize > 8 {
                    offset+=8-offset%8;
                }
                offset+=osize;
                writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(osize), program.stack_ptr(), offset, ireg.to_string())?;
                // if (offset-osize)%8 > 0 {
                //     offset-=8-(offset-osize)%8;
                // }
                // offset-=osize;
                int_ptr_count-=1
            }
            else {
                let osize = iarg.get_size(program);
                let reg = Register::RAX.to_byte_size(osize);
                writeln!(f, "   mov {}, {} [{}+{}]",reg.to_string(), size_to_nasm_type(osize),program.stack_ptr(),offset_of_ins+shadow_space)?;
                if offset%8+osize > 8 {
                    offset+=8-offset%8;
                }
                offset+=osize;
                writeln!(f, "   mov {} [{}-{}], {}",size_to_nasm_type(osize),program.stack_ptr(),offset,reg.to_string())?;
                //if (offset_of_ins-osize)%8 > 0 {
                //    offset_of_ins-=8-(offset_of_ins-osize)%8;
                //}
                
                offset_of_ins-=osize;
                offset_of_ins-=offset_of_ins%8;
                int_ptr_count-=1
            }
        };
    }
    Ok(0)
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
                            Op2.LEIRnasm(vec![*Reg1,oreg2], f, program, build,&local_vars, &bufs,stack_size, loc,inst_count.clone(),0)?;
                        }
                        OfP::LOCALVAR(varOrg) => {
                            let var = com_expect!(loc,get_local_build(&local_vars,varOrg),"Error: Unknown variable found during compilation {}",varOrg);
                            let oreg  = Register::RAX.to_byte_size(var.typ.get_size(program));
                            let oreg2 = Register::RBX.to_byte_size(var.typ.get_size(program));
                            let result = Op2.result_of_c(program, build, local_vars, &bufs, loc).unwrap();
                            if result.get_size(program) < var.typ.get_size(program) {
                                writeln!(f, "   xor {}, {}",Register::RAX.to_byte_size(var.typ.get_size(program)),Register::RAX.to_byte_size(var.typ.get_size(program)))?
                            }
                            let oregs = Op2.LEIRnasm(vec![oreg,oreg2,Register::RCX.to_byte_size(var.typ.get_size(program))], f, program,build, &local_vars, &bufs,stack_size, loc,inst_count.clone(),0)?;
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
                    let res_right = expr.right.as_ref().unwrap().result_of_c(program, build, &local_vars, &bufs,loc).unwrap();
                    
                    com_assert!(loc, res_right.is_some_ptr(), "Error: Cannot dereference void pointer");
                    let oregs = expr.right.as_ref().unwrap().LEIRnasm(vec![Register::RAX.to_byte_size(res_right.get_size(program)),Register::RBX.to_byte_size(res_right.get_size(program)),Register::RCX.to_byte_size(res_right.get_size(program))], f, program, build, &local_vars, &bufs,stack_size, loc,inst_count.clone(),0)?;
                    let res_deref_val = res_right.get_ptr_val().unwrap();
                    writeln!(f, "   mov rsi, {}",oregs[0].to_string())?;
                    let oregs2 = Op2.LEIRnasm(vec![Register::RAX.to_byte_size(res_deref_val.get_size(program)),Register::RBX.to_byte_size(res_deref_val.get_size(program)),Register::RCX.to_byte_size(res_deref_val.get_size(program))], f, program, build, &local_vars, &bufs,stack_size, loc,inst_count.clone(),1)?;
                    writeln!(f, "   mov {} [rsi], {}",size_to_nasm_type(res_deref_val.get_size(program)),oregs2[0].to_byte_size(res_deref_val.get_size(program)).to_string())?;
                }
            }
            Instruction::CALLRAW(Word, contract) => {
                let sp_taken = nasm_x86_64_prep_args(program, build, f, contract, stack_size, &local_vars,&bufs)?;
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
                        op2.LEIRnasm(vec![reg2,reg3], f, program,build, &local_vars, &bufs,stack_size, loc,inst_count.clone(),0)?;
                        writeln!(f, "   add {}, {}",reg1.to_string(), reg2.to_string())?;
                    }
                    OfP::LOCALVAR(var1) => {
                        let var1 = com_expect!(loc,get_local_build(&local_vars,var1),"Error: Unknown variable found during compilation {}",var1);
                        let reg1 = Register::RAX.to_byte_size(var1.typ.get_size(program));
                        let reg2 = Register::RBX.to_byte_size(reg1.size());
                        op2.LEIRnasm(vec![reg1,reg2], f, program,build, &local_vars, &bufs,stack_size, loc,inst_count.clone(),0)?;
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
                        op2.LEIRnasm(vec![reg2,reg3], f, program,build, &local_vars, &bufs,stack_size, loc,inst_count.clone(),0)?;
                        writeln!(f, "   sub {}, {}",reg1.to_string(), reg2.to_string())?;
                    }
                    OfP::LOCALVAR(var1) => {
                        let var1 = com_expect!(loc,get_local_build(&local_vars,var1),"Error: Unknown variable found during compilation {}",var1);
                        let reg1 = Register::RAX.to_byte_size(var1.typ.get_size(program));
                        let reg2 = Register::RBX.to_byte_size(reg1.size());
                        op2.LEIRnasm(vec![reg1,reg2], f, program,build, &local_vars, &bufs,stack_size, loc,inst_count.clone(),0)?;
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
                        let res = op2.LEIRnasm(vec![reg2], f, program,build, &local_vars, &bufs,stack_size, loc,inst_count.clone(),0)?;
                        if res[0].to_byte_size(8) != Register::RAX {
                            writeln!(f, "   mov {}, {}",Register::RAX.to_byte_size(res[0].size()),res[0])?
                        }
                        writeln!(f, "   cqo")?;
                        writeln!(f, "   imul {}",reg1.to_string())?;
                    }
                    OfP::LOCALVAR(var1) => {
                        let var1 = com_expect!(loc,get_local_build(&local_vars, var1),"Error: Unknown variable found during compilation {}",var1);
                        let reg1 = Register::RAX.to_byte_size(var1.typ.get_size(program));
                        let res = op2.LEIRnasm(vec![reg1], f, program,build, &local_vars, &bufs,stack_size, loc,inst_count.clone(),0)?;
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
                let sp_taken = nasm_x86_64_prep_args(program, build, f, args, stack_size, &local_vars,&bufs)?;
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
                let _regs = expr.LEIRnasm(vec![Register::RAX,Register::RBX,Register::RCX], f, program, build, &local_vars, &bufs,stack_size, loc,inst_count.clone(),0)?;
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
                let olabel = format!(".IF_SCOPE_{}",binst);
                let condition = match s.typ {
                    NormalScopeType::IF(ref c) => c,
                    _ => panic!("Unreachable")
                };
                condition.jumpif_nasm_x86_64(&olabel,f, program, build, loc, stack_size, local_vars, &bufs,inst_count.clone(),0)?;
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
                let olabel = format!(".WHILE_SCOPE_END_{}",inst_count);
                let fallback_label = format!(".WHILE_SCOPE_{}",inst_count);
                writeln!(f, "   {}:",fallback_label)?;

                let condition = match s.typ {
                    NormalScopeType::WHILE(ref c) => c,
                    _ => panic!("Unreachable")
                };
                condition.jumpifn_nasm_x86_64(&olabel,f, program, build, loc, stack_size, local_vars, &bufs,inst_count.clone(),0)?;
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
            if program.in_mode != OptimizationMode::DEBUG && !optimization.usedFuncs.contains(function_name) && program.print_unused_warns && program.print_unused_funcs {
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
    println!("{}- nasm_x86 (Secondary to x86_64, first features get implemented for 64 bit and afterwards to 32 bit)",indent);
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
    Architectures.insert("windows_x86_64".to_owned(), Architecture { bits: 64, platform: "windows".to_string(), cextern_prefix: "".to_owned(),  func_prefix:"".to_owned(),  options: ArcOps { argumentPassing: ArcPassType::CUSTOM(ArcCustomOps { nums_ptrs: Some(vec![RCX, RDX,R8,R9]), floats: Some(vec![XMM0,XMM1,XMM2,XMM3]), returns: Some(vec![RAX]), on_overflow_stack: true, shadow_space: 40}) }        , obj_extension: "obj".to_owned(), flags: ArcFlags { nasm: vec!["-f".to_string(),"win64".to_string()] }});
    Architectures.insert("windows_x86".to_owned(),    Architecture { bits: 32, platform: "windows".to_string(), cextern_prefix: "_".to_owned(), func_prefix:"_".to_owned(), options: ArcOps { argumentPassing: ArcPassType::PUSHALL }                                                                                                                                                                            , obj_extension: "obj".to_owned(), flags: ArcFlags { nasm: vec!["-f".to_string(),"win32".to_string()] }});
    Architectures.insert("linux_x86_64".to_owned(),   Architecture { bits: 64, platform: "linux".to_string()  , cextern_prefix: "".to_owned(),  func_prefix:"".to_owned(),  options: ArcOps { argumentPassing: ArcPassType::CUSTOM(ArcCustomOps { nums_ptrs: Some(vec![RDI, RSI,RDX,RCX,R8,R9]), floats: Some(vec![XMM0,XMM1,XMM2,XMM3]), returns: Some(vec![RAX]), on_overflow_stack: true, shadow_space: 0 }) }, obj_extension: "o".to_owned()  , flags: ArcFlags { nasm: vec!["-f".to_string(),"elf64".to_string()] }});
    Architectures.insert("linux_x86".to_owned(),      Architecture { bits: 32, platform: "linux".to_string()  , cextern_prefix: "_".to_owned(), func_prefix:"_".to_owned(), options: ArcOps { argumentPassing: ArcPassType::PUSHALL }                                                                                                                                                                            , obj_extension: "o".to_owned()  , flags: ArcFlags { nasm: vec!["-f".to_string(),"elf32".to_string()] }});
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
        "nasm_x86" => {
            todo!("Nasm x86 is not supported yet");
        }
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
        "nasm_x86" => {
            todo!("Nasm x86 is not supported yet");
        }
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
                if !gcc.status.success() {
                    println!("--------------");
                    println!("Gcc:  \n{:?}",gcc);
                    println!("--------------");
                    exit(nasm.status.code().unwrap_or(0));
                }
                println!("--------------");
                println!("   - Finished build successfully");
                println!("--------------");
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
