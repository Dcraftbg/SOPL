use std::collections::HashMap;
use crate::cmdprogram::*;
use crate::{*, panic};
#[macro_export]
macro_rules! par_error {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(P) [ERROR] {}: {}", $token.loc_display(), message);
        exit(1);
    });
}
#[macro_export]
macro_rules! par_assert {
    ($token:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(P) [ERROR] {}: {}", $token.loc_display(), message);
            exit(1);
        }
    });
}
#[macro_export]
macro_rules! par_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(P) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
    });
}
#[macro_export]
macro_rules! par_info {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(P) [INFO] {}: {}", $token.loc_display(), message);
    });
}
#[macro_export]
macro_rules! par_warn {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(P) [WARN] {}: {}", $token.loc_display(), message);
    });
}
#[derive(Debug,Clone,PartialEq)]
pub enum PtrTyp {
    VOID,
    TYP(Box<VarType>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ptr {
    pub typ: PtrTyp,
    pub inner_ref: usize,
}
impl Ptr {
    pub fn deref(&self) -> VarType {
        if self.inner_ref > 0 {
            VarType::PTR(Ptr {typ: self.typ.clone(), inner_ref: self.inner_ref-1 })
        }
        else {
            match &self.typ {
                PtrTyp::VOID => self::panic!("Cannot deref void pointer"),
                PtrTyp::TYP(t) => *(*t).clone(),
            }
        }
    }
    pub fn ref_to(typ: VarType) -> Self {
        if typ.is_ptr() {
            match typ.clone() {
                VarType::PTR(mut p) => {
                    p.inner_ref += 1;
                    p
                }
                _ => self::panic!("Unreachable")
            }
        }
        else {
            Self{inner_ref: 0, typ: PtrTyp::TYP(Box::new(typ))}
        }
    }
}
impl PtrTyp {
    pub fn to_string(&self) -> String {
        match self {
            Self::TYP(typ) => {
                typ.to_string(false)
            }
            Self::VOID => "void".to_owned()
        }
    }

}
#[derive(Clone, Debug, PartialEq)]
pub enum VarType {
    CHAR,
    SHORT,
    BOOLEAN,
    INT,
    LONG,
    PTR(Ptr),
    CUSTOM(usize),
}
impl VarType {
    pub fn is_numeric(&self) -> bool {
        match self {
            Self::PTR(_) => true,
            Self::CHAR | Self::SHORT  | Self::INT | Self::LONG => true,
            _ => false
        }
    }
    pub fn is_ptr(&self) -> bool {
        match self {
            Self::PTR(_) => true,
            _ => false
        }
    }
    pub fn get_ptr_val(&self) -> Option<VarType> {
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
                            _ => self::panic!("Unreachable"),

                        }
                    }
                }
            }
            _ => None
        }
    }
    pub fn is_some_ptr(&self) -> bool {
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
    pub fn to_string(&self, isplural: bool) -> String {
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
    pub fn weak_eq(&self, other: &Self) -> bool {
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
#[derive(Debug, Clone)]
pub struct BuildBuf {
    pub typ: VarType,
    pub size: usize,
    pub offset: usize,
}
impl BuildBuf {
    pub fn from_typ(typ: VarType) -> Self {
        Self { typ, size: 0, offset: 0 }
    }
    pub fn from_parse_buf(typ: VarType, size: usize) -> Self {
        Self { typ, size, offset: 0 }
    }
}

#[derive(Debug)]
pub struct BuildProgram {
    pub externals:    HashMap<String, External>,
    pub functions:    HashMap<String, Function>,
    pub stringdefs:   Vec<ProgramString>,
    pub constdefs:    HashMap<String, RawConstValue>,
    pub dll_imports:  HashMap<String, DLL_import>,
    pub dll_exports:  HashMap<String, DLL_export>,
    pub global_vars:  HashMap<String, GlobalVar>,
    pub buffers:      Vec<BuildBuf>,
    pub stringoffset: usize,
}
impl BuildProgram {
    pub fn insert_new_str(&mut self, str: ProgramString) -> usize {
        let id = self.stringdefs.len()+self.stringoffset;
        self.stringdefs.push(str);
        id
    }
    pub fn contains_symbol(&self, str: &String) -> bool {
        self.constdefs.contains_key(str) || self.dll_imports.contains_key(str) || self.externals.contains_key(str) || self.functions.contains_key(str) || self.global_vars.contains_key(str)
    }
    pub fn get_location_of_symbol(&self, str: &String) -> Option<ProgramLocation> {
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
    pub fn get_contract_of_symbol(&self, str: &String) -> Option<AnyContract> {
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
    pub fn new() -> Self {
        Self { externals: HashMap::new(), functions: HashMap::new(),stringdefs: Vec::new(), constdefs: HashMap::new(), dll_imports: HashMap::new(), dll_exports: HashMap::new(),
            global_vars: HashMap::new(), 
            buffers: Vec::new(),
            stringoffset: 0
        }
    }
}


pub fn tokens_to_expression(body: &[Token],build: &mut BuildProgram, program: &CmdProgram,locals: &Vec<Locals>, buffers: &mut Vec<BuildBuf>, loc: &ProgramLocation) -> Expression {
    
    let mut bracketcount = 0;
    if body.len() == 0 {
        //panic!("Error: Cannot evaluate empty body");
        par_error!(loc, "Cannot evaluate empty body");
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
            currentETree.right = Some(tokens_to_expression(&body[i..i2],build,program,locals,buffers,&body[i].location));
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
                    let expr = tokens_to_expression(&body[index_from..i-1], build, program, locals,buffers,&body[i-1].location);
                    
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
    //println!("Final currentETree: {:#?}",currentETree);
    Expression::expr(Box::new(currentETree))
}

#[derive(Debug,Clone)]
pub enum ExternalType {
    RawExternal,
    CExternal
}
#[derive(Debug,Clone)]
pub struct External {
    pub typ: ExternalType,
    pub loc: ProgramLocation,
    pub contract: Option<AnyContract>
}
impl ExternalType {
   pub fn to_string(&self) -> String {
       match self {
           ExternalType::RawExternal => "RawExternal".to_string(),
           ExternalType::CExternal => "CExternal".to_string()
       }
   }
}
