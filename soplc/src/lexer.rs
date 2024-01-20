use std::process::exit;
use std::rc::Rc;
use std::collections::{HashMap,HashSet};
use std::fmt::Display;
use std::fs::File;
use std::io::Write;
use crate::{parser::*,cmdprogram::*,utils::*};
use crate::*;
#[derive(Debug, Clone,PartialEq)]
pub enum RawConstValueType{
    CHAR(i8),
    SHORT(i16),
    INT(i32),
    LONG(i64),
    STR(usize),
    PTR(Ptr, i64)
}
impl RawConstValueType {
    pub fn size(&self, program: &CmdProgram) -> usize {
        match self {
            Self::CHAR(_) => 1,
            Self::SHORT(_)   => 2,
            Self::INT(_)  => 4,
            Self::LONG(_) => 8,
            Self::STR(_) => todo!("This"),
            Self::PTR(_, _) => (program.architecture.bits/8) as usize,
            _ => self::panic!("This should be unreachable")
        }
    }
    pub fn to_string(&self, build: &BuildProgram) -> String {
        match self {
            Self::SHORT(v)  => format!("{}",v),
            Self::CHAR(v)    => format!("{}",v),
            Self::INT(v)    => format!("{}",v),
            Self::LONG(v)   => format!("{}",v),
            Self::STR(v)  => format!("\"{}\"",build.stringdefs[v.to_owned()].Data),
            Self::PTR(_, p) => format!("ptr {}",p)
        }
    }
    pub fn to_type(&self, build: &BuildProgram) -> Vec<VarType> {
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
    pub fn get_num_data(&self) -> i64 {
        match self {
            RawConstValueType::LONG(val) => val.clone(),
            RawConstValueType::INT(val) => val.clone() as i64,
            RawConstValueType::SHORT(val) => val.clone() as i64,
            RawConstValueType::CHAR(val) => val.clone() as i64,
            RawConstValueType::STR(_) => todo!(),
            RawConstValueType::PTR(_,val) => val.clone(),
        }
    }
}
#[derive(Debug, Clone,PartialEq)]
pub struct RawConstValue {
    pub typ: RawConstValueType,
    pub loc: ProgramLocation,
}
pub type CallArgs = Vec<OfP>;
#[derive(Debug, Clone)]
pub enum OfP {
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
    pub fn to_string(&self, build: &BuildProgram) -> String {
        match self {
            Self::LOCALVAR(v)      => format!("Local var {}",v),
            Self::BUFFER(b)        => format!("Buffer {}",b),
            Self::CONST(v)         => v.to_string(build),
            Self::GLOBALVAR(v)     => format!("Global var {}",v),
            Self::RESULT(v, _)     => format!("Result of {}",v)
        }
    }
    pub fn var_type_t(&self, build: &BuildProgram, local_vars: &Vec<Locals>, buffers: &Vec<BuildBuf>) -> Option<VarType> {
        
        match self {
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
    pub fn var_type(&self, build: &BuildProgram, local_vars: &Vec<HashMap<String, LocalVariable>>, buffers: &Vec<BuildBuf>) -> Option<VarType> {
        match self {
            Self::CONST(v) => Some(v.to_type(build)[0].clone()),
            Self::LOCALVAR(v) => Some(get_local_build(local_vars, v).unwrap().typ.clone()),
            Self::RESULT(f, _) => if let Some(f) = build.functions.get(f) { f.contract.Outputs.get(0).cloned() } else if let Some(f) = build.externals.get(f) { f.contract.as_ref().unwrap().Outputs.get(0).cloned()}else {None},
            Self::BUFFER(i) => {
                Some(VarType::PTR(Ptr::ref_to(buffers[i.to_owned()].typ.clone())))
            },
            Self::GLOBALVAR(v) => Some(build.global_vars.get(v).unwrap().typ.var_type(build)),
        } 
    }
    pub fn from_token(token: &Token, build: &mut BuildProgram, _: &CmdProgram, locals: &Vec<Locals>) -> Option<Self> {
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
            TokenType::StringType(val) => Some(Self::CONST(RawConstValueType::STR(build.insert_new_str(ProgramString { Typ: ProgramStringType::STR, Data:  val.clone()})))),
            TokenType::CStringType(val)=> Some(Self::CONST(RawConstValueType::STR(build.insert_new_str(ProgramString { Typ: ProgramStringType::CSTR, Data: val.clone() })))),
            TokenType::Number8(val)      => Some(Self::CONST(RawConstValueType::CHAR(val.clone()))),
            TokenType::Number16(val)      => Some(Self::CONST(RawConstValueType::SHORT(val.clone()))),
            TokenType::Number32(val)      => Some(Self::CONST(RawConstValueType::INT(val.clone()))),
            TokenType::Number64(val)      => Some(Self::CONST(RawConstValueType::LONG(val.clone()))),
            _ => None
        }
    }

}


#[derive(Debug, Clone)]
pub enum Expression {
    val(OfP),
    expr(Box<ExprTree>)
}
impl Expression {
    pub fn unwrap_val(&self) -> &OfP {
        if let Expression::val(v) = self {
            v
        }
        else {
            self::panic!("ERROR: Cannot unwrap on non val type! {:?}",self);
        }
    }
    pub fn unwrap_expr(&self) -> &Box<ExprTree> {
        if let Expression::expr(v) = self {
            v
        }
        else {
            self::panic!("ERROR: Cannot unwrap on non expr type!");
        }
    }
    pub fn is_expr(&self) -> bool {
        match self {
            Self::expr(_) => true,
            _ => false
        }
    }
    pub fn is_ofp(&self) -> bool {
        match self {
            Self::val(_) => true,
            _ => false
        }
    }
    pub fn result_of_c(&self,program: &CmdProgram,build: &BuildProgram, local_vars: &Vec<HashMap<String,LocalVariable>>, buffers: &Vec<BuildBuf>, loc: &ProgramLocation) -> Option<VarType>  {
        match self {
            Self::val(v) => v.var_type(build, local_vars,buffers),
            Self::expr(s) => {
                if let Some(v1) = &s.left {
                    let res1 = v1.result_of_c(program, build, local_vars,buffers,loc);
                    if let Some(v2) = &s.right {
                        let res2 = v2.result_of_c(program, build, local_vars,buffers,loc);
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
                           self::panic!("Unreachable")
                        }
                    }
                    else {
                        res1
                    }
                }
                else if let Some(v2) = &s.right {
                    if s.op == Op::STAR {
                        let res = v2.result_of_c(program, build, local_vars,buffers,loc);
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
                        v2.result_of_c(program, build, local_vars,buffers,loc)
                    }
                }
                else {
                    self::panic!("Unreachable")
                }
            }
        }
    }
    pub fn result_of(&self,program: &CmdProgram, build: &BuildProgram, local_vars: &Vec<Locals>, buffers: &Vec<BuildBuf>, loc: &ProgramLocation) -> Option<VarType> {
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
                            self::panic!("Unreachable at {}",loc.loc_display())
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
                    self::panic!("Unreachable")
                }
            }
        }
        
    }
}
#[derive(Debug, Clone)]
pub struct ExprTree {
    pub left:  Option<Expression>,
    pub right: Option<Expression>,
    pub op: Op
}
impl ExprTree {
    pub fn eval_const(&self) -> Option<RawConstValue> {
        match self.op {
            Op::NONE      => self::panic!("this should be unreachable"),
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
            Op::AND => todo!(),
            Op::OR => todo!(),
        }

    }
    pub fn new() -> Self {
        Self { left: None, right: None, op: Op::NONE }
    }
}

#[macro_export]
macro_rules! lex_error {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(L) [ERROR] {}: {}", $token.loc_display(), message);
        exit(1);
    });
}
#[macro_export]
macro_rules! lex_assert {
    ($token:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(L) [ERROR] {}: {}", $token.loc_display(), message);
            exit(1);
        }
    });
}
#[macro_export]
macro_rules! lex_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(L) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
    });
}
#[macro_export]
macro_rules! lex_info {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(L) [INFO] {}: {}", $token.loc_display(), message);
    });
}
#[macro_export]
macro_rules! lex_warn {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(L) [WARN] {}: {}", $token.loc_display(), message);
    });
}
#[derive(Clone,Debug, PartialEq)]
pub struct ProgramLocation {
    pub file: Rc<String>,
    pub linenumber: i32,
    pub character:  i32,
}
impl ProgramLocation {
    pub fn loc_display(&self) -> String{
        format!("{}:{}:{}",self.file,self.linenumber,self.character)
    }
}
#[repr(u32)]
#[derive(Clone, Copy,Debug,PartialEq )]
pub enum IntrinsicType {
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
impl IntrinsicType {
    pub fn to_string(&self,isplural: bool) -> String{
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
pub enum SetOp {
    SET,
    PLUSSET,
    MINUSSET,
    MULSET,
    DIVSET,
}

impl SetOp {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "="  => Some(Self::SET),
            "+=" => Some(Self::PLUSSET),
            "-=" => Some(Self::MINUSSET),
            "*=" => Some(Self::MULSET),
            "/=" => Some(Self::DIVSET),
            _ => None
        }
    }
    pub fn to_string(&self) -> &str {
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
pub enum Op {
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
    // Logic Ops
    AND,
    OR
}
impl Op {
    pub fn from_str(s: &str) -> Option<Self> {
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
            "&&"=> Some(Op::AND),
            "||"=> Some(Op::OR),
            _ => None
        }
    }
    pub fn to_string(&self) -> &str {
        match self {
            Op::NONE      => "None",
            Op::PLUS      => "+" ,
            Op::MINUS     => "-" ,
            Op::DIV       => "/" ,
            Op::STAR      => "*" ,
            Op::EQ        => "==",
            Op::NEQ       => "!=",
            Op::GT        => ">" ,
            Op::GTEQ      => ">=",
            Op::LT        => "<" ,
            Op::LTEQ      => "<=",
            Op::NOT       => "!" ,
            Op::REMAINDER => "%",
            Op::BAND      => "&",
            Op::BOR       => "|",
            Op::BNOT      => "~",
            Op::SRIGHT    => ">>",
            Op::SLEFT     => "<<",
            Op::AND       => "&&",
            Op::OR        => "||"
        }
    }
    pub fn get_priority(&self, currentETree: &ExprTree) -> usize {
        match self {
            Self::BAND | Self::BNOT | Self::BOR | Self::SRIGHT | Self::SLEFT       => 6,
            Self::AND | Self::OR => 1,
            Self::NOT =>  6,
            Self::EQ   | Self::NEQ | Self::GT | Self::LT | Self::GTEQ | Self::LTEQ  => 2,
            Self::PLUS | Self::MINUS                                               => 3,
            Self::STAR                                                             => {
                if currentETree.left.is_none() {2} else {4}},
            Self::DIV | Self::REMAINDER                                            => 4,
            Self::NONE  => self::panic!("This should not occur"),
        }
    }
    pub fn is_boolean(&self) -> bool {
        match self {
            Self::EQ   | Self::NEQ | Self::GT | Self::LT | Self::GTEQ | Self::LTEQ | Self::OR | Self::AND | Self::NOT => true,
            _ => false
        }
    }
}
#[derive(Debug,PartialEq,Clone)]
pub enum TokenType {
    WordType      (String),
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
    pub fn unwrap_numeric(&self, build: &BuildProgram) -> Option<i64> {
        match self {
            TokenType::Number8(data)   => Some(data.clone() as i64),
            TokenType::Number16(data) => Some(data.clone() as i64),
            TokenType::Number32(data) => Some(data.clone() as i64),
            TokenType::Number64(data) => Some(data.clone() as i64),
            TokenType::WordType(data) => {
                let t = build.constdefs.get(data)?;
                match &t.typ {
                    RawConstValueType::INT(data)      => Some(data.clone() as i64),
                    &RawConstValueType::LONG(data)    => Some(data.clone() as i64),
                    _ => None
                }
            }
            _ => None
        }
    }
    pub fn unwrap_setop(&self) -> &SetOp {
        match self {
            Self::SETOperation(op) => op,
            _ => self::panic!("Unreachable")
        }
    }
    pub fn is_setop(&self) -> bool {
        match self {
            Self::SETOperation(_) => true,
            _ => false
        }
    }
    pub fn to_string(&self,isplural:bool) -> String {
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
            TokenType::Operation(op) => {
                if isplural {format!("Operations").to_string()} else {format!("Operation {}",op.to_string()).to_string()}
            },
            TokenType::SETOperation(sop) => {
                if isplural {format!("Set Operations").to_string()} else {format!("Set Operation {}",sop.to_string()).to_string()}
            },

        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub location: ProgramLocation
}
impl Token {
    pub fn loc_display(&self) -> String {
        self.location.loc_display()
    }
    pub fn is_word(&self) -> bool {
        match self.typ {
            TokenType::WordType(_) => true,
            _ => false
        }
    }
    pub fn unwrap_word(&self) -> Option<&String> {
        match &self.typ {
            TokenType::WordType(data) => Some(data),
            _ => None
        }
    }
    pub fn unwrap_string(&self) -> Option<&String> {
        match &self.typ {
            TokenType::StringType(data) | TokenType::CStringType(data) => Some(data),
            _ => None
        }
    }
}
pub struct Lexer<'a> {
    pub source: Vec<char>,
    pub cursor: usize,
    pub currentLocation: ProgramLocation,
    pub Intrinsics: &'a HashMap<String,IntrinsicType>,
    pub Definitions: &'a HashMap<String,VarType>,
    pub CurrentFuncs: HashSet<String>
}

impl<'a> Lexer<'a> {
    pub fn is_newline(&mut self) -> bool {
        if let Some(c) = self.cchar_s() {
            c=='\n' || c=='\r'
        }
        else {
            true
        }

    }
    pub fn trim_left(&mut self) -> bool {
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
    pub fn is_not_empty(&self) -> bool {
        self.cursor < self.source.len()
    }
    pub fn is_not_empty_offset(&self, offset: usize) -> bool {
        self.cursor+offset < self.source.len()
    }
    pub fn new(source: &'a String, Intrinsics: &'a HashMap<String, IntrinsicType>, Definitions: &'a HashMap<String,VarType>, CurrentFuncs: HashSet<String>) -> Self {
        Self {
            source: source.chars().collect(),
            cursor: 0,
            currentLocation: ProgramLocation { file: Rc::new(String::new()), linenumber: 1, character: 0 },
            Intrinsics,
            Definitions,
            CurrentFuncs,
        }
    }
    pub fn drop_char(&mut self){
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
    pub fn drop_line(&mut self) {
        while self.is_not_empty() && self.source.get(self.cursor).unwrap() != &'\n' {
            self.cursor += 1;
        }
        self.cursor += 1;
        self.currentLocation.character = 0;
        self.currentLocation.linenumber += 1;
    }
    pub fn loc_display(&self) -> String {
        self.currentLocation.loc_display()
    }
    pub fn pop_symbol(&mut self) -> Option<String> {
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
    pub fn cchar(&mut self) -> char {
        self.source.get(self.cursor).unwrap().clone()
    }
    pub fn cchar_s(&mut self) -> Option<char> {
        self.source.get(self.cursor).clone().copied()
    }
    pub fn cchar_offset(&mut self, offset: usize) -> Option<char> {
        if self.is_not_empty_offset(offset) {
            return Some(self.source.get(self.cursor+offset).unwrap().clone())
        }
        None
    }



    pub fn next_line(&mut self) -> Option<Vec<Token>> {
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
                        self::panic!("Error: Abruptly ran out of chars!");
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
                                    self::panic!("Unknown number combo: {}",outstr);
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
                                self::panic!("Unknown number combo: {}",outstr);
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

