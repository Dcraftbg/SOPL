use std::process::exit;
use std::rc::Rc;
use std::collections::{HashMap,HashSet};
use std::fmt::Display;
use std::fs::File;
use std::io::Write;
use crate::{parser::*,cmdprogram::*,register::*,utils::*};
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
    pub fn LRNasm(&self, f: &mut File, build: &BuildProgram, program: &CmdProgram, iregs: &Vec<Register>) -> std::io::Result<Vec<Register>> {
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
pub struct RawConstValue {
    pub typ: RawConstValueType,
    pub loc: ProgramLocation,
}
pub type CallArgs = Vec<OfP>;
#[derive(Debug, Clone)]
pub enum OfP {
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
    pub fn to_string(&self, build: &BuildProgram) -> String {
        match self {
            Self::REGISTER(reg)    => reg.to_string(),
            Self::LOCALVAR(v)      => format!("Local var {}",v),
            Self::BUFFER(b)        => format!("Buffer {}",b),
            Self::CONST(v)         => v.to_string(build),
            Self::GLOBALVAR(v)     => format!("Global var {}",v),
            Self::RESULT(v, _)     => format!("Result of {}",v)
        }
    }
    pub fn var_type_t(&self, build: &BuildProgram, local_vars: &Vec<Locals>, buffers: &Vec<BuildBuf>) -> Option<VarType> {
        
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
    pub fn var_type(&self, build: &BuildProgram, local_vars: &Vec<HashMap<String, LocalVariable>>, buffers: &Vec<BuildBuf>) -> Option<VarType> {
        match self {
            Self::REGISTER(reg) => Some(reg.to_var_type()),
            Self::CONST(v) => Some(v.to_type(build)[0].clone()),
            Self::LOCALVAR(v) => Some(get_local_build(local_vars, v).unwrap().typ.clone()),
            Self::RESULT(f, _) => if let Some(f) = build.functions.get(f) { f.contract.Outputs.get(0).cloned() } else if let Some(f) = build.externals.get(f) { f.contract.as_ref().unwrap().Outputs.get(0).cloned()}else {None},
            Self::BUFFER(i) => {
                Some(VarType::PTR(Ptr::ref_to(buffers[i.to_owned()].typ.clone())))
            },
            Self::GLOBALVAR(v) => Some(build.global_vars.get(v).unwrap().typ.var_type(build)),
        } 
    }
    pub fn LOIRGNasm(&self, regs: Vec<Register>, f: &mut File, program: &CmdProgram,build: &BuildProgram, local_vars: &Vec<HashMap<String, LocalVariable>>, buffers: &Vec<BuildBuf>, stack_size: usize, loc: &ProgramLocation) -> std::io::Result<Vec<Register>>{
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
                let sp_taken = nasm_x86_64_prep_args(program, build, f, args, stack_size, local_vars,buffers)?;
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
            TokenType::Register(reg) => Some(Self::REGISTER(reg.clone())),
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
    pub fn jumpif_nasm_x86_64(&self, label: &String, f: &mut File, program: &CmdProgram, build: &BuildProgram, loc: &ProgramLocation, stack_size: usize, local_vars: &Vec<HashMap<String, LocalVariable>>,buffers: &Vec<BuildBuf>,binst: usize,expr_count: usize) -> io::Result<()>{
        match self {
            Self::val(v) => {
                match v {
                    OfP::BUFFER(_) => com_error!(loc,"Cannot have buffers in conditions"),
                    OfP::CONST(cv) => {
                        match cv {
                            RawConstValueType::CHAR(c) => {
                                if *c != 0{
                                    writeln!(f,"   jmp {}",label)?;
                                }
                            }
                            RawConstValueType::SHORT(c) => {
                                if *c != 0 {
                                    writeln!(f,"   jmp {}",label)?
                                }
                            }
                            RawConstValueType::INT(c) => {
                                if *c != 0{
                                    writeln!(f,"   jmp {}",label)?;
                                }
                            }
                            RawConstValueType::LONG(c) => {
                                if *c != 0 {
                                    writeln!(f,"   jmp {}",label)?;
                                }
                            }
                            RawConstValueType::PTR(_, c) => {
                                if *c != 0 {
                                    writeln!(f,"   jmp {}",label)?
                                }
                            }
                            RawConstValueType::STR(_) => {
                                com_error!(loc, "Error: Cannot have Strings inside conditions")
                            }
                        }
                    }
                    OfP::RESULT(func, args) => {
                        let sp_taken = nasm_x86_64_prep_args(program, build, f, args, stack_size, local_vars,buffers)?;
                        writeln!(f, "   call {}",func)?;
                        if sp_taken-stack_size > 0 {
                            writeln!(f, "   add {}, {}",program.stack_ptr(),sp_taken-stack_size)?
                        }
                        let reg = Register::RAX.to_byte_size(build.get_contract_of_symbol(func).unwrap().Outputs.get(0).unwrap_or(&VarType::LONG).get_size(program));
                        writeln!(f, "   cmp {}, 0", reg)?;
                        writeln!(f, "   jnz {}",label)?;
                        /*
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
                         */
                    }
                    OfP::GLOBALVAR(_) => {
                        todo!("Cannot have global variables in contract")
                    }
                    OfP::LOCALVAR(v) => {
                        let var = get_local_build(local_vars, v).unwrap();
                        let reg = Register::RAX.to_byte_size(var.typ.get_size(program));
                        if stack_size-var.operand > 0 {
                            writeln!(f, "   mov {}, {} [{}-{}]",reg,size_to_nasm_type(var.typ.get_size(program)),program.stack_ptr(),stack_size-var.operand)?
                        }
                        else {
                            writeln!(f, "   mov {}, {} [{}]",reg,size_to_nasm_type(var.typ.get_size(program)),program.stack_ptr())?
                        }
                        writeln!(f, "   cmp {}, 0",reg)?;
                        writeln!(f, "   jnz {}",label)?;
                    }
                    OfP::REGISTER(v) => {
                        writeln!(f, "   cmp {}, 0",v)?;
                        writeln!(f, "   jnz {}",label)?
                    },
                }
            }
            Self::expr(con) => {
                com_assert!(loc,con.op.is_boolean(), "Error: Expected boolean operation but found something else");
                use Register::*;
                match con.op {
                    Op::EQ => {
                        let res_right = con.right.as_ref().unwrap().result_of_c(program, build, local_vars, buffers,loc).unwrap();
                        let res_left = con.left.as_ref().unwrap().result_of_c(program, build, local_vars, buffers,loc).unwrap();
                        let eval_regs = vec![RAX,RBX,RCX,RDX];
                        if res_right.get_size(program) > res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",RSI,RSI)?;
                        }
                        let left_regs = con.left.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        let mut reg = RSI.to_byte_size(left_regs[0].size());
                        if res_right.get_size(program) < res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",eval_regs[0],eval_regs[0])?;
                        }
                        writeln!(f, "   mov {}, {}",reg,left_regs[0])?;
                        let mut right_regs = con.right.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        if reg.size() > right_regs[0].size() {
                            right_regs[0] = right_regs[0].to_byte_size(reg.size())
                        }
                        if reg.size() < right_regs[0].size() {
                            reg = reg.to_byte_size(right_regs[0].size())
                        }
                        writeln!(f, "   cmp {}, {}",reg,right_regs[0])?;
                        writeln!(f, "   jz {}",label)?;
                        /*
                        let res_right = right.result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let res_left = left.result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        if res_left.get_size(program) > res_right.get_size(program) {
                            writeln!(f,"   xor {}, {}",regs[1],regs[1])?
                        }
                        else if res_right.get_size(program) > res_left.get_size(program) {
                            writeln!(f,"   xor {}, {}",regs[0],regs[0])?
                        }
                        let mut left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size,loc,binst,expr_count+1)?
                        let mut right_oregs = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc)?;
                        if left_oregs[0].size() > right_oregs[0].size() {
                            
                            right_oregs[0] = right_oregs[0].to_byte_size(left_oregs[0].size());
                        }
                        else if left_oregs[0].size() < right_oregs[0].size() {
                            left_oregs[0] = left_oregs[0].to_byte_size(right_oregs[0].size());
                        }
                         */
                    }
                    Op::NEQ => {
                        let res_right = con.right.as_ref().unwrap().result_of_c(program, build, local_vars, buffers,loc).unwrap();
                        let res_left = con.left.as_ref().unwrap().result_of_c(program, build, local_vars,buffers, loc).unwrap();
                        let eval_regs = vec![RAX,RBX,RCX,RDX];
                        if res_right.get_size(program) > res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",RSI,RSI)?;
                        }
                        let left_regs = con.left.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        let mut reg = RSI.to_byte_size(left_regs[0].size());
                        if res_right.get_size(program) < res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",eval_regs[0],eval_regs[0])?;
                        }
                        writeln!(f, "   mov {}, {}",reg,left_regs[0])?;
                        let mut right_regs = con.right.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        if reg.size() > right_regs[0].size() {
                            right_regs[0] = right_regs[0].to_byte_size(reg.size())
                        }
                        if reg.size() < right_regs[0].size() {
                            reg = reg.to_byte_size(right_regs[0].size())
                        }
                        writeln!(f, "   cmp {}, {}",reg,right_regs[0])?;
                        writeln!(f, "   jnz {}",label)?;
                    }
                    Op::GT => {
                        let res_right = con.right.as_ref().unwrap().result_of_c(program, build, local_vars, buffers,loc).unwrap();
                        let res_left = con.left.as_ref().unwrap().result_of_c(program, build, local_vars, buffers,loc).unwrap();
                        let eval_regs = vec![RAX,RBX,RCX,RDX];
                        if res_right.get_size(program) > res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",RSI,RSI)?;
                        }
                        let left_regs = con.left.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        let mut reg = RSI.to_byte_size(left_regs[0].size());
                        if res_right.get_size(program) < res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",eval_regs[0],eval_regs[0])?;
                        }
                        writeln!(f, "   mov {}, {}",reg,left_regs[0])?;
                        let mut right_regs = con.right.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        if reg.size() > right_regs[0].size() {
                            right_regs[0] = right_regs[0].to_byte_size(reg.size())
                        }
                        if reg.size() < right_regs[0].size() {
                            reg = reg.to_byte_size(right_regs[0].size())
                        }
                        writeln!(f, "   cmp {}, {}",reg,right_regs[0])?;
                        writeln!(f, "   jg {}",label)?;
                    }
                    Op::LT => {
                        let res_right = con.right.as_ref().unwrap().result_of_c(program, build, local_vars, buffers,loc).unwrap();
                        let res_left = con.left.as_ref().unwrap().result_of_c(program, build, local_vars, buffers,loc).unwrap();
                        let eval_regs = vec![RAX,RBX,RCX,RDX];
                        if res_right.get_size(program) > res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",RSI,RSI)?;
                        }
                        let left_regs = con.left.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        let mut reg = RSI.to_byte_size(left_regs[0].size());
                        if res_right.get_size(program) < res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",eval_regs[0],eval_regs[0])?;
                        }
                        writeln!(f, "   mov {}, {}",reg,left_regs[0])?;
                        let mut right_regs = con.right.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        if reg.size() > right_regs[0].size() {
                            right_regs[0] = right_regs[0].to_byte_size(reg.size())
                        }
                        if reg.size() < right_regs[0].size() {
                            reg = reg.to_byte_size(right_regs[0].size())
                        }
                        writeln!(f, "   cmp {}, {}",reg,right_regs[0])?;
                        writeln!(f, "   jl {}",label)?;
                    }
                    Op::GTEQ => {
                        let res_right = con.right.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let res_left = con.left.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let eval_regs = vec![RAX,RBX,RCX,RDX];
                        if res_right.get_size(program) > res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",RSI,RSI)?;
                        }
                        let left_regs = con.left.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        let mut reg = RSI.to_byte_size(left_regs[0].size());
                        if res_right.get_size(program) < res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",eval_regs[0],eval_regs[0])?;
                        }
                        writeln!(f, "   mov {}, {}",reg,left_regs[0])?;
                        let mut right_regs = con.right.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        if reg.size() > right_regs[0].size() {
                            right_regs[0] = right_regs[0].to_byte_size(reg.size())
                        }
                        if reg.size() < right_regs[0].size() {
                            reg = reg.to_byte_size(right_regs[0].size())
                        }
                        writeln!(f, "   cmp {}, {}",reg,right_regs[0])?;
                        writeln!(f, "   jge {}",label)?;
                    }
                    Op::LTEQ => {
                        let res_right = con.right.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let res_left = con.left.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let eval_regs = vec![RAX,RBX,RCX,RDX];
                        if res_right.get_size(program) > res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",RSI,RSI)?;
                        }
                        let left_regs = con.left.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        let mut reg = RSI.to_byte_size(left_regs[0].size());
                        if res_right.get_size(program) < res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",eval_regs[0],eval_regs[0])?;
                        }
                        writeln!(f, "   mov {}, {}",reg,left_regs[0])?;
                        let mut right_regs = con.right.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        if reg.size() > right_regs[0].size() {
                            right_regs[0] = right_regs[0].to_byte_size(reg.size())
                        }
                        if reg.size() < right_regs[0].size() {
                            reg = reg.to_byte_size(right_regs[0].size())
                        }
                        writeln!(f, "   cmp {}, {}",reg,right_regs[0])?;
                        writeln!(f, "   jle {}",label)?;
                    }
                    Op::AND => {
                        let left = con.left.as_ref().unwrap();
                        let right = con.right.as_ref().unwrap();
                        let and_end = format!(".AND_CONDITION_{}_{}",binst,expr_count);
                        let and_false = and_end.clone()+"_FALSE";
                        left.jumpifn_nasm_x86_64(&and_false, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+1)?;
                        right.jumpifn_nasm_x86_64(&and_false, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+2)?;
                        writeln!(f, "   jmp {}",label)?;
                        writeln!(f, "{}_FALSE:",and_end)?;
                    }
                    Op::OR => {
                        let left = con.left.as_ref().unwrap();
                        let right = con.right.as_ref().unwrap();
                        left.jumpif_nasm_x86_64(label, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+1)?;
                        right.jumpif_nasm_x86_64(label, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+2)?;
                    }
                    Op::NOT => {
                        let right = con.right.as_ref().unwrap();
                        right.jumpifn_nasm_x86_64(label, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+1)?;
                    }
                    _ => todo!("Unhandled")
                }
            },
        }
        Ok(())
    }
    pub fn jumpifn_nasm_x86_64(&self, label: &String, f: &mut File, program: &CmdProgram, build: &BuildProgram, loc: &ProgramLocation, stack_size: usize, local_vars: &Vec<HashMap<String, LocalVariable>>,buffers: &Vec<BuildBuf>,binst: usize, expr_count: usize) -> io::Result<()>{
        match self {
            Self::val(v) => {
                match v {
                    OfP::BUFFER(_) => com_error!(loc,"Cannot have buffers in conditions"),
                    OfP::CONST(cv) => {
                        match cv {
                            RawConstValueType::CHAR(c) => {
                                if *c == 0{
                                    writeln!(f,"   jmp {}",label)?;
                                }
                            }
                            RawConstValueType::SHORT(c) => {
                                if *c == 0 {
                                    writeln!(f,"   jmp {}",label)?
                                }
                            }
                            RawConstValueType::INT(c) => {
                                if *c == 0{
                                    writeln!(f,"   jmp {}",label)?;
                                }
                            }
                            RawConstValueType::LONG(c) => {
                                if *c == 0 {
                                    writeln!(f,"   jmp {}",label)?;
                                }
                            }
                            RawConstValueType::PTR(_, c) => {
                                if *c == 0 {
                                    writeln!(f,"   jmp {}",label)?
                                }
                            }
                            RawConstValueType::STR(_) => {
                                com_error!(loc, "Error: Cannot have Strings inside conditions")
                            }
                        }
                    }
                    OfP::RESULT(func, args) => {
                        let sp_taken = nasm_x86_64_prep_args(program, build, f, args, stack_size, local_vars,buffers)?;
                        writeln!(f, "   call {}",func)?;
                        if sp_taken-stack_size > 0 {
                            writeln!(f, "   add {}, {}",program.stack_ptr(),sp_taken-stack_size)?
                        }
                        let reg = Register::RAX.to_byte_size(build.get_contract_of_symbol(func).unwrap().Outputs.get(0).unwrap_or(&VarType::LONG).get_size(program));
                        writeln!(f, "   cmp {}, 0", reg)?;
                        writeln!(f, "   jz {}",label)?;
                    }
                    OfP::GLOBALVAR(_) => {
                        todo!("Cannot have global variables in contract")
                    }
                    OfP::LOCALVAR(v) => {
                        let var = get_local_build(local_vars, v).unwrap();
                        let reg = Register::RAX.to_byte_size(var.typ.get_size(program));
                        if stack_size-var.operand > 0 {
                            writeln!(f, "   mov {}, {} [{}-{}]",reg,size_to_nasm_type(var.typ.get_size(program)),program.stack_ptr(),stack_size-var.operand)?
                        }
                        else {
                            writeln!(f, "   mov {}, {} [{}]",reg,size_to_nasm_type(var.typ.get_size(program)),program.stack_ptr())?
                        }
                        writeln!(f, "   cmp {}, 0",reg)?;
                        writeln!(f, "   jz {}",label)?;
                    }
                    OfP::REGISTER(v) => {
                        writeln!(f, "   cmp {}, 0",v)?;
                        writeln!(f, "   jz {}",label)?
                    },
                }
            }
            Self::expr(con) => {
                com_assert!(loc,con.op.is_boolean(), "Error: Expected boolean operation but found something else");
                use Register::*;
                match con.op {
                    Op::EQ => {
                        let res_right = con.right.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let res_left = con.left.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let eval_regs = vec![RAX,RBX,RCX,RDX];
                        if res_right.get_size(program) > res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",RSI,RSI)?;
                        }
                        let left_regs = con.left.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        let mut reg = RSI.to_byte_size(left_regs[0].size());
                        if res_right.get_size(program) < res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",eval_regs[0],eval_regs[0])?;
                        }
                        writeln!(f, "   mov {}, {}",reg,left_regs[0])?;
                        let mut right_regs = con.right.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        if reg.size() > right_regs[0].size() {
                            right_regs[0] = right_regs[0].to_byte_size(reg.size())
                        }
                        if reg.size() < right_regs[0].size() {
                            reg = reg.to_byte_size(right_regs[0].size())
                        }
                        writeln!(f, "   cmp {}, {}",reg,right_regs[0])?;
                        writeln!(f, "   jnz {}",label)?;
                    }
                    Op::NEQ => {
                        let res_right = con.right.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let res_left = con.left.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let eval_regs = vec![RAX,RBX,RCX,RDX];
                        if res_right.get_size(program) > res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",RSI,RSI)?;
                        }
                        let left_regs = con.left.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        let mut reg = RSI.to_byte_size(left_regs[0].size());
                        if res_right.get_size(program) < res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",eval_regs[0],eval_regs[0])?;
                        }
                        writeln!(f, "   mov {}, {}",reg,left_regs[0])?;
                        let mut right_regs = con.right.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        if reg.size() > right_regs[0].size() {
                            right_regs[0] = right_regs[0].to_byte_size(reg.size())
                        }
                        if reg.size() < right_regs[0].size() {
                            reg = reg.to_byte_size(right_regs[0].size())
                        }
                        writeln!(f, "   cmp {}, {}",reg,right_regs[0])?;
                        writeln!(f, "   jz {}",label)?;
                    }
                    Op::GT => {
                        let res_right = con.right.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let res_left = con.left.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let eval_regs = vec![RAX,RBX,RCX,RDX];
                        if res_right.get_size(program) > res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",RSI,RSI)?;
                        }
                        let left_regs = con.left.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        let mut reg = RSI.to_byte_size(left_regs[0].size());
                        if res_right.get_size(program) < res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",eval_regs[0],eval_regs[0])?;
                        }
                        writeln!(f, "   mov {}, {}",reg,left_regs[0])?;
                        let mut right_regs = con.right.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        if reg.size() > right_regs[0].size() {
                            right_regs[0] = right_regs[0].to_byte_size(reg.size())
                        }
                        if reg.size() < right_regs[0].size() {
                            reg = reg.to_byte_size(right_regs[0].size())
                        }
                        writeln!(f, "   cmp {}, {}",reg,right_regs[0])?;
                        writeln!(f, "   jle {}",label)?;
                    }
                    Op::LT => {
                        let res_right = con.right.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let res_left = con.left.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let eval_regs = vec![RAX,RBX,RCX,RDX];
                        if res_right.get_size(program) > res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",RSI,RSI)?;
                        }
                        let left_regs = con.left.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        let mut reg = RSI.to_byte_size(left_regs[0].size());
                        if res_right.get_size(program) < res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",eval_regs[0],eval_regs[0])?;
                        }
                        writeln!(f, "   mov {}, {}",reg,left_regs[0])?;
                        let mut right_regs = con.right.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        if reg.size() > right_regs[0].size() {
                            right_regs[0] = right_regs[0].to_byte_size(reg.size())
                        }
                        if reg.size() < right_regs[0].size() {
                            reg = reg.to_byte_size(right_regs[0].size())
                        }
                        writeln!(f, "   cmp {}, {}",reg,right_regs[0])?;
                        writeln!(f, "   jge {}",label)?;
                    }
                    Op::GTEQ => {
                        let res_right = con.right.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let res_left = con.left.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let eval_regs = vec![RAX,RBX,RCX,RDX];
                        if res_right.get_size(program) > res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",RSI,RSI)?;
                        }
                        let left_regs = con.left.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        let mut reg = RSI.to_byte_size(left_regs[0].size());
                        if res_right.get_size(program) < res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",eval_regs[0],eval_regs[0])?;
                        }
                        writeln!(f, "   mov {}, {}",reg,left_regs[0])?;
                        let mut right_regs = con.right.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        if reg.size() > right_regs[0].size() {
                            right_regs[0] = right_regs[0].to_byte_size(reg.size())
                        }
                        if reg.size() < right_regs[0].size() {
                            reg = reg.to_byte_size(right_regs[0].size())
                        }
                        writeln!(f, "   cmp {}, {}",reg,right_regs[0])?;
                        writeln!(f, "   jl {}",label)?;
                    }
                    Op::LTEQ => {
                        let res_right = con.right.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let res_left = con.left.as_ref().unwrap().result_of_c(program, build, local_vars, buffers, loc).unwrap();
                        let eval_regs = vec![RAX,RBX,RCX,RDX];
                        if res_right.get_size(program) > res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",RSI,RSI)?;
                        }
                        let left_regs = con.left.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        let mut reg = RSI.to_byte_size(left_regs[0].size());
                        if res_right.get_size(program) < res_left.get_size(program) {
                            writeln!(f, "   xor {}, {}",eval_regs[0],eval_regs[0])?;
                        }
                        writeln!(f, "   mov {}, {}",reg,left_regs[0])?;
                        let mut right_regs = con.right.as_ref().unwrap().LEIRnasm(eval_regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        if reg.size() > right_regs[0].size() {
                            right_regs[0] = right_regs[0].to_byte_size(reg.size())
                        }
                        if reg.size() < right_regs[0].size() {
                            reg = reg.to_byte_size(right_regs[0].size())
                        }
                        writeln!(f, "   cmp {}, {}",reg,right_regs[0])?;
                        writeln!(f, "   jg {}",label)?;
                    }
                    Op::AND => {
                        let left = con.left.as_ref().unwrap();
                        let right = con.right.as_ref().unwrap();
                        let and_end = format!(".AND_CONDITION_{}_{}",binst,expr_count);
                        let and_false = and_end.clone()+"_TRUE";
                        left.jumpif_nasm_x86_64(&and_false, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+1)?;
                        right.jumpif_nasm_x86_64(&and_false, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+2)?;
                        writeln!(f, "   jmp {}",label)?;
                        writeln!(f, "{}_TRUE:",and_end)?;   
                    }
                    Op::OR => {
                        let left = con.left.as_ref().unwrap();
                        let right = con.right.as_ref().unwrap();
                        left.jumpifn_nasm_x86_64(label, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+1)?;
                        right.jumpifn_nasm_x86_64(label, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+2)?;
                    }
                    Op::NOT => {
                        let right = con.right.as_ref().unwrap();
                        right.jumpif_nasm_x86_64(label, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+1)?;
                    }
                    _ => todo!("Unhandled")
                }
            },
        }
        Ok(())
    }
    //pub fn jump(&self, label: String) -> 
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
impl Expression {
    pub fn LEIRnasm(&self,regs: Vec<Register>,f: &mut File, program: &CmdProgram,build: &BuildProgram,local_vars: &Vec<HashMap<String, LocalVariable>>, buffers: &Vec<BuildBuf>,stack_size: usize,loc: &ProgramLocation, binst: usize, expr_count: usize) -> std::io::Result<Vec<Register>>{
        match self {
            Expression::val(v) => {
               v.LOIRGNasm(regs, f, program, build, local_vars,buffers, stack_size, loc)
            },
            Expression::expr(e) => {
                e.eval_nasm(regs,f,program,build,local_vars,buffers,stack_size,loc,binst, expr_count)
            },
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
    pub fn eval_nasm(&self,regs: Vec<Register>,f: &mut File,program: &CmdProgram,build: &BuildProgram,local_vars: &Vec<HashMap<String, LocalVariable>>, buffers: &Vec<BuildBuf>,stack_size: usize,loc: &ProgramLocation,binst: usize,expr_count: usize) -> std::io::Result<Vec<Register>>{
        #[allow(unused_assignments)]
        let mut o: Vec<Register> = Vec::new();
        match self.op {
            Op::NONE  => com_error!(loc,"Error: Cannot evaluate NONE operation!"),
            Op::PLUS  => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::PLUS.to_string());
                let right       = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::PLUS.to_string());
                
                //let res_right = right.result_of_c(program, build, local_vars, buffers, loc).unwrap();
                //let res_left = left.result_of_c(program, build, local_vars, buffers, loc).unwrap();

                //if res_right.get_size(program) > res_left.get_size(program) {
                //    writeln!(f, "   xor {}, {}",regs[1],regs[1])?;
                //}
                //else if res_right.get_size(program) < res_left.get_size(program) {
                //    writeln!(f, "   xor {}, {}",regs[0],regs[0])?;
                //}

                match left {
                    Expression::expr(left_expr) => {
                        match right {
                            Expression::expr(right_expr) => {
                                let res_right = right.result_of_c(program, build, local_vars, buffers,loc).unwrap();
                                let res_left = left.result_of_c(program, build, local_vars, buffers,loc).unwrap();
                                if res_left.get_size(program) > res_right.get_size(program) {
                                    writeln!(f,"   xor {}, {}",regs[1],regs[1])?
                                }
                                else if res_right.get_size(program) > res_left.get_size(program) {
                                    writeln!(f,"   xor {}, {}",regs[0],regs[0])?
                                }
                                let mut left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count+1)?;
                                let mut right_oregs = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count+1)?;

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
                                        let left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count+1)?;
                                        writeln!(f,"   add {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let res_right = right.result_of_c(program, build, local_vars, buffers,loc).unwrap();
                                        let res_left = left.result_of_c(program, build, local_vars, buffers,loc).unwrap();
                                        if res_left.get_size(program) > res_right.get_size(program) {
                                            writeln!(f,"   xor {}, {}",regs[1],regs[1])?
                                        }
                                        else if res_right.get_size(program) > res_left.get_size(program) {
                                            writeln!(f,"   xor {}, {}",regs[0],regs[0])?
                                        }
                                        let mut left_oregs = left_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count+1)?;
                                        let mut right = right_val.LOIRGNasm(vec![regs[1]], f, program, build, local_vars, buffers, stack_size,loc)?;
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
                                        let right_oregs = right_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count+1)?;
                                        writeln!(f,"   add {}, {}",right_oregs[0],v.get_num_data())?;
                                        o = right_oregs;
                                    }

                                    _ => {
                                        let res_right = right.result_of_c(program, build, local_vars, buffers,loc).unwrap();
                                        let res_left = left.result_of_c(program, build, local_vars, buffers,loc).unwrap();
                                        if res_left.get_size(program) > res_right.get_size(program) {
                                            writeln!(f,"   xor {}, {}",regs[0],regs[0])?
                                        }
                                        else if res_right.get_size(program) > res_left.get_size(program) {
                                            writeln!(f,"   xor {}, {}",regs[1],regs[1])?
                                        }
                                        let mut right_oregs = right_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count+1)?;
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
                                        let res_right = right.result_of_c(program, build, local_vars, buffers,loc).unwrap();
                                        let res_left = left.result_of_c(program, build, local_vars, buffers,loc).unwrap();
                                        if res_left.get_size(program) > res_right.get_size(program) {
                                            writeln!(f,"   xor {}, {}",regs[1],regs[1])?
                                        }
                                        else if res_right.get_size(program) > res_left.get_size(program) {
                                            writeln!(f,"   xor {}, {}",regs[0],regs[0])?
                                        }
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
                
                let res_right = right.result_of_c(program, build, local_vars, buffers,loc).unwrap();
                let res_left = left.result_of_c(program, build, local_vars, buffers,loc).unwrap();

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
                                let mut left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count+1)?;
                                let mut right_oregs = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count+1)?;
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
                                        let left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count+1)?;
                                        writeln!(f,"   sub {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count+1)?;
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
                                        let right_oregs = right_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size,loc,binst,expr_count+1)?;
                                        writeln!(f,"   sub {}, {}",right_oregs[0],v.get_num_data())?;
                                        o = right_oregs;
                                    }

                                    _ => {
                                        let mut right_oregs = right_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
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
                        let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                        let rightregs1 = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                        if leftregs1[0].to_byte_size(8) != Register::RAX {
                            writeln!(f, "   mov {}, {}",Register::RAX.to_byte_size(leftregs1[0].size()),leftregs1[0])?;
                        }
                        writeln!(f,"   cqo")?;
                        writeln!(f, "   idiv {}",rightregs1[0])?;
                        o = vec![Register::RAX.to_byte_size(leftregs1[0].size())];
                    }
                    Expression::val(right_val) => {
                        let left_oregs = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
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
                        let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                        let rightregs1 = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                        com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                        if leftregs1[0].to_byte_size(8) != Register::RDX {
                            writeln!(f, "   mov {}, {}",Register::RDX.to_byte_size(leftregs1[0].size()),leftregs1[0])?;
                        }
                        writeln!(f,"   cqo")?;
                        writeln!(f, "   idiv {}",rightregs1[0])?;
                        o = vec![Register::RDX.to_byte_size(leftregs1[0].size())];
                    }
                    Expression::val(right_val) => {
                        let left_oregs = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
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
                            let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                            com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                            let rightregs1 = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                            com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                            if leftregs1[0].to_byte_size(8) != Register::RAX {
                                writeln!(f, "   mov {}, {}",Register::RAX.to_byte_size(leftregs1[0].size()),leftregs1[0])?;
                            }
                            writeln!(f,"   cqo")?;
                            writeln!(f, "   imul {}",rightregs1[0])?;
                            o = vec![Register::RAX.to_byte_size(leftregs1[0].size())];
                        }
                        Expression::val(right_val) => {
                            let left_oregs = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
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
                    let rightregs1 = right.LEIRnasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                    com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                    let rego = &rightregs1[0];
                    let resof = right.result_of_c(program, build, local_vars,buffers,loc).unwrap();
                    com_assert!(loc, resof.is_some_ptr(), "Error: Cannot dereference void pointer!");
                    let resof = resof.get_ptr_val().unwrap();
                    writeln!(f, "   mov {}, {} [{}]",rego.to_byte_size(resof.get_size(program)).to_string(),size_to_nasm_type(resof.get_size(program)),rego.to_string())?;
                    o = vec![rego.to_byte_size(resof.get_size(program))];
                }
            },
            Op::EQ    => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::EQ   .to_string());
                let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::EQ   .to_string());
                let rightregs1 = right.LEIRnasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                writeln!(f, "   cmp {}, {}",leftregs1[0].to_string(),rightregs1[0].to_string())?;
                writeln!(f, "   sete {}",leftregs1[0].to_byte_size(1).to_string())?;
                o = vec![leftregs1[0].to_byte_size(1)];
            },
            Op::NEQ   => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::NEQ  .to_string());
                let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::NEQ  .to_string());
                let rightregs1 = right.LEIRnasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                writeln!(f, "   cmp {}, {}",leftregs1[0].to_string(),rightregs1[0].to_string())?;
                writeln!(f, "   setne {}",leftregs1[0].to_byte_size(1).to_string())?;
                o = vec![leftregs1[0].to_byte_size(1)];
            },
            Op::GT    => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::GT   .to_string());
                let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::GT   .to_string());
                let rightregs1 = right.LEIRnasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                writeln!(f, "   cmp {}, {}",leftregs1[0].to_string(),rightregs1[0].to_string())?;
                writeln!(f, "   setg {}",leftregs1[0].to_byte_size(1).to_string())?;
                o = vec![leftregs1[0].to_byte_size(1)];

            },
            Op::GTEQ  => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::GTEQ .to_string());
                let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::GTEQ .to_string());
                let rightregs1 = right.LEIRnasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                writeln!(f, "   cmp {}, {}",leftregs1[0].to_string(),rightregs1[0].to_string())?;
                writeln!(f, "   setge {}",leftregs1[0].to_byte_size(1).to_string())?;
                o = vec![leftregs1[0].to_byte_size(1)];

            },
            Op::LT    => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::LT   .to_string());
                let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::LT   .to_string());
                let rightregs1 = right.LEIRnasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                writeln!(f, "   cmp {}, {}",leftregs1[0].to_string(),rightregs1[0].to_string())?;
                writeln!(f, "   setl {}",leftregs1[0].to_byte_size(1).to_string())?;
                o = vec![leftregs1[0].to_byte_size(1)];

            },
            Op::LTEQ  => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::LTEQ .to_string());
                let leftregs1 = left.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                com_assert!(loc,leftregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                let right = com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::LTEQ .to_string());
                let rightregs1 = right.LEIRnasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
                com_assert!(loc,rightregs1.len() == 1, "TODO: Handle multi-parameter loading for expressions!");
                writeln!(f, "   cmp {}, {}",leftregs1[0].to_string(),rightregs1[0].to_string())?;
                writeln!(f, "   setle {}",leftregs1[0].to_byte_size(1).to_string())?;
                o = vec![leftregs1[0].to_byte_size(1)];

            },
            Op::NOT   => {
                let right = self.right.as_ref().unwrap();
                let res = right.LEIRnasm(regs.clone(), f, program, build, local_vars, buffers, stack_size, loc, binst, expr_count)?;
                writeln!(f, "  cmp {}, 0",res[0])?;
                writeln!(f, "  sete {}",regs[0].to_byte_size(1))?;
                o = vec![regs[0].to_byte_size(1)]
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
                
                let res_right = right.result_of_c(program, build, local_vars, buffers, loc).unwrap();
                let res_left = left.result_of_c(program, build, local_vars, buffers, loc).unwrap();

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
                                let mut left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size,loc,binst,expr_count+1)?;
                                let mut right_oregs = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
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
                                        let left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size,loc,binst,expr_count+1)?;
                                        writeln!(f,"   shr {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
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
                                        let right_oregs = right_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size,loc,binst,expr_count+1)?;
                                        writeln!(f,"   shr {}, {}",right_oregs[0],v.get_num_data())?;
                                        o = right_oregs;
                                    }

                                    _ => {
                                        let mut right_oregs = right_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
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
                
                let res_right = right.result_of_c(program, build, local_vars, buffers, loc).unwrap();
                let res_left = left.result_of_c(program, build, local_vars, buffers, loc).unwrap();

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
                                let mut left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size,loc,binst,expr_count+1)?;
                                let mut right_oregs = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
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
                                        let left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size,loc,binst,expr_count+1)?;
                                        writeln!(f,"   shl {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
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
                                        let right_oregs = right_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size,loc,binst,expr_count+1)?;
                                        writeln!(f,"   shl {}, {}",right_oregs[0],v.get_num_data())?;
                                        o = right_oregs;
                                    }

                                    _ => {
                                        let mut right_oregs = right_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
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
                
                let res_right = right.result_of_c(program, build, local_vars, buffers, loc).unwrap();
                let res_left = left.result_of_c(program, build, local_vars, buffers, loc).unwrap();

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
                                let mut left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size,loc,binst,expr_count+1)?;
                                let mut right_oregs = right_expr.eval_nasm(regs[1..].to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
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
                                        let left_oregs = left_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size,loc,binst,expr_count+1)?;
                                        writeln!(f,"   or {}, {}",left_oregs[0],v.get_num_data())?;
                                        o = left_oregs;
                                    }
                                    _ => {
                                        let mut left_oregs = left_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
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
                                        let right_oregs = right_expr.eval_nasm(regs.clone(), f, program, build, local_vars, buffers, stack_size,loc,binst,expr_count+1)?;
                                        writeln!(f,"   or {}, {}",right_oregs[0],v.get_num_data())?;
                                        o = right_oregs;
                                    }

                                    _ => {
                                        let mut right_oregs = right_expr.eval_nasm(regs.to_vec(), f, program, build, local_vars, buffers, stack_size, loc,binst,expr_count)?;
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
            Op::AND => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::OR.to_string());
                let right= com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::OR.to_string());
                let suc_label = format!(".AND_UNSUCCESS_EXPR_{}_{}",binst,expr_count);
                left.jumpifn_nasm_x86_64(&suc_label, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+1)?;
                right.jumpifn_nasm_x86_64(&suc_label, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+2)?;
                writeln!(f, "   mov {}, 1",regs[0].to_byte_size(1))?;
                writeln!(f, "   jmp {}_END",suc_label)?;
                writeln!(f, "   {}:",suc_label)?;
                writeln!(f, "   mov {}, 0",regs[0].to_byte_size(1))?;
                writeln!(f, "   jmp {}_END",suc_label)?;
                writeln!(f, "   {}_END:",suc_label)?;
                o = vec![regs[0].to_byte_size(1)]                
            },
            Op::OR  => {
                let left = com_expect!(loc,self.left.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::OR.to_string());
                let right= com_expect!(loc,self.right.as_ref(),"Error: Cannot evaluate Op '{}' without left parameter",Op::OR.to_string());
                let suc_label = format!(".OR_SUCCESS_EXPR_{}_{}",binst,expr_count);
                left.jumpif_nasm_x86_64(&suc_label, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+1)?;
                right.jumpif_nasm_x86_64(&suc_label, f, program, build, loc, stack_size, local_vars, buffers, binst, expr_count+2)?;
                writeln!(f, "   mov {}, 0",regs[0].to_byte_size(1))?;
                writeln!(f, "   jmp {}_END",suc_label)?;
                writeln!(f, "   {}:",suc_label)?;
                writeln!(f, "   mov {}, 1",regs[0].to_byte_size(1))?;
                writeln!(f, "   jmp {}_END",suc_label)?;
                writeln!(f, "   {}_END:",suc_label)?;
                o = vec![regs[0].to_byte_size(1)]                
            },

        }
        Ok(o)
        //todo!("ERROR: this should stop here")
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

