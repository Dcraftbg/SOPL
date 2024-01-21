use core::fmt;
use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::module::Linkage;
use inkwell::targets::{Target, TargetMachine, TargetTriple};
use inkwell::types::{FunctionType, BasicType, AnyType, AnyTypeEnum};
use inkwell::values::{BasicValue, IntValue, AnyValueEnum, IntMathValue, AnyValue, PointerValue, BasicValueEnum, ArrayValue, GlobalValue};
use inkwell::{context::Context, module::Module};
use inkwell::builder::{Builder, BuilderError};

use crate::{CmdProgram, BuildProgram, io, Function, FunctionContract, VarType, PtrTyp, Instruction, Expression, OfP, RawConstValue, RawConstValueType, AnyContract, ProgramStringType, ProgramString};
#[derive(Debug)]
struct LocalLLVM<'ctx> {
   ptr: PointerValue<'ctx>,
   var: VarType 
}
type LocalsLLVM<'ctx> = HashMap<String, LocalLLVM<'ctx>>;
#[derive(Debug)]
enum BuildEnvError {
    Builder(BuilderError),
    AnyValueEnumConvertion,
    FunctionNotFound,
    ParameterNotFound,
}
impl fmt::Display for BuildEnvError {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
       // TODO: Also accept the location for slightly more information
       match self {
           Self::Builder(b) => {
               writeln!(f, "Builder Error: {}",b)?
           },
           Self::AnyValueEnumConvertion => {
               writeln!(f, "Could not convert Any value to Basic value")?
           },
           Self::FunctionNotFound => {
               writeln!(f, "Could not find function")?
           }
           Self::ParameterNotFound => {
               writeln!(f, "Could not find parameter")?
           }
       }
       Ok(())
   } 
}
impl From<BuilderError> for BuildEnvError {
    fn from(err: BuilderError) -> Self {
        BuildEnvError::Builder(err)
    }
}
type BuildEnvResult<T> = Result<T, BuildEnvError>;
// TODO: use &ProgramString instead of full copy
struct BuildEnv<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    locals: LocalsLLVM<'ctx>,
    strings: Vec<ProgramString>,
    stringoff: usize,
}
impl <'ctx> BuildEnv<'ctx> {
    fn var_type_to_type<'a>(&'a self, typ: &VarType) -> inkwell::types::BasicTypeEnum<'ctx> {
        match typ {
            VarType::INT => self.context.i32_type().as_basic_type_enum(),
            VarType::LONG => self.context.i64_type().as_basic_type_enum(),
            VarType::CHAR => self.context.i8_type().as_basic_type_enum(),
            VarType::BOOLEAN => self.context.i16_type().as_basic_type_enum(),
            VarType::SHORT => self.context.i16_type().as_basic_type_enum(),
            VarType::PTR(p) => {
               match p.typ {
                   PtrTyp::VOID => {
                       let void = self.context.i8_type();
                       let mut typ = void.as_basic_type_enum();
                       for _ in 0..p.inner_ref+1 {
                          typ = typ.ptr_type(AddressSpace::default()).as_basic_type_enum()
                       }
                       typ.as_basic_type_enum()
                   }
                   PtrTyp::TYP(ref typ) => {
                       let mut typ = self.var_type_to_type(typ).as_basic_type_enum();
                       for _ in 0..p.inner_ref+1 {
                          typ = typ.ptr_type(AddressSpace::default()).as_basic_type_enum()
                       }
                       typ.as_basic_type_enum()
                   }
                   _ => todo!("PtrType: {:?}", p.typ)
               }
            }
            _ => todo!("VarType: {:?}", typ)
        }
    }
    fn build_contract<'a>(&'a self, c: &FunctionContract) -> FunctionType<'ctx>
    {
        let mut param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = Vec::with_capacity(c.Inputs.len());
        for (_,input) in c.Inputs.iter() {
            param_types.push(self.var_type_to_type(input).into());
        } 
        match c.Output.as_ref() {
           Some(v) => {
               self.var_type_to_type(v).fn_type(&param_types, false)
           } 
           None => self.context.void_type().fn_type(&param_types, false)
        }
    }
    fn get_str<'a>(&'a self, id: usize) -> &ProgramString {
        return self.strings.get(id-self.stringoff).expect("Invalid id")
    }
    fn build_expr<'a>(&'a self, e: &Expression) -> BuildEnvResult<AnyValueEnum<'ctx>>{
        match e {
            Expression::val(v) => {
                match v {
                    OfP::CONST(val) => {
                        match val {
                            RawConstValueType::INT(v) => {
                                let i32_t = self.context.i32_type();
                                let val = *v;
                                let vali64 = val as i64;
                                let valu64 = unsafe { *((&vali64 as *const i64) as *const u64) };
                                Ok(i32_t.const_int(valu64, true).into())
                            }
                            RawConstValueType::STR(s) => {
                                let id = *s;
                                let stri = self.get_str(id);
                                match stri.Typ {
                                    ProgramStringType::STR => todo!("ProgramStringType::STR is not yet done for OfP"),
                                    ProgramStringType::CSTR => 
                                        Ok(
                                        self.builder.build_global_string_ptr(&stri.Data, "str")?
                                          .as_pointer_value()
                                          .into()
                                        ),
                                }
                            }
                            _ => todo!("TODO: Other val const value: {:?}",val)
                        }
                    }
                    OfP::LOCALVAR(name) => {
                        println!("Local var: {}",name);
                        let a = self.locals.get(name).expect("[ERROR] Finding local. Typechecking probably failed");
                        let var = self.var_type_to_type(&a.var);
                        Ok(self.builder.build_load(var, a.ptr.clone(), "load")?
                            .into())
                    }
                    OfP::RESULT(name, args) => {
                        let func = self.module.get_function(name).expect("This should not fail");
                        let mut fargs = Vec::with_capacity(args.len());
                        for arg in args {
                            let expr = Expression::val(arg.clone());
                            let val = self.build_expr(&expr)?;
                            fargs.push(Self::any_value_into_basic_value(&val).expect("This should work").into());
                        }
                        Ok(self.builder.build_call(func, fargs.as_slice(), "funccall")?.as_any_value_enum())
                    }
                    _ => todo!("TODO: Other OfP for build_expr: {:?}",v)
                }
            }
            Expression::expr(expr) => {
                match expr.op {
                    crate::Op::PLUS => {
                        let left  = self.build_expr(expr.left.as_ref().unwrap())?;
                        let right = self.build_expr(expr.right.as_ref().unwrap())?;
                        match left {
                            AnyValueEnum::IntValue(left) => {
                                assert!(right.is_int_value() || right.is_float_value(), "Type checking failed and trying to add integer to other type");
                                let right = right.into_int_value();
                                Ok(self.builder.build_int_add(left, right, "add")?.into())
                            }
                            _ => todo!("Unsupported value for addition: {:?}",left)
                        }
                    }
                    _ => todo!("TODO Other ops in build_expr {:?}",expr.op)
                }
            }
            _ => todo!("TODO: Expression in build_expr")
        }
    }
    fn build_any_contract<'a>(&'a self, c: &AnyContract) -> FunctionType<'ctx>
    {
        let mut param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = Vec::with_capacity(c.InputPool.body.len());
        for input in c.InputPool.body.iter() {
            param_types.push(self.var_type_to_type(input).into());
        } 
        match c.Output.as_ref() {
           Some(v) => {
               self.var_type_to_type(v).fn_type(&param_types, false)
           } 
           None => self.context.void_type().fn_type(&param_types, false)
        }
    }
    fn any_value_into_basic_value(val: &AnyValueEnum<'ctx>) -> BuildEnvResult<BasicValueEnum<'ctx>> {
        match val {
            AnyValueEnum::IntValue(v)     => Ok(v.clone().into()),
            AnyValueEnum::FloatValue(v)   => Ok(v.clone().into()),
            AnyValueEnum::PointerValue(v) => Ok(v.clone().into()),
            AnyValueEnum::ArrayValue(v)   => Ok(v.clone().into()),
            _ => Err(BuildEnvError::AnyValueEnumConvertion)
        }
    }
    fn build(&mut self, _p: &CmdProgram, b: &BuildProgram) -> BuildEnvResult<()>
    {
        self.stringoff = b.stringoffset;
        assert!(b.dll_imports.len() == 0, "TODO: DLL-imports in llvm native");
        assert!(b.dll_exports.len() == 0, "TODO: DLL-exports in llvm native");
        assert!(b.global_vars.len() == 0, "TODO: Global vars");
        assert!(b.buffers.len() == 0, "TODO: Buffers");
        self.strings = b.stringdefs.clone();
        for (name, external) in b.externals.iter() {
            assert!(!external.contract.InputPool.is_dynamic, "TODO: Dynamic externals are not yet done");
            let ty = self.build_any_contract(&external.contract);
            self.module.add_function(name, ty, Some(Linkage::External));
        }
        for (fname, func) in b.functions.iter() {
            let ty = self.build_contract(&func.contract);
            let f = self.module.add_function(&fname, ty, None);
            let entry = self.context.append_basic_block(f, "entry");
            self.builder.position_at_end(entry);
            let mut locals: LocalsLLVM = LocalsLLVM::with_capacity(func.locals.len());
            for (i, (name, local)) in func.locals.iter().enumerate() {
                let ty = self.var_type_to_type(local);
                let ptr = self.builder.build_alloca(ty, name)?;
                locals.insert(name.clone(), LocalLLVM {var: local.clone(), ptr});
                let v = f.get_nth_param(i as u32).ok_or(BuildEnvError::ParameterNotFound)?;
                self.builder.build_store(ptr, v)?;
            }
            self.locals = locals;
            assert!(func.buffers.len() == 0, "TODO: Buffers");
            for (_,inst) in func.body.iter() {
                match inst {
                    // TODO: remove this
                    Instruction::FNBEGIN() => {}
                    Instruction::RET(exp) => {
                        let ret = self.build_expr(exp)?;
                        let r = match ret {
                            AnyValueEnum::IntValue(v) => v.as_basic_value_enum(),
                            _ => todo!("Unsupported return value {:?}",ret)
                        };
                        self.builder.build_return(Some(&r))?;                  
                    }
                    Instruction::CALLRAW(name, args) => {
                        let func = self.module.get_function(name).ok_or(BuildEnvError::FunctionNotFound)?;
                        let mut fargs = Vec::with_capacity(args.len());
                        for arg in args {
                            let expr = Expression::val(arg.clone());
                            let val = self.build_expr(&expr)?;
                            fargs.push(Self::any_value_into_basic_value(&val)?.into());
                        }
                        self.builder.build_call(func, fargs.as_slice(), "call")?;
                    }
                    _ => todo!("TODO: Instruction: {:?}", inst)
                }
            }
        }
        Ok(())
    }
}

pub fn build_llvm_native(p: &CmdProgram, b: &BuildProgram, path: &str) -> io::Result<()> {
    let context = Context::create();
    let module = context.create_module(&p.path);
    let builder=  context.create_builder();

    Target::initialize_native(&Default::default()).map_err(|err| {
        io::Error::new(io::ErrorKind::Other, err.to_string())
    })?;
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).map_err(|err| {
        io::Error::new(io::ErrorKind::Other, err.to_string())
    })?;
    // TODO: Allow configuration for this:
    let machine = target.create_target_machine(
        &triple,
        "",
        "",
        inkwell::OptimizationLevel::Default,
        inkwell::targets::RelocMode::Default,
        inkwell::targets::CodeModel::Default
    ).ok_or(
        io::Error::new(io::ErrorKind::Other, "Could not create machine target!")
    )?;
    let mut env = BuildEnv {context: &context, builder, module, locals: LocalsLLVM::default(), strings: Vec::new(), stringoff: 0};
    if let Err(err) = env.build(p, b) {
        eprintln!("[ERROR] Could not build environment: {}",err);
        return Err(io::Error::new(io::ErrorKind::Other, "Could not build environment"));
    }
    machine.write_to_file(
        &env.module,
        inkwell::targets::FileType::Object,
        &std::path::Path::new(path)
    ).map_err(|err| {
        io::Error::new(io::ErrorKind::Other, err.to_string())
    })?;
    Ok(())
}
