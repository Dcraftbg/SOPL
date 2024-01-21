use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::targets::{Target, TargetMachine, TargetTriple};
use inkwell::types::{FunctionType, BasicType, AnyType, AnyTypeEnum};
use inkwell::values::{BasicValue, IntValue, AnyValueEnum, IntMathValue, AnyValue, PointerValue};
use inkwell::{context::Context, module::Module};
use inkwell::builder::Builder;

use crate::{CmdProgram, BuildProgram, io, Function, FunctionContract, VarType, PtrTyp, Instruction, Expression, OfP, RawConstValue, RawConstValueType};
#[derive(Debug)]
struct LocalLLVM<'ctx> {
   ptr: PointerValue<'ctx>,
   var: VarType 
}
type LocalsLLVM<'ctx> = HashMap<String, LocalLLVM<'ctx>>;
struct BuildEnv<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    locals: LocalsLLVM<'ctx>
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
                       void.ptr_type(AddressSpace::default()).as_basic_type_enum()
                   }
                   _ => todo!()
               }
            }
            _ => todo!()
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
    fn build_expr<'a>(&'a self, e: &Expression) -> AnyValueEnum<'ctx> {
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
                                i32_t.const_int(valu64, true).into()
                            }
                            _ => todo!("TODO: Other val const value: {:?}",val)
                        }
                    }
                    OfP::LOCALVAR(name) => {
                        println!("Local var: {}",name);
                        let a = self.locals.get(name).expect("It should get it");
                        let var = self.var_type_to_type(&a.var);
                        self.builder.build_load(var, a.ptr.clone(), "load").expect("This should work").into()
                    }
                    _ => todo!("TODO: Other OfP for build_expr: {:?}",v)
                }
            }
            Expression::expr(expr) => {
                match expr.op {
                    crate::Op::PLUS => {
                        let left  = self.build_expr(expr.left.as_ref().unwrap());
                        let right = self.build_expr(expr.right.as_ref().unwrap());
                        match left {
                            AnyValueEnum::IntValue(left) => {
                                assert!(right.is_int_value() || right.is_float_value(), "Type checking failed and trying to add integer to other type");
                                let right = right.into_int_value();
                                self.builder.build_int_add(left, right, "Addition").expect("You should not fail I hope").into()
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
    fn build(&mut self, _p: &CmdProgram, b: &BuildProgram)
    {
        assert!(b.dll_imports.len() == 0, "TODO: DLL-imports in llvm native");
        assert!(b.dll_exports.len() == 0, "TODO: DLL-exports in llvm native");
        assert!(b.global_vars.len() == 0, "TODO: Global vars");
        assert!(b.buffers.len() == 0, "TODO: Buffers");
        assert!(b.externals.len() == 0, "TODO: Externals");
        for (fname, func) in b.functions.iter() {
            let ty = self.build_contract(&func.contract);
            let f = self.module.add_function(&fname, ty, None);
            let entry = self.context.append_basic_block(f, "entry");
            self.builder.position_at_end(entry);
            let mut locals: LocalsLLVM = LocalsLLVM::with_capacity(func.locals.len());
            for (i, (name, local)) in func.locals.iter().enumerate() {
                let ty = self.var_type_to_type(local);
                let ptr = self.builder.build_alloca(ty, name).expect("You should work!");
                locals.insert(name.clone(), LocalLLVM {var: local.clone(), ptr});
                let v = f.get_nth_param(i as u32).expect("This should work");
                self.builder.build_store(ptr, v).expect("This should work");
            }
            self.locals = locals;
        
            assert!(func.buffers.len() == 0, "TODO: Buffers");
            for (_,inst) in func.body.iter() {
                match inst {
                    // TODO: remove this
                    Instruction::FNBEGIN() => {}
                    Instruction::RET(exp) => {
                        let ret = self.build_expr(exp);
                        let r = match ret {
                            AnyValueEnum::IntValue(v) => v.as_basic_value_enum(),
                            _ => todo!("Unsupported return value {:?}",ret)
                        };
                        self.builder.build_return(Some(&r)).expect("I hope this don't fail :D");                  
                    }
                    _ => todo!("TODO: Instruction: {:?}", inst)
                }
            }
        }
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
    let mut env = BuildEnv {context: &context, builder, module, locals: LocalsLLVM::default()};
    env.build(p, b);
    println!("Module {}",env.module.to_string());
    machine.write_to_file(
        &env.module,
        inkwell::targets::FileType::Object,
        &std::path::Path::new(path)
    ).map_err(|err| {
        io::Error::new(io::ErrorKind::Other, err.to_string())
    })?;
    Ok(())
}
