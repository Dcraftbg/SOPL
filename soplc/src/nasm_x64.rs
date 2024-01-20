use std::fs::File;
use crate::{BuildProgram, CmdProgram, OptimizationMode, GlobalVarType, RawConstValueType, ExternalType};
use std::io::Write;
pub fn size_to_nasm_type(size: usize) -> String {
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
