use std::fmt::Display;
#[repr(u32)]
#[derive(Clone, Copy,Debug, PartialEq)]
pub enum Register {
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
    ESI,
    EDI,
    // 16 bit
    AX,
    BX,
    CX,
    DX,
    SP,
    BP,
    SI,
    DI,
    // 8 bit low
    AL,
    BL,
    CL,
    DL,
    SIL,
    DIL,
    // 8 bit high
    AH,
    BH,
    CH,
    DH,



    // General Purpose registers
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    // General Purpose registers 32
    R8D,
    R9D,
    R10D,
    R11D,
    R12D,
    R13D,
    R14D,
    R15D,
    // General Purpose registers 16
    R8W,
    R9W,
    R10W,
    R11W,
    R12W,
    R13W,
    R14W,
    R15W,
    // General Purpose registers 8
    R8B,
    R9B,
    R10B,
    R11B,
    R12B,
    R13B,
    R14B,
    R15B,

    //Floating point 32 arithmetics
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,

}
impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_string())
    }
}
impl Register {
    pub fn to_string(&self) -> String {
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
            Register::R9  => "r9".to_string(),
            Register::R10 => "r10".to_string(),
            Register::R11 => "r11".to_string(),
            Register::R12 => "r12".to_string(),
            Register::R13 => "r13".to_string(),
            Register::R14 => "r14".to_string(),
            Register::R15 => "r15".to_string(),
            Register::XMM0 => "xmm0".to_string(),
            Register::XMM1 => "xmm1".to_string(),
            Register::XMM2 => "xmm2".to_string(),
            Register::XMM3 => "xmm3".to_string(),
            Register::XMM4 => "xmm4".to_string(),
            Register::XMM5 => "xmm5".to_string(),
            Register::XMM6 => "xmm6".to_string(),
            Register::XMM7 => "xmm7".to_string(),
            Register::ESI => "esi".to_string(),
            Register::EDI => "edi".to_string(),
            Register::SI  => "si".to_string(),
            Register::DI  => "di".to_string(),
            Register::SIL => "sil".to_string(),
            Register::DIL => "dil".to_string(),

            Register::R8D  => "r8d".to_string(),
            Register::R9D  => "r9d".to_string(),
            Register::R10D => "r10d".to_string(),
            Register::R11D => "r11d".to_string(),
            Register::R12D => "r12d".to_string(),
            Register::R13D => "r13d".to_string(),
            Register::R14D => "r14d".to_string(),
            Register::R15D => "r15d".to_string(),
            Register::R8W  => "r8w".to_string(),
            Register::R9W  => "r9w".to_string(),
            Register::R10W => "r10w".to_string(),
            Register::R11W => "r11w".to_string(),
            Register::R12W => "r12w".to_string(),
            Register::R13W => "r13w".to_string(),
            Register::R14W => "r14w".to_string(),
            Register::R15W => "r15w".to_string(),
            Register::R8B  => "r8b".to_string(),
            Register::R9B  => "r9b".to_string(),
            Register::R10B => "r10b".to_string(),
            Register::R11B => "r11b".to_string(),
            Register::R12B => "r12b".to_string(),
            Register::R13B => "r13b".to_string(),
            Register::R14B => "r14b".to_string(),
            Register::R15B => "r15b".to_string(),
        }
    }
    pub fn from_string(stri: &String) -> Option<Self> {
        match stri.as_str() {
             "RAX"  | "rax"  => Some(Register::RAX),
             "RBX"  | "rbx"  => Some(Register::RBX),
             "RCX"  | "rcx"  => Some(Register::RCX),
             "RDX"  | "rdx"  => Some(Register::RDX),
             "RSP"  | "rsp"  => Some(Register::RSP),
             "RBP"  | "rbp"  => Some(Register::RBP),
             "EAX"  | "eax"  => Some(Register::EAX),
             "EBX"  | "ebx"  => Some(Register::EBX),
             "ECX"  | "ecx"  => Some(Register::ECX),
             "EDX"  | "edx"  => Some(Register::EDX),
             "ESP"  | "esp"  => Some(Register::ESP),
             "EBP"  | "ebp"  => Some(Register::EBP),
             "AX"   | "ax"   => Some(Register::AX ),
             "BX"   | "bx"   => Some(Register::BX ),
             "CX"   | "cx"   => Some(Register::CX ),
             "DX"   | "dx"   => Some(Register::DX ),
             "SP"   | "sp"   => Some(Register::SP ),
             "BP"   | "bp"   => Some(Register::BP ),
             "AL"   | "al"   => Some(Register::AL ),
             "BL"   | "bl"   => Some(Register::BL ),
             "CL"   | "cl"   => Some(Register::CL ),
             "DL"   | "dl"   => Some(Register::DL ),
             "AH"   | "ah"   => Some(Register::AH ),
             "BH"   | "bh"   => Some(Register::BH ),
             "CH"   | "ch"   => Some(Register::CH ),
             "DH"   | "dh"   => Some(Register::DH ),
             "R8"   | "r8"   => Some(Register::R8),
             "R9"   | "r9"   => Some(Register::R9),
             "R10"  | "r10"  => Some(Register::R10),
             "R11"  | "r11"  => Some(Register::R11),
             "R12"  | "r12"  => Some(Register::R12),
             "R13"  | "r13"  => Some(Register::R13),
             "R14"  | "r14"  => Some(Register::R14),
             "R15"  | "r15"  => Some(Register::R15),
             "XMM0" | "XMM0" => Some(Register::XMM0),
             "XMM1" | "XMM1" => Some(Register::XMM1),
             "XMM2" | "XMM2" => Some(Register::XMM2),
             "XMM3" | "XMM3" => Some(Register::XMM3),
             "RSI" | "rsi" => Some(Register::RSI),
             "RDI" | "rdi" => Some(Register::RDI),

             "R8D" | "r8d" => Some(Register::R8D),

            _ => None
        }
        //TODO ^ Add the rest of the registers as strings
    }
    pub fn size(&self) -> usize {
        match self {
            Register::RAX  => 8,
            Register::RBX  => 8,
            Register::RCX  => 8,
            Register::RDX  => 8,
            Register::RSP  => 8,
            Register::RBP  => 8,
            Register::RSI  => 8,
            Register::RDI  => 8,
            Register::EAX  => 4,
            Register::EBX  => 4,
            Register::ECX  => 4,
            Register::EDX  => 4,
            Register::ESP  => 4,
            Register::EBP  => 4,
            Register::ESI  => 4,
            Register::EDI  => 4,
            Register::AX   => 2,
            Register::BX   => 2,
            Register::CX   => 2,
            Register::DX   => 2,
            Register::SP   => 2,
            Register::BP   => 2,
            Register::SI   => 2,
            Register::DI   => 2,
            Register::AL   => 1,
            Register::BL   => 1,
            Register::CL   => 1,
            Register::DL   => 1,
            Register::AH   => 1,
            Register::BH   => 1,
            Register::CH   => 1,
            Register::DH   => 1,
            Register::SIL  => 1,
            Register::DIL  => 1,
            Register::RSI  => 8,
            Register::RDI  => 8,
            Register::R8   => 8,
            Register::R9   => 8,
            Register::R10  => 8,
            Register::R11  => 8,
            Register::R12  => 8,
            Register::R13  => 8,
            Register::R14  => 8,
            Register::R15  => 8,
            Register::XMM0 => 4,
            Register::XMM1 => 4,
            Register::XMM2 => 4,
            Register::XMM3 => 4,
            Register::XMM4 => 4,
            Register::XMM5 => 4,
            Register::XMM6 => 4,
            Register::XMM7 => 4,
            Register::R8D  => 4,
            Register::R9D  => 4,
            Register::R10D => 4,
            Register::R11D => 4,
            Register::R12D => 4,
            Register::R13D => 4,
            Register::R14D => 4,
            Register::R15D => 4,
            Register::R8W  => 2,
            Register::R9W  => 2,
            Register::R10W => 2,
            Register::R11W => 2,
            Register::R12W => 2,
            Register::R13W => 2,
            Register::R14W => 2,
            Register::R15W => 2,
            Register::R8B  => 1,
            Register::R9B  => 1,
            Register::R10B => 1,
            Register::R11B => 1,
            Register::R12B => 1,
            Register::R13B => 1,
            Register::R14B => 1,
            Register::R15B => 1,
        }
    }
    pub fn to_byte_size(&self, size: usize) -> Self {
        match size {
            8 => {
              match self {
                Register::RAX | Register::EAX  | Register::AX | Register::AH | Register::AL => Register::RAX,
                Register::RBX | Register::EBX  | Register::BX | Register::BH | Register::BL => Register::RBX,
                Register::RCX | Register::ECX  | Register::CX | Register::CH | Register::CL => Register::RCX,
                Register::RDX | Register::EDX  | Register::DX | Register::DH | Register::DL => Register::RDX,
                Register::RSI | Register::ESI | Register::SI  | Register::SIL => Register::RSI,
                Register::RDI | Register::EDI | Register::DI  | Register::DIL => Register::RDI,
                Register::RSP | Register::ESP  | Register::SP => Register::RSP,
                Register::RBP | Register::EBP  | Register::BP => Register::RBP,
                Register::R8  | Register::R8D | Register::R8W | Register::R8B => Register::R8,
                Register::R9  | Register::R9D | Register::R9W | Register::R9B => Register::R9,
                Register::R10  | Register::R10D | Register::R10W | Register::R10B => Register::R10,
                Register::R11  | Register::R11D | Register::R11W | Register::R11B => Register::R11,
                Register::R12  | Register::R12D | Register::R12W | Register::R12B => Register::R12,
                Register::R13  | Register::R13D | Register::R13W | Register::R13B => Register::R13,
                Register::R14  | Register::R14D | Register::R14W | Register::R14B => Register::R14,
                Register::R15  | Register::R15D | Register::R15W | Register::R15B => Register::R15,
                Register::XMM0 => todo!(),
                Register::XMM1 => todo!(),
                Register::XMM2 => todo!(),
                Register::XMM3 => todo!(),
                Register::XMM4 => todo!(),
                Register::XMM5 => todo!(),
                Register::XMM6 => todo!(),
                Register::XMM7 => todo!(),
              }
            },
            4 => {
                match self {
                    Register::RAX | Register::EAX  | Register::AX | Register::AH | Register::AL => Register::EAX,
                    Register::RBX | Register::EBX  | Register::BX | Register::BH | Register::BL => Register::EBX,
                    Register::RCX | Register::ECX  | Register::CX | Register::CH | Register::CL => Register::ECX,
                    Register::RDX | Register::EDX  | Register::DX | Register::DH | Register::DL => Register::EDX,
                    Register::RSI | Register::ESI | Register::SI  | Register::SIL => Register::ESI,
                    Register::RDI | Register::EDI | Register::DI  | Register::DIL => Register::EDI,
                    Register::RSP | Register::ESP  | Register::SP => Register::ESP,
                    Register::RBP | Register::EBP  | Register::BP => Register::EBP,

                    Register::R8  | Register::R8D | Register::R8W | Register::R8B => Register::R8D,
                    Register::R9  | Register::R9D | Register::R9W | Register::R9B => Register::R9D,
                    Register::R10  | Register::R10D | Register::R10W | Register::R10B => Register::R10D,
                    Register::R11  | Register::R11D | Register::R11W | Register::R11B => Register::R11D,
                    Register::R12  | Register::R12D | Register::R12W | Register::R12B => Register::R12D,
                    Register::R13  | Register::R13D | Register::R13W | Register::R13B => Register::R13D,
                    Register::R14  | Register::R14D | Register::R14W | Register::R14B => Register::R14D,
                    Register::R15  | Register::R15D | Register::R15W | Register::R15B => Register::R15D,
                    _ => todo!()
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
                    Register::RSI | Register::ESI | Register::SI  | Register::SIL => Register::SI,

                    Register::RDI | Register::EDI | Register::DI  | Register::DIL => Register::DI,

                    Register::R8  | Register::R8D | Register::R8W | Register::R8B => Register::R8W,
                    Register::R9  | Register::R9D | Register::R9W | Register::R9B => Register::R9W,
                    Register::R10  | Register::R10D | Register::R10W | Register::R10B => Register::R10W,
                    Register::R11  | Register::R11D | Register::R11W | Register::R11B => Register::R11W,
                    Register::R12  | Register::R12D | Register::R12W | Register::R12B => Register::R12W,
                    Register::R13  | Register::R13D | Register::R13W | Register::R13B => Register::R13W,
                    Register::R14  | Register::R14D | Register::R14W | Register::R14B => Register::R14W,
                    Register::R15  | Register::R15D | Register::R15W | Register::R15B => Register::R15W,
                    _ => todo!()
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
                    Register::RSI | Register::ESI | Register::SI | Register::SIL  => Register::SIL,
                    Register::RDI | Register::EDI | Register::DI  | Register::DIL => Register::DIL,

                    Register::R8   | Register::R8D  | Register::R8W  | Register::R8B  => Register::R8B,
                    Register::R9   | Register::R9D  | Register::R9W  | Register::R9B  => Register::R9B,
                    Register::R10  | Register::R10D | Register::R10W | Register::R10B => Register::R10B,
                    Register::R11  | Register::R11D | Register::R11W | Register::R11B => Register::R11B,
                    Register::R12  | Register::R12D | Register::R12W | Register::R12B => Register::R12B,
                    Register::R13  | Register::R13D | Register::R13W | Register::R13B => Register::R13B,
                    Register::R14  | Register::R14D | Register::R14W | Register::R14B => Register::R14B,
                    Register::R15  | Register::R15D | Register::R15W | Register::R15B => Register::R15B,
                    _ => todo!()
                  }
            },
            _ => {
                panic!("Unexpected use case for to_bit_size!");
            }
        }
    }
}
