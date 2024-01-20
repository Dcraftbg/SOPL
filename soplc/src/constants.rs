use crate::*;

#[derive(Debug, PartialEq, Clone)]
pub enum ConstValueType {
    BOOLEAN(bool),
    CHAR(i8),
    SHORT(i16),
    INT(i32),
    LONG(i64),
    STR(String, ProgramStringType),
    PTR(Ptr, i64),
}

#[derive(Debug,PartialEq,Clone)]
pub struct ConstValue {
    pub typ: ConstValueType,
    pub loc: ProgramLocation
}
impl ConstValueType {
    pub fn unwrap_int_data(&self) -> Option<i64> {
        match self {
            Self::INT(d) => Some(d.clone() as i64),
            Self::LONG(d) => Some(d.clone() as i64),
            Self::SHORT(d) => Some(d.clone() as i64),
            Self::CHAR(d) => Some(d.clone() as i64),
            Self::BOOLEAN(d) => Some(d.clone() as i64),
            _ => None
        }
    }
    pub fn unwrap_str_data(&self) -> Option<&String>  {
        match self {
            ConstValueType::STR(d, _) => Some(d),
            _ => None
        }
    }
    pub fn is_int(&self) -> bool {
        match self {
            Self::INT(_) | Self::LONG(_) | Self::SHORT(_) | Self::CHAR(_) | Self::BOOLEAN(_) | Self::PTR(_,_) => true,
            _ => false
        }
    }
    pub fn weak_cast(&self, typ: &VarType) -> Option<ConstValueType> {
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
    pub fn to_var_type(&self) -> Option<VarType> {
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
    pub fn is_eq_vartype(&self, vartyp: &VarType) -> bool {
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
    pub fn mul(&self, Other: &ConstValueType) -> Result<ConstValueType,String> {
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
    pub fn sub(&self, Other: &ConstValueType) -> Result<ConstValueType,String> {
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
    pub fn add(&self, Other: &ConstValueType) -> Result<ConstValueType,String> {
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

pub type RawConstants = HashMap<String, RawConstValue>;
