pub mod encodings;
pub use encodings::*;

pub fn inst_r(_funct7:u32, _rs2:u8, _rs1:u8, _funct3:u32, _rd:u8, _opcode:u32) -> u32 {
    let funct7 = _funct7 & ((1 <<  7) - 1);
    let rs2    = _rs2    & ((1 <<  5) - 1);
    let rs1    = _rs1    & ((1 <<  5) - 1);
    let funct3 = _funct3 & ((1 <<  3) - 1);
    let rd     = _rd     & ((1 <<  5) - 1);
    let opcode = (_opcode << 2 | 0b11) & ((1 <<  7) - 1);
    return ((funct7 as u32) << (5 + 5 + 3 + 5 + 7))
        | ((rs2 as u32)     << (5 + 3 + 5 + 7))
        | ((rs1 as u32)     << (3 + 5 + 7))
        | ((funct3 as u32)  << (5 + 7))
        | ((rd as u32)      << 7)
        | (opcode as u32);
}

pub fn inst_i(_imm:u16, _rs1:u8, _funct3:u32, _rd:u8, _opcode:u32) -> u32 {
    let imm    = _imm    & ((1 << 12) - 1);
    let rs1    = _rs1    & ((1 <<  5) - 1);
    let funct3 = _funct3 & ((1 <<  3) - 1);
    let rd     = _rd     & ((1 <<  5) - 1);
    let opcode = (_opcode << 2 | 0b11) & ((1 <<  7) - 1);
    return ((imm as u32)   << (5 + 3 + 5 + 7))
        | ((rs1 as u32)    << (3 + 5 + 7))
        | ((funct3 as u32) << (5 + 7))
        | ((rd as u32)     << 7)
        | (opcode as u32);
}

pub fn inst_i_shamt(_l_or_a:u8, _shamt:u8, _rs1:u8, _funct3:u32, _rd:u8, _opcode:u32) -> u32 {
    let l_or_a = _l_or_a & ((1 << 6) - 1);
    let shamt  = _shamt  & ((1 << 6) - 1);
    let rs1    = _rs1    & ((1 << 5) - 1);
    let funct3 = _funct3 & ((1 << 3) - 1);
    let rd     = _rd     & ((1 << 5) - 1);
    let opcode = (_opcode << 2 | 0b11) & ((1 << 7) - 1);
    return ((l_or_a as u32) << (6 + 5 + 3 + 5 + 7))
        | ((shamt as u32)   << (5 + 3 + 5 + 7))
        | ((rs1 as u32)     << (3 + 5 + 7))
        | ((funct3 as u32)  << (5 + 7))
        | ((rd as u32)     << 7)
        | (opcode as u32);
}

pub fn inst_u(_imm:u32, _rd:u8, _opcode:u32) -> u32 {
    let imm    = _imm    & ((1 << 20) - 1);
    let rd     = _rd     & ((1 <<  5) - 1);
    let opcode = (_opcode << 2 | 0b11) & ((1 <<  7) - 1);
    return ((imm as u32)   << (5 + 7))
        | ((rd as u32)     << 7)
        | (opcode as u32);
}

pub fn inst_cr(_funct4:u16, _rd:u8, _rs2:u8, _opcode:u16) -> u16 {
    let funct4 = _funct4 & ((1 <<  4) - 1);
    let rd     = _rd     & ((1 <<  5) - 1);
    let rs2    = _rs2    & ((1 <<  5) - 1);
    let opcode = _opcode & ((1 <<  2) - 1);
    return ((funct4 as u16)   << (5 + 5 + 2))
        | ((rd as u16)    << (5 + 2))
        | ((rs2 as u16) << (2))
        | (opcode as u16);
}

pub fn inst_add(rd:u8, rs1:u8, rs2:u8) -> u32 {
    inst_r(FUNCT7_ADD, rs2, rs1, FUNCT3_ADD_SUB, rd, OP)
}
