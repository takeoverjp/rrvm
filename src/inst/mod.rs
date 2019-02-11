pub mod encodings;
pub use encodings::*;

fn extract8(bits:u8, lo:u8, len:u8) -> u8 {
    (bits >> lo) & ((1 << len) - 1)
}

fn extract16(bits:u16, lo:u8, len:u8) -> u16 {
    (bits >> lo) & ((1 << len) - 1)
}

fn inst_r(_funct7:u32, _rs2:u8, _rs1:u8, _funct3:u32, _rd:u8, _opcode:u32) -> u32 {
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

fn inst_i(_imm:u16, _rs1:u8, _funct3:u32, _rd:u8, _opcode:u32) -> u32 {
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

fn inst_i_shamt(_l_or_a:u8, _shamt:u8, _rs1:u8, _funct3:u32, _rd:u8, _opcode:u32) -> u32 {
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

fn inst_u(_imm:u32, _rd:u8, _opcode:u32) -> u32 {
    let imm    = _imm    & ((1 << 20) - 1);
    let rd     = _rd     & ((1 <<  5) - 1);
    let opcode = (_opcode << 2 | 0b11) & ((1 <<  7) - 1);
    return ((imm as u32)   << (5 + 7))
        | ((rd as u32)     << 7)
        | (opcode as u32);
}

fn inst_cr(_funct4:u16, _rd:u8, _rs2:u8, _opcode:u16) -> u16 {
    let funct4 = _funct4 & ((1 <<  4) - 1);
    let rd     = _rd     & ((1 <<  5) - 1);
    let rs2    = _rs2    & ((1 <<  5) - 1);
    let opcode = _opcode & ((1 <<  2) - 1);
    return ((funct4 as u16)   << (5 + 5 + 2))
        | ((rd as u16)    << (5 + 2))
        | ((rs2 as u16) << (2))
        | (opcode as u16);
}

fn inst_ci(_funct3:u16, _rd:u8, _imm:u8, _opcode:u16) -> u16 {
    let funct3 = _funct3 & ((1 <<  4) - 1);
    let rd     = _rd     & ((1 <<  5) - 1);
    let imm_hi = (_imm    & ((1 <<  6) - 1)) >> 5;
    let imm_lo = _imm    & ((1 <<  5) - 1);
    let opcode = _opcode & ((1 <<  2) - 1);
    return ((funct3 as u16)   << (1 + 5 + 5 + 2))
        | ((imm_hi as u16)    << (5 + 5 + 2))
        | ((rd as u16)    << (5 + 2))
        | ((imm_lo as u16) << (2))
        | (opcode as u16);
}

/// Returns instruction code of `add`.
///
/// ```asm
/// add rd, rs1, rs2
/// ```
pub fn inst_add(rd:usize, rs1:usize, rs2:usize) -> u32 {
    inst_r(FUNCT7_ADD, rs2 as u8, rs1 as u8, FUNCT3_ADD_SUB, rd as u8, OP)
}

/// Returns instruction code of `sub`.
///
/// ```asm
/// sub rd, rs1, rs2
/// ```
pub fn inst_sub(rd:usize, rs1:usize, rs2:usize) -> u32 {
    inst_r(FUNCT7_SUB, rs2 as u8, rs1 as u8, FUNCT3_ADD_SUB, rd as u8, OP)
}

/// Returns instruction code of `lb`.
///
/// ```asm
/// lb rd, offset(rs1)
/// ```
pub fn inst_lb(rd:usize, offset: u16, rs1:usize) -> u32 {
    inst_i(offset, rs1 as u8, FUNCT3_LB, rd as u8, LOAD)
}

/// Returns instruction code of `lh`.
///
/// ```asm
/// lh rd, offset(rs1)
/// ```
pub fn inst_lh(rd:usize, offset: u16, rs1:usize) -> u32 {
    inst_i(offset, rs1 as u8, FUNCT3_LH, rd as u8, LOAD)
}

/// Returns instruction code of `addi`.
///
/// ```asm
/// addi rd, rs1, imm
/// ```
pub fn inst_addi(rd:usize, rs1:usize, imm: u16) -> u32 {
    inst_i(imm, rs1 as u8, FUNCT3_ADDI, rd as u8, OP_IMM)
}

/// Returns instruction code of `slli`.
///
/// ```asm
/// slli rd, rs1, shamt
/// ```
pub fn inst_slli(rd:usize, rs1:usize, shamt: u8) -> u32 {
    inst_i_shamt(0b000000, shamt, rs1 as u8, FUNCT3_SLLI, rd as u8, OP_IMM)
}

/// Returns instruction code of `slti`.
///
/// ```asm
/// slti rd, rs1, imm
/// ```
pub fn inst_slti(rd:usize, rs1:usize, imm: u16) -> u32 {
    inst_i(imm, rs1 as u8, FUNCT3_SLTI, rd as u8, OP_IMM)
}

/// Returns instruction code of `sltiu`.
///
/// ```asm
/// sltiu rd, rs1, imm
/// ```
pub fn inst_sltiu(rd:usize, rs1:usize, imm: u16) -> u32 {
    inst_i(imm, rs1 as u8, FUNCT3_SLTIU, rd as u8, OP_IMM)
}

/// Returns instruction code of `xori`.
///
/// ```asm
/// xori rd, rs1, imm
/// ```
pub fn inst_xori(rd:usize, rs1:usize, imm: u16) -> u32 {
    inst_i(imm, rs1 as u8, FUNCT3_XORI, rd as u8, OP_IMM)
}

/// Returns instruction code of `srli`.
///
/// ```asm
/// srli rd, rs1, shamt
/// ```
pub fn inst_srli(rd:usize, rs1:usize, shamt: u8) -> u32 {
    inst_i_shamt(0b000000, shamt, rs1 as u8, FUNCT3_SRLI_SRAI, rd as u8, OP_IMM)
}

/// Returns instruction code of `srai`.
///
/// ```asm
/// srai rd, rs1, shamt
/// ```
pub fn inst_srai(rd:usize, rs1:usize, shamt: u8) -> u32 {
    inst_i_shamt(0b010000, shamt, rs1 as u8, FUNCT3_SRLI_SRAI, rd as u8, OP_IMM)
}

/// Returns instruction code of `ori`.
///
/// ```asm
/// ori rd, rs1, imm
/// ```
pub fn inst_ori(rd:usize, rs1:usize, imm: u16) -> u32 {
    inst_i(imm, rs1 as u8, FUNCT3_ORI, rd as u8, OP_IMM)
}

/// Returns instruction code of `andi`.
///
/// ```asm
/// andi rd, rs1, imm
/// ```
pub fn inst_andi(rd:usize, rs1:usize, imm: u16) -> u32 {
    inst_i(imm, rs1 as u8, FUNCT3_ANDI, rd as u8, OP_IMM)
}

/// Returns instruction code of `auipc`.
///
/// ```asm
/// auipc rd, imm
/// ```
pub fn inst_auipc(rd:usize, immidiate: u32) -> u32 {
    inst_u(immidiate, rd as u8, AUIPC)
}

/// Returns instruction code of `lui`.
///
/// ```asm
/// lui rd, imm
/// ```
pub fn inst_lui(rd:usize, immidiate: u32) -> u32 {
    inst_u(immidiate, rd as u8, LUI)
}

/// Returns instruction code of `c.mv`.
///
/// ```asm
/// c.mv rd, rs2
/// ```
pub fn inst_c_mv(rd:usize, rs2:usize) -> u16 {
    inst_cr(FUNCT4_C_MV, rd as u8, rs2 as u8, OP_C2)
}

/// Returns whether `c.mv` or not.
pub fn is_c_mv(inst:u16) -> bool {
    let funct4 = extract16(inst, 12, 4);
    let opcode = extract16(inst, 0, 2);
    (funct4 == FUNCT4_C_MV) && (opcode == OP_C2)
}

#[test]
fn test_is_c_mv() {
    assert_eq!(true,  is_c_mv(inst_c_mv(2, 1)));
    assert_eq!(false, is_c_mv(inst_c_add(2, 1)));
}

/// Decompresses `c.mv rd, rs2` to `add rd, rs0, rs2`.
pub fn dec_c_mv(inst:u16) -> u32 {
    let rd  = extract16(inst, 7, 5) as usize;
    let rs2 = extract16(inst, 2, 5) as usize;

    inst_add(rd, 0, rs2)
}

#[test]
fn test_dec_c_mv() {
    assert_eq!(inst_add( 2, 0,  1),  dec_c_mv(inst_c_mv( 2,  1)));
    assert_eq!(inst_add(31, 0,  1),  dec_c_mv(inst_c_mv(31,  1)));
    assert_eq!(inst_add( 2, 0, 31),  dec_c_mv(inst_c_mv( 2, 31)));
}

/// Returns instruction code of `c.add`.
///
/// ```asm
/// c.add rd, rs2
/// ```
pub fn inst_c_add(rd:usize, rs2:usize) -> u16 {
    inst_cr(FUNCT4_C_ADD, rd as u8, rs2 as u8, OP_C2)
}

/// Returns whether `c.add` or not.
pub fn is_c_add(inst:u16) -> bool {
    let funct4 = extract16(inst, 12, 4);
    let opcode = extract16(inst, 0, 2);
    (funct4 == FUNCT4_C_ADD) && (opcode == OP_C2)
}

#[test]
fn test_is_c_add() {
    assert_eq!(true,  is_c_add(inst_c_add(2, 1)));
    assert_eq!(false, is_c_add(inst_c_mv(2, 1)));
}

/// Decompresses `c.add rd, rs2` to `add rd, rd, rs2`.
pub fn dec_c_add(inst:u16) -> u32 {
    let rd  = extract16(inst, 7, 5) as usize;
    let rs2 = extract16(inst, 2, 5) as usize;

    inst_add(rd, rd, rs2)
}

#[test]
fn test_dec_c_add() {
    assert_eq!(inst_add( 2,  2,  1),  dec_c_add(inst_c_add( 2,  1)));
    assert_eq!(inst_add(31, 31,  1),  dec_c_add(inst_c_add(31,  1)));
    assert_eq!(inst_add( 2,  2, 31),  dec_c_add(inst_c_add( 2, 31)));
}

/// Returns instruction code of `c.addi`.
///
/// ```asm
/// c.addi rd, imm
/// ```
pub fn inst_c_addi(rd:usize, imm:u8) -> u16 {
    inst_ci(FUNCT3_C_ADDI, rd as u8, imm, OP_C1)
}

/// Returns whether `c.addi` or not.
pub fn is_c_addi(inst:u16) -> bool {
    let funct3 = extract16(inst, 13, 3);
    let opcode = extract16(inst, 0, 2);
    (funct3 == FUNCT3_C_ADDI) && (opcode == OP_C1)
}

#[test]
fn test_is_c_addi() {
    assert_eq!(true,  is_c_addi(inst_c_addi(2, 1)));
    assert_eq!(false, is_c_addi(inst_c_add(2, 1)));
    assert_eq!(false, is_c_addi(inst_c_mv(2, 1)));
}

/// Decompresses `c.addi rd, imm` to `addi rd, rd, imm`.
pub fn dec_c_addi(inst:u16) -> u32 {
    let rd  = extract16(inst, 7, 5) as usize;
    let imm_hi = extract16(inst, 12, 1);
    let imm_lo = extract16(inst, 2, 5);
    let imm = (imm_hi << 5) | imm_lo;

    inst_addi(rd, rd, imm)
}

#[test]
fn test_dec_c_addi() {
    assert_eq!(inst_addi(2, 2, 1), dec_c_addi(inst_c_addi(2, 1)));
    assert_eq!(inst_addi(31,31, 1), dec_c_addi(inst_c_addi(31, 1)));
    assert_eq!(inst_addi(2, 2, 0b11_1111), dec_c_addi(inst_c_addi(2, 0b11_1111)));
}

/// Returns instruction code of `c.addi16sp`.
///
/// ```asm
/// c.addi16sp sp, imm
/// ```
pub fn inst_c_addi16sp(rd:usize, imm:u8) -> u16 {
    assert_eq!(2, rd);
    let imm_9   = extract8(imm, 5, 1);
    let imm_8_7 = extract8(imm, 3, 2);
    let imm_6   = extract8(imm, 2, 1);
    let imm_5   = extract8(imm, 1, 1);
    let imm_4   = extract8(imm, 0, 1);
    let imm = (imm_9 << 5)
        | (imm_4 << 4)
        | (imm_6 << 3)
        | (imm_8_7 << 1)
        | imm_5;
    inst_ci(FUNCT3_C_ADDI16SP, rd as u8, imm, OP_C1)
}

/// Returns whether `c.addi16sp` or not.
pub fn is_c_addi16sp(inst:u16) -> bool {
    let funct3 = extract16(inst, 13, 3);
    let rd     = extract16(inst, 7, 5);
    let opcode = extract16(inst, 0, 2);
    (funct3 == FUNCT3_C_ADDI16SP) && (rd == 2) && (opcode == OP_C1)
}

#[test]
fn test_is_c_addi16sp() {
    assert_eq!(true,  is_c_addi16sp(inst_c_addi16sp(2, 1)));
    assert_eq!(false, is_c_addi16sp(inst_c_addi(2, 1)));
    assert_eq!(false, is_c_addi16sp(inst_c_add(2, 1)));
    assert_eq!(false, is_c_addi16sp(inst_c_mv(2, 1)));
}

/// Decompresses `c.addi16sp imm` to `addi sp, sp, imm*16`.
pub fn dec_c_addi16sp(inst:u16) -> u32 {
    let rd  = extract16(inst, 7, 5) as usize;
    let imm_9   = extract16(inst, 12, 1);
    let imm_8_7 = extract16(inst, 3, 2);
    let imm_6   = extract16(inst, 5, 1);
    let imm_5   = extract16(inst, 2, 1);
    let imm_4   = extract16(inst, 6, 1);
    let imm = (imm_9 << 9)
        | (imm_8_7 << 7)
        | (imm_6 << 6)
        | (imm_5 << 5)
        | (imm_4 << 4);

    inst_addi(rd, rd, imm)
}

#[test]
fn test_dec_c_addi16sp() {
    assert_eq!(inst_addi(2, 2, 1*16), dec_c_addi16sp(inst_c_addi16sp(2, 1)));
    assert_eq!(inst_addi(2, 2, 0b11_1111*16), dec_c_addi16sp(inst_c_addi16sp(2, 0b11_1111)));
}

pub fn decompress(c_inst: u16) -> u32 {
    if is_c_mv (c_inst) {
        dec_c_mv (c_inst)
    } else if is_c_add (c_inst) {
        dec_c_add (c_inst)
    } else if is_c_addi (c_inst) {
        dec_c_addi (c_inst)
    } else if is_c_addi16sp (c_inst) {
        dec_c_addi16sp (c_inst)
    } else {
        unimplemented!("c_inst = 0x{:04x}", c_inst)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_c_mv() {
        // c.mv r2, r1
        let c_inst: u16 = inst_c_mv(2, 1);
        // add r2, r0, r1
        let inst: u32 = inst_add(2, 0, 1);
        assert_eq!(inst, decompress(c_inst));
    }

    #[test]
    fn test_c_add() {
        // c.add r2, r1
        let c_inst: u16 = inst_c_add(2, 1);
        // add r2, r2, r1
        let inst: u32 = inst_add(2, 2, 1);
        assert_eq!(inst, decompress(c_inst));
    }
}
