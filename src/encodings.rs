const OPCODE_MASK : u32 = 0b00000000_00000000_00000000_01111100;
const RD_MASK     : u32 = 0b00000000_00000000_00001111_10000000;
const FUNCT3_MASK : u32 = 0b00000000_00000000_01110000_00000000;
const RS1_MASK    : u32 = 0b00000000_00001111_10000000_00000000;
const IMM12_MASK  : u32 = 0b11111111_11110000_00000000_00000000;
const RS2_MASK    : u32 = 0b00000001_11110000_00000000_00000000;
const SHAMT_MASK  : u32 = 0b00000011_11110000_00000000_00000000;
const FUNCT7_MASK : u32 = 0b11111110_00000000_00000000_00000000;
const FENCE_PRED_MASK : u32 = 0b00001111_00000000_00000000_00000000;
const FENCE_SUCC_MASK : u32 = 0b00000000_11110000_00000000_00000000;
const OPCODE_SHIFT : u8 = 2;
const RD_SHIFT     : u8 = 7;
const FUNCT3_SHIFT : u8 = 12;
const RS1_SHIFT    : u8 = 15;
const IMM12_SHIFT  : u8 = 20;
const RS2_SHIFT    : u8 = 20;
const SHAMT_SHIFT  : u8 = 20;
const FUNCT7_SHIFT : u8 = 25;
const FENCE_PRED_SHIFT : u8 = 24;
const FENCE_SUCC_SHIFT : u8 = 20;

pub const LOAD          : u32 = 0b00_000;
pub const LOAD_FP       : u32 = 0b00_001;
pub const CUSTOM_0      : u32 = 0b00_010;
pub const MISC_MEM      : u32 = 0b00_011;
pub const OP_IMM        : u32 = 0b00_100;
pub const AUIPC         : u32 = 0b00_101;
pub const OP_IMM_32     : u32 = 0b00_110;
pub const LONG_OP_48    : u32 = 0b00_111;
pub const STORE         : u32 = 0b01_000;
pub const STORE_FP      : u32 = 0b01_001;
pub const CUSTOM_1      : u32 = 0b01_010;
pub const AMO           : u32 = 0b01_011;
pub const OP            : u32 = 0b01_100;
pub const LUI           : u32 = 0b01_101;
pub const OP_32         : u32 = 0b01_110;
pub const LONG_OP_64    : u32 = 0b01_111;
pub const MADD          : u32 = 0b10_000;
pub const MSUB          : u32 = 0b10_001;
pub const NMSUB         : u32 = 0b10_010;
pub const NMADD         : u32 = 0b10_011;
pub const OP_FP         : u32 = 0b10_100;
//pub const RESERVED      : u32 = 0b10_101;
pub const CUSTOM2       : u32 = 0b10_110;
pub const LONG_OP_48_2  : u32 = 0b10_111;
pub const BRANCH        : u32 = 0b11_000;
pub const JALR          : u32 = 0b11_001;
//pub const RESERVED      : u32 = 0b11_010;
pub const JAL           : u32 = 0b11_011;
pub const SYSTEM        : u32 = 0b11_100;
//pub const RESERVED      : u32 = 0b11_101;
pub const CUSTOM3       : u32 = 0b11_110;
pub const LONG_OP_GE_80 : u32 = 0b11_111;

pub fn get_opcode(inst: u32) -> u32 {
    (inst & OPCODE_MASK) >> OPCODE_SHIFT
}

pub fn get_rd(inst: u32) -> u32 {
    (inst & RD_MASK) >> RD_SHIFT
}

pub fn get_funct3(inst: u32) -> u32 {
    (inst & FUNCT3_MASK) >> FUNCT3_SHIFT
}

pub fn get_rs1(inst: u32) -> u32 {
    (inst & RS1_MASK) >> RS1_SHIFT
}

pub fn get_imm12(inst: u32) -> u32 {
    (inst & IMM12_MASK) >> IMM12_SHIFT
}

pub fn get_rs2(inst: u32) -> u32 {
    (inst & RS2_MASK) >> RS2_SHIFT
}

pub fn get_shamt(inst: u32) -> u32 {
    (inst & SHAMT_MASK) >> SHAMT_SHIFT
}

pub fn get_funct7(inst: u32) -> u32 {
    (inst & FUNCT7_MASK) >> FUNCT7_SHIFT
}

pub fn get_fence_pred(inst: u32) -> u32 {
    (inst & FENCE_PRED_MASK) >> FENCE_PRED_SHIFT
}

pub fn get_fence_succ(inst: u32) -> u32 {
    (inst & FENCE_SUCC_MASK) >> FENCE_SUCC_SHIFT
}
