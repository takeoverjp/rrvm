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

pub const FUNCT7_MULDIV : u32 = 0b0000001;
pub const FUNCT7_SRL    : u32 = 0b0000000;
pub const FUNCT7_SRA    : u32 = 0b0100000;
pub const FUNCT7_ADD    : u32 = 0b0000000;
pub const FUNCT7_SUB    : u32 = 0b0100000;
pub const FUNCT7_ADDW   : u32 = 0b0000000;
pub const FUNCT7_SUBW   : u32 = 0b0100000;
pub const FUNCT7_MULW   : u32 = 0b0000001;
pub const FUNCT7_SRLW   : u32 = 0b0000000;
pub const FUNCT7_SRAW   : u32 = 0b0100000;

pub const FUNCT3_LB           : u32 = 0b000;
pub const FUNCT3_LH           : u32 = 0b001;
pub const FUNCT3_LW           : u32 = 0b010;
pub const FUNCT3_LD           : u32 = 0b011;
pub const FUNCT3_LBU          : u32 = 0b100;
pub const FUNCT3_LHU          : u32 = 0b101;
pub const FUNCT3_LWU          : u32 = 0b110;
pub const FUNCT3_FENCE        : u32 = 0b000;
pub const FUNCT3_FENCE_I      : u32 = 0b001;
pub const FUNCT3_ADDI         : u32 = 0b000;
pub const FUNCT3_SLLI         : u32 = 0b001;
pub const FUNCT3_SLTI         : u32 = 0b010;
pub const FUNCT3_SLTIU        : u32 = 0b011;
pub const FUNCT3_XORI         : u32 = 0b100;
pub const FUNCT3_SRLI_SRAI    : u32 = 0b101;
pub const FUNCT3_ORI          : u32 = 0b110;
pub const FUNCT3_ANDI         : u32 = 0b111;
pub const FUNCT3_ADDIW        : u32 = 0b000;
pub const FUNCT3_SLLIW        : u32 = 0b001;
pub const FUNCT3_SRLIW_SRAIW  : u32 = 0b101;
pub const FUNCT3_SB           : u32 = 0b000;
pub const FUNCT3_SH           : u32 = 0b001;
pub const FUNCT3_SW           : u32 = 0b010;
pub const FUNCT3_SD           : u32 = 0b011;
pub const FUNCT3_MUL          : u32 = 0b000;
pub const FUNCT3_MULH         : u32 = 0b001;
pub const FUNCT3_MULHSU       : u32 = 0b010;
pub const FUNCT3_MULHU        : u32 = 0b011;
pub const FUNCT3_ADD_SUB      : u32 = 0b000;
pub const FUNCT3_SLL          : u32 = 0b001;
pub const FUNCT3_SLT          : u32 = 0b010;
pub const FUNCT3_SLTU         : u32 = 0b011;
pub const FUNCT3_XOR          : u32 = 0b100;
pub const FUNCT3_SRL_SRA      : u32 = 0b101;
pub const FUNCT3_OR           : u32 = 0b110;
pub const FUNCT3_AND          : u32 = 0b111;
pub const FUNCT3_ADDW_SUBW    : u32 = 0b000;
pub const FUNCT3_SLLW         : u32 = 0b001;
pub const FUNCT3_SRLW_SRAW    : u32 = 0b101;
pub const FUNCT3_BEQ          : u32 = 0b000;
pub const FUNCT3_BNE          : u32 = 0b001;
pub const FUNCT3_BLT          : u32 = 0b100;
pub const FUNCT3_BGE          : u32 = 0b101;
pub const FUNCT3_BLTU         : u32 = 0b110;
pub const FUNCT3_BGEU         : u32 = 0b111;
pub const FUNCT3_ECALL_EBREAK : u32 = 0b000;
pub const FUNCT3_CSRRW        : u32 = 0b001;
pub const FUNCT3_CSRRS        : u32 = 0b010;
pub const FUNCT3_CSRRC        : u32 = 0b011;
pub const FUNCT3_CSRRWI       : u32 = 0b101;
pub const FUNCT3_CSRRSI       : u32 = 0b110;
pub const FUNCT3_CSRRCI       : u32 = 0b111;

pub const _OP_C0 : u16 = 0b00;
pub const OP_C1 : u16 = 0b01;
pub const OP_C2 : u16 = 0b10;
pub const _FUNCT3_C_ADDI4SPN : u16 = 0b000;
pub const FUNCT4_C_MV       : u16 = 0b1000;
pub const FUNCT4_C_ADD      : u16 = 0b1001;
pub const FUNCT3_C_ADDI     : u16 = 0b100;

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
