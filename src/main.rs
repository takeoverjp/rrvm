#[macro_use]
extern crate log;
extern crate env_logger;
extern crate getopts;
extern crate memmap;

use std::{env, process};
use std::fs::File;
use getopts::Options;
use memmap::MmapOptions;

const OPCODE_MASK : u32 = 0b00000000_00000000_00000000_01111100;
const RD_MASK     : u32 = 0b00000000_00000000_00001111_10000000;
const FUNCT3_MASK : u32 = 0b00000000_00000000_01110000_00000000;
const RS1_MASK    : u32 = 0b00000000_00001111_10000000_00000000;
const IMM12_MASK  : u32 = 0b11111111_11110000_00000000_00000000;
const RS2_MASK    : u32 = 0b00000001_11110000_00000000_00000000;
const FUNCT7_MASK : u32 = 0b11111110_00000000_00000000_00000000;
const OPCODE_SHIFT : u8 = 2;
const RD_SHIFT     : u8 = 7;
const FUNCT3_SHIFT : u8 = 12;
const RS1_SHIFT    : u8 = 15;
const IMM12_SHIFT  : u8 = 20;
const RS2_SHIFT    : u8 = 20;
const FUNCT7_SHIFT : u8 = 25;

const LOAD          : u32 = 0b00_000;
const LOAD_FP       : u32 = 0b00_001;
const CUSTOM_0      : u32 = 0b00_010;
const MISC_MEM      : u32 = 0b00_011;
const OP_IMM        : u32 = 0b00_100;
const AUIPC         : u32 = 0b00_101;
const OP_IMM_32     : u32 = 0b00_110;
const LONG_OP_48    : u32 = 0b00_111;
const STORE         : u32 = 0b01_000;
const STORE_FP      : u32 = 0b01_001;
const CUSTOM_1      : u32 = 0b01_010;
const AMO           : u32 = 0b01_011;
const OP            : u32 = 0b01_100;
const LUI           : u32 = 0b01_101;
const OP_32         : u32 = 0b01_110;
const LONG_OP_64    : u32 = 0b01_111;
const MADD          : u32 = 0b10_000;
const MSUB          : u32 = 0b10_001;
const NMSUB         : u32 = 0b10_010;
const NMADD         : u32 = 0b10_011;
const OP_FP         : u32 = 0b10_100;
//const RESERVED      : u32 = 0b10_101;
const CUSTOM2       : u32 = 0b10_110;
const LONG_OP_48_2  : u32 = 0b10_111;
const BRANCH        : u32 = 0b11_000;
const JALR          : u32 = 0b11_001;
//const RESERVED      : u32 = 0b11_010;
const JAL           : u32 = 0b11_011;
const SYSTEM        : u32 = 0b11_100;
//const RESERVED      : u32 = 0b11_101;
const CUSTOM3       : u32 = 0b11_110;
const LONG_OP_GE_80 : u32 = 0b11_111;

#[derive(Debug)]
struct Args {
    prog: String,
    log_level: String,
    memory: usize,
}

struct ControlStatusRegister {
    mepc : u64
}

static ABI_NAME: [&'static str; 32] = [
    "zero", "ra", "sp", "gp", "tp", "t0","t1","t2",
    "s0/fp", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
    "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
    "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"];

struct RegisterFile {
    csr : ControlStatusRegister,
    pc : u64,
    x : [u64; 32]
}

impl RegisterFile {
    fn new() -> RegisterFile {
        RegisterFile {
            csr: ControlStatusRegister {
                mepc : 0
            },
            pc: 0,
            x: [0; 32]
        }
    }
    fn dump(&self) {
        trace!("pc = 0x{:x}", self.pc);
        for (idx, x) in self.x.iter().enumerate() {
            trace!("x[{:2?}]({:5}) = 0x{:016x}", idx, ABI_NAME[idx], x);
        }
    }
}

fn u8x4_to_u32(x0:u8, x1:u8, x2:u8, x3:u8) -> u32 {
    return (x0 as u32)
        | ((x1 as u32) << 8)
        | ((x2 as u32) << 16)
        | ((x3 as u32) << 24);
}

fn get_memmap(file_path: &str) -> memmap::Mmap {
    let file = match File::open(file_path) {
        Ok(file) => file,
        Err(e) => {
            error!("file({}) open error {:?}", file_path, e);
            std::process::exit(-1);
        }
    };

    let mmap = unsafe {
        match MmapOptions::new().map(&file) {
            Ok(map) => map,
            Err(e) => {
                error!("memmap error {:?}", e);
                std::process::exit(-1);
            }
        }
    };

    mmap
}

fn get_opcode(inst: u32) -> u32 {
    (inst & OPCODE_MASK) >> OPCODE_SHIFT
}

fn get_rd(inst: u32) -> u32 {
    (inst & RD_MASK) >> RD_SHIFT
}

fn get_funct3(inst: u32) -> u32 {
    (inst & FUNCT3_MASK) >> FUNCT3_SHIFT
}

fn get_rs1(inst: u32) -> u32 {
    (inst & RS1_MASK) >> RS1_SHIFT
}

fn get_imm12(inst: u32) -> u32 {
    (inst & IMM12_MASK) >> IMM12_SHIFT
}

fn get_rs2(inst: u32) -> u32 {
    (inst & RS2_MASK) >> RS2_SHIFT
}

fn get_funct7(inst: u32) -> u32 {
    (inst & FUNCT7_MASK) >> FUNCT7_SHIFT
}

fn fetch(map: &memmap::Mmap, pc: usize) -> u32 {
    u8x4_to_u32(map[pc+0], map[pc+1], map[pc+2], map[pc+3])
}

fn sign_ext(val: u64, size: u8) -> i64 {
    let mut ret = val as i64;

    if (val & (1 << (size - 1))) != 0 {
        ret |= ((1 << (64 - size)) - 1) << size;
    }

    ret
}

#[test]
fn test_sign_ext() {
    assert_eq!(-1, sign_ext(0xf, 4));
    assert_eq!(7, sign_ext(0x7, 4));
    assert_eq!(-1, sign_ext(0xff, 8));
    assert_eq!(0x7f, sign_ext(0x7f, 8));
}

fn handle_load(map: &Vec<u8>, reg: &mut RegisterFile, inst: u32) {
    const FUNCT3_LB  : u32 = 0b000;
    const FUNCT3_LH  : u32 = 0b001;
    const FUNCT3_LW  : u32 = 0b010;
    const FUNCT3_LD  : u32 = 0b011;
    const FUNCT3_LBU : u32 = 0b100;
    const FUNCT3_LHU : u32 = 0b101;
    const FUNCT3_LWU : u32 = 0b110;

    let funct3 = get_funct3(inst);
    let rd     = get_rd(inst) as usize;
    let rs1    = get_rs1(inst) as usize;
    let imm  = get_imm12(inst);
    let offset = sign_ext(imm as u64, 12);
    let addr   = (reg.x[rs1] as i64 + offset) as u64;

    if mmio_region(addr) {
        warn!("{}: {}: Not implemented", file!(), line!());
    } else {
        let addr = addr as usize;
        match funct3 {
            FUNCT3_LB  => {
                info!("lb {},{}({})",
                      ABI_NAME[rd], offset, ABI_NAME[rs1]);
                reg.x[rd] = map[addr] as u64;
                reg.x[rd] = sign_ext(reg.x[rd], 8) as u64;
            },
            FUNCT3_LH  => {
                info!("lh {},{}({})",
                      ABI_NAME[rd], offset, ABI_NAME[rs1]);
                reg.x[rd] = (map[addr] as u64)
                    | ((map[addr+1] as u64) << 8);
                reg.x[rd] = sign_ext(reg.x[rd], 16) as u64;
            },
            FUNCT3_LW  => {
                info!("lw {},{}({})",
                      ABI_NAME[rd], offset, ABI_NAME[rs1]);
                reg.x[rd] = (map[addr] as u64)
                    | ((map[addr+1] as u64) << 8)
                    | ((map[addr+2] as u64) << 16)
                    | ((map[addr+3] as u64) << 24);
                reg.x[rd] = sign_ext(reg.x[rd], 32) as u64;
            },
            FUNCT3_LD  => {
                info!("ld {},{}({})",
                      ABI_NAME[rd], offset, ABI_NAME[rs1]);
                reg.x[rd] = (map[addr] as u64)
                    | ((map[addr+1] as u64) << 8)
                    | ((map[addr+2] as u64) << 16)
                    | ((map[addr+3] as u64) << 24)
                    | ((map[addr+4] as u64) << 32)
                    | ((map[addr+5] as u64) << 40)
                    | ((map[addr+6] as u64) << 48)
                    | ((map[addr+7] as u64) << 56);
            },
            FUNCT3_LBU => {
                info!("lbu {},{}({})",
                      ABI_NAME[rd], offset, ABI_NAME[rs1]);
                reg.x[rd] = map[addr] as u64;
            },
            FUNCT3_LHU => {
                reg.x[rd] = (map[addr] as u64)
                    | ((map[addr+1] as u64) << 8);
            },
            FUNCT3_LWU => {
                info!("lwu {},{}({})",
                      ABI_NAME[rd], offset, ABI_NAME[rs1]);
                reg.x[rd] = (map[addr] as u64)
                    | ((map[addr+1] as u64) << 8)
                    | ((map[addr+2] as u64) << 16)
                    | ((map[addr+3] as u64) << 24);
            },
            _ => warn!("{}: {}: unknown funct3 0x{:x}",
                          file!(), line!(), funct3)
        }
    }
    debug!("load from 0x{:016x}, val = 0x{:016x}", addr, reg.x[rd]);
}

fn handle_load_fp(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_custom_0(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_misc_mem(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_op_imm(reg: &mut RegisterFile, inst: u32) {
    const FUNCT3_ADDI      : u32 = 0b000;
    const FUNCT3_SLLI      : u32 = 0b001;
    const FUNCT3_SLTI      : u32 = 0b010;
    const FUNCT3_SLTIU     : u32 = 0b011;
    const FUNCT3_XORI      : u32 = 0b100;
    const FUNCT3_SRLI_SRAI : u32 = 0b101;
    const FUNCT7_SRLI      : u32 = 0b0000000;
    const FUNCT7_SRAI      : u32 = 0b0100000;
    const FUNCT3_ORI       : u32 = 0b110;
    const FUNCT3_ANDI      : u32 = 0b111;

    let funct3 = get_funct3(inst);
    let rd     = get_rd(inst) as usize;
    let rs1    = get_rs1(inst) as usize;
    let imm  = get_imm12(inst) as u64;
    let simm = sign_ext(imm, 12);
    let shamt  = get_rs2(inst) as u8;
    let funct7 = get_funct7(inst);

    match funct3 {
        FUNCT3_ADDI      => {
            info!("addi {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], simm);
            reg.x[rd] = ((reg.x[rs1] as i64) + simm) as u64;
        },
        FUNCT3_SLLI      => {
            info!("slli {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], shamt);
            reg.x[rd] = reg.x[rs1] << shamt;
        },
        FUNCT3_SLTI      => {
            info!("slti {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], simm);
            reg.x[rd] = ((reg.x[rs1] as i64) < (simm as i64)) as u64;
        },
        FUNCT3_SLTIU     => {
            info!("sltiu {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], simm as u64);
            reg.x[rd] = (reg.x[rs1] < (simm as u64)) as u64;
        },
        FUNCT3_XORI      => {
            info!("xori {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], simm);
            reg.x[rd] = reg.x[rs1] ^ (simm as u64)
        }
        FUNCT3_SRLI_SRAI => match funct7 {
            FUNCT7_SRLI  => {
                info!("srli {},{},{}",
                      ABI_NAME[rd], ABI_NAME[rs1], shamt);
                reg.x[rd] = reg.x[rs1] >> shamt;
            },
            FUNCT7_SRAI  => {
                info!("srai {},{},{}",
                      ABI_NAME[rd], ABI_NAME[rs1], shamt);
                reg.x[rd] = reg.x[rs1] >> shamt;
                reg.x[rd] = sign_ext(reg.x[rd], 64-shamt) as u64;
            },
            _ => warn!("{}: {}: unknown funct7 0x{:x}",
                          file!(), line!(), funct7)
        }
        FUNCT3_ORI       => {
            info!("ori {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], simm);
            reg.x[rd] = reg.x[rs1] | (simm as u64);
        },
        FUNCT3_ANDI      => {
            info!("andi {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], simm);
            reg.x[rd] = reg.x[rs1] & (simm as u64);
        },
        _ => warn!("{}: {}: unknown funct3 0x{:x}",
                      file!(), line!(), funct3)
    }
}

fn handle_auipc(reg: &mut RegisterFile, inst: u32) {
    let rd  = get_rd(inst) as usize;

    info!("auipc {},{}", ABI_NAME[rd], inst >> 12);
    reg.x[rd] = reg.pc + (inst & 0b11111111_11111111_11110000_00000000) as u64
}

fn handle_op_imm_32(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_long_op_48(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn mmio_region(addr: u64) -> bool {
    const UART_BASE: u64 = 0x80000000;
    addr >= UART_BASE
}

fn mmio_store(addr: u64, val: u64) {
    const UART_TXBUF: u64 = 0x80000000 + 0x08;
    if addr == UART_TXBUF {
        println!("@@@ {:?}", (val as u8) as char);
    }
}

fn handle_store(map: &mut Vec<u8>, reg: &RegisterFile, inst: u32) {
    const FUNCT3_SB  : u32 = 0b000;
    const FUNCT3_SH  : u32 = 0b001;
    const FUNCT3_SW  : u32 = 0b010;
    const FUNCT3_SD  : u32 = 0b011;

    let funct3 = get_funct3(inst);
    let rs1    = get_rs1(inst) as usize;
    let rs2    = get_rs2(inst) as usize;
    let imm   = ((get_funct7(inst) << 5) + get_rd(inst)) as u64;
    let offset = sign_ext(imm, 12);
    let addr   = (reg.x[rs1] as i64 + offset) as u64;

    debug!("store {} to 0x{:016x}", reg.x[rs2], addr);
    if mmio_region(addr) {
        mmio_store(addr, reg.x[rs2]);
    } else {
        match funct3 {
            FUNCT3_SB => {
                info!("sb {},{}({})",
                      ABI_NAME[rs2], offset, ABI_NAME[rs1]);
                map[addr as usize] = reg.x[rs2] as u8;
            },
            FUNCT3_SH => {
                info!("sh {},{}({})",
                      ABI_NAME[rs2], offset, ABI_NAME[rs1]);
                map[addr as usize] = (reg.x[rs2] & 0xff) as u8;
                map[(addr+1) as usize] = (reg.x[rs2] >> 8 & 0xff) as u8;
            },
            FUNCT3_SW => {
                info!("sw {},{}({})",
                      ABI_NAME[rs2], offset, ABI_NAME[rs1]);
                map[addr as usize] = (reg.x[rs2] & 0xff) as u8;
                map[(addr+1) as usize] = (reg.x[rs2] >> 8 & 0xff) as u8;
                map[(addr+2) as usize] = (reg.x[rs2] >> 16 & 0xff) as u8;
                map[(addr+3) as usize] = (reg.x[rs2] >> 24 & 0xff) as u8;
            },
            FUNCT3_SD => {
                info!("sd {},{}({})",
                      ABI_NAME[rs2], offset, ABI_NAME[rs1]);
                map[addr as usize] = (reg.x[rs2] & 0xff) as u8;
                map[(addr+1) as usize] = (reg.x[rs2] >> 8 & 0xff) as u8;
                map[(addr+2) as usize] = (reg.x[rs2] >> 16 & 0xff) as u8;
                map[(addr+3) as usize] = (reg.x[rs2] >> 24 & 0xff) as u8;
                map[(addr+4) as usize] = (reg.x[rs2] >> 32 & 0xff) as u8;
                map[(addr+5) as usize] = (reg.x[rs2] >> 40 & 0xff) as u8;
                map[(addr+6) as usize] = (reg.x[rs2] >> 48 & 0xff) as u8;
                map[(addr+7) as usize] = (reg.x[rs2] >> 56 & 0xff) as u8;
            },
            _ => warn!("{}: {}: unknown funct3 0x{:x}",
                          file!(), line!(), funct3)
        }
    }
}

fn handle_store_fp(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_custom_1(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_amo(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_op(reg: &mut RegisterFile, inst: u32) {
    const FUNCT3_ADD_SUB : u32 = 0b000;
    const FUNCT7_ADD     : u32 = 0b0000000;
    const FUNCT7_SUB     : u32 = 0b0100000;
    const FUNCT3_SLL     : u32 = 0b001;
    const FUNCT3_SLT     : u32 = 0b010;
    const FUNCT3_SLTU    : u32 = 0b011;
    const FUNCT3_XOR     : u32 = 0b100;
    const FUNCT3_SRL_SRA : u32 = 0b101;
    const FUNCT7_SRL     : u32 = 0b0000000;
    const FUNCT7_SRA     : u32 = 0b0100000;
    const FUNCT3_OR      : u32 = 0b110;
    const FUNCT3_AND     : u32 = 0b111;

    let funct3 = get_funct3(inst);
    let rd     = get_rd(inst) as usize;
    let rs1    = get_rs1(inst) as usize;
    let rs2    = get_rs2(inst) as usize;
    let funct7 = get_funct7(inst);

    match funct3 {
        FUNCT3_ADD_SUB => match funct7 {
            FUNCT7_ADD => {
                info!("add {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
                reg.x[rd] = reg.x[rs1] + reg.x[rs2];
            },
            FUNCT7_SUB => {
                info!("sub {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
                reg.x[rd] = reg.x[rs1] - reg.x[rs2];
            },
            _ => warn!("{}: {}: unknown funct7 0x{:x}",
                          file!(), line!(), funct7)
        },
        FUNCT3_SLL     => {
            info!("sll {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
            reg.x[rd] = reg.x[rs1] << reg.x[rs2];
        },
        FUNCT3_SLT     => {
            info!("slt {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
            reg.x[rd] = ((reg.x[rs1] as i64) < (reg.x[rs2] as i64)) as u64;
        },
        FUNCT3_SLTU    => {
            info!("slu {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
            reg.x[rd] = (reg.x[rs1] < reg.x[rs2]) as u64;
        },
        FUNCT3_XOR     => {
            info!("xor {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
            reg.x[rd] = reg.x[rs1] - reg.x[rs2];
        },
        FUNCT3_SRL_SRA => match funct7 {
            FUNCT7_SRL => {
                info!("srl {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
                reg.x[rd] = reg.x[rs1] - reg.x[rs2];
            },
            FUNCT7_SRA => {
                info!("sra {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
                reg.x[rd] = reg.x[rs1] - reg.x[rs2]; // TODO
            },
            _ => warn!("{}: {}: unknown funct7 0x{:x}",
                          file!(), line!(), funct7)
        },
        FUNCT3_OR      => {
            info!("or {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
            reg.x[rd] = reg.x[rs1] | reg.x[rs2];
        },
        FUNCT3_AND     => {
            info!("and {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
            reg.x[rd] = reg.x[rs1] & reg.x[rs2];
        },
        _ => warn!("{}: {}: unknown funct3 0x{:x}",
                      file!(), line!(), funct3)
    }
}

fn handle_lui(reg: &mut RegisterFile, inst: u32) {
    let rd  = get_rd(inst) as usize;
    info!("lui {},0x{:x}", ABI_NAME[rd], inst >> 12);

    reg.x[rd] = (inst & 0b11111111_11111111_11110000_00000000) as u64
}

fn handle_op_32(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_long_op_64(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_madd(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_msub(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_nmsub(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_nmadd(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_op_fp(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_custom2(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_long_op_48_2(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_branch(reg: &mut RegisterFile, inst: u32) {
    const FUNCT3_BEQ:  u32 = 0b000;
    const FUNCT3_BNE:  u32 = 0b001;
    const FUNCT3_BLT:  u32 = 0b100;
    const FUNCT3_BGE:  u32 = 0b101;
    const FUNCT3_BLTU: u32 = 0b110;
    const FUNCT3_BGEU: u32 = 0b111;

    let funct3 = get_funct3(inst);
    let rs1    = get_rs1(inst) as usize;
    let rs2    = get_rs2(inst) as usize;
    let imm = ((((inst >> 31) & 0b1) << 12)
        | (((inst >> 7) & 0b1) << 11)
        | (((inst >> 25) & 0b11_1111) << 5)
        | (((inst >> 8) & 0b1111) << 1)) as u64;
    let offset = sign_ext(imm, 12);

    let mut jump = false;
    match funct3 {
        FUNCT3_BEQ  => {
            info!("beq {},{},{}",
                  ABI_NAME[rs1], ABI_NAME[rs2], offset);
            jump = reg.x[rs1] == reg.x[rs2];
        },
        FUNCT3_BNE  => {
            info!("bne {},{},{}",
                  ABI_NAME[rs1], ABI_NAME[rs2], offset);
            jump = reg.x[rs1] != reg.x[rs2];
        },
        FUNCT3_BLT  => {
            info!("blt {},{},{}",
                  ABI_NAME[rs1], ABI_NAME[rs2], offset);
            jump = reg.x[rs1] < reg.x[rs2]; // TODO : sign
        },
        FUNCT3_BGE  => {
            info!("bge {},{},{}",
                  ABI_NAME[rs1], ABI_NAME[rs2], offset);
            jump = reg.x[rs1] >= reg.x[rs2]; // TODO : sign
        },
        FUNCT3_BLTU => {
            info!("bltu {},{},{}",
                  ABI_NAME[rs1], ABI_NAME[rs2], offset);
            jump = reg.x[rs1] < reg.x[rs2];
        },
        FUNCT3_BGEU => {
            info!("bgeu {},{},{}",
                  ABI_NAME[rs1], ABI_NAME[rs2], offset);
            jump = reg.x[rs1] >= reg.x[rs2];
        },
        _ => warn!("{}: {}: unknown funct3 0x{:x}",
                      file!(), line!(), funct3)
    }

    if jump {
        reg.pc = ((reg.pc as i64) + offset) as u64;
        debug!("branch to 0x{:016x}", reg.pc);
        reg.pc -= 4; // increment after the handler.
    } else {
        debug!("not branch");
    }
}

fn handle_jalr(reg: &mut RegisterFile, inst: u32) {
    let rd     = get_rd(inst) as usize;
    let rs1    = get_rs1(inst) as usize;
    let imm  = get_imm12(inst) as u64;
    let offset: i64 = sign_ext(imm, 12);

    if rd != 0 {
        reg.x[rd] = reg.pc + 4;
    }
    reg.pc = (reg.x[rs1] as i64 + offset) as u64;
    info!("jalr {},{}({})", ABI_NAME[rd], offset, ABI_NAME[rs1]);
    debug!("jump and link register to 0x{:016x}", reg.pc);
    reg.pc -= 4; // increment after the handler.
}

fn handle_jal(reg: &mut RegisterFile, inst: u32) {
    let imm = ((((inst >> 31) & 0b1) << 20)
        | (((inst >> 12) & 0b1111_1111) << 12)
        | (((inst >> 20) & 0b1) << 11)
        | (((inst >> 21) & 0b11_1111_1111) << 1)) as u64;
    let rd = get_rd(inst) as usize;
    let offset: i64 = sign_ext(imm, 20);

    if rd != 0 {
        reg.x[rd] = reg.pc + 4;
    }
    reg.pc = (reg.pc as i64 + offset) as u64;
    info!("jal {},{:x}", ABI_NAME[rd], reg.pc);
    debug!("jump to 0x{:016x}", reg.pc);
    reg.pc -= 4; // increment after the handler.
}

fn handle_system(reg: &mut RegisterFile, inst: u32) {
    const FUNCT3_ECALL_EBREAK : u32 = 0b000;
    const FUNCT3_CSRRW        : u32 = 0b001;
    const FUNCT3_CSRRS        : u32 = 0b010;
    const FUNCT3_CSRRC        : u32 = 0b011;
    const FUNCT3_CSRRWI       : u32 = 0b101;
    const FUNCT3_CSRRSI       : u32 = 0b110;
    const FUNCT3_CSRRCI       : u32 = 0b111;
    const CSR_MEPC            : u32 = 0x341;

    let funct3 = get_funct3(inst);
    let rd     = get_rd(inst) as usize;
    let rs1    = get_rs1(inst) as usize;
    let csr    = get_imm12(inst);

    match funct3 {
        FUNCT3_ECALL_EBREAK => unimplemented!(),
        FUNCT3_CSRRW        => match csr {
            CSR_MEPC => {
                info!("csrrw {},mepc,{}", ABI_NAME[rd], ABI_NAME[rs1]);
                if rd != 0 {
                    reg.x[rd] = reg.csr.mepc;
                }
                reg.csr.mepc = reg.x[rs1];
            },
            _ => warn!("{}: {}: unknown csr 0x{:x}", file!(), line!(), csr)
        },
        FUNCT3_CSRRS        => unimplemented!(),
        FUNCT3_CSRRC        => unimplemented!(),
        FUNCT3_CSRRWI       => unimplemented!(),
        FUNCT3_CSRRSI       => unimplemented!(),
        FUNCT3_CSRRCI       => unimplemented!(),
        _ => warn!("{}: {}: unknown funct3 0x{:x}", file!(), line!(), funct3)
    }
}

fn handle_custom3(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}

fn handle_long_op_ge_80(_reg: &RegisterFile, _inst: u32) {
    warn!("{}: {}: Not implemented", file!(), line!());
}


fn print_usage(program: &str, opts: &Options) {
    let brief = format!("Usage: {} PROG [options]", program);
    print!("{}", opts.usage(&brief));
    process::exit(0);
}

fn parse_args() -> Args {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optopt("m", "memory",
                "Specify the amount of memory (default: 128 MB).",
                "MEMORY");
    opts.optopt("l", "log-level",
                "Specify the log level.
                 Ex: 'error' > 'warn' > 'info' > 'debug' > 'trace'",
                "LEVEL");
    opts.optflag("h", "help", "print this help menu");

    let matches = opts.parse(&args[1..])
        .unwrap_or_else(|f| panic!(f.to_string()));

    if matches.opt_present("h")
        || matches.free.is_empty() {
        print_usage(&program, &opts);
    }

    let log_level = match matches.opt_str("l") {
        Some(value) => value,
        None => String::from("warn")
    };

    let memory = match matches.opt_get::<usize>("m") {
        Ok(result) => match result {
            Some(value) => value,
            None => 128 * 1024 * 1024
        },
        Err(e) => {
            error!("memmap error {:?}", e);
            std::process::exit(-1);
        }
    };

    Args {
        prog: matches.free[0].clone(),
        log_level: log_level,
        memory: memory,
    }
}

fn main() {
    let args = parse_args();
    debug!("{:?}", args);

    let env = env_logger::Env::default()
        .filter_or(env_logger::DEFAULT_FILTER_ENV, args.log_level);
    env_logger::Builder::from_env(env)
        .default_format_timestamp(false)
        .init();

    let mut reg = RegisterFile::new();
    let map = get_memmap(&args.prog);
    let mut mem = map.to_vec();
    unsafe {
        mem.set_len(args.memory);
    }

    loop {
        let inst : u32 = fetch(&map, reg.pc as usize);
        info!("{:08x}: 0x{:08x}", reg.pc, inst);

        let opcode = get_opcode(inst);
        match opcode {
            LOAD          => handle_load(&mem, &mut reg, inst),
            LOAD_FP       => handle_load_fp(&mut reg, inst),
            CUSTOM_0      => handle_custom_0(&mut reg, inst),
            MISC_MEM      => handle_misc_mem(&mut reg, inst),
            OP_IMM        => handle_op_imm(&mut reg, inst),
            AUIPC         => handle_auipc(&mut reg, inst),
            OP_IMM_32     => handle_op_imm_32(&mut reg, inst),
            LONG_OP_48    => handle_long_op_48(&mut reg, inst),
            STORE         => handle_store(&mut mem, &mut reg, inst),
            STORE_FP      => handle_store_fp(&mut reg, inst),
            CUSTOM_1      => handle_custom_1(&mut reg, inst),
            AMO           => handle_amo(&mut reg, inst),
            OP            => handle_op(&mut reg, inst),
            LUI           => handle_lui(&mut reg, inst),
            OP_32         => handle_op_32(&mut reg, inst),
            LONG_OP_64    => handle_long_op_64(&mut reg, inst),
            MADD          => handle_madd(&mut reg, inst),
            MSUB          => handle_msub(&mut reg, inst),
            NMSUB         => handle_nmsub(&mut reg, inst),
            NMADD         => handle_nmadd(&mut reg, inst),
            OP_FP         => handle_op_fp(&mut reg, inst),
            CUSTOM2       => handle_custom2(&mut reg, inst),
            LONG_OP_48_2  => handle_long_op_48_2(&mut reg, inst),
            BRANCH        => handle_branch(&mut reg, inst),
            JALR          => handle_jalr(&mut reg, inst),
            JAL           => handle_jal(&mut reg, inst),
            SYSTEM        => handle_system(&mut reg, inst),
            CUSTOM3       => handle_custom3(&mut reg, inst),
            LONG_OP_GE_80 => handle_long_op_ge_80(&mut reg, inst),
            _ => warn!("{}: {}: unknown opcode 0x{:x}",
                          file!(), line!(), opcode)
        }

        reg.pc += 4;

        reg.dump();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn inst_i(_imm:u16, _rs1:u8, _funct3:u8, _rsd:u8, _opcode:u8) -> u32 {
        let imm    = _imm & ((1 << 12) - 1);
        let rs1    = _rs1 & ((1 <<  5) - 1);
        let funct3 = _funct3 & ((1 <<  3) - 1);
        let rsd    = _rsd & ((1 <<  5) - 1);
        let opcode = _opcode & ((1 <<  7) - 1);
        return ((imm as u32) << (5 + 3 + 5 + 7))
            | ((rs1 as u32) << (3 + 5 + 7))
            | ((funct3 as u32) << (5 + 7))
            | ((rsd as u32) << 7)
            | (opcode as u32);
    }

    #[test]
    fn test_lh() {
        // load positive harf-word (16bit)
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(4, 1, 0b001, 2, 0b0000011);
        let mem: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 6, 7];
        reg.x[1] = 0x2;
        handle_load(&mem, &mut reg, inst);
        assert_eq!(0x0706, reg.x[2]);
    }

    #[test]
    fn test_lh_neg() {
        // load negative harf-word (16bit)
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(4, 1, 0b001, 2, 0b0000011);
        let mem: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 0xfe, 0xff];
        reg.x[1] = 0x2;
        handle_load(&mem, &mut reg, inst);
        assert_eq!(-2i64 as u64, reg.x[2]);
    }

    #[test]
    fn test_addi() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(7, 1, 0b000, 2, 0b0010011);
        reg.x[1] = 0b001;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0b1000, reg.x[2]);
    }

    #[test]
    fn test_addi_neg() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(-4i16 as u16, 1, 0b000, 2, 0b0010011);
        reg.x[1] = 0b001;
        handle_op_imm(&mut reg, inst);
        assert_eq!(-3i64 as u64, reg.x[2]);
    }

    #[test]
    fn test_slli() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(7, 1, 0b001, 2, 0b0010011);
        reg.x[1] = 0b001;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0b10000000, reg.x[2]);
    }

    #[test]
    fn test_slti() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(7, 1, 0b010, 2, 0b0010011);
        reg.x[1] = 0b111;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0, reg.x[2]);

        reg.x[1] = 0b110;
        handle_op_imm(&mut reg, inst);
        assert_eq!(1, reg.x[2]);

        reg.x[1] = 0xffff_ffff_ffff_ffff;
        handle_op_imm(&mut reg, inst);
        assert_eq!(1, reg.x[2]);
    }

    #[test]
    fn test_sltiu() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(7, 1, 0b011, 2, 0b0010011);
        reg.x[1] = 0b111;
        handle_op_imm(&mut reg, inst);
        assert_eq!(reg.x[2], 0);

        reg.x[1] = 0b110;
        handle_op_imm(&mut reg, inst);
        assert_eq!(reg.x[2], 1);

        reg.x[1] = 0xffff_ffff_ffff_ffff;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0, reg.x[2]);
    }

    #[test]
    fn test_xori_pos() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(0b001010101010, 1, 0b100, 2, 0b0010011);
        reg.x[1] = 0b111111111111;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0b110101010101, reg.x[2],
                   "0x{:016x}", reg.x[2]);
    }

    #[test]
    fn test_xori_neg() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(0b101010101010, 1, 0b100, 2, 0b0010011);
        reg.x[1] = 0b111111111111;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0xffff_ffff_ffff_f555, reg.x[2],
                   "0x{:016x}", reg.x[2]);
    }

    #[test]
    fn test_srli() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(0b0000000_00100, 1, 0b101, 2, 0b0010011);
        reg.x[1] = 0xffff_ffff_ffff_ffff;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0x0fff_ffff_ffff_ffff, reg.x[2],
                   "0x{:016x}", reg.x[2]);
    }

    #[test]
    fn test_srai() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(0b0100000_00100, 1, 0b101, 2, 0b0010011);
        reg.x[1] = 0xffff_ffff_ffff_ffff;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0xffff_ffff_ffff_ffff, reg.x[2],
                   "0x{:016x}", reg.x[2]);
    }

    #[test]
    fn test_ori_pos() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(0b0010101_01010, 1, 0b110, 2, 0b0010011);
        reg.x[1] = 0b010111000000;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0b011111101010, reg.x[2],
                   "0b{:b}", reg.x[2]);
    }

    #[test]
    fn test_ori_neg() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(0b1010101_01010, 1, 0b110, 2, 0b0010011);
        reg.x[1] = 0b010111000000;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0xffff_ffff_ffff_ffea, reg.x[2],
                   "0x{:016x}", reg.x[2]);
    }

    #[test]
    fn test_andi() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(0b1010101_01010, 1, 0b111, 2, 0b0010011);
        reg.x[1] = 0b010101111111;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0b000000101010, reg.x[2], "{:b}", reg.x[2]);
    }
}
