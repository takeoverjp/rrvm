#[macro_use]
extern crate log;
extern crate env_logger;
extern crate getopts;
extern crate memmap;
extern crate hex;

mod inst;
mod csr;
mod elf;
mod memory;

use inst::*;
use csr::*;
use elf::*;
use memory::*;

use std::{env, process};
use std::io::Write;
use std::fs::File;
use getopts::Options;
use memmap::MmapOptions;

const XLEN: u32 = 64;

#[derive(Debug)]
struct Args {
    prog: String,
    log_level: String,
    log_spike: bool,
    memory: usize,
    offset: u64,
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
            csr: ControlStatusRegister::new(),
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

fn get_memmap(file_path: &str) -> memmap::Mmap {
    let file = match File::open(file_path) {
        Ok(file) => file,
        Err(e) => {
            writeln!(std::io::stderr(), "file({}) open error {:?}", file_path, e).unwrap();
            std::process::exit(1);
        }
    };

    let mmap = unsafe {
        match MmapOptions::new().map(&file) {
            Ok(map) => map,
            Err(e) => {
                writeln!(std::io::stderr(), "memmap error {:?}", e).unwrap();
                std::process::exit(1);
            }
        }
    };

    mmap
}

fn sign_ext(val: u64, size: u8) -> i64 {
    if size == XLEN as u8 {
        return val as i64;
    }

    let mut ret = val as i64;

    let sign_mask = if size == 1 {
        1
    } else {
        1 << (size - 1)
    };

    let mask: i64 = 1 << (64 - size);
    let mask = mask.wrapping_sub(1) << size;
    if (val & sign_mask) != 0 {
        ret |= mask;
    } else {
        ret &= !mask;
    }

    ret
}

#[test]
fn test_sign_ext() {
    assert_eq!(-1, sign_ext(0xf, 4));
    assert_eq!(7, sign_ext(0x7, 4));
    assert_eq!(7, sign_ext(0xf007, 4));
    assert_eq!(-1, sign_ext(0xff, 8));
    assert_eq!(0x7f, sign_ext(0x7f, 8));
    assert_eq!(0x7f, sign_ext(0xf07f, 8));
}

fn handle_load(mem: &Memory, reg: &mut RegisterFile, inst: u32) {
    let funct3 = get_funct3(inst);
    let rd     = get_rd(inst) as usize;
    let rs1    = get_rs1(inst) as usize;
    let imm  = get_imm12(inst);
    let offset = sign_ext(imm as u64, 12);
    let addr   = (reg.x[rs1] as i64 + offset) as u64;

    if mmio_region(addr) {
        unimplemented!();
    } else {
        match funct3 {
            FUNCT3_LB  => {
                info!("lb {},{}({})",
                      ABI_NAME[rd], offset, ABI_NAME[rs1]);
                reg.x[rd] = sign_ext(mem.lb(addr) as u64, 8) as u64;
            },
            FUNCT3_LH  => {
                info!("lh {},{}({})",
                      ABI_NAME[rd], offset, ABI_NAME[rs1]);
                reg.x[rd] = sign_ext(mem.lh(addr) as u64, 16) as u64;
            },
            FUNCT3_LW  => {
                info!("lw {},{}({})",
                      ABI_NAME[rd], offset, ABI_NAME[rs1]);
                reg.x[rd] = sign_ext(mem.lw(addr) as u64, 32) as u64;
            },
            FUNCT3_LD  => {
                info!("ld {},{}({})",
                      ABI_NAME[rd], offset, ABI_NAME[rs1]);
                reg.x[rd] = mem.ld(addr);
            },
            FUNCT3_LBU => {
                info!("lbu {},{}({})",
                      ABI_NAME[rd], offset, ABI_NAME[rs1]);
                reg.x[rd] = mem.lb(addr) as u64;
            },
            FUNCT3_LHU => {
                info!("lhu {},{}({})",
                      ABI_NAME[rd], offset, ABI_NAME[rs1]);
                reg.x[rd] = mem.lh(addr) as u64;
            },
            FUNCT3_LWU => {
                info!("lwu {},{}({})",
                      ABI_NAME[rd], offset, ABI_NAME[rs1]);
                reg.x[rd] = mem.lw(addr) as u64
            },
            _ => warn!("{}: {}: unknown funct3 0x{:x}",
                          file!(), line!(), funct3)
        }
    }
    debug!("load from 0x{:016x}, val = 0x{:016x}", addr, reg.x[rd]);
}

fn handle_load_fp(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_custom_0(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_misc_mem(_reg: &RegisterFile, inst: u32) {
    let funct3 = get_funct3(inst);

    match funct3 {
        FUNCT3_FENCE => {
            // nothing to do
            let pred = get_fence_pred(inst);
            let succ = get_fence_succ(inst);
            info!("fence pred={:b} succ={:b}", pred, succ);
        },
        FUNCT3_FENCE_I => {
            // nothing to do
            info!("fence.i");
        },
        _ => unimplemented!(),
    }
}

fn handle_op_imm(reg: &mut RegisterFile, inst: u32) {
    let funct3 = get_funct3(inst);
    let rd     = get_rd(inst) as usize;
    let rs1    = get_rs1(inst) as usize;
    let imm  = get_imm12(inst) as u64;
    let simm = sign_ext(imm, 12);
    let shamt  = get_shamt(inst) as u8;

    match funct3 {
        FUNCT3_ADDI      => {
            info!("addi {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], simm);
            reg.x[rd] = (reg.x[rs1] as i64).wrapping_add(simm) as u64;
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
        },
        FUNCT3_SRLI_SRAI => {
            if inst & (1 << 30) == 0 {
                info!("srli {},{},{}",
                      ABI_NAME[rd], ABI_NAME[rs1], shamt);
                reg.x[rd] = reg.x[rs1] >> shamt;
            } else {
                info!("srai {},{},{}",
                      ABI_NAME[rd], ABI_NAME[rs1], shamt);
                reg.x[rd] = reg.x[rs1] >> shamt;
                reg.x[rd] = sign_ext(reg.x[rd], 64-shamt) as u64;
            }
        },
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
    let imm = sign_ext((inst & 0b11111111_11111111_11110000_00000000) as u64, 32);

    info!("auipc {},{}", ABI_NAME[rd], inst >> 12);
    reg.x[rd] = (reg.pc as i64 + imm) as u64;
}

fn handle_op_imm_32(reg: &mut RegisterFile, inst: u32) {
    let funct3 = get_funct3(inst);
    let rd     = get_rd(inst) as usize;
    let rs1    = get_rs1(inst) as usize;
    let rs1w   = reg.x[rs1] as u32;
    let imm  = get_imm12(inst) as u64;
    let simm = sign_ext(imm, 12);
    let shamt  = (get_shamt(inst) & 0b11111) as u8;

    match funct3 {
        FUNCT3_ADDIW       => {
            info!("addiw {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], simm);
            let result64 = (reg.x[rs1] as i64 + simm) as u64;
            reg.x[rd] = sign_ext(result64, 32) as u64;
        },
        FUNCT3_SLLIW       => {
            info!("slliw {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], shamt);
            reg.x[rd] = sign_ext((reg.x[rs1] << shamt) as u64, 32) as u64;
        },
        FUNCT3_SRLIW_SRAIW => {
            if inst & (1 << 30) == 0 {
                info!("srliw {},{},{}",
                      ABI_NAME[rd], ABI_NAME[rs1], shamt);
                reg.x[rd] = sign_ext((rs1w >> shamt) as u64, 32) as u64;
            } else {
                info!("sraiw {},{},{}",
                      ABI_NAME[rd], ABI_NAME[rs1], shamt);
                reg.x[rd] = sign_ext((rs1w >> shamt) as u64, 32-shamt) as u64;
            }
        },
        _ => warn!("{}: {}: unknown funct3 0x{:x}",
                      file!(), line!(), funct3)
    }
}

fn handle_long_op_48(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

const RISCV_TESTS_TOHOST: u64 = 0x80001000;
fn mmio_region(addr: u64) -> bool {
    addr == RISCV_TESTS_TOHOST
}

fn mmio_store(addr: u64, val: u64) {
    match addr {
        RISCV_TESTS_TOHOST => {
            if val == 1 {
                writeln!(std::io::stderr(), "@@@ riscv-tests: success").unwrap();
                std::process::exit(0);
            } else {
                writeln!(std::io::stderr(), "@@@ riscv-tests: failed {:?}", val >> 1).unwrap();
                std::process::exit(1);
            }
        },
        _ => unimplemented!(),
    }
}

fn handle_store(mem: &mut Memory, reg: &RegisterFile, inst: u32) {
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
                mem.sb(addr, reg.x[rs2] as u8);
            },
            FUNCT3_SH => {
                info!("sh {},{}({})",
                      ABI_NAME[rs2], offset, ABI_NAME[rs1]);
                mem.sh(addr, reg.x[rs2] as u16);
            },
            FUNCT3_SW => {
                info!("sw {},{}({})",
                      ABI_NAME[rs2], offset, ABI_NAME[rs1]);
                mem.sw(addr, reg.x[rs2] as u32);
            },
            FUNCT3_SD => {
                info!("sd {},{}({})",
                      ABI_NAME[rs2], offset, ABI_NAME[rs1]);
                mem.sd(addr, reg.x[rs2] as u64);
            },
            _ => warn!("{}: {}: unknown funct3 0x{:x}",
                          file!(), line!(), funct3)
        }
    }
}

fn handle_store_fp(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_custom_1(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_amo(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_muldiv(reg: &mut RegisterFile, funct3: u32, rd: usize, rs1: usize, rs2: usize) {
    match funct3 {
        FUNCT3_MUL => {
            info!("mul {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
            reg.x[rd] = reg.x[rs1].wrapping_mul(reg.x[rs2]);
        },
        FUNCT3_MULH => {
            info!("mulh {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
            let tmp = ((reg.x[rs1] as i64) as i128).wrapping_mul((reg.x[rs2] as i64) as i128);
            reg.x[rd] = (tmp >> XLEN) as u64;
        },
        FUNCT3_MULHSU => {
            info!("mulhsu {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
            let tmp = ((reg.x[rs1] as i64) as i128).wrapping_mul((reg.x[rs2] as u64) as i128);
            reg.x[rd] = (tmp >> XLEN) as u64;
        },
        FUNCT3_MULHU => {
            info!("mulhu {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
            let tmp = ((reg.x[rs1] as u64) as i128).wrapping_mul((reg.x[rs2] as u64) as i128);
            reg.x[rd] = (tmp >> XLEN) as u64;
        },
        _ => warn!("{}: {}: unknown funct3 0x{:x}",
                   file!(), line!(), funct3)
    }
}

fn handle_op(reg: &mut RegisterFile, inst: u32) {
    const SHIFT_MASK     : u64 = 0b111111;

    let funct3 = get_funct3(inst);
    let rd     = get_rd(inst) as usize;
    let rs1    = get_rs1(inst) as usize;
    let rs2    = get_rs2(inst) as usize;
    let funct7 = get_funct7(inst);
    let shamt = (reg.x[rs2] & SHIFT_MASK) as u8;

    if funct7 == FUNCT7_MULDIV {
        return handle_muldiv(reg, funct3, rd, rs1, rs2);
    }

    match funct3 {
        FUNCT3_ADD_SUB => match funct7 {
            FUNCT7_ADD => {
                info!("add {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
                reg.x[rd] = reg.x[rs1].wrapping_add(reg.x[rs2]);
            },
            FUNCT7_SUB => {
                info!("sub {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
                reg.x[rd] = reg.x[rs1].wrapping_sub(reg.x[rs2]);
            },
            _ => warn!("{}: {}: unknown funct7 0x{:x}",
                          file!(), line!(), funct7)
        },
        FUNCT3_SLL     => {
            info!("sll {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
            reg.x[rd] = reg.x[rs1] << shamt;
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
            reg.x[rd] = reg.x[rs1] ^ reg.x[rs2];
        },
        FUNCT3_SRL_SRA => match funct7 {
            FUNCT7_SRL => {
                info!("srl {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
                reg.x[rd] = reg.x[rs1] >> shamt;
            },
            FUNCT7_SRA => {
                info!("sra {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
                reg.x[rd] = reg.x[rs1] >> shamt;
                reg.x[rd] = sign_ext(reg.x[rd], 64-shamt) as u64;
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
    let imm = sign_ext((inst & 0b11111111_11111111_11110000_00000000) as u64, 32);

    info!("lui {},0x{:x}", ABI_NAME[rd], inst >> 12);
    reg.x[rd] = imm as u64;
}

fn handle_op_32(reg: &mut RegisterFile, inst: u32) {
    const SHIFT_MASK       : u32 = 0b11111;

    let funct3 = get_funct3(inst);
    let rd     = get_rd(inst) as usize;
    let rs1    = get_rs1(inst) as usize;
    let rs1w   = reg.x[rs1] as u32;
    let rs2    = get_rs2(inst) as usize;
    let rs2w   = reg.x[rs2] as u32;
    let shamt = (rs2w & SHIFT_MASK) as u8;
    let funct7 = get_funct7(inst);

    match funct3 {
        FUNCT3_ADDW_SUBW => match funct7 {
            FUNCT7_ADDW => {
                info!("addw {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
                reg.x[rd] = sign_ext(rs1w.wrapping_add(rs2w) as u64, 32) as u64;
            },
            FUNCT7_SUBW => {
                info!("subw {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
                reg.x[rd] = sign_ext(rs1w.wrapping_sub(rs2w) as u64, 32) as u64;
            },
            FUNCT7_MULW => {
                info!("mulw {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
                reg.x[rd] = sign_ext(rs1w.wrapping_mul(rs2w) as u64, 32) as u64;
            },
            _ => warn!("{}: {}: unknown funct7 0x{:x}",
                          file!(), line!(), funct7)
        },
        FUNCT3_SLLW     => {
            info!("sllw {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
            reg.x[rd] = sign_ext((rs1w << shamt) as u64, 32) as u64;
        },
        FUNCT3_SRLW_SRAW => match funct7 {
            FUNCT7_SRLW => {
                info!("srlw {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
                reg.x[rd] = sign_ext((rs1w >> shamt) as u64, 32) as u64;
            },
            FUNCT7_SRAW => {
                info!("sraw {},{},{}", ABI_NAME[rd], ABI_NAME[rs1], ABI_NAME[rs2]);
                reg.x[rd] = sign_ext((rs1w >> shamt) as u64, 32 - shamt) as u64;
            },
            _ => warn!("{}: {}: unknown funct7 0x{:x}",
                          file!(), line!(), funct7)
        },
        _ => warn!("{}: {}: unknown funct3 0x{:x}",
                      file!(), line!(), funct3)
    }
}

fn handle_long_op_64(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_madd(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_msub(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_nmsub(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_nmadd(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_op_fp(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_custom2(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_long_op_48_2(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_branch(reg: &mut RegisterFile, inst: u32) {
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
            jump = (reg.x[rs1] as i64) < (reg.x[rs2] as i64);
        },
        FUNCT3_BGE  => {
            info!("bge {},{},{}",
                  ABI_NAME[rs1], ABI_NAME[rs2], offset);
            jump = (reg.x[rs1] as i64) >= (reg.x[rs2] as i64);
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
    const MCAUSE_BREAK        : u64 = (0 << (XLEN -1)) |  3u64;
    const MCAUSE_USER_ECALL         : u64 = (0 << (XLEN -1)) |  8u64;
    const MCAUSE_SUPERVISOR_ECALL   : u64 = (0 << (XLEN -1)) |  9u64;
    const MCAUSE_MACHINE_ECALL      : u64 = (0 << (XLEN -1)) | 11u64;

    const MPP_USER       : u8 = 0b00;
    const MPP_SUPERVISOR : u8 = 0b10;
    const MPP_MACHINE    : u8 = 0b11;

    let funct3 = get_funct3(inst);
    let rd     = get_rd(inst) as usize;
    let rs1    = get_rs1(inst) as usize;
    let csr_addr = get_imm12(inst);
    let csr_val = reg.csr.get(csr_addr);
    let csr_name = ControlStatusRegister::get_name(csr_addr);
    let mpp = reg.csr.get_mpp();

    match funct3 {
        FUNCT3_ECALL_EBREAK => {
            const IMM12_ECALL  : u32 = 0b0000_0000_0000;
            const IMM12_EBREAK : u32 = 0b0000_0000_0001;
            const IMM12_MRET   : u32 = 0b0011_0000_0010;

            let imm12 = get_imm12(inst);

            match imm12 {
                IMM12_ECALL  => {
                    info!("ecall");
                    let mcause = match mpp {
                        MPP_USER       => MCAUSE_USER_ECALL,
                        MPP_SUPERVISOR => MCAUSE_SUPERVISOR_ECALL,
                        MPP_MACHINE    => MCAUSE_MACHINE_ECALL,
                        _ => unimplemented!(),
                    };
                    reg.csr.set(CSR_MCAUSE, mcause);
                    reg.csr.set(CSR_MEPC, reg.pc);
                    reg.pc = reg.csr.get(CSR_MTVEC);
                    reg.pc -= 4; // increment after the handler.
                },
                IMM12_EBREAK => {
                    info!("ebreak");
                    reg.csr.set(CSR_MCAUSE, MCAUSE_BREAK);
                    reg.csr.set(CSR_MEPC, reg.pc);
                    reg.pc = reg.csr.get(CSR_MTVEC);
                    reg.pc -= 4; // increment after the handler.
                },
                IMM12_MRET   => {
                    info!("mret");
                    reg.pc = reg.csr.get(CSR_MEPC);
                    debug!("mret jumps to 0x{:016x}", reg.pc);
                    reg.pc -= 4; // increment after the handler.
                }
                _ => unimplemented!(),
            }
        },
        FUNCT3_CSRRW        => {
            info!("csrrw {},{},{}", ABI_NAME[rd], csr_name, ABI_NAME[rs1]);
            let new_val = reg.x[rs1];
            reg.csr.set(csr_addr, new_val);
            if rd != 0 {
                reg.x[rd] = csr_val;
            }
        },
        FUNCT3_CSRRS        => {
            info!("csrrs {},{},{}", ABI_NAME[rd], csr_name, ABI_NAME[rs1]);
            let new_val = csr_val | reg.x[rs1];
            reg.csr.set(csr_addr, new_val);
            if rd != 0 {
                reg.x[rd] = csr_val;
            }
        },
        FUNCT3_CSRRC        => unimplemented!(),
        FUNCT3_CSRRWI       => {
            info!("csrrwi {},{},{}", ABI_NAME[rd], csr_name, ABI_NAME[rs1]);
            if rd != 0 {
                reg.x[rd] = csr_val;
            }
            let new_val = rs1 as u64;
            reg.csr.set(csr_addr, new_val);
        }
        FUNCT3_CSRRSI       => unimplemented!(),
        FUNCT3_CSRRCI       => unimplemented!(),
        _ => warn!("{}: {}: unknown funct3 0x{:x}", file!(), line!(), funct3)
    }
}

fn handle_custom3(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}

fn handle_long_op_ge_80(_reg: &RegisterFile, _inst: u32) {
    unimplemented!();
}


fn print_usage(program: &str, opts: &Options) {
    let brief = format!("Usage: {} PROG [options]", program);
    print!("{}", opts.usage(&brief));
    process::exit(0);
}

fn str2u64(numstr: &str) -> u64 {
    let fmt = &numstr[0..2];
    let len = numstr.len();
    if fmt == "0x" {
        let numstr = &numstr[2..len];
        match hex::decode(numstr) {
            Ok(result) => {
                let len = result.len();
                let mut offset = 0;
                for (i, b) in result.into_iter().enumerate() {
                    offset |= (b as u64) << ((len - i - 1) * 8);
                }
                return offset;
            }
            Err(e) => {
                writeln!(std::io::stderr(), "decode {:?} failed: {:?}", numstr, e).unwrap();
                std::process::exit(1);
            }
        };
    } else {
        return numstr.parse().unwrap();
    }
}

fn parse_args() -> Args {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optopt("m", "memory",
                "Specify the amount of memory (default: 128 MB).",
                "MEMORY");
    opts.optopt("o", "offset",
                "Specify text start address (default: 0x0)",
                "OFFSET");
    opts.optopt("l", "log-level",
                "Specify the log level.
                 Ex: 'error' > 'warn' > 'info' > 'debug' > 'trace'",
                "LEVEL");
    opts.optflag("s", "log-spike",
                "Output the spike trace log to compare.");
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

    let log_spike = matches.opt_present("s");

    let memory = match matches.opt_str("m") {
        Some(value) => value,
        None => String::from("0x08000000"), // 128MB
    };
    let memory = str2u64(&memory) as usize;

    let offset = match matches.opt_str("o") {
        Some(value) => value,
        None => String::from("0x00000000")
    };
    let offset = str2u64(&offset);

    Args {
        prog: matches.free[0].clone(),
        log_level: log_level,
        log_spike: log_spike,
        memory: memory,
        offset: offset,
    }
}

fn decompress(c_inst: u16) -> u32 {
    println!("c_inst = 0x{:04x}", c_inst);

    0xffffffff
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
    let mut vec = map.to_vec();

    let elf = Elf::new(&vec);
    reg.pc = if elf.is_elf() {
        elf.entry_point_address()
    } else {
        args.offset
    };
    info!("entry_point_address = 0x{:x}", reg.pc);

    let mut mem = Memory::new(&mut vec, &elf);

    loop {
        let is_comp = (mem.lb(reg.pc) & 0b11) != 0b11;
        if is_comp {
            let c_inst = mem.lh(reg.pc);
            let inst = decompress(c_inst);
            info!("{:08x}: 0x{:04x} -> 0x{:08x} (compressed)", reg.pc, c_inst, inst);
            unimplemented!();
        }

        let inst : u32 = mem.lw(reg.pc);
        info!("{:08x}: 0x{:08x}", reg.pc, inst);
        if args.log_spike {
            println!("core   0: 0x{:016x} (0x{:08x}", reg.pc, inst);
        }

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
        reg.x[0] = 0;

        reg.dump();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let mut reg = RegisterFile::new();
        // add r3, r1, r2
        let inst: u32 = inst_add(3, 1, 2);
        reg.x[1] = 2;
        reg.x[2] = 3;

        handle_op(&mut reg, inst);

        assert_eq!(reg.x[1] + reg.x[2], reg.x[3]);
        assert_eq!(2, reg.x[1]);
        assert_eq!(3, reg.x[2]);
    }

    #[test]
    fn test_sub() {
        let mut reg = RegisterFile::new();
        // sub r3, r1, r2
        let inst: u32 = inst_sub(3, 1, 2);
        reg.x[1] = 3;
        reg.x[2] = 2;

        handle_op(&mut reg, inst);

        assert_eq!(reg.x[1] - reg.x[2], reg.x[3]);
        assert_eq!(3, reg.x[1]);
        assert_eq!(2, reg.x[2]);
    }

    #[test]
    fn test_lb() {
        let mut reg = RegisterFile::new();
        let mut vec: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 6, 7];
        let elf = Elf::new(&vec);
        let mem = Memory::new(&mut vec, &elf);
        // lb r2, 4(r1)
        let inst: u32 = inst_lb(2, 4, 1);
        reg.x[1] = 0x2;

        handle_load(&mem, &mut reg, inst);

        assert_eq!(0x06, reg.x[2]);
    }

    #[test]
    fn test_lb_negative_value() {
        let mut reg = RegisterFile::new();
        let mut vec: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 0xfe, 7];
        let elf = Elf::new(&vec);
        let mem = Memory::new(&mut vec, &elf);
        // lb r2, 4(r1)
        let inst: u32 = inst_lb(2, 4, 1);
        reg.x[1] = 0x2;

        handle_load(&mem, &mut reg, inst);

        assert_eq!(-2i64 as u64, reg.x[2]);
    }

    #[test]
    fn test_lb_negative_offset() {
        let mut reg = RegisterFile::new();
        let mut vec: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 6, 7];
        let elf = Elf::new(&vec);
        let mem = Memory::new(&mut vec, &elf);
        // lb r2, -4(r1)
        let inst: u32 = inst_lb(2, -4i16 as u16, 1);
        reg.x[1] = 10;

        handle_load(&mem, &mut reg, inst);

        assert_eq!(6, reg.x[2]);
    }

    #[test]
    fn test_lh() {
        let mut reg = RegisterFile::new();
        let mut vec: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 6, 7];
        let elf = Elf::new(&vec);
        let mem = Memory::new(&mut vec, &elf);
        // lh r2, 4(r1)
        let inst: u32 = inst_lh(2, 4, 1);
        reg.x[1] = 0x2;

        handle_load(&mem, &mut reg, inst);

        assert_eq!(0x0706, reg.x[2]);
    }

    #[test]
    fn test_lh_negative_value() {
        let mut reg = RegisterFile::new();
        let mut vec: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 0xfe, 0xff];
        let elf = Elf::new(&vec);
        let mem = Memory::new(&mut vec, &elf);
        // lh r2, 4(r1)
        let inst: u32 = inst_lh(2, 4, 1);
        reg.x[1] = 0x2;

        handle_load(&mem, &mut reg, inst);

        assert_eq!(-2i64 as u64, reg.x[2]);
    }

    #[test]
    fn test_lh_negative_offset() {
        let mut reg = RegisterFile::new();
        let mut vec: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 6, 7];
        let elf = Elf::new(&vec);
        let mem = Memory::new(&mut vec, &elf);
        // lh r2, 4(r1)
        let inst: u32 = inst_lh(2, -4i16 as u16, 1);
        reg.x[1] = 10;
        handle_load(&mem, &mut reg, inst);
        assert_eq!(0x0706, reg.x[2]);
    }

    #[test]
    fn test_addi() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_addi(2, 1, 7);
        reg.x[1] = 0b001;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0b1000, reg.x[2]);
    }

    #[test]
    fn test_addi_neg() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_addi(2, 1, -4i16 as u16);
        reg.x[1] = 0b001;
        handle_op_imm(&mut reg, inst);
        assert_eq!(-3i64 as u64, reg.x[2]);
    }

    #[test]
    fn test_slli() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i_shamt(0b000000, 7, 1, FUNCT3_SLLI, 2, OP_IMM);
        reg.x[1] = 1;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0b10000000, reg.x[2]);
    }

    #[test]
    fn test_slli_6bit() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i_shamt(0b000000, 32, 1, FUNCT3_SLLI, 2, OP_IMM);
        reg.x[1] = 1;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0x1_0000_0000, reg.x[2]);
    }

    #[test]
    fn test_slti() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(7, 1, FUNCT3_SLTI, 2, OP_IMM);
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
        let inst: u32 = inst_i(7, 1, FUNCT3_SLTIU, 2, OP_IMM);
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
        let inst: u32 = inst_i(0b001010101010, 1, FUNCT3_XORI, 2, OP_IMM);
        reg.x[1] = 0b111111111111;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0b110101010101, reg.x[2],
                   "0x{:016x}", reg.x[2]);
    }

    #[test]
    fn test_xori_neg() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(0b101010101010, 1, FUNCT3_XORI, 2, OP_IMM);
        reg.x[1] = 0b111111111111;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0xffff_ffff_ffff_f555, reg.x[2],
                   "0x{:016x}", reg.x[2]);
    }

    #[test]
    fn test_srli() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i_shamt(0b000000, 4, 1, FUNCT3_SRLI_SRAI, 2, OP_IMM);
        reg.x[1] = 0xffff_ffff_ffff_ffff;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0x0fff_ffff_ffff_ffff, reg.x[2],
                   "0x{:016x}", reg.x[2]);
    }

    #[test]
    fn test_srli_6bit() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i_shamt(0b000000, 32, 1, FUNCT3_SRLI_SRAI, 2, OP_IMM);
        reg.x[1] = 0x123_0000_0000;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0x123, reg.x[2],
                   "0x{:016x}", reg.x[2]);
    }

    #[test]
    fn test_srai() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i_shamt(0b010000, 4, 1, FUNCT3_SRLI_SRAI, 2, OP_IMM);
        reg.x[1] = 0xffff_ffff_ffff_ffff;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0xffff_ffff_ffff_ffff, reg.x[2],
                   "0x{:016x}", reg.x[2]);
    }

    #[test]
    fn test_srai_6bit() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i_shamt(0b010000, 32, 1, FUNCT3_SRLI_SRAI, 2, OP_IMM);
        reg.x[1] = 0x123_0000_0000;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0x123, reg.x[2],
                   "0x{:016x}", reg.x[2]);
    }

    #[test]
    fn test_ori_pos() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(0b0010101_01010, 1, FUNCT3_ORI, 2, OP_IMM);
        reg.x[1] = 0b010111000000;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0b011111101010, reg.x[2],
                   "0b{:b}", reg.x[2]);
    }

    #[test]
    fn test_ori_neg() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(0b1010101_01010, 1, FUNCT3_ORI, 2, OP_IMM);
        reg.x[1] = 0b010111000000;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0xffff_ffff_ffff_ffea, reg.x[2],
                   "0x{:016x}", reg.x[2]);
    }

    #[test]
    fn test_andi() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(0b1010101_01010, 1, FUNCT3_ANDI, 2, OP_IMM);
        reg.x[1] = 0b010101111111;
        handle_op_imm(&mut reg, inst);
        assert_eq!(0b000000101010, reg.x[2], "{:b}", reg.x[2]);
    }

    #[test]
    fn test_auipc() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_auipc(2, 0x123);
        reg.pc = 0xabc;
        handle_auipc(&mut reg, inst);
        assert_eq!(0x123abc, reg.x[2], "0x{:x}", reg.x[2])
    }

    #[test]
    fn test_auipc_neg() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_auipc(2, -8i32 as u32);
        reg.pc = 0x8abc;
        handle_auipc(&mut reg, inst);
        assert_eq!(0xabc, reg.x[2], "0x{:x}", reg.x[2])
    }

    #[test]
    fn test_lui() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_lui(2, 0x123);
        handle_lui(&mut reg, inst);
        assert_eq!(0x123000, reg.x[2], "0x{:x}", reg.x[2])
    }

    #[test]
    fn test_lui_neg() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_lui(2, -8i32 as u32);
        handle_lui(&mut reg, inst);
        assert_eq!((-8i64 * 0x1000) as u64, reg.x[2], "0x{:x}", reg.x[2])
    }

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
