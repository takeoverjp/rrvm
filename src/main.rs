#[macro_use]
extern crate log;
extern crate env_logger;
extern crate getopts;
extern crate memmap;
extern crate hex;

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
const FENCE_PRED_MASK : u32 = 0b00001111_00000000_00000000_00000000;
const FENCE_SUCC_MASK : u32 = 0b00000000_11110000_00000000_00000000;
const OPCODE_SHIFT : u8 = 2;
const RD_SHIFT     : u8 = 7;
const FUNCT3_SHIFT : u8 = 12;
const RS1_SHIFT    : u8 = 15;
const IMM12_SHIFT  : u8 = 20;
const RS2_SHIFT    : u8 = 20;
const FUNCT7_SHIFT : u8 = 25;
const FENCE_PRED_SHIFT : u8 = 24;
const FENCE_SUCC_SHIFT : u8 = 20;

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
    offset: u64,
}

struct ControlStatusRegister {
    // User mode
    ustatus        : u64, // User status register.
    uie            : u64, // User interrupt-enable register.
    utvec          : u64, // User trap handler base address.
    uscratch       : u64, // Scratch register for user trap handlers.
    uepc           : u64, // User exception program counter.
    ucause         : u64, // User trap cause.
    utval          : u64, // User bad address or instruction.
    uip            : u64, // User interrupt pending.
    fflags         : u64, // Floating-Point Accrued Exceptions.
    frm            : u64, // Floating-Point Dynamic Rounding Mode.
    fcsr           : u64, // Floating-Point Control and Sta
    cycle          : u64, // Cycle counter for RDCYCLE instruction.
    time           : u64, // Timer for RDTIME instruction.
    instret        : u64, // Instructions-retired counter for RDINSTRET instruction.
    hpmcounter3    : u64, // Performance-monitoring counter.
    hpmcounter4    : u64, // Performance-monitoring counter.
    hpmcounter31   : u64, // Performance-monitoring counter.
    cycleh         : u64, // Upper 32 bits of {\tt cycle}, RV32I only.
    timeh          : u64, // Upper 32 bits of {\tt time}, RV32I only.
    instreth       : u64, // Upper 32 bits of {\tt instret}, RV32I only.
    hpmcounter3h   : u64, // Upper 32 bits of {\tt hpmcounter3}, RV32I only.
    hpmcounter4h   : u64, // Upper 32 bits of {\tt hpmcounter4}, RV32I only.
    hpmcounter31h  : u64, // Upper 32 bits of {\tt hpmcounter31}, RV32I only.
    // Supervisor mode
    sstatus        : u64, // Supervisor status register.
    sedeleg        : u64, // Supervisor exception delegation register.
    sideleg        : u64, // Supervisor interrupt delegation register.
    sie            : u64, // Supervisor interrupt-enable register.
    stvec          : u64, // Supervisor trap handler base address.
    scounteren     : u64, // Supervisor counter enable.
    sscratch       : u64, // Scratch register for supervisor trap handlers.
    sepc           : u64, // Supervisor exception program counter.
    scause         : u64, // Supervisor trap cause.
    stval          : u64, // Supervisor bad address or instruction.
    sip            : u64, // Supervisor interrupt pending.
    satp           : u64, // Supervisor address translation and protection.
    // Machine mode
    mvendorid      : u64, // Vendor ID.
    marchid        : u64, // Architecture ID.
    mimpid         : u64, // Implementation ID.
    mhartid        : u64, // Hardware thread ID.
    mstatus        : u64, // Machine status register.
    misa           : u64, // ISA and extensions
    medeleg        : u64, // Machine exception delegation register.
    mideleg        : u64, // Machine interrupt delegation register.
    mie            : u64, // Machine interrupt-enable register.
    mtvec          : u64, // Machine trap-handler base address.
    mcounteren     : u64, // Machine counter enable.
    mscratch       : u64, // Scratch register for machine trap handlers.
    mepc           : u64, // Machine exception program counter.
    mcause         : u64, // Machine trap cause.
    mtval          : u64, // Machine bad address or instruction.
    mip            : u64, // Machine interrupt pending.
    pmpcfg0        : u64, // Physical memory protection configuration.
    pmpcfg1        : u64, // Physical memory protection configuration, RV32 only.
    pmpcfg2        : u64, // Physical memory protection configuration.
    pmpcfg3        : u64, // Physical memory protection configuration, RV32 only.
    pmpaddr0       : u64, // Physical memory protection address register.
    pmpaddr1       : u64, // Physical memory protection address register.
    pmpaddr15      : u64, // Physical memory protection address register.
    mcycle         : u64, // Machine cycle counter.
    minstret       : u64, // Machine instructions-retired counter.
    mhpmcounter3   : u64, // Machine performance-monitoring counter.
    mhpmcounter4   : u64, // Machine performance-monitoring counter.
    mhpmcounter31  : u64, // Machine performance-monitoring counter.
    mcycleh        : u64, // Upper 32 bits of {\tt mcycle}, RV32I only.
    minstreth      : u64, // Upper 32 bits of {\tt minstret}, RV32I only.
    mhpmcounter3h  : u64, // Upper 32 bits of {\tt mhpmcounter3}, RV32I only.
    mhpmcounter4h  : u64, // Upper 32 bits of {\tt mhpmcounter4}, RV32I only.
    mhpmcounter31h : u64, // Upper 32 bits of {\tt mhpmcounter31}, RV32I only.
    mcountinhibit  : u64, // Machine counter-inhibit register.
    mhpmevent3     : u64, // Machine performance-monitoring event selector.
    mhpmevent4     : u64, // Machine performance-monitoring event selector.
    mhpmevent31    : u64, // Machine performance-monitoring event selector.
    tselect        : u64, // Debug/Trace trigger register select.
    tdata1         : u64, // First Debug/Trace trigger data register.
    tdata2         : u64, // Second Debug/Trace trigger data register.
    tdata3         : u64, // Third Debug/Trace trigger data register.
    // Device
    dcsr           : u64, // Debug control and status register.
    dpc            : u64, // Debug PC.
    dscratch       : u64, // Debug scratch register.
}

// User mode
const CSR_USTATUS        : u32 = 0x000;
const CSR_UIE            : u32 = 0x004;
const CSR_UTVEC          : u32 = 0x005;
const CSR_USCRATCH       : u32 = 0x040;
const CSR_UEPC           : u32 = 0x041;
const CSR_UCAUSE         : u32 = 0x042;
const CSR_UTVAL          : u32 = 0x043;
const CSR_UIP            : u32 = 0x044;
const CSR_FFLAGS         : u32 = 0x001;
const CSR_FRM            : u32 = 0x002;
const CSR_FCSR           : u32 = 0x003;
const CSR_CYCLE          : u32 = 0xC00;
const CSR_TIME           : u32 = 0xC01;
const CSR_INSTRET        : u32 = 0xC02;
const CSR_HPMCOUNTER3    : u32 = 0xC03;
const CSR_HPMCOUNTER4    : u32 = 0xC04;
const CSR_HPMCOUNTER31   : u32 = 0xC1F;
const CSR_CYCLEH         : u32 = 0xC80;
const CSR_TIMEH          : u32 = 0xC81;
const CSR_INSTRETH       : u32 = 0xC82;
const CSR_HPMCOUNTER3H   : u32 = 0xC83;
const CSR_HPMCOUNTER4H   : u32 = 0xC84;
const CSR_HPMCOUNTER31H  : u32 = 0xC9F;
// Supervisor mode
const CSR_SSTATUS        : u32 = 0x100;
const CSR_SEDELEG        : u32 = 0x102;
const CSR_SIDELEG        : u32 = 0x103;
const CSR_SIE            : u32 = 0x104;
const CSR_STVEC          : u32 = 0x105;
const CSR_SCOUNTEREN     : u32 = 0x106;
const CSR_SSCRATCH       : u32 = 0x140;
const CSR_SEPC           : u32 = 0x141;
const CSR_SCAUSE         : u32 = 0x142;
const CSR_STVAL          : u32 = 0x143;
const CSR_SIP            : u32 = 0x144;
const CSR_SATP           : u32 = 0x180;
// Machine mode
const CSR_MVENDORID      : u32 = 0xF11;
const CSR_MARCHID        : u32 = 0xF12;
const CSR_MIMPID         : u32 = 0xF13;
const CSR_MHARTID        : u32 = 0xF14;
const CSR_MSTATUS        : u32 = 0x300;
const CSR_MISA           : u32 = 0x301;
const CSR_MEDELEG        : u32 = 0x302;
const CSR_MIDELEG        : u32 = 0x303;
const CSR_MIE            : u32 = 0x304;
const CSR_MTVEC          : u32 = 0x305;
const CSR_MCOUNTEREN     : u32 = 0x306;
const CSR_MSCRATCH       : u32 = 0x340;
const CSR_MEPC           : u32 = 0x341;
const CSR_MCAUSE         : u32 = 0x342;
const CSR_MTVAL          : u32 = 0x343;
const CSR_MIP            : u32 = 0x344;
const CSR_PMPCFG0        : u32 = 0x3A0;
const CSR_PMPCFG1        : u32 = 0x3A1;
const CSR_PMPCFG2        : u32 = 0x3A2;
const CSR_PMPCFG3        : u32 = 0x3A3;
const CSR_PMPADDR0       : u32 = 0x3B0;
const CSR_PMPADDR1       : u32 = 0x3B1;
const CSR_PMPADDR15      : u32 = 0x3BF;
const CSR_MCYCLE         : u32 = 0xB00;
const CSR_MINSTRET       : u32 = 0xB02;
const CSR_MHPMCOUNTER3   : u32 = 0xB03;
const CSR_MHPMCOUNTER4   : u32 = 0xB04;
const CSR_MHPMCOUNTER31  : u32 = 0xB1F;
const CSR_MCYCLEH        : u32 = 0xB80;
const CSR_MINSTRETH      : u32 = 0xB82;
const CSR_MHPMCOUNTER3H  : u32 = 0xB83;
const CSR_MHPMCOUNTER4H  : u32 = 0xB84;
const CSR_MHPMCOUNTER31H : u32 = 0xB9F;
const CSR_MCOUNTINHIBIT  : u32 = 0x320;
const CSR_MHPMEVENT3     : u32 = 0x323;
const CSR_MHPMEVENT4     : u32 = 0x324;
const CSR_MHPMEVENT31    : u32 = 0x33F;
const CSR_TSELECT        : u32 = 0x7A0;
const CSR_TDATA1         : u32 = 0x7A1;
const CSR_TDATA2         : u32 = 0x7A2;
const CSR_TDATA3         : u32 = 0x7A3;
// Device
const CSR_DCSR           : u32 = 0x7B0;
const CSR_DPC            : u32 = 0x7B1;
const CSR_DSCRATCH       : u32 = 0x7B2;

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
                // User mode
                ustatus        : 0,
                uie            : 0,
                utvec          : 0,
                uscratch       : 0,
                uepc           : 0,
                ucause         : 0,
                utval          : 0,
                uip            : 0,
                fflags         : 0,
                frm            : 0,
                fcsr           : 0,
                cycle          : 0,
                time           : 0,
                instret        : 0,
                hpmcounter3    : 0,
                hpmcounter4    : 0,
                hpmcounter31   : 0,
                cycleh         : 0,
                timeh          : 0,
                instreth       : 0,
                hpmcounter3h   : 0,
                hpmcounter4h   : 0,
                hpmcounter31h  : 0,
                // Supervisor mode
                sstatus        : 0,
                sedeleg        : 0,
                sideleg        : 0,
                sie            : 0,
                stvec          : 0,
                scounteren     : 0,
                sscratch       : 0,
                sepc           : 0,
                scause         : 0,
                stval          : 0,
                sip            : 0,
                satp           : 0,
                // Machine mode
                mvendorid      : 0,
                marchid        : 0,
                mimpid         : 0,
                mhartid        : 0,
                mstatus        : 0,
                misa           : 0,
                medeleg        : 0,
                mideleg        : 0,
                mie            : 0,
                mtvec          : 0,
                mcounteren     : 0,
                mscratch       : 0,
                mepc           : 0,
                mcause         : 0,
                mtval          : 0,
                mip            : 0,
                pmpcfg0        : 0,
                pmpcfg1        : 0,
                pmpcfg2        : 0,
                pmpcfg3        : 0,
                pmpaddr0       : 0,
                pmpaddr1       : 0,
                pmpaddr15      : 0,
                mcycle         : 0,
                minstret       : 0,
                mhpmcounter3   : 0,
                mhpmcounter4   : 0,
                mhpmcounter31  : 0,
                mcycleh        : 0,
                minstreth      : 0,
                mhpmcounter3h  : 0,
                mhpmcounter4h  : 0,
                mhpmcounter31h : 0,
                mcountinhibit  : 0,
                mhpmevent3     : 0,
                mhpmevent4     : 0,
                mhpmevent31    : 0,
                tselect        : 0,
                tdata1         : 0,
                tdata2         : 0,
                tdata3         : 0,
                // Device
                dcsr           : 0,
                dpc            : 0,
                dscratch       : 0,
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

fn get_fence_pred(inst: u32) -> u32 {
    (inst & FENCE_PRED_MASK) >> FENCE_PRED_SHIFT
}

fn get_fence_succ(inst: u32) -> u32 {
    (inst & FENCE_SUCC_MASK) >> FENCE_SUCC_SHIFT
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

fn handle_misc_mem(_reg: &RegisterFile, inst: u32) {
    const FUNCT3_FENCE   : u32 = 0b000;
    const FUNCT3_FENCE_I : u32 = 0b001;

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

fn get_csr(reg: &mut RegisterFile, addr: u32) -> u64 {
    match addr {
        // User mode
        CSR_USTATUS        => reg.csr.ustatus,
        CSR_UIE            => reg.csr.uie,
        CSR_UTVEC          => reg.csr.utvec,
        CSR_USCRATCH       => reg.csr.uscratch,
        CSR_UEPC           => reg.csr.uepc,
        CSR_UCAUSE         => reg.csr.ucause,
        CSR_UTVAL          => reg.csr.utval,
        CSR_UIP            => reg.csr.uip,
        CSR_FFLAGS         => reg.csr.fflags,
        CSR_FRM            => reg.csr.frm,
        CSR_FCSR           => reg.csr.fcsr,
        CSR_CYCLE          => reg.csr.cycle,
        CSR_TIME           => reg.csr.time,
        CSR_INSTRET        => reg.csr.instret,
        CSR_HPMCOUNTER3    => reg.csr.hpmcounter3,
        CSR_HPMCOUNTER4    => reg.csr.hpmcounter4,
        CSR_HPMCOUNTER31   => reg.csr.hpmcounter31,
        CSR_CYCLEH         => reg.csr.cycleh,
        CSR_TIMEH          => reg.csr.timeh,
        CSR_INSTRETH       => reg.csr.instreth,
        CSR_HPMCOUNTER3H   => reg.csr.hpmcounter3h,
        CSR_HPMCOUNTER4H   => reg.csr.hpmcounter4h,
        CSR_HPMCOUNTER31H  => reg.csr.hpmcounter31h,
        // Supervisor mode
        CSR_SSTATUS        => reg.csr.sstatus,
        CSR_SEDELEG        => reg.csr.sedeleg,
        CSR_SIDELEG        => reg.csr.sideleg,
        CSR_SIE            => reg.csr.sie,
        CSR_STVEC          => reg.csr.stvec,
        CSR_SCOUNTEREN     => reg.csr.scounteren,
        CSR_SSCRATCH       => reg.csr.sscratch,
        CSR_SEPC           => reg.csr.sepc,
        CSR_SCAUSE         => reg.csr.scause,
        CSR_STVAL          => reg.csr.stval,
        CSR_SIP            => reg.csr.sip,
        CSR_SATP           => reg.csr.satp,
        // Machine mode
        CSR_MVENDORID      => reg.csr.mvendorid,
        CSR_MARCHID        => reg.csr.marchid,
        CSR_MIMPID         => reg.csr.mimpid,
        CSR_MHARTID        => reg.csr.mhartid,
        CSR_MSTATUS        => reg.csr.mstatus,
        CSR_MISA           => reg.csr.misa,
        CSR_MEDELEG        => reg.csr.medeleg,
        CSR_MIDELEG        => reg.csr.mideleg,
        CSR_MIE            => reg.csr.mie,
        CSR_MTVEC          => reg.csr.mtvec,
        CSR_MCOUNTEREN     => reg.csr.mcounteren,
        CSR_MSCRATCH       => reg.csr.mscratch,
        CSR_MEPC           => reg.csr.mepc,
        CSR_MCAUSE         => reg.csr.mcause,
        CSR_MTVAL          => reg.csr.mtval,
        CSR_MIP            => reg.csr.mip,
        CSR_PMPCFG0        => reg.csr.pmpcfg0,
        CSR_PMPCFG1        => reg.csr.pmpcfg1,
        CSR_PMPCFG2        => reg.csr.pmpcfg2,
        CSR_PMPCFG3        => reg.csr.pmpcfg3,
        CSR_PMPADDR0       => reg.csr.pmpaddr0,
        CSR_PMPADDR1       => reg.csr.pmpaddr1,
        CSR_PMPADDR15      => reg.csr.pmpaddr15,
        CSR_MCYCLE         => reg.csr.mcycle,
        CSR_MINSTRET       => reg.csr.minstret,
        CSR_MHPMCOUNTER3   => reg.csr.mhpmcounter3,
        CSR_MHPMCOUNTER4   => reg.csr.mhpmcounter4,
        CSR_MHPMCOUNTER31  => reg.csr.mhpmcounter31,
        CSR_MCYCLEH        => reg.csr.mcycleh,
        CSR_MINSTRETH      => reg.csr.minstreth,
        CSR_MHPMCOUNTER3H  => reg.csr.mhpmcounter3h,
        CSR_MHPMCOUNTER4H  => reg.csr.mhpmcounter4h,
        CSR_MHPMCOUNTER31H => reg.csr.mhpmcounter31h,
        CSR_MCOUNTINHIBIT  => reg.csr.mcountinhibit,
        CSR_MHPMEVENT3     => reg.csr.mhpmevent3,
        CSR_MHPMEVENT4     => reg.csr.mhpmevent4,
        CSR_MHPMEVENT31    => reg.csr.mhpmevent31,
        CSR_TSELECT        => reg.csr.tselect,
        CSR_TDATA1         => reg.csr.tdata1,
        CSR_TDATA2         => reg.csr.tdata2,
        CSR_TDATA3         => reg.csr.tdata3,
        // Device
        CSR_DCSR           => reg.csr.dcsr,
        CSR_DPC            => reg.csr.dpc,
        CSR_DSCRATCH       => reg.csr.dscratch,
        _ => {
            error!("{}: {}: unknown csr addr 0x{:x}", file!(), line!(), addr);
            std::process::exit(-1);
        }
    }
}

fn get_csr_name(addr: u32) -> &'static str {
    match addr {
        // User mode
        CSR_USTATUS        => "ustatus",
        CSR_UIE            => "uie",
        CSR_UTVEC          => "utvec",
        CSR_USCRATCH       => "uscratch",
        CSR_UEPC           => "uepc",
        CSR_UCAUSE         => "ucause",
        CSR_UTVAL          => "utval",
        CSR_UIP            => "uip",
        CSR_FFLAGS         => "fflags",
        CSR_FRM            => "frm",
        CSR_FCSR           => "fcsr",
        CSR_CYCLE          => "cycle",
        CSR_TIME           => "time",
        CSR_INSTRET        => "instret",
        CSR_HPMCOUNTER3    => "hpmcounter3",
        CSR_HPMCOUNTER4    => "hpmcounter4",
        CSR_HPMCOUNTER31   => "hpmcounter31",
        CSR_CYCLEH         => "cycleh",
        CSR_TIMEH          => "timeh",
        CSR_INSTRETH       => "instreth",
        CSR_HPMCOUNTER3H   => "hpmcounter3h",
        CSR_HPMCOUNTER4H   => "hpmcounter4h",
        CSR_HPMCOUNTER31H  => "hpmcounter31h",
        // Supervisor mode
        CSR_SSTATUS        => "sstatus",
        CSR_SEDELEG        => "sedeleg",
        CSR_SIDELEG        => "sideleg",
        CSR_SIE            => "sie",
        CSR_STVEC          => "stvec",
        CSR_SCOUNTEREN     => "scounteren",
        CSR_SSCRATCH       => "sscratch",
        CSR_SEPC           => "sepc",
        CSR_SCAUSE         => "scause",
        CSR_STVAL          => "stval",
        CSR_SIP            => "sip",
        CSR_SATP           => "satp",
        // Machine mode
        CSR_MVENDORID      => "mvendorid",
        CSR_MARCHID        => "marchid",
        CSR_MIMPID         => "mimpid",
        CSR_MHARTID        => "mhartid",
        CSR_MSTATUS        => "mstatus",
        CSR_MISA           => "misa",
        CSR_MEDELEG        => "medeleg",
        CSR_MIDELEG        => "mideleg",
        CSR_MIE            => "mie",
        CSR_MTVEC          => "mtvec",
        CSR_MCOUNTEREN     => "mcounteren",
        CSR_MSCRATCH       => "mscratch",
        CSR_MEPC           => "mepc",
        CSR_MCAUSE         => "mcause",
        CSR_MTVAL          => "mtval",
        CSR_MIP            => "mip",
        CSR_PMPCFG0        => "pmpcfg0",
        CSR_PMPCFG1        => "pmpcfg1",
        CSR_PMPCFG2        => "pmpcfg2",
        CSR_PMPCFG3        => "pmpcfg3",
        CSR_PMPADDR0       => "pmpaddr0",
        CSR_PMPADDR1       => "pmpaddr1",
        CSR_PMPADDR15      => "pmpaddr15",
        CSR_MCYCLE         => "mcycle",
        CSR_MINSTRET       => "minstret",
        CSR_MHPMCOUNTER3   => "mhpmcounter3",
        CSR_MHPMCOUNTER4   => "mhpmcounter4",
        CSR_MHPMCOUNTER31  => "mhpmcounter31",
        CSR_MCYCLEH        => "mcycleh",
        CSR_MINSTRETH      => "minstreth",
        CSR_MHPMCOUNTER3H  => "mhpmcounter3h",
        CSR_MHPMCOUNTER4H  => "mhpmcounter4h",
        CSR_MHPMCOUNTER31H => "mhpmcounter31h",
        CSR_MCOUNTINHIBIT  => "mcountinhibit",
        CSR_MHPMEVENT3     => "mhpmevent3",
        CSR_MHPMEVENT4     => "mhpmevent4",
        CSR_MHPMEVENT31    => "mhpmevent31",
        CSR_TSELECT        => "tselect",
        CSR_TDATA1         => "tdata1",
        CSR_TDATA2         => "tdata2",
        CSR_TDATA3         => "tdata3",
        // Device
        CSR_DCSR           => "dcsr",
        CSR_DPC            => "dpc",
        CSR_DSCRATCH       => "dscratch",
        _ => {
            error!("{}: {}: unknown csr addr 0x{:x}", file!(), line!(), addr);
            std::process::exit(-1);
        }
    }
}

fn set_csr(reg: &mut RegisterFile, addr: u32, val: u64) {
    match addr {
        // User mode
        CSR_USTATUS        => {reg.csr.ustatus = val;},
        CSR_UIE            => {reg.csr.uie = val;},
        CSR_UTVEC          => {reg.csr.utvec = val;},
        CSR_USCRATCH       => {reg.csr.uscratch = val;},
        CSR_UEPC           => {reg.csr.uepc = val;},
        CSR_UCAUSE         => {reg.csr.ucause = val;},
        CSR_UTVAL          => {reg.csr.utval = val;},
        CSR_UIP            => {reg.csr.uip = val;},
        CSR_FFLAGS         => {reg.csr.fflags = val;},
        CSR_FRM            => {reg.csr.frm = val;},
        CSR_FCSR           => {reg.csr.fcsr = val;},
        CSR_CYCLE          => {reg.csr.cycle = val;},
        CSR_TIME           => {reg.csr.time = val;},
        CSR_INSTRET        => {reg.csr.instret = val;},
        CSR_HPMCOUNTER3    => {reg.csr.hpmcounter3 = val;},
        CSR_HPMCOUNTER4    => {reg.csr.hpmcounter4 = val;},
        CSR_HPMCOUNTER31   => {reg.csr.hpmcounter31 = val;},
        CSR_CYCLEH         => {reg.csr.cycleh = val;},
        CSR_TIMEH          => {reg.csr.timeh = val;},
        CSR_INSTRETH       => {reg.csr.instreth = val;},
        CSR_HPMCOUNTER3H   => {reg.csr.hpmcounter3h = val;},
        CSR_HPMCOUNTER4H   => {reg.csr.hpmcounter4h = val;},
        CSR_HPMCOUNTER31H  => {reg.csr.hpmcounter31h = val;},
        // Supervisor mode
        CSR_SSTATUS        => {reg.csr.sstatus = val;},
        CSR_SEDELEG        => {reg.csr.sedeleg = val;},
        CSR_SIDELEG        => {reg.csr.sideleg = val;},
        CSR_SIE            => {reg.csr.sie = val;},
        CSR_STVEC          => {reg.csr.stvec = val;},
        CSR_SCOUNTEREN     => {reg.csr.scounteren = val;},
        CSR_SSCRATCH       => {reg.csr.sscratch = val;},
        CSR_SEPC           => {reg.csr.sepc = val;},
        CSR_SCAUSE         => {reg.csr.scause = val;},
        CSR_STVAL          => {reg.csr.stval = val;},
        CSR_SIP            => {reg.csr.sip = val;},
        CSR_SATP           => {reg.csr.satp = val;},
        // Machine mode
        CSR_MVENDORID      => {reg.csr.mvendorid = val;},
        CSR_MARCHID        => {reg.csr.marchid = val;},
        CSR_MIMPID         => {reg.csr.mimpid = val;},
        CSR_MHARTID        => {reg.csr.mhartid = val;},
        CSR_MSTATUS        => {reg.csr.mstatus = val;},
        CSR_MISA           => {reg.csr.misa = val;},
        CSR_MEDELEG        => {reg.csr.medeleg = val;},
        CSR_MIDELEG        => {reg.csr.mideleg = val;},
        CSR_MIE            => {reg.csr.mie = val;},
        CSR_MTVEC          => {reg.csr.mtvec = val;},
        CSR_MCOUNTEREN     => {reg.csr.mcounteren = val;},
        CSR_MSCRATCH       => {reg.csr.mscratch = val;},
        CSR_MEPC           => {reg.csr.mepc = val;},
        CSR_MCAUSE         => {reg.csr.mcause = val;},
        CSR_MTVAL          => {reg.csr.mtval = val;},
        CSR_MIP            => {reg.csr.mip = val;},
        CSR_PMPCFG0        => {reg.csr.pmpcfg0 = val;},
        CSR_PMPCFG1        => {reg.csr.pmpcfg1 = val;},
        CSR_PMPCFG2        => {reg.csr.pmpcfg2 = val;},
        CSR_PMPCFG3        => {reg.csr.pmpcfg3 = val;},
        CSR_PMPADDR0       => {reg.csr.pmpaddr0 = val;},
        CSR_PMPADDR1       => {reg.csr.pmpaddr1 = val;},
        CSR_PMPADDR15      => {reg.csr.pmpaddr15 = val;},
        CSR_MCYCLE         => {reg.csr.mcycle = val;},
        CSR_MINSTRET       => {reg.csr.minstret = val;},
        CSR_MHPMCOUNTER3   => {reg.csr.mhpmcounter3 = val;},
        CSR_MHPMCOUNTER4   => {reg.csr.mhpmcounter4 = val;},
        CSR_MHPMCOUNTER31  => {reg.csr.mhpmcounter31 = val;},
        CSR_MCYCLEH        => {reg.csr.mcycleh = val;},
        CSR_MINSTRETH      => {reg.csr.minstreth = val;},
        CSR_MHPMCOUNTER3H  => {reg.csr.mhpmcounter3h = val;},
        CSR_MHPMCOUNTER4H  => {reg.csr.mhpmcounter4h = val;},
        CSR_MHPMCOUNTER31H => {reg.csr.mhpmcounter31h = val;},
        CSR_MCOUNTINHIBIT  => {reg.csr.mcountinhibit = val;},
        CSR_MHPMEVENT3     => {reg.csr.mhpmevent3 = val;},
        CSR_MHPMEVENT4     => {reg.csr.mhpmevent4 = val;},
        CSR_MHPMEVENT31    => {reg.csr.mhpmevent31 = val;},
        CSR_TSELECT        => {reg.csr.tselect = val;},
        CSR_TDATA1         => {reg.csr.tdata1 = val;},
        CSR_TDATA2         => {reg.csr.tdata2 = val;},
        CSR_TDATA3         => {reg.csr.tdata3 = val;},
        // Device
        CSR_DCSR           => {reg.csr.dcsr = val;},
        CSR_DPC            => {reg.csr.dpc = val;},
        CSR_DSCRATCH       => {reg.csr.dscratch = val;},
        _ => {
            error!("{}: {}: unknown csr addr 0x{:x}", file!(), line!(), addr);
            std::process::exit(-1);
        }
    }
}

fn handle_system(reg: &mut RegisterFile, inst: u32) {
    const FUNCT3_ECALL_EBREAK : u32 = 0b000;
    const FUNCT3_CSRRW        : u32 = 0b001;
    const FUNCT3_CSRRS        : u32 = 0b010;
    const FUNCT3_CSRRC        : u32 = 0b011;
    const FUNCT3_CSRRWI       : u32 = 0b101;
    const FUNCT3_CSRRSI       : u32 = 0b110;
    const FUNCT3_CSRRCI       : u32 = 0b111;

    let funct3 = get_funct3(inst);
    let rd     = get_rd(inst) as usize;
    let rs1    = get_rs1(inst) as usize;
    let csr_addr = get_imm12(inst);
    let csr_val = get_csr(reg, csr_addr);
    let csr_name = get_csr_name(csr_addr);

    match funct3 {
        FUNCT3_ECALL_EBREAK => unimplemented!(),
        FUNCT3_CSRRW        => {
            info!("csrrw {},{},{}", ABI_NAME[rd], csr_name, ABI_NAME[rs1]);
            let new_val = reg.x[rs1];
            set_csr(reg, csr_addr, new_val);
            if rd != 0 {
                reg.x[rd] = csr_val;
            }
        },
        FUNCT3_CSRRS        => {
            info!("csrrs {},{},{}", ABI_NAME[rd], csr_name, ABI_NAME[rs1]);
            let new_val = csr_val | reg.x[rs1];
            set_csr(reg, csr_addr, new_val);
            if rd != 0 {
                reg.x[rd] = csr_val;
            }
        },
        FUNCT3_CSRRC        => unimplemented!(),
        FUNCT3_CSRRWI       => {
            if rd != 0 {
                reg.x[rd] = csr_val;
            }
            let new_val = rs1 as u64;
            set_csr(reg, csr_addr, new_val);
        }
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
                error!("decode {:?} failed: {:?}", numstr, e);
                std::process::exit(-1);
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

    let memory = match matches.opt_str("m") {
        Some(value) => value,
        None => String::from("0x08000000"), // 128MB
    };
    let memory = str2u64(&memory) as usize;

    let offset = match matches.opt_str("o") {
        Some(value) => value,
        None => String::from("0")
    };
    let offset = str2u64(&offset);

    Args {
        prog: matches.free[0].clone(),
        log_level: log_level,
        memory: memory,
        offset: offset,
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
    reg.pc += args.offset;
    let map = get_memmap(&args.prog);
    let mut mem = map.to_vec();
    unsafe {
        mem.set_len(args.memory);
    }

    loop {
        let inst : u32 = fetch(&map, (reg.pc - args.offset) as usize);
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
    fn test_lb() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(4, 1, 0b000, 2, 0b0000011);
        let mem: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 6, 7];
        reg.x[1] = 0x2;
        handle_load(&mem, &mut reg, inst);
        assert_eq!(0x06, reg.x[2]);
    }

    #[test]
    fn test_lb_negative_value() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(4, 1, 0b000, 2, 0b0000011);
        let mem: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 0xfe, 7];
        reg.x[1] = 0x2;
        handle_load(&mem, &mut reg, inst);
        assert_eq!(-2i64 as u64, reg.x[2]);
    }

    #[test]
    fn test_lb_negative_offset() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(-4i16 as u16, 1, 0b000, 2, 0b0000011);
        let mem: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 6, 7];
        reg.x[1] = 10;
        handle_load(&mem, &mut reg, inst);
        assert_eq!(6, reg.x[2]);
    }

    #[test]
    fn test_lh() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(4, 1, 0b001, 2, 0b0000011);
        let mem: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 6, 7];
        reg.x[1] = 0x2;
        handle_load(&mem, &mut reg, inst);
        assert_eq!(0x0706, reg.x[2]);
    }

    #[test]
    fn test_lh_negative_value() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(4, 1, 0b001, 2, 0b0000011);
        let mem: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 0xfe, 0xff];
        reg.x[1] = 0x2;
        handle_load(&mem, &mut reg, inst);
        assert_eq!(-2i64 as u64, reg.x[2]);
    }

    #[test]
    fn test_lh_negative_offset() {
        let mut reg = RegisterFile::new();
        let inst: u32 = inst_i(-4i16 as u16, 1, 0b001, 2, 0b0000011);
        let mem: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 6, 7];
        reg.x[1] = 10;
        handle_load(&mem, &mut reg, inst);
        assert_eq!(0x0706, reg.x[2]);
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
