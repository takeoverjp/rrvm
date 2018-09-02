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
    verbose: bool,
    memory: usize,
}

struct ControlStatusRegister {
    mepc : u64
}

struct RegisterFile {
    csr : ControlStatusRegister,
    pc : u64,
    x : [u64; 32]
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
            println!("file({}) open error {:?}", file_path, e);
            std::process::exit(-1);
        }
    };

    let mmap = unsafe {
        match MmapOptions::new().map(&file) {
            Ok(map) => map,
            Err(e) => {
                println!("memmap error {:?}", e);
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

fn dump_reg(reg: &RegisterFile) {
    println!("pc = 0x{:x}", reg.pc);
    for (idx, x) in reg.x.iter().enumerate() {
        println!("x[{:2?}] = 0x{:016x}", idx, x);
    }
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
    let imm12  = get_imm12(inst);
    let addr   = reg.x[rs1] + imm12 as u64;

    println!("load from 0x{:016x}", addr);
    if mmio_region(addr) {
        println!("{}: {}: Not implemented", file!(), line!());
    } else {
        let addr = addr as usize;
        reg.x[rd] = map[addr] as u64;
    }

    match funct3 {
        FUNCT3_LB  => println!("Load byte"),
        FUNCT3_LH  => println!("Load half word"),
        FUNCT3_LW  => println!("Load word"),
        FUNCT3_LD  => println!("Load double word"),
        FUNCT3_LBU => println!("Load unsigned byte"),
        FUNCT3_LHU => println!("Load unsigned half word"),
        FUNCT3_LWU => println!("Load unsigned word"),
        _ => println!("{}: {}: unknown funct3 0x{:x}",
                      file!(), line!(), funct3)
    }
}

fn handle_load_fp(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_custom_0(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_misc_mem(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
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
    let imm12  = get_imm12(inst);
    let shamt  = get_rs2(inst);
    let funct7 = get_funct7(inst);

    match funct3 {
        FUNCT3_ADDI      => reg.x[rd] = reg.x[rs1] + (imm12 as u64),
        FUNCT3_SLLI      => reg.x[rd] = reg.x[rs1] << shamt,
        FUNCT3_SLTI      => reg.x[rd] = ((reg.x[rs1] as i64) < (imm12 as i64)) as u64,
        FUNCT3_SLTIU     => reg.x[rd] = (reg.x[rs1] < (imm12 as u64)) as u64,
        FUNCT3_XORI      => reg.x[rd] = reg.x[rs1] ^ (imm12 as u64),
        FUNCT3_SRLI_SRAI => match funct7 {
            FUNCT7_SRLI  => reg.x[rd] = reg.x[rs1] >> shamt,
            FUNCT7_SRAI  => reg.x[rd] = reg.x[rs1] >> shamt, // TODO
            _ => println!("{}: {}: unknown funct7 0x{:x}",
                          file!(), line!(), funct7)
        }
        FUNCT3_ORI       => reg.x[rd] = reg.x[rs1] + (imm12 as u64),
        FUNCT3_ANDI      => reg.x[rd] = reg.x[rs1] + (imm12 as u64),
        _ => println!("{}: {}: unknown funct3 0x{:x}",
                      file!(), line!(), funct3)
    }
}

fn handle_auipc(reg: &mut RegisterFile, inst: u32) {
    let rd  = get_rd(inst) as usize;

    reg.x[rd] = reg.pc + (inst & 0b11111111_11111111_11110000_00000000) as u64
}

fn handle_op_imm_32(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_long_op_48(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
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
    let imm12   = (get_funct7(inst) << 5) + get_rd(inst);
    let addr   = reg.x[rs1] + imm12 as u64;

    println!("store {} to 0x{:016x}", reg.x[rs2], addr);
    if mmio_region(addr) {
        mmio_store(addr, reg.x[rs2]);
    } else {
        map[addr as usize] = reg.x[rs2] as u8; // TODO size
    }

    match funct3 {
        FUNCT3_SB => println!("Store byte"),
        FUNCT3_SH => println!("Store half word"),
        FUNCT3_SW => println!("Store word"),
        FUNCT3_SD => println!("Store double word"),
        _ => println!("{}: {}: unknown funct3 0x{:x}",
                      file!(), line!(), funct3)
    }
}

fn handle_store_fp(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_custom_1(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_amo(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
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
            FUNCT7_ADD => reg.x[rd] = reg.x[rs1] + reg.x[rs2],
            FUNCT7_SUB => reg.x[rd] = reg.x[rs1] - reg.x[rs2],
            _ => println!("{}: {}: unknown funct7 0x{:x}",
                          file!(), line!(), funct7)
        },
        FUNCT3_SLL     => reg.x[rd] = reg.x[rs1] << reg.x[rs2],
        FUNCT3_SLT     => reg.x[rd] = ((reg.x[rs1] as i64) < (reg.x[rs2] as i64)) as u64,
        FUNCT3_SLTU    => reg.x[rd] = (reg.x[rs1] < reg.x[rs2]) as u64,
        FUNCT3_XOR     => reg.x[rd] = reg.x[rs1] - reg.x[rs2],
        FUNCT3_SRL_SRA => match funct7 {
            FUNCT7_SRL => reg.x[rd] = reg.x[rs1] - reg.x[rs2],
            FUNCT7_SRA => reg.x[rd] = reg.x[rs1] - reg.x[rs2], // TODO
            _ => println!("{}: {}: unknown funct7 0x{:x}",
                          file!(), line!(), funct7)
        },
        FUNCT3_OR      => reg.x[rd] = reg.x[rs1] | reg.x[rs2],
        FUNCT3_AND     => reg.x[rd] = reg.x[rs1] & reg.x[rs2],
        _ => println!("{}: {}: unknown funct3 0x{:x}",
                      file!(), line!(), funct3)
    }
}

fn handle_lui(reg: &mut RegisterFile, inst: u32) {
    let rd  = get_rd(inst) as usize;

    reg.x[rd] = (inst & 0b11111111_11111111_11110000_00000000) as u64
}

fn handle_op_32(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_long_op_64(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_madd(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_msub(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_nmsub(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_nmadd(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_op_fp(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_custom2(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_long_op_48_2(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
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
    let imm = (((inst >> 31) & 0b1) << 12)
        | (((inst >> 7) & 0b1) << 11)
        | (((inst >> 25) & 0b11_1111) << 5)
        | (((inst >> 8) & 0b1111) << 1);
    let offset: i64 = if ((inst >> 31) & 0b1) == 1 {
        imm as i64 | (0xffff_ffff_ffff_f << 12)
    } else {
        imm as i64
    };

    let mut jump = false;
    match funct3 {
        FUNCT3_BEQ  => jump = reg.x[rs1] == reg.x[rs2],
        FUNCT3_BNE  => jump = reg.x[rs1] != reg.x[rs2],
        FUNCT3_BLT  => jump = reg.x[rs1] < reg.x[rs2], // TODO : sign
        FUNCT3_BGE  => jump = reg.x[rs1] >= reg.x[rs2], // TODO : sign
        FUNCT3_BLTU => jump = reg.x[rs1] < reg.x[rs2],
        FUNCT3_BGEU => jump = reg.x[rs1] >= reg.x[rs2],
        _ => println!("{}: {}: unknown funct3 0x{:x}",
                      file!(), line!(), funct3)
    }

    if jump {
        reg.pc = ((reg.pc as i64) + offset) as u64;
        println!("branch to 0x{:016x}", reg.pc);
        reg.pc -= 4; // increment after the handler.
    } else {
        println!("not branch");
    }
}

fn handle_jalr(reg: &mut RegisterFile, inst: u32) {
    let rd     = get_rd(inst) as usize;
    let rs1    = get_rs1(inst) as usize;
    let imm12  = get_imm12(inst);
    let offset: i64 = if ((inst >> 31) & 0b1) == 1 {
        imm12 as i64 | (0xffff_ffff_ffff_f << 12)
    } else {
        imm12 as i64
    };

    reg.x[rd] = reg.pc + 4;
    reg.pc = (reg.x[rs1] as i64 + offset) as u64;
    println!("jump and link register to 0x{:016x}", reg.pc);
    reg.pc -= 4; // increment after the handler.
}

fn handle_jal(reg: &mut RegisterFile, inst: u32) {
    let imm = (((inst >> 31) & 0b1) << 20)
        | (((inst >> 12) & 0b1111_1111) << 12)
        | (((inst >> 20) & 0b1) << 11)
        | (((inst >> 21) & 0b11_1111_1111) << 1); // TODO : sign extend
    let rd = get_rd(inst) as usize;
    let offset: i64 = if ((imm >> 20) & 0b1) == 1 {
        imm as i64 | (0xffff_ffff_fff << 20)
    } else {
        imm as i64
    };

    reg.x[rd] = reg.pc + 4;
    reg.pc = (reg.pc as i64 + offset) as u64;
    println!("jump to 0x{:016x}", reg.pc);
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
                if rd != 0 {
                    reg.x[rd] = reg.csr.mepc;
                }
                reg.csr.mepc = reg.x[rs1];
                println!("CSR_MEPC")
            },
            _ => println!("{}: {}: unknown csr 0x{:x}", file!(), line!(), csr)
        },
        FUNCT3_CSRRS        => unimplemented!(),
        FUNCT3_CSRRC        => unimplemented!(),
        FUNCT3_CSRRWI       => unimplemented!(),
        FUNCT3_CSRRSI       => unimplemented!(),
        FUNCT3_CSRRCI       => unimplemented!(),
        _ => println!("{}: {}: unknown funct3 0x{:x}", file!(), line!(), funct3)
    }
}

fn handle_custom3(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
}

fn handle_long_op_ge_80(_reg: &RegisterFile, _inst: u32) {
    println!("{}: {}: Not implemented", file!(), line!());
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
    opts.optflag("v", "verbose", "print verbose log");
    opts.optflag("h", "help", "print this help menu");

    let matches = opts.parse(&args[1..])
        .unwrap_or_else(|f| panic!(f.to_string()));

    if matches.opt_present("h")
        || matches.free.is_empty() {
        print_usage(&program, &opts);
    }

    let memory = match matches.opt_get::<usize>("m") {
        Ok(result) => match result {
            Some(value) => value,
            None => 128 * 1024 * 1024
        },
        Err(e) => {
            println!("memmap error {:?}", e);
            std::process::exit(-1);
        }
    };

    Args {
        prog: matches.free[0].clone(),
        verbose: matches.opt_present("v"),
        memory: memory,
    }
}

fn main() {
    let args = parse_args();
    if args.verbose { println!("{:?}", args); }
    let map = get_memmap(args.prog.as_str());
    let mut mem = map.to_vec();
    unsafe {
        mem.set_len(args.memory);
    }

    let mut reg = RegisterFile {
        csr: ControlStatusRegister {
            mepc : 0
        },
        pc: 0,
        x: [0; 32]
    };

    loop {
        let inst : u32 = fetch(&map, reg.pc as usize);
        println!("{:016x}: 0x{:08x} 0b{:032b} ", reg.pc, inst, inst);

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
            _ => println!("{}: {}: unknown opcode 0x{:x}",
                          file!(), line!(), opcode)
        }

        reg.pc += 4;

        if args.verbose {
            dump_reg(&reg);
        }
    }
}
