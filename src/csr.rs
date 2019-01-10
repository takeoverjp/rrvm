use std;
use std::io::Write;

// User mode
pub const CSR_USTATUS        : u32 = 0x000;
pub const CSR_UIE            : u32 = 0x004;
pub const CSR_UTVEC          : u32 = 0x005;
pub const CSR_USCRATCH       : u32 = 0x040;
pub const CSR_UEPC           : u32 = 0x041;
pub const CSR_UCAUSE         : u32 = 0x042;
pub const CSR_UTVAL          : u32 = 0x043;
pub const CSR_UIP            : u32 = 0x044;
pub const CSR_FFLAGS         : u32 = 0x001;
pub const CSR_FRM            : u32 = 0x002;
pub const CSR_FCSR           : u32 = 0x003;
pub const CSR_CYCLE          : u32 = 0xC00;
pub const CSR_TIME           : u32 = 0xC01;
pub const CSR_INSTRET        : u32 = 0xC02;
pub const CSR_HPMCOUNTER3    : u32 = 0xC03;
pub const CSR_HPMCOUNTER4    : u32 = 0xC04;
pub const CSR_HPMCOUNTER31   : u32 = 0xC1F;
pub const CSR_CYCLEH         : u32 = 0xC80;
pub const CSR_TIMEH          : u32 = 0xC81;
pub const CSR_INSTRETH       : u32 = 0xC82;
pub const CSR_HPMCOUNTER3H   : u32 = 0xC83;
pub const CSR_HPMCOUNTER4H   : u32 = 0xC84;
pub const CSR_HPMCOUNTER31H  : u32 = 0xC9F;
// Supervisor mode
pub const CSR_SSTATUS        : u32 = 0x100;
pub const CSR_SEDELEG        : u32 = 0x102;
pub const CSR_SIDELEG        : u32 = 0x103;
pub const CSR_SIE            : u32 = 0x104;
pub const CSR_STVEC          : u32 = 0x105;
pub const CSR_SCOUNTEREN     : u32 = 0x106;
pub const CSR_SSCRATCH       : u32 = 0x140;
pub const CSR_SEPC           : u32 = 0x141;
pub const CSR_SCAUSE         : u32 = 0x142;
pub const CSR_STVAL          : u32 = 0x143;
pub const CSR_SIP            : u32 = 0x144;
pub const CSR_SATP           : u32 = 0x180;
// Machine mode
pub const CSR_MVENDORID      : u32 = 0xF11;
pub const CSR_MARCHID        : u32 = 0xF12;
pub const CSR_MIMPID         : u32 = 0xF13;
pub const CSR_MHARTID        : u32 = 0xF14;
pub const CSR_MSTATUS        : u32 = 0x300;
pub const CSR_MISA           : u32 = 0x301;
pub const CSR_MEDELEG        : u32 = 0x302;
pub const CSR_MIDELEG        : u32 = 0x303;
pub const CSR_MIE            : u32 = 0x304;
pub const CSR_MTVEC          : u32 = 0x305;
pub const CSR_MCOUNTEREN     : u32 = 0x306;
pub const CSR_MSCRATCH       : u32 = 0x340;
pub const CSR_MEPC           : u32 = 0x341;
pub const CSR_MCAUSE         : u32 = 0x342;
pub const CSR_MTVAL          : u32 = 0x343;
pub const CSR_MIP            : u32 = 0x344;
pub const CSR_PMPCFG0        : u32 = 0x3A0;
pub const CSR_PMPCFG1        : u32 = 0x3A1;
pub const CSR_PMPCFG2        : u32 = 0x3A2;
pub const CSR_PMPCFG3        : u32 = 0x3A3;
pub const CSR_PMPADDR0       : u32 = 0x3B0;
pub const CSR_PMPADDR1       : u32 = 0x3B1;
pub const CSR_PMPADDR15      : u32 = 0x3BF;
pub const CSR_MCYCLE         : u32 = 0xB00;
pub const CSR_MINSTRET       : u32 = 0xB02;
pub const CSR_MHPMCOUNTER3   : u32 = 0xB03;
pub const CSR_MHPMCOUNTER4   : u32 = 0xB04;
pub const CSR_MHPMCOUNTER31  : u32 = 0xB1F;
pub const CSR_MCYCLEH        : u32 = 0xB80;
pub const CSR_MINSTRETH      : u32 = 0xB82;
pub const CSR_MHPMCOUNTER3H  : u32 = 0xB83;
pub const CSR_MHPMCOUNTER4H  : u32 = 0xB84;
pub const CSR_MHPMCOUNTER31H : u32 = 0xB9F;
pub const CSR_MCOUNTINHIBIT  : u32 = 0x320;
pub const CSR_MHPMEVENT3     : u32 = 0x323;
pub const CSR_MHPMEVENT4     : u32 = 0x324;
pub const CSR_MHPMEVENT31    : u32 = 0x33F;
pub const CSR_TSELECT        : u32 = 0x7A0;
pub const CSR_TDATA1         : u32 = 0x7A1;
pub const CSR_TDATA2         : u32 = 0x7A2;
pub const CSR_TDATA3         : u32 = 0x7A3;
// Device
pub const CSR_DCSR           : u32 = 0x7B0;
pub const CSR_DPC            : u32 = 0x7B1;
pub const CSR_DSCRATCH       : u32 = 0x7B2;

pub struct ControlStatusRegister {
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

impl ControlStatusRegister {
    pub fn new() -> ControlStatusRegister {
        ControlStatusRegister {
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
        }
    }
    pub fn get_name(addr: u32) -> &'static str {
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
                writeln!(std::io::stderr(), "{}: {}: unknown csr addr 0x{:x}", file!(), line!(), addr).unwrap();
                std::process::exit(1);
            }
        }
    }

    pub fn get(&self, addr: u32) -> u64 {
        match addr {
            // User mode
            CSR_USTATUS        => self.ustatus,
            CSR_UIE            => self.uie,
            CSR_UTVEC          => self.utvec,
            CSR_USCRATCH       => self.uscratch,
            CSR_UEPC           => self.uepc,
            CSR_UCAUSE         => self.ucause,
            CSR_UTVAL          => self.utval,
            CSR_UIP            => self.uip,
            CSR_FFLAGS         => self.fflags,
            CSR_FRM            => self.frm,
            CSR_FCSR           => self.fcsr,
            CSR_CYCLE          => self.cycle,
            CSR_TIME           => self.time,
            CSR_INSTRET        => self.instret,
            CSR_HPMCOUNTER3    => self.hpmcounter3,
            CSR_HPMCOUNTER4    => self.hpmcounter4,
            CSR_HPMCOUNTER31   => self.hpmcounter31,
            CSR_CYCLEH         => self.cycleh,
            CSR_TIMEH          => self.timeh,
            CSR_INSTRETH       => self.instreth,
            CSR_HPMCOUNTER3H   => self.hpmcounter3h,
            CSR_HPMCOUNTER4H   => self.hpmcounter4h,
            CSR_HPMCOUNTER31H  => self.hpmcounter31h,
            // Supervisor mode
            CSR_SSTATUS        => self.sstatus,
            CSR_SEDELEG        => self.sedeleg,
            CSR_SIDELEG        => self.sideleg,
            CSR_SIE            => self.sie,
            CSR_STVEC          => self.stvec,
            CSR_SCOUNTEREN     => self.scounteren,
            CSR_SSCRATCH       => self.sscratch,
            CSR_SEPC           => self.sepc,
            CSR_SCAUSE         => self.scause,
            CSR_STVAL          => self.stval,
            CSR_SIP            => self.sip,
            CSR_SATP           => self.satp,
            // Machine mode
            CSR_MVENDORID      => self.mvendorid,
            CSR_MARCHID        => self.marchid,
            CSR_MIMPID         => self.mimpid,
            CSR_MHARTID        => self.mhartid,
            CSR_MSTATUS        => self.mstatus,
            CSR_MISA           => self.misa,
            CSR_MEDELEG        => self.medeleg,
            CSR_MIDELEG        => self.mideleg,
            CSR_MIE            => self.mie,
            CSR_MTVEC          => self.mtvec,
            CSR_MCOUNTEREN     => self.mcounteren,
            CSR_MSCRATCH       => self.mscratch,
            CSR_MEPC           => self.mepc,
            CSR_MCAUSE         => self.mcause,
            CSR_MTVAL          => self.mtval,
            CSR_MIP            => self.mip,
            CSR_PMPCFG0        => self.pmpcfg0,
            CSR_PMPCFG1        => self.pmpcfg1,
            CSR_PMPCFG2        => self.pmpcfg2,
            CSR_PMPCFG3        => self.pmpcfg3,
            CSR_PMPADDR0       => self.pmpaddr0,
            CSR_PMPADDR1       => self.pmpaddr1,
            CSR_PMPADDR15      => self.pmpaddr15,
            CSR_MCYCLE         => self.mcycle,
            CSR_MINSTRET       => self.minstret,
            CSR_MHPMCOUNTER3   => self.mhpmcounter3,
            CSR_MHPMCOUNTER4   => self.mhpmcounter4,
            CSR_MHPMCOUNTER31  => self.mhpmcounter31,
            CSR_MCYCLEH        => self.mcycleh,
            CSR_MINSTRETH      => self.minstreth,
            CSR_MHPMCOUNTER3H  => self.mhpmcounter3h,
            CSR_MHPMCOUNTER4H  => self.mhpmcounter4h,
            CSR_MHPMCOUNTER31H => self.mhpmcounter31h,
            CSR_MCOUNTINHIBIT  => self.mcountinhibit,
            CSR_MHPMEVENT3     => self.mhpmevent3,
            CSR_MHPMEVENT4     => self.mhpmevent4,
            CSR_MHPMEVENT31    => self.mhpmevent31,
            CSR_TSELECT        => self.tselect,
            CSR_TDATA1         => self.tdata1,
            CSR_TDATA2         => self.tdata2,
            CSR_TDATA3         => self.tdata3,
            // Device
            CSR_DCSR           => self.dcsr,
            CSR_DPC            => self.dpc,
            CSR_DSCRATCH       => self.dscratch,
            _ => {
                writeln!(std::io::stderr(), "{}: {}: unknown csr addr 0x{:x}", file!(), line!(), addr).unwrap();
                std::process::exit(1);
            }
        }
    }

    pub fn set(&mut self, addr: u32, val: u64) {
        match addr {
            // User mode
            CSR_USTATUS        => {self.ustatus = val;},
            CSR_UIE            => {self.uie = val;},
            CSR_UTVEC          => {self.utvec = val;},
            CSR_USCRATCH       => {self.uscratch = val;},
            CSR_UEPC           => {self.uepc = val;},
            CSR_UCAUSE         => {self.ucause = val;},
            CSR_UTVAL          => {self.utval = val;},
            CSR_UIP            => {self.uip = val;},
            CSR_FFLAGS         => {self.fflags = val;},
            CSR_FRM            => {self.frm = val;},
            CSR_FCSR           => {self.fcsr = val;},
            CSR_CYCLE          => {self.cycle = val;},
            CSR_TIME           => {self.time = val;},
            CSR_INSTRET        => {self.instret = val;},
            CSR_HPMCOUNTER3    => {self.hpmcounter3 = val;},
            CSR_HPMCOUNTER4    => {self.hpmcounter4 = val;},
            CSR_HPMCOUNTER31   => {self.hpmcounter31 = val;},
            CSR_CYCLEH         => {self.cycleh = val;},
            CSR_TIMEH          => {self.timeh = val;},
            CSR_INSTRETH       => {self.instreth = val;},
            CSR_HPMCOUNTER3H   => {self.hpmcounter3h = val;},
            CSR_HPMCOUNTER4H   => {self.hpmcounter4h = val;},
            CSR_HPMCOUNTER31H  => {self.hpmcounter31h = val;},
            // Supervisor mode
            CSR_SSTATUS        => {self.sstatus = val;},
            CSR_SEDELEG        => {self.sedeleg = val;},
            CSR_SIDELEG        => {self.sideleg = val;},
            CSR_SIE            => {self.sie = val;},
            CSR_STVEC          => {self.stvec = val;},
            CSR_SCOUNTEREN     => {self.scounteren = val;},
            CSR_SSCRATCH       => {self.sscratch = val;},
            CSR_SEPC           => {self.sepc = val;},
            CSR_SCAUSE         => {self.scause = val;},
            CSR_STVAL          => {self.stval = val;},
            CSR_SIP            => {self.sip = val;},
            CSR_SATP           => {self.satp = val;},
            // Machine mode
            CSR_MVENDORID      => {self.mvendorid = val;},
            CSR_MARCHID        => {self.marchid = val;},
            CSR_MIMPID         => {self.mimpid = val;},
            CSR_MHARTID        => {self.mhartid = val;},
            CSR_MSTATUS        => {self.mstatus = val;},
            CSR_MISA           => {self.misa = val;},
            CSR_MEDELEG        => {self.medeleg = val;},
            CSR_MIDELEG        => {self.mideleg = val;},
            CSR_MIE            => {self.mie = val;},
            CSR_MTVEC          => {self.mtvec = val;},
            CSR_MCOUNTEREN     => {self.mcounteren = val;},
            CSR_MSCRATCH       => {self.mscratch = val;},
            CSR_MEPC           => {self.mepc = val;},
            CSR_MCAUSE         => {self.mcause = val;},
            CSR_MTVAL          => {self.mtval = val;},
            CSR_MIP            => {self.mip = val;},
            CSR_PMPCFG0        => {self.pmpcfg0 = val;},
            CSR_PMPCFG1        => {self.pmpcfg1 = val;},
            CSR_PMPCFG2        => {self.pmpcfg2 = val;},
            CSR_PMPCFG3        => {self.pmpcfg3 = val;},
            CSR_PMPADDR0       => {self.pmpaddr0 = val;},
            CSR_PMPADDR1       => {self.pmpaddr1 = val;},
            CSR_PMPADDR15      => {self.pmpaddr15 = val;},
            CSR_MCYCLE         => {self.mcycle = val;},
            CSR_MINSTRET       => {self.minstret = val;},
            CSR_MHPMCOUNTER3   => {self.mhpmcounter3 = val;},
            CSR_MHPMCOUNTER4   => {self.mhpmcounter4 = val;},
            CSR_MHPMCOUNTER31  => {self.mhpmcounter31 = val;},
            CSR_MCYCLEH        => {self.mcycleh = val;},
            CSR_MINSTRETH      => {self.minstreth = val;},
            CSR_MHPMCOUNTER3H  => {self.mhpmcounter3h = val;},
            CSR_MHPMCOUNTER4H  => {self.mhpmcounter4h = val;},
            CSR_MHPMCOUNTER31H => {self.mhpmcounter31h = val;},
            CSR_MCOUNTINHIBIT  => {self.mcountinhibit = val;},
            CSR_MHPMEVENT3     => {self.mhpmevent3 = val;},
            CSR_MHPMEVENT4     => {self.mhpmevent4 = val;},
            CSR_MHPMEVENT31    => {self.mhpmevent31 = val;},
            CSR_TSELECT        => {self.tselect = val;},
            CSR_TDATA1         => {self.tdata1 = val;},
            CSR_TDATA2         => {self.tdata2 = val;},
            CSR_TDATA3         => {self.tdata3 = val;},
            // Device
            CSR_DCSR           => {self.dcsr = val;},
            CSR_DPC            => {self.dpc = val;},
            CSR_DSCRATCH       => {self.dscratch = val;},
            _ => {
                writeln!(std::io::stderr(), "{}: {}: unknown csr addr 0x{:x}", file!(), line!(), addr).unwrap();
                std::process::exit(1);
            }
        }
    }

    pub fn get_mpp(&self) -> u8 {
        ((self.mstatus >> 11) & 0b11) as u8
    }
}
