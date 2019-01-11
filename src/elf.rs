use std::fmt;

#[derive(Debug)]
pub struct ElfHeader {
    ei_magic: [u8; 4],
    ei_class: u8,
    ei_data: u8,
    ei_version: u8,
    ei_osabi: u8,
    ei_abiversion: u8,
    _ei_pad: [u8; 7],
    e_type: u16,
    e_machine: u16,
    e_version: u32,
    e_entry: u64,
    e_phoff: u64,
    e_shoff: u64,
    e_flags: u32,
}

impl ElfHeader {
    pub fn new(bin: &Vec<u8>) -> ElfHeader {
        ElfHeader {
            ei_magic: [bin[0], bin[1], bin[2], bin[3]],
            ei_class: bin[4],
            ei_data: bin[5],
            ei_version: bin[6],
            ei_osabi: bin[7],
            ei_abiversion: bin[8],
            _ei_pad: [0; 7],
            e_type: sli2u16(&bin[0x10..0x12]),
            e_machine: sli2u16(&bin[0x12..0x14]),
            e_version: sli2u32(&bin[0x14..0x18]),
            e_entry: sli2u64(&bin[0x18..0x20]),
            e_phoff: sli2u64(&bin[0x20..0x28]),
            e_shoff: sli2u64(&bin[0x28..0x30]),
            e_flags: sli2u32(&bin[0x30..0x34]),
        }

    }
    pub fn is_elf(&self) -> bool {
        self.ei_magic == [0x7f, b'E', b'L', b'F']
    }

    fn class2str(&self) -> &str {
        match self.ei_class {
            1 => "32bit",
            2 => "64bit",
            _ => "unknown",
        }
    }

    fn data2str(&self) -> &str {
        match self.ei_data {
            1 => "LE",
            2 => "BE",
            _ => "unknown",
        }
    }

    fn version2str(&self) -> &str {
        match self.ei_version {
            1 => "original version",
            _ => "unknown",
        }
    }

    fn osabi2str(&self) -> &str {
        match self.ei_osabi {
            0x00 => "System V",
            _ => "unknown",
        }
    }

    fn type2str(&self) -> &str {
        match self.e_type {
            0x00 => "ET_NONE",
            0x01 => "ET_REL",
            0x02 => "ET_EXEC",
            _ => "unknown",
        }
    }

    fn machine2str(&self) -> &str {
        match self.e_machine {
            0x00 => "No specific",
            0x02 => "SPARC",
            0x03 => "x86",
            0x28 => "ARM",
            0x32 => "IA-64",
            0x3E => "x86-64",
            0xB7 => "AArch64",
            0xF3 => "RISC-V",
            _ => "unknown",
        }
    }
}

impl fmt::Display for ElfHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if ! self.is_elf() {
            return write!(f, "not ELF");
        }

        write!(f, r"== ELF ==
  ei_class: {}
  ei_data: {}
  ei_version: {}
  ei_osabi: {}
  ei_abiversion: {}
  e_type: {}
  e_machine: {}",
               self.class2str(),
               self.data2str(),
               self.version2str(),
               self.osabi2str(),
               self.ei_abiversion,
               self.type2str(),
               self.machine2str(),
        )
    }
}

#[cfg(test)]
mod test_elf {
    use super::*;

    #[test]
    fn test_is_elf() {
        let bin = vec![0x7f, b'E', b'L', b'F', 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0,
        ];
        let hdr = ElfHeader::new(&bin);
        assert_eq!(true, hdr.is_elf());
    }
}


fn sli2u16(s: &[u8]) -> u16 {
    assert_eq!(2, s.len());
    (s[0] as u16) | (s[1] as u16) << 8
}

fn sli2u32(s: &[u8]) -> u32 {
    assert_eq!(4, s.len());
    (s[0] as u32) | (s[1] as u32) << 8 | (s[2] as u32) << 16 | (s[3] as u32) << 24
}

fn sli2u64(s: &[u8]) -> u64 {
    assert_eq!(8, s.len());
    (s[0] as u64) | (s[1] as u64) << 8 | (s[2] as u64) << 16 | (s[3] as u64) << 24
    | (s[4] as u64) << 32 | (s[5] as u64) << 40 | (s[6] as u64) << 48 | (s[7] as u64) << 56
}

#[test]
fn test_sli2u16() {
    let ret = sli2u16(&[0xab, 0xcd]);
    assert_eq!(0xcdab, ret, "0x{:04x}", ret);
}

#[test]
fn test_sli2u32() {
    let ret = sli2u32(&[0x12, 0x34, 0xab, 0xcd]);
    assert_eq!(0xcdab3412, ret, "0x{:08x}", ret);
}

#[test]
fn test_sli2u64() {
    let ret = sli2u64(&[0x01, 0x23, 0x45, 0x67,
                        0x89, 0xab, 0xcd, 0xef]);
    assert_eq!(0xefcdab8967452301, ret, "0x{:016x}", ret);
}
