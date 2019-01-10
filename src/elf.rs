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
            e_type: (bin[0x10] as u16) | (bin[0x11] as u16) << 8,
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
  e_type: {}",
               self.class2str(),
               self.data2str(),
               self.version2str(),
               self.osabi2str(),
               self.ei_abiversion,
               self.type2str(),
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
        ];
        let hdr = ElfHeader::new(&bin);
        assert_eq!(true, hdr.is_elf());
    }
}
