use std::fmt;

pub struct ElfHeader {
    ei_magic: [u8; 4],
    ei_class: u8,
    ei_data: u8,
    ei_version: u8,
    ei_osabi: u8,
    ei_abiversion: u8,
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
        }

    }
    pub fn is_elf(&self) -> bool {
        self.ei_magic == [0x7f, b'E', b'L', b'F']
    }
}

impl fmt::Display for ElfHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.ei_magic != [0x7f, b'E', b'L', b'F'] {
            return Ok(());
        }

        write!(f, r"== ELF ==
  ei_class: {}
  ei_data: {}
  ei_version: {}
  ei_osabi: {}
  ei_abiversion: {}",
               self.ei_class,
               self.ei_data,
               self.ei_version,
               self.ei_osabi,
               self.ei_abiversion)
    }
}

#[cfg(test)]
mod test_elf {
    use super::*;

    #[test]
    fn test_is_elf() {
        let bin = vec![0x7f, b'E', b'L', b'F', 0, 0, 0, 0, 0];
        let hdr = ElfHeader::new(&bin);
        assert_eq!(true, hdr.is_elf());
    }
}
