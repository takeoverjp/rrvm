struct ElfHeader {
    e_ident: [u8; 4],
    e_type: u8,
    e_machine: u8,
    e_version: u8,
    e_entry: u8,
}

impl ElfHeader {
    pub fn new(bin: &Vec<u8>) -> ElfHeader {
        ElfHeader {
            e_ident: [bin[0], bin[1], bin[2], bin[3]],
            e_type: bin[4],
            e_machine: bin[5],
            e_version: bin[6],
            e_entry: bin[7],
        }
    }
    pub fn is_elf(self) -> bool {
        self.e_ident == [0x7f, b'E', b'L', b'F']
    }
}

#[cfg(test)]
mod test_elf {
    use super::*;

    #[test]
    fn test_is_elf() {
        let bin = vec![0x7f, b'E', b'L', b'F', 0, 0, 0, 0];
        let hdr = ElfHeader::new(&bin);
        assert_eq!(true, hdr.is_elf());
    }
}
