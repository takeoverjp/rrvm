use elf::Elf;

#[derive(Debug)]
pub struct Memory<'a> {
    map: &'a mut Vec<u8>,
    elf: &'a Elf,
    entry_point_address: u64,
    entry_point_offset: u64,
}

impl<'a> Memory<'a> {
    pub fn new(vec: &'a mut Vec<u8>, elf: &'a Elf) -> Memory<'a> {
        Memory {
            map: vec,
            elf: elf,
            entry_point_address:
            if elf.is_elf() {
                elf.entry_point_address()
            } else {
                0
            },
            entry_point_offset:
            if elf.is_elf() {
                elf.entry_point_offset().unwrap()
            } else {
                0
            },
        }
    }

    fn addr2index(&self, addr: u64) -> usize {
        (addr - self.entry_point_address + self.entry_point_offset) as usize
    }

    fn load(&self, addr: u64, size: u64) -> u64 {
        let index = self.addr2index(addr);
        let mut data: u64 = 0;
        for cnt in 0..size {
            data |= (self.map[index + cnt as usize] as u64) << (cnt * 8);
        }

        data
    }

    pub fn ld(&self, addr: u64) -> u64 {
        self.load(addr, 8)
    }

    pub fn lw(&self, addr: u64) -> u32 {
        self.load(addr, 4) as u32
    }

    pub fn lh(&self, addr: u64) -> u16 {
        self.load(addr, 2) as u16
    }

    pub fn lb(&self, addr: u64) -> u8 {
        self.load(addr, 1) as u8
    }

    fn store(&mut self, addr: u64, data: u64, size: u64) {
        let index = self.addr2index(addr);
        for cnt in 0..size {
            self.map[index + cnt as usize] = ((data >> (cnt * 8)) & 0xff) as u8;
        }
    }

    pub fn sd(&mut self, addr: u64, data: u64) {
        self.store(addr, data, 8)
    }

    pub fn sw(&mut self, addr: u64, data: u32) {
        self.store(addr, data as u64, 4)
    }

    pub fn sh(&mut self, addr: u64, data: u16) {
        self.store(addr, data as u64, 2)
    }

    pub fn sb(&mut self, addr: u64, data: u8) {
        self.store(addr, data as u64, 1)
    }
}
