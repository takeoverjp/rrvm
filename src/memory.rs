extern crate memmap;

use elf::Elf;

#[derive(Debug)]
pub struct Memory<'a> {
    map: &'a memmap::Mmap,
    elf: &'a Elf,
    entry_point_address: u64,
    entry_point_offset: u64,
}

impl<'a> Memory<'a> {
    pub fn new(map: &'a memmap::Mmap, elf: &'a Elf) -> Memory<'a> {
        Memory {
            map: map,
            elf: elf,
            entry_point_address: elf.entry_point_address(),
            entry_point_offset: elf.entry_point_offset().unwrap(),
        }
    }

    pub fn ld(&self, addr: u64) -> u64 {
        self.addr2data(addr, 8)
    }

    pub fn lw(&self, addr: u64) -> u32 {
        self.addr2data(addr, 4) as u32
    }

    pub fn lh(&self, addr: u64) -> u16 {
        self.addr2data(addr, 2) as u16
    }

    pub fn lb(&self, addr: u64) -> u8 {
        self.addr2data(addr, 1) as u8
    }

    fn addr2data(&self, addr: u64, size: u64) -> u64 {
        let index = self.addr2index(addr);
        let mut data: u64 = 0;
        for cnt in 0..size {
            data |= (self.map[index + cnt as usize] as u64) << (cnt * 8);
        }

        data
    }

    fn addr2index(&self, addr: u64) -> usize {
        (addr - self.entry_point_address + self.entry_point_offset) as usize
    }
}
