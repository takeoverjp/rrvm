extern crate memmap;

use elf::Elf;

#[derive(Debug)]
pub struct Memory<'a> {
    map: &'a memmap::Mmap,
    elf: &'a Elf,
    entry_point_address: u64,
    entry_point_offset: u64,
}

fn u8x4_to_u32(x0:u8, x1:u8, x2:u8, x3:u8) -> u32 {
    return (x0 as u32)
        | ((x1 as u32) << 8)
        | ((x2 as u32) << 16)
        | ((x3 as u32) << 24);
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

    pub fn lw(&self, addr: u64) -> u32 {
        let index = (addr - self.entry_point_address + self.entry_point_offset) as usize;
        u8x4_to_u32(self.map[index+0], self.map[index+1], self.map[index+2], self.map[index+3])
    }

    pub fn _lb(&self, addr: u64) -> u8 {
        self.map[addr as usize]
    }
}
