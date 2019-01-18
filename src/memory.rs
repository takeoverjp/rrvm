extern crate memmap;

use elf::Elf;

#[derive(Debug)]
pub struct Memory<'a> {
    map: &'a memmap::Mmap,
    elf: &'a Elf,
    entry_point_address: u64,
    entry_point_offset: u64,
}

fn u8x8_to_u64(x0:u8, x1:u8, x2:u8, x3:u8,
               x4:u8, x5:u8, x6:u8, x7:u8) -> u64 {
    (x0 as u64)
        | ((x1 as u64) << 8)
        | ((x2 as u64) << 16)
        | ((x3 as u64) << 24)
        | ((x4 as u64) << 32)
        | ((x5 as u64) << 40)
        | ((x6 as u64) << 48)
        | ((x7 as u64) << 56)
}

fn u8x4_to_u32(x0:u8, x1:u8, x2:u8, x3:u8) -> u32 {
    (x0 as u32)
        | ((x1 as u32) << 8)
        | ((x2 as u32) << 16)
        | ((x3 as u32) << 24)
}

fn u8x2_to_u16(x0:u8, x1:u8) -> u16 {
    (x0 as u16)
        | ((x1 as u16) << 8)
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
        let index = (addr - self.entry_point_address + self.entry_point_offset) as usize;
        u8x8_to_u64(self.map[index+0], self.map[index+1], self.map[index+2], self.map[index+3],
                    self.map[index+4], self.map[index+5], self.map[index+6], self.map[index+7])
    }

    pub fn lw(&self, addr: u64) -> u32 {
        let index = (addr - self.entry_point_address + self.entry_point_offset) as usize;
        u8x4_to_u32(self.map[index+0], self.map[index+1], self.map[index+2], self.map[index+3])
    }

    pub fn lh(&self, addr: u64) -> u16 {
        let index = (addr - self.entry_point_address + self.entry_point_offset) as usize;
        u8x2_to_u16(self.map[index+0], self.map[index+1])
    }

    pub fn lb(&self, addr: u64) -> u8 {
        self.map[addr as usize]
    }
}
