use armv4t_emu::Memory;
use std::collections::HashMap;

pub const NEG: i32 = (-1 * 0x7fffffff) - 1;

pub trait TargetMemory {
    fn write_data(&mut self, content: &[u8], target_addr: u32);

    fn write_i32(&mut self, target_addr: u32, value: i32);
    fn write_u32(&mut self, target_addr: u32, value: u32);

    fn read_i32(&self, target_addr: u32) -> i32;
    fn read_u32(&self, target_addr: u32) -> u32;

    fn read_u8(&self, target_addr: u32) -> u8;
}

pub fn write_u32(content: &mut [u8], offset: usize, value: u32) {
    content[offset] = (value & 0xff) as u8;
    content[offset + 1] = ((value >> 8) & 0xff) as u8;
    content[offset + 2] = ((value >> 16) & 0xff) as u8;
    content[offset + 3] = ((value >> 24) & 0xff) as u8;
}

pub fn read_u16(content: &[u8], offset: usize) -> u16 {
    (content[offset] as u16) | ((content[offset + 1] as u16) << 8)
}

pub fn read_u24(content: &[u8], offset: usize) -> u32 {
    read_u16(content, offset) as u32 | ((content[offset + 2] as u32) << 16)
}

pub fn read_u32(content: &[u8], offset: usize) -> u32 {
    read_u24(content, offset) | ((content[offset + 3] as u32) << 24)
}

pub fn read_i32(content: &[u8], offset: usize) -> i32 {
    let first_24 = read_u24(content, offset) as i32;
    let msb = content[offset + 3] as i32;
    if (msb & 0x80) != 0 {
        NEG + (first_24 | ((msb & 0x7f) << 24))
    } else {
        first_24 | (msb << 24)
    }
}

#[derive(Clone)]
pub struct PagedMemory {
    zeroed: Vec<u32>,
    pages: HashMap<u32, Vec<u32>>,
}

impl Default for PagedMemory {
    fn default() -> Self {
        let mut zeroed = Vec::new();
        for _z in 0..1024 {
            zeroed.push(0);
        }
        PagedMemory {
            zeroed,
            pages: HashMap::default(),
        }
    }
}

impl PagedMemory {
    fn get_slice(&self, addr: u32) -> (usize, Option<&Vec<u32>>) {
        let selection = ((addr >> 2) & 0x3ff) as usize;
        (selection, self.pages.get(&(addr & 0xfffff000)))
    }

    fn get_mut_slice(&mut self, addr: u32, create: bool) -> (usize, Option<&mut Vec<u32>>) {
        let selection = ((addr >> 2) & 0x3ff) as usize;
        let target = addr & 0xfffff000;
        if self.pages.contains_key(&target) {
            // Ensure the top level doesn't suffer lifetime polution.
            if let Some(s) = self.pages.get_mut(&target) {
                return (selection, Some(s));
            } else {
                todo!();
            }
        } else {
            if create {
                self.pages.insert(target, self.zeroed.clone());
                return self.get_mut_slice(addr, create);
            } else {
                (selection, None)
            }
        }
    }
}

impl Memory for PagedMemory {
    fn r8(&mut self, addr: u32) -> u8 {
        let (selection, slice) = if let (sel, Some(s)) = self.get_slice(addr) {
            (sel, s)
        } else {
            return 0;
        };

        let offset = addr & 3;
        let selected = slice[selection];
        ((selected >> (offset * 8)) & 0xff) as u8
    }

    fn r16(&mut self, addr: u32) -> u16 {
        let (selection, slice) = if let (sel, Some(s)) = self.get_slice(addr) {
            (sel, s)
        } else {
            return 0;
        };

        let offset = addr & 3;
        let selected = slice[selection];
        ((selected >> (offset * 16)) & 0xffff) as u16
    }

    fn r32(&mut self, addr: u32) -> u32 {
        let (selection, slice) = if let (sel, Some(s)) = self.get_slice(addr) {
            (sel, s)
        } else {
            return 0;
        };

        slice[selection]
    }

    fn w8(&mut self, addr: u32, val: u8) {
        let (selection, slice) = if let (sel, Some(s)) = self.get_mut_slice(addr, true) {
            (sel, s)
        } else {
            todo!();
        };

        let offset = (addr & 3) * 8;
        let mask = 0xff << offset;
        slice[selection] = slice[selection] & !mask | ((val as u32) << offset);
    }

    fn w16(&mut self, addr: u32, val: u16) {
        let (selection, slice) = if let (sel, Some(s)) = self.get_mut_slice(addr, true) {
            (sel, s)
        } else {
            todo!();
        };

        let offset = (addr & 3) * 8;
        let mask = 0xffff << offset;
        slice[selection] = slice[selection] & !mask | ((val as u32) << offset);
    }

    fn w32(&mut self, addr: u32, val: u32) {
        let (selection, slice) = if let (sel, Some(s)) = self.get_mut_slice(addr, true) {
            (sel, s)
        } else {
            todo!();
        };

        slice[selection] = val;
    }
}

impl TargetMemory for PagedMemory {
    fn write_data(&mut self, content: &[u8], target_addr: u32) {
        for (i, b) in content.iter().enumerate() {
            self.w8(target_addr + i as u32, *b);
        }
    }
    fn write_i32(&mut self, target_addr: u32, value: i32) {
        let stripped = (value & 0x7fffffff) as u32;
        if value < 0 {
            self.write_u32(target_addr, stripped | 0x80000000);
        } else {
            self.write_u32(target_addr, stripped);
        }
    }
    fn write_u32(&mut self, target_addr: u32, value: u32) {
        self.w32(target_addr, value);
    }

    fn read_i32(&self, target_addr: u32) -> i32 {
        let uread = self.read_u32(target_addr);
        let cvt1 = (uread & 0x7fffffff) as i32;
        if (uread & 0x80000000) != 0 {
            (NEG + cvt1) as i32
        } else {
            uread as i32
        }
    }

    fn read_u32(&self, target_addr: u32) -> u32 {
        let (selection, slice) = if let (sel, Some(s)) = self.get_slice(target_addr) {
            (sel, s)
        } else {
            return 0;
        };

        slice[selection]
    }

    fn read_u8(&self, target_addr: u32) -> u8 {
        let (selection, slice) = if let (sel, Some(s)) = self.get_slice(target_addr) {
            (sel, s)
        } else {
            return 0;
        };

        let word = slice[selection];
        (word >> (8 * (target_addr & 3))) as u8
    }
}
