// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::cartridge::Rom;

const RAM: u16 = 0x0000;
const RAM_MIRROR_END: u16 = 0x01FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRROR_END: u16 = 0x3FFF;
pub const ROM: u16 = 0x8000;

pub trait Memory 
{
    fn mem_read(&self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&self, addr: u16) -> u16 {
        let low = self.mem_read(addr) as u16;
        let high = self.mem_read(addr+1) as u16;

        (high << 8) | low
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        let high = (data >> 8) as u8;
        let low = data as u8;
        self.mem_write(addr, low);
        self.mem_write(addr+1, high);
    }
}

pub struct Bus {
    cpu_vram: [u8; 2048],
    rom: Option<Rom>,
}

impl Default for Bus {
    fn default() -> Self {
        Self::new()
    }
}

impl Bus {
    pub fn new() -> Self {
        Bus {
            cpu_vram: [0; 2048],
            rom: None,
        }
    }

    pub fn load_rom(&mut self, rom: Rom) {
        self.rom = Some(rom);
    }

    pub fn read_prg_rom(&self, addr: u16) -> u8 {
        match &self.rom {
            None => {
                println!("Attempting to read with no loaded Rom. Returning 0x00.");
                0x00
            }
            Some(rom) => {
                let mut adjusted_addr = addr - ROM;
                if rom.prg_rom.len() == 0x4000 && adjusted_addr >= 0x4000 {
                    adjusted_addr %= 0x4000;
                }

                if (adjusted_addr as usize) < rom.prg_rom.len() {
                    rom.prg_rom[adjusted_addr as usize]
                }
                else {
                    0x00
                }

            }
        }
    }
}

impl Memory for Bus {
    fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            RAM ..= RAM_MIRROR_END => {
                //println!("Bus::mem_read -- reading from ram at addr: {:X}", addr);
                let mirror_down_addr = addr & 0b00000111_11111111;
                self.cpu_vram[mirror_down_addr as usize]
            }

            PPU_REGISTERS ..= PPU_REGISTERS_MIRROR_END => {
                let _mirror_down_addr = addr & 0b00100000_00000111;
                todo!("PPU is not supported yet. {:X}", addr)
            }

            ROM ..= 0xFFFF => {
                self.read_prg_rom(addr)
            }

            _=> {
                println!("Bus::mem_read -- ignoring memory access at {:X}", addr);
                0
            }
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM ..= RAM_MIRROR_END => {
                //println!("Bus::mem_write -- writing {:X} to ram at addr: {:X}", data, addr);
                let mirror_down_addr = addr & 0b00000111_11111111;
                self.cpu_vram[mirror_down_addr as usize] = data;
                //let value = self.mem_read(addr);
                //println!("Bus::mem_write -- reading {:X} to ram at addr: {:X}", data, addr);
            }

            PPU_REGISTERS ..= PPU_REGISTERS_MIRROR_END => {
                let _mirror_down_addr = addr & 0b00100000_00000111;
                todo!("PPU is not supported yet.")
            }

            ROM ..= 0xFFFF => {
                panic!("Attempting to write to the cartridge rom with addr {:x}", addr);
            }

            _=> {
                println!("Bus::mem_read -- ignoring memory access at {:x}", addr);
            }
        }        
    }
}