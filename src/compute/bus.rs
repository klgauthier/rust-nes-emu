// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::{cartridge::{Mirroring, Rom}, graphics::ppu::PPU, utils::errors::MemReadError};

const RAM: u16 = 0x0000;
const RAM_MIRROR_END: u16 = 0x01FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRROR_END: u16 = 0x3FFF;
pub const ROM: u16 = 0x8000;

pub const INTERRUPT_VECTOR_NMI: u16 = 0xFFFA;
pub const INTERRUPT_VECTOR_RESET: u16 = 0xFFFC;
pub const INTERRUPT_VECTOR_IRQ: u16 = 0xFFFE;

pub trait Memory 
{
    fn mem_read(&mut self, addr: u16) -> Result<u8, MemReadError>;
    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&mut self, addr: u16) -> Result<u16, MemReadError> {
        let low = self.mem_read(addr)? as u16;
        let high = self.mem_read(addr+1)? as u16;

        Ok((high << 8) | low)
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
    ppu: PPU,

    pub cycles: usize,
}

impl Default for Bus {
    fn default() -> Self {
        Self::new()
    }
}

impl Bus {
    pub fn new() -> Self {
        let ppu = PPU::new(vec![], Mirroring::FourScreen);
        Bus {
            cpu_vram: [0; 2048],
            rom: None,
            ppu,
            cycles: 0,
        }
    }
    
    pub fn tick(&mut self, cycles_increment: u8) {
        self.cycles += cycles_increment as usize;
        self.ppu.tick(cycles_increment * 3);
    }

    pub fn poll_nmi_interrupt(&mut self) -> bool {
        self.ppu.poll_nmi_interrupt()
    }

    pub fn load_rom(&mut self, rom: Rom) {
        self.ppu.chr_rom = rom.chr_rom.clone();
        self.ppu.mirroring = rom.screen_mirroring;
        self.rom = Some(rom);
    }

    pub fn read_prg_rom(&self, addr: u16) -> Result<u8, MemReadError> {
        match &self.rom {
            None => {
                println!("Attempting to read with no loaded Rom. Returning 0x00.");
                Err(MemReadError::new(addr))
            }
            Some(rom) => {
                let mut adjusted_addr = addr - ROM;
                if rom.prg_rom.len() == 0x4000 && adjusted_addr >= 0x4000 {
                    adjusted_addr %= 0x4000;
                }

                if (adjusted_addr as usize) < rom.prg_rom.len() {
                    Ok(rom.prg_rom[adjusted_addr as usize])
                }
                else {
                    Ok(0x00)
                }

            }
        }
    }

    pub fn get_ppu_cycles(&self) -> usize {
        self.ppu.cycles
    }
}

impl Memory for Bus {
    fn mem_read(&mut self, addr: u16) -> Result<u8, MemReadError> {
        match addr {
            RAM ..= RAM_MIRROR_END => {
                //println!("Bus::mem_read -- reading from ram at addr: {:X}", addr);
                let mirror_down_addr = addr & 0b00000111_11111111;
                Ok(self.cpu_vram[mirror_down_addr as usize])
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                panic!("Attempt to read from write-only PPU address {:X}", addr);
            }
            0x2007 => self.ppu.read_data(),

            PPU_REGISTERS ..= PPU_REGISTERS_MIRROR_END => {
                let _mirror_down_addr = addr & 0b00100000_00000111;
                todo!("PPU is not supported yet. {:X}", addr)
            }

            ROM ..= 0xFFFF => {
                self.read_prg_rom(addr)
            }

            _=> {
                println!("Bus::mem_read -- ignoring memory access at {:X}", addr);
                Ok(0)
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