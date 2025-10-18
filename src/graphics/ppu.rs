// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::cartridge::Mirroring;
use crate::graphics::{addr_register::AddrRegister, control_register::ControlRegister};
use crate::utils::errors::MemReadError;

pub struct PPU {
    pub chr_rom: Vec<u8>,
    pub palette_table: [u8; 32],
    pub vram: [u8; 2048],
    pub oam_data: [u8; 256],
    pub mirroring: Mirroring,

    internal_data_buf: u8,

    addr: AddrRegister,
    pub ctrl: ControlRegister,
}

impl PPU {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        PPU { 
            chr_rom, 
            palette_table: [0; 32], 
            vram: [0; 2048], 
            oam_data: [0; 256], 
            mirroring,
            internal_data_buf: 0,
            addr: AddrRegister::new(),
            ctrl: ControlRegister::new(),
        }
    }

    fn write_to_ppu_addr(&mut self, value: u8) {
        self.addr.update(value);
    }

    fn write_to_ctrl(&mut self, value: u8) {
        self.ctrl.update(value);
    }

    fn increment_vram_addr(&mut self) {
        self.addr.increment(self.ctrl.get_vram_addr_increment());
    }

    pub fn read_data(&mut self) -> Result<u8, MemReadError>  {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0..=0x1FFF => {
                let result = self.internal_data_buf;
                self.internal_data_buf = match self.chr_rom.get(addr as usize) {
                    Some(new_value) => *new_value,
                    None => return Err(MemReadError::new(addr)),
                };

                Ok(result)
            },
            0x2000..=0x2FFF => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.vram[self.mirror_vram_addr(addr) as usize];

                Ok(result)
            },
            0x3000..=0x3EFF => Err(MemReadError::new(addr)),
            0x3F00..=0x3FFF => {
                Ok(self.palette_table[(addr - 0x3F00) as usize])
            },
            _ => Err(MemReadError::new(addr))
        }
    }

    pub fn mirror_vram_addr(&self, addr: u16) -> u16 {
        let mirrored_vram = addr & 0x2FFF;
        let vram_index = mirrored_vram - 0x2000;
        let name_table = vram_index / 0x400;
        match (&self.mirroring, name_table) {
           (Mirroring::Vertical, 2) | (Mirroring::Vertical, 3) => vram_index - 0x800,
           (Mirroring::Horizontal, 2) => vram_index - 0x400,
           (Mirroring::Horizontal, 1) => vram_index - 0x400,
           (Mirroring::Horizontal, 3) => vram_index - 0x800,
           _ => vram_index,
        }
    }
}



