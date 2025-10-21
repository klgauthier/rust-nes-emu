// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::cartridge::Mirroring;
use crate::graphics::control_register::ControlFlags;
use crate::graphics::mask_register::MaskRegister;
use crate::graphics::status_register::{StatusFlags, StatusRegister};
use crate::graphics::{addr_register::AddrRegister, control_register::ControlRegister};
use crate::utils::errors::MemReadError;

const HBLANK_CYCLE: u16 = 341;
const VBLANK_INTERRUPT: u16 = 241;
const VBLANK_CYCLE: u16 = 262;

pub struct PPU {
    pub chr_rom: Vec<u8>,
    pub palette_table: [u8; 32],
    pub vram: [u8; 2048],
    pub oam_data: [u8; 256],
    pub mirroring: Mirroring,

    addr: AddrRegister,
    pub ctrl: ControlRegister,
    status: StatusRegister,
    mask: MaskRegister,

    internal_data_buf: u8,
    pub cycles: usize,
    scanline: u16,
    nmi_interrupt_active: bool,
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
            status: StatusRegister::new(),
            mask: MaskRegister::new(),
            cycles: 0,
            scanline: 0,
            nmi_interrupt_active: false
        }
    }

    pub fn tick(&mut self, cycles_increment: u8) {
        self.cycles += cycles_increment as usize;

        if self.cycles >= HBLANK_CYCLE as usize {
            self.cycles %= HBLANK_CYCLE as usize;
            self.scanline += 1;

            if self.scanline == VBLANK_INTERRUPT {
                if self.ctrl.get_flag(ControlFlags::GenerateNMI) {
                    self.status.set_flag(StatusFlags::AtVBlank, true);
                    todo!("Trigger an NMI Interrupt")
                }
            }

            if self.scanline >= VBLANK_CYCLE {
                self.scanline = 0;
                self.status.set_flag(StatusFlags::AtVBlank, false);
            }
        }
    }

    pub fn poll_nmi_interrupt(&mut self) -> bool {
        match self.nmi_interrupt_active {
            true => {
                self.nmi_interrupt_active = false;

                true
            },
            false => false,
        }
    }

    fn write_to_ppu_addr(&mut self, value: u8) {
        self.addr.update(value);
    }

    fn write_to_ctrl(&mut self, value: u8) {
        let previous_nmi_status = self.ctrl.get_flag(ControlFlags::GenerateNMI);
        self.ctrl.update(value);

        if !previous_nmi_status && self.ctrl.get_flag(ControlFlags::GenerateNMI) && self.status.get_flag(StatusFlags::AtVBlank) {
            self.nmi_interrupt_active = true;
        }
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



