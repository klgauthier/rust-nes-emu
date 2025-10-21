// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::utils::bitflags::BitFlagU8;

#[derive(Clone, Copy, Debug)]
pub enum ControlFlags {
    NameTable1,
    NameTable2,
    VRamAddIncrement,
    SpritePatternAddr,
    BackgroundPatternAddr,
    SpriteSize,
    MasterSlaveSelect,
    GenerateNMI,
}

impl Into<u8> for ControlFlags {
    fn into(self) -> u8 {
        self as u8
    }
}

pub struct ControlRegister {
    flags: BitFlagU8,
}

impl Default for ControlRegister {
    fn default() -> Self {
        Self::new()
    }
}

impl ControlRegister {
    pub fn new() -> Self {
        ControlRegister { 
            flags: BitFlagU8::new(0x0) 
        }
    }

    pub fn get_vram_addr_increment(&self) -> u8 {
        match self.flags.get_flag(ControlFlags::VRamAddIncrement) {
            true => 32,
            false => 1,
        }
    }

    pub fn update(&mut self, data: u8) {
        self.flags = BitFlagU8::new(data);
    }

    pub fn get_flag(&self, flag: ControlFlags) -> bool {
        self.flags.get_flag(flag)
    }

    pub fn set_flag(&mut self, flag: ControlFlags, value: bool) {
        self.flags.set_flag(flag, value);
    }
}
