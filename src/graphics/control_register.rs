// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::utils::bitflags::BitFlag;

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

pub struct ControlRegister {
    flags: u8,
}

impl BitFlag<ControlFlags> for ControlRegister {
    fn get_flag(&self, flag: ControlFlags) -> bool {
        let bit = (self.flags >> flag as u8) & 1;
        
        bit != 0
    }

    fn set_flag(&mut self, flag: ControlFlags, value: bool) {
        let result: u8 = (value as u8) << (flag as u8);

        self.flags &= !(1 << (flag as u8));
        self.flags |= result;
    }
}

impl Default for ControlRegister {
    fn default() -> Self {
        Self::new()
    }
}

impl ControlRegister {
    pub fn new() -> Self {
        ControlRegister { 
            flags: 0 
        }
    }

    pub fn get_vram_addr_increment(&self) -> u8 {
        match self.get_flag(ControlFlags::VRamAddIncrement) {
            true => 32,
            false => 1,
        }
    }

    pub fn update(&mut self, data: u8) {
        self.flags = data;
    }
}