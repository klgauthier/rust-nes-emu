// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::utils::bitflags::BitFlagU8;

#[derive(Clone, Copy, Debug)]
pub enum StatusFlags {
    Id0,
    Id1,
    Id2,
    Id3,
    Id4,
    SpriteOverflow,
    AtSprite0,
    AtVBlank,
}

impl From<StatusFlags> for u8 {
    fn from(val: StatusFlags) -> u8 {
        val as u8
    }
}

pub struct StatusRegister {
    flags: BitFlagU8,
}

impl StatusRegister {
    pub fn new() -> Self {
        StatusRegister { 
            flags: BitFlagU8::new(0x0) 
        }
    }

    pub fn get_flag(&self, flag: StatusFlags) -> bool {
        self.flags.get_flag(flag)
    }

    pub fn set_flag(&mut self, flag: StatusFlags, value: bool) {
        self.flags.set_flag(flag, value);
    }
}

impl Default for StatusRegister {
    fn default() -> Self {
        StatusRegister::new()
    }
}
