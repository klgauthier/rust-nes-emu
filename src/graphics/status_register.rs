// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::utils::bitflags::BitFlag;

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

pub struct StatusRegister {
    flags: u8,
}

impl BitFlag<StatusFlags> for StatusRegister {
    fn get_flag(&self, flag: StatusFlags) -> bool {
        let bit = (self.flags >> flag as u8) & 1;
        
        bit != 0
    }

    fn set_flag(&mut self, flag: StatusFlags, value: bool) {
        let result: u8 = (value as u8) << (flag as u8);

        self.flags &= !(1 << (flag as u8));
        self.flags |= result;
    }
}

impl StatusRegister {
    pub fn new() -> Self {
        StatusRegister { 
            flags: 0 
        }
    }
}

impl Default for StatusRegister {
    fn default() -> Self {
        StatusRegister::new()
    }
}