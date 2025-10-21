// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::utils::bitflags::BitFlagU8;

#[derive(Clone, Copy, Debug)]
pub enum MaskFlags {
    Greyscale,
    ShowBackgroundLeft8,
    ShowSpritesLeft8,
    EnableBackgroundRendering,
    EnableSpriteRendering,
    EmphasizeRed,
    EmphasizeGreen,
    EmphasizeBlue,
}

impl Into<u8> for MaskFlags {
    fn into(self) -> u8 {
        self as u8
    }
}

pub struct MaskRegister {
    flags: BitFlagU8,
}

impl MaskRegister {
    pub fn new() -> Self {
        MaskRegister { 
            flags: BitFlagU8::new(0x0) 
        }
    }

    pub fn get_flag(&self, flag: MaskFlags) -> bool {
        self.flags.get_flag(flag)
    }
    
    pub fn set_flag(&mut self, flag: MaskFlags, value: bool) {
        self.flags.set_flag(flag, value);
    }
}

impl Default for MaskRegister {
    fn default() -> Self {
        MaskRegister::new()
    }
}
