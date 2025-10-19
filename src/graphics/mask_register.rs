// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::utils::bitflags::BitFlag;

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

pub struct MaskRegister {
    flags: u8,
}

impl BitFlag<MaskFlags> for MaskRegister {
    fn get_flag(&self, flag: MaskFlags) -> bool {
        let bit = (self.flags >> flag as u8) & 1;
        
        bit != 0
    }

    fn set_flag(&mut self, flag: MaskFlags, value: bool) {
        let result: u8 = (value as u8) << (flag as u8);

        self.flags &= !(1 << (flag as u8));
        self.flags |= result;
    }
}

impl MaskRegister {
    pub fn new() -> Self {
        MaskRegister { 
            flags: 0 
        }
    }
}

impl Default for MaskRegister {
    fn default() -> Self {
        MaskRegister::new()
    }
}