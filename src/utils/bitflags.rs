// Copyright 2025 Kevin Gauthier. All rights reserved.

use std::ops::{BitOr, BitAnd, BitOrAssign, BitAndAssign, Deref};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct BitFlagU8(u8);

impl BitFlagU8 {
    pub fn new(raw: u8) -> Self {
        Self(raw)
    }

    pub fn get_flag<T: Into<u8>>(&self, flag: T) -> bool {
        let bit: u8 = (self.0 >> flag.into()) & 1;

        bit != 0
    }

    pub fn set_flag<T: Into<u8>>(&mut self, flag: T, value: bool) {
        let flag_as_u8: u8 = flag.into();
        let result = (value as u8) << flag_as_u8;

        self.0 &= !(1 << flag_as_u8);
        self.0 |= result;
    }

    pub fn get(&self) -> u8 {
        self.0
    }
}

impl BitOr for BitFlagU8 {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl BitOrAssign for BitFlagU8 {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 = self.0 | rhs.0;
    }
}

impl BitAnd for BitFlagU8 {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl BitAndAssign for BitFlagU8 {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 = self.0 & rhs.0;
    }
}

impl From<u8> for BitFlagU8 {
    fn from(raw: u8) -> Self {
        Self::new(raw)
    }
}

impl AsRef<u8> for BitFlagU8 {
    fn as_ref(&self) -> &u8 {
        &self.0
    }
}

impl Deref for BitFlagU8 {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

