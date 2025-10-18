// Copyright 2025 Kevin Gauthier. All rights reserved.

const MIRROR_DOWN_ADDR: u16 = 0x3FFF;

pub struct AddrRegister {
    value: (u8, u8),
    high_ptr: bool,
}

impl Default for AddrRegister {
    fn default() -> Self {
        Self::new()
    }
}

impl AddrRegister {
    pub fn new() -> Self {
        AddrRegister { 
            value: (0, 0), 
            high_ptr: true
        }
    }

    pub fn update(&mut self, data: u8) {
        match self.high_ptr {
            true => self.value.0 = data,
            false => self.value.1 = data,
        }

        if self.get() > MIRROR_DOWN_ADDR {
            self.set(self.get() & MIRROR_DOWN_ADDR);
        }

        self.high_ptr = !self.high_ptr;
    }

    pub fn increment(&mut self, inc: u8) {
        let low = self.value.1;
        self.value.1 = self.value.1.wrapping_add(inc);

        if low > self.value.1 {
            self.value.0 = self.value.0.wrapping_add(1);
        }

        if self.get() > MIRROR_DOWN_ADDR {
            self.set(self.get() & MIRROR_DOWN_ADDR)
        }
    }

    pub fn reset_latch(&mut self) {
        self.high_ptr = true;
    }

    pub fn get(&self) -> u16 {
        ((self.value.0 as u16) << 8) | (self.value.1 as u16)
    }
    
    fn set(&mut self, data: u16) {
        self.value.0 = (data >> 8) as u8;
        self.value.1 = (data & 0xFF) as u8;
    }
}