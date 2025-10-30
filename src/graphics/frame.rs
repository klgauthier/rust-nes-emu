// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::graphics::palette::{ColorRGB888};

#[derive(Debug)]
pub struct Frame {
    pub data: Vec<u8>,
}

impl Frame {
    const WIDTH: usize = 256;
    const HEIGHT: usize = 240;

    pub fn new() -> Self {
        Frame {
            data: vec![0; Frame::WIDTH * Frame::HEIGHT * 3],
        }
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, color: ColorRGB888) {
        let index = Frame::calculate_index(x, y);

        if index + 2 < self.data.len() {
            self.data[index] = color.r;
            self.data[index + 1] = color.g;
            self.data[index + 2] = color.b;
        }
    }

    pub fn set_pixel_palette(&mut self, x: usize, y: usize, palette_index: usize) {
        let color = ColorRGB888::from_palette(palette_index);
        self.set_pixel(x, y, color);
    }

    fn calculate_index(x: usize, y: usize) -> usize {
        (y * 3 * Frame::WIDTH) + (x * 3)
    }
}

impl Default for Frame {
    fn default() -> Self {
        Self::new()
    }
}

