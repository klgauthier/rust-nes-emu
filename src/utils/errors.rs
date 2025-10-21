// Copyright 2025 Kevin Gauthier. All rights reserved.

use std::{error, fmt};

#[derive(Debug, Clone)]
pub struct MemReadError {
    read_addr: u16
}

impl fmt::Display for MemReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error reading from address [{:X}]", self.read_addr)
    }
}

impl MemReadError {
    pub fn new(read_addr: u16) -> Self {
        MemReadError { read_addr }
    }
}

impl error::Error for MemReadError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(self)
    }
}

#[derive(Debug, Clone)]
pub enum CPUError {
    MemReadError(MemReadError),
    BreakError,
    HaltError,
}

impl From<MemReadError> for CPUError {
    fn from(value: MemReadError) -> Self {
        CPUError::MemReadError(value)
    }
}

impl fmt::Display for CPUError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CPUError::MemReadError(mem_read_error) => mem_read_error.fmt(f),
            CPUError::BreakError => write!(f, "Program encountered a BRK op loop."),
            CPUError::HaltError => write!(f, "Program encountered a KIL op."),
        }
    }
}

impl error::Error for CPUError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            CPUError::MemReadError(mem_read_error) => Some(mem_read_error),
            CPUError::BreakError => None,
            CPUError::HaltError => None,
        }
    }
}
