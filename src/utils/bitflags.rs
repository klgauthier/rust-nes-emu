// Copyright 2025 Kevin Gauthier. All rights reserved.

pub trait BitFlag<T> {
    fn get_flag(&self, flag: T) -> bool;
    fn set_flag(&mut self, flag: T, value: bool);
}