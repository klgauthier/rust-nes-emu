// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::opcodes;

pub struct CPU
{
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: u8,
    pub program_counter: u16,
    memory: [u8; 0xFFFF]
}

const ADDR_PRG_ROM : usize = 0x8000;

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum CPUFlags
{
    Carry,
    Zero,
    InterruptDisable,
    DecimalMode,
    Break,
    Overflow,
    Negative,
}

#[derive(Clone, Copy, Debug)]
pub enum AddressingMode
{
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    NoneAddressing,
}

impl CPU
{
    
    pub fn new() -> Self
    {
        return CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: 0,
            program_counter: 0,
            memory: [0; 0xFFFF]
        }
    }

    pub fn load(&mut self, program: Vec<u8>)
    {
        self.memory[ADDR_PRG_ROM .. (ADDR_PRG_ROM + program.len())].copy_from_slice(&program[..]);
        self.program_counter = ADDR_PRG_ROM as u16;
    }

    pub fn load_and_run(&mut self, program: Vec<u8>)
    {
        self.load(program);
        self.run();
    }

    pub fn run(&mut self)
    {
        loop 
        {
            let opscode = self.mem_read(self.program_counter);
            self.program_counter += 1;

            match opscode
            {
                0x00 => {
                    return;
                }

                0x85 => { // STA ZeroPage
                    self.sta(&AddressingMode::ZeroPage);
                    self.program_counter += 1;
                }

                0x95 => { // STA ZeroPage X
                    self.sta(&AddressingMode::ZeroPageX);
                    self.program_counter += 1;
                }

                0xA5 => { // LDA Zero Page
                    self.lda(&AddressingMode::ZeroPage);
                    self.program_counter += 1;
                }

                0xA9 => { // LDA Immediate
                    self.lda(&AddressingMode::Immediate);
                    self.program_counter += 1;
                }

                0xAD => { // LDA Absolute
                    self.lda(&AddressingMode::Absolute);
                    self.program_counter += 2;
                }

                0xAA => { // TAX
                    self.tax();
                }

                0xC8 => { // INY
                    self.iny();
                }

                0xE8 => { // INX
                    self.inx();
                }
                
                _ => todo!(),
            }
        }
    }

    fn mem_read(&self, addr: u16) -> u8
    {
        return self.memory[addr as usize];
    }

    fn mem_read_u16(&self, addr: u16) -> u16
    {
        let low = self.mem_read(addr) as u16;
        let high = self.mem_read(addr+1) as u16;
        return (high << 8) | low;
    }

    fn mem_write(&mut self, addr: u16, data: u8)
    {
        self.memory[addr as usize] = data;
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16)
    {
        let low = (data >> 8) as u8;
        let high = (data & 0xff) as u8;
        self.mem_write(addr, low);
        self.mem_write(addr+1, high);
    }

    fn lda(&mut self, mode: &AddressingMode)
    {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn sta(&mut self, mode: &AddressingMode)
    {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn tax(&mut self)
    {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }
    
    fn inx(&mut self)
    {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn iny(&mut self)
    {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn set_flag(&mut self, flag: CPUFlags, value: bool)
    {
        let result: u8 = (value as u8) << (flag as u8);

        self.status = self.status & !(1 << (flag as u8));
        self.status = self.status | result;
    }

    fn update_zero_and_negative_flags(&mut self, value: u8)
    {
        self.set_flag(CPUFlags::Zero, value == 0);
        self.set_flag(CPUFlags::Negative, (value & 0b1000_0000) > 0);
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 
    {
        match mode 
        {
            AddressingMode::Immediate => return self.program_counter,
            AddressingMode::ZeroPage => return self.mem_read(self.program_counter) as u16,
            AddressingMode::ZeroPageX => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                return addr;
            },
            AddressingMode::ZeroPageY => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                return addr;
            },
            AddressingMode::Absolute => return self.mem_read_u16(self.program_counter),
            AddressingMode::AbsoluteX => {
                let pos = self.mem_read_u16(self.program_counter);
                let addr = pos.wrapping_add(self.register_x as u16);
                return addr;
            },
            AddressingMode::AbsoluteY => {
                let pos = self.mem_read_u16(self.program_counter);
                let addr = pos.wrapping_add(self.register_y as u16);
                return addr;
            },
            AddressingMode::IndirectX => {
                let base: u8 = self.mem_read(self.program_counter);

                let ptr = base.wrapping_add(self.register_x);
                let low = self.mem_read(ptr as u16);
                let high = self.mem_read(ptr.wrapping_add(1) as u16);
                return (high as u16) << 8 | (low as u16);
            },
            AddressingMode::IndirectY => {
                let base: u8 = self.mem_read(self.program_counter);

                let low = self.mem_read(base as u16);
                let high = self.mem_read(base.wrapping_add(1) as u16);
                let deref_base = (high as u16) << 8 | (low as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                return deref;
            },
            AddressingMode::NoneAddressing => {
                panic!("get_operand_address -- Mode {:?} is not supported!", mode);
            },
        }
    }
}

#[cfg(test)]
mod test
{
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
       let mut cpu = CPU::new();
       cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
       assert_eq!(cpu.register_a, 0x05);
       assert!(cpu.status & 0b0000_0010 == 0b00);
       assert!(cpu.status & 0b1000_0000 == 0);
   }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
   fn test_0xaa_tax_move_a_to_x() {
       let mut cpu = CPU::new();
       cpu.register_a = 10;
       cpu.load_and_run(vec![0xaa, 0x00]);
 
       assert_eq!(cpu.register_x, 10)
   }

   #[test]
   fn test_5_ops_working_together() {
       let mut cpu = CPU::new();
       cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
 
       assert_eq!(cpu.register_x, 0xc1)
   }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.register_x = 0xff;
        cpu.load_and_run(vec![0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_iny_overflow() {
        let mut cpu = CPU::new();
        cpu.register_y = 0xff;
        cpu.load_and_run(vec![0xc8, 0xc8, 0x00]);

        assert_eq!(cpu.register_y, 1)
    }

    #[test]
   fn test_lda_from_memory() {
       let mut cpu = CPU::new();
       cpu.mem_write(0x10, 0x55);

       cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

       assert_eq!(cpu.register_a, 0x55);
   }
}