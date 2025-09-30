// Copyright 2025 Kevin Gauthier. All rights reserved.

use std::collections::HashMap;
use crate::opcodes;

pub struct CPU
{
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: u8,
    pub program_counter: u16,
    pub stack_pointer: u16,
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
    Break2,
    Overflow,
    Negative,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
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

trait Memory 
{
    fn mem_read(&self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&self, addr: u16) -> u16
    {
        let low = self.mem_read(addr) as u16;
        let high = self.mem_read(addr+1) as u16;
        return (high << 8) | low;
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16)
    {
        let low = (data >> 8) as u8;
        let high = (data & 0xff) as u8;
        self.mem_write(addr, low);
        self.mem_write(addr+1, high);
    }
}

impl Memory for CPU
{
    fn mem_read(&self, addr: u16) -> u8
    {
        return self.memory[addr as usize];
    }

    fn mem_write(&mut self, addr: u16, data: u8)
    {
        self.memory[addr as usize] = data;
    }
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
            stack_pointer: 0,
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
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;

        loop 
        {
            let opcode = self.mem_read(self.program_counter);
            let opcode_info = opcodes.get(&opcode).expect(&format!("CPU::run -- Missing opscode: {:x}", opcode));
            self.program_counter += 1;

            match opcode_info.instruction
            {
                /* INTERRUPTS */
                "BRK" => {
                    return;
                }

                /* BRANCHES */
                "BCC" => {
                    self.bcc();
                }

                "BCS" => {
                    self.bcs();
                }

                "BEQ" => {
                    self.beq();
                }

                "BNE" => {
                    self.bne();
                }

                "BMI" => {
                    self.bmi();
                }

                "BPL" => {
                    self.bpl();
                }

                "BVC" => {
                    self.bvc();
                }

                "BVS" => {
                    self.bvs();
                }

                "JMP" => {
                    self.jmp(&opcode_info.mode);
                }

                "JSR" => {
                    self.jsr(&opcode_info.mode);
                }

                /* MOVES */
                "STA" => {
                    self.sta(&opcode_info.mode);
                }

                "LDA"=> {
                    self.lda(&opcode_info.mode);
                }

                "LDX"=> {
                    self.ldx(&opcode_info.mode);
                }

                "LDY"=> {
                    self.ldy(&opcode_info.mode);
                }

                "TAX" => {
                    self.tax();
                }

                /* FLAGS */
                "CLC" => {
                    self.clc();
                }

                "CLD" => {
                    self.cld();
                }

                "CLI" => {
                    self.cli();
                }

                "CLV" => {
                    self.clv();
                }

                /* ARITHMETIC */
                "ADC" => {
                    self.adc(&opcode_info.mode);
                }

                "ASL" => {
                    if opcode == 0x0A {
                        self.asl_accumulator();
                    }
                    else {
                        self.asl_memory(&opcode_info.mode);
                    }
                }

                //https://www.nesdev.org/obelisk-6502-guide/reference.html#LSR
                "LSR" => todo!(),

                "INC" => {
                    self.inc(&opcode_info.mode);
                }

                "INY" => {
                    self.iny();
                }

                "INX" => {
                    self.inx();
                }

                "DEC" => {
                    self.dec(&opcode_info.mode);
                }

                "DEX" => {
                    self.dex();
                }

                "DEY" => {
                    self.dey();
                }

                /* LOGIC */
                "AND" => {
                    self.and(&opcode_info.mode);
                }

                "BIT" => {
                    self.bit(&opcode_info.mode);
                }

                "CMP" => {
                    self.cmp(&opcode_info.mode)
                }

                "CPX" => {
                    self.cpx(&opcode_info.mode)
                }

                "CPY" => {
                    self.cpy(&opcode_info.mode)
                }

                "EOR" => {
                    self.eor(&opcode_info.mode);
                }
                
                _ => todo!(),
            }

            
            self.program_counter += (opcode_info.len-1) as u16;
        }
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let value = self.retreive_from_mem(mode);

        let (result, carry) = self.register_a.overflowing_add(value);
        self.register_a = result;
        self.set_flag(CPUFlags::Carry, carry);
    }

    fn and(&mut self, mode: &AddressingMode) {
        let value = self.retreive_from_mem(mode);

        self.register_a &= value;
    }

    fn asl_accumulator(&mut self) {
        self.set_flag(CPUFlags::Carry, self.register_a >= 0b1000_0000);

        self.register_a = self.register_a << 1;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn asl_memory(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_flag(CPUFlags::Carry, value >= 0b1000_0000);
        let shifted_value = value << 1;
        self.mem_write(addr, shifted_value);
        self.update_zero_and_negative_flags(shifted_value);
    }

    fn bcc(&mut self) {
        self.branch_for_flag_value(CPUFlags::Carry, false);
    }

    fn bcs(&mut self) {
        self.branch_for_flag_value(CPUFlags::Carry, true);
    }

    fn beq(&mut self) {
        self.branch_for_flag_value(CPUFlags::Zero, true);
    }

    fn bpl(&mut self) {
        self.branch_for_flag_value(CPUFlags::Negative, false);
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let value = self.retreive_from_mem(mode);
        
        self.set_flag(CPUFlags::Overflow, (value & 0b0010_0000) > 0);
        self.set_flag(CPUFlags::Negative, (value & 0b0100_0000) > 0);
        self.set_flag(CPUFlags::Zero, value & self.register_a == 0);
    }

    fn bmi(&mut self) {
        self.branch_for_flag_value(CPUFlags::Negative, true);
    }

    fn bne(&mut self) {
        self.branch_for_flag_value(CPUFlags::Zero, false);
    }

    fn bvc(&mut self) {
        self.branch_for_flag_value(CPUFlags::Overflow, false);
    }

    fn bvs(&mut self) {
        self.branch_for_flag_value(CPUFlags::Overflow, true);
    }

    fn clc(&mut self) {
        self.set_flag(CPUFlags::Carry, false);
    }

    fn cld(&mut self) {
        self.set_flag(CPUFlags::DecimalMode, false);
    }

    fn cli(&mut self) {
        self.set_flag(CPUFlags::InterruptDisable, false);
    }

    fn clv(&mut self) {
        self.set_flag(CPUFlags::Overflow, false);
    }

    fn cmp(&mut self, mode: &AddressingMode) {
        let value = self.retreive_from_mem(mode);

        self.compare(self.register_a, value);
    }

    fn cpx(&mut self, mode: &AddressingMode) {
        let value = self.retreive_from_mem(mode);
        
        self.compare(self.register_x, value);
    }

    fn cpy(&mut self, mode: &AddressingMode) {
        let value = self.retreive_from_mem(mode);
        
        self.compare(self.register_y, value);
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let new_value = value.wrapping_sub(1);
        self.mem_write(addr, new_value);
        self.update_zero_and_negative_flags(new_value);
    }

    fn dex(&mut self) {
        let value = self.register_x.wrapping_sub(1);
        self.register_x = value;
        self.update_zero_and_negative_flags(value);
    }

    fn dey(&mut self) {
        let value = self.register_y.wrapping_sub(1);
        self.register_y = value;
        self.update_zero_and_negative_flags(value);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let value = self.retreive_from_mem(mode);

        self.register_a = self.register_a ^ value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let new_value = value.wrapping_add(1);
        self.mem_write(addr, new_value);
        self.update_zero_and_negative_flags(new_value);
    }
    
    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn jmp(&mut self, mode: &AddressingMode) {
        let mut addr = self.get_operand_address(mode);

        if *mode == AddressingMode::NoneAddressing {
            // emulating 6502 bug with reading at the boundary of pages
            // ref: https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP

            if (addr & 0x00FF) == 0x00FF {
                let high = self.mem_read(addr & 0xFF00);
                addr = (addr & 0x00FF) | (high << 8) as u16;
            }
        }

        let value = self.mem_read_u16(addr);

        // reducing program by 2 to account for the program counter getting increased by opcode length
        self.program_counter = value.saturating_sub(2); 
    }

    fn jsr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.stack_pointer = self.program_counter.saturating_add(2);
        self.program_counter = addr.saturating_sub(2);
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let value = self.retreive_from_mem(mode);

        self.register_a = value;
        self.update_zero_and_negative_flags(value);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let value = self.retreive_from_mem(mode);

        self.register_x = value;
        self.update_zero_and_negative_flags(value);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let value = self.retreive_from_mem(mode);

        self.register_y = value;
        self.update_zero_and_negative_flags(value);
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn get_flag(&mut self, flag: CPUFlags) -> bool {
        return (self.status & (flag as u8)) == 1;
    }

    fn set_flag(&mut self, flag: CPUFlags, value: bool) {
        let result: u8 = (value as u8) << (flag as u8);

        self.status = self.status & !(1 << (flag as u8));
        self.status = self.status | result;
    }

    fn branch_for_flag_value(&mut self, flag: CPUFlags, value: bool) {
        if self.get_flag(flag) == value {
            let offset = self.mem_read(self.program_counter);
            self.offset_program_counter(offset);
        }
    }

    fn compare(&mut self, register_value: u8, memory_value: u8) {
        self.set_flag(CPUFlags::Carry, register_value >= memory_value);
        self.update_zero_and_negative_flags(register_value.wrapping_sub(memory_value));

    }

    fn update_zero_and_negative_flags(&mut self, value: u8) {
        self.set_flag(CPUFlags::Zero, value == 0);
        self.set_flag(CPUFlags::Negative, (value & 0b1000_0000) > 0);
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
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

    fn retreive_from_mem(&self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        return self.mem_read(addr);
    }

    fn offset_program_counter(&mut self, offset: u8) {
        self.program_counter = self.program_counter.saturating_add_signed(offset as i16);
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