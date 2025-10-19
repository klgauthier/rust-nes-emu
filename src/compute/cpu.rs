// Copyright 2025 Kevin Gauthier. All rights reserved.

use std::collections::HashMap;
use crate::compute::{bus, opcodes};
use crate::compute::arguments::Arguments;
use crate::compute::bus::{Bus, Memory};
use crate::cartridge::Rom;
use crate::logging::{ANSIColor, FileLog, LogWrite, Logger, TerminalLog};
use crate::utils::bitflags::BitFlag;
use crate::utils::errors::{CPUError, MemReadError};

pub struct CPU
{
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: u8,
    pub program_counter: u16,
    pub stack_pointer: u8,
    pub bus: Bus,

    delay_branch_succeed: bool,
    delay_page_crossed: bool,
}

const ADDR_STACK_TOP : u16 = 0x01FF;
const ADDR_STACK_BOTTOM : u16 = 0x0100;

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum CPUFlags {
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
pub enum AddressingMode {
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

#[derive(Clone, Copy, Debug)]
pub struct Operand {
    args: Arguments,
    mode: AddressingMode
}

impl Operand {
    fn new(args: Arguments, mode: AddressingMode) -> Self {
        Operand { 
            args, 
            mode 
        }
    }
}

impl Memory for CPU {
    fn mem_read(&mut self, addr: u16) -> Result<u8, MemReadError> {
        self.bus.mem_read(addr)
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data);
    }

    fn mem_read_u16(&mut self, addr: u16) -> Result<u16, MemReadError> {
        self.bus.mem_read_u16(addr)
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        self.bus.mem_write_u16(addr, data);
    }
}


type OpFunction = fn(&mut CPU, Operand) -> Option<CPUError>;

trait Operations {
    fn adc(&mut self, operand: Operand) -> Option<CPUError>;
    fn and(&mut self, operand: Operand) -> Option<CPUError>;
    fn asl(&mut self, operand: Operand) -> Option<CPUError>;
    fn bcc(&mut self, operand: Operand) -> Option<CPUError>;
    fn bcs(&mut self, operand: Operand) -> Option<CPUError>;
    fn beq(&mut self, operand: Operand) -> Option<CPUError>;
    fn bit(&mut self, operand: Operand) -> Option<CPUError>;
    fn bmi(&mut self, operand: Operand) -> Option<CPUError>;
    fn bne(&mut self, operand: Operand) -> Option<CPUError>;
    fn bpl(&mut self, operand: Operand) -> Option<CPUError>;
    fn brk(&mut self, operand: Operand) -> Option<CPUError>;
    fn bvc(&mut self, operand: Operand) -> Option<CPUError>;
    fn bvs(&mut self, operand: Operand) -> Option<CPUError>;
    fn clc(&mut self, operand: Operand) -> Option<CPUError>;
    fn cld(&mut self, operand: Operand) -> Option<CPUError>;
    fn cli(&mut self, operand: Operand) -> Option<CPUError>;
    fn clv(&mut self, operand: Operand) -> Option<CPUError>;
    fn cmp(&mut self, operand: Operand) -> Option<CPUError>;
    fn cpx(&mut self, operand: Operand) -> Option<CPUError>;
    fn cpy(&mut self, operand: Operand) -> Option<CPUError>;
    fn dec(&mut self, operand: Operand) -> Option<CPUError>;
    fn dex(&mut self, operand: Operand) -> Option<CPUError>;
    fn dey(&mut self, operand: Operand) -> Option<CPUError>;
    fn eor(&mut self, operand: Operand) -> Option<CPUError>;
    fn inc(&mut self, operand: Operand) -> Option<CPUError>;    
    fn inx(&mut self, operand: Operand) -> Option<CPUError>;
    fn iny(&mut self, operand: Operand) -> Option<CPUError>;
    fn jmp(&mut self, operand: Operand) -> Option<CPUError>;
    fn jsr(&mut self, operand: Operand) -> Option<CPUError>;
    fn lda(&mut self, operand: Operand) -> Option<CPUError>;
    fn ldx(&mut self, operand: Operand) -> Option<CPUError>;
    fn ldy(&mut self, operand: Operand) -> Option<CPUError>;
    fn lsr(&mut self, operand: Operand) -> Option<CPUError>;
    fn nop(&mut self, operand: Operand) -> Option<CPUError>;
    fn ora(&mut self, operand: Operand) -> Option<CPUError>;
    fn pha(&mut self, operand: Operand) -> Option<CPUError>;
    fn php(&mut self, operand: Operand) -> Option<CPUError>;
    fn pla(&mut self, operand: Operand) -> Option<CPUError>;
    fn plp(&mut self, operand: Operand) -> Option<CPUError>;
    fn rol(&mut self, operand: Operand) -> Option<CPUError>;
    fn ror(&mut self, operand: Operand) -> Option<CPUError>;
    fn rti(&mut self, operand: Operand) -> Option<CPUError>;
    fn rts(&mut self, operand: Operand) -> Option<CPUError>;
    fn sbc(&mut self, operand: Operand) -> Option<CPUError>;
    fn sec(&mut self, operand: Operand) -> Option<CPUError>;
    fn sed(&mut self, operand: Operand) -> Option<CPUError>;
    fn sei(&mut self, operand: Operand) -> Option<CPUError>;
    fn sta(&mut self, operand: Operand) -> Option<CPUError>;
    fn stx(&mut self, operand: Operand) -> Option<CPUError>;
    fn sty(&mut self, operand: Operand) -> Option<CPUError>;
    fn tax(&mut self, operand: Operand) -> Option<CPUError>;
    fn tay(&mut self, operand: Operand) -> Option<CPUError>;
    fn tsx(&mut self, operand: Operand) -> Option<CPUError>;
    fn txa(&mut self, operand: Operand) -> Option<CPUError>;
    fn txs(&mut self, operand: Operand) -> Option<CPUError>;
    fn tya(&mut self, operand: Operand) -> Option<CPUError>;
}

impl Operations for CPU {
    fn adc(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;

        let (result, carry, overflow) = CPU::accumulator_add(self.register_a, value);

        self.register_a = result;
        self.set_flag(CPUFlags::Carry, carry);
        self.set_flag(CPUFlags::Overflow, overflow);

        None
    }

    fn and(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;
        
        self.register_a &= value;

        None
    }

    fn asl(&mut self, operand: Operand) -> Option<CPUError> {
        match operand.args {
            Arguments::None => { // accumulator mode
                self.set_flag(CPUFlags::Carry, self.register_a >= 0b1000_0000);

                self.register_a <<= 1;
                self.update_zero_and_negative_flags(self.register_a);
            }

            Arguments::One(_) => (),

            Arguments::Two(_) => { // memory mode
                let addr = self.retreive_operand_address(operand).ok()?;
                let value = self.mem_read(addr).ok()?;

                self.set_flag(CPUFlags::Carry, value >= 0b1000_0000);
                let shifted_value = value << 1;
                self.mem_write(addr, shifted_value);
                self.update_zero_and_negative_flags(shifted_value);
            }
        }

        None
    }

    fn bcc(&mut self, operand: Operand) -> Option<CPUError> {
        self.branch_for_flag_value(CPUFlags::Carry, false, operand.args.get_arg1()?);

        None
    }

    fn bcs(&mut self, operand: Operand) -> Option<CPUError> {
        self.branch_for_flag_value(CPUFlags::Carry, true, operand.args.get_arg1()?);

        None
    }

    fn beq(&mut self, operand: Operand) -> Option<CPUError> {
        self.branch_for_flag_value(CPUFlags::Zero, true, operand.args.get_arg1()?);

        None
    }

    fn bit(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;
        
        self.set_flag(CPUFlags::Overflow, (value & 0b0010_0000) != 0);
        self.set_flag(CPUFlags::Negative, (value & 0b0100_0000) != 0);
        self.set_flag(CPUFlags::Zero, value & self.register_a == 0);

        None
    }

    fn bmi(&mut self, operand: Operand) -> Option<CPUError> {
        self.branch_for_flag_value(CPUFlags::Negative, true, operand.args.get_arg1()?);

        None
    }

    fn bne(&mut self, operand: Operand) -> Option<CPUError> {
        self.branch_for_flag_value(CPUFlags::Zero, false, operand.args.get_arg1()?);

        None
    }

    fn bpl(&mut self, operand: Operand) -> Option<CPUError> {
        self.branch_for_flag_value(CPUFlags::Negative, false, operand.args.get_arg1()?);

        None
    }

    fn brk(&mut self, _operand: Operand) -> Option<CPUError> {
        self.push_stack_u16(self.program_counter);
        self.push_stack(self.status);
        self.program_counter = self.mem_read_u16(0xFFFE).ok()?;
        self.set_flag(CPUFlags::Break, true);

        Some(CPUError::BreakError)
    }

    fn bvc(&mut self, operand: Operand) -> Option<CPUError> {
        self.branch_for_flag_value(CPUFlags::Overflow, false, operand.args.get_arg1()?);

        None
    }

    fn bvs(&mut self, operand: Operand) -> Option<CPUError> {
        self.branch_for_flag_value(CPUFlags::Overflow, true, operand.args.get_arg1()?);

        None
    }

    fn clc(&mut self, _operand: Operand) -> Option<CPUError> {
        self.set_flag(CPUFlags::Carry, false);

        None
    }

    fn cld(&mut self, _operand: Operand) -> Option<CPUError> {
        self.set_flag(CPUFlags::DecimalMode, false);

        None
    }

    fn cli(&mut self, _operand: Operand) -> Option<CPUError> {
        self.set_flag(CPUFlags::InterruptDisable, false);

        None
    }

    fn clv(&mut self, _operand: Operand) -> Option<CPUError> {
        self.set_flag(CPUFlags::Overflow, false);

        None
    }

    fn cmp(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;

        self.compare(self.register_a, value);

        None
    }

    fn cpx(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;
        
        self.compare(self.register_x, value);

        None
    }

    fn cpy(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;
        
        self.compare(self.register_y, value);

        None
    }

    fn dec(&mut self, operand: Operand) -> Option<CPUError> {
        let addr = self.retreive_operand_address(operand).ok()?;
        let value = self.mem_read(addr).ok()?;

        let new_value = value.wrapping_sub(1);
        self.mem_write(addr, new_value);
        self.update_zero_and_negative_flags(new_value);

        None
    }

    fn dex(&mut self, _operand: Operand) -> Option<CPUError> {
        let value = self.register_x.wrapping_sub(1);
        self.register_x = value;
        self.update_zero_and_negative_flags(value);

        None
    }

    fn dey(&mut self, _operand: Operand) -> Option<CPUError> {
        let value = self.register_y.wrapping_sub(1);
        self.register_y = value;
        self.update_zero_and_negative_flags(value);

        None
    }

    fn eor(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;

        self.register_a ^= value;
        self.update_zero_and_negative_flags(self.register_a);

        None
    }

    fn inc(&mut self, operand: Operand) -> Option<CPUError> {
        let addr = self.retreive_operand_address(operand).ok()?;
        let value = self.mem_read(addr).ok()?;

        let new_value = value.wrapping_add(1);
        self.mem_write(addr, new_value);
        self.update_zero_and_negative_flags(new_value);

        None
    }
    
    fn inx(&mut self, _operand: Operand) -> Option<CPUError> {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);

        None
    }

    fn iny(&mut self, _operand: Operand) -> Option<CPUError> {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);

        None
    }

    fn jmp(&mut self, operand: Operand) -> Option<CPUError> {
        if let Arguments::Two(mut addr) = operand.args {
            if operand.mode == AddressingMode::NoneAddressing {
                // emulating 6502 bug with reading at the boundary of pages
                // ref: https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP

                if (addr & 0x00FF) == 0x00FF {
                    let high = self.mem_read(addr & 0xFF00).ok()?;
                    addr = self.mem_read(addr & 0x00FF).ok()? as u16 | ((high as u16) << 8);
                }
                else {
                    addr = self.mem_read_u16(addr).ok()?;
                }
            }

            //println!("\tCPU::jmp -- Addr: {:x}", addr);
            self.program_counter = addr; 
        }

        None
    }

    fn jsr(&mut self, operand: Operand) -> Option<CPUError> {
        let addr = self.retreive_operand_address(operand).ok()?;

        //println!("\tCPU::jsr -- Addr: {:x}, Pushing: {:x}", addr, self.program_counter.wrapping_add(2));

        self.push_stack_u16(self.program_counter.wrapping_add(2));
        self.program_counter = addr;

        None
    }

    fn lda(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;

        self.register_a = value;
        self.update_zero_and_negative_flags(value);

        None
    }

    fn ldx(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;

        self.register_x = value;
        self.update_zero_and_negative_flags(value);

        None
    }

    fn ldy(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;

        self.register_y = value;
        self.update_zero_and_negative_flags(value);

        None
    }

    fn lsr(&mut self, operand: Operand) -> Option<CPUError> {
        match operand.args {
            Arguments::None => { // accumulator mode
                self.set_flag(CPUFlags::Carry, (self.register_a & 0b0000_0001) != 0);
                self.register_a >>= 1;
                self.update_zero_and_negative_flags(self.register_a);
            }

            Arguments::One(_) => (),

            Arguments::Two(_) => { // memory mode
                let addr = self.retreive_operand_address(operand).ok()?;
                let value = self.mem_read(addr).ok()?;

                self.set_flag(CPUFlags::Carry, (value & 0b0000_0001) != 0);
                let shifted_value = value >> 1;
                self.mem_write(addr, shifted_value);
                self.update_zero_and_negative_flags(shifted_value);
            }
        }

        None
    }
    
    fn nop(&mut self, _operand: Operand) -> Option<CPUError> {
        None
    }

    fn ora(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;

        self.register_a |= value;
        self.update_zero_and_negative_flags(self.register_a);

        None
    }

    fn pha(&mut self, _operand: Operand) -> Option<CPUError> {
        self.push_stack(self.register_a);

        None
    }

    fn php(&mut self, _operand: Operand) -> Option<CPUError> {
        let status_state = self.status | (1 << CPUFlags::Break as u8) | (1 << CPUFlags::Break2 as u8); 
        self.push_stack(status_state);

        None
    }

    fn pla(&mut self, _operand: Operand) -> Option<CPUError> {
        self.register_a = self.pop_stack().ok()?;
        self.update_zero_and_negative_flags(self.register_a);

        None
    }

    fn plp(&mut self, _operand: Operand) -> Option<CPUError> {
        self.status &= (1 << CPUFlags::Break as u8) & (1 << CPUFlags::Break2 as u8);
        self.status |= self.pop_stack().ok()? & !(1 << CPUFlags::Break as u8) & !(1 << CPUFlags::Break2 as u8);

        None
    }

    fn rol(&mut self, operand: Operand) -> Option<CPUError> {
        match operand.args {
            Arguments::None => { // accumulator mode
                let old_carry = self.get_flag(CPUFlags::Carry) as u8;
                self.set_flag(CPUFlags::Carry, (self.register_a & 0b1000_0000) != 0);
                self.register_a = (self.register_a << 1) | old_carry;
                self.update_zero_and_negative_flags(self.register_a);
            }

            Arguments::One(_) => (),

            Arguments::Two(_) => { // memory mode
                let addr = self.retreive_operand_address(operand).ok()?;
                let value = self.mem_read(addr).ok()?;

                let old_carry = self.get_flag(CPUFlags::Carry) as u8;
                self.set_flag(CPUFlags::Carry, (value & 0b1000_0000) != 0);
                let result= (value << 1) | old_carry;
                self.mem_write(addr, result);
                self.set_flag(CPUFlags::Zero, self.register_a == 0);
                self.set_flag(CPUFlags::Negative, (result & 0b1000_0000) != 0);
            }
        }

        None
    }

    fn ror(&mut self, operand: Operand) -> Option<CPUError> {
        match operand.args {
            Arguments::None => { // accumulator mode
                let old_carry = self.get_flag(CPUFlags::Carry) as u8;
                self.set_flag(CPUFlags::Carry, (self.register_a & 0b0000_0001) != 0);
                self.register_a = (self.register_a >> 1) | (old_carry << 7);
                self.update_zero_and_negative_flags(self.register_a);
            }

            Arguments::One(_) => (),

            Arguments::Two(_) => { // memory mode
                let old_carry = self.get_flag(CPUFlags::Carry) as u8;
                let addr = self.retreive_operand_address(operand).ok()?;
                let value = self.mem_read(addr).ok()?;

                self.set_flag(CPUFlags::Carry, (value & 0b0000_0001) != 0);
                let result= (value >> 1) | (old_carry << 7);
                self.mem_write(addr, result);
                self.set_flag(CPUFlags::Zero, self.register_a == 0);
                self.set_flag(CPUFlags::Negative, (result & 0b1000_0000) != 0);
            }
        }

        None
    }

    fn rti(&mut self, _operand: Operand) -> Option<CPUError> {
        self.status = self.pop_stack().ok()?;
        self.program_counter = self.pop_stack_u16().ok()?;

        None
    }

    fn rts(&mut self, _operand: Operand) -> Option<CPUError> {
        self.program_counter = self.pop_stack_u16().ok()?;

        //println!("\tCPU::rts -- Popped addr: {:x}", self.program_counter);

        None
    }

    fn sbc(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;

        let value_2c = (!value).wrapping_add(1);
        let (result, carry, overflow) = CPU::accumulator_add(self.register_a, value_2c);

        self.register_a = result;
        self.set_flag(CPUFlags::Carry, carry);
        self.set_flag(CPUFlags::Overflow, overflow);

        None
    }

    fn sec(&mut self, _operand: Operand) -> Option<CPUError> {
        self.set_flag(CPUFlags::Carry, true);

        None
    }

    fn sed(&mut self, _operand: Operand) -> Option<CPUError> {
        self.set_flag(CPUFlags::DecimalMode, true);

        None
    }

    fn sei(&mut self, _operand: Operand) -> Option<CPUError> {
        self.set_flag(CPUFlags::InterruptDisable, true);

        None
    }

    fn sta(&mut self, operand: Operand) -> Option<CPUError> {
        let addr = self.retreive_operand_address(operand).ok()?;
        self.mem_write(addr, self.register_a);

        None
    }

    fn stx(&mut self, operand: Operand) -> Option<CPUError> {
        let addr = self.retreive_operand_address(operand).ok()?;
        self.mem_write(addr, self.register_x);

        None
    }

    fn sty(&mut self, operand: Operand) -> Option<CPUError> {
        let addr = self.retreive_operand_address(operand).ok()?;
        self.mem_write(addr, self.register_y);

        None
    }

    fn tax(&mut self, _operand: Operand) -> Option<CPUError> {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);

        None
    }

    fn tay(&mut self, _operand: Operand) -> Option<CPUError> {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);

        None
    }

    fn tsx(&mut self, _operand: Operand) -> Option<CPUError> {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);

        None
    }

    fn txa(&mut self, _operand: Operand) -> Option<CPUError> {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);

        None
    }

    fn txs(&mut self, _operand: Operand) -> Option<CPUError> {
        self.stack_pointer = self.register_x;

        None
    }

    fn tya(&mut self, _operand: Operand) -> Option<CPUError> {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);

        None
    }
}

trait IllegalOperations {
    fn aac(&mut self, operand: Operand) -> Option<CPUError>;
    fn aax(&mut self, operand: Operand) -> Option<CPUError>;
    fn arr(&mut self, operand: Operand) -> Option<CPUError>;
    fn asr(&mut self, operand: Operand) -> Option<CPUError>;
    fn atx(&mut self, operand: Operand) -> Option<CPUError>;
    fn axa(&mut self, operand: Operand) -> Option<CPUError>;
    fn axs(&mut self, operand: Operand) -> Option<CPUError>;
    fn dcp(&mut self, operand: Operand) -> Option<CPUError>;
    fn isc(&mut self, operand: Operand) -> Option<CPUError>;
    fn kil(&mut self, operand: Operand) -> Option<CPUError>;
    fn lar(&mut self, operand: Operand) -> Option<CPUError>;
    fn lax(&mut self, operand: Operand) -> Option<CPUError>;
    fn rla(&mut self, operand: Operand) -> Option<CPUError>;
    fn rra(&mut self, operand: Operand) -> Option<CPUError>;
    fn slo(&mut self, operand: Operand) -> Option<CPUError>;
    fn sre(&mut self, operand: Operand) -> Option<CPUError>;
    fn sxa(&mut self, operand: Operand) -> Option<CPUError>;
    fn sya(&mut self, operand: Operand) -> Option<CPUError>;
    fn xaa(&mut self, operand: Operand) -> Option<CPUError>;
    fn xas(&mut self, operand: Operand) -> Option<CPUError>;
}

impl IllegalOperations for CPU {
    fn aac(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;
        self.register_a &= value;
        self.update_zero_and_negative_flags(self.register_a);
        self.set_flag(CPUFlags::Carry, self.get_flag(CPUFlags::Negative));

        None
    }

    fn aax(&mut self, operand: Operand) -> Option<CPUError> {
        let addr = self.retreive_operand_address(operand).ok()?;
        let value = self.register_x & self.register_a;
        self.update_zero_and_negative_flags(value);
        self.mem_write(addr, value);

        None
    }

    fn arr(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;
        self.register_a &= value;
        self.ror(Operand { args: Arguments::None, mode: AddressingMode::NoneAddressing })?;
        self.update_zero_and_negative_flags(self.register_a);
        let bit5: bool = ((self.register_a >> 5) & 1) != 0;
        let bit6: bool = ((self.register_a >> 6) & 1) != 0;
        match (bit5, bit6) {
            (true, true) => {
                self.set_flag(CPUFlags::Carry,      true);
                self.set_flag(CPUFlags::Overflow,   false);
            },
            (true, false) => {
                self.set_flag(CPUFlags::Carry,      false);
                self.set_flag(CPUFlags::Overflow,   true);
            },
            (false, true) => {
                self.set_flag(CPUFlags::Carry,      true);
                self.set_flag(CPUFlags::Overflow,   true);
            },
            (false, false) => {
                self.set_flag(CPUFlags::Carry,      false);
                self.set_flag(CPUFlags::Overflow,   false);
            },
        }

        None
    }

    fn asr(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;
        self.register_a &= value;
        self.lsr(Operand { args: Arguments::None, mode: AddressingMode::NoneAddressing });

        None
    }

    fn atx(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;
        self.register_a &= value;
        self.tax(Operand { args: Arguments::None, mode: AddressingMode::NoneAddressing })?;

        None
    }

    fn axa(&mut self, operand: Operand) -> Option<CPUError> {
       let addr = self.retreive_operand_address(operand).ok()?;
       let value = self.register_a & self.register_x & 7;
       self.mem_write(addr, value);

       None
    }

    fn axs(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;
        let value_2c = (!value).wrapping_add(1);

        self.register_x &= self.register_a;

        let (result, carry, _overflow) = CPU::accumulator_add(self.register_a, value_2c);

        self.register_x = result;
        self.set_flag(CPUFlags::Carry, carry);
        self.update_zero_and_negative_flags(self.register_x);

        None
    }

    fn dcp(&mut self, operand: Operand) -> Option<CPUError> {
        let addr = self.retreive_operand_address(operand).ok()?;
        let value = self.mem_read(addr).ok()?;
        let dec1 = (!1u8).wrapping_add(1);

        let (result, carry, _overflow) = CPU::accumulator_add(value, dec1);
        self.mem_write(addr, result);
        self.set_flag(CPUFlags::Carry, carry);

        None
    }

    fn isc(&mut self, operand: Operand) -> Option<CPUError> {
        let addr = self.retreive_operand_address(operand).ok()?;
        let value = self.mem_read(addr).ok()?.wrapping_add(1);
        self.mem_write(addr, value);

        let value2c = (!value).wrapping_add(1);

        let (result, carry, overflow) = CPU::accumulator_add(self.register_a, value2c);
        self.register_a = result;
        self.set_flag(CPUFlags::Carry, carry);
        self.set_flag(CPUFlags::Overflow, overflow);
        self.update_zero_and_negative_flags(self.register_a);

        None
    }

    fn kil(&mut self, _operand: Operand) -> Option<CPUError> {
       Some(CPUError::HaltError)
    }

    fn lar(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;
        let result = value & self.stack_pointer;
        self.register_a = result;
        self.register_x = result;
        self.stack_pointer = result;
        self.update_zero_and_negative_flags(result);

        None
    }

    fn lax(&mut self, operand: Operand) -> Option<CPUError> {
        let value = self.retreive_operand_value(operand).ok()?;
        self.register_a = value;
        self.register_x = value;
        self.update_zero_and_negative_flags(value);

        None
    }

    fn rla(&mut self, operand: Operand) -> Option<CPUError> {
        self.rol(operand)?;
        self.and(operand)?;

        None
    }

    fn rra(&mut self, operand: Operand) -> Option<CPUError> {
        self.ror(operand)?;
        self.adc(operand)?;

        None
    }

    fn slo(&mut self, operand: Operand) -> Option<CPUError> {
       self.asl(operand)?;
       self.ora(operand)?;

       None
    }

    fn sre(&mut self, operand: Operand) -> Option<CPUError> {
       self.lsr(operand)?;
       self.eor(operand)?;

       None
    }

    fn sxa(&mut self, operand: Operand) -> Option<CPUError> {
        let addr = self.retreive_operand_address(operand).ok()?;
        let high_byte = (addr >> 8) as u8;
        let result = self.register_x & high_byte.wrapping_add(1);
        self.mem_write(addr, result);

        None
    }

    fn sya(&mut self, operand: Operand) -> Option<CPUError> {
        let addr = self.retreive_operand_address(operand).ok()?;
        let high_byte = (addr >> 8) as u8;
        let result = self.register_y & high_byte.wrapping_add(1);
        self.mem_write(addr, result);

        None
    }

    fn xaa(&mut self, _operand: Operand) -> Option<CPUError> {
       self.register_a = 0xFF;

       None
    }

    fn xas(&mut self, operand: Operand) -> Option<CPUError> {
        self.stack_pointer = self.register_x & self.register_a;

        let addr = self.retreive_operand_address(operand).ok()?;
        let high_byte = (addr >> 8) as u8;
        let result = self.stack_pointer & high_byte.wrapping_add(1);
        self.mem_write(addr, result);

        None
    }
}

impl Default for CPU {
    fn default() -> Self {
        Self::new()
    }
}

impl BitFlag<CPUFlags> for CPU {

    fn get_flag(&self, flag: CPUFlags) -> bool {
        let bit = (self.status >> flag as u8) & 1;
        
        bit != 0
    }

    fn set_flag(&mut self, flag: CPUFlags, value: bool) {
        let result: u8 = (value as u8) << (flag as u8);

        self.status &= !(1 << (flag as u8));
        self.status |= result;
    }
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: (1 << CPUFlags::InterruptDisable as u8),
            program_counter: bus::ROM,
            stack_pointer: (ADDR_STACK_TOP - ADDR_STACK_BOTTOM) as u8,
            bus: Bus::new(),

            delay_branch_succeed: false,
            delay_page_crossed: false
        }
    }

    pub fn load(&mut self, rom: Rom, start_pointer: Option<u16>) -> Option<MemReadError> {
        println!("Loading Rom:");

        rom.print_rom();
        self.bus.load_rom(rom);

        if let Some(pointer) = start_pointer { 
            self.program_counter = self.mem_read_u16(pointer).ok()?;
        }

        None
    }

    pub fn load_and_run(&mut self, rom: Rom, start_pointer: Option<u16>) {
        self.load(rom,start_pointer);
        self.run();
    }

    pub fn build_rom_load_and_run(&mut self, program: Vec<u8>, start_pointer: Option<u16>) {
        let rom = Rom::build_rom(program);
        self.load_and_run(rom, start_pointer);
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F) -> Option<CPUError>
    where 
        F: FnMut(&mut CPU),
    {
        let opcodes: &HashMap<u8, &'static opcodes::OpCode> = &opcodes::OPCODES_MAP;

        let mut fn_map: HashMap<u8, OpFunction> = HashMap::default();
        for (bytecode, opcode) in opcodes {
            let opfunction: OpFunction = match opcode.instruction {
                /* INTERRUPTS */
                "BRK" => Self::brk,
                "NOP" => Self::nop,
                "RTI" => Self::rti,

                /* BRANCHES */
                "BCC" => Self::bcc,
                "BCS" => Self::bcs,
                "BEQ" => Self::beq,
                "BNE" => Self::bne,
                "BMI" => Self::bmi,
                "BPL" => Self::bpl,
                "BVC" => Self::bvc,
                "BVS" => Self::bvs,
                "JMP" => Self::jmp,
                "JSR" => Self::jsr,
                "RTS" => Self::rts,

                /* MOVES */
                "STA" => Self::sta,
                "STX" => Self::stx,
                "STY" => Self::sty,
                "LDA" => Self::lda,
                "LDX" => Self::ldx,
                "LDY" => Self::ldy,
                "TAX" => Self::tax,
                "TXA" => Self::txa,
                "TAY" => Self::tay,
                "TYA" => Self::tya,
                "TSX" => Self::tsx,
                "TXS" => Self::txs,

                /* FLAGS */
                "CLC" => Self::clc,
                "SEC" => Self::sec,
                "CLD" => Self::cld,
                "SED" => Self::sed,
                "CLI" => Self::cli,
                "SEI" => Self::sei,
                "CLV" => Self::clv,
                "PHA" => Self::pha,
                "PLA" => Self::pla,
                "PHP" => Self::php,
                "PLP" => Self::plp,

                /* ARITHMETIC */
                "ADC" => Self::adc,
                "SBC" => Self::sbc,
                "ASL" => Self::asl,
                "LSR" => Self::lsr,
                "ROL" => Self::rol,
                "ROR" => Self::ror,
                "INC" => Self::inc,
                "INY" => Self::iny,
                "INX" => Self::inx,
                "DEC" => Self::dec,
                "DEX" => Self::dex,
                "DEY" => Self::dey,

                /* LOGIC */
                "AND" => Self::and,
                "BIT" => Self::bit,
                "CMP" => Self::cmp,
                "CPX" => Self::cpx,
                "CPY" => Self::cpy,
                "EOR" => Self::eor,
                "ORA" => Self:: ora,

                /* ---------------------
                 * ILLEGAL OPERATIONS 
                 * ---------------------
                 */
                "AAC" => Self::aac,                
                "AAX" => Self::aax,
                "ARR" => Self::arr,
                "ASR" => Self::asr,
                "ATX" => Self::atx,
                "AXA" => Self::axa,
                "AXS" => Self::axs,
                "DCP" => Self::dcp,
                "DOP" => Self::nop,
                "ISC" => Self::isc,
                "KIL" => Self::kil,
                "LAR" => Self::lar,
                "LAX" => Self::lax,
                "RLA" => Self::rla,                
                "RRA" => Self::rra,
                "SLO" => Self::slo,
                "SRE" => Self::sre,
                "SXA" => Self::sxa,
                "SYA" => Self::sya,
                "TOP" => Self::nop,
                "XAA" => Self::xaa,
                "XAS" => Self::xas,

                _ => panic!("Unsupported instruction: {:X}", bytecode),

            };

            fn_map.insert(*bytecode, opfunction);
        };
        
        let mut term_log = TerminalLog::new();
        term_log.log_write(&TerminalLog::wrap_color("\n--- Starting Run ---\n".to_string(), ANSIColor::Green));

        let mut file_log = FileLog::new().expect("Failed to open file.");

        loop {
            if self.bus.poll_nmi_interrupt() {
                self.interrupt_nmi()?;
            }

            let bytecode = self.mem_read(self.program_counter).ok()?;

            let opcode = opcodes.get(&bytecode).unwrap_or_else(|| panic!("CPU::run -- Missing opcode: {:x}", bytecode));
            self.program_counter += 1;
            let starting_program_counter = self.program_counter;

            let args = match opcode.len-1 {
                0 => Arguments::None,
                1 => {
                    let parse_u8 = self.mem_read(self.program_counter);
                    if let Err(e) = parse_u8 {
                        return Some(CPUError::from(e));
                    }
                    Arguments::One(parse_u8.unwrap())
                },
                2 => {
                    let parse_u16 = self.mem_read_u16(self.program_counter);
                    if let Err(e) = parse_u16 {
                        return Some(CPUError::from(e));
                    }
                    Arguments::Two(parse_u16.unwrap())
                },
                _ => panic!("Unsupported length for opcode: {:X}", bytecode),
            };
            
            //term_log.log_opcode_running(opcode_info, &args, self);
            file_log.log_opcode_running(opcode, &args, self);

            match fn_map[&bytecode](self, Operand::new(args, opcode.mode)) {
                Some(err) => match err {
                    CPUError::BreakError => {
                        let new_addr = self.bus.mem_read_u16(bus::INTERRUPT_VECTOR_IRQ).ok()?;

                        let brk_loop_check = self.mem_read(new_addr).ok()?;
                        if brk_loop_check == 0x00 {
                            return Some(CPUError::BreakError)
                        }

                        self.program_counter = new_addr;
                    },
                    _ => return Some(err),
                }
                None => ()
            }

            if starting_program_counter == self.program_counter {
                self.program_counter += (opcode.len-1) as u16;
            }

            self.bus.tick(opcode.cycles);

            callback(self);
        }
    }

    fn interrupt_nmi(&mut self) -> Option<MemReadError> {
        self.push_stack_u16(self.program_counter);
        let mut flag = self.status.clone();
        flag &= !(1 << CPUFlags::Break2 as u8 | 1 << CPUFlags::Break as u8);
        flag |= 0b10 << CPUFlags::Break as u8;

        self.push_stack(flag);
        self.set_flag(CPUFlags::InterruptDisable, true);

        self.bus.tick(2);
        self.program_counter = self.mem_read_u16(0xFFFA).ok()?;

        None
    }

    // ================
    // HELPER FUNCTIONS
    // ================

    fn branch_for_flag_value(&mut self, flag: CPUFlags, value: bool, offset: u8) {
        if self.get_flag(flag) == value {
            self.offset_program_counter(offset);
        }
    }

    fn compare(&mut self, register_value: u8, memory_value: u8) {
        self.set_flag(CPUFlags::Carry, register_value >= memory_value);
        self.update_zero_and_negative_flags(register_value.wrapping_sub(memory_value));

    }

    fn update_zero_and_negative_flags(&mut self, value: u8) {
        self.set_flag(CPUFlags::Zero, value == 0);
        self.set_flag(CPUFlags::Negative, (value & 0b1000_0000) != 0);
    }

    fn retreive_operand_value(&mut self, operand: Operand) -> Result<u8, MemReadError> {
        match (operand.mode, operand.args) {
            (AddressingMode::Immediate, Arguments::One(arg)) => Ok(arg),
            (AddressingMode::ZeroPage, Arguments::One(arg)) => self.mem_read_zero_page(arg, 0),
            (AddressingMode::ZeroPageX, Arguments::One(arg)) => self.mem_read_zero_page(arg, self.register_x),
            (AddressingMode::ZeroPageY, Arguments::One(arg)) => self.mem_read_zero_page(arg, self.register_y),
            (AddressingMode::Absolute, Arguments::Two(arg)) => self.mem_read(arg),
            (AddressingMode::AbsoluteX, Arguments::Two(arg)) => self.mem_read_absolute(arg, self.register_x),
            (AddressingMode::AbsoluteY, Arguments::Two(arg)) => self.mem_read_absolute(arg, self.register_y),
            (AddressingMode::IndirectX, Arguments::One(arg)) => {
                let low = self.mem_read_zero_page(arg, self.register_x)?;
                let high = self.mem_read_zero_page(arg, self.register_x+1)?;

                self.mem_read(Arguments::combine_args(low, high))
            },
            (AddressingMode::IndirectY, Arguments::One(arg)) => {
                let base = self.mem_read_u16(arg as u16)?;

                self.mem_read_absolute(base, self.register_y)
            },
            _ => panic!("retreive_operand_value -- Mode {:?} is not supported!", operand.mode),
        }
    }

    fn retreive_operand_address(&mut self, operand: Operand) -> Result<u16, MemReadError> {
        match (operand.mode, operand.args) {
            (AddressingMode::ZeroPage, Arguments::One(arg)) => Ok(arg as u16),
            (AddressingMode::ZeroPageX, Arguments::One(arg)) => Ok(arg.wrapping_add(self.register_x) as u16),
            (AddressingMode::ZeroPageY, Arguments::One(arg)) => Ok(arg.wrapping_add(self.register_y) as u16),
            (AddressingMode::Absolute, Arguments::Two(arg)) => Ok(arg),
            (AddressingMode::AbsoluteX, Arguments::Two(arg)) => Ok(arg.wrapping_add(self.register_x as u16)),
            (AddressingMode::AbsoluteY, Arguments::Two(arg)) => Ok(arg.wrapping_add(self.register_y as u16)),
            (AddressingMode::IndirectX, Arguments::One(arg)) => {
                let low = self.mem_read_zero_page(arg, self.register_x)?;
                let high = self.mem_read_zero_page(arg, self.register_x+1)?;
                
                Ok(Arguments::combine_args(low, high))
            },
            (AddressingMode::IndirectY, Arguments::One(arg)) => {
                let base = self.mem_read_u16(arg as u16)?;

                Ok(base.wrapping_add(self.register_y as u16))
            },
            _ => panic!("retreive_operand_address -- Mode {:?} is not supported!", operand.mode),
        }
    }

    fn mem_read_zero_page(&mut self, base: u8, offset: u8) -> Result<u8, MemReadError> {
        self.mem_read(base.wrapping_add(offset) as u16)
    }

    fn mem_read_absolute(&mut self, base: u16, offset: u8) -> Result<u8, MemReadError> {
        self.mem_read(base.wrapping_add(offset as u16))
    }

    fn offset_program_counter(&mut self, offset: u8) {
        let offset_amount = offset as i8;
        self.program_counter = self.program_counter.wrapping_add(1).wrapping_add_signed(offset_amount as i16);
    }

    fn push_stack(&mut self, value: u8) {
        self.mem_write(CPU::get_stack_address(self.stack_pointer), value);
        //println!("\t\tCPU::push_stack -- value:{:X} @ {:X}", value, CPU::get_stack_address(self.stack_pointer));
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn push_stack_u16(&mut self, value: u16) {
        let high = (value >> 8) as u8;
        let low = value as u8;
        self.push_stack(high);
        self.push_stack(low);
    }

    fn pop_stack(&mut self) -> Result<u8, MemReadError> {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        
        self.mem_read(CPU::get_stack_address(self.stack_pointer))
    }

    fn pop_stack_u16(&mut self) -> Result<u16, MemReadError> {
        let low = self.pop_stack()?;
        let high = self.pop_stack()?;

        Ok(((high as u16) << 8) | low as u16)
    }

    fn get_stack_address(stack_pointer: u8) -> u16 {
        ADDR_STACK_BOTTOM + (stack_pointer as u16)
    }

    fn accumulator_add(a: u8, b: u8) -> (u8, bool, bool) {
        let (result, carry) = a.overflowing_add(b);
        let overflow = ((a^result)&(b^result)&0b1000_0000) != 0;

        (result, carry, overflow)
    }
}

#[cfg(test)]
mod test {
    use crate::compute::bus;

    use super::*;

    fn test_run(bytes: Vec<u8>) -> CPU {
        let mut cpu = CPU::new();
        cpu.build_rom_load_and_run(bytes, None);
        return cpu;
    }

    #[test]
    fn test_adc_immediate_0x40_plus_0x01() {
        let cpu = test_run(vec![0xa9, 0x40, 0x69, 0x01, 0x00]);
        assert!(cpu.register_a == 0x41);
    }

    #[test]
    fn test_adc_immediate_flags_overflow_carry() {
        let cpu = test_run(vec![0xa9, 0x80, 0x69, 0x80, 0x00]);
        assert!(cpu.get_flag(CPUFlags::Overflow));
        assert!(cpu.get_flag(CPUFlags::Carry));
    }

    #[test]
    fn test_and_0x03_and_0x01() {
        let cpu = test_run(vec![0xa9, 0x03, 0x29, 0x01, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_asl_0x01() {
        let cpu = test_run(vec![0xa9, 0x01, 0x0A, 0x00]);
        assert!(cpu.register_a == 0x02);
    }

    #[test]
    fn test_bcc_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, false);
        cpu.build_rom_load_and_run(vec![0x90, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0x01);
    }
    
    #[test]
    fn test_bcc_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, true);
        cpu.build_rom_load_and_run(vec![0x90, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_bcs_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, true);
        cpu.build_rom_load_and_run(vec![0xB0, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_bcs_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, false);
        cpu.build_rom_load_and_run(vec![0xB0, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_beq_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Zero, true);
        cpu.build_rom_load_and_run(vec![0xF0, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_beq_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Zero, false);
        cpu.build_rom_load_and_run(vec![0xF0, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_bne_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Zero, false);
        cpu.build_rom_load_and_run(vec![0xD0, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_bne_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Zero, true);
        cpu.build_rom_load_and_run(vec![0xD0, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_bit_is_zero() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x0000, 0x00);
        cpu.build_rom_load_and_run(vec![0xA9, 0x04, 0x24, 0x00, 0x00], None);
        assert!(cpu.get_flag(CPUFlags::Zero));
    }

    #[test]
    fn test_bit_mem_flags_set() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x0000, 0xFF);
        cpu.build_rom_load_and_run(vec![0xA9, 0x00, 0x24, 0x00, 0x00], None);
        assert!(cpu.get_flag(CPUFlags::Overflow));
        assert!(cpu.get_flag(CPUFlags::Negative));
    }

    #[test]
    fn test_bit_mem_flags_not_set() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x0000, 0x00);
        cpu.build_rom_load_and_run(vec![0xA9, 0x00, 0x24, 0x00, 0x00], None);
        assert!(!cpu.get_flag(CPUFlags::Overflow));
        assert!(!cpu.get_flag(CPUFlags::Negative));
    }

    #[test]
    fn test_bmi_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Negative, true);
        cpu.build_rom_load_and_run(vec![0x30, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_bmi_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Negative, false);
        cpu.build_rom_load_and_run(vec![0x30, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_bpl_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Negative, false);
        cpu.build_rom_load_and_run(vec![0x10, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_bpl_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Negative, true);
        cpu.build_rom_load_and_run(vec![0x10, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_bvc_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Overflow, false);
        cpu.build_rom_load_and_run(vec![0x50, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_bvc_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Overflow, true);
        cpu.build_rom_load_and_run(vec![0x50, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_bvs_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Overflow, true);
        cpu.build_rom_load_and_run(vec![0x70, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_bvs_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Overflow, false);
        cpu.build_rom_load_and_run(vec![0x70, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00], None);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_clc() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, true);
        cpu.build_rom_load_and_run(vec![0x18, 0x00], None);
        assert!(!cpu.get_flag(CPUFlags::Carry));
    }

    #[test]
    fn test_sec() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, false);
        cpu.build_rom_load_and_run(vec![0x38, 0x00], None);
        assert!(cpu.get_flag(CPUFlags::Carry));
    }

    #[test]
    fn test_cld() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::DecimalMode, true);
        cpu.build_rom_load_and_run(vec![0xD8, 0x00], None);
        assert!(!cpu.get_flag(CPUFlags::DecimalMode));
    }

    #[test]
    fn test_sed() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::DecimalMode, false);
        cpu.build_rom_load_and_run(vec![0xF8, 0x00], None);
        assert!(cpu.get_flag(CPUFlags::DecimalMode));
    }

    #[test]
    fn test_cli() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::InterruptDisable, true);
        cpu.build_rom_load_and_run(vec![0x58, 0x00], None);
        assert!(!cpu.get_flag(CPUFlags::InterruptDisable));
    }

    #[test]
    fn test_sei() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::InterruptDisable, false);
        cpu.build_rom_load_and_run(vec![0x78, 0x00], None);
        assert!(cpu.get_flag(CPUFlags::InterruptDisable));
    }

    #[test]
    fn test_clv() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Overflow, true);
        cpu.build_rom_load_and_run(vec![0xB8, 0x00], None);
        assert!(!cpu.get_flag(CPUFlags::Overflow));
    }

    #[test]
    fn test_cmp_equal() {
        let cpu = test_run(vec![0xa9, 0x02, 0xC9, 0x02, 0x00]);
        assert!(cpu.get_flag(CPUFlags::Carry));
        assert!(cpu.get_flag(CPUFlags::Zero));
    }

    #[test]
    fn test_cmp_greater() {
        let cpu = test_run(vec![0xa9, 0x02, 0xC9, 0x01, 0x00]);
        assert!(cpu.get_flag(CPUFlags::Carry));
        assert!(!cpu.get_flag(CPUFlags::Zero));
    }

    #[test]
    fn test_cmp_lesser() {
        let cpu = test_run(vec![0xa9, 0x01, 0xC9, 0x02, 0x00]);
        assert!(!cpu.get_flag(CPUFlags::Carry));
        assert!(!cpu.get_flag(CPUFlags::Zero));
    }

    #[test]
    fn test_cmp_negative_carry() {
        let cpu = test_run(vec![0xa9, 0xFF, 0xC9, 0x01, 0x00]);
        assert!(cpu.get_flag(CPUFlags::Negative));
    }

    #[test]
    fn test_cpx_equal() {
        let cpu = test_run(vec![0xA2, 0x02, 0xE0, 0x02, 0x00]);
        assert!(cpu.get_flag(CPUFlags::Carry));
        assert!(cpu.get_flag(CPUFlags::Zero));
    }

    #[test]
    fn test_cpx_greater() {
        let cpu = test_run(vec![0xA2, 0x02, 0xE0, 0x01, 0x00]);
        assert!(cpu.get_flag(CPUFlags::Carry));
        assert!(!cpu.get_flag(CPUFlags::Zero));
    }

    #[test]
    fn test_cpx_lesser() {
        let cpu = test_run(vec![0xA2, 0x01, 0xE0, 0x02, 0x00]);
        assert!(!cpu.get_flag(CPUFlags::Carry));
        assert!(!cpu.get_flag(CPUFlags::Zero));
    }

    #[test]
    fn test_cpx_negative_carry() {
        let cpu = test_run(vec![0xA2, 0xFF, 0xE0, 0x01, 0x00]);
        assert!(cpu.get_flag(CPUFlags::Negative));
    }

    #[test]
    fn test_cpy_equal() {
        let cpu = test_run(vec![0xA0, 0x02, 0xC0, 0x02, 0x00]);
        assert!(cpu.get_flag(CPUFlags::Carry));
        assert!(cpu.get_flag(CPUFlags::Zero));
    }

    #[test]
    fn test_cpy_greater() {
        let cpu = test_run(vec![0xA0, 0x02, 0xC0, 0x01, 0x00]);
        assert!(cpu.get_flag(CPUFlags::Carry));
        assert!(!cpu.get_flag(CPUFlags::Zero));
    }

    #[test]
    fn test_cpy_lesser() {
        let cpu = test_run(vec![0xA0, 0x01, 0xC0, 0x02, 0x00]);
        assert!(!cpu.get_flag(CPUFlags::Carry));
        assert!(!cpu.get_flag(CPUFlags::Zero));
    }

    #[test]
    fn test_cpy_negative_carry() {
        let cpu = test_run(vec![0xA0, 0xFF, 0xC0, 0x01, 0x00]);
        assert!(cpu.get_flag(CPUFlags::Negative));
    }

    #[test]
    fn test_dec_0x2() {
        let mut cpu = CPU::new();
        const ADDR: u16 = 0x0000;
        cpu.mem_write(ADDR, 0x02);
        cpu.build_rom_load_and_run(vec![0xC6, 0x00, 0x00], None);
        assert!(cpu.mem_read(ADDR).unwrap()==0x01);
    }

    #[test]
    fn test_inc_0x0() {
        let mut cpu = CPU::new();
        const ADDR: u16 = 0x0000;
        cpu.mem_write(ADDR, 0x00);
        cpu.build_rom_load_and_run(vec![0xE6, 0x00, 0x00], None);
        assert!(cpu.mem_read(ADDR).unwrap()==0x01);
    }

    #[test]
    fn test_dex_0x2() {
        let cpu = test_run(vec![0xA2, 0x02, 0xCA, 0x00]);
        assert!(cpu.register_x==0x01);
    }

    #[test]
    fn test_inx_0x0() {
        let cpu = test_run(vec![0xA2, 0x00, 0xE8, 0x00]);
        assert!(cpu.register_x==0x01);
    }

    #[test]
    fn test_dey_0x2() {
        let cpu = test_run(vec![0xA0, 0x02, 0x88, 0x00]);
        assert!(cpu.register_y==0x01);
    }

    #[test]
    fn test_iny_0x0() {
        let cpu = test_run(vec![0xA0, 0x00, 0xC8, 0x00]);
        assert!(cpu.register_y==0x01);
    }

    #[test]
    fn test_eor_0x03_and_0x01() {
        let cpu = test_run(vec![0xA9, 0x03, 0x49, 0x01, 0x00]);
        assert!(cpu.register_a == 0x02);
    }

    #[test]
    fn test_jmp_absolute_0x1234() {
        let cpu = test_run(vec![0x4C, 0x34, 0x12, 0x00]);
        assert!(cpu.program_counter == 0x1234+1);
    }

    #[test]
    fn test_jmp_indirect_0x0000_to_0x1234() {
        let mut cpu = CPU::new();
        const ADDR: u16 = 0x0000;
        const ADDR_2: u16 = 0x1234;

        cpu.mem_write_u16(ADDR, ADDR_2);
        cpu.mem_write_u16(ADDR_2, 0x00);
        cpu.build_rom_load_and_run(vec![0x6C, 0x00, 0x00, 0x00], None);

        assert!(cpu.program_counter == (ADDR_2+1)); // +1 because of the break execute
    }

    #[test]
    fn test_jmp_indirect_bug_0x00ff_to_0x1234() {
        let mut cpu = CPU::new();
        const ADDR_LOW: u16 = 0x00FF;
        const ADDR_HIGH: u16 = 0x0000;
        const ADDR_2_LOW: u8 = 0x34;
        const ADDR_2_HIGH: u8 = 0x12;
        const ADDR_2_RESULT: u16 = 0x1234;

        cpu.mem_write(ADDR_LOW, ADDR_2_LOW);
        cpu.mem_write(ADDR_HIGH, ADDR_2_HIGH);
        cpu.build_rom_load_and_run(vec![0x6C, 0xFF, 0x00, 0x00], None);

        assert!(cpu.program_counter == (ADDR_2_RESULT+1)); // +1 because of the break execute
    }

    #[test]
    fn test_jsr() {
        let mut cpu = test_run(vec![0xEA, 0xEA, 0x20, 0x06, 0x80, 0x00, 0xEA, 0x00]);
        assert!(cpu.program_counter == bus::ROM+8);
        let stack_value = cpu.pop_stack_u16().unwrap();
        assert!(stack_value == 0x8006-1);
    }

    #[test]
    fn test_lda_0x01() {
        let cpu = test_run(vec![0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_ldx_0x01() {
        let cpu = test_run(vec![0xA2, 0x01, 0x00]);
        assert!(cpu.register_x == 0x01);
    }

    #[test]
    fn test_ldy_0x01() {
        let cpu = test_run(vec![0xA0, 0x01, 0x00]);
        assert!(cpu.register_y == 0x01);
    }

    #[test]
    fn test_lsr_0x02_to_0x01() {
        let cpu = test_run(vec![0xA9, 0x02, 0x4A, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_nop() {
        let mut cpu = CPU::new();
        let start_counter = cpu.program_counter;
        cpu.build_rom_load_and_run(vec![0xEA, 0x00], None);
        assert!(cpu.program_counter == (start_counter+2));
    }

    #[test]
    fn test_ora_0x04_and_0x01() {
        let cpu = test_run(vec![0xA9, 0x04, 0x09, 0x01, 0x00]);
        assert!(cpu.register_a == 0x05);
    }

    #[test]
    fn test_pha_0x02() {
        let mut cpu = CPU::new();
        cpu.status = 0x0;
        cpu.build_rom_load_and_run(vec![0xA9, 0x02, 0x48, 0x00], None);
        let value = cpu.pop_stack().unwrap();
        assert!(value == 0x02);
    }

    #[test]
    fn test_php_0x02() {
        let mut cpu = CPU::new();
        cpu.status = 0x0;
        cpu.set_flag(CPUFlags::Zero, true);
        cpu.build_rom_load_and_run(vec![0x08, 0x00], None);
        let stack_value = cpu.pop_stack().unwrap();
        assert!(stack_value == (0x02 | (1 << CPUFlags::Break as u8) | (1 << CPUFlags::Break2 as u8)));
    }

    #[test]
    fn test_pla_0x01() {
        let mut cpu = CPU::new();
        cpu.status = 0x0;
        cpu.push_stack(0x01);
        cpu.build_rom_load_and_run(vec![0x68, 0x00], None);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_plp_0x01() {
        let mut cpu = CPU::new();
        cpu.push_stack(0x01);
        cpu.build_rom_load_and_run(vec![0x28, 0x00], None);
        assert!(cpu.status == 0x01);
    }

    #[test]
    fn test_rol_0x80_with_carry_to_0x01() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, true);
        cpu.build_rom_load_and_run(vec![0xA9, 0x80, 0x2A, 0x00], None);
        assert!(cpu.register_a == 0x01);
        assert!(cpu.get_flag(CPUFlags::Carry));
    }

    #[test]
    fn test_rol_0x80_no_carry_to_0x00() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, false);
        cpu.build_rom_load_and_run(vec![0xA9, 0x80, 0x2A, 0x00], None);
        assert!(cpu.register_a == 0x00);
        assert!(cpu.get_flag(CPUFlags::Carry));
    }

    #[test]
    fn test_ror_0x00_with_carry_to_0x80() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, true);
        cpu.build_rom_load_and_run(vec![0xA9, 0x00, 0x6A, 0x00], None);
        assert!(cpu.register_a == 0x80);
        assert!(!cpu.get_flag(CPUFlags::Carry));
    }

    #[test]
    fn test_rti_0x8004() {
        let mut cpu = CPU::new();
        cpu.push_stack_u16(0x8002);
        cpu.push_stack(0x01);
        let start_counter = cpu.program_counter;
        cpu.build_rom_load_and_run(vec![0x40, 0x00, 0xEA, 0x00], None);
        assert!(cpu.status == 0x01);
        assert!(cpu.program_counter == start_counter+4);
    }

    #[test]
    fn test_rts_0x8004() {
        let mut cpu = CPU::new();
        cpu.push_stack_u16(0x8002);
        let start_counter = cpu.program_counter;
        cpu.build_rom_load_and_run(vec![0x60, 0x00, 0xEA, 0x00], None);
        assert!(cpu.program_counter == start_counter+4);
    }

    #[test]
    fn test_sbc_0x04_minus_0x03() {
        let cpu = test_run(vec![0xA9, 0x04, 0xE9, 0x03, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_sbc_0x01_minus_0x02() {
        let cpu = test_run(vec![0xA9, 0x01, 0xE9, 0x02, 0x00]);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_sta_0x01() {
        let mut cpu = test_run(vec![0xA9, 0x01, 0x85, 0x00, 0x00]);
        assert!(cpu.mem_read(0x00).unwrap() == 0x01);
    }

    #[test]
    fn test_stx_0x01() {
        let mut cpu = test_run(vec![0xA2, 0x01, 0x86, 0x00, 0x00]);
        assert!(cpu.mem_read(0x00).unwrap() == 0x01);
    }

    #[test]
    fn test_txa_0x01() {
        let cpu = test_run(vec![0xA2, 0x01, 0x8A, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_sty_0x01() {
        let mut cpu = test_run(vec![0xA0, 0x01, 0x84, 0x00, 0x00]);
        assert!(cpu.mem_read(0x00).unwrap() == 0x01);
    }

    #[test]
    fn test_tya_0x01() {
        let cpu = test_run(vec![0xA0, 0x01, 0x98, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_tax_0x01() {
        let cpu = test_run(vec![0xA9, 0x01, 0xAA, 0x00]);
        assert!(cpu.register_x == 0x01);
    }

    #[test]
    fn test_tay_0x01() {
        let cpu = test_run(vec![0xA9, 0x01, 0xA8, 0x00]);
        assert!(cpu.register_y == 0x01);
    }

    #[test]
    fn test_tsx_0x01() {
        let mut cpu = CPU::new();
        cpu.push_stack(0x01);
        cpu.build_rom_load_and_run(vec![0xBA, 0x00], None);
        assert!(cpu.register_x == ((ADDR_STACK_TOP - ADDR_STACK_BOTTOM - 1) as u8));
    }

    #[test]
    fn test_txs() {
        let mut cpu = CPU::new();
        const TEST_VALUE: u8 = (ADDR_STACK_TOP - ADDR_STACK_BOTTOM - 1) as u8;
        cpu.build_rom_load_and_run(vec![0xA2, TEST_VALUE, 0x9A, 0x00], None);
        assert!(cpu.stack_pointer == TEST_VALUE);
    }

    #[test]
    fn test_aac_0x83_and_0x82() {
        let cpu = test_run(vec![0xA9, 0x83, 0x0B, 0x82, 0x00]);
        assert!(cpu.register_a == 0x82);
        assert!(cpu.get_flag(CPUFlags::Negative));
        assert!(cpu.get_flag(CPUFlags::Carry));
    }

    #[test]
    fn test_aax_0x83_and_0x82() {
        let mut cpu = CPU::new();
        cpu.register_x = 0x83;
        cpu.register_a = 0x82;
        cpu.build_rom_load_and_run(vec![0x8F, 0x00, 0x00, 0x00], None);
        assert!(cpu.mem_read(0x0000).unwrap() == (0x83 & 0x82));
        assert!(cpu.get_flag(CPUFlags::Negative));
    }

    #[test]
    fn test_arr_0x83_and_0x82() {
        let mut cpu = CPU::new();
        cpu.register_a = 0x83;
        cpu.build_rom_load_and_run(vec![0x6B, 0x82, 0x00], None);
        assert!(cpu.register_a == 0x41);
        assert!(cpu.get_flag(CPUFlags::Overflow));
    }

    #[test]
    fn test_asr_0x83_and_0x82() {
        let mut cpu = CPU::new();
        cpu.register_a = 0x83;
        cpu.build_rom_load_and_run(vec![0x4B, 0x82, 0x00], None);
        assert!(cpu.register_a == 0x41);
    }

    #[test]
    fn test_atx_0x83_and_0x82() {
        let mut cpu = CPU::new();
        cpu.register_a = 0x83;
        cpu.build_rom_load_and_run(vec![0xAB, 0x82, 0x00], None);
        assert!(cpu.register_x == 0x82);
    }

    #[test]
    fn test_axa_0x83_and_0x82() {
        let mut cpu = CPU::new();
        cpu.register_a = 0x83;
        cpu.register_x = 0x82;
        cpu.build_rom_load_and_run(vec![0x9F, 0x00, 0x00, 0x00], None);
        assert!(cpu.mem_read(0x0000).unwrap() == 0x2);
    }
}
