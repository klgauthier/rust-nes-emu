// Copyright 2025 Kevin Gauthier. All rights reserved.

use std::collections::HashMap;
use crate::cartridge::Rom;
use crate::logging::{ANSIColor, Arguments, Logging};
use crate::{bus, opcodes};
use crate::bus::{Bus, Memory};

pub struct CPU
{
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: u8,
    pub program_counter: u16,
    pub stack_pointer: u8,
    pub bus: Bus,

    pub cycle: u32,
}

const ADDR_STACK_TOP : u16 = 0x01FF;
const ADDR_STACK_BOTTOM : u16 = 0x0100;

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

impl Memory for CPU
{
    fn mem_read(&self, addr: u16) -> u8 {
        return self.bus.mem_read(addr);
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data);
    }

    fn mem_read_u16(&self, addr: u16) -> u16 {
        return self.bus.mem_read_u16(addr);
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        self.bus.mem_write_u16(addr, data);
    }
}

impl CPU
{
    pub fn new() -> Self {
        return CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: 0 | (1 << CPUFlags::InterruptDisable as u8),
            program_counter: bus::ROM,
            stack_pointer: (ADDR_STACK_TOP - ADDR_STACK_BOTTOM) as u8,
            bus: Bus::new(),
            cycle: 0,
        }
    }

    pub fn load(&mut self, rom: Rom, start_pointer: Option<u16>) {
        println!("Loading Rom:");
        rom.print_rom();
        self.bus.load_rom(rom);
        match start_pointer {
            Some(pointer) => self.program_counter = self.mem_read_u16(pointer),
            None => ()
        }
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

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where 
        F: FnMut(&mut CPU),
    {
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;
        
        let mut logger = Logging::new(false, true);
        logger.log_line(logger.wrap_color("\n--- Starting Run ---\n".to_string(), ANSIColor::Green));

        loop {
            let opcode = self.mem_read(self.program_counter);
            let opcode_info = opcodes.get(&opcode).expect(&format!("CPU::run -- Missing opscode: {:x}", opcode));
            self.program_counter += 1;
            let starting_program_counter = self.program_counter;

            let args = match opcode_info.len-1 {
                0 => Arguments::None,
                1 => Arguments::One(self.mem_read(self.program_counter)),
                2 => Arguments::Two(self.mem_read_u16(self.program_counter)),
                _ => panic!("Unsupported length for opcode: {:X}", opcode),
            };
            logger.log_running_opcode(&self, opcode_info, args);

            //println!("CPU::run -- Addr:{addr:x} | {opcode}: {mode:?} ${hex:x}", addr = self.program_counter-1, opcode = opcode_info.instruction, mode = opcode_info.mode, hex = opcode);

            match opcode_info.instruction {
                /* INTERRUPTS */
                "BRK" => {
                    return;
                }

                "NOP" => {
                    self.nop();
                }

                "RTI" => {
                    self.rti();
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
                    self.jsr(&opcode_info.mode, opcode_info.len.saturating_sub(1));
                }

                "RTS" => {
                    self.rts();
                }

                /* MOVES */
                "STA" => {
                    self.sta(&opcode_info.mode);
                }

                "STX" => {
                    self.stx(&opcode_info.mode);
                }

                "STY" => {
                    self.sty(&opcode_info.mode);
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

                "TXA" => {
                    self.txa();
                }

                "TAY" => {
                    self.tay();
                }

                "TYA" => {
                    self.tya();
                }

                "TSX" => {
                    self.tsx();
                }

                "TXS" => {
                    self.txs();
                }

                /* FLAGS */
                "CLC" => {
                    self.clc();
                }

                "SEC" => {
                    self.sec();
                }

                "CLD" => {
                    self.cld();
                }

                "SED" => {
                    self.sed();
                }

                "CLI" => {
                    self.cli();
                }

                "SEI" => {
                    self.sei();
                }

                "CLV" => {
                    self.clv();
                }

                "PHA" => {
                    self.pha();
                }

                "PLA" => {
                    self.pla();
                }

                "PHP" => {
                    self.php();
                }

                "PLP" => {
                    self.plp();
                }

                /* ARITHMETIC */
                "ADC" => {
                    self.adc(&opcode_info.mode);
                }

                "SBC" => {
                    self.sbc(&opcode_info.mode);
                }

                "ASL" => {
                    if opcode == 0x0A {
                        self.asl_accumulator();
                    }
                    else {
                        self.asl_memory(&opcode_info.mode);
                    }
                }

                "LSR" => {
                    if opcode == 0x4A {
                        self.lsr_accumulator();
                    }
                    else {
                        self.lsr_memory(&opcode_info.mode);
                    }
                }

                "ROL" => {
                    if opcode == 0x2A {
                        self.rol_accumulator();
                    }
                    else {
                        self.rol_memory(&opcode_info.mode);
                    }

                }

                "ROR" => {
                    if opcode == 0x6A {
                        self.ror_accumulator();
                    }
                    else {
                        self.ror_memory(&opcode_info.mode);
                    }
                }

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

                "ORA" => {
                    self.ora(&opcode_info.mode);
                }
                
                _ => todo!(),
            }

            if starting_program_counter == self.program_counter {
                self.program_counter += (opcode_info.len-1) as u16;
            }

            self.cycle += opcode_info.cycles as u32;

            callback(self);
        }
    }

    // ==========
    // OPERATIONS
    // ==========
    
    fn adc(&mut self, mode: &AddressingMode) {
        let value = self.retreive_from_mem(mode);

        let (result, carry, overflow) = CPU::accumulator_add(self.register_a, value);

        self.register_a = result;
        self.set_flag(CPUFlags::Carry, carry);
        self.set_flag(CPUFlags::Overflow, overflow);
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
        
        self.set_flag(CPUFlags::Overflow, (value & 0b0010_0000) != 0);
        self.set_flag(CPUFlags::Negative, (value & 0b0100_0000) != 0);
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
        let mut addr = self.get_operand_address(&AddressingMode::Absolute);

        if *mode == AddressingMode::NoneAddressing {
            // emulating 6502 bug with reading at the boundary of pages
            // ref: https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP

            if (addr & 0x00FF) == 0x00FF {
                let high = self.mem_read(addr & 0xFF00);
                addr = self.mem_read(addr & 0x00FF) as u16 | ((high as u16) << 8);
            }
            else {
                addr = self.mem_read_u16(addr);
            }
        }

        //println!("\tCPU::jmp -- Addr: {:x}", addr);
        self.program_counter = addr; 
    }

    fn jsr(&mut self, mode: &AddressingMode, offset: u8) {
        let addr = self.get_operand_address(mode);

        //println!("\tCPU::jsr -- Addr: {:x}, Pushing: {:x}", addr, self.program_counter.wrapping_add(offset as u16));

        self.push_stack_u16(self.program_counter.wrapping_add(offset as u16));
        self.program_counter = addr;
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

    fn lsr_accumulator(&mut self) {
        self.set_flag(CPUFlags::Carry, (self.register_a & 0b0000_0001) != 0);
        self.register_a = self.register_a >> 1;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn lsr_memory(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_flag(CPUFlags::Carry, (value & 0b0000_0001) != 0);
        let shifted_value = value >> 1;
        self.mem_write(addr, shifted_value);
        self.update_zero_and_negative_flags(shifted_value);
    }
    
    fn nop(&self) {

    }

    fn ora(&mut self, mode: &AddressingMode) {
        let value = self.retreive_from_mem(mode);

        self.register_a = self.register_a | value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn pha(&mut self) {
        self.push_stack(self.register_a);
    }

    fn php(&mut self) {
        self.push_stack(self.status);
    }

    fn pla(&mut self) {
        self.register_a = self.pop_stack();
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn plp(&mut self) {
        self.status = self.pop_stack();
    }

    fn rol_accumulator(&mut self) {
        let old_carry = self.get_flag(CPUFlags::Carry) as u8;
        self.set_flag(CPUFlags::Carry, (self.register_a & 0b1000_0000) != 0);
        self.register_a = (self.register_a << 1) | old_carry;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn rol_memory(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let old_carry = self.get_flag(CPUFlags::Carry) as u8;
        self.set_flag(CPUFlags::Carry, (value & 0b1000_0000) != 0);
        let result= (value << 1) | old_carry;
        self.mem_write(addr, result);
        self.set_flag(CPUFlags::Zero, self.register_a == 0);
        self.set_flag(CPUFlags::Negative, (result & 0b1000_0000) != 0);
    }

    fn ror_accumulator(&mut self) {
        let old_carry = self.get_flag(CPUFlags::Carry) as u8;
        self.set_flag(CPUFlags::Carry, (self.register_a & 0b0000_0001) != 0);
        self.register_a = (self.register_a >> 1) | (old_carry << 7);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn ror_memory(&mut self, mode: &AddressingMode) {
        let old_carry = self.get_flag(CPUFlags::Carry) as u8;
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_flag(CPUFlags::Carry, (value & 0b0000_0001) != 0);
        let result= (value >> 1) | (old_carry << 7);
        self.mem_write(addr, result);
        self.set_flag(CPUFlags::Zero, self.register_a == 0);
        self.set_flag(CPUFlags::Negative, (result & 0b1000_0000) != 0);
    }

    fn rti(&mut self) {
        self.status = self.pop_stack();
        self.program_counter = self.pop_stack_u16();
    }

    fn rts(&mut self) {
        self.program_counter = self.pop_stack_u16();

        //println!("\tCPU::rts -- Popped addr: {:x}", self.program_counter);
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let value = self.retreive_from_mem(mode);

        let value_2c = (!value).wrapping_add(1);
        let (result, carry, overflow) = CPU::accumulator_add(self.register_a, value_2c);

        self.register_a = result;
        self.set_flag(CPUFlags::Carry, carry);
        self.set_flag(CPUFlags::Overflow, overflow);
    }

    fn sec(&mut self) {
        self.set_flag(CPUFlags::Carry, true);
    }

    fn sed(&mut self) {
        self.set_flag(CPUFlags::DecimalMode, true);
    }

    fn sei(&mut self) {
        self.set_flag(CPUFlags::InterruptDisable, true);
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn tsx(&mut self) {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
    }

    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    // ================
    // HELPER FUNCTIONS
    // ================

    fn get_flag(&self, flag: CPUFlags) -> bool {
        let bit = (self.status >> flag as u8) & 1;
        return bit != 0;
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
        self.set_flag(CPUFlags::Negative, (value & 0b1000_0000) != 0);
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

    fn pop_stack(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        let value = self.mem_read(CPU::get_stack_address(self.stack_pointer));
        //println!("\t\tCPU::pop_stack -- value:{:x}", value);
        return value;
    }

    fn pop_stack_u16(&mut self) -> u16 {
        let low = self.pop_stack();
        let high = self.pop_stack();
        return ((high as u16) << 8) | low as u16;
    }

    pub fn get_ppu_cycle(&self) -> u32 {
        return self.cycle * 7;
    }

    fn get_stack_address(stack_pointer: u8) -> u16 {
        return ADDR_STACK_BOTTOM + (stack_pointer as u16);
    }

    fn accumulator_add(a: u8, b: u8) -> (u8, bool, bool) {
        let (result, carry) = a.overflowing_add(b);
        let overflow = ((a^result)&(b^result)&0b1000_0000) != 0;
        return (result, carry, overflow);
    }
}

#[cfg(test)]
mod test {
    use crate::bus;

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
        assert!(cpu.mem_read(ADDR)==0x01);
    }

    #[test]
    fn test_inc_0x0() {
        let mut cpu = CPU::new();
        const ADDR: u16 = 0x0000;
        cpu.mem_write(ADDR, 0x00);
        cpu.build_rom_load_and_run(vec![0xE6, 0x00, 0x00], None);
        assert!(cpu.mem_read(ADDR)==0x01);
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

        println!("{:X}", cpu.program_counter);

        assert!(cpu.program_counter == (ADDR_2_RESULT+1)); // +1 because of the break execute
    }

    #[test]
    fn test_jsr() {
        let mut cpu = test_run(vec![0xEA, 0xEA, 0x20, 0x06, 0x80, 0x00, 0xEA, 0x00]);
        println!("{:X}", cpu.program_counter);
        assert!(cpu.program_counter == bus::ROM+8);
        println!("Stack pointer: {:X}", cpu.stack_pointer);
        println!("Stack data @{:X} = {:X}", CPU::get_stack_address(cpu.stack_pointer-1), cpu.mem_read(CPU::get_stack_address(cpu.stack_pointer-1)));
        println!("Stack data @{:X} = {:X}", CPU::get_stack_address(cpu.stack_pointer  ),  cpu.mem_read(CPU::get_stack_address(cpu.stack_pointer  )));
        println!("Stack data @{:X} = {:X}", CPU::get_stack_address(cpu.stack_pointer+1),  cpu.mem_read(CPU::get_stack_address(cpu.stack_pointer+1)));
        println!("Stack data @{:X} = {:X}", CPU::get_stack_address(cpu.stack_pointer+2),  cpu.mem_read(CPU::get_stack_address(cpu.stack_pointer+2)));
        let stack_value = cpu.pop_stack_u16();
        println!("Stack Value: {:X}", stack_value);
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
        let value = cpu.pop_stack();
        assert!(value == 0x02);
    }

    #[test]
    fn test_php_0x02() {
        let mut cpu = CPU::new();
        cpu.status = 0x0;
        cpu.set_flag(CPUFlags::Zero, true);
        cpu.build_rom_load_and_run(vec![0x08, 0x00], None);
        let stack_value = cpu.pop_stack();
        assert!(stack_value == 0x02);
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
        let cpu = test_run(vec![0xA9, 0x01, 0x85, 0x00, 0x00]);
        assert!(cpu.mem_read(0x00) == 0x01);
    }

    #[test]
    fn test_stx_0x01() {
        let cpu = test_run(vec![0xA2, 0x01, 0x86, 0x00, 0x00]);
        assert!(cpu.mem_read(0x00) == 0x01);
    }

    #[test]
    fn test_txa_0x01() {
        let cpu = test_run(vec![0xA2, 0x01, 0x8A, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_sty_0x01() {
        let cpu = test_run(vec![0xA0, 0x01, 0x84, 0x00, 0x00]);
        assert!(cpu.mem_read(0x00) == 0x01);
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
}