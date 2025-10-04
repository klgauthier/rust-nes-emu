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
    pub stack_pointer: u8,
    memory: [u8; 0xFFFF]
}

const ADDR_PRG_ROM : usize = 0x600; // change back to 0x8000
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

pub trait Memory 
{
    fn mem_read(&self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&self, addr: u16) -> u16 {
        let low = self.mem_read(addr) as u16;
        let high = self.mem_read(addr+1) as u16;
        return (high << 8) | low;
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        let high = (data >> 8) as u8;
        let low = data as u8;
        self.mem_write(addr, low);
        self.mem_write(addr+1, high);
    }
}

impl Memory for CPU
{
    fn mem_read(&self, addr: u16) -> u8 {
        return self.memory[addr as usize];
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }
}

impl CPU
{
    pub fn new() -> Self {
        return CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: 0,
            program_counter: 0,
            stack_pointer: (ADDR_STACK_TOP - ADDR_STACK_BOTTOM) as u8,
            memory: [0; 0xFFFF]
        }
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[ADDR_PRG_ROM .. (ADDR_PRG_ROM + program.len())].copy_from_slice(&program[..]);
        self.program_counter = ADDR_PRG_ROM as u16;

        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;
        let mut i = 0;
        while i < program.len() {
            let instr = program[i];
            let opcode = opcodes[&instr];
            let arg_len = (opcode.len-1) as usize;

            print!("0x{addr:x} | {name} (${code:x})", addr=ADDR_PRG_ROM+i, name=opcode.instruction, code=instr);

            match arg_len {
                0 => {},
                1 => print!(": [{data:x}]", data=program[i+1]),
                2 => {
                    let low = program[i+1];
                    let high = program[i+2];
                    let args = ((high as u16) << 8) | (low as u16);
                    print!(": [{data:x}]", data=args)
                }
                _ => print!("???"),
            }

            println!("");

            i += arg_len+1;
        }
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.run();
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where 
        F: FnMut(&mut CPU),
    {
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;

        loop {
            let opcode = self.mem_read(self.program_counter);
            let opcode_info = opcodes.get(&opcode).expect(&format!("CPU::run -- Missing opscode: {:x}", opcode));
            self.program_counter += 1;
            let starting_program_counter = self.program_counter;

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
        self.set_flag(CPUFlags::Carry, (self.register_a & 0b1000_0000) != 0);
        self.register_a = self.register_a << 1;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn rol_memory(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_flag(CPUFlags::Carry, (value & 0b1000_0000) != 0);
        let result= value << 1;
        self.mem_write(addr, result);
        self.set_flag(CPUFlags::Zero, self.register_a == 0);
        self.set_flag(CPUFlags::Negative, (result & 0b1000_0000) != 0);
    }

    fn ror_accumulator(&mut self) {
        self.set_flag(CPUFlags::Carry, (self.register_a & 0b0000_0001) != 0);
        self.register_a = self.register_a >> 1;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn ror_memory(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_flag(CPUFlags::Carry, (value & 0b0000_0001) != 0);
        let result= value >> 1;
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
        //println!("\t\tCPU::push_stack -- value:{:x}", value);
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
mod test
{
    use super::*;

    fn test_run(bytes: Vec<u8>) -> CPU {
        let mut cpu = CPU::new();
        cpu.load_and_run(bytes);
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
        cpu.load_and_run(vec![0x90, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0x01);
    }
    
    #[test]
    fn test_bcc_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, true);
        cpu.load_and_run(vec![0x90, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_bcs_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, true);
        cpu.load_and_run(vec![0xB0, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_bcs_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, false);
        cpu.load_and_run(vec![0xB0, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_beq_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Zero, true);
        cpu.load_and_run(vec![0xF0, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_beq_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Zero, false);
        cpu.load_and_run(vec![0xF0, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_bne_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Zero, false);
        cpu.load_and_run(vec![0xD0, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_bne_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Zero, true);
        cpu.load_and_run(vec![0xD0, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_bit_is_zero() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x0000, 0x00);
        cpu.load_and_run(vec![0xA9, 0x04, 0x24, 0x00, 0x00]);
        assert!(cpu.get_flag(CPUFlags::Zero));
    }

    #[test]
    fn test_bit_mem_flags_set() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x0000, 0xFF);
        cpu.load_and_run(vec![0xA9, 0x00, 0x24, 0x00, 0x00]);
        assert!(cpu.get_flag(CPUFlags::Overflow));
        assert!(cpu.get_flag(CPUFlags::Negative));
    }

    #[test]
    fn test_bit_mem_flags_not_set() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x0000, 0x00);
        cpu.load_and_run(vec![0xA9, 0x00, 0x24, 0x00, 0x00]);
        assert!(!cpu.get_flag(CPUFlags::Overflow));
        assert!(!cpu.get_flag(CPUFlags::Negative));
    }

    #[test]
    fn test_bmi_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Negative, true);
        cpu.load_and_run(vec![0x30, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_bmi_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Negative, false);
        cpu.load_and_run(vec![0x30, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_bpl_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Negative, false);
        cpu.load_and_run(vec![0x10, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_bpl_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Negative, true);
        cpu.load_and_run(vec![0x10, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_bvc_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Overflow, false);
        cpu.load_and_run(vec![0x50, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_bvc_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Overflow, true);
        cpu.load_and_run(vec![0x50, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_bvs_pass() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Overflow, true);
        cpu.load_and_run(vec![0x70, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0x01);
    }

    #[test]
    fn test_bvs_fail() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Overflow, false);
        cpu.load_and_run(vec![0x70, 0x03, 0xA9, 0xFF, 0x00, 0xA9, 0x01, 0x00]);
        assert!(cpu.register_a == 0xFF);
    }

    #[test]
    fn test_clc() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, true);
        cpu.load_and_run(vec![0x18, 0x00]);
        assert!(!cpu.get_flag(CPUFlags::Carry));
    }

    #[test]
    fn test_sec() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Carry, false);
        cpu.load_and_run(vec![0x38, 0x00]);
        assert!(cpu.get_flag(CPUFlags::Carry));
    }

    #[test]
    fn test_cld() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::DecimalMode, true);
        cpu.load_and_run(vec![0xD8, 0x00]);
        assert!(!cpu.get_flag(CPUFlags::DecimalMode));
    }

    #[test]
    fn test_sed() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::DecimalMode, false);
        cpu.load_and_run(vec![0xF8, 0x00]);
        assert!(cpu.get_flag(CPUFlags::DecimalMode));
    }

    #[test]
    fn test_cli() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::InterruptDisable, true);
        cpu.load_and_run(vec![0x58, 0x00]);
        assert!(!cpu.get_flag(CPUFlags::InterruptDisable));
    }

    #[test]
    fn test_sei() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::InterruptDisable, false);
        cpu.load_and_run(vec![0x78, 0x00]);
        assert!(cpu.get_flag(CPUFlags::InterruptDisable));
    }

    #[test]
    fn test_clv() {
        let mut cpu = CPU::new();
        cpu.set_flag(CPUFlags::Overflow, true);
        cpu.load_and_run(vec![0xB8, 0x00]);
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
        cpu.load_and_run(vec![0xC6, 0x00, 0x00]);
        assert!(cpu.mem_read(ADDR)==0x01);
    }

    #[test]
    fn test_inc_0x0() {
        let mut cpu = CPU::new();
        const ADDR: u16 = 0x0000;
        cpu.mem_write(ADDR, 0x00);
        cpu.load_and_run(vec![0xE6, 0x00, 0x00]);
        assert!(cpu.mem_read(ADDR)==0x01);
    }

    #[test]
    fn test_dex_0x2() {
        let cpu = test_run(vec![0xA2, 0x02, 0xE6, 0x00]);
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
        assert!(cpu.program_counter == 0x1234);
    }

    #[test]
    fn test_jmp_indirect_0x0000_to_0x1234() {
        let mut cpu = CPU::new();
        const ADDR: u16 = 0x0000;
        const ADDR_2: u16 = 0x1234;

        cpu.mem_write_u16(ADDR, ADDR_2);
        cpu.mem_write_u16(ADDR_2, 0x00);
        cpu.load_and_run(vec![0x6C, 0x00, 0x00, 0x00]);

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
        cpu.load_and_run(vec![0x6C, 0xFF, 0x00, 0x00]);

        assert!(cpu.program_counter == (ADDR_2_RESULT+1)); // +1 because of the break execute
    }

}