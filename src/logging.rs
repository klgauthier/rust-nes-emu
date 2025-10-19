// Copyright 2025 Kevin Gauthier. All rights reserved.

use std::{fs::File, io::{BufWriter, Write}, path::PathBuf};

use chrono::{Datelike, Local, Timelike};

use crate::compute::bus::Memory;
use crate::compute::cpu::{AddressingMode, CPU};
use crate::compute::opcodes::OpCode;
use crate::compute::arguments::Arguments;

const LOG_FOLDER_PATH: &str = "logs";

#[derive(Debug, Clone, Copy)]
pub enum ANSIColor {
    Reset   = 0,
    Black   = 90,
    Red     = 91,
    Green   = 92,
    Yellow  = 93,
    Blue    = 94,
    Magenta = 95,
    Cyan    = 96,
    White   = 97
}

pub trait Logger {
    fn log_opcode(&mut self, address: u16, opcode: &OpCode, args: &Arguments);
    fn log_opcode_running(&mut self, opcode: &OpCode, args: &Arguments, cpu: &mut CPU);

    fn format_address(address: u16) -> String { format!("{:0>4X}", address) }

    fn format_opcode_hex_data(opcode: &OpCode, args: &Arguments) -> String {
        format!("{:0>2X} {}", opcode.code, args.to_separate_args_string())
    }

    fn format_opcode(opcode: &OpCode) -> String {
        opcode.instruction.to_string()
    }

    fn format_args(args: &Arguments, addressing_mode: &AddressingMode) -> String {
        match addressing_mode {
            AddressingMode::Immediate => format!("#${}", args),
            AddressingMode::ZeroPage => format!("${}", args),
            AddressingMode::ZeroPageX => format!("${},X", args),
            AddressingMode::ZeroPageY => format!("${},Y", args),
            AddressingMode::Absolute => format!("${}", args),
            AddressingMode::AbsoluteX => format!("${},X", args),
            AddressingMode::AbsoluteY => format!("${},Y", args),
            AddressingMode::IndirectX => format!("(${},X)", args),
            AddressingMode::IndirectY => format!("(${}),Y", args),
            AddressingMode::NoneAddressing => {
                match args {
                    Arguments::None => String::new(),
                    Arguments::One(_) | Arguments::Two(_) => format!("${} ", args),
                }
                // todo!("Handle some edge case instructions");
            },
        }
    }

    fn format_opcode_working_addresses(opcode: &OpCode, args: &Arguments, cpu: &mut CPU) -> String {
        let formatted = match opcode.mode {
            AddressingMode::ZeroPage => {
                format!("= {:0>2X}", cpu.mem_read(args.get_arg1().unwrap() as u16).unwrap())
            },
            AddressingMode::ZeroPageX => {
                let addr = args.get_arg1().unwrap().wrapping_add(cpu.register_x) as u16;
                format!("@ {:0>4X} = {:0>2X}", addr, cpu.mem_read(addr).unwrap())
            },
            AddressingMode::ZeroPageY => {
                let addr = args.get_arg1().unwrap().wrapping_add(cpu.register_y) as u16;
                format!("@ {:0>4X} = {:0>2X}", addr, cpu.mem_read(addr).unwrap())
            },
            AddressingMode::Absolute => {
                let operand_addr = match args { Arguments::Two(a) => a, _ => unreachable!()};
                format!("= {:0>2X}", cpu.mem_read(*operand_addr).unwrap())
            },
            AddressingMode::AbsoluteX => {
                let operand_addr = match args { Arguments::Two(a) => a, _ => unreachable!()};
                let addr = operand_addr.wrapping_add(cpu.register_x as u16);
                format!("@ {:0>4X} = {:0>2X}", addr, cpu.mem_read(addr).unwrap())
            },
            AddressingMode::AbsoluteY => {
                let operand_addr = match args { Arguments::Two(a) => a, _ => unreachable!()};
                let addr = operand_addr.wrapping_add(cpu.register_y as u16);
                format!("@ {:0>4X} = {:0>2X}", addr, cpu.mem_read(addr).unwrap())
            },
            AddressingMode::IndirectX => {
                let addr_offset = args.get_arg1().unwrap().wrapping_add(cpu.register_x);
                let addr = addr_offset as u16;
                format!("@ {:0>2X} = {:0>4X} = {:0>2X}", addr_offset, addr, cpu.mem_read(addr).unwrap())
            },
            AddressingMode::IndirectY => {
                let operand_addr = match args { Arguments::One(a) => cpu.mem_read_u16(*a as u16).unwrap(), _ => unreachable!()};
                let addr = operand_addr.wrapping_add(cpu.register_y as u16);
                format!("= {:0>4X} @ {:0>4X} = {:0>2X}", operand_addr, addr, cpu.mem_read(addr).unwrap())
            },
            AddressingMode::NoneAddressing | AddressingMode::Immediate => {
                "".to_owned()
            }
        };
        
        format!("{:<16}", formatted)
    }

    fn format_args_and_working_addresses(args: &Arguments, opcode: &OpCode, cpu: &mut CPU) -> String {
        let combined = format!("{} {}", Self::format_args(args, &opcode.mode), Self::format_opcode_working_addresses(opcode, args, cpu));
        format!("{:<26}", combined)
    }

    fn format_cpu_state(cpu: &CPU) -> String {
        let ppu1 = cpu.bus.get_ppu_cycles() / 1000;
        let ppu2 = cpu.bus.get_ppu_cycles() % 1000;
        
        format!("A:{reg_a:0>2X} X:{reg_x:0>2X} Y:{reg_y:0>2X} P:{status:0>2X} P:{status:0>2X} SP:{stack:0>2X} PPU:{ppu1:>3},{ppu2:>3} CYC:{cycle}",
            reg_a   = cpu.register_a,
            reg_x   = cpu.register_x,
            reg_y   = cpu.register_y,
            status  = cpu.status,
            stack   = cpu.stack_pointer,
            ppu1    = ppu1,
            ppu2    = ppu2,
            cycle   = cpu.bus.cycles
        )
    }
}

pub trait LogWrite {
    fn log_write(&mut self, string: &str);
}

pub struct FileLog {
    buf_writer: BufWriter<File>
}

impl FileLog {
    pub fn new() -> Result<FileLog, std::io::Error> {
        
        let now = Local::now();
        let now_string = format!("{year}-{month}-{day}-{hour}-{min}-{sec}", 
            year = now.year(),
            month = now.month(),
            day = now.day(),
            hour = now.hour(),
            min = now.minute(),
            sec = now.second()
        );

        let mut path = PathBuf::new();
        path.push(LOG_FOLDER_PATH);
        path.push(now_string);
        path.set_extension("log");
        let file_result = File::create(&path);

        match file_result {
            Ok(file) => {
                Ok(FileLog {
                    buf_writer: BufWriter::new(file),
                })
            }

            Err(e) => Err(e),
        }
    }
}

impl LogWrite for FileLog {
    fn log_write(&mut self, string: &str) {
        self.buf_writer.write_all(format!("{}\n", string).as_bytes()).expect("Error logging to file.");
    }
}

impl Logger for FileLog {
    fn log_opcode(&mut self, address: u16, opcode: &OpCode, args: &Arguments) {
        let line = format!("{addr} {opcode_hex_data} {opcode} {opcode_args}",
            addr = Self::format_address(address),
            opcode_hex_data = Self::format_opcode_hex_data(opcode, args),
            opcode = Self::format_opcode(opcode),
            opcode_args = Self::format_args(args, &opcode.mode)
        );
        self.log_write(&line);
    }

    fn log_opcode_running(&mut self, opcode: &OpCode, args: &Arguments, cpu: &mut CPU) {
        let line = format!("{addr} {opcode_hex_data} {opcode} {working_addrs} {cpu_state}",
            addr = Self::format_address(cpu.program_counter),
            opcode_hex_data = Self::format_opcode_hex_data(opcode, args),
            opcode = Self::format_opcode(opcode),
            working_addrs = Self::format_args_and_working_addresses(args, opcode, cpu),
            cpu_state = Self::format_cpu_state(cpu)
        );
        self.log_write(&line);
    }
}

pub struct TerminalLog {

}

impl Default for TerminalLog {
    fn default() -> Self {
        Self::new()
    }
}

impl TerminalLog {
    pub fn new() -> TerminalLog {
        TerminalLog {
        }
    }

    fn make_color(color: ANSIColor) -> String {
        format!("{}{}{}", "\x1b[", format_args!("{:?}", color as u8), "m")
    }

    pub fn wrap_color(inner: String, color: ANSIColor) -> String {
        format!("{}{}{}", Self::make_color(color), inner, Self::make_color(ANSIColor::Reset))
    }
}

impl LogWrite for TerminalLog {
    fn log_write(&mut self, string: &str) {
        println!("{}", string);
    }
}

impl Logger for TerminalLog {
    fn log_opcode(&mut self, address: u16, opcode: &OpCode, args: &Arguments) {
        let line = format!("{addr} {opcode_hex_data} {opcode} {opcode_args}",
            addr = Self::format_address(address),
            opcode_hex_data = Self::format_opcode_hex_data(opcode, args),
            opcode = Self::format_opcode(opcode),
            opcode_args = Self::format_args(args, &opcode.mode)
        );
        self.log_write(&line);
    }

    fn log_opcode_running(&mut self, opcode: &OpCode, args: &Arguments, cpu: &mut CPU) {
        let line = format!("{addr} {opcode_hex_data} {opcode} {working_addrs} {cpu_state}",
            addr = Self::format_address(cpu.program_counter),
            opcode_hex_data = Self::format_opcode_hex_data(opcode, args),
            opcode = Self::format_opcode(opcode),
            working_addrs = Self::format_args_and_working_addresses(args, opcode, cpu),
            cpu_state = Self::format_cpu_state(cpu)
        );
        self.log_write(&line);
    }

    fn format_address(address: u16) -> String { 
        Self::wrap_color( format!("{:0>4X}", address) , ANSIColor::Blue)
    }
}