// Copyright 2025 Kevin Gauthier. All rights reserved.

use std::{fs::File, io::{BufWriter, Write}, path::PathBuf};

use chrono::{Datelike, Local, Timelike};

use crate::{bus::Memory, cpu::{AddressingMode, CPU}, opcodes::OpCode};

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

#[derive(Clone, Copy, Debug)]
pub enum Arguments {
    None,
    One(u8),
    Two(u16),
}

impl Arguments {
    pub fn get_arg1(&self) -> Option<u8> {
        match &self {
            Arguments::None => None,
            Arguments::One(arg) => Some(*arg),
            Arguments::Two(arg) => Some((arg & 0xFF) as u8),
        }
    }
    
    pub fn get_arg2(&self) -> Option<u8> {
        match &self {
            Arguments::None => None,
            Arguments::One(_) => None,
            Arguments::Two(arg) => Some((arg >> 8) as u8),
        }
    }

    pub fn to_string(&self) -> String {
        match &self {
            Arguments::None => "".to_string(),
            Arguments::One(arg) => format!("{:0>2X}", arg),
            Arguments::Two(arg) => format!("{:0>4X}", arg),
        }
    }

    pub fn to_separate_args_string(&self) -> String  {
        match &self {
            Arguments::None => "     ".to_string(),
            Arguments::One(arg) => format!("{:0>2X}   ", arg),
            Arguments::Two(_) => format!("{:0>2X} {:0>2X}", self.get_arg1().unwrap(), self.get_arg2().unwrap()),
        }
    }
}

trait LogWrite {
    fn log_write(&mut self, string: &str);
}

struct FileLog {
    buf_writer: BufWriter<File>
}

impl LogWrite for FileLog {
    fn log_write(&mut self, string: &str) {
        self.buf_writer.write(format!("{}\n", string).as_bytes()).expect("Error logging to file.");
    }
}

struct TerminalLog {
}

impl LogWrite for TerminalLog {
    fn log_write(&mut self, string: &str) {
        println!("{}", string);
    }
}

pub struct Logging {
    current_color: ANSIColor,
    terminal_writer: Option<TerminalLog>,
    file_writer: Option<FileLog>
}

impl Logging {
    pub fn new(log_terminal: bool, log_file: bool) -> Logging {
        let file_writer: Option<FileLog> = match log_file {
            true => {
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
                let file = File::create(&path)
                    .expect(&format!("Failed to open file at: {:?}", &path));
                
                Some(FileLog { buf_writer: BufWriter::new(file) })
            },
            false => None,
        };

        let terminal_writer: Option<TerminalLog> = match log_terminal {
            true => {
                Some(TerminalLog {  })
            },
            false => None,
        };

        Logging {
            current_color: ANSIColor::Reset,
            terminal_writer: terminal_writer,
            file_writer: file_writer,
        }
    }

    pub fn set_color(&mut self, color: ANSIColor) {
        self.current_color = color.clone();
        print!("{}", Self::make_color(color));
    }

    pub fn reset_color(&mut self) {
        self.set_color(ANSIColor::Reset);
    }

    pub fn wrap_color(&self, inner: String, color: ANSIColor) -> String {
        format!("{}{}{}", Self::make_color(color), inner, Self::make_color(self.current_color))
    }

    pub fn make_color(color: ANSIColor) -> String {
        format!("{}{}{}", "\x1b[", format!("{:?}", color as u8), "m")
    }

    pub fn log_opcode(&mut self, address: u16, opcode: &OpCode, arg1: Option<u8>, arg2: Option<u8>) {
        let args = match (arg1, arg2) {
            (None, None) => Arguments::None,
            (None, Some(_)) => panic!("Arg2 without an arg1."),
            (Some(arg1), None) => Arguments::One(arg1),
            (Some(arg1), Some(arg2)) => Arguments::Two((arg1 as u16) | ((arg2 as u16) << 8))
        };

        self.log_line(self.make_opcode_part1(address, opcode, args));
    }

    pub fn log_running_opcode(&mut self, cpu: &CPU, opcode: &OpCode, args: Arguments) {
        let part1 = self.make_opcode_part1(cpu.program_counter, opcode, args);
        
        let part2 = match opcode.mode {
            AddressingMode::ZeroPage => {
                format!("= {:0>2X}              ", cpu.mem_read(args.get_arg1().unwrap() as u16))
            },
            AddressingMode::ZeroPageX => {
                let addr = args.get_arg1().unwrap().wrapping_add(cpu.register_x) as u16;
                format!("@ {:0>4X} = {:0>2X}     ", addr, cpu.mem_read(addr))
            },
            AddressingMode::ZeroPageY => {
                let addr = args.get_arg1().unwrap().wrapping_add(cpu.register_y) as u16;
                format!("@ {:0>4X} = {:0>2X}     ", addr, cpu.mem_read(addr))
            },
            AddressingMode::Absolute => {
                let operand_addr = match args { Arguments::Two(a) => a, _ => unreachable!()};
                format!("= {:0>2X}            ", cpu.mem_read(operand_addr))
            },
            AddressingMode::AbsoluteX => {
                let operand_addr = match args { Arguments::Two(a) => a, _ => unreachable!()};
                let addr = operand_addr.wrapping_add(cpu.register_x as u16);
                format!("@ {:0>4X} = {:0>2X}     ", addr, cpu.mem_read(addr))
            },
            AddressingMode::AbsoluteY => {
                let operand_addr = match args { Arguments::Two(a) => a, _ => unreachable!()};
                let addr = operand_addr.wrapping_add(cpu.register_y as u16);
                format!("@ {:0>4X} = {:0>2X}     ", addr, cpu.mem_read(addr))
            },
            AddressingMode::IndirectX => {
                let addr_offset = args.get_arg1().unwrap().wrapping_add(cpu.register_x);
                let addr = addr_offset as u16;
                format!("@ {:0>2X} = {:0>4X} = {:0>2X}", addr_offset, addr, cpu.mem_read(addr))
            },
            AddressingMode::IndirectY => {
                let operand_addr = match args { Arguments::One(a) => cpu.mem_read_u16(a as u16), _ => unreachable!()};
                let addr = operand_addr.wrapping_add(cpu.register_y as u16);
                format!("= {:0>4X} @ {:0>4X} = {:0>2X}", operand_addr, addr, cpu.mem_read(addr))
            },
            AddressingMode::NoneAddressing | AddressingMode::Immediate => {
                if opcode.len == 1 {
                    format!("                     ")
                }
                else {
                    format!("                 ")
                }
            } 
        };

        let ppu1 = cpu.get_ppu_cycle() / 1000;
        let ppu2 = cpu.get_ppu_cycle() % 1000;
        let part3 = format!{"A:{reg_a:0>2X} X:{reg_x:0>2X} Y:{reg_y:0>2X} P:{status:0>2X} P:{status:0>2X} SP:{stack:0>2X} PPU:{ppu1:>3},{ppu2:>3} CYC:{cycle}",
            reg_a   = cpu.register_a,
            reg_x   = cpu.register_x,
            reg_y   = cpu.register_y,
            status  = cpu.status,
            stack   = cpu.stack_pointer,
            ppu1    = ppu1,
            ppu2    = ppu2,
            cycle   = cpu.cycle
        };

        self.log_line(format!{"{} {} {}", part1, part2, part3});
    }

    fn make_opcode_part1(&self, address: u16, opcode: &OpCode, args: Arguments) -> String {
        let addr = self.wrap_color(format!("{:0>4X}", address), ANSIColor::Blue);
        let op_hex = format!("{:0>2X} {}",
            opcode.code,
            args.to_separate_args_string()
        );
        let instr = opcode.instruction;
        let mut instr_arg: String = args.to_string();
        instr_arg = Self::format_args(instr_arg, &opcode.mode, opcode.instruction);

        return format!{"{}  {}  {} {}", addr, op_hex, instr, instr_arg};
    }

    pub fn log_line(&mut self, line: String) {
        if self.terminal_writer.is_some() {
            self.terminal_writer.as_mut().unwrap().log_write(&line);
        }

        if self.file_writer.is_some() {
            self.file_writer.as_mut().unwrap().log_write(&line);
        }
    }

    fn format_args(args: String, addressing_mode: &AddressingMode, _instruction: &str) -> String {
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
                if args.is_empty() {
                    format!("")
                }
                else {
                    format!("${} ", args)
                }
                // todo!("Handle some edge case instructions");
            },
        }
    }

}