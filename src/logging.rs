// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::{cpu::{AddressingMode, CPU}, opcodes::OpCode};

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

pub struct LogLocations {
    terminal: bool,
    file: bool
}

pub struct Logging {
    locations: LogLocations,
    current_color: ANSIColor,
}

impl Logging {
    pub fn new(log_terminal: bool, log_file: bool) -> Logging {
        Logging {
            locations: LogLocations{ terminal: log_terminal, file: log_file },
            current_color: ANSIColor::Reset,
        }
    }

    pub fn set_color(&mut self, color: ANSIColor) {
        self.current_color = color.clone();
        print!("{}", Self::make_color(color));
    }

    pub fn reset_color(&mut self) {
        self.set_color(ANSIColor::Reset);
    }

    fn wrap_color(&self, inner: String, color: ANSIColor) -> String {
        format!("{}{}{}", Self::make_color(color), inner, Self::make_color(self.current_color))
    }

    fn make_color(color: ANSIColor) -> String {
        format!("{}{}{}", "\x1b[", format!("{:?}", color as u8), "m")
    }

    pub fn log_opcode(&self, address: u16, opcode: &OpCode, arg1: Option<u8>, arg2: Option<u8>) {
        self.log_line(self.make_opcode_part1(address, opcode, arg1, arg2));
    }

    pub fn log_running_opcode(&self, cpu: &CPU, opcode: &OpCode, arg1: Option<u8>, arg2: Option<u8>) {
        let part1 = self.make_opcode_part1(cpu.program_counter, opcode, arg1, arg2);

        self.log_line(format!{"{} {}", part1, "todo"});
    }

    fn make_opcode_part1(&self, address: u16, opcode: &OpCode, arg1: Option<u8>, arg2: Option<u8>) -> String {
        let addr = self.wrap_color(format!("{:0>4X}", address), ANSIColor::Blue);
        let op_hex = format!("{:0>2X} {} {}",
            opcode.code,
            match arg1 { Some(arg) => format!("{:0>2X}", arg), None => "  ".to_string()},
            match arg2 { Some(arg) => format!("{:0>2X}", arg), None => "  ".to_string()}
        );
        let instr = opcode.instruction;
        let mut instr_arg: String = match (arg1, arg2) {
            (None, None) => format!("     "),
            (Some(arg1), None) => format!("${:0>2X}", arg1),
            (Some(arg1), Some(arg2)) => format!("${:0>4X}", ((arg2 as u16) << 8) | (arg1 as u16)),
            (None, Some(_)) => panic!("Logging:log_opcode -- arg2 is set but arg1 is None")
        };
        instr_arg = Self::format_args(instr_arg, &opcode.mode, opcode.instruction);

        return format!{"{}\t{}\t{} {}", addr, op_hex, instr, instr_arg};
    }

    fn log_line(&self, line: String) {
        if self.locations.terminal {
            println!("{}", line);
        }

        if self.locations.file {
            todo!();
        }
    }

    fn format_args(args: String, addressing_mode: &AddressingMode, _instruction: &str) -> String {
        match addressing_mode {
            AddressingMode::Immediate => format!("#{}", args),
            AddressingMode::ZeroPage => format!("{}", args),
            AddressingMode::ZeroPageX => format!("{},X", args),
            AddressingMode::ZeroPageY => format!("{},Y", args),
            AddressingMode::Absolute => format!("{}", args),
            AddressingMode::AbsoluteX => format!("{},X", args),
            AddressingMode::AbsoluteY => format!("{},Y", args),
            AddressingMode::IndirectX => format!("({},X)", args),
            AddressingMode::IndirectY => format!("({}),Y", args),
            AddressingMode::NoneAddressing => {
                format!("{}", args)
                // todo!("Handle some edge case instructions");
            },
        }
    }

}