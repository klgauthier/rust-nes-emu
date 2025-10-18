// Copyright 2025 Kevin Gauthier. All rights reserved.

use std::collections::HashMap;

use crate::compute::arguments::Arguments;
use crate::compute::cpu::AddressingMode;
use crate::compute::opcodes::{self, OpCode};
use crate::logging::{FileLog, Logger, TerminalLog};
use crate::rom_format::format_ines1::INes1;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

pub enum SupportedFileTypes {
    INes1(INes1),
}

pub struct Rom {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper: u8,
    pub screen_mirroring: Mirroring
}

impl Rom {
    pub fn new(raw: Vec<u8>) -> Result<Rom, String> {
        match INes1::is_correct_file_type(&raw) {
            true => {
                let imported_result = INes1::new(raw);

                let imported = imported_result?;

                Ok(Rom { 
                    prg_rom: imported.prg_rom_data, 
                    chr_rom: imported.chr_rom_data, 
                    mapper: imported.mapper_type, 
                    screen_mirroring: imported.mirroring_type 
                })
            }

            false => Err("File type is not a supported one.".to_string())
        }
    }

    pub fn build_rom(program: Vec<u8>) -> Rom {
        Rom {
            prg_rom: program.to_vec(), 
            chr_rom: Vec::new(), 
            mapper: 0x00, 
            screen_mirroring: Mirroring::Horizontal, 

        }
    }

    pub fn print_rom(&self) {
        let mut in_0_block = false;
        let mut first_0_in_block = true;

        let mut file_log = FileLog::new().expect("File failed to open.");
        let mut term_log = TerminalLog::new();
        
        let mut i: usize = 0;
        while i < self.prg_rom.len() {
            let instr = self.prg_rom[i];

            let opcode = self.get_opcode(instr);

            match (instr, in_0_block, first_0_in_block) {
                // mark 0 encountered
                (0x0, false, true) => {
                    first_0_in_block = false;

                    self.print_instr(&mut file_log, &opcode, i);
                    self.print_instr(&mut term_log, &opcode, i);
                }

                // start 0 block
                (0x0, false, false) => {
                    println!("...");
                    in_0_block = true;
                }

                // ignored 0
                (0x0, true, _) => {}

                // end 0 block
                (0x1..=0xFF, true, _) => {
                    in_0_block = false;
                    first_0_in_block = true;

                    self.print_instr(&mut file_log, &opcode, i);
                    self.print_instr(&mut term_log, &opcode, i);
                }

                (0x1..=0xFF, false, _) => {
                    first_0_in_block = true;

                    self.print_instr(&mut file_log, &opcode, i);
                    self.print_instr(&mut term_log, &opcode, i);
                }
            }

            i += opcode.len as usize;        
        }
    }

    fn print_instr<T: Logger>(&self, logger: &mut T, opcode: &OpCode, index: usize) {
        const ADDR_PRG_ROM : usize = 0x8000;

        let arg_len = (opcode.len-1) as usize;

        let arg1 = if arg_len >= 1 { Some(self.prg_rom[index+1])} else {None};
        let arg2 = if arg_len == 2 { Some(self.prg_rom[index+2])} else {None};

        let args = match (arg1, arg2) {
            (None, None) => Arguments::None,
            (None, Some(_)) => unreachable!(),
            (Some(arg), None) => Arguments::One(arg),
            (Some(arg1), Some(arg2)) => Arguments::Two(Arguments::combine_args(arg1, arg2)),
        };

        logger.log_opcode((ADDR_PRG_ROM+index) as u16, opcode, &args);
    }

    fn get_opcode(&self, instr: u8) -> OpCode {
        let opcodes: &HashMap<u8, &'static opcodes::OpCode> = &opcodes::OPCODES_MAP;

        let fetch_opcode = opcodes.get(&instr);

        match fetch_opcode {
            Some(oc) => OpCode::new(oc.code, oc.instruction, oc.len, oc.cycles,  oc.mode),
            None => OpCode::new(instr, "???", 1, 1, AddressingMode::NoneAddressing)
        }
    }
}