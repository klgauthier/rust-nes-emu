// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::compute::cpu::AddressingMode;
use std::collections::HashMap;

#[derive(Eq, PartialEq, Hash)]
pub struct OpCode
{
    pub code: u8,
    pub instruction: &'static str,
    pub len: u8,
    pub cycles: u8,
    pub mode: AddressingMode,
}

impl OpCode {
    pub fn new(code: u8, instruction: &'static str, len: u8, cycles: u8, mode: AddressingMode) -> Self
    {
        OpCode {
            code,
            instruction,
            len,
            cycles,
            mode,
        }
    }
}

lazy_static!
{
    pub static ref CPU_OP_CODES: Vec<OpCode> = vec![
        // Table Source: https://www.nesdev.org/obelisk-6502-guide/reference.html

        /* ADC - Add with Carry */
        OpCode::new(0x69, "ADC", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x65, "ADC", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x75, "ADC", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0x6D, "ADC", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x7D, "ADC", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0x79, "ADC", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY),
        OpCode::new(0x61, "ADC", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0x71, "ADC", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY),

        /* AND - Logical AND */
        OpCode::new(0x29, "AND", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x25, "AND", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x35, "AND", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0x2D, "AND", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x3D, "AND", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0x39, "AND", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY),
        OpCode::new(0x21, "AND", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0x31, "AND", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY),

        /* ASL - Arithmetic Shift Left */
        OpCode::new(0x0A, "ASL", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x06, "ASL", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x16, "ASL", 2, 6, AddressingMode::ZeroPageX),
        OpCode::new(0x0E, "ASL", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x1E, "ASL", 3, 7, AddressingMode::AbsoluteX),

        /* BCC - Branch if Carry Clear */
        OpCode::new(0x90, "BCC", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, AddressingMode::NoneAddressing),

        /* BCS - Branch if Carry Set */
        OpCode::new(0xB0, "BCS", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, AddressingMode::NoneAddressing),

        /* BEQ - Branch if Equal */
        OpCode::new(0xF0, "BEQ", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, AddressingMode::NoneAddressing), 

        /* BIT - Bit Test */
        OpCode::new(0x24, "BIT", 2, 3, AddressingMode::ZeroPage), 
        OpCode::new(0x2C, "BIT", 3, 4, AddressingMode::Absolute), 

        /* BMI - Branch if Minus */
        OpCode::new(0x30, "BMI", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, AddressingMode::NoneAddressing), 

        /* BNE - Branch if Not Equal */
        OpCode::new(0xD0, "BNE", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, AddressingMode::NoneAddressing), 

        /* BPL - Branch if Positive */
        OpCode::new(0x10, "BPL", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, AddressingMode::NoneAddressing), 

        /* BRK - Force Interrupt */
        OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing),

        /* BVC - Branch if Overflow Clear */
        OpCode::new(0x50, "BVC", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, AddressingMode::NoneAddressing), 

        /* BVS - Branch if Overflow Set */
        OpCode::new(0x70, "BVS", 2, 2 /* +1 if branch succeeds, +2 if to a new page */, AddressingMode::NoneAddressing), 

        /* CLC - Clear Carry Flag */
        OpCode::new(0x18, "CLC", 1, 2, AddressingMode::NoneAddressing), 

        /* CDM - Clear Decimal Mode */
        OpCode::new(0xD8, "CLD", 1, 2, AddressingMode::NoneAddressing), 

        /* CLI - Clear Decimal Mode */
        OpCode::new(0x58, "CLI", 1, 2, AddressingMode::NoneAddressing), 

        /* CLV - Clear Overflow Flag */
        OpCode::new(0xB8, "CLV", 1, 2, AddressingMode::NoneAddressing), 

        /* CMP - Compare */
        OpCode::new(0xC9, "CMP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xC5, "CMP", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xD5, "CMP", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0xCD, "CMP", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xDD, "CMP", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0xD9, "CMP", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY),
        OpCode::new(0xC1, "CMP", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0xD1, "CMP", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY),

        /* CPX - Compare X Register */
        OpCode::new(0xE0, "CPX", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xE4, "CPX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xEC, "CPX", 3, 4, AddressingMode::Absolute),

        /* CPY - Compare Y Register */
        OpCode::new(0xC0, "CPY", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xC4, "CPY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xCC, "CPY", 3, 4, AddressingMode::Absolute),

        /* DEC - Decrement Memory */
        OpCode::new(0xC6, "DEC", 2, 2, AddressingMode::ZeroPage),
        OpCode::new(0xD6, "DEC", 2, 3, AddressingMode::ZeroPageX),
        OpCode::new(0xCE, "DEC", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xDE, "DEC", 3, 4, AddressingMode::AbsoluteX),

        /* DEX - Decrement X Register */
        OpCode::new(0xCA, "DEX", 1, 2, AddressingMode::NoneAddressing),
        
        /* DEY - Decrement Y Register */
        OpCode::new(0x88, "DEY", 1, 2, AddressingMode::NoneAddressing),
        
        /* EOR - Exclusive OR */
        OpCode::new(0x49, "EOR", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x45, "EOR", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x55, "EOR", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0x4D, "EOR", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x5D, "EOR", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0x59, "EOR", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY),
        OpCode::new(0x41, "EOR", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0x51, "EOR", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY),

        /* INC - Increment Memory */
        OpCode::new(0xE6, "INC", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0xF6, "INC", 2, 6, AddressingMode::ZeroPageX),
        OpCode::new(0xEE, "INC", 3, 6, AddressingMode::Absolute),
        OpCode::new(0xFE, "INC", 3, 7, AddressingMode::AbsoluteX),

        /* INX - Increment X Register */
        OpCode::new(0xE8, "INX", 1, 2, AddressingMode::NoneAddressing),
        
        /* INY - Increment Y Register */
        OpCode::new(0xC8, "INY", 1, 2, AddressingMode::NoneAddressing),
        
        /* JMP - Jump */
        OpCode::new(0x4C, "JMP", 3, 3, AddressingMode::Absolute),
        OpCode::new(0x6C, "JMP", 3, 5, AddressingMode::NoneAddressing /* Indirect addressing requires 6502 bug emulation */), 
        
        /* JSR - Jump to Subroutine */
        OpCode::new(0x20, "JSR", 3, 6, AddressingMode::Absolute),
        
        /* LDA - Load Accumulator */
        OpCode::new(0xA9, "LDA", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xA5, "LDA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xB5, "LDA", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0xAD, "LDA", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xBD, "LDA", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0xB9, "LDA", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY),
        OpCode::new(0xA1, "LDA", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0xB1, "LDA", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY),

        /* LDX - Load X Register */
        OpCode::new(0xA2, "LDX", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xA6, "LDX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xB6, "LDX", 2, 4, AddressingMode::ZeroPageY),
        OpCode::new(0xAE, "LDX", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xBE, "LDX", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY),

        /* LDY - Load Y Register */
        OpCode::new(0xA0, "LDY", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xA4, "LDY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xB4, "LDY", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0xAC, "LDY", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xBC, "LDY", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),

        /* LSR - Logical Shift Right */
        OpCode::new(0x4A, "LSR", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x46, "LSR", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x56, "LSR", 2, 6, AddressingMode::ZeroPageX),
        OpCode::new(0x4E, "LSR", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x5E, "LSR", 3, 7, AddressingMode::AbsoluteX),

        /* NOP - No Operation */
        OpCode::new(0xEA, "NOP", 1, 2, AddressingMode::NoneAddressing),

        /* ORA - Logical Inclusive OR */
        OpCode::new(0x09, "ORA", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x05, "ORA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x15, "ORA", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0x0D, "ORA", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x1D, "ORA", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0x19, "ORA", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY),
        OpCode::new(0x01, "ORA", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0x11, "ORA", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY),

        /* PHA - Push Accumulator */
        OpCode::new(0x48, "PHA", 1, 3, AddressingMode::NoneAddressing),

        /* PHP - Push Processor Status */
        OpCode::new(0x08, "PHP", 1, 3, AddressingMode::NoneAddressing),
        
        /* PLA - Pull Accumulator */
        OpCode::new(0x68, "PLA", 1, 4, AddressingMode::NoneAddressing),

        /* PLP - Pull Processor Status */
        OpCode::new(0x28, "PLP", 1, 4, AddressingMode::NoneAddressing),

        /* ROL - Rotate Left */
        OpCode::new(0x2A, "ROL", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x26, "ROL", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x36, "ROL", 2, 6, AddressingMode::ZeroPageX),
        OpCode::new(0x2E, "ROL", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x3E, "ROL", 3, 7, AddressingMode::AbsoluteX),

        /* ROR - Rotate Right */
        OpCode::new(0x6A, "ROR", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x66, "ROR", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x76, "ROR", 2, 6, AddressingMode::ZeroPageX),
        OpCode::new(0x6E, "ROR", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x7E, "ROR", 3, 7, AddressingMode::AbsoluteX),

        /* RTI - Return from Interrupt */
        OpCode::new(0x40, "RTI", 1, 6, AddressingMode::NoneAddressing),

        /* RTS - Return from Subroutine */
        OpCode::new(0x60, "RTS", 1, 6, AddressingMode::NoneAddressing),

        /* SBC - Subtract with Carry */
        OpCode::new(0xE9, "SBC", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xE5, "SBC", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xF5, "SBC", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0xED, "SBC", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xFD, "SBC", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0xF9, "SBC", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY),
        OpCode::new(0xE1, "SBC", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0xF1, "SBC", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY),

        /* SEC - Set Carry Flag */
        OpCode::new(0x38, "SEC", 1, 2, AddressingMode::NoneAddressing),

        /* SED - Set Decimal Flag */
        OpCode::new(0xF8, "SED", 1, 2, AddressingMode::NoneAddressing),

        /* SEI - Set Interrupt Disable */
        OpCode::new(0x78, "SEI", 1, 2, AddressingMode::NoneAddressing),

        /* STA - Store Accumulator */
        OpCode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0x8D, "STA", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x9D, "STA", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0x99, "STA", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY),
        OpCode::new(0x81, "STA", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0x91, "STA", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY),

         /* STX - Store X Register */
        OpCode::new(0x86, "STX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x96, "STX", 2, 4, AddressingMode::ZeroPageY),
        OpCode::new(0x8E, "STX", 3, 4, AddressingMode::Absolute),

         /* STY - Store Y Register */
        OpCode::new(0x84, "STY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x94, "STY", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0x8C, "STY", 3, 4, AddressingMode::Absolute),

        /* TAX - Transfer Accumulator to X */
        OpCode::new(0xAA, "TAX", 1, 2, AddressingMode::NoneAddressing),

        /* TAY - Transfer Accumulator to Y */
        OpCode::new(0xA8, "TAY", 1, 2, AddressingMode::NoneAddressing),

        /* TSX - Transfer Stack Pointer to X */
        OpCode::new(0xBA, "TSX", 1, 2, AddressingMode::NoneAddressing),

        /* TXA - Transfer X to Accumulator */
        OpCode::new(0x8A, "TXA", 1, 2, AddressingMode::NoneAddressing),

        /* TXS - Transfer X to Stack Pointer */
        OpCode::new(0x9A, "TXS", 1, 2, AddressingMode::NoneAddressing),

        /* TYA - Transfer Y to Accumulator */
        OpCode::new(0x98, "TYA", 1, 2, AddressingMode::NoneAddressing),

        /* ----------------------- */ 
        /* ----ILLEGAL OPCODES---- */
        /* ----------------------- */

        /* AAC - AND with Accumulator */
        OpCode::new(0x0B, "AAC", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x2B, "AAC", 2, 2, AddressingMode::Immediate),

        /* AAX - AND X with Accumulator */
        OpCode::new(0x87, "AAX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x97, "AAX", 2, 4, AddressingMode::ZeroPageY),
        OpCode::new(0x83, "AAX", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0x8F, "AAX", 3, 4, AddressingMode::Absolute),

        /* ARR - AND with Accumulator, then rotate right */
        OpCode::new(0x6B, "ARR", 2, 2, AddressingMode::Immediate),

        /* ASR - AND with Accumulator, then shift right */
        OpCode::new(0x4B, "ASR", 2, 2, AddressingMode::Immediate),

        /* ATX - AND with Accumulator, then transfer to X */
        OpCode::new(0xAB, "ATX", 2, 2, AddressingMode::Immediate),

        /* AXA - AND X Register with Accumulator, then AND with 7 */ 
        OpCode::new(0x9F, "AXA", 3, 5, AddressingMode::AbsoluteY),
        OpCode::new(0x93, "AXA", 2, 6, AddressingMode::IndirectY),

        /* AXS - AND X Register with Accumulator, then subtract */ 
        OpCode::new(0xCB, "AXS", 2, 2, AddressingMode::Immediate),

        /* DCP - Subtract 1 from memory */ 
        OpCode::new(0xC7, "DCP", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0xD7, "DCP", 2, 6, AddressingMode::ZeroPageX),
        OpCode::new(0xCF, "DCP", 3, 6, AddressingMode::Absolute),
        OpCode::new(0xDF, "DCP", 3, 7, AddressingMode::AbsoluteX),
        OpCode::new(0xDB, "DCP", 3, 7, AddressingMode::AbsoluteY),
        OpCode::new(0xC3, "DCP", 2, 8, AddressingMode::IndirectX),
        OpCode::new(0xD3, "DCP", 2, 8, AddressingMode::IndirectY),

        /* DOP - Double NOP */ 
        OpCode::new(0x04, "DOP", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x14, "DOP", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0x34, "DOP", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0x44, "DOP", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x54, "DOP", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0x64, "DOP", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x74, "DOP", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0x80, "DOP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x82, "DOP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x89, "DOP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xC2, "DOP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xD4, "DOP", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0xE2, "DOP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xF4, "DOP", 2, 4, AddressingMode::ZeroPageX),

        /* ISC - Increment Memory, then subtract */ 
        OpCode::new(0xE7, "ISC", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0xF7, "ISC", 2, 6, AddressingMode::ZeroPageX),
        OpCode::new(0xEF, "ISC", 3, 6, AddressingMode::Absolute),
        OpCode::new(0xFF, "ISC", 3, 7, AddressingMode::AbsoluteX),
        OpCode::new(0xFB, "ISC", 3, 7, AddressingMode::AbsoluteY),
        OpCode::new(0xE3, "ISC", 2, 8, AddressingMode::IndirectX),
        OpCode::new(0xF3, "ISC", 2, 8, AddressingMode::IndirectY),

        /* KIL - Stop program counter */ 
        OpCode::new(0x02, "KIL", 1, 0, AddressingMode::NoneAddressing),
        OpCode::new(0x12, "KIL", 1, 0, AddressingMode::NoneAddressing),
        OpCode::new(0x22, "KIL", 1, 0, AddressingMode::NoneAddressing),
        OpCode::new(0x32, "KIL", 1, 0, AddressingMode::NoneAddressing),
        OpCode::new(0x42, "KIL", 1, 0, AddressingMode::NoneAddressing),
        OpCode::new(0x52, "KIL", 1, 0, AddressingMode::NoneAddressing),
        OpCode::new(0x62, "KIL", 1, 0, AddressingMode::NoneAddressing),
        OpCode::new(0x72, "KIL", 1, 0, AddressingMode::NoneAddressing),
        OpCode::new(0x92, "KIL", 1, 0, AddressingMode::NoneAddressing),
        OpCode::new(0xB2, "KIL", 1, 0, AddressingMode::NoneAddressing),
        OpCode::new(0xD2, "KIL", 1, 0, AddressingMode::NoneAddressing),
        OpCode::new(0xF2, "KIL", 1, 0, AddressingMode::NoneAddressing),

        /* LAR - AND Memory and Stack Pointer, transfer to A, X, Stack Pointer */ 
        OpCode::new(0xBB, "LAR", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY),

        /* LAX - Load Acc and X Register */ 
        OpCode::new(0xA7, "LAX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xB7, "LAX", 2, 4, AddressingMode::ZeroPageY),
        OpCode::new(0xAF, "LAX", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xBF, "LAX", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteY),
        OpCode::new(0xA3, "LAX", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0xB3, "LAX", 2, 5 /* +1 if page crossed */, AddressingMode::IndirectY),

        /* NOP - No operation */ 
        OpCode::new(0x1A, "NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x3A, "NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x5A, "NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x7A, "NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xDA, "NOP", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xFA, "NOP", 1, 2, AddressingMode::NoneAddressing),

        /* RLA - Rotate left, then add acc */ 
        OpCode::new(0x27, "RLA", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x37, "RLA", 2, 6, AddressingMode::ZeroPageX),
        OpCode::new(0x2F, "RLA", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x3F, "RLA", 3, 7, AddressingMode::AbsoluteX),
        OpCode::new(0x3B, "RLA", 3, 7, AddressingMode::AbsoluteY),
        OpCode::new(0x23, "RLA", 2, 8, AddressingMode::IndirectX),
        OpCode::new(0x33, "RLA", 2, 8, AddressingMode::IndirectY),

        /* RRA - Rotate right, then add acc */ 
        OpCode::new(0x67, "RRA", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x77, "RRA", 2, 6, AddressingMode::ZeroPageX),
        OpCode::new(0x6F, "RRA", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x7F, "RRA", 3, 7, AddressingMode::AbsoluteX),
        OpCode::new(0x7B, "RRA", 3, 7, AddressingMode::AbsoluteY),
        OpCode::new(0x63, "RRA", 2, 8, AddressingMode::IndirectX),
        OpCode::new(0x73, "RRA", 2, 8, AddressingMode::IndirectY),

        /* SBC - Subtract with Carry */ 
        OpCode::new(0xEB, "SBC", 2, 2, AddressingMode::Immediate),

        /* SLO - Shift memory left, then OR accumulator with memory */ 
        OpCode::new(0x07, "SLO", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x17, "SLO", 2, 6, AddressingMode::ZeroPageX),
        OpCode::new(0x0F, "SLO", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x1F, "SLO", 3, 7, AddressingMode::AbsoluteX),
        OpCode::new(0x1B, "SLO", 3, 7, AddressingMode::AbsoluteY),
        OpCode::new(0x03, "SLO", 2, 8, AddressingMode::IndirectX),
        OpCode::new(0x13, "SLO", 2, 8, AddressingMode::IndirectY),

        /* SRE - Shift Right, then EOR */ 
        OpCode::new(0x47, "SRE", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x57, "SRE", 2, 6, AddressingMode::ZeroPageX),
        OpCode::new(0x4F, "SRE", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x5F, "SRE", 3, 7, AddressingMode::AbsoluteX),
        OpCode::new(0x5B, "SRE", 3, 7, AddressingMode::AbsoluteY),
        OpCode::new(0x43, "SRE", 2, 8, AddressingMode::IndirectX),
        OpCode::new(0x53, "SRE", 2, 8, AddressingMode::IndirectY),

        /* SXA - AND X with high byte */ 
        OpCode::new(0x9E, "SXA", 3, 5, AddressingMode::AbsoluteY),

        /* SYA - AND Y with high byte */
        OpCode::new(0x9C, "SYA", 3, 5, AddressingMode::AbsoluteX),

        /* TOP - Triple NOP */ 
        OpCode::new(0x0C, "TOP", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x1C, "TOP", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0x3C, "TOP", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0x5C, "TOP", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0x7C, "TOP", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0xDC, "TOP", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0xFC, "TOP", 3, 4 /* +1 if page crossed */, AddressingMode::AbsoluteX),

        /* XAA - Unstable command, we put 0xFF into Acc */ 
        OpCode::new(0x8B, "XAA", 2, 2, AddressingMode::Immediate),

        /* XAS - AND X with Acc to Stack Pointer, AND Stack Pointer and high byte to Memory */ 
        OpCode::new(0x9B, "XAS", 3, 5, AddressingMode::AbsoluteY),
        
    ];

    pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
        let mut map = HashMap::new();
        for op_code in &*CPU_OP_CODES {
            map.insert(op_code.code, op_code);
        }
        return map;
    };
}
