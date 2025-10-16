// Copyright 2025 Kevin Gauthier. All rights reserved.

use crate::cartridge::Mirroring;

const INES1_ID: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
const INES1_VER_CODE: u8 = 0;

const HEADER_SIZE: usize = 16;
const TRAINER_SIZE: usize = 512;

pub struct INes1 {
    pub id: [u8; 4],
    pub prg_rom_bank_count: u8,
    pub prg_ram_bank_count: u8,
    pub chr_rom_bank_count: u8,
    pub mapper_type: u8,
    pub mirroring_type: Mirroring,
    pub has_battery_backed_ram: bool,
    pub trainer_data: Option<Vec<u8>>,
    pub prg_rom_data: Vec<u8>,
    pub chr_rom_data: Vec<u8>,
}

impl INes1 {
    pub fn new(raw: Vec<u8>) -> Result<INes1, String> {
        let header = &raw[0..=16];
    
        let id = &header[0..=3];
        let prg_rom_bank_count = &header[4];
        let chr_rom_bank_count = &header[5];
        let control_byte_1 = &header[6];
        let control_byte_2 = &header[7];
        let prg_ram_bank_count = &header[8];

        if id != INES1_ID {
            return Err(format!("File is not in the iNES file format. Got file id: {:x} {:x} {:x} {:x}", id[0], id[1], id[2], id[3]).to_string());
        }

        let version = control_byte_2 & 0b00001111;

        if version != INES1_VER_CODE {
            return Err(format!("Only iNes1 file format is supported. Got file version code: {:x}", version).to_string());
        }

        let mapper_type = (control_byte_1 >> 4) | (control_byte_2 & 0b1111_0000);
        
        let vert_mirror = (control_byte_1 & 0b1) != 0;
        let four_screen_mirror = (control_byte_1 & 0b0000_1000) != 0;
        let mirroring_type = match (vert_mirror, four_screen_mirror) {
            (false, false) => Mirroring::Horizontal,
            (true, false) => Mirroring::Vertical,
            (_, true) => Mirroring::FourScreen,
        };

        let has_battery_backed_ram = (control_byte_1 & 0b0000_0010) != 0;

        let has_trainer_data = (control_byte_1 & 0b0000_0100) != 0;
        let trainer_data = match has_trainer_data {
            false => None,
            true => {
                Some(raw[HEADER_SIZE..(TRAINER_SIZE+TRAINER_SIZE)].to_vec())
            }
        };

        let prg_rom_start = HEADER_SIZE + if has_trainer_data { TRAINER_SIZE } else { 0 };
        let chr_rom_start = prg_rom_start + Self::get_prg_rom_size(*prg_rom_bank_count);
        let chr_rom_end = chr_rom_start + Self::get_chr_rom_size(*chr_rom_bank_count);

        Ok(INes1 {
            id: [id[0], id[1], id[2], id[3]],
            prg_rom_bank_count: *prg_rom_bank_count,
            prg_ram_bank_count: *prg_ram_bank_count,
            chr_rom_bank_count: *chr_rom_bank_count,
            mapper_type,
            mirroring_type,
            has_battery_backed_ram,
            trainer_data,
            prg_rom_data: raw[prg_rom_start..chr_rom_start].to_vec(),
            chr_rom_data: raw[chr_rom_start..chr_rom_end].to_vec(),
        })
    }

    pub fn get_prg_rom_size(prg_rom_bank_count: u8) -> usize {
        (prg_rom_bank_count as usize) * 16 * 1024
    }

    pub fn get_chr_rom_size(chr_rom_bank_count: u8) -> usize {
        (chr_rom_bank_count as usize) * 8 * 1024
    }

    pub fn get_prg_ram_size(prg_ram_bank_count: u8) -> usize {
        (prg_ram_bank_count as usize) * 8 * 1024
    }

    pub fn is_correct_file_type(raw: &[u8]) -> bool {
        let header = &raw[0..=16];
    
        let id = &header[0..=3];
        let control_byte_2 = &header[7];

        if id != INES1_ID {
            println!("Failed to load due to not matching id. {} {}",
            format_args!("\n\tGot:      {:x} {:x} {:x} {:x} ", id[0], id[1], id[2], id[3]),
            format_args!("\n\tExpected: {:x} {:x} {:x} {:x} ", INES1_ID[0], INES1_ID[1], INES1_ID[2], INES1_ID[3]));
            return false;
        }

        let version = control_byte_2 & 0b00001111;

        if version != INES1_VER_CODE {
            println!("Failed to incorrect version data. {} {}",
            format_args!("\n\tGot:      {:x}", version),
            format_args!("\n\tExpected: {:x}", INES1_VER_CODE));
            return false;
        }

        true
    }
}