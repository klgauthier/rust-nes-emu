// Copyright 2025 Kevin Gauthier. All rights reserved.

pub mod bus;
pub mod cartridge;
pub mod cpu;
pub mod logging;
pub mod opcodes;
pub mod rom_format;

use bus::Memory;
use cpu::CPU;
use rand::Rng;
use sdl2::event::Event;
use sdl2::EventPump;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;
use std::fs;
use std::path::Path;
use std::time::Duration;

use crate::cartridge::Rom;

#[macro_use]
extern crate lazy_static;

const SCALE: f32 = 10f32;
const RESOLUTION: u32 = 32*(SCALE as u32);

fn main() {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
    .window("Snake game", RESOLUTION, RESOLUTION)
    .position_centered()
    .build().unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();
    canvas.set_scale(SCALE, SCALE).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator.create_texture_target(PixelFormatEnum::RGB24, 32, 32).unwrap();

    let mut cpu = CPU::new();
    let rom_file_data = load_binary_from_file(Path::new("roms/snake.nes"));
    let rom = Rom::new(rom_file_data);
    cpu.load_and_run(rom.expect("Rom failed to build."));

    let mut screen_state = [0 as u8; 32*3*32];
    let mut rng = rand::thread_rng();

    cpu.run_with_callback(move |cpu| {


        handle_user_input(cpu, &mut event_pump);
        cpu.mem_write(0xFE, rng.gen_range(1, 16));
        
        if read_screen_state(cpu, &mut screen_state) {
            texture.update(None, &screen_state, 32*3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();

            let timeout = Duration::from_millis(32);
            std::thread::park_timeout(timeout);
        }

    });
}

fn load_binary_from_file(path: &Path) -> Vec<u8> {
    let result = fs::read(path);
    return match result {
        Err(e) => panic!("{}", e),
        Ok(data) => data,
    }
}

fn handle_user_input(cpu: &mut CPU, event_pump: &mut EventPump) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. } | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                std::process::exit(0);
            }

            Event::KeyDown { keycode: Some(Keycode::W), .. } => {
                cpu.mem_write(0xFF, 0x77);
            }
            Event::KeyDown { keycode: Some(Keycode::S), .. } => {
                cpu.mem_write(0xFF, 0x73);
            }
            Event::KeyDown { keycode: Some(Keycode::A), .. } => {
                cpu.mem_write(0xFF, 0x61);
            }
            Event::KeyDown { keycode: Some(Keycode::D), .. } => {
                cpu.mem_write(0xFF, 0x64);
            }

            _ => { }
        }
    }
}

fn color(byte: u8) -> Color {
    match byte {
        0 => sdl2::pixels::Color::BLACK,
       1 => sdl2::pixels::Color::WHITE,
       2 | 9 => sdl2::pixels::Color::GREY,
       3 | 10 => sdl2::pixels::Color::RED,
       4 | 11 => sdl2::pixels::Color::GREEN,
       5 | 12 => sdl2::pixels::Color::BLUE,
       6 | 13 => sdl2::pixels::Color::MAGENTA,
       7 | 14 => sdl2::pixels::Color::YELLOW,
       _ => sdl2::pixels::Color::CYAN,
    }
}

fn read_screen_state(cpu: &CPU, frame: &mut [u8; 32*3*32]) -> bool {
    let mut frame_idx = 0;
    let mut update = false;
    for i in 0x0200..0x0600 {
        let color_idx = cpu.mem_read(i as u16);
        let (b1, b2, b3) = color(color_idx).rgb();
        if frame[frame_idx] != b1 || frame[frame_idx+1] != b2 || frame[frame_idx+2] != b3 {
            frame[frame_idx] = b1;
            frame[frame_idx+1] = b2;
            frame[frame_idx+2] = b3;
            update = true;
        }
        frame_idx += 3;
    }
    return update;
}