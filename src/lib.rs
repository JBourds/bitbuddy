#![allow(warnings)]
//#![no_std]
pub const BITS_IN_BYTE: usize = 8;
pub const BITS_IN_WORD: usize = core::mem::size_of::<usize>() * BITS_IN_BYTE;
pub extern crate paste;
pub mod bitfield;
pub mod bitmap;
pub mod errors;
pub mod types;
mod math;

