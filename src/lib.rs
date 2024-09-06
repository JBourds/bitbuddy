#![allow(warnings)]
#![no_std]
pub const BITS_IN_BYTE: usize = 8;
pub extern crate paste;
pub mod bitfield;
pub mod bitmap;
pub mod errors;
pub mod types;
mod math;

