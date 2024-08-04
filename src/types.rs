/// memory/types.rs
///
/// File containing type definitions pertaining to memory.
///
/// Author: Jordan Bourdeau
/// Last Modified: 3/8/24

/// Enum representing the state of a region of a bitmap.
///     * `Full`:  All bits are set.
///     * `Empty`: No bits in the region are set.
///     * `Dirty`: Some bits in the region are set.
#[derive(Debug, PartialEq)]
pub enum RegionStatus {
    Full,
    Empty,
    Dirty,
}

/// Enum representing the state of a bit.
///     * `Set` = 1.
///     * `Free` = 0.
#[derive(Debug, PartialEq)]
pub enum BitStatus {
    Set,
    Free,
}

pub struct Byte;

impl Byte {
    pub const SIZE_BITS: usize = 8;
}

pub struct Kilobyte;

impl Kilobyte {
    pub const SIZE_BYTES: usize = 1024;
}

pub struct Megabyte;

impl Megabyte {
    pub const SIZE_BYTES: usize = 1024 * Kilobyte::SIZE_BYTES;
}

pub struct Gigabyte;

impl Gigabyte {
    pub const SIZE_BYTES: usize = 1024 * Megabyte::SIZE_BYTES;
}

pub struct Word;

impl Word {
    pub const SIZE_BYTES: usize = core::mem::size_of::<usize>();
    pub const SIZE_BITS: usize = Self::SIZE_BYTES * Byte::SIZE_BITS;
    pub const FULL: usize = usize::MAX;
    pub const EMPTY: usize = usize::MIN;
}
