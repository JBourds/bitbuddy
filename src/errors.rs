/// errors.rs
///
/// File containing the memory errors for use in Result objects.
///
/// Author: Jordan Bourdeau
/// Last Modified: 3/8/24
use thiserror_no_std::Error;

use super::bitmap::BitmapSegmentBounds;

#[derive(Error, Debug, PartialEq)]
pub enum BitfieldError {
    #[error("Value {} is too large to fit in {} bits.", value, num_bits)]
    ValueTooLarge { value: usize, num_bits: usize },
}

#[derive(Error, Debug, PartialEq)]
pub enum BitmapError {
    #[error("No bit index found")]
    NoBitIndex,
    #[error("Bit index {:?} already set", bit_index)]
    BitAlreadySet { bit_index: usize },
    #[error("Bit index {:?} out of bounds", bit_index)]
    BitIndexOutOfBounds { bit_index: usize },
    #[error("Expected bounds: {:?} but found bounds {:?} on bit segment", 
        expected.get_bounds(), provided.get_bounds())]
    BitRegionOutOfBounds {
        provided: BitmapSegmentBounds,
        expected: BitmapSegmentBounds,
    },
    #[error("Bit region already set: {:?} in bit segment", region.get_bounds())]
    BitRegionAlreadySet { region: BitmapSegmentBounds },
    #[error("Bitmap segment upper bound must be less than lower bound")]
    BitmapSegmentBounds,
    #[error("Bitmap segment is full")]
    BitmapSegmentFull,
    #[error("No free bit index")]
    NoFreeBitIndex,
    #[error("Iterator out of bounds")]
    IteratorOutOfBounds,
    #[error("Empty iterator")]
    EmptyIterator,
    #[error("Unknown error encountered")]
    UnknownError,
}

#[derive(Debug, Error, PartialEq)]
pub enum BoundsError {
    #[error("Index out of bounds.")]
    IndexOutOfBounds,
    #[error("Region out of bounds.")]
    RegionOutOfBounds,
    #[error("Zero-sized region")]
    ZeroSizedRegion,
}
