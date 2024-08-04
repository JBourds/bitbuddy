/// bitfield.rs
///
/// Module containing struct definitions for the Bitmap interface.
///
/// Author: Jordan Bourdeau
/// Last Modified: 3/8/24
use super::errors::BitmapError;
use super::math::ceiling;
use super::types::{BitStatus, Byte, RegionStatus, Word};

/// Struct representing the inclusive lower/upper relative bounds on accessible bits in a bitmap segment.
/// # Fields:
/// * lower: Lower inclusive bound for bit index.
/// * upper: Upper inclusive bound for bit index.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BitmapSegmentBounds {
    lower: usize,
    upper: usize,
}

impl BitmapSegmentBounds {
    /// Constructor for segment bounds. Expects lower to be < upper.
    /// # Arguments:
    /// * lower: Lower inclusive index for the range.
    /// * upper: Upper inclusive index for the range.
    /// Returns a Result type with the struct if arguments are valid, and an error otherwise.
    const fn new(lower: usize, upper: usize) -> Result<Self, BitmapError> {
        if upper < lower {
            Err(BitmapError::BitmapSegmentBounds)
        } else {
            Ok(Self { lower, upper })
        }
    }

    /// Function to return the tuple with the bounds.
    pub const fn get_bounds(&self) -> (usize, usize) {
        (self.lower, self.upper)
    }

    /// Function to signal whether a bit index is within the bounds.
    /// # Arguments:
    /// * bit_index: The bit index being checked.
    /// Returns true if the bit is in bounds, false otherwise.
    const fn includes_value(&self, bit_index: usize) -> bool {
        bit_index >= self.lower && bit_index <= self.upper
    }

    /// Function to signal whether a bit index range is within the bounds.
    /// # Arguments:
    /// * start_index: Starting index of the range.
    /// * end_index:   Ending index of the range.
    /// Returns true if the whole range is valid, false otherwise (or with ill-formed ranges where lower > upper).
    const fn includes_range(&self, start_index: usize, end_index: usize) -> bool {
        if start_index > end_index {
            return false;
        }

        start_index >= self.lower && end_index <= self.upper
    }

    const fn default() -> Self {
        Self {
            lower: 0,
            upper: Word::SIZE_BITS - 1,
        }
    }
}

/// Helper struct used to store a pointer and the index in the pointer's usize for a bitmap region.
/// Also contantins a suite of methods for performing bitwise operations on specified chunk(s) of the bitmap.
/// # Fields:
/// * pointer:   Pointer to the specific bitmap segment (broken up on words boundaries).
/// * bit_index: Optional bit index for set/get/clear operations.
/// * bounds:    The inclusive, relative index bounds for the available region in a segment.
#[derive(Debug)]
pub struct BitmapSegment {
    pointer: *mut usize,
    bit_index: Option<usize>,
    bounds: BitmapSegmentBounds,
}

impl BitmapSegment {
    /// Constructor for a bitmap segment.
    /// WARNING: bitfield_base and bitfield_length MUST be correct or you will clobber memory!
    /// # Arguments:
    /// * bitfield_base:    A pointer to the base of the bitfield region.
    /// * bit_index:        The bit index offset from the base to construct the segment from.
    /// * bitfield_length:  The length of the bitfield in bits.
    /// Returns a Result with a BitmapSegment if the bit is within bounds and an error if not.
    pub const unsafe fn new(
        bitfield_base: *mut usize,
        bit_index: usize,
        bitfield_length: usize,
    ) -> Result<Self, BitmapError> {
        if bit_index >= bitfield_length {
            return Err(BitmapError::BitIndexOutOfBounds { bit_index });
        }
        let word_index = bit_index / Word::SIZE_BITS;
        let pointer = unsafe { bitfield_base.offset(word_index as isize) };
        let bit_index = bit_index % Word::SIZE_BITS;

        Ok(Self {
            pointer,
            bit_index: Some(bit_index),
            bounds: BitmapSegmentBounds::default(),
        })
    }

    /// Method for setting the segment's inclusive bounds.
    /// Used to limit any operations which might act on bits beyond a specified region.
    /// Requires the upper bound is < number of bits in a word and lower bound < upper bound.
    /// # Arguments:
    /// * bounds: The segment bounds to enforce when conducting operations.
    /// Returns a boolean for whether the operation succeeded.
    fn set_bounds(&mut self, bounds: BitmapSegmentBounds) -> bool {
        if bounds.lower <= bounds.upper && bounds.upper < Word::SIZE_BITS {
            self.bounds = bounds;
            true
        } else {
            false
        }
    }

    // ----------------- Methods for setting bits/blocks as used -----------------

    /// Method to set the referenced bit as being used (1).
    /// Returns a Result type with true if it succeeds, and an error if there is not bit index or the region is already set.
    pub fn set_bit(&mut self) -> Result<bool, BitmapError> {
        if let Some(bit_index) = self.bit_index {
            self.set_specified_bit(bit_index)
        } else {
            Err(BitmapError::NoBitIndex)
        }
    }

    /// Method to set a specific as being used (1).
    /// Returns a Result type with true if it succeeds, and an error if there is not bit index or the region is already set.
    pub fn set_specified_bit(&mut self, bit_index: usize) -> Result<bool, BitmapError> {
        // If the bit is already set or the index is out of bounds return an error
        if let Ok(BitStatus::Set) = self.get_specified_bit(bit_index) {
            return Err(BitmapError::BitAlreadySet { bit_index });
        } else if !self.bounds.includes_value(bit_index) {
            return Err(BitmapError::BitIndexOutOfBounds { bit_index });
        }

        let mask: usize = 1 << bit_index;
        unsafe { *self.pointer |= mask };
        Ok(true)
    }

    /// Method to set a region of bits as being used (1).
    /// Does not require the region to be clear.
    /// # Arguments:
    /// * start_index: Starting index for the bit being set.
    /// * length:      Number of bits to set as being used.
    /// Returns a Result type with true if it succeeds or length == 0, and an error if the bit region is out of bounds.
    fn set_bits(&mut self, start_index: usize, length: usize) -> Result<bool, BitmapError> {
        // Trivial case, do nothing
        if length == 0 {
            return Ok(true);
        }

        let end_index = start_index + length - 1;

        if self.bounds.includes_range(start_index, end_index) {
            // How many bits need to be set to 0 on the left/right hand sides of the region
            let mask_start = start_index;
            let mask_end = Word::SIZE_BITS - end_index - 1;

            // Shift the region to the right to get the end boundary
            // Ex. Shift right 4 (end index is 27 on 32-bit)
            // 1111 1111 1111 1111 1111 1111 1111 1111 >> (4) = 0000 1111 1111 1111 1111 1111 1111 1111
            let mut mask = Word::FULL >> mask_end;

            // AND with a region shifted to the left to get the start boundary
            // Ex. Shift left 4 (end index is 27 on 32-bit)
            // 1111 1111 1111 1111 1111 1111 1111 1111 << (4) = 1111 1111 1111 1111 1111 1111 1111 0000
            //
            // 1111 1111 1111 1111 1111 1111 1111 0000 &
            // 0000 1111 1111 1111 1111 1111 1111 1111
            // ---------------------------------------
            // 0000 1111 1111 1111 1111 1111 1111 0000
            mask &= Word::FULL << mask_start;

            unsafe {
                // OR the bitmap with the region of all 1s to set it
                *self.pointer |= mask;
                Ok(true)
            }
        } else {
            let provided = BitmapSegmentBounds::new(start_index, start_index + length - 1)?;
            let expected = self.bounds;
            Err(BitmapError::BitRegionOutOfBounds { provided, expected })
        }
    }

    /// Method to set all bits in the BitmapSegment as being used within the specified region.
    /// Requires that all bits are currently unused, and all bits are available in the region (no bounds set) or that allow_overwrite is true.
    /// # Arguments:
    /// * allow_overwrite: Boolean flag for whether to allow setting a section which isn't completely empty.
    /// Returns a Result type with true if the region is successfully set, and an error otherwise.
    pub fn set_region_bits(&mut self, allow_overwrite: bool) -> Result<bool, BitmapError> {
        // Length is the difference between start and end, +1 for if they are the same.
        let length = self.bounds.upper - self.bounds.lower + 1;
        if allow_overwrite || self.get_region_status(self.bounds) == RegionStatus::Empty {
            self.set_bits(self.bounds.lower, length)
        } else {
            Err(BitmapError::BitRegionAlreadySet {
                region: self.bounds,
            })
        }
    }

    // ----------------- Methods for setting bits/blocks as free -----------------

    /// Method to unset the bit referenced by the bitfield segment as free (0).
    /// Returns a Result type with true if it succeeds, and false if the bit_index is None.
    pub fn clear_bit(&mut self) -> Result<bool, BitmapError> {
        if let Some(bit_index) = self.bit_index {
            self.clear_specified_bit(bit_index)
        } else {
            Err(BitmapError::NoBitIndex)
        }
    }

    /// Method to unset a specified bit in the segment as free (0).
    /// # Arguments:
    /// * bit_index: The bit index to clear.
    /// Returns a Result type with true if it succeeds, and false if the bit_index is None.
    pub fn clear_specified_bit(&mut self, bit_index: usize) -> Result<bool, BitmapError> {
        if self.bounds.includes_value(bit_index) {
            let mask: usize = 1 << bit_index;
            unsafe { *self.pointer &= !mask };
            Ok(true)
        } else {
            Err(BitmapError::BitIndexOutOfBounds { bit_index })
        }
    }

    /// Method to set a region of bits as being free (0).
    /// MUST make sure it is okay for the region to be cleared.
    /// # Arguments:
    /// * start_index: Starting index for the bit being set.
    /// * length:      Number of bits to set as being used.
    /// Returns a Result type with true if it succeeds or length == 0, and an error if the bit region is out of bounds.
    pub fn clear_bits(&mut self, start_index: usize, length: usize) -> Result<bool, BitmapError> {
        // Trivial case, do nothing
        if length == 0 {
            return Ok(true);
        }

        let end_index = start_index + length - 1;

        // Check that the start index and length are within bounds
        if self.bounds.includes_range(start_index, end_index) {
            // How many bits need to be set to 0 on the left/right hand sides of the region
            let mask_start = start_index;
            let mask_end = Word::SIZE_BITS - end_index - 1;

            // Shift the region to the right to get the end boundary
            // Ex. Shift right 4 (end index is 27 on 32-bit)
            // 1111 1111 1111 1111 1111 1111 1111 1111 >> (4) = 0000 1111 1111 1111 1111 1111 1111 1111
            let mut mask = Word::FULL >> mask_end;

            // AND with a region shifted to the left to get the start boundary
            // Ex. Shift left 4 (end index is 27 on 32-bit)
            // 1111 1111 1111 1111 1111 1111 1111 1111 << (4) = 1111 1111 1111 1111 1111 1111 1111 0000
            //
            // 1111 1111 1111 1111 1111 1111 1111 0000 &
            // 0000 1111 1111 1111 1111 1111 1111 1111
            // ---------------------------------------
            // 0000 1111 1111 1111 1111 1111 1111 0000
            mask &= Word::FULL << mask_start;

            // Invert the mask so there are zeros in the middle and 1s on the outside
            // !0000 1111 1111 1111 1111 1111 1111 0000 = 1111 0000 0000 0000 0000 0000 0000 1111
            //                                          Start ^                         End ^
            // Set all the bits within the region to 0s
            mask = !mask;

            unsafe {
                *self.pointer &= mask;
            }

            Ok(true)
        } else {
            let provided = match BitmapSegmentBounds::new(start_index, start_index + length - 1) {
                Ok(bounds) => bounds,
                Err(err) => {
                    return Err(err);
                }
            };
            let expected = self.bounds;
            Err(BitmapError::BitRegionOutOfBounds { provided, expected })
        }
    }

    /// Method to clear all the bits in the region by making them 0.
    /// MUST make sure it is okay for the region to be cleared.
    /// Returns a Result with true if it succeeds, and an error if it fails (ex. Trying to clear a region outside of bounds).
    pub fn clear_region_bits(&mut self) -> Result<bool, BitmapError> {
        let length = self.bounds.upper - self.bounds.lower + 1;
        self.clear_bits(self.bounds.lower, length)
    }

    // ----------------- Methods for retrieving bit/block status -----------------

    /// Method to retrieve the bit status for the bit index referenced by the bitfield segment.
    /// Returns a Result type with the bit status if it succeeds, and an error if the bit_index is None.
    pub fn get_bit(&self) -> Result<BitStatus, BitmapError> {
        if let Some(bit_index) = self.bit_index {
            self.get_specified_bit(bit_index)
        } else {
            Err(BitmapError::NoBitIndex)
        }
    }

    /// Method to retrieve the status for a specified bit.
    /// # Arguments:
    /// * bit_index: The bit index in the segment to return the status for.
    /// Returns a Result type with the bit status if it is in bounds, and an error otherwise.
    pub fn get_specified_bit(&self, bit_index: usize) -> Result<BitStatus, BitmapError> {
        if self.bounds.includes_value(bit_index) {
            let result = unsafe { (*self.pointer >> bit_index) & 1 };
            match result {
                1 => Ok(BitStatus::Set),
                _ => Ok(BitStatus::Free),
            }
        } else {
            Err(BitmapError::BitIndexOutOfBounds { bit_index })
        }
    }

    // ----------------- Methods for locating free bits -----------------

    /// Helper method for checking if the region within the segment bounds is full.
    /// Returns an enum for the region status.
    fn get_region_status(&self, bounds: BitmapSegmentBounds) -> RegionStatus {
        // AND the word value with a section with 1s in the specified region
        let region_length: usize = bounds.upper - bounds.lower;
        let mut mask: usize = !(0 << region_length);
        // Left shift the region of 1s to match up with the start offset
        mask <<= self.bounds.lower;
        // AND it with 0s at the end corresponding to the upper bound
        // The -1 is to account for 0-based indexing
        mask &= Word::FULL >> (Word::SIZE_BITS - self.bounds.upper - 1);

        // AND the value with the mask, which will keep any bit statuses in the target region
        let masked_value = unsafe { *self.pointer & mask };

        // The segment is full if every bit in the target region was set, making it equal to the mask
        if masked_value == mask {
            RegionStatus::Full
        } else if masked_value == 0 {
            RegionStatus::Empty
        } else {
            RegionStatus::Dirty
        }
    }

    /// Method to retrieve the first free bit index in a segment.
    /// Returns a Result type, with a usize corresponding to the bit index if there is a free space,
    /// or an error if the segment is full.
    pub fn get_free_index(&self) -> Result<usize, BitmapError> {
        if self.get_region_status(self.bounds) == RegionStatus::Full {
            return Err(BitmapError::BitmapSegmentFull);
        }

        // Set bounds (upper has + 1 since it is inclusive range)
        let lower = self.bounds.lower;
        let upper = self.bounds.upper + 1;

        // Iterate over every bit in the region until we find an available one
        for bit_index in lower..upper {
            let mask: usize = 1 << bit_index;
            let masked_value = unsafe { *self.pointer & mask };
            // Masked value will equal the mask when the bit is set
            // If it does not equal the mask, we have a free bit!
            if masked_value != mask {
                return Ok(bit_index);
            }
        }

        // Should not get here, but need it so all control paths return
        Err(BitmapError::NoFreeBitIndex)
    }

    /// Returns the value stored in the segment as a usize.
    pub fn get_value(&self) -> usize {
        unsafe { *self.pointer }
    }

    /// Returns the start address which the segment is located at as a usize.
    pub fn get_start_address(&self) -> usize {
        self.pointer as usize
    }

    /// Returns the end address which the segment is located at as a usize.
    pub fn get_end_address(&self) -> usize {
        self.pointer as usize + Word::SIZE_BYTES
    }
}

/// Iterator over bitfield segments from a specified point in the bitfield onward.
/// Sets appropriate bounds for the BitmapSegments returned so sections can be
/// easily set/freed as needed.
/// # Fields:
/// * bitfield_base:  Pointer to the base of the bitfield.
/// * start_bit:      The first bit in the allowed region.
/// * end_bit:        The last bit in the specified region.
/// * current_bits:   The first bit in the current segment.
/// * max_bits:       The maximum number of bits available to be iterated over.
#[derive(Debug)]
pub struct BitmapSegmentIterator {
    bitfield_base: *mut usize,
    start_bit: usize,
    end_bit: usize,
    current_bit: usize,
    max_bits: usize,
}

impl BitmapSegmentIterator {
    /// Constructor for a BitmapSegmentIterator.
    /// Determine the address of the first BitmapSegment and the final segment.
    /// WARNING: bitfield_base and max_bits MUST be correct or you will clobber memory!
    /// # Arguments:
    /// * bitfield_base: The base pointer of the bitfield.
    /// * start_bit:     The location of the first bit being iterated over relative to the bitfield base.
    /// * length:        The size of the region being iterated over in bits.
    /// * max_bits:      The maximum number of bits available to be iterated over.
    /// Returns Result with an initialized struct if all values are correct, and an error if not.
    pub unsafe fn new(
        bitfield_base: *mut usize,
        start_bit: usize,
        length: usize,
        max_bits: usize,
    ) -> Result<Self, BitmapError> {
        if start_bit == max_bits || start_bit + length > max_bits {
            return Err(BitmapError::IteratorOutOfBounds);
        } else if length == 0 {
            return Err(BitmapError::EmptyIterator);
        }

        // Compute the end bit as the last bit in the region relative to the base pointer
        let end_bit = start_bit + length;
        let current_bit = 0;

        Ok(Self {
            bitfield_base,
            start_bit,
            end_bit,
            current_bit,
            max_bits,
        })
    }
}

impl Iterator for BitmapSegmentIterator {
    type Item = BitmapSegment;

    /// While the current address is less than the end address for the bitmap, increment by
    /// a word at a time corresponding to a bitmap segment and return it.
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next iterator. If it is the first one, pull segment from the start bit
        if self.current_bit == 0 {
            self.current_bit = self.start_bit;
        }
        // Case where the iterator is done (next segment would be past the ending bit).
        if self.current_bit >= self.end_bit {
            return None;
        }

        // Create the segment
        let segment =
            unsafe { BitmapSegment::new(self.bitfield_base, self.current_bit, self.max_bits) };
        match segment {
            Ok(mut segment) => {
                // Unset start bit (single bit operations shouldn't be performed)
                segment.bit_index = None;

                // Bit offset from the base pointer of the segment start
                let segment_start_bit =
                    (segment.pointer as usize - self.bitfield_base as usize) * Byte::SIZE_BITS;

                // Compute and set the segment bounds
                let lower_bound = {
                    // If the start bit comes after the start of the segment, then set the bounds
                    // as the offset between the two.
                    // Otherwise, this will always be 0.
                    if self.start_bit > segment_start_bit {
                        self.start_bit - segment_start_bit
                    } else {
                        0
                    }
                };

                let upper_bound = {
                    // If the end bit comes before the end of the segment, then set the bounds
                    // as the offset between the two.
                    // Otherwise, this will always be equal to the number of bits in a word.
                    if self.end_bit < segment_start_bit + Word::SIZE_BITS {
                        self.end_bit - segment_start_bit - 1
                    } else {
                        Word::SIZE_BITS - 1
                    }
                };

                let bounds = BitmapSegmentBounds::new(lower_bound, upper_bound).ok()?;
                segment.set_bounds(bounds);

                // Increment the start bit to be in the next section and return the segment
                self.current_bit += Word::SIZE_BITS;
                Some(segment)
            }
            // Something very bad has gone wrong
            Err(err) => panic!("Unknown Error {}", err),
        }
    }
}

/// Struct providing a high-level interface for interacting with a bitmap.
/// Performs operations over word-sized segments at a time.
/// # Fields:
/// * base:   Pointer to the base of the bitmap.
/// * length: Length of the bitmap in bits. Used in bounds checking.
#[derive(Debug)]
pub struct Bitmap {
    base: *mut usize,
    length: usize,
}

impl Bitmap {
    /// Constructor for a bitfield which takes an arbitrary mutable pointer and a length of the bitfield.
    /// NOTE: While access is limited on areas outside of the bitfield region, the base pointer must be to
    /// a valid location in memory or else it will clobber memory!
    /// # Arguments:
    /// * base:   Mutable pointer to the base of the bitfield.
    /// * length: Length in bits of how long the bitfield spans for.
    pub const unsafe fn new(base: *mut usize, length: usize) -> Self {
        let bitfield = Self { base, length };
        bitfield
    }

    // --------------------- Methods operating on a single bit ---------------------

    /// Private method used to retrieve the appropriate bitfield segment for doing operations on.
    /// # Arguments:
    /// * bit_index: The bit index relative to the start of the bitfield to retrieve a segment for.
    /// Returns a Result with a bitfield segment if the index is valid, and an error otherwise.
    pub fn get_bit_segment(&mut self, bit_index: usize) -> Result<BitmapSegment, BitmapError> {
        unsafe { BitmapSegment::new(self.base, bit_index, self.length) }
    }

    /// Method used to retrieve the first free bit index in the bitfield.
    /// Returns a Result with a usize corresponding to the bit index if there is an available bit, or an error otherwise.
    pub fn get_first_free_bit_index(&self) -> Result<usize, BitmapError> {
        self.get_first_free_bit_index_from_offset(0)
    }

    /// Method used to retrieve the first free bit index in the bitfield from a specified offset.
    /// # Arguments:
    /// * offset: Usize corresponding to the bit offset to start from.
    /// Returns a Result with a usize corresponding to the bit index if there is an available bit, or an error otherwise.
    pub fn get_first_free_bit_index_from_offset(
        &self,
        offset: usize,
    ) -> Result<usize, BitmapError> {
        // Construct an iterator to iterate over segments with from an offset
        // The iterator also does bounds checking for us
        let bitmap_segment_iterator = unsafe {
            BitmapSegmentIterator::new(self.base, offset, self.length - offset, self.length)?
        };
        let mut index_count = 0;
        for segment in bitmap_segment_iterator {
            match segment.get_free_index() {
                Err(_err) => index_count += Word::SIZE_BITS,
                Ok(bit_index) => {
                    return Ok(bit_index + index_count);
                }
            }
        }

        Err(BitmapError::NoFreeBitIndex)
    }

    /// Method to retrieve the bit status of an index.
    /// # Arguments:
    /// * bit_index: Bit index relative to the start of the bitfield to retrieve the status for.
    /// Returns a result type with the BitStatus if the index is a valid range, and an error otherwise.
    pub fn get_bit(&self, bit_index: usize) -> Result<BitStatus, BitmapError> {
        // Get the segment directly so we can use an immutable self reference
        let segment = unsafe { BitmapSegment::new(self.base, bit_index, self.length) };
        segment?.get_bit()
    }

    /// Method to set the bit status of an index to used.
    /// # Arguments:
    /// * bit_index: Bit index relative to the start of the bitfield to set to used.
    /// Returns a result with true if the index is a valid range, and an error otherwise.
    pub fn set_bit(&mut self, bit_index: usize) -> Result<bool, BitmapError> {
        match self.get_bit_segment(bit_index)?.set_bit() {
            Ok(status) => Ok(status),
            Err(err) => {
                // Catch the error with the relative index and rethrow it with the
                // absolute bit index from the start of the bitfield.
                match err {
                    BitmapError::BitAlreadySet { .. } => {
                        Err(BitmapError::BitAlreadySet { bit_index })
                    }
                    BitmapError::BitIndexOutOfBounds { .. } => {
                        Err(BitmapError::BitIndexOutOfBounds { bit_index })
                    }
                    // Should never get here
                    _ => Err(BitmapError::UnknownError),
                }
            }
        }
    }

    /// Method to clear the bit status of an index to unused.
    /// # Arguments:
    /// * bit_index: Bit index relative to the start of the bitfield to set to unused.
    /// Returns a result with true if the index is a valid range, and an error otherwise.
    pub fn clear_bit(&mut self, bit_index: usize) -> Result<bool, BitmapError> {
        self.get_bit_segment(bit_index)?.clear_bit()
    }

    // --------------------- Methods operating on a bit region ---------------------

    /// Method to mass set all the bits in a specified region to used.
    /// # Arguments:
    /// * start_index:      Starting bit index for the region.
    /// * length:           Number of bits to set.
    /// * allow_overwrite:  Boolean flag for whether to allow setting a section which isn't completely empty.
    /// Returns a Result type with true if the range is valid, and an error otherwise.
    pub fn set_region_bits(
        &mut self,
        start_index: usize,
        length: usize,
        allow_overwrite: bool,
    ) -> Result<bool, BitmapError> {
        // Construct an iterator to iterate over segments with
        let bitmap_segment_iterator =
            unsafe { BitmapSegmentIterator::new(self.base, start_index, length, self.length)? };

        if allow_overwrite || self.get_region_status(start_index, length)? == RegionStatus::Empty {
            for mut segment in bitmap_segment_iterator {
                match segment.set_region_bits(allow_overwrite) {
                    Err(error) => {
                        return Err(error);
                    }
                    Ok(_) => continue,
                }
            }
        } else {
            return Err(BitmapError::BitRegionAlreadySet {
                region: BitmapSegmentBounds::new(start_index, start_index + length - 1)?,
            });
        }

        Ok(true)
    }

    /// Method to clear all the bits in a specified region to used.
    /// # Arguments:
    /// * start_index: Starting bit index for the region.
    /// * length:      Number of bits to set.
    /// Returns a Result type with true if the range is valid, and an error otherwise.
    pub fn clear_region_bits(
        &mut self,
        start_index: usize,
        length: usize,
    ) -> Result<bool, BitmapError> {
        // Construct an iterator to iterate over segments with
        let bitmap_segment_iterator =
            unsafe { BitmapSegmentIterator::new(self.base, start_index, length, self.length)? };
        for mut segment in bitmap_segment_iterator {
            match segment.clear_region_bits() {
                Err(error) => {
                    return Err(error);
                }
                Ok(_) => continue,
            }
        }

        Ok(true)
    }

    /// Method to get the region status for a region spanning multiple segments.
    /// # Arguments:
    /// * start_index: The starting bit index for the region to look over.
    /// * length:      The length (in bits) of the region being iterated over.
    /// Returns a RegionStatus corresponding to the status of the entire region.
    pub fn get_region_status(
        &mut self,
        start_index: usize,
        length: usize,
    ) -> Result<RegionStatus, BitmapError> {
        let mut segment_count = 0;
        let mut status = RegionStatus::Empty;
        let bitmap_segment_iterator =
            unsafe { BitmapSegmentIterator::new(self.base, start_index, length, self.length) };

        // Update the status accordingly
        for segment in bitmap_segment_iterator.unwrap() {
            // Update the overall status based on the status fo this segment
            match segment.get_region_status(segment.bounds) {
                // If a dirty region is ever encountered then the whole region is dirty
                RegionStatus::Dirty => {
                    return Ok(RegionStatus::Dirty);
                }
                // If a full bit region is found, update status to full if it is the first segment
                // or return Dirty otherwise.
                RegionStatus::Full => match (segment_count, &status) {
                    (0, _) => status = RegionStatus::Full,
                    (_, RegionStatus::Empty) => {
                        return Ok(RegionStatus::Dirty);
                    }
                    (_, _) => {}
                },
                // Since status is empty by default, do nothing unless all bit regions before now have been full
                RegionStatus::Empty => match (segment_count, &status) {
                    (_, RegionStatus::Full) => {
                        return Ok(RegionStatus::Dirty);
                    }
                    (_, _) => {}
                },
            }
            segment_count += 1;
        }

        Ok(status)
    }

    /// Method to get the word stored at a certain bit index
    /// # Arguments:
    /// * bit_index: Bit index relative to the start of the bitfield to return the word value around.
    /// Returns a Result with the word value containing a specific bit index if it is a valid index, and an error otherwise.
    pub fn get_word_value_at(&mut self, bit_index: usize) -> Result<usize, BitmapError> {
        let segment = self.get_bit_segment(bit_index)?;
        unsafe { Ok(*segment.pointer) }
    }

    /// Method to get the starting address of the bitfield.
    pub fn get_start_address(&self) -> usize {
        self.base as *const _ as usize
    }

    /// Method to get the address after the end of the bit field.
    /// Returns the next available byte.
    pub fn get_end_address(&self) -> usize {
        self.get_start_address() + self.get_size_bytes()
    }

    /// Returns the size of the bitfield region.
    pub fn get_size_bytes(&self) -> usize {
        ceiling(self.length as f32 / Byte::SIZE_BITS as f32)
    }
}

// ---------------------- Module Tests ----------------------
#[cfg(test)]
mod tests {
    use super::super::errors::BitmapError;
    use super::super::types::{BitStatus, RegionStatus, Word};
    use super::{Bitmap, BitmapSegment, BitmapSegmentBounds, BitmapSegmentIterator};

    // ---------------------- Bitmap Tests ----------------------

    #[test]
    fn test_init() {
        // Construct a bitfield
        const NUM_WORDS: usize = 4;
        let memory: [usize; NUM_WORDS] = [0; NUM_WORDS];
        let memory_ptr = &memory as *const usize as *mut usize;
        let length = NUM_WORDS * Word::SIZE_BITS;
        let bitfield = unsafe { Bitmap::new(memory_ptr, length) };

        // Verify base and length are correct
        // Largely trivial but a sanity check
        assert_eq!(memory_ptr as usize, bitfield.base as usize);
        assert_eq!(length, bitfield.length);
    }

    #[test]
    fn test_get_bit_segment() {
        // Construct a bitfield
        const NUM_WORDS: usize = 4;
        let memory: [usize; NUM_WORDS] = [0; NUM_WORDS];
        let memory_ptr = &memory as *const usize as *mut usize;
        let length = NUM_WORDS * Word::SIZE_BITS;
        let mut bitfield = unsafe { Bitmap::new(memory_ptr, length) };

        // Verify invalid index beyond length doesn't work
        let expected = BitmapError::BitIndexOutOfBounds { bit_index: length };
        match bitfield.get_bit_segment(length) {
            Ok(segment) => assert!(
                false,
                "Should not be able to retrieve bit at index {} but found \n{:?}",
                length, segment
            ),
            Err(err) => {
                assert_eq!(expected, err);
            }
        }

        // Test with every valid bit index to ensure expected results
        let expected_bounds = BitmapSegmentBounds::new(0, Word::SIZE_BITS - 1).unwrap();
        for i in 0..length {
            let segment = bitfield.get_bit_segment(i);
            match segment {
                Ok(segment) => {
                    let expected_index = i % Word::SIZE_BITS;
                    assert_eq!(expected_index, segment.bit_index.unwrap());
                    assert_eq!(expected_bounds, segment.bounds);
                }
                Err(err) => assert!(false, "Encountered error {}", err),
            }
        }
    }

    #[test]
    fn test_get_first_free_bit_index() {
        // Construct a bitfield
        const NUM_WORDS: usize = 4;
        let mut memory: [usize; NUM_WORDS] = [0; NUM_WORDS];
        let memory_ptr = &memory as *const usize as *mut usize;
        let length = NUM_WORDS * Word::SIZE_BITS;
        let bitfield = unsafe { Bitmap::new(memory_ptr, length) };

        // With all 0s, the first free index should be at 0.
        let free_index = bitfield.get_first_free_bit_index().unwrap();
        assert_eq!(0, free_index);
        // Set the first index (0) to 1. Expect first available index to be 1 now.
        memory[0] = 1;
        let free_index = bitfield.get_first_free_bit_index().unwrap();
        assert_eq!(1, free_index);

        // Make it so there are no free indices
        for i in 0..NUM_WORDS {
            memory[i] = Word::FULL;
        }
        let free_index = bitfield.get_first_free_bit_index();
        assert_eq!(Err(BitmapError::NoFreeBitIndex), free_index);

        // Make it so the very last index is free
        memory[NUM_WORDS - 1] >>= 1;
        let free_index = bitfield.get_first_free_bit_index().unwrap();
        assert_eq!(length - 1, free_index);
    }

    #[test]
    fn test_get_bit() {
        // Construct a bitfield
        const NUM_WORDS: usize = 4;
        let mut memory: [usize; NUM_WORDS] = [0; NUM_WORDS];
        let memory_ptr = &memory as *const usize as *mut usize;
        let length = NUM_WORDS * Word::SIZE_BITS;
        let bitfield = unsafe { Bitmap::new(memory_ptr, length) };

        // Retrieve the bit status corresponding to every bit, make sure it is free.
        for i in 0..length {
            assert_eq!(Ok(BitStatus::Free), bitfield.get_bit(i));
        }

        // Verify out of bounds returns an error
        let expected = BitmapError::BitIndexOutOfBounds { bit_index: length };
        assert_eq!(Err(expected), bitfield.get_bit(length));

        // Set bits and verify it now returns all BitStatus::Set
        memory.iter_mut().for_each(|word| *word = Word::FULL);

        // Retrieve the bit status corresponding to every bit, make sure it is set.
        for i in 0..length {
            assert_eq!(Ok(BitStatus::Set), bitfield.get_bit(i));
        }
    }

    #[test]
    fn test_set_bit() {
        // Construct a bitfield
        const NUM_WORDS: usize = 4;
        let memory: [usize; NUM_WORDS] = [0; NUM_WORDS];
        let memory_ptr = &memory as *const usize as *mut usize;
        let length = NUM_WORDS * Word::SIZE_BITS;
        let mut bitfield = unsafe { Bitmap::new(memory_ptr, length) };

        // Verify every bit within bounds can be set
        for bit_index in 0..length {
            assert_eq!(Ok(BitStatus::Free), bitfield.get_bit(bit_index));
            assert_eq!(Ok(true), bitfield.set_bit(bit_index));
            assert_eq!(Ok(BitStatus::Set), bitfield.get_bit(bit_index));
        }

        // Verify out of bounds returns an error
        let expected = BitmapError::BitIndexOutOfBounds { bit_index: length };
        assert_eq!(Err(expected), bitfield.set_bit(length));

        // Verify bits cannot be set again
        for bit_index in 0..length {
            let expected = BitmapError::BitAlreadySet {
                bit_index: bit_index,
            };
            assert_eq!(Ok(BitStatus::Set), bitfield.get_bit(bit_index));
            assert_eq!(Err(expected), bitfield.set_bit(bit_index));
            assert_eq!(Ok(BitStatus::Set), bitfield.get_bit(bit_index));
        }
    }

    #[test]
    fn test_clear_bit() {
        // Construct a bitfield
        const NUM_WORDS: usize = 4;
        let memory: [usize; NUM_WORDS] = [0; NUM_WORDS];
        let memory_ptr = &memory as *const usize as *mut usize;
        let length = NUM_WORDS * Word::SIZE_BITS;
        let mut bitfield = unsafe { Bitmap::new(memory_ptr, length) };

        // Verify clearing an already empty bit succeeds but does nothing
        for bit_index in 0..length {
            assert_eq!(BitStatus::Free, bitfield.get_bit(bit_index).unwrap());
            assert_eq!(true, bitfield.clear_bit(bit_index).unwrap());
            assert_eq!(BitStatus::Free, bitfield.get_bit(bit_index).unwrap());
        }

        // Verify an out-of-bounds bit cannot be cleared
        let expected = BitmapError::BitIndexOutOfBounds { bit_index: length };
        assert_eq!(Err(expected), bitfield.clear_bit(length));

        // Verify clearing a set bit works
        for bit_index in 0..length {
            assert_eq!(BitStatus::Free, bitfield.get_bit(bit_index).unwrap());
            let _ = bitfield.set_bit(bit_index);
            assert_eq!(BitStatus::Set, bitfield.get_bit(bit_index).unwrap());
            assert_eq!(true, bitfield.clear_bit(bit_index).unwrap());
            assert_eq!(BitStatus::Free, bitfield.get_bit(bit_index).unwrap());
        }
    }

    #[test]
    fn test_set_region_bits() {
        // Construct a bitfield
        const NUM_WORDS: usize = 4;
        let mut memory: [usize; NUM_WORDS] = [0; NUM_WORDS];
        let memory_ptr = &memory as *const usize as *mut usize;
        let length = NUM_WORDS * Word::SIZE_BITS;
        let mut bitfield = unsafe { Bitmap::new(memory_ptr, length) };

        // Verify setting the entire region works
        assert_eq!(true, bitfield.set_region_bits(0, length, false).unwrap());
        // Reset memory values
        for i in 0..NUM_WORDS {
            assert_eq!(Word::FULL, memory[i]);
            memory[i] = 0;
        }

        // Verify setting out of bounds changes nothing
        // Start index out of bounds
        let start_index = length;
        let expected = BitmapError::IteratorOutOfBounds;
        match bitfield.set_region_bits(start_index, 0, false) {
            Err(err) => {
                assert_eq!(expected, err);
            }
            Ok(_) => {
                unreachable!();
            }
        }

        // Length out of bounds
        let expected = BitmapError::IteratorOutOfBounds;
        match bitfield.set_region_bits(0, length + 1, false) {
            Err(err) => {
                assert_eq!(expected, err);
            }
            Ok(_) => {
                unreachable!();
            }
        }

        // Verify nothing changed
        for i in 0..NUM_WORDS {
            assert_eq!(Word::EMPTY, memory[i]);
        }

        // Verify a sub region can be set
        let region_offset = 10;
        let region_length = 10;
        assert_eq!(
            true,
            bitfield
                .set_region_bits(region_offset, region_length, false)
                .unwrap()
        );
        let expected_mask = 0xFFC00;
        assert_eq!(expected_mask, memory[0]);

        // Verify none of the other regions changed
        for i in 1..NUM_WORDS {
            assert_eq!(Word::EMPTY, memory[i])
        }

        // Verify trying to set in a region with set bits returns an error.
        let start = 0;
        let region_length = 30;
        let expected = BitmapError::BitRegionAlreadySet {
            region: BitmapSegmentBounds::new(start, start + region_length - 1).unwrap(),
        };
        match bitfield.set_region_bits(start, region_length, false) {
            Err(err) => {
                assert_eq!(expected, err);
            }
            Ok(_) => {
                assert!(
                    false,
                    "Was able to set region bits without getting an error."
                );
            }
        }

        // Verify none of the other regions changed
        for i in 1..NUM_WORDS {
            assert_eq!(Word::EMPTY, memory[i])
        }

        // Length of 0
        let expected = BitmapError::EmptyIterator;
        match bitfield.set_region_bits(0, 0, false) {
            Err(err) => {
                assert_eq!(expected, err);
            }
            Ok(_) => {
                unreachable!();
            }
        }

        // Verify none of the other regions changed
        for i in 1..NUM_WORDS {
            assert_eq!(Word::EMPTY, memory[i])
        }
    }

    #[test]
    fn test_clear_region_bits() {
        // Construct a bitfield
        const NUM_WORDS: usize = 4;
        let mut memory: [usize; NUM_WORDS] = [Word::FULL; NUM_WORDS];
        let memory_ptr = &memory as *const usize as *mut usize;
        let length = NUM_WORDS * Word::SIZE_BITS;
        let mut bitfield = unsafe { Bitmap::new(memory_ptr, length) };

        // Verify clearing the entire region works
        assert_eq!(true, bitfield.clear_region_bits(0, length).unwrap());
        // Check that it worked and reset values.
        for i in 0..NUM_WORDS {
            assert_eq!(0, memory[i]);
            memory[i] = Word::FULL;
        }

        // Verify clearing out of bounds changes nothing
        // Start index out of bounds
        let start_index = length;
        assert_eq!(
            bitfield.clear_region_bits(start_index, 0),
            Err(BitmapError::IteratorOutOfBounds)
        );
        assert_eq!(
            bitfield.clear_region_bits(0, length + 1),
            Err(BitmapError::IteratorOutOfBounds)
        );

        // Check that nothing changed
        for i in 0..NUM_WORDS {
            assert_eq!(Word::FULL, memory[i]);
        }

        // Verify a sub region can be cleared
        let start_offset = 10;
        let length = Word::SIZE_BITS + 3;
        bitfield.clear_region_bits(start_offset, length).unwrap();
        assert_eq!(1023, memory[0]); // Only have the lowest 10 bits remaining, 2 ^ 10 - 1 = 1023
        assert_eq!(Word::FULL & !((1 << 13) - 1), memory[1]); // Clears the first 13 bits of the second word, (2 ^ 31 - 1) - (2 ^ 13 - 1) = 4294959104
                                                              // Verify none of the other words were affected
        for i in 2..NUM_WORDS {
            assert_eq!(Word::FULL, memory[i]);
        }

        // Reset values
        for i in 0..NUM_WORDS {
            memory[i] = Word::FULL;
        }

        // Length of 0
        let expected = BitmapError::EmptyIterator;
        match bitfield.clear_region_bits(0, 0) {
            Err(err) => {
                assert_eq!(expected, err);
            }
            Ok(_) => {
                unreachable!();
            }
        }

        // Check that nothing changed
        for i in 0..NUM_WORDS {
            assert_eq!(Word::FULL, memory[i]);
        }
    }

    // ---------------------- Bitmap Segment Iterator Tests ----------------------

    fn run_bitfield_segment_iterator_tests() {
        const NUM_WORDS: usize = 4;
        let memory: [usize; NUM_WORDS] = [0; NUM_WORDS];
        let base_ptr: *mut usize = &memory as *const usize as *mut usize;
        let length = Word::SIZE_BITS * NUM_WORDS;
        let iterator = unsafe { BitmapSegmentIterator::new(base_ptr, 0, length, length).unwrap() };

        let mut bit_count = 0;

        // Verify behavior with a clean alignment and no start/end limitations
        for segment in iterator {
            assert_eq!(0, segment.bounds.lower);
            assert_eq!(Word::SIZE_BITS - 1, segment.bounds.upper);

            bit_count += segment.bounds.upper - segment.bounds.lower + 1;
        }
        assert_eq!(length, bit_count);

        // Verify behavior with a start and end offset
        let start_offset = 11;
        let end_offset = 15;
        let expected_bit_count = length - start_offset - end_offset;
        let iterator = unsafe {
            BitmapSegmentIterator::new(base_ptr, start_offset, expected_bit_count, length).unwrap()
        };
        bit_count = 0;

        for segment in iterator {
            // First iterator should have different bounds
            if bit_count == 0 {
                assert_eq!(start_offset, segment.bounds.lower);
            } else {
                assert_eq!(0, segment.bounds.lower);
            }

            // Last iterator should have different bounds
            let remaining_bits = expected_bit_count - bit_count;
            if remaining_bits < Word::SIZE_BITS {
                assert_eq!(remaining_bits - 1, segment.bounds.upper);
            } else {
                assert_eq!(Word::SIZE_BITS - 1, segment.bounds.upper);
            }

            bit_count += segment.bounds.upper - segment.bounds.lower + 1;
        }
        assert_eq!(expected_bit_count, bit_count);
    }

    // ---------------------- Bitmap Segment Tests ----------------------

    // ---------------------- Helper Functions ----------------------

    fn assert_get_bit(segment: &BitmapSegment, bit_index: Option<usize>, expected: BitStatus) {
        // Can either specify a bit or just use the segment's default
        if let Some(bit_index) = bit_index {
            match segment.get_specified_bit(bit_index) {
                Ok(status) => {
                    assert_eq!(expected, status);
                }
                Err(err) => {
                    assert!(false, "{}", err);
                }
            }
        } else {
            match segment.get_bit() {
                Ok(status) => {
                    assert_eq!(expected, status);
                }
                Err(err) => {
                    assert!(false, "{}", err);
                }
            }
        }
    }

    fn assert_invalid_get_bit(
        segment: &BitmapSegment,
        bit_index: Option<usize>,
        expected: BitmapError,
    ) {
        // Can either specify a bit or just use the segment's default
        if let Some(bit_index) = bit_index {
            match segment.get_specified_bit(bit_index) {
                Ok(status) => {
                    assert!(false, "Unexpectedly encountered status: {:?}", status);
                }
                Err(err) => {
                    assert_eq!(expected, err);
                }
            }
        } else {
            match segment.get_bit() {
                Ok(status) => {
                    assert!(false, "Unexpectedly encountered status: {:?}", status);
                    assert_eq!(true, false);
                }
                Err(err) => {
                    assert_eq!(expected, err);
                }
            }
        }
    }

    fn assert_set_bit(segment: &mut BitmapSegment, bit_index: Option<usize>, expected: bool) {
        // Can either specify a bit or just use the segment's default
        if let Some(bit_index) = bit_index {
            match segment.set_specified_bit(bit_index) {
                Ok(status) => {
                    assert_eq!(expected, status);
                }
                Err(err) => {
                    assert!(false, "Unexpectedly encountered error: {}", err);
                }
            }
        } else {
            match segment.set_bit() {
                Ok(status) => {
                    assert_eq!(expected, status);
                }
                Err(err) => {
                    assert!(false, "Unexpectedly encountered error: {}", err);
                }
            }
        }
    }

    fn assert_invalid_set_bit(
        segment: &mut BitmapSegment,
        bit_index: Option<usize>,
        expected: BitmapError,
    ) {
        // Can either specify a bit or just use the segment's default
        if let Some(bit_index) = bit_index {
            match segment.set_specified_bit(bit_index) {
                Ok(status) => {
                    assert!(false, "Unexpectedly encountered status: {:?}", status);
                }
                Err(err) => {
                    assert_eq!(expected, err);
                }
            }
        } else {
            match segment.set_bit() {
                Ok(status) => {
                    assert!(false, "Unexpectedly encountered status: {:?}", status);
                }
                Err(err) => {
                    assert_eq!(expected, err);
                }
            }
        }
    }

    fn assert_clear_bit(segment: &mut BitmapSegment, bit_index: Option<usize>, expected: bool) {
        // Can either specify a bit or just use the segment's default
        if let Some(bit_index) = bit_index {
            match segment.clear_specified_bit(bit_index) {
                Ok(status) => {
                    assert_eq!(expected, status);
                }
                Err(err) => {
                    assert!(false, "Unexpectedly encountered error: {}", err);
                }
            }
        } else {
            match segment.clear_bit() {
                Ok(status) => {
                    assert_eq!(expected, status);
                }
                Err(err) => {
                    assert!(false, "Unexpectedly encountered error: {}", err);
                }
            }
        }
    }

    fn assert_invalid_clear_bit(
        segment: &mut BitmapSegment,
        bit_index: Option<usize>,
        expected: BitmapError,
    ) {
        // Can either specify a bit or just use the segment's default
        if let Some(bit_index) = bit_index {
            match segment.clear_specified_bit(bit_index) {
                Ok(status) => {
                    assert!(false, "Unexpectedly encountered status: {:?}", status);
                }
                Err(err) => {
                    assert_eq!(expected, err);
                }
            }
        } else {
            match segment.clear_bit() {
                Ok(status) => {
                    assert!(false, "Unexpectedly encountered status: {:?}", status);
                }
                Err(err) => {
                    assert_eq!(expected, err);
                }
            }
        }
    }

    fn assert_index_found(segment: &BitmapSegment, expected: usize) {
        match segment.get_free_index() {
            Ok(bit_index) => {
                assert_eq!(expected, bit_index);
            }
            Err(err) => {
                assert!(
                    false,
                    "Segment should not be full but encountered error {}",
                    err
                );
            }
        }
    }

    fn assert_segment_full(segment: &BitmapSegment) {
        match segment.get_free_index() {
            Ok(bit_index) => {
                assert!(
                    false,
                    "Should not be able to find free index but found index {}",
                    bit_index
                );
            }
            Err(err) => {
                assert_eq!(BitmapError::BitmapSegmentFull, err);
            }
        }
    }

    fn assert_clear_bits(
        segment: &mut BitmapSegment,
        bounds: Option<BitmapSegmentBounds>,
        expected: usize,
    ) {
        if let Some(bounds) = bounds {
            let start_index = bounds.lower;
            let length = bounds.upper - bounds.lower + 1;
            match segment.clear_bits(start_index, length) {
                Ok(_) => {
                    let value = unsafe { *segment.pointer };
                    assert_eq!(expected, value);
                }
                Err(_err) => {
                    assert!(
                        false,
                        "Error when clearing all region bits without bounds set"
                    );
                }
            }
        } else {
            match segment.clear_region_bits() {
                Ok(_) => {
                    let value = unsafe { *segment.pointer };
                    assert_eq!(expected, value);
                }
                Err(_err) => {
                    assert!(
                        false,
                        "Error when clearing all region bits without bounds set"
                    );
                }
            }
        }
    }

    fn assert_invalid_clear_bits(
        segment: &mut BitmapSegment,
        bounds: Option<BitmapSegmentBounds>,
        expected: BitmapError,
    ) {
        if let Some(bounds) = bounds {
            let start_index = bounds.lower;
            let length = bounds.upper - bounds.lower + 1;
            match segment.clear_bits(start_index, length) {
                Ok(_) => {
                    assert!(
                        false,
                        "Should not be able to clear bit range {:?}",
                        bounds.get_bounds()
                    );
                }
                Err(err) => {
                    assert_eq!(expected, err);
                }
            }
        } else {
            match segment.clear_region_bits() {
                Ok(_) => {
                    assert!(
                        false,
                        "Should not be able to clear bit range {:?}",
                        segment.bounds.get_bounds()
                    );
                }
                Err(err) => {
                    assert_eq!(expected, err);
                }
            }
        }
    }

    fn assert_set_bits(
        segment: &mut BitmapSegment,
        bounds: Option<BitmapSegmentBounds>,
        expected: usize,
    ) {
        if let Some(bounds) = bounds {
            let start_index = bounds.lower;
            let length = bounds.upper - bounds.lower + 1;
            match segment.set_bits(start_index, length) {
                Ok(_) => {
                    let value = unsafe { *segment.pointer };
                    assert_eq!(expected, value);
                }
                Err(_err) => {
                    assert!(
                        false,
                        "Error when setting all region bits without bounds set"
                    );
                }
            }
        } else {
            match segment.set_region_bits(false) {
                Ok(_) => {
                    let value = unsafe { *segment.pointer };
                    assert_eq!(expected, value);
                }
                Err(_err) => {
                    assert!(
                        false,
                        "Error when setting all region bits without bounds set"
                    );
                }
            }
        }
    }

    fn assert_invalid_set_bits(
        segment: &mut BitmapSegment,
        bounds: Option<BitmapSegmentBounds>,
        expected: BitmapError,
    ) {
        if let Some(bounds) = bounds {
            let start_index = bounds.lower;
            let length = bounds.upper - bounds.lower + 1;
            match segment.set_bits(start_index, length) {
                Ok(_) => {
                    assert!(
                        false,
                        "Should not be able to set bit range {:?}",
                        bounds.get_bounds()
                    );
                }
                Err(err) => {
                    assert_eq!(expected, err);
                }
            }
        } else {
            match segment.set_region_bits(false) {
                Ok(_) => {
                    assert!(
                        false,
                        "Should not be able to set bit range {:?}",
                        segment.bounds.get_bounds()
                    );
                }
                Err(err) => {
                    assert_eq!(expected, err);
                }
            }
        }
    }

    // ---------------------- Bitmap Segment Test Cases ----------------------

    #[test]
    fn test_segment_init() {
        const NUM_WORDS: usize = 12;
        let memory: [usize; NUM_WORDS] = [0; NUM_WORDS];
        let base_ptr: *mut usize = &memory as *const usize as *mut usize;
        let length = Word::SIZE_BITS * NUM_WORDS;

        // Verify out of bounds index doesn't work
        let segment = unsafe { BitmapSegment::new(base_ptr, length, length) };
        let expected = BitmapError::BitIndexOutOfBounds { bit_index: length };
        match segment {
            Ok(segment) => {
                assert!(
                    false,
                    "Should not be able to retrieve bit at index {} but found \n{:?}",
                    length, segment
                );
            }
            Err(err) => {
                assert_eq!(expected, err);
            }
        }

        // Test every increment
        for i in 0..NUM_WORDS {
            let segment =
                unsafe { BitmapSegment::new(base_ptr, i * Word::SIZE_BITS, length) }.unwrap();
            assert_eq!(
                base_ptr as usize + i * Word::SIZE_BYTES,
                segment.pointer as usize
            );
        }

        // Test uneven increments
        let start_bit = 6 * Word::SIZE_BITS + 4;
        let segment = unsafe { BitmapSegment::new(base_ptr, start_bit, length) }.unwrap();
        // Pointer should be aligned on same word boundary
        assert_eq!(
            base_ptr as usize + 6 * Word::SIZE_BYTES,
            segment.pointer as usize
        );
    }

    #[test]
    fn test_segment_set_bounds() {
        let segment_memory: usize = 0;
        let ptr = &segment_memory as *const usize as *mut usize;
        let mut segment = unsafe { BitmapSegment::new(ptr, 0, Word::SIZE_BITS) }.unwrap();

        assert_eq!(0, segment.bounds.lower);
        assert_eq!(Word::SIZE_BITS - 1, segment.bounds.upper);

        let lower = 1;
        let upper = Word::SIZE_BITS - 1;

        // Set the bounds, verify return is correct and changes were made
        let bounds = BitmapSegmentBounds::new(lower, upper).unwrap();
        assert_eq!(true, segment.set_bounds(bounds));
        assert_eq!(lower, segment.bounds.lower);
        assert_eq!(upper, segment.bounds.upper);

        // Verify upper bound cannot be set beyond number of bits in a word
        let new_upper = Word::SIZE_BITS;
        let bounds = BitmapSegmentBounds::new(lower, new_upper).unwrap();
        assert_eq!(false, segment.set_bounds(bounds));
        assert_eq!(lower, segment.bounds.lower);
        assert_eq!(upper, segment.bounds.upper);
    }

    #[test]
    fn test_segment_get_region_status() {
        let segment_memory: usize = Word::FULL;
        let ptr = &segment_memory as *const usize as *mut usize;
        let mut segment = unsafe { BitmapSegment::new(ptr, 0, Word::SIZE_BITS) }.unwrap();

        // Test with no bounds
        assert_eq!(
            RegionStatus::Full,
            segment.get_region_status(segment.bounds)
        );

        unsafe {
            *segment.pointer -= 1;
        }
        assert_eq!(
            RegionStatus::Dirty,
            segment.get_region_status(segment.bounds)
        );

        unsafe {
            *segment.pointer = Word::EMPTY;
        }
        assert_eq!(
            RegionStatus::Empty,
            segment.get_region_status(segment.bounds)
        );

        // Test with bounds
        let lower = Word::EMPTY + 6;
        let upper = Word::SIZE_BITS - 6;
        segment.set_bounds(BitmapSegmentBounds::new(lower, upper).unwrap());

        // Verify min/max values still pass
        unsafe { *segment.pointer = Word::FULL }
        assert_eq!(
            RegionStatus::Full,
            segment.get_region_status(segment.bounds)
        );

        unsafe {
            *segment.pointer = Word::EMPTY;
        }
        assert_eq!(
            RegionStatus::Empty,
            segment.get_region_status(segment.bounds)
        );

        // This is out of the region, so it should still register as full
        let mask: usize = !(1 << upper + 1);
        unsafe { *segment.pointer = Word::FULL & mask };
        assert_eq!(
            RegionStatus::Full,
            segment.get_region_status(segment.bounds)
        );

        // This is just in the region, so it should return the upper index
        let mask: usize = !(1 << upper);
        unsafe { *segment.pointer = Word::FULL & mask };
        assert_eq!(
            RegionStatus::Dirty,
            segment.get_region_status(segment.bounds)
        );
    }

    #[test]
    fn test_segment_get_free_index() {
        let segment_memory: usize = Word::FULL;
        let ptr = &segment_memory as *const usize as *mut usize;
        let mut segment = unsafe { BitmapSegment::new(ptr, 0, Word::SIZE_BITS) }.unwrap();

        assert_segment_full(&segment);

        // Free a space at the very last index
        let expected_index = Word::SIZE_BITS - 1;
        unsafe { *segment.pointer = Word::FULL >> 1 };
        assert_index_found(&segment, expected_index);

        // Free a space at the very first index
        let expected_index = 0;
        unsafe { *segment.pointer = Word::FULL << 1 };
        assert_index_found(&segment, expected_index);

        // Free a space a few bits in and verify it gets found
        let expected_index = 4;
        // Mask  = 0000 0000 0000 0000 0000 0000 1110 1111
        // Value = 1111 1111 1111 1111 1111 1111 1110 1111
        // Index is found here ---------------------^ (index 4)
        let mask: usize = !(1 << 4);
        unsafe { *segment.pointer = Word::FULL & mask };
        assert_index_found(&segment, expected_index);

        // Test with bounds that exclude available indices

        let lower = Word::EMPTY + 6;
        let upper = Word::SIZE_BITS - 6;
        segment.set_bounds(BitmapSegmentBounds::new(lower, upper).unwrap());

        // Since lower bound is out of range for the available index it should find nothing
        //            End v                    Start v
        // Region = 1111 1|111 1111 1111 1111 1111 11| 10 1111
        assert_segment_full(&segment);

        // Out of range on the upper end
        let mask: usize = !(1 << upper + 1);
        unsafe { *segment.pointer = Word::FULL & mask };
        assert_segment_full(&segment);

        // In range but just barely on the upper end
        let mask: usize = !(1 << upper);
        unsafe { *segment.pointer = Word::FULL & mask };
        assert_index_found(&segment, upper);

        // In range but just barely on the lower end
        let mask: usize = !(1 << lower);
        unsafe { *segment.pointer = Word::FULL & mask };
        assert_index_found(&segment, lower);

        // In range in the middle
        let expected_index = lower + 4;
        let mask: usize = !(1 << expected_index);
        unsafe { *segment.pointer = Word::FULL & mask };
        assert_index_found(&segment, expected_index);
    }

    /// Covers set_bit and set_specified_bit.
    #[test]
    fn test_segment_set_bit() {
        let segment_memory: usize = 0;
        let ptr = &segment_memory as *const usize as *mut usize;
        let mut segment = unsafe { BitmapSegment::new(ptr, 0, Word::SIZE_BITS) }.unwrap();

        // Verify bit is currently 0
        assert_get_bit(&segment, None, BitStatus::Free);

        // Set the bit
        assert_set_bit(&mut segment, None, true);

        // Verify bit index 0 was set
        assert_get_bit(&segment, None, BitStatus::Set);

        // Verify bounds work as expected
        let lower = 1;
        let upper = Word::SIZE_BITS - 2;
        let bounds = BitmapSegmentBounds::new(lower, upper).unwrap();
        assert_eq!(true, segment.set_bounds(bounds));

        // Minimum boundary
        assert_set_bit(&mut segment, Some(lower), true);
        // Lower out of bounds
        assert_invalid_set_bit(
            &mut segment,
            Some(lower - 1),
            BitmapError::BitIndexOutOfBounds {
                bit_index: lower - 1,
            },
        );
        // Maximum boundary
        assert_set_bit(&mut segment, Some(upper), true);
        // Upper out of bounds
        assert_invalid_set_bit(
            &mut segment,
            Some(upper + 1),
            BitmapError::BitIndexOutOfBounds {
                bit_index: upper + 1,
            },
        );
    }

    /// Covers set_bits and set_region_bits
    #[test]
    fn test_segment_set_bits() {
        let segment_memory: usize = Word::EMPTY;
        let ptr = &segment_memory as *const usize as *mut usize;
        let mut segment = unsafe { BitmapSegment::new(ptr, 0, Word::SIZE_BITS) }.unwrap();

        // Verify we can clear all region bits in the region without bounds set
        let expected = Word::FULL;
        assert_set_bits(&mut segment, None, expected);

        // Reset word value to be empty
        unsafe {
            *segment.pointer = Word::EMPTY;
        }

        // Verify we can set a subset of the region alone
        let start_offset = 24;
        let bounds = BitmapSegmentBounds::new(start_offset, Word::SIZE_BITS - 1).unwrap();
        let expected = Word::FULL & !((1 << start_offset) - 1);
        assert_set_bits(&mut segment, Some(bounds), expected);

        // Verify setting bits fails if it is not included within the bit segment bounds.
        let length = Word::SIZE_BITS - start_offset + 10;
        let bounds = BitmapSegmentBounds::new(start_offset, start_offset + length).unwrap();
        let expected = BitmapError::BitRegionOutOfBounds {
            provided: bounds,
            expected: segment.bounds,
        };
        assert_invalid_set_bits(&mut segment, Some(bounds), expected);

        // Reset word value to be empty
        unsafe {
            *segment.pointer = Word::EMPTY;
        }

        // Set new bounds
        let start_index = 0;
        let length = 20;
        let bounds = BitmapSegmentBounds::new(start_index, start_index + length - 1).unwrap();
        segment.set_bounds(bounds);

        // Verify setting region bits only clears target region
        let expected = 0xFFFFF;
        assert_set_bits(&mut segment, None, expected);

        // Verify trying to set out of bounds fails.
        let start_index = Word::SIZE_BITS;
        let bounds = BitmapSegmentBounds::new(start_index, start_index + length).unwrap();
        let expected = BitmapError::BitRegionOutOfBounds {
            provided: bounds,
            expected: segment.bounds,
        };
        assert_invalid_set_bits(&mut segment, Some(bounds), expected);

        // Verify setting bits succeeds if all conditions are met

        // Reset word value to be full
        unsafe {
            *segment.pointer = Word::EMPTY;
        }

        // Set first 4 bits only
        let start_index = 0;
        let length = 4;
        let bounds = BitmapSegmentBounds::new(start_index, start_index + length - 1).unwrap();
        let expected = 0xF;
        assert_set_bits(&mut segment, Some(bounds), expected);
    }

    /// Covers clear_bit and clear_specified_bit.
    #[test]
    fn test_segment_clear_bit() {
        let segment_memory: usize = Word::FULL;
        let ptr = &segment_memory as *const usize as *mut usize;
        let mut segment = unsafe { BitmapSegment::new(ptr, 0, Word::SIZE_BITS) }.unwrap();

        // Verify clearing a set bit works
        assert_get_bit(&segment, None, BitStatus::Set);
        assert_clear_bit(&mut segment, None, true);
        assert_get_bit(&segment, None, BitStatus::Free);

        // Verify we can clear every bit within the region and see its state change
        unsafe {
            *segment.pointer = Word::FULL;
        }
        for i in 0..Word::SIZE_BITS {
            assert_get_bit(&segment, Some(i), BitStatus::Set);
            assert_clear_bit(&mut segment, Some(i), true);
            assert_get_bit(&segment, Some(i), BitStatus::Free);
        }

        // Verify error result when retrieving a bit when none is set for the struct's bit_index field
        segment.bit_index = None;
        assert_invalid_clear_bit(&mut segment, None, BitmapError::NoBitIndex);

        // Verify bounds work as expected
        let lower = 1;
        let upper = Word::SIZE_BITS - 2;
        let bounds = BitmapSegmentBounds::new(lower, upper).unwrap();
        assert_eq!(true, segment.set_bounds(bounds));

        // Minimum boundary
        assert_clear_bit(&mut segment, Some(lower), true);
        assert_get_bit(&segment, Some(lower), BitStatus::Free);
        // Lower out of bounds
        assert_invalid_clear_bit(
            &mut segment,
            Some(lower - 1),
            BitmapError::BitIndexOutOfBounds {
                bit_index: lower - 1,
            },
        );
        // Maximum boundary
        assert_clear_bit(&mut segment, Some(upper), true);
        assert_get_bit(&segment, Some(upper), BitStatus::Free);
        // Upper out of bounds
        assert_invalid_clear_bit(
            &mut segment,
            Some(upper + 1),
            BitmapError::BitIndexOutOfBounds {
                bit_index: upper + 1,
            },
        );
    }

    /// Covers clear_bits and clear_region_bits
    #[test]
    fn test_segment_clear_bits() {
        let segment_memory: usize = Word::FULL;
        let ptr = &segment_memory as *const usize as *mut usize;
        let mut segment = unsafe { BitmapSegment::new(ptr, 0, Word::SIZE_BITS) }.unwrap();

        // Verify we can clear all region bits in the region without bounds set
        let expected = Word::EMPTY;
        assert_clear_bits(&mut segment, None, expected);

        // Reset word value to be full
        unsafe {
            *segment.pointer = Word::FULL;
        }

        // Verify we can clear a subset of the region alone
        let bounds = BitmapSegmentBounds::new(24, Word::SIZE_BITS - 1).unwrap();
        let expected = Word::FULL >> (Word::SIZE_BITS - 24);
        assert_clear_bits(&mut segment, Some(bounds), expected);

        // Verify clearing bits fails if it is not included within the bit segment bounds.
        let start_offset = 24;
        let length = Word::SIZE_BITS - start_offset + 10;
        let bounds = BitmapSegmentBounds::new(start_offset, start_offset + length).unwrap();
        let expected = BitmapError::BitRegionOutOfBounds {
            provided: bounds,
            expected: segment.bounds,
        };
        assert_invalid_clear_bits(&mut segment, Some(bounds), expected);

        // Reset word value to be full
        unsafe {
            *segment.pointer = Word::FULL;
        }

        // Set new bounds
        let start_index = 0;
        let length = 20;
        let bounds = BitmapSegmentBounds::new(start_index, start_index + length - 1).unwrap();
        segment.set_bounds(bounds);

        // Verify clearing region bits only clears target region
        let expected = Word::FULL << length;
        assert_clear_bits(&mut segment, None, expected);

        // Verify trying to clear out of bounds fails.
        let start_index = Word::SIZE_BITS;
        let bounds = BitmapSegmentBounds::new(start_index, start_index + length).unwrap();
        let expected = BitmapError::BitRegionOutOfBounds {
            provided: bounds,
            expected: segment.bounds,
        };
        assert_invalid_clear_bits(&mut segment, Some(bounds), expected);

        // Verify clearing bits succeeds if all conditions are met

        // Reset word value to be full
        unsafe {
            *segment.pointer = Word::FULL;
        }

        // Clear first 4 bits only
        let start_index = 0;
        let length = 4;
        let bounds = BitmapSegmentBounds::new(start_index, start_index + length - 1).unwrap();
        let expected = Word::FULL << length;
        assert_clear_bits(&mut segment, Some(bounds), expected);
    }

    /// Covers get_bit and get_specified_bit.
    #[test]
    fn test_segment_get_bit() {
        let segment_memory: usize = 0;
        let ptr = &segment_memory as *const usize as *mut usize;
        let mut segment = unsafe { BitmapSegment::new(ptr, 0, Word::SIZE_BITS) }.unwrap();

        // Verify bits can be retrieved
        assert_get_bit(&segment, None, BitStatus::Free);
        for i in 0..Word::SIZE_BITS {
            assert_get_bit(&segment, Some(i), BitStatus::Free);
        }

        // Verify error result when retrieving a bit when none is set for the struct's bit_index field
        segment.bit_index = None;
        assert_invalid_get_bit(&segment, None, BitmapError::NoBitIndex);

        // Verify bounds work as expected
        let lower = 1;
        let upper = Word::SIZE_BITS - 2;
        let bounds = BitmapSegmentBounds::new(lower, upper).unwrap();
        assert_eq!(true, segment.set_bounds(bounds));

        // Minimum boundary
        assert_get_bit(&mut segment, Some(lower), BitStatus::Free);
        // Lower out of bounds
        assert_invalid_get_bit(
            &mut segment,
            Some(lower - 1),
            BitmapError::BitIndexOutOfBounds {
                bit_index: lower - 1,
            },
        );
        // Maximum boundary
        assert_get_bit(&mut segment, Some(upper), BitStatus::Free);
        // Upper out of bounds
        assert_invalid_get_bit(
            &mut segment,
            Some(upper + 1),
            BitmapError::BitIndexOutOfBounds {
                bit_index: upper + 1,
            },
        );
    }
}
