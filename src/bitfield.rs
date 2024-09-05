use paste::paste;

use super::errors::BitfieldError;

const BITS_IN_BYTE: usize = 8;
const BITS_IN_WORD: usize = core::mem::size_of::<usize>() * BITS_IN_BYTE;

macro_rules! ceiling {
    ($value:expr) => {
        if $value > ($value as usize as f32) {
            ($value + 1.0) as usize
        } else {
            $value as usize
        }
    };
}

/// Figure out how many bytes are requires to represent a value
macro_rules! bytes_required {
    ($($bits:expr),*) => {
        ceiling!((0 $(+$bits)*) as f32 / BITS_IN_BYTE as f32)
    };
}

#[macro_export]
macro_rules! bitfield {
    ($name:ident { $(($field:ident, $bits:expr)),* $(,)? }) => {
        /// Figure out how many bits to skip before we get to the target
        macro_rules! shift {
            ($target:ident) => {
                {
                    let mut counter = 0;
                    loop {
                        $(
                            if stringify!($field) == stringify!($target) {
                                break counter;
                            }
                            counter += $bits;
                        )*
                    }
                }
            }
        }


        #[derive(Debug)]
        #[repr(packed)]
        struct $name {
            bytes: [u8; bytes_required!($($bits),*)],
        };

         impl $name {
            pub fn new() -> $name {
                $name {
                    bytes: [0; bytes_required!($($bits),*)],
                }
            }

            #[inline]
            fn byte_index(shift: usize) -> usize {
                shift / BITS_IN_BYTE
            }

            #[inline]
            pub fn size_bytes(&self) -> usize {
                self.bytes.len()
            }
        }

        paste::paste! {
            $(
                if $bits > BITS_IN_WORD {
                    panic!("Items in bitfield > {} bits not supported", BITS_IN_WORD);
                }
                impl $name {
                    pub fn [<get_ $field>](&self) -> usize {
                        let start_bit = shift!($field);
                        let end_bit = start_bit + $bits;
                        let start_offset = start_bit % BITS_IN_BYTE;
                        let end_offset = BITS_IN_BYTE - end_bit % BITS_IN_BYTE;
                        let mut byte_index = Self::byte_index(start_bit);
                        let mut current_bit = start_bit + start_offset;
                        let mut value = 0;

                        while current_bit <= end_bit {
                            value <<= BITS_IN_BYTE;
                            value |= self.bytes[byte_index] as usize;
                            current_bit += BITS_IN_BYTE;
                            byte_index += 1;
                        }
                        value >>= end_offset;

                        value
                    }
                    
                    pub fn [<set_ $field>](&mut self, mut value: usize) -> Result<bool, $crate::errors::BitfieldError> {
                        if value > (2_usize.pow($bits) - 1) {
                            return Err($crate::errors::BitfieldError::ValueTooLarge { value, num_bits: $bits });
                        }

                        let start_bit = shift!($field);
                        let end_bit = start_bit + $bits;
                        let start_offset = start_bit % BITS_IN_BYTE;
                        let end_offset = BITS_IN_BYTE - end_bit % BITS_IN_BYTE;
                        let mut byte_index = Self::byte_index(start_bit);
                        let mut current_bit = start_bit + start_offset;

                        // Update mask as we go
                        // Slow implementation due to using usize for bits
                        let mut mask = (2_usize.pow($bits as u32) - 1) << start_offset;
                        while current_bit <= end_bit {
                            let byte_mask = mask & core::u8::MAX as usize;
                            let byte_value = (value & byte_mask) >> start_offset;

                            self.bytes[byte_index] &= !byte_mask as u8;
                            self.bytes[byte_index] |= byte_value as u8;

                            current_bit += BITS_IN_BYTE;
                            byte_index += 1;
                            mask >>= BITS_IN_BYTE;
                            value >>= BITS_IN_BYTE;
                        }
                         

                        Ok(true)
                    }

       
                }
            )*
        }
    };}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ceiling() {
        assert_eq!(ceiling!(65.0 / 8.0), 9);
        assert_eq!(ceiling!(0.0 / 8.0), 0);
        assert_eq!(ceiling!(8.0 / 8.0), 1);
    }

    #[test]
    fn test_struct_create() {
        bitfield! { Test {} };
        let t = Test::new();
        bitfield! {
            Test2 {
                (a, 64),
            }
        };
        let t = Test2::new();
        assert_eq!(8, t.size_bytes());
        bitfield! {
            Test3 {
                (a, 64),
                (b, 1),
            }
        };
        let t = Test3::new();
        assert_eq!(9, t.size_bytes());
        bitfield! {
            Test4 {
                (a, 0)
            }
        };
        let t = Test4::new();
        assert_eq!(0, t.size_bytes());
    }

    #[test]
    fn test_struct_get() {
        bitfield! {
            Test {
                (a, 32),
                (b, 16),
                (c, 5),
            }
        };

        let mut t = Test::new();
        for i in 0..t.size_bytes() {
            t.bytes[i] = core::u8::MAX ;
        }
        assert_eq!(t.get_a(), core::u32::MAX as usize);
        assert_eq!(t.get_b(), core::u16::MAX as usize);
        assert_eq!(t.get_c(), (core::u8::MAX >> 3) as usize);
    }

    #[test]
    fn test_struct_set() { 
        bitfield! {
            Test {
                (a, 32),
                (b, 16),
                (c, 5),
            }
        };

        let mut t = Test::new();
        // Overwrite behavior gets rid of extra bits
        t.set_a(7).unwrap();
        assert_eq!(t.bytes[0], 7);
        t.set_a(6).unwrap();
        assert_eq!(t.bytes[0], 6);

        // Check writing a value too larger
        let value = 2_usize.pow(5);
        let expected = Err(BitfieldError::ValueTooLarge {
            value: value,
            num_bits: 5,
        });
        assert_eq!(t.set_c(value), expected);
        let expected = [6, 0, 0, 0, 0, 0, 0];
        assert_eq!(expected, t.bytes);

        // Check maximum value
        let value = 2_usize.pow(5) - 1;
        t.set_c(value).unwrap();
        let expected = [6, 0, 0, 0, 0, 0, value as u8];
        assert_eq!(expected, t.bytes);
    }
}
