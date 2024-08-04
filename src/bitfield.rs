use paste::paste;

use super::errors::BitfieldError;

macro_rules! ceiling {
    ($value:expr) => {
        if value > (value as usize as f32) {
            (value + 1.0) as usize
        } else {
            value as usize
        }
    };
}

macro_rules! words_required {
    ($($bits:expr),*) => {
        {
            const num_bits: usize = 0 $(+$bits)* ;
            const word_size: usize = core::mem::size_of::<usize>() * 8;
            const value: f32 = (num_bits as f32) / (word_size as f32);
            ceiling!(value)
        }
    };
}

#[macro_export]
macro_rules! bitfield {
    ($name:ident { $(($field:ident, $bits:expr)),* $(,)? }) => {


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
        struct $name {
            words: [usize; words_required!($($bits),*)],
        };

        impl $name {
            pub fn new() -> $name {
                $name {
                    words: [0; words_required!($($bits),*)],
                }
            }

            #[inline]
            fn word_index(shift: usize) -> usize {
                shift / Self::word_bits()
            }

            #[inline]
            fn word_bits() -> usize {
                core::mem::size_of::<usize>() * 8
            }
        }

        paste::paste! {
            $(
                if $bits > core::mem::size_of::<usize>() * 8 {
                    panic!("Items in bitfield > 64 bits not supported");
                }
                impl $name {
                    pub fn [<get_ $field>](&self) -> usize {
                        let shift = shift!($field);
                        let word_index = Self::word_index(shift);

                        // Beginning bits of the word
                        let mut result = self.words[word_index] >> (shift % Self::word_bits());

                        let bits_from_boundary = (shift % Self::word_bits() + $bits) as isize - Self::word_bits() as isize;

                        if bits_from_boundary > 0 {
                            // Crossed a boundary, get remaining bits
                            let mask = (self.words[word_index + 1] & ((1 << bits_from_boundary) - 1));
                            result |= (mask << ($bits - bits_from_boundary));
                        }

                        result & (std::usize::MAX >> (Self::word_bits() - $bits))
                    }

                    pub fn [<set_ $field>](&mut self, value: usize) -> Result<bool, $crate::errors::BitfieldError> {
                        if value > 2_usize.pow($bits) - 1 {
                            return Err($crate::errors::BitfieldError::ValueTooLarge{ value, num_bits: $bits });
                        }
                        let shift = shift!($field);
                        let word_index = Self::word_index(shift);
                        let local_shift = shift % Self::word_bits();
                        let mask = (!value << local_shift) | ((1 << local_shift) - 1);

                        self.words[word_index] &= mask;
                        self.words[word_index] |= (value << local_shift);

                        let bits_from_boundary = (shift % Self::word_bits() + $bits) as isize - Self::word_bits() as isize;

                        if bits_from_boundary > 0 {
                            // Crossed a boundary, get remaining bits
                            let mask = (self.words[word_index + 1] << (bits_from_boundary as usize));
                            self.words[word_index + 1] &= (mask | (value >> ($bits - bits_from_boundary)));
                        }

                        Ok(true)
                    }
                }
            )*
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

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
        assert_eq!(1, t.words.len());
        bitfield! {
            Test3 {
                (a, 64),
                (b, 1),
            }
        };
        let t = Test3::new();
        assert_eq!(2, t.words.len());
        bitfield! {
            Test4 {
                (a, 0)
            }
        };
        let t = Test4::new();
        assert_eq!(0, t.words.len());
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
        t.words[0] = std::u64::MAX as usize;
        assert_eq!(t.get_a(), std::u32::MAX as usize);
        assert_eq!(t.get_b(), std::u16::MAX as usize);
        assert_eq!(t.get_c(), 0x1F);
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
        t.set_a(6).unwrap();
        assert_eq!(t.words[0], 6);
        t.set_a(7).unwrap();
        assert_eq!(t.words[0], 7);
        let value = 2_usize.pow(5);
        let expected = Err(BitfieldError::ValueTooLarge {
            value: value,
            num_bits: 5,
        });
        assert_eq!(t.set_c(value), expected);
        assert_eq!(t.words[0], 7);
        t.set_c(6).unwrap();
        assert_eq!(t.words[0], (6 << 48) | 7);
    }
}
