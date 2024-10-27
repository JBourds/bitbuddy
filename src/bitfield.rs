use super::errors::BitfieldError;

pub const BITS_IN_BYTE: usize = 8;

#[macro_export]
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
#[macro_export]
macro_rules! bytes_required {
    ($($bits:expr),*) => {
        $crate::ceiling!((0 $(+$bits)*) as f32 / $crate::bitfield::BITS_IN_BYTE as f32)
    };
}

#[macro_export]
macro_rules! bitfield {
    ($name:ident
     $(with $($derive:ident),+ $(,)?)?
     {
         $(($field:ident, $bits:expr)),* $(,)?
     }) => {
        // TODO: Compile time check for number of bits?
        // TODO: Have way to exclude get/set methods for fields with a
        // certain name?


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


        #[derive($($($derive),*)?)]
        #[repr(packed)]
        pub struct $name {
            bytes: [u8; $crate::bytes_required!($($bits),*)],
        }

         impl $name {
            fn __new() -> $name {
                $name {
                    bytes: [0; $crate::bytes_required!($($bits),*)],
                }
            }

            #[inline]
            fn byte_index(shift: usize) -> usize {
                shift / $crate::bitfield::BITS_IN_BYTE
            }

            #[inline]
            fn size_bytes(&self) -> usize {
                self.bytes.len()
            }
        }

        // Auto-implement a debug formatting which displays
        // binary values for each register.
        impl core::fmt::Debug for $name {
            fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                let mut debug_struct = fmt.debug_struct(stringify!($name));
                $crate::paste::paste! {
                    $(
                        debug_struct.field(stringify!($field), &format_args!("0b{:b}", self.[< __get_ $field>]()));
                    )*
                    debug_struct.finish()
                }
            }
        }

        $crate::paste::paste! {
            $(
                impl $name {
                    /// Cap at number of bits in usize
                    #[allow(non_snake_case)]
                    fn [<__get_ $field>](&self) -> usize {
                        if $bits > $crate::BITS_IN_WORD {
                            panic!("Don't support get/set operations on bitfields with more bits than in a word.");
                        } else if $bits <= 0 {
                            panic!("Need a positive, non-negative number of bytes.");
                        }

                        let mut current_bit = shift!($field);
                        let end_bit = current_bit + $bits;

                        let start_offset = current_bit % $crate::bitfield::BITS_IN_BYTE;
                        let end_offset = ($crate::bitfield::BITS_IN_BYTE - end_bit % $crate::BITS_IN_BYTE) & 7;

                        let mut byte_index = Self::byte_index(current_bit);

                        let mut value = self.bytes[byte_index] as usize;
                        current_bit += $crate::bitfield::BITS_IN_BYTE - start_offset;

                        let mut byte_number = 0;
                        while current_bit < end_bit {
                            byte_index += 1;
                            byte_number += 1;
                            value |= ((self.bytes[byte_index] as usize) << (byte_number * $crate::BITS_IN_BYTE));
                            current_bit += $crate::bitfield::BITS_IN_BYTE;
                        }

                        // We know this value will always be between 0 and 31
                        #[allow(arithmetic_overflow)]
                        let mask = core::usize::MAX >> ($crate::BITS_IN_WORD - $bits);
                        value >>= start_offset;
                        value &= mask;

                        value
                    }

                    #[allow(non_snake_case)]
                    fn [<__set_ $field>](&mut self, mut value: usize) -> Result<bool, $crate::errors::BitfieldError> {
                        if value > (2_usize.pow($bits) - 1) {
                            return Err($crate::errors::BitfieldError::ValueTooLarge { value, num_bits: $bits });
                        }

                        let start_bit = shift!($field);
                        let end_bit = start_bit + $bits;
                        let mut start_offset = start_bit % $crate::bitfield::BITS_IN_BYTE;
                        let end_offset = $crate::bitfield::BITS_IN_BYTE - end_bit % $crate::BITS_IN_BYTE;
                        let mut byte_index = Self::byte_index(start_bit);
                        let mut mask = (2_usize.pow($bits as u32) - 1);
                        let mut current_bit = start_bit;
                        while current_bit < end_bit {
                            let byte_value = ((mask as u8) & (value as u8)) << start_offset;
                            self.bytes[byte_index] &= !((mask << start_offset) as u8);
                            self.bytes[byte_index] |= byte_value;

                            current_bit += ($crate::bitfield::BITS_IN_BYTE - start_offset);
                            mask >>= ($crate::bitfield::BITS_IN_BYTE - start_offset);
                            value >>= ($crate::bitfield::BITS_IN_BYTE - start_offset);
                            start_offset = 0;
                            byte_index += 1;
                        }

                        Ok(true)
                    }
                }
            )*
        }
    };}

#[cfg(test)]
mod no_std_tests {
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
        let t = Test::__new();
        bitfield! {
            Test2 {
                (a, 64),
            }
        }
        let t = Test2::__new();
        assert_eq!(8, t.size_bytes());
        bitfield! {
            Test3 {
                (a, 64),
                (b, 1),
            }
        }
        let t = Test3::__new();
        assert_eq!(9, t.size_bytes());
        bitfield! {
            Test4 {
                (a, 0)
            }
        }
        let t = Test4::__new();
        assert_eq!(0, t.size_bytes());

        bitfield! {
            Test5 {
                (reserved, 9)
            }
        }
        let t = Test5::__new();
        assert_eq!(t.size_bytes(), 2);
        t.__get_reserved();
    }

    #[test]
    fn test_struct_get() {
        bitfield! {
            Test {
                (a, 32),
                (b, 16),
                (c, 5),
                (d, 1),
            }
        }

        let mut t = Test::__new();
        for i in 0..t.size_bytes() {
            t.bytes[i] = core::u8::MAX;
        }
        assert_eq!(t.__get_a(), core::u32::MAX as usize);
        assert_eq!(t.__get_b(), core::u16::MAX as usize);
        assert_eq!(t.__get_c(), (core::u8::MAX >> 3) as usize);
        assert_eq!(t.__get_d(), 1);

        t.bytes[3] = 0;
        assert_eq!(t.__get_a(), (core::u32::MAX as usize) & 0x00FFFFFF);
        t.bytes[1] = 0;
        assert_eq!(t.__get_a(), (core::u32::MAX as usize) & 0x00FF00FF);
        t.bytes[3] = 5;
        assert_eq!(t.__get_a(), (core::u32::MAX as usize) & 0x05FF00FF);
    }

    #[test]
    fn test_struct_set() {
        bitfield! {
            Test {
                (a, 32),
                (b, 16),
                (c, 5),
                (d, 1),
            }
        }

        let mut t = Test::__new();
        assert_eq!(7, t.size_bytes());
        // Overwrite behavior gets rid of extra bits
        t.__set_a(7).unwrap();
        assert_eq!(t.bytes[0], 7);
        t.__set_a(6).unwrap();
        assert_eq!(t.bytes[0], 6);

        // Check writing a value too larger
        let value = 2_usize.pow(5);
        let expected = Err(BitfieldError::ValueTooLarge {
            value: value,
            num_bits: 5,
        });
        assert_eq!(t.__set_c(value), expected);
        let expected = [6, 0, 0, 0, 0, 0, 0];
        assert_eq!(expected, t.bytes);

        // Check maximum value
        let value = 2_usize.pow(5) - 1;
        t.__set_c(value).unwrap();
        let expected = [6, 0, 0, 0, 0, 0, value as u8];
        assert_eq!(expected, t.bytes);

        // Verify setting a single bit works
        assert_eq!(t.bytes[6], value as u8);
        t.__set_d(1).unwrap();
        assert_eq!(t.bytes[6], (1 << 5) | value as u8);
        t.__set_d(0).unwrap();
        assert_eq!(t.bytes[6], value as u8);
    }

    #[test]
    fn test_get_set_uneven() {
        // Test setting when a struct does not have even increment bits
        bitfield! {
            Uneven
            with Default {
                (a, 3),
                (b, 7),
                (c, 5),
            }
        }

        let mut t = Uneven::default();
        t.__set_a(0b101).unwrap();
        t.__set_b(0b1100101).unwrap();
        t.__set_c(0b10111).unwrap();
        assert_eq!(0b101, t.__get_a());
        assert_eq!(0b1100101, t.__get_b());
        assert_eq!(0b10111, t.__get_c());
    }

    #[test]
    fn test_mock_example() {
        bitfield! {
            ConfigAddress {
                (register_offset, 8),  // Offset into a 256 byte configuration space
                (function_number, 3),
                (device_number, 5),
                (bus_number, 8),
                (reserved, 7),
                (enable, 1),
            }
        }
        impl ConfigAddress {
            pub fn masked_new(
                bus: u8,
                slot: u8,
                func: u8,
                offset: u8,
            ) -> Result<Self, crate::errors::BitfieldError> {
                Self::checked_new(offset, func & 0xE, func & 0xF8, bus, true)
            }

            pub fn checked_new(
                register_offset: u8,
                func: u8,
                device: u8,
                bus: u8,
                enable: bool,
            ) -> Result<Self, crate::errors::BitfieldError> {
                let mut obj = Self::__new();
                obj.__set_register_offset(register_offset.into())?;
                obj.__set_function_number(func.into())?;
                obj.__set_device_number(device.into())?;
                obj.__set_bus_number(bus.into())?;
                obj.__set_enable(if enable { 1 } else { 0 })?;
                Ok(obj)
            }

            /// Bitwise operation either selects first or second word from the 32-bit
            /// value provided by it.
            #[inline]
            pub fn __get_off__set_word(&self, value: u32) -> u16 {
                ((value >> ((self.__get_register_offset() & 2) << 3)) & 0xFFFF) as u16
            }

            #[inline]
            pub fn get(&self) -> u32 {
                unsafe { *(self.bytes.as_ptr() as *const u32) }
            }
        }
        let config = ConfigAddress::checked_new(0x0A, 3, 5, 2, true).unwrap();
        let expected = (1 << 31) | 0x0A | (3 << 8) | (5 << 11) | (2 << 16);
        assert_eq!(expected, config.get());
        assert_eq!(0x0A, config.__get_register_offset());
        assert_eq!(3, config.__get_function_number());
        assert_eq!(5, config.__get_device_number());
        assert_eq!(2, config.__get_bus_number());
        assert_eq!(0, config.__get_reserved());
        assert_eq!(1, config.__get_enable());
    }

    #[test]
    fn test_mock_example_2() {
        bitfield! {
            PageTableEntry
            with Clone, Copy, PartialEq {
                (present, 1),
                (rw, 1),
                (user, 1),
                (write_through, 1),
                (cache_disable, 1),
                (accessed, 1),
                (dirty, 1),
                (page_attribute, 1),
                (global, 1),
                (available, 3), // Unused by processor
                (frame, 20),
            }
        }

        impl PageTableEntry {
            fn set_frame(&mut self, frame_addr: u32) {
                self.__set_frame((frame_addr >> 12) as usize).unwrap();
            }
            fn get_frame(&self) -> u32 {
                (self.__get_frame() << 12) as u32
            }

            fn is_present(&self) -> bool {
                self.__get_present() == 1
            }
            fn set_present(&mut self, present: bool) {
                self.__set_present(if present { 1 } else { 0 }).unwrap();
            }

            fn set_writable(&mut self, writable: bool) {
                self.__set_rw(if writable { 1 } else { 0 }).unwrap();
            }
            fn is_writable(&self) -> bool {
                self.__get_rw() == 1
            }

            fn set_user(&mut self, user: bool) {
                self.__set_user(if user { 1 } else { 0 }).unwrap();
            }
            fn is_user(&self) -> bool {
                self.__get_user() == 1
            }

            fn set_write_through(&mut self, write_through: bool) {
                self.__set_write_through(if write_through { 1 } else { 0 })
                    .unwrap();
            }
            fn is_write_through(&self) -> bool {
                self.__get_write_through() == 1
            }

            fn set_cacheable(&mut self, cacheable: bool) {
                self.__set_cache_disable(if cacheable { 0 } else { 1 })
                    .unwrap();
            }
            fn is_cacheable(&self) -> bool {
                self.__get_cache_disable() == 1
            }

            fn set_accessed(&mut self, accessed: bool) {
                self.__set_accessed(if accessed { 1 } else { 0 }).unwrap();
            }
            fn is_accessed(&self) -> bool {
                self.__get_accessed() == 1
            }
        }

        impl AsRef<u32> for PageTableEntry {
            fn as_ref(&self) -> &u32 {
                let ptr = self.bytes.as_ptr() as *const u32;
                unsafe { &*ptr }
            }
        }

        impl From<u32> for PageTableEntry {
            fn from(value: u32) -> Self {
                Self {
                    bytes: value.to_le_bytes(),
                }
            }
        }

        impl Default for PageTableEntry {
            fn default() -> Self {
                let mut obj = Self::__new();
                obj.set_writable(true);
                obj.set_present(true);
                obj
            }
        }

        impl PageTableEntry {
            fn gframe(&self) -> u32 {
                (self.__get_frame()) as u32
            }
            pub fn set_dirty(&mut self, dirty: bool) {
                self.__set_dirty(if dirty { 1 } else { 0 }).unwrap();
            }
            pub fn is_dirty(&self) -> bool {
                self.__get_dirty() == 1
            }

            pub fn set_page_attribute(&mut self, page_attribute: bool) {
                self.__set_page_attribute(if page_attribute { 1 } else { 0 })
                    .unwrap();
            }
            pub fn is_page_attribute(&self) -> bool {
                self.__get_page_attribute() == 1
            }

            pub fn set_global(&mut self, global: bool) {
                self.__set_global(if global { 1 } else { 0 }).unwrap();
            }
            pub fn is_global(&self) -> bool {
                self.__get_global() == 1
            }
        }

        let address = 0x200000;
        let mut entry = PageTableEntry::default();
        assert_eq!(*entry.as_ref(), 0x3);
        entry.set_frame(address);
        assert_eq!(*entry.as_ref(), 0x200003);
        assert_eq!(address, entry.get_frame());
    }

    #[test]
    fn test_derive() {
        bitfield! {
            Printable
            with Default, Clone {
                (field1, 4),
                (field2, 4),
            }
        }

        let t = Printable::default();
        let cloned = t.clone();
    }
}

#[cfg(test)]
mod std_tests {
    use super::*;

    // FIXME: Figure out how to make this test run in a std environment
    // while others run in no_std
    //#[test]
    //fn test_debug_print() {
    //    bitfield! {
    //        B
    //        with Default {
    //            (field1, 4),
    //            (field2, 6),
    //            (field3, 2),
    //        }
    //    }
    //    let mut b = B::default();
    //    b.__set_field1(0b1010).unwrap();
    //    b.__set_field2(0b110011).unwrap();
    //    b.__set_field3(0b11).unwrap();
    //    println!("{:#?}", b);
    //}
}
