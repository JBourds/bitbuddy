/// math.rs
///
/// File containing math utility functions.
///
/// Author: Jordan Bourdeau
/// Last Modified: 2/27/24

#[inline]
pub fn ceiling(value: f32) -> usize {
    if value > (value as usize as f32) {
        (value + 1.0) as usize
    } else {
        value as usize
    }
}

#[inline]
pub fn max(left: usize, right: usize) -> usize {
    if left >= right {
        left
    } else {
        right
    }
}

#[inline]
pub fn min(left: usize, right: usize) -> usize {
    if left <= right {
        left
    } else {
        right
    }
}
