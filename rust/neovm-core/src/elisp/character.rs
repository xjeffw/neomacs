//! Character operation builtins from Emacs character.c.
//!
//! Provides:
//! - `max-char` -- maximum character code
//! - `characterp` -- check if value is a character
//! - `unibyte-char-to-multibyte` -- identity conversion
//! - `multibyte-char-to-unibyte` -- return CHAR & 0xFF
//! - `char-width` -- display width of a character
//! - `string-width` -- sum of char-width for all chars in a string
//! - `char-direction` -- left-to-right or right-to-left
//! - `chars-in-region` -- count characters between two positions
//! - `string-bytes` -- byte length of UTF-8 string
//! - `char-resolve-modifiers` -- resolve modifier bits into base character
//! - `get-byte` -- get byte value at position

use super::error::{signal, EvalResult, Flow};
use super::value::*;

#[path = "character/builtins.rs"]
mod builtins;
#[path = "character/helpers.rs"]
mod helpers;

use helpers::{
    unicode_char_width, CHAR_ALT, CHAR_CTL, CHAR_HYPER, CHAR_META, CHAR_MODIFIER_MASK, CHAR_SHIFT,
    CHAR_SUPER,
};

pub(crate) use builtins::*;

#[cfg(test)]
#[path = "character/tests.rs"]
mod tests;
