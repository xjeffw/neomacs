use super::{signal, Flow, Value};

pub(super) fn expect_args(name: &str, args: &[Value], n: usize) -> Result<(), Flow> {
    if args.len() != n {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

pub(super) fn expect_min_max_args(
    name: &str,
    args: &[Value],
    min: usize,
    max: usize,
) -> Result<(), Flow> {
    if args.len() < min || args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

pub(super) fn expect_int(val: &Value) -> Result<i64, Flow> {
    match val {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), other.clone()],
        )),
    }
}

pub(super) fn expect_string(val: &Value) -> Result<String, Flow> {
    match val {
        Value::Str(s) => Ok((**s).clone()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

// ---------------------------------------------------------------------------
// Unicode width helpers
// ---------------------------------------------------------------------------

/// Return the display width of a Unicode code point.
///
/// - ASCII control chars (0-31): width 0
/// - ASCII printable (32-126): width 1
/// - CJK ranges: width 2
/// - Everything else: width 1
pub(super) fn unicode_char_width(cp: u32) -> i64 {
    // ASCII control characters
    if cp <= 31 {
        return 0;
    }
    // ASCII printable
    if cp <= 126 {
        return 1;
    }
    // CJK and fullwidth ranges → width 2
    if is_wide_char(cp) {
        return 2;
    }
    // Default
    1
}

/// Check if a code point falls in a CJK / fullwidth range.
fn is_wide_char(cp: u32) -> bool {
    // Hangul Jamo
    (0x1100..=0x115F).contains(&cp)
    // CJK Radicals Supplement through Yi Radicals
    || (0x2E80..=0xA4CF).contains(&cp)
    // Hangul Syllables
    || (0xAC00..=0xD7AF).contains(&cp)
    // CJK Compatibility Ideographs
    || (0xF900..=0xFAFF).contains(&cp)
    // Vertical forms, CJK Compatibility Forms, Small Form Variants
    || (0xFE10..=0xFE6F).contains(&cp)
    // Fullwidth Forms (but not halfwidth katakana 0xFF61-0xFFDC)
    || (0xFF01..=0xFF60).contains(&cp)
    // Fullwidth sign variants
    || (0xFFE0..=0xFFE6).contains(&cp)
    // CJK Unified Ideographs Extension B and beyond
    || (0x20000..=0x2FFFF).contains(&cp)
    // CJK Unified Ideographs Extension G and beyond
    || (0x30000..=0x3FFFF).contains(&cp)
}

// ---------------------------------------------------------------------------
// Modifier bit constants
// ---------------------------------------------------------------------------

pub(super) const CHAR_META: i64 = 0x8000000; // 27th bit
pub(super) const CHAR_CTL: i64 = 0x4000000; // 26th bit
pub(super) const CHAR_SHIFT: i64 = 0x2000000; // 25th bit
pub(super) const CHAR_HYPER: i64 = 0x1000000; // 24th bit
pub(super) const CHAR_SUPER: i64 = 0x0800000; // 23rd bit
pub(super) const CHAR_ALT: i64 = 0x0400000; // 22nd bit

pub(super) const CHAR_MODIFIER_MASK: i64 =
    CHAR_META | CHAR_CTL | CHAR_SHIFT | CHAR_HYPER | CHAR_SUPER | CHAR_ALT;
