use super::helpers::{
    expect_args, expect_int, expect_min_max_args, expect_string, CHAR_CTL, CHAR_MODIFIER_MASK,
    CHAR_SHIFT,
};
use super::{signal, unicode_char_width, EvalResult, Value};

/// `(max-char &optional UNICODE)` -- return the maximum character code.
///
/// If UNICODE is non-nil, return 0x10FFFF (max Unicode scalar value).
/// Otherwise return 0x3FFFFF (max internal character code including
/// raw bytes).
pub(crate) fn builtin_max_char(args: Vec<Value>) -> EvalResult {
    expect_min_max_args("max-char", &args, 0, 1)?;
    let unicode = args.first().map_or(false, |v| v.is_truthy());
    if unicode {
        Ok(Value::Int(0x10FFFF))
    } else {
        Ok(Value::Int(0x3FFFFF))
    }
}

/// `(characterp OBJECT &optional IGNORE)` -- return t if OBJECT is a character.
///
/// A character is either a `Value::Char` or an integer in the valid
/// character range (0..=0x3FFFFF).
pub(crate) fn builtin_characterp(args: Vec<Value>) -> EvalResult {
    expect_min_max_args("characterp", &args, 1, 2)?;
    let is_char = match &args[0] {
        Value::Char(_) => true,
        Value::Int(n) => *n >= 0 && *n <= 0x3FFFFF,
        _ => false,
    };
    Ok(Value::bool(is_char))
}

/// `(unibyte-char-to-multibyte CHAR)` -- convert a unibyte char to multibyte.
///
/// In our VM all strings are UTF-8 (multibyte), so this is the identity
/// for valid characters.
pub(crate) fn builtin_unibyte_char_to_multibyte(args: Vec<Value>) -> EvalResult {
    expect_args("unibyte-char-to-multibyte", &args, 1)?;
    match &args[0] {
        Value::Char(_) | Value::Int(_) => Ok(args[0].clone()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), other.clone()],
        )),
    }
}

/// `(multibyte-char-to-unibyte CHAR)` -- convert a multibyte char to unibyte.
///
/// Returns CHAR & 0xFF (the low byte of the character code).
pub(crate) fn builtin_multibyte_char_to_unibyte(args: Vec<Value>) -> EvalResult {
    expect_args("multibyte-char-to-unibyte", &args, 1)?;
    let code = match &args[0] {
        Value::Char(c) => *c as i64,
        Value::Int(n) => *n,
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("characterp"), other.clone()],
            ));
        }
    };
    Ok(Value::Int(code & 0xFF))
}

/// `(char-width CHAR)` -- return the display width of CHAR.
///
/// Uses basic Unicode width detection: CJK characters are width 2,
/// ASCII control characters are width 0, everything else is width 1.
pub(crate) fn builtin_char_width(args: Vec<Value>) -> EvalResult {
    expect_args("char-width", &args, 1)?;
    let cp = match &args[0] {
        Value::Char(c) => *c as u32,
        Value::Int(n) => {
            if *n < 0 {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("characterp"), args[0].clone()],
                ));
            }
            *n as u32
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("characterp"), other.clone()],
            ));
        }
    };
    Ok(Value::Int(unicode_char_width(cp)))
}

/// `(string-width STRING &optional FROM TO)` -- return the display width of STRING.
///
/// Sums `char-width` for each character in the string (or the substring
/// from FROM to TO, 0-indexed character positions).
pub(crate) fn builtin_string_width(args: Vec<Value>) -> EvalResult {
    expect_min_max_args("string-width", &args, 1, 3)?;
    let s = expect_string(&args[0])?;
    let chars: Vec<char> = s.chars().collect();
    let len = chars.len();

    let from = if args.len() > 1 && args[1].is_truthy() {
        let f = expect_int(&args[1])? as usize;
        if f > len {
            return Err(signal(
                "args-out-of-range",
                vec![args[0].clone(), args[1].clone()],
            ));
        }
        f
    } else {
        0
    };

    let to = if args.len() > 2 && args[2].is_truthy() {
        let t = expect_int(&args[2])? as usize;
        if t > len || t < from {
            return Err(signal(
                "args-out-of-range",
                vec![args[0].clone(), args[2].clone()],
            ));
        }
        t
    } else {
        len
    };

    let width: i64 = chars[from..to]
        .iter()
        .map(|c| unicode_char_width(*c as u32))
        .sum();
    Ok(Value::Int(width))
}

/// `(char-direction CHAR)` -- return the direction of CHAR.
///
/// Returns 0 for left-to-right, 1 for right-to-left.
/// Handles basic bidi by checking for Arabic and Hebrew code point ranges.
pub(crate) fn builtin_char_direction(args: Vec<Value>) -> EvalResult {
    expect_args("char-direction", &args, 1)?;
    let cp = match &args[0] {
        Value::Char(c) => *c as u32,
        Value::Int(n) => {
            if *n < 0 {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("characterp"), args[0].clone()],
                ));
            }
            *n as u32
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("characterp"), other.clone()],
            ));
        }
    };

    // Right-to-left ranges: Arabic, Hebrew, and related blocks.
    let rtl = (0x0590..=0x05FF).contains(&cp)   // Hebrew
        || (0x0600..=0x06FF).contains(&cp)       // Arabic
        || (0x0700..=0x074F).contains(&cp)       // Syriac
        || (0x0780..=0x07BF).contains(&cp)       // Thaana
        || (0x07C0..=0x07FF).contains(&cp)       // NKo
        || (0x0800..=0x083F).contains(&cp)       // Samaritan
        || (0x0840..=0x085F).contains(&cp)       // Mandaic
        || (0xFB50..=0xFDFF).contains(&cp)       // Arabic Presentation Forms-A
        || (0xFE70..=0xFEFF).contains(&cp)       // Arabic Presentation Forms-B
        || (0x10800..=0x10FFF).contains(&cp); // Various RTL historic scripts

    Ok(Value::Int(if rtl { 1 } else { 0 }))
}

/// `(chars-in-region BEG END)` -- return the number of characters between BEG and END.
///
/// In our VM characters are Unicode scalar values (code points), so
/// this is simply END - BEG.
pub(crate) fn builtin_chars_in_region(args: Vec<Value>) -> EvalResult {
    expect_args("chars-in-region", &args, 2)?;
    let beg = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    Ok(Value::Int((end - beg).abs()))
}

/// `(string-bytes STRING)` -- return the number of bytes in STRING's UTF-8
/// representation.
pub(crate) fn builtin_string_bytes(args: Vec<Value>) -> EvalResult {
    expect_args("string-bytes", &args, 1)?;
    let s = expect_string(&args[0])?;
    Ok(Value::Int(s.len() as i64))
}

/// `(char-resolve-modifiers CHAR)` -- resolve modifier bits in CHAR.
///
/// Handles the following modifier bits:
/// - Meta (0x8000000): kept as-is (OR'd with base char)
/// - Control (0x4000000): convert to control character when applicable
/// - Shift (0x2000000): upcase alphabetic characters
///
/// Other modifier bits (hyper, super, alt) are kept as-is.
pub(crate) fn builtin_char_resolve_modifiers(args: Vec<Value>) -> EvalResult {
    expect_args("char-resolve-modifiers", &args, 1)?;
    let code = expect_int(&args[0])?;

    let modifiers = code & CHAR_MODIFIER_MASK;
    let mut base = code & !CHAR_MODIFIER_MASK;
    let mut remaining_mods = modifiers;

    // Resolve Shift: upcase lowercase ASCII letters.
    if remaining_mods & CHAR_SHIFT != 0 {
        if base >= 'a' as i64 && base <= 'z' as i64 {
            base = base - 'a' as i64 + 'A' as i64;
            remaining_mods &= !CHAR_SHIFT;
        }
    }

    // Resolve Control: convert to control character.
    if remaining_mods & CHAR_CTL != 0 {
        if base >= '@' as i64 && base <= '_' as i64 {
            // '@' (64) -> 0, 'A' (65) -> 1, ... '_' (95) -> 31
            base &= 0x1F;
            remaining_mods &= !CHAR_CTL;
        } else if base >= 'a' as i64 && base <= 'z' as i64 {
            // Lowercase also converted: 'a' -> 1, etc.
            base &= 0x1F;
            remaining_mods &= !CHAR_CTL;
        } else if base == '?' as i64 {
            // C-? is DEL (127)
            base = 127;
            remaining_mods &= !CHAR_CTL;
        }
    }

    Ok(Value::Int(base | remaining_mods))
}

/// `(get-byte &optional POS STRING)` -- get byte value at position.
///
/// When STRING is provided, return the byte at position POS (0-indexed)
/// in the UTF-8 encoding of STRING.  When STRING is omitted, this is a
/// stub returning 0 (would need buffer access for full implementation).
///
/// POS defaults to 0 when omitted.
pub(crate) fn builtin_get_byte(args: Vec<Value>) -> EvalResult {
    expect_min_max_args("get-byte", &args, 0, 2)?;

    let pos = if !args.is_empty() && args[0].is_truthy() {
        expect_int(&args[0])? as usize
    } else {
        0
    };

    if args.len() >= 2 {
        let s = expect_string(&args[1])?;
        let bytes = s.as_bytes();
        if pos >= bytes.len() {
            return Err(signal(
                "args-out-of-range",
                vec![args[1].clone(), Value::Int(pos as i64)],
            ));
        }
        Ok(Value::Int(bytes[pos] as i64))
    } else {
        // Stub: without buffer context, return 0.
        Ok(Value::Int(0))
    }
}
