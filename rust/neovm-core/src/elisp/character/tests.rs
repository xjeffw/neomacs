use super::*;

// ===== max-char =====

#[test]
fn max_char_no_args() {
    let result = builtin_max_char(vec![]).unwrap();
    assert_eq!(result.as_int(), Some(0x3FFFFF));
}

#[test]
fn max_char_unicode_nil() {
    let result = builtin_max_char(vec![Value::Nil]).unwrap();
    assert_eq!(result.as_int(), Some(0x3FFFFF));
}

#[test]
fn max_char_unicode_t() {
    let result = builtin_max_char(vec![Value::True]).unwrap();
    assert_eq!(result.as_int(), Some(0x10FFFF));
}

#[test]
fn max_char_too_many_args() {
    let result = builtin_max_char(vec![Value::True, Value::Nil]);
    assert!(result.is_err());
}

// ===== characterp =====

#[test]
fn characterp_char() {
    let result = builtin_characterp(vec![Value::Char('A')]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn characterp_valid_int() {
    let result = builtin_characterp(vec![Value::Int(65)]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn characterp_zero() {
    let result = builtin_characterp(vec![Value::Int(0)]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn characterp_max_internal() {
    let result = builtin_characterp(vec![Value::Int(0x3FFFFF)]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn characterp_too_large() {
    let result = builtin_characterp(vec![Value::Int(0x400000)]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn characterp_negative() {
    let result = builtin_characterp(vec![Value::Int(-1)]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn characterp_string() {
    let result = builtin_characterp(vec![Value::string("a")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn characterp_nil() {
    let result = builtin_characterp(vec![Value::Nil]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn characterp_with_ignore() {
    // Second argument should be ignored
    let result = builtin_characterp(vec![Value::Char('B'), Value::True]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn characterp_wrong_arg_count() {
    let result = builtin_characterp(vec![]);
    assert!(result.is_err());
}

// ===== unibyte-char-to-multibyte =====

#[test]
fn unibyte_to_multibyte_char() {
    let result = builtin_unibyte_char_to_multibyte(vec![Value::Char('A')]).unwrap();
    match result {
        Value::Char(c) => assert_eq!(c, 'A'),
        _ => panic!("expected Char"),
    }
}

#[test]
fn unibyte_to_multibyte_int() {
    let result = builtin_unibyte_char_to_multibyte(vec![Value::Int(200)]).unwrap();
    assert_eq!(result.as_int(), Some(200));
}

#[test]
fn unibyte_to_multibyte_wrong_type() {
    let result = builtin_unibyte_char_to_multibyte(vec![Value::string("a")]);
    assert!(result.is_err());
}

#[test]
fn unibyte_to_multibyte_wrong_args() {
    let result = builtin_unibyte_char_to_multibyte(vec![]);
    assert!(result.is_err());
}

// ===== multibyte-char-to-unibyte =====

#[test]
fn multibyte_to_unibyte_char_ascii() {
    let result = builtin_multibyte_char_to_unibyte(vec![Value::Char('A')]).unwrap();
    assert_eq!(result.as_int(), Some(65));
}

#[test]
fn multibyte_to_unibyte_char_high() {
    // Unicode char with high code point — only low byte returned
    let result = builtin_multibyte_char_to_unibyte(vec![Value::Int(0x1234)]).unwrap();
    assert_eq!(result.as_int(), Some(0x34));
}

#[test]
fn multibyte_to_unibyte_zero() {
    let result = builtin_multibyte_char_to_unibyte(vec![Value::Int(0)]).unwrap();
    assert_eq!(result.as_int(), Some(0));
}

#[test]
fn multibyte_to_unibyte_0xff() {
    let result = builtin_multibyte_char_to_unibyte(vec![Value::Int(0xFF)]).unwrap();
    assert_eq!(result.as_int(), Some(0xFF));
}

#[test]
fn multibyte_to_unibyte_wrong_type() {
    let result = builtin_multibyte_char_to_unibyte(vec![Value::Nil]);
    assert!(result.is_err());
}

// ===== char-width =====

#[test]
fn char_width_ascii_printable() {
    let result = builtin_char_width(vec![Value::Char('A')]).unwrap();
    assert_eq!(result.as_int(), Some(1));
}

#[test]
fn char_width_space() {
    let result = builtin_char_width(vec![Value::Char(' ')]).unwrap();
    assert_eq!(result.as_int(), Some(1));
}

#[test]
fn char_width_control_null() {
    let result = builtin_char_width(vec![Value::Int(0)]).unwrap();
    assert_eq!(result.as_int(), Some(0));
}

#[test]
fn char_width_control_tab() {
    let result = builtin_char_width(vec![Value::Int(9)]).unwrap();
    assert_eq!(result.as_int(), Some(0));
}

#[test]
fn char_width_control_newline() {
    let result = builtin_char_width(vec![Value::Int(10)]).unwrap();
    assert_eq!(result.as_int(), Some(0));
}

#[test]
fn char_width_cjk_ideograph() {
    // U+4E2D = Chinese character "zhong" (middle)
    let result = builtin_char_width(vec![Value::Int(0x4E2D)]).unwrap();
    assert_eq!(result.as_int(), Some(2));
}

#[test]
fn char_width_hangul_jamo() {
    // U+1100 = Hangul Choseong Kiyeok
    let result = builtin_char_width(vec![Value::Int(0x1100)]).unwrap();
    assert_eq!(result.as_int(), Some(2));
}

#[test]
fn char_width_hangul_syllable() {
    // U+AC00 = Hangul syllable GA
    let result = builtin_char_width(vec![Value::Int(0xAC00)]).unwrap();
    assert_eq!(result.as_int(), Some(2));
}

#[test]
fn char_width_fullwidth_exclamation() {
    // U+FF01 = Fullwidth exclamation mark
    let result = builtin_char_width(vec![Value::Int(0xFF01)]).unwrap();
    assert_eq!(result.as_int(), Some(2));
}

#[test]
fn char_width_cjk_ext_b() {
    // U+20000 = CJK Unified Ideographs Extension B
    let result = builtin_char_width(vec![Value::Int(0x20000)]).unwrap();
    assert_eq!(result.as_int(), Some(2));
}

#[test]
fn char_width_latin_supplement() {
    // U+00E9 = Latin small letter e with acute (not wide)
    let result = builtin_char_width(vec![Value::Char('\u{00E9}')]).unwrap();
    assert_eq!(result.as_int(), Some(1));
}

#[test]
fn char_width_tilde() {
    // U+007E = tilde (last ASCII printable)
    let result = builtin_char_width(vec![Value::Char('~')]).unwrap();
    assert_eq!(result.as_int(), Some(1));
}

#[test]
fn char_width_del() {
    // U+007F = DEL (not printable, not in 0-31 range)
    let result = builtin_char_width(vec![Value::Int(0x7F)]).unwrap();
    assert_eq!(result.as_int(), Some(1));
}

#[test]
fn char_width_wrong_type() {
    let result = builtin_char_width(vec![Value::string("a")]);
    assert!(result.is_err());
}

#[test]
fn char_width_negative() {
    let result = builtin_char_width(vec![Value::Int(-1)]);
    assert!(result.is_err());
}

// ===== string-width =====

#[test]
fn string_width_ascii() {
    let result = builtin_string_width(vec![Value::string("hello")]).unwrap();
    assert_eq!(result.as_int(), Some(5));
}

#[test]
fn string_width_empty() {
    let result = builtin_string_width(vec![Value::string("")]).unwrap();
    assert_eq!(result.as_int(), Some(0));
}

#[test]
fn string_width_cjk() {
    // Two CJK characters, each width 2
    let result = builtin_string_width(vec![Value::string("\u{4E2D}\u{6587}")]).unwrap();
    assert_eq!(result.as_int(), Some(4));
}

#[test]
fn string_width_mixed() {
    // "A" (1) + U+4E2D (2) = 3
    let result = builtin_string_width(vec![Value::string("A\u{4E2D}")]).unwrap();
    assert_eq!(result.as_int(), Some(3));
}

#[test]
fn string_width_with_from_to() {
    // "hello" from 1 to 3 = "el" = width 2
    let result =
        builtin_string_width(vec![Value::string("hello"), Value::Int(1), Value::Int(3)]).unwrap();
    assert_eq!(result.as_int(), Some(2));
}

#[test]
fn string_width_from_only() {
    // "hello" from 2 to end = "llo" = width 3
    let result = builtin_string_width(vec![Value::string("hello"), Value::Int(2)]).unwrap();
    assert_eq!(result.as_int(), Some(3));
}

#[test]
fn string_width_from_out_of_range() {
    let result = builtin_string_width(vec![Value::string("hi"), Value::Int(10)]);
    assert!(result.is_err());
}

#[test]
fn string_width_to_out_of_range() {
    let result = builtin_string_width(vec![Value::string("hi"), Value::Int(0), Value::Int(10)]);
    assert!(result.is_err());
}

#[test]
fn string_width_to_before_from() {
    let result = builtin_string_width(vec![Value::string("hello"), Value::Int(3), Value::Int(1)]);
    assert!(result.is_err());
}

#[test]
fn string_width_wrong_type() {
    let result = builtin_string_width(vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn string_width_nil_from_uses_zero() {
    // Nil FROM means start from 0
    let result =
        builtin_string_width(vec![Value::string("hello"), Value::Nil, Value::Int(3)]).unwrap();
    assert_eq!(result.as_int(), Some(3));
}

// ===== char-direction =====

#[test]
fn char_direction_ascii() {
    let result = builtin_char_direction(vec![Value::Char('A')]).unwrap();
    assert_eq!(result.as_int(), Some(0)); // LTR
}

#[test]
fn char_direction_hebrew() {
    // U+05D0 = Hebrew Alef
    let result = builtin_char_direction(vec![Value::Int(0x05D0)]).unwrap();
    assert_eq!(result.as_int(), Some(1)); // RTL
}

#[test]
fn char_direction_arabic() {
    // U+0627 = Arabic Alef
    let result = builtin_char_direction(vec![Value::Int(0x0627)]).unwrap();
    assert_eq!(result.as_int(), Some(1)); // RTL
}

#[test]
fn char_direction_arabic_presentation() {
    // U+FB50 = Arabic Presentation Forms-A
    let result = builtin_char_direction(vec![Value::Int(0xFB50)]).unwrap();
    assert_eq!(result.as_int(), Some(1)); // RTL
}

#[test]
fn char_direction_cjk() {
    // CJK is LTR
    let result = builtin_char_direction(vec![Value::Int(0x4E2D)]).unwrap();
    assert_eq!(result.as_int(), Some(0)); // LTR
}

#[test]
fn char_direction_wrong_type() {
    let result = builtin_char_direction(vec![Value::string("x")]);
    assert!(result.is_err());
}

#[test]
fn char_direction_negative() {
    let result = builtin_char_direction(vec![Value::Int(-1)]);
    assert!(result.is_err());
}

// ===== chars-in-region =====

#[test]
fn chars_in_region_basic() {
    let result = builtin_chars_in_region(vec![Value::Int(1), Value::Int(10)]).unwrap();
    assert_eq!(result.as_int(), Some(9));
}

#[test]
fn chars_in_region_same() {
    let result = builtin_chars_in_region(vec![Value::Int(5), Value::Int(5)]).unwrap();
    assert_eq!(result.as_int(), Some(0));
}

#[test]
fn chars_in_region_reversed() {
    // abs(10 - 1) = 9
    let result = builtin_chars_in_region(vec![Value::Int(10), Value::Int(1)]).unwrap();
    assert_eq!(result.as_int(), Some(9));
}

#[test]
fn chars_in_region_wrong_args() {
    let result = builtin_chars_in_region(vec![Value::Int(1)]);
    assert!(result.is_err());
}

#[test]
fn chars_in_region_wrong_type() {
    let result = builtin_chars_in_region(vec![Value::string("a"), Value::Int(1)]);
    assert!(result.is_err());
}

// ===== string-bytes =====

#[test]
fn string_bytes_ascii() {
    let result = builtin_string_bytes(vec![Value::string("hello")]).unwrap();
    assert_eq!(result.as_int(), Some(5));
}

#[test]
fn string_bytes_empty() {
    let result = builtin_string_bytes(vec![Value::string("")]).unwrap();
    assert_eq!(result.as_int(), Some(0));
}

#[test]
fn string_bytes_multibyte() {
    // U+4E2D is 3 bytes in UTF-8
    let result = builtin_string_bytes(vec![Value::string("\u{4E2D}")]).unwrap();
    assert_eq!(result.as_int(), Some(3));
}

#[test]
fn string_bytes_mixed() {
    // "A" (1 byte) + U+00E9 (2 bytes) + U+4E2D (3 bytes) = 6 bytes
    let result = builtin_string_bytes(vec![Value::string("A\u{00E9}\u{4E2D}")]).unwrap();
    assert_eq!(result.as_int(), Some(6));
}

#[test]
fn string_bytes_wrong_type() {
    let result = builtin_string_bytes(vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn string_bytes_wrong_args() {
    let result = builtin_string_bytes(vec![]);
    assert!(result.is_err());
}

// ===== char-resolve-modifiers =====

#[test]
fn resolve_modifiers_no_mods() {
    // Plain 'A' with no modifiers
    let result = builtin_char_resolve_modifiers(vec![Value::Int('A' as i64)]).unwrap();
    assert_eq!(result.as_int(), Some('A' as i64));
}

#[test]
fn resolve_modifiers_shift_lowercase() {
    // Shift + 'a' -> 'A'
    let code = 'a' as i64 | CHAR_SHIFT;
    let result = builtin_char_resolve_modifiers(vec![Value::Int(code)]).unwrap();
    assert_eq!(result.as_int(), Some('A' as i64));
}

#[test]
fn resolve_modifiers_shift_uppercase() {
    // Shift + 'A' -> 'A' with shift bit still set (already uppercase)
    let code = 'A' as i64 | CHAR_SHIFT;
    let result = builtin_char_resolve_modifiers(vec![Value::Int(code)]).unwrap();
    assert_eq!(result.as_int(), Some('A' as i64 | CHAR_SHIFT));
}

#[test]
fn resolve_modifiers_control_a() {
    // Ctrl + 'a' -> 1 (C-a)
    let code = 'a' as i64 | CHAR_CTL;
    let result = builtin_char_resolve_modifiers(vec![Value::Int(code)]).unwrap();
    assert_eq!(result.as_int(), Some(1));
}

#[test]
fn resolve_modifiers_control_upper_a() {
    // Ctrl + 'A' -> 1 (C-A)
    let code = 'A' as i64 | CHAR_CTL;
    let result = builtin_char_resolve_modifiers(vec![Value::Int(code)]).unwrap();
    assert_eq!(result.as_int(), Some(1));
}

#[test]
fn resolve_modifiers_control_at() {
    // Ctrl + '@' -> 0 (C-@, NUL)
    let code = '@' as i64 | CHAR_CTL;
    let result = builtin_char_resolve_modifiers(vec![Value::Int(code)]).unwrap();
    assert_eq!(result.as_int(), Some(0));
}

#[test]
fn resolve_modifiers_control_question() {
    // Ctrl + '?' -> 127 (DEL)
    let code = '?' as i64 | CHAR_CTL;
    let result = builtin_char_resolve_modifiers(vec![Value::Int(code)]).unwrap();
    assert_eq!(result.as_int(), Some(127));
}

#[test]
fn resolve_modifiers_meta_a() {
    // Meta + 'a' -> 'a' | CHAR_META (meta bit preserved)
    let code = 'a' as i64 | CHAR_META;
    let result = builtin_char_resolve_modifiers(vec![Value::Int(code)]).unwrap();
    assert_eq!(result.as_int(), Some('a' as i64 | CHAR_META));
}

#[test]
fn resolve_modifiers_meta_control_a() {
    // Meta + Ctrl + 'a' -> 1 | CHAR_META
    let code = 'a' as i64 | CHAR_META | CHAR_CTL;
    let result = builtin_char_resolve_modifiers(vec![Value::Int(code)]).unwrap();
    assert_eq!(result.as_int(), Some(1 | CHAR_META));
}

#[test]
fn resolve_modifiers_shift_control_a() {
    // Shift + Ctrl + 'a' -> Ctrl resolves on 'A'
    // Shift 'a' -> 'A', then Ctrl 'A' -> 1
    let code = 'a' as i64 | CHAR_SHIFT | CHAR_CTL;
    let result = builtin_char_resolve_modifiers(vec![Value::Int(code)]).unwrap();
    assert_eq!(result.as_int(), Some(1));
}

#[test]
fn resolve_modifiers_hyper_preserved() {
    // Hyper modifier is kept as-is
    let code = 'a' as i64 | CHAR_HYPER;
    let result = builtin_char_resolve_modifiers(vec![Value::Int(code)]).unwrap();
    assert_eq!(result.as_int(), Some('a' as i64 | CHAR_HYPER));
}

#[test]
fn resolve_modifiers_super_preserved() {
    let code = 'a' as i64 | CHAR_SUPER;
    let result = builtin_char_resolve_modifiers(vec![Value::Int(code)]).unwrap();
    assert_eq!(result.as_int(), Some('a' as i64 | CHAR_SUPER));
}

#[test]
fn resolve_modifiers_alt_preserved() {
    let code = 'x' as i64 | CHAR_ALT;
    let result = builtin_char_resolve_modifiers(vec![Value::Int(code)]).unwrap();
    assert_eq!(result.as_int(), Some('x' as i64 | CHAR_ALT));
}

#[test]
fn resolve_modifiers_control_on_non_alpha() {
    // Ctrl + '1' -> '1' with Ctrl bit still set (no resolution)
    let code = '1' as i64 | CHAR_CTL;
    let result = builtin_char_resolve_modifiers(vec![Value::Int(code)]).unwrap();
    assert_eq!(result.as_int(), Some('1' as i64 | CHAR_CTL));
}

#[test]
fn resolve_modifiers_wrong_type() {
    let result = builtin_char_resolve_modifiers(vec![Value::string("a")]);
    assert!(result.is_err());
}

#[test]
fn resolve_modifiers_wrong_args() {
    let result = builtin_char_resolve_modifiers(vec![]);
    assert!(result.is_err());
}

// ===== get-byte =====

#[test]
fn get_byte_no_args() {
    // Stub: returns 0 when no string
    let result = builtin_get_byte(vec![]).unwrap();
    assert_eq!(result.as_int(), Some(0));
}

#[test]
fn get_byte_with_string_pos0() {
    let result = builtin_get_byte(vec![Value::Int(0), Value::string("ABC")]).unwrap();
    assert_eq!(result.as_int(), Some(65)); // 'A'
}

#[test]
fn get_byte_with_string_pos1() {
    let result = builtin_get_byte(vec![Value::Int(1), Value::string("ABC")]).unwrap();
    assert_eq!(result.as_int(), Some(66)); // 'B'
}

#[test]
fn get_byte_with_string_pos2() {
    let result = builtin_get_byte(vec![Value::Int(2), Value::string("ABC")]).unwrap();
    assert_eq!(result.as_int(), Some(67)); // 'C'
}

#[test]
fn get_byte_multibyte_string() {
    // U+00E9 = 0xC3 0xA9 in UTF-8
    let result = builtin_get_byte(vec![Value::Int(0), Value::string("\u{00E9}")]).unwrap();
    assert_eq!(result.as_int(), Some(0xC3));
}

#[test]
fn get_byte_multibyte_string_second_byte() {
    let result = builtin_get_byte(vec![Value::Int(1), Value::string("\u{00E9}")]).unwrap();
    assert_eq!(result.as_int(), Some(0xA9));
}

#[test]
fn get_byte_out_of_range() {
    let result = builtin_get_byte(vec![Value::Int(5), Value::string("hi")]);
    assert!(result.is_err());
}

#[test]
fn get_byte_nil_pos_defaults_to_zero() {
    let result = builtin_get_byte(vec![Value::Nil, Value::string("XY")]).unwrap();
    assert_eq!(result.as_int(), Some(88)); // 'X'
}

#[test]
fn get_byte_too_many_args() {
    let result = builtin_get_byte(vec![Value::Int(0), Value::string("a"), Value::Nil]);
    assert!(result.is_err());
}

// ===== unicode_char_width helper =====

#[test]
fn width_helper_control_chars() {
    for cp in 0..=31u32 {
        assert_eq!(
            unicode_char_width(cp),
            0,
            "control char {} should be width 0",
            cp
        );
    }
}

#[test]
fn width_helper_printable_ascii() {
    for cp in 32..=126u32 {
        assert_eq!(unicode_char_width(cp), 1, "ASCII {} should be width 1", cp);
    }
}

#[test]
fn width_helper_cjk_ranges() {
    // Sample from each CJK range
    assert_eq!(unicode_char_width(0x1100), 2, "Hangul Jamo start");
    assert_eq!(unicode_char_width(0x115F), 2, "Hangul Jamo end");
    assert_eq!(unicode_char_width(0x2E80), 2, "CJK Radicals start");
    assert_eq!(unicode_char_width(0x3000), 2, "CJK Symbols");
    assert_eq!(unicode_char_width(0x4E00), 2, "CJK Unified Ideographs");
    assert_eq!(unicode_char_width(0xA4CF), 2, "Yi Radicals end");
    assert_eq!(unicode_char_width(0xAC00), 2, "Hangul Syllables start");
    assert_eq!(unicode_char_width(0xD7AF), 2, "Hangul Syllables end");
    assert_eq!(unicode_char_width(0xF900), 2, "CJK Compat Ideographs");
    assert_eq!(unicode_char_width(0xFE10), 2, "Vertical Forms");
    assert_eq!(unicode_char_width(0xFF01), 2, "Fullwidth start");
    assert_eq!(unicode_char_width(0xFF60), 2, "Fullwidth end");
    assert_eq!(unicode_char_width(0xFFE0), 2, "Fullwidth signs start");
    assert_eq!(unicode_char_width(0xFFE6), 2, "Fullwidth signs end");
    assert_eq!(unicode_char_width(0x20000), 2, "CJK Ext B start");
    assert_eq!(unicode_char_width(0x2FFFF), 2, "CJK Ext B end");
    assert_eq!(unicode_char_width(0x30000), 2, "CJK Ext G start");
    assert_eq!(unicode_char_width(0x3FFFF), 2, "CJK Ext G end");
}

#[test]
fn width_helper_non_wide_non_ascii() {
    // Latin supplement (not wide)
    assert_eq!(unicode_char_width(0x00E9), 1);
    // Greek (not wide)
    assert_eq!(unicode_char_width(0x03B1), 1);
    // Emoji outside CJK (e.g. U+1F600)
    assert_eq!(unicode_char_width(0x1F600), 1);
}

// ===== is_wide_char helper =====

#[test]
fn is_wide_boundaries() {
    // Just outside ranges should not be wide
    assert!(!is_wide_char(0x10FF)); // before Hangul Jamo
    assert!(!is_wide_char(0x1160)); // after Hangul Jamo
    assert!(!is_wide_char(0x2E7F)); // before CJK Radicals
    assert!(!is_wide_char(0xA4D0)); // after Yi Radicals
    assert!(!is_wide_char(0xABFF)); // before Hangul Syllables
    assert!(!is_wide_char(0xD7B0)); // after Hangul Syllables
    assert!(!is_wide_char(0xFF61)); // halfwidth katakana (not fullwidth)
    assert!(!is_wide_char(0xFFE7)); // after fullwidth signs
}

// ===== modifier constants =====

#[test]
fn modifier_bit_positions() {
    assert_eq!(CHAR_META, 1 << 27);
    assert_eq!(CHAR_CTL, 1 << 26);
    assert_eq!(CHAR_SHIFT, 1 << 25);
    assert_eq!(CHAR_HYPER, 1 << 24);
    assert_eq!(CHAR_SUPER, 1 << 23);
    assert_eq!(CHAR_ALT, 1 << 22);
}

#[test]
fn modifier_mask_covers_all() {
    let all = CHAR_META | CHAR_CTL | CHAR_SHIFT | CHAR_HYPER | CHAR_SUPER | CHAR_ALT;
    assert_eq!(CHAR_MODIFIER_MASK, all);
}
