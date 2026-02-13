use super::args::{expect_args, expect_min_args, expect_range_args, expect_string};
use super::{EvalResult, Value};

/// `(internal-make-lisp-face FACE &optional FRAME)` -> return FACE.
///
/// In real Emacs this allocates a face in the face cache. Stub returns FACE
/// unchanged.
pub(crate) fn builtin_internal_make_lisp_face(args: Vec<Value>) -> EvalResult {
    expect_range_args("internal-make-lisp-face", &args, 1, 2)?;
    Ok(args[0].clone())
}

/// `(internal-get-lisp-face-attribute FACE ATTR &optional FRAME)` -> unspecified.
///
/// Returns the `unspecified` symbol, indicating the attribute has not been set.
pub(crate) fn builtin_internal_get_lisp_face_attribute(args: Vec<Value>) -> EvalResult {
    expect_range_args("internal-get-lisp-face-attribute", &args, 2, 3)?;
    Ok(Value::symbol("unspecified"))
}

/// `(internal-set-lisp-face-attribute FACE ATTR VALUE &optional FRAME)` -> VALUE.
///
/// Stub: records nothing, returns VALUE.
pub(crate) fn builtin_internal_set_lisp_face_attribute(args: Vec<Value>) -> EvalResult {
    expect_range_args("internal-set-lisp-face-attribute", &args, 3, 4)?;
    Ok(args[2].clone())
}

/// `(internal-lisp-face-equal-p FACE1 FACE2 &optional FRAME)` -> t.
///
/// Stub: all faces are considered equal (no attribute storage).
pub(crate) fn builtin_internal_lisp_face_equal_p(args: Vec<Value>) -> EvalResult {
    expect_range_args("internal-lisp-face-equal-p", &args, 2, 3)?;
    Ok(Value::True)
}

/// `(internal-lisp-face-empty-p FACE &optional FRAME)` -> t.
///
/// Stub: all faces are considered empty (no attributes set).
pub(crate) fn builtin_internal_lisp_face_empty_p(args: Vec<Value>) -> EvalResult {
    expect_range_args("internal-lisp-face-empty-p", &args, 1, 2)?;
    Ok(Value::True)
}

/// `(internal-lisp-face-p FACE)` -> t if FACE is a symbol, nil otherwise.
///
/// In real Emacs, checks the face registry.  Stub checks if FACE is a symbol.
pub(crate) fn builtin_internal_lisp_face_p(args: Vec<Value>) -> EvalResult {
    expect_args("internal-lisp-face-p", &args, 1)?;
    match &args[0] {
        Value::Symbol(_) => Ok(Value::True),
        Value::True => Ok(Value::True),
        _ => Ok(Value::Nil),
    }
}

/// `(internal-copy-lisp-face FROM TO &optional FRAME NEW-FRAME)` -> TO.
///
/// Stub: returns TO unchanged.
pub(crate) fn builtin_internal_copy_lisp_face(args: Vec<Value>) -> EvalResult {
    expect_range_args("internal-copy-lisp-face", &args, 2, 4)?;
    Ok(args[1].clone())
}

/// `(internal-merge-in-global-face FACE LFACE &optional FRAME)` -> LFACE.
///
/// Stub: returns LFACE unchanged.
pub(crate) fn builtin_internal_merge_in_global_face(args: Vec<Value>) -> EvalResult {
    expect_range_args("internal-merge-in-global-face", &args, 2, 3)?;
    Ok(args[1].clone())
}

/// `(face-font FACE &optional FRAME CHARACTER)` -> nil.
///
/// Returns the font name used for FACE.  Stub returns nil.
pub(crate) fn builtin_face_font(args: Vec<Value>) -> EvalResult {
    expect_range_args("face-font", &args, 1, 3)?;
    Ok(Value::Nil)
}

/// `(face-attribute-relative-p ATTRIBUTE VALUE)` -> nil.
///
/// Returns t if VALUE is a relative face attribute value.  Stub returns nil.
pub(crate) fn builtin_face_attribute_relative_p(args: Vec<Value>) -> EvalResult {
    expect_args("face-attribute-relative-p", &args, 2)?;
    Ok(Value::Nil)
}

/// `(face-attributes-as-vector PLIST)` -> vector.
///
/// Converts a face attribute plist to a vector.  Collects plist values into a
/// vector (skipping keys).  If the input is not a valid plist, returns an
/// empty vector.
pub(crate) fn builtin_face_attributes_as_vector(args: Vec<Value>) -> EvalResult {
    expect_args("face-attributes-as-vector", &args, 1)?;
    let plist = &args[0];
    let items = match list_to_vec(plist) {
        Some(v) => v,
        None => {
            if plist.is_nil() {
                return Ok(Value::vector(vec![]));
            }
            return Ok(Value::vector(vec![]));
        }
    };
    // A plist has alternating key-value pairs; collect the values.
    let mut values = Vec::new();
    let mut i = 0;
    while i + 1 < items.len() {
        values.push(items[i + 1].clone());
        i += 2;
    }
    Ok(Value::vector(values))
}

/// `(merge-face-attribute ATTRIBUTE VALUE1 VALUE2)` -> VALUE1.
///
/// Merge two face attribute values.  Stub returns VALUE1.
pub(crate) fn builtin_merge_face_attribute(args: Vec<Value>) -> EvalResult {
    expect_args("merge-face-attribute", &args, 3)?;
    Ok(args[1].clone())
}

/// `(x-family-fonts &optional FAMILY FRAME)` -> nil.
///
/// Return a list of available fonts matching FAMILY.  Stub returns nil.
pub(crate) fn builtin_x_family_fonts(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-family-fonts", &args, 0, 2)?;
    Ok(Value::Nil)
}

/// `(x-list-fonts PATTERN &optional FACE FRAME MAXIMUM WIDTH)` -> nil.
///
/// Return a list of font names matching PATTERN.  Stub returns nil.
pub(crate) fn builtin_x_list_fonts(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-list-fonts", &args, 1, 5)?;
    Ok(Value::Nil)
}

/// `(color-distance COLOR1 COLOR2 &optional FRAME)` -> 0.
///
/// Return an integer distance between two colors.  Stub returns 0.
pub(crate) fn builtin_color_distance(args: Vec<Value>) -> EvalResult {
    expect_range_args("color-distance", &args, 2, 3)?;
    Ok(Value::Int(0))
}

/// `(color-gray-p COLOR &optional FRAME)` -> nil.
///
/// Return t if COLOR is a shade of gray.  Stub returns nil.
pub(crate) fn builtin_color_gray_p(args: Vec<Value>) -> EvalResult {
    expect_range_args("color-gray-p", &args, 1, 2)?;
    Ok(Value::Nil)
}

/// `(color-supported-p COLOR &optional FRAME BACKGROUND-P)` -> t.
///
/// Return t if COLOR can be displayed.  Stub always returns t.
pub(crate) fn builtin_color_supported_p(args: Vec<Value>) -> EvalResult {
    expect_range_args("color-supported-p", &args, 1, 3)?;
    Ok(Value::True)
}

/// `(color-values COLOR &optional FRAME)` -> list of (R G B) or nil.
///
/// Return a list of 16-bit RGB values for COLOR.  Implements basic named
/// color lookup; returns nil for unknown colors.
pub(crate) fn builtin_color_values(args: Vec<Value>) -> EvalResult {
    expect_range_args("color-values", &args, 1, 2)?;
    let color_name = expect_string(&args[0])?;
    let lower = color_name.to_ascii_lowercase();

    // Parse "#RRGGBB" hex colors.
    if lower.starts_with('#') {
        let hex = &lower[1..];
        let (r, g, b) = match hex.len() {
            // #RGB -> expand each digit to 16-bit
            3 => {
                let r = u16::from_str_radix(&hex[0..1], 16);
                let g = u16::from_str_radix(&hex[1..2], 16);
                let b = u16::from_str_radix(&hex[2..3], 16);
                match (r, g, b) {
                    (Ok(r), Ok(g), Ok(b)) => {
                        // 0xF -> 0xFFFF (multiply by 0x1111)
                        (r * 0x1111, g * 0x1111, b * 0x1111)
                    }
                    _ => return Ok(Value::Nil),
                }
            }
            // #RRGGBB -> expand to 16-bit
            6 => {
                let r = u16::from_str_radix(&hex[0..2], 16);
                let g = u16::from_str_radix(&hex[2..4], 16);
                let b = u16::from_str_radix(&hex[4..6], 16);
                match (r, g, b) {
                    (Ok(r), Ok(g), Ok(b)) => {
                        // 0xFF -> 0xFFFF (multiply by 0x0101)
                        (r * 0x0101, g * 0x0101, b * 0x0101)
                    }
                    _ => return Ok(Value::Nil),
                }
            }
            // #RRRRGGGGBBBB -> already 16-bit
            12 => {
                let r = u16::from_str_radix(&hex[0..4], 16);
                let g = u16::from_str_radix(&hex[4..8], 16);
                let b = u16::from_str_radix(&hex[8..12], 16);
                match (r, g, b) {
                    (Ok(r), Ok(g), Ok(b)) => (r, g, b),
                    _ => return Ok(Value::Nil),
                }
            }
            _ => return Ok(Value::Nil),
        };
        return Ok(Value::list(vec![
            Value::Int(r as i64),
            Value::Int(g as i64),
            Value::Int(b as i64),
        ]));
    }

    // Named color lookup.
    let rgb: Option<(i64, i64, i64)> = match lower.as_str() {
        "black" => Some((0, 0, 0)),
        "white" => Some((65535, 65535, 65535)),
        "red" => Some((65535, 0, 0)),
        "green" => Some((0, 65535, 0)),
        "blue" => Some((0, 0, 65535)),
        "yellow" => Some((65535, 65535, 0)),
        "cyan" | "aqua" => Some((0, 65535, 65535)),
        "magenta" | "fuchsia" => Some((65535, 0, 65535)),
        "gray" | "grey" => Some((32896, 32896, 32896)),
        "darkgray" | "darkgrey" | "dark gray" | "dark grey" => Some((43690, 43690, 43690)),
        "lightgray" | "lightgrey" | "light gray" | "light grey" => Some((54227, 54227, 54227)),
        "orange" => Some((65535, 42405, 0)),
        "brown" => Some((42405, 10794, 10794)),
        "pink" => Some((65535, 49344, 52171)),
        "purple" => Some((32896, 0, 32896)),
        "navy" => Some((0, 0, 32896)),
        "maroon" => Some((32896, 0, 0)),
        "olive" => Some((32896, 32896, 0)),
        "teal" => Some((0, 32896, 32896)),
        "silver" => Some((49344, 49344, 49344)),
        _ => None,
    };

    match rgb {
        Some((r, g, b)) => Ok(Value::list(vec![
            Value::Int(r),
            Value::Int(g),
            Value::Int(b),
        ])),
        None => Ok(Value::Nil),
    }
}

/// `(display-supports-face-attributes-p ATTRIBUTES &optional DISPLAY)` -> t.
///
/// Return t if the display supports the given face attributes.  Stub returns t.
pub(crate) fn builtin_display_supports_face_attributes_p(args: Vec<Value>) -> EvalResult {
    expect_range_args("display-supports-face-attributes-p", &args, 1, 2)?;
    Ok(Value::True)
}

/// `(internal-face-x-get-resource RESOURCE CLASS &optional FRAME)` -> nil.
///
/// Return the X resource value for RESOURCE/CLASS.  Stub returns nil.
pub(crate) fn builtin_internal_face_x_get_resource(args: Vec<Value>) -> EvalResult {
    expect_range_args("internal-face-x-get-resource", &args, 2, 3)?;
    Ok(Value::Nil)
}

/// `(tty-suppress-bold-inverse-default-colors FLAG)` -> nil.
///
/// Suppress bold/inverse for default colors on TTY.  Stub returns nil.
pub(crate) fn builtin_tty_suppress_bold_inverse_default_colors(args: Vec<Value>) -> EvalResult {
    expect_args("tty-suppress-bold-inverse-default-colors", &args, 1)?;
    Ok(Value::Nil)
}

/// `(dump-colors)` -> nil.
///
/// Dump the internal color table (for debugging).  Stub returns nil.
pub(crate) fn builtin_dump_colors(args: Vec<Value>) -> EvalResult {
    expect_args("dump-colors", &args, 0)?;
    Ok(Value::Nil)
}

/// `(face-spec-set-match-display DISPLAY FRAME)` -> t or nil.
///
/// Return t if the display specification DISPLAY matches FRAME.
/// Stub: returns t for `t` or nil display specs, nil otherwise.
pub(crate) fn builtin_face_spec_set_match_display(args: Vec<Value>) -> EvalResult {
    expect_args("face-spec-set-match-display", &args, 2)?;
    match &args[0] {
        Value::True => Ok(Value::True),
        Value::Nil => Ok(Value::True),
        _ => Ok(Value::True),
    }
}

/// `(internal-set-font-selection-order ORDER)` -> nil.
///
/// Set the order of font attributes for selection.  Stub returns nil.
pub(crate) fn builtin_internal_set_font_selection_order(args: Vec<Value>) -> EvalResult {
    expect_args("internal-set-font-selection-order", &args, 1)?;
    Ok(Value::Nil)
}

/// `(internal-set-alternative-font-family-alist ALIST)` -> nil.
///
/// Set the alternative font family alist.  Stub returns nil.
pub(crate) fn builtin_internal_set_alternative_font_family_alist(args: Vec<Value>) -> EvalResult {
    expect_args("internal-set-alternative-font-family-alist", &args, 1)?;
    Ok(Value::Nil)
}

/// `(internal-set-alternative-font-registry-alist ALIST)` -> nil.
///
/// Set the alternative font registry alist.  Stub returns nil.
pub(crate) fn builtin_internal_set_alternative_font_registry_alist(args: Vec<Value>) -> EvalResult {
    expect_args("internal-set-alternative-font-registry-alist", &args, 1)?;
    Ok(Value::Nil)
}
