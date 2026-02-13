use super::*;

// ===== internal-make-lisp-face =====

#[test]
fn make_lisp_face_returns_face() {
    let face = Value::symbol("default");
    let result = builtin_internal_make_lisp_face(vec![face.clone()]).unwrap();
    assert!(eq_value(&result, &face));
}

#[test]
fn make_lisp_face_with_frame() {
    let face = Value::symbol("bold");
    let frame = Value::Nil;
    let result = builtin_internal_make_lisp_face(vec![face.clone(), frame]).unwrap();
    assert!(eq_value(&result, &face));
}

#[test]
fn make_lisp_face_no_args_errors() {
    let result = builtin_internal_make_lisp_face(vec![]);
    assert!(result.is_err());
}

#[test]
fn make_lisp_face_too_many_args_errors() {
    let result = builtin_internal_make_lisp_face(vec![Value::symbol("a"), Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

// ===== internal-get-lisp-face-attribute =====

#[test]
fn get_lisp_face_attribute_returns_unspecified() {
    let result = builtin_internal_get_lisp_face_attribute(vec![
        Value::symbol("default"),
        Value::Keyword("foreground".to_string()),
    ])
    .unwrap();
    assert!(eq_value(&result, &Value::symbol("unspecified")));
}

#[test]
fn get_lisp_face_attribute_with_frame() {
    let result = builtin_internal_get_lisp_face_attribute(vec![
        Value::symbol("default"),
        Value::Keyword("background".to_string()),
        Value::Nil,
    ])
    .unwrap();
    assert!(eq_value(&result, &Value::symbol("unspecified")));
}

#[test]
fn get_lisp_face_attribute_wrong_args() {
    let result = builtin_internal_get_lisp_face_attribute(vec![Value::symbol("default")]);
    assert!(result.is_err());
}

// ===== internal-set-lisp-face-attribute =====

#[test]
fn set_lisp_face_attribute_returns_value() {
    let val = Value::string("red");
    let result = builtin_internal_set_lisp_face_attribute(vec![
        Value::symbol("default"),
        Value::Keyword("foreground".to_string()),
        val.clone(),
    ])
    .unwrap();
    assert!(equal_value(&result, &val, 0));
}

#[test]
fn set_lisp_face_attribute_with_frame() {
    let val = Value::Int(120);
    let result = builtin_internal_set_lisp_face_attribute(vec![
        Value::symbol("bold"),
        Value::Keyword("weight".to_string()),
        val.clone(),
        Value::Nil,
    ])
    .unwrap();
    assert!(eq_value(&result, &val));
}

#[test]
fn set_lisp_face_attribute_too_few_args() {
    let result = builtin_internal_set_lisp_face_attribute(vec![
        Value::symbol("default"),
        Value::Keyword("foreground".to_string()),
    ]);
    assert!(result.is_err());
}

// ===== internal-lisp-face-equal-p =====

#[test]
fn lisp_face_equal_p_returns_t() {
    let result =
        builtin_internal_lisp_face_equal_p(vec![Value::symbol("default"), Value::symbol("bold")])
            .unwrap();
    assert!(result.is_truthy());
}

#[test]
fn lisp_face_equal_p_with_frame() {
    let result = builtin_internal_lisp_face_equal_p(vec![
        Value::symbol("default"),
        Value::symbol("default"),
        Value::Nil,
    ])
    .unwrap();
    assert!(result.is_truthy());
}

#[test]
fn lisp_face_equal_p_too_few_args() {
    let result = builtin_internal_lisp_face_equal_p(vec![Value::symbol("default")]);
    assert!(result.is_err());
}

// ===== internal-lisp-face-empty-p =====

#[test]
fn lisp_face_empty_p_returns_t() {
    let result = builtin_internal_lisp_face_empty_p(vec![Value::symbol("default")]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn lisp_face_empty_p_with_frame() {
    let result =
        builtin_internal_lisp_face_empty_p(vec![Value::symbol("default"), Value::Nil]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn lisp_face_empty_p_no_args_errors() {
    let result = builtin_internal_lisp_face_empty_p(vec![]);
    assert!(result.is_err());
}

// ===== internal-lisp-face-p =====

#[test]
fn lisp_face_p_symbol_returns_t() {
    let result = builtin_internal_lisp_face_p(vec![Value::symbol("default")]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn lisp_face_p_t_returns_t() {
    let result = builtin_internal_lisp_face_p(vec![Value::True]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn lisp_face_p_non_symbol_returns_nil() {
    let result = builtin_internal_lisp_face_p(vec![Value::Int(42)]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn lisp_face_p_string_returns_nil() {
    let result = builtin_internal_lisp_face_p(vec![Value::string("default")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn lisp_face_p_wrong_args() {
    let result = builtin_internal_lisp_face_p(vec![]);
    assert!(result.is_err());
}

// ===== internal-copy-lisp-face =====

#[test]
fn copy_lisp_face_returns_to() {
    let to = Value::symbol("new-face");
    let result =
        builtin_internal_copy_lisp_face(vec![Value::symbol("default"), to.clone()]).unwrap();
    assert!(eq_value(&result, &to));
}

#[test]
fn copy_lisp_face_with_frames() {
    let to = Value::symbol("copy");
    let result = builtin_internal_copy_lisp_face(vec![
        Value::symbol("src"),
        to.clone(),
        Value::Nil,
        Value::Nil,
    ])
    .unwrap();
    assert!(eq_value(&result, &to));
}

#[test]
fn copy_lisp_face_too_few_args() {
    let result = builtin_internal_copy_lisp_face(vec![Value::symbol("src")]);
    assert!(result.is_err());
}

// ===== internal-merge-in-global-face =====

#[test]
fn merge_in_global_face_returns_lface() {
    let lface = Value::vector(vec![Value::Nil; 8]);
    let result =
        builtin_internal_merge_in_global_face(vec![Value::symbol("default"), lface.clone()])
            .unwrap();
    // Should return the lface vector
    if let Value::Vector(_) = &result {
        // ok
    } else {
        panic!("expected vector");
    }
}

#[test]
fn merge_in_global_face_with_frame() {
    let lface = Value::vector(vec![]);
    let result = builtin_internal_merge_in_global_face(vec![
        Value::symbol("bold"),
        lface.clone(),
        Value::Nil,
    ])
    .unwrap();
    if let Value::Vector(_) = &result {
        // ok
    } else {
        panic!("expected vector");
    }
}

#[test]
fn merge_in_global_face_too_few_args() {
    let result = builtin_internal_merge_in_global_face(vec![Value::symbol("default")]);
    assert!(result.is_err());
}

// ===== face-font =====

#[test]
fn face_font_returns_nil() {
    let result = builtin_face_font(vec![Value::symbol("default")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn face_font_with_frame() {
    let result = builtin_face_font(vec![Value::symbol("default"), Value::Nil]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn face_font_with_character() {
    let result =
        builtin_face_font(vec![Value::symbol("default"), Value::Nil, Value::Char('A')]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn face_font_no_args_errors() {
    let result = builtin_face_font(vec![]);
    assert!(result.is_err());
}

// ===== face-attribute-relative-p =====

#[test]
fn face_attribute_relative_p_returns_nil() {
    let result = builtin_face_attribute_relative_p(vec![
        Value::Keyword("height".to_string()),
        Value::Float(1.2),
    ])
    .unwrap();
    assert!(result.is_nil());
}

#[test]
fn face_attribute_relative_p_wrong_args() {
    let result = builtin_face_attribute_relative_p(vec![Value::Keyword("height".to_string())]);
    assert!(result.is_err());
}

// ===== face-attributes-as-vector =====

#[test]
fn face_attributes_as_vector_basic() {
    let plist = Value::list(vec![
        Value::Keyword("foreground".to_string()),
        Value::string("red"),
        Value::Keyword("background".to_string()),
        Value::string("blue"),
    ]);
    let result = builtin_face_attributes_as_vector(vec![plist]).unwrap();
    if let Value::Vector(v) = &result {
        let v = v.lock().unwrap();
        assert_eq!(v.len(), 2);
        assert!(equal_value(&v[0], &Value::string("red"), 0));
        assert!(equal_value(&v[1], &Value::string("blue"), 0));
    } else {
        panic!("expected vector");
    }
}

#[test]
fn face_attributes_as_vector_empty() {
    let result = builtin_face_attributes_as_vector(vec![Value::Nil]).unwrap();
    if let Value::Vector(v) = &result {
        let v = v.lock().unwrap();
        assert_eq!(v.len(), 0);
    } else {
        panic!("expected vector");
    }
}

#[test]
fn face_attributes_as_vector_odd_elements() {
    // Odd number of elements: last key has no value, so it's skipped.
    let plist = Value::list(vec![
        Value::Keyword("foreground".to_string()),
        Value::string("red"),
        Value::Keyword("background".to_string()),
    ]);
    let result = builtin_face_attributes_as_vector(vec![plist]).unwrap();
    if let Value::Vector(v) = &result {
        let v = v.lock().unwrap();
        assert_eq!(v.len(), 1);
        assert!(equal_value(&v[0], &Value::string("red"), 0));
    } else {
        panic!("expected vector");
    }
}

#[test]
fn face_attributes_as_vector_wrong_args() {
    let result = builtin_face_attributes_as_vector(vec![]);
    assert!(result.is_err());
}

// ===== merge-face-attribute =====

#[test]
fn merge_face_attribute_returns_value1() {
    let v1 = Value::string("red");
    let result = builtin_merge_face_attribute(vec![
        Value::Keyword("foreground".to_string()),
        v1.clone(),
        Value::string("blue"),
    ])
    .unwrap();
    assert!(equal_value(&result, &v1, 0));
}

#[test]
fn merge_face_attribute_wrong_args() {
    let result = builtin_merge_face_attribute(vec![
        Value::Keyword("foreground".to_string()),
        Value::string("red"),
    ]);
    assert!(result.is_err());
}

// ===== x-family-fonts =====

#[test]
fn x_family_fonts_no_args() {
    let result = builtin_x_family_fonts(vec![]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn x_family_fonts_with_family() {
    let result = builtin_x_family_fonts(vec![Value::string("Monospace")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn x_family_fonts_with_frame() {
    let result = builtin_x_family_fonts(vec![Value::string("Monospace"), Value::Nil]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn x_family_fonts_too_many_args() {
    let result = builtin_x_family_fonts(vec![Value::string("Monospace"), Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

// ===== x-list-fonts =====

#[test]
fn x_list_fonts_basic() {
    let result = builtin_x_list_fonts(vec![Value::string("*courier*")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn x_list_fonts_with_all_args() {
    let result = builtin_x_list_fonts(vec![
        Value::string("*"),
        Value::symbol("default"),
        Value::Nil,
        Value::Int(10),
        Value::Int(80),
    ])
    .unwrap();
    assert!(result.is_nil());
}

#[test]
fn x_list_fonts_no_args_errors() {
    let result = builtin_x_list_fonts(vec![]);
    assert!(result.is_err());
}

// ===== color-distance =====

#[test]
fn color_distance_returns_zero() {
    let result = builtin_color_distance(vec![Value::string("red"), Value::string("blue")]).unwrap();
    assert!(eq_value(&result, &Value::Int(0)));
}

#[test]
fn color_distance_with_frame() {
    let result = builtin_color_distance(vec![
        Value::string("red"),
        Value::string("blue"),
        Value::Nil,
    ])
    .unwrap();
    assert!(eq_value(&result, &Value::Int(0)));
}

#[test]
fn color_distance_wrong_args() {
    let result = builtin_color_distance(vec![Value::string("red")]);
    assert!(result.is_err());
}

// ===== color-gray-p =====

#[test]
fn color_gray_p_returns_nil() {
    let result = builtin_color_gray_p(vec![Value::string("gray")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn color_gray_p_with_frame() {
    let result = builtin_color_gray_p(vec![Value::string("red"), Value::Nil]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn color_gray_p_no_args_errors() {
    let result = builtin_color_gray_p(vec![]);
    assert!(result.is_err());
}

// ===== color-supported-p =====

#[test]
fn color_supported_p_returns_t() {
    let result = builtin_color_supported_p(vec![Value::string("red")]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn color_supported_p_with_background() {
    let result =
        builtin_color_supported_p(vec![Value::string("blue"), Value::Nil, Value::True]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn color_supported_p_no_args_errors() {
    let result = builtin_color_supported_p(vec![]);
    assert!(result.is_err());
}

// ===== color-values =====

#[test]
fn color_values_black() {
    let result = builtin_color_values(vec![Value::string("black")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(0)));
    assert!(eq_value(&items[1], &Value::Int(0)));
    assert!(eq_value(&items[2], &Value::Int(0)));
}

#[test]
fn color_values_white() {
    let result = builtin_color_values(vec![Value::string("white")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(65535)));
    assert!(eq_value(&items[1], &Value::Int(65535)));
    assert!(eq_value(&items[2], &Value::Int(65535)));
}

#[test]
fn color_values_red() {
    let result = builtin_color_values(vec![Value::string("red")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(65535)));
    assert!(eq_value(&items[1], &Value::Int(0)));
    assert!(eq_value(&items[2], &Value::Int(0)));
}

#[test]
fn color_values_green() {
    let result = builtin_color_values(vec![Value::string("green")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(0)));
    assert!(eq_value(&items[1], &Value::Int(65535)));
    assert!(eq_value(&items[2], &Value::Int(0)));
}

#[test]
fn color_values_blue() {
    let result = builtin_color_values(vec![Value::string("blue")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(0)));
    assert!(eq_value(&items[1], &Value::Int(0)));
    assert!(eq_value(&items[2], &Value::Int(65535)));
}

#[test]
fn color_values_yellow() {
    let result = builtin_color_values(vec![Value::string("yellow")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(65535)));
    assert!(eq_value(&items[1], &Value::Int(65535)));
    assert!(eq_value(&items[2], &Value::Int(0)));
}

#[test]
fn color_values_cyan() {
    let result = builtin_color_values(vec![Value::string("cyan")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(0)));
    assert!(eq_value(&items[1], &Value::Int(65535)));
    assert!(eq_value(&items[2], &Value::Int(65535)));
}

#[test]
fn color_values_magenta() {
    let result = builtin_color_values(vec![Value::string("magenta")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(65535)));
    assert!(eq_value(&items[1], &Value::Int(0)));
    assert!(eq_value(&items[2], &Value::Int(65535)));
}

#[test]
fn color_values_case_insensitive() {
    let result = builtin_color_values(vec![Value::string("BLACK")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(0)));
}

#[test]
fn color_values_unknown_returns_nil() {
    let result = builtin_color_values(vec![Value::string("chartreuse-sparkle")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn color_values_hex_rrggbb() {
    // #FF0000 -> red in 16-bit: 65535, 0, 0
    let result = builtin_color_values(vec![Value::string("#FF0000")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(65535)));
    assert!(eq_value(&items[1], &Value::Int(0)));
    assert!(eq_value(&items[2], &Value::Int(0)));
}

#[test]
fn color_values_hex_rgb_short() {
    // #F00 -> red: 0xF * 0x1111 = 0xFFFF = 65535
    let result = builtin_color_values(vec![Value::string("#F00")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(65535)));
    assert!(eq_value(&items[1], &Value::Int(0)));
    assert!(eq_value(&items[2], &Value::Int(0)));
}

#[test]
fn color_values_hex_rrrrggggbbbb() {
    // #FFFF00000000 -> 65535, 0, 0
    let result = builtin_color_values(vec![Value::string("#FFFF00000000")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(65535)));
    assert!(eq_value(&items[1], &Value::Int(0)));
    assert!(eq_value(&items[2], &Value::Int(0)));
}

#[test]
fn color_values_hex_invalid_returns_nil() {
    let result = builtin_color_values(vec![Value::string("#GGG")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn color_values_hex_wrong_length_returns_nil() {
    let result = builtin_color_values(vec![Value::string("#ABCDE")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn color_values_with_frame() {
    let result = builtin_color_values(vec![Value::string("white"), Value::Nil]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(65535)));
}

#[test]
fn color_values_non_string_errors() {
    let result = builtin_color_values(vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn color_values_no_args_errors() {
    let result = builtin_color_values(vec![]);
    assert!(result.is_err());
}

// ===== display-supports-face-attributes-p =====

#[test]
fn display_supports_face_attributes_p_returns_t() {
    let attrs = Value::list(vec![
        Value::Keyword("weight".to_string()),
        Value::symbol("bold"),
    ]);
    let result = builtin_display_supports_face_attributes_p(vec![attrs]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn display_supports_face_attributes_p_with_display() {
    let attrs = Value::Nil;
    let result = builtin_display_supports_face_attributes_p(vec![attrs, Value::Nil]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn display_supports_face_attributes_p_no_args_errors() {
    let result = builtin_display_supports_face_attributes_p(vec![]);
    assert!(result.is_err());
}

// ===== internal-face-x-get-resource =====

#[test]
fn face_x_get_resource_returns_nil() {
    let result = builtin_internal_face_x_get_resource(vec![
        Value::string("foreground"),
        Value::string("Foreground"),
    ])
    .unwrap();
    assert!(result.is_nil());
}

#[test]
fn face_x_get_resource_with_frame() {
    let result = builtin_internal_face_x_get_resource(vec![
        Value::string("foreground"),
        Value::string("Foreground"),
        Value::Nil,
    ])
    .unwrap();
    assert!(result.is_nil());
}

#[test]
fn face_x_get_resource_too_few_args() {
    let result = builtin_internal_face_x_get_resource(vec![Value::string("foreground")]);
    assert!(result.is_err());
}

// ===== tty-suppress-bold-inverse-default-colors =====

#[test]
fn tty_suppress_bold_returns_nil() {
    let result = builtin_tty_suppress_bold_inverse_default_colors(vec![Value::True]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn tty_suppress_bold_no_args_errors() {
    let result = builtin_tty_suppress_bold_inverse_default_colors(vec![]);
    assert!(result.is_err());
}

// ===== dump-colors =====

#[test]
fn dump_colors_returns_nil() {
    let result = builtin_dump_colors(vec![]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn dump_colors_wrong_args() {
    let result = builtin_dump_colors(vec![Value::Int(1)]);
    assert!(result.is_err());
}

// ===== face-spec-set-match-display =====

#[test]
fn face_spec_set_match_display_t() {
    let result = builtin_face_spec_set_match_display(vec![Value::True, Value::Nil]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn face_spec_set_match_display_nil() {
    let result = builtin_face_spec_set_match_display(vec![Value::Nil, Value::Nil]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn face_spec_set_match_display_other() {
    let display = Value::list(vec![Value::cons(
        Value::symbol("type"),
        Value::symbol("graphic"),
    )]);
    let result = builtin_face_spec_set_match_display(vec![display, Value::Nil]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn face_spec_set_match_display_wrong_args() {
    let result = builtin_face_spec_set_match_display(vec![Value::True]);
    assert!(result.is_err());
}

// ===== internal-set-font-selection-order =====

#[test]
fn set_font_selection_order_returns_nil() {
    let order = Value::vector(vec![
        Value::Keyword("weight".to_string()),
        Value::Keyword("slant".to_string()),
    ]);
    let result = builtin_internal_set_font_selection_order(vec![order]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn set_font_selection_order_wrong_args() {
    let result = builtin_internal_set_font_selection_order(vec![]);
    assert!(result.is_err());
}

// ===== internal-set-alternative-font-family-alist =====

#[test]
fn set_alternative_font_family_alist_returns_nil() {
    let alist = Value::list(vec![Value::cons(
        Value::string("Monospace"),
        Value::list(vec![Value::string("Courier")]),
    )]);
    let result = builtin_internal_set_alternative_font_family_alist(vec![alist]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn set_alternative_font_family_alist_wrong_args() {
    let result = builtin_internal_set_alternative_font_family_alist(vec![]);
    assert!(result.is_err());
}

// ===== internal-set-alternative-font-registry-alist =====

#[test]
fn set_alternative_font_registry_alist_returns_nil() {
    let alist = Value::Nil;
    let result = builtin_internal_set_alternative_font_registry_alist(vec![alist]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn set_alternative_font_registry_alist_wrong_args() {
    let result = builtin_internal_set_alternative_font_registry_alist(vec![]);
    assert!(result.is_err());
}

// ===== color-values extended named colors =====

#[test]
fn color_values_gray() {
    let result = builtin_color_values(vec![Value::string("gray")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    // gray has equal R=G=B
    assert!(eq_value(&items[0], &items[1]));
    assert!(eq_value(&items[1], &items[2]));
}

#[test]
fn color_values_grey_alias() {
    let result1 = builtin_color_values(vec![Value::string("gray")]).unwrap();
    let result2 = builtin_color_values(vec![Value::string("grey")]).unwrap();
    assert!(equal_value(&result1, &result2, 0));
}

#[test]
fn color_values_aqua_alias() {
    let result1 = builtin_color_values(vec![Value::string("cyan")]).unwrap();
    let result2 = builtin_color_values(vec![Value::string("aqua")]).unwrap();
    assert!(equal_value(&result1, &result2, 0));
}

#[test]
fn color_values_fuchsia_alias() {
    let result1 = builtin_color_values(vec![Value::string("magenta")]).unwrap();
    let result2 = builtin_color_values(vec![Value::string("fuchsia")]).unwrap();
    assert!(equal_value(&result1, &result2, 0));
}

#[test]
fn color_values_orange() {
    let result = builtin_color_values(vec![Value::string("orange")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    // Orange has max red, some green, no blue
    assert!(eq_value(&items[0], &Value::Int(65535)));
    assert!(eq_value(&items[2], &Value::Int(0)));
}

#[test]
fn color_values_purple() {
    let result = builtin_color_values(vec![Value::string("purple")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    // Purple: R=32896, G=0, B=32896
    assert!(eq_value(&items[0], &Value::Int(32896)));
    assert!(eq_value(&items[1], &Value::Int(0)));
    assert!(eq_value(&items[2], &Value::Int(32896)));
}

#[test]
fn color_values_hex_808080() {
    // #808080 -> 0x80 * 0x0101 = 0x8080 = 32896
    let result = builtin_color_values(vec![Value::string("#808080")]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert_eq!(items.len(), 3);
    assert!(eq_value(&items[0], &Value::Int(32896)));
    assert!(eq_value(&items[1], &Value::Int(32896)));
    assert!(eq_value(&items[2], &Value::Int(32896)));
}
