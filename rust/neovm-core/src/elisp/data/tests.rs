use super::*;

// =======================================================================
// Pure builtins
// =======================================================================

// ----- symbol-plist -----

#[test]
fn symbol_plist_returns_nil() {
    let result = builtin_symbol_plist(vec![Value::symbol("foo")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn symbol_plist_nil_symbol() {
    let result = builtin_symbol_plist(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn symbol_plist_wrong_type() {
    let result = builtin_symbol_plist(vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn symbol_plist_wrong_arity() {
    let result = builtin_symbol_plist(vec![]);
    assert!(result.is_err());
}

// ----- setplist -----

#[test]
fn setplist_returns_newplist() {
    let plist = Value::list(vec![Value::symbol("a"), Value::Int(1)]);
    let result = builtin_setplist(vec![Value::symbol("foo"), plist.clone()]);
    assert!(result.is_ok());
    assert!(equal_value(&result.unwrap(), &plist, 0));
}

#[test]
fn setplist_wrong_type() {
    let result = builtin_setplist(vec![Value::Int(1), Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn setplist_wrong_arity() {
    let result = builtin_setplist(vec![Value::symbol("foo")]);
    assert!(result.is_err());
}

// ----- symbol-name -----

#[test]
fn symbol_name_basic() {
    let result = builtin_symbol_name(vec![Value::symbol("hello")]).unwrap();
    assert_eq!(result.as_str(), Some("hello"));
}

#[test]
fn symbol_name_nil() {
    let result = builtin_symbol_name(vec![Value::Nil]).unwrap();
    assert_eq!(result.as_str(), Some("nil"));
}

#[test]
fn symbol_name_t() {
    let result = builtin_symbol_name(vec![Value::True]).unwrap();
    assert_eq!(result.as_str(), Some("t"));
}

#[test]
fn symbol_name_wrong_type() {
    let result = builtin_symbol_name(vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn symbol_name_wrong_arity() {
    let result = builtin_symbol_name(vec![]);
    assert!(result.is_err());
}

// ----- make-symbol -----

#[test]
fn make_symbol_basic() {
    let result = builtin_make_symbol(vec![Value::string("my-sym")]).unwrap();
    assert_eq!(result.as_symbol_name(), Some("my-sym"));
}

#[test]
fn make_symbol_wrong_type() {
    let result = builtin_make_symbol(vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn make_symbol_wrong_arity() {
    let result = builtin_make_symbol(vec![]);
    assert!(result.is_err());
}

// ----- indirect-variable -----

#[test]
fn indirect_variable_passthrough() {
    let sym = Value::symbol("x");
    let result = builtin_indirect_variable(vec![sym.clone()]).unwrap();
    assert!(eq_value(&result, &sym));
}

#[test]
fn indirect_variable_non_symbol() {
    // Should still pass through (identity for any type)
    let val = Value::Int(42);
    let result = builtin_indirect_variable(vec![val.clone()]).unwrap();
    assert!(eq_value(&result, &val));
}

#[test]
fn indirect_variable_wrong_arity() {
    let result = builtin_indirect_variable(vec![]);
    assert!(result.is_err());
}

// ----- subr-name -----

#[test]
fn subr_name_basic() {
    let result = builtin_subr_name(vec![Value::Subr("+".to_string())]).unwrap();
    assert_eq!(result.as_str(), Some("+"));
}

#[test]
fn subr_name_wrong_type() {
    let result = builtin_subr_name(vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn subr_name_wrong_arity() {
    let result = builtin_subr_name(vec![]);
    assert!(result.is_err());
}

// ----- byte-code-function-p -----

#[test]
fn byte_code_function_p_false() {
    let result = builtin_byte_code_function_p(vec![Value::Int(42)]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn byte_code_function_p_nil_for_lambda() {
    let params = LambdaParams::simple(vec![]);
    let lambda = Value::Lambda(std::sync::Arc::new(LambdaData {
        params,
        body: vec![],
        env: None,
        docstring: None,
    }));
    let result = builtin_byte_code_function_p(vec![lambda]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn byte_code_function_p_wrong_arity() {
    let result = builtin_byte_code_function_p(vec![]);
    assert!(result.is_err());
}

// ----- module-function-p -----

#[test]
fn module_function_p_always_nil() {
    let result = builtin_module_function_p(vec![Value::Int(42)]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn module_function_p_wrong_arity() {
    let result = builtin_module_function_p(vec![]);
    assert!(result.is_err());
}

// ----- number-or-marker-p -----

#[test]
fn number_or_marker_p_int() {
    let result = builtin_number_or_marker_p(vec![Value::Int(42)]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn number_or_marker_p_float() {
    let result = builtin_number_or_marker_p(vec![Value::Float(3.14)]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn number_or_marker_p_string() {
    let result = builtin_number_or_marker_p(vec![Value::string("hello")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn number_or_marker_p_nil() {
    let result = builtin_number_or_marker_p(vec![Value::Nil]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn number_or_marker_p_wrong_arity() {
    let result = builtin_number_or_marker_p(vec![]);
    assert!(result.is_err());
}

// ----- integer-or-marker-p -----

#[test]
fn integer_or_marker_p_int() {
    let result = builtin_integer_or_marker_p(vec![Value::Int(10)]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn integer_or_marker_p_float() {
    let result = builtin_integer_or_marker_p(vec![Value::Float(1.0)]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn integer_or_marker_p_symbol() {
    let result = builtin_integer_or_marker_p(vec![Value::symbol("x")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn integer_or_marker_p_wrong_arity() {
    let result = builtin_integer_or_marker_p(vec![]);
    assert!(result.is_err());
}

// ----- natnump -----

#[test]
fn natnump_zero() {
    let result = builtin_natnump(vec![Value::Int(0)]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn natnump_positive() {
    let result = builtin_natnump(vec![Value::Int(42)]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn natnump_negative() {
    let result = builtin_natnump(vec![Value::Int(-1)]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn natnump_float() {
    let result = builtin_natnump(vec![Value::Float(1.0)]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn natnump_wrong_arity() {
    let result = builtin_natnump(vec![]);
    assert!(result.is_err());
}

// ----- fixnump -----

#[test]
fn fixnump_int() {
    let result = builtin_fixnump(vec![Value::Int(42)]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn fixnump_float() {
    let result = builtin_fixnump(vec![Value::Float(1.0)]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn fixnump_string() {
    let result = builtin_fixnump(vec![Value::string("42")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn fixnump_wrong_arity() {
    let result = builtin_fixnump(vec![]);
    assert!(result.is_err());
}

// ----- bignump -----

#[test]
fn bignump_always_nil() {
    let result = builtin_bignump(vec![Value::Int(i64::MAX)]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn bignump_wrong_arity() {
    let result = builtin_bignump(vec![]);
    assert!(result.is_err());
}

// ----- bare-symbol -----

#[test]
fn bare_symbol_passthrough() {
    let sym = Value::symbol("x");
    let result = builtin_bare_symbol(vec![sym.clone()]).unwrap();
    assert!(eq_value(&result, &sym));
}

#[test]
fn bare_symbol_nil() {
    let result = builtin_bare_symbol(vec![Value::Nil]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn bare_symbol_wrong_arity() {
    let result = builtin_bare_symbol(vec![]);
    assert!(result.is_err());
}

// ----- symbol-with-pos-p -----

#[test]
fn symbol_with_pos_p_always_nil() {
    let result = builtin_symbol_with_pos_p(vec![Value::symbol("x")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn symbol_with_pos_p_wrong_arity() {
    let result = builtin_symbol_with_pos_p(vec![]);
    assert!(result.is_err());
}

// ----- remove-pos-from-symbol -----

#[test]
fn remove_pos_from_symbol_passthrough() {
    let sym = Value::symbol("x");
    let result = builtin_remove_pos_from_symbol(vec![sym.clone()]).unwrap();
    assert!(eq_value(&result, &sym));
}

#[test]
fn remove_pos_from_symbol_wrong_arity() {
    let result = builtin_remove_pos_from_symbol(vec![]);
    assert!(result.is_err());
}

// ----- logcount -----

#[test]
fn logcount_zero() {
    let result = builtin_logcount(vec![Value::Int(0)]).unwrap();
    assert!(eq_value(&result, &Value::Int(0)));
}

#[test]
fn logcount_one() {
    let result = builtin_logcount(vec![Value::Int(1)]).unwrap();
    assert!(eq_value(&result, &Value::Int(1)));
}

#[test]
fn logcount_seven() {
    // 7 = 0b111, so 3 bits set
    let result = builtin_logcount(vec![Value::Int(7)]).unwrap();
    assert!(eq_value(&result, &Value::Int(3)));
}

#[test]
fn logcount_power_of_two() {
    // 256 = 0b100000000, so 1 bit set
    let result = builtin_logcount(vec![Value::Int(256)]).unwrap();
    assert!(eq_value(&result, &Value::Int(1)));
}

#[test]
fn logcount_negative_one() {
    // -1 in two's complement is all 1s. !(-1) = 0, so logcount = 0
    let result = builtin_logcount(vec![Value::Int(-1)]).unwrap();
    assert!(eq_value(&result, &Value::Int(0)));
}

#[test]
fn logcount_negative_two() {
    // -2 in two's complement: !(-2) = 1, so logcount = 1
    let result = builtin_logcount(vec![Value::Int(-2)]).unwrap();
    assert!(eq_value(&result, &Value::Int(1)));
}

#[test]
fn logcount_wrong_type() {
    let result = builtin_logcount(vec![Value::Float(1.5)]);
    assert!(result.is_err());
}

#[test]
fn logcount_wrong_arity() {
    let result = builtin_logcount(vec![]);
    assert!(result.is_err());
}

// =======================================================================
// Eval-dependent builtins
// =======================================================================

// ----- boundp -----

#[test]
fn boundp_unbound() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_boundp(&mut eval, vec![Value::symbol("x")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn boundp_bound_in_obarray() {
    let mut eval = super::super::eval::Evaluator::new();
    eval.obarray.set_symbol_value("x", Value::Int(42));
    let result = builtin_boundp(&mut eval, vec![Value::symbol("x")]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn boundp_bound_in_dynamic() {
    let mut eval = super::super::eval::Evaluator::new();
    eval.dynamic.push(std::collections::HashMap::new());
    eval.dynamic
        .last_mut()
        .unwrap()
        .insert("y".to_string(), Value::Int(10));
    let result = builtin_boundp(&mut eval, vec![Value::symbol("y")]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn boundp_wrong_type() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_boundp(&mut eval, vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn boundp_wrong_arity() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_boundp(&mut eval, vec![]);
    assert!(result.is_err());
}

// ----- fboundp -----

#[test]
fn fboundp_unbound() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_fboundp(&mut eval, vec![Value::symbol("my-func")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn fboundp_bound() {
    let mut eval = super::super::eval::Evaluator::new();
    eval.obarray
        .set_symbol_function("my-func", Value::Subr("+".to_string()));
    let result = builtin_fboundp(&mut eval, vec![Value::symbol("my-func")]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn fboundp_wrong_type() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_fboundp(&mut eval, vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn fboundp_wrong_arity() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_fboundp(&mut eval, vec![]);
    assert!(result.is_err());
}

// ----- symbol-value -----

#[test]
fn symbol_value_nil() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_symbol_value(&mut eval, vec![Value::Nil]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn symbol_value_t() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_symbol_value(&mut eval, vec![Value::True]).unwrap();
    assert!(eq_value(&result, &Value::True));
}

#[test]
fn symbol_value_from_obarray() {
    let mut eval = super::super::eval::Evaluator::new();
    eval.obarray.set_symbol_value("x", Value::Int(42));
    let result = builtin_symbol_value(&mut eval, vec![Value::symbol("x")]).unwrap();
    assert!(eq_value(&result, &Value::Int(42)));
}

#[test]
fn symbol_value_from_dynamic() {
    let mut eval = super::super::eval::Evaluator::new();
    eval.obarray.set_symbol_value("x", Value::Int(1));
    eval.dynamic.push(std::collections::HashMap::new());
    eval.dynamic
        .last_mut()
        .unwrap()
        .insert("x".to_string(), Value::Int(99));
    let result = builtin_symbol_value(&mut eval, vec![Value::symbol("x")]).unwrap();
    // Dynamic binding should shadow the obarray
    assert!(eq_value(&result, &Value::Int(99)));
}

#[test]
fn symbol_value_void_variable() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_symbol_value(&mut eval, vec![Value::symbol("unbound")]);
    assert!(result.is_err());
}

#[test]
fn symbol_value_wrong_type() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_symbol_value(&mut eval, vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn symbol_value_wrong_arity() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_symbol_value(&mut eval, vec![]);
    assert!(result.is_err());
}

// ----- symbol-function -----

#[test]
fn symbol_function_found() {
    let mut eval = super::super::eval::Evaluator::new();
    eval.obarray
        .set_symbol_function("my-func", Value::Subr("car".to_string()));
    let result = builtin_symbol_function(&mut eval, vec![Value::symbol("my-func")]).unwrap();
    assert!(eq_value(&result, &Value::Subr("car".to_string())));
}

#[test]
fn symbol_function_void() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_symbol_function(&mut eval, vec![Value::symbol("nonexistent")]);
    assert!(result.is_err());
}

#[test]
fn symbol_function_wrong_type() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_symbol_function(&mut eval, vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn symbol_function_wrong_arity() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_symbol_function(&mut eval, vec![]);
    assert!(result.is_err());
}

// ----- fset -----

#[test]
fn fset_basic() {
    let mut eval = super::super::eval::Evaluator::new();
    let func = Value::Subr("+".to_string());
    let result = builtin_fset(&mut eval, vec![Value::symbol("add"), func.clone()]).unwrap();
    assert!(eq_value(&result, &func));
    // Verify it was set
    assert!(eval.obarray.fboundp("add"));
    assert!(eq_value(
        eval.obarray.symbol_function("add").unwrap(),
        &func
    ));
}

#[test]
fn fset_overwrite() {
    let mut eval = super::super::eval::Evaluator::new();
    eval.obarray
        .set_symbol_function("f", Value::Subr("+".to_string()));
    let new_func = Value::Subr("-".to_string());
    builtin_fset(&mut eval, vec![Value::symbol("f"), new_func.clone()]).unwrap();
    assert!(eq_value(
        eval.obarray.symbol_function("f").unwrap(),
        &new_func
    ));
}

#[test]
fn fset_wrong_type() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_fset(&mut eval, vec![Value::Int(42), Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn fset_wrong_arity() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_fset(&mut eval, vec![Value::symbol("f")]);
    assert!(result.is_err());
}

// ----- set -----

#[test]
fn set_basic() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_set(&mut eval, vec![Value::symbol("x"), Value::Int(42)]).unwrap();
    assert!(eq_value(&result, &Value::Int(42)));
    assert!(eq_value(
        eval.obarray.symbol_value("x").unwrap(),
        &Value::Int(42)
    ));
}

#[test]
fn set_overwrite() {
    let mut eval = super::super::eval::Evaluator::new();
    eval.obarray.set_symbol_value("x", Value::Int(1));
    builtin_set(&mut eval, vec![Value::symbol("x"), Value::Int(2)]).unwrap();
    assert!(eq_value(
        eval.obarray.symbol_value("x").unwrap(),
        &Value::Int(2)
    ));
}

#[test]
fn set_wrong_type() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_set(&mut eval, vec![Value::Int(42), Value::Int(1)]);
    assert!(result.is_err());
}

#[test]
fn set_wrong_arity() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_set(&mut eval, vec![Value::symbol("x")]);
    assert!(result.is_err());
}

// ----- set-default -----

#[test]
fn set_default_basic() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_set_default(&mut eval, vec![Value::symbol("x"), Value::Int(100)]).unwrap();
    assert!(eq_value(&result, &Value::Int(100)));
    assert!(eq_value(
        eval.obarray.symbol_value("x").unwrap(),
        &Value::Int(100)
    ));
}

#[test]
fn set_default_wrong_arity() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_set_default(&mut eval, vec![]);
    assert!(result.is_err());
}

// ----- default-value -----

#[test]
fn default_value_nil() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_default_value(&mut eval, vec![Value::Nil]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn default_value_t() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_default_value(&mut eval, vec![Value::True]).unwrap();
    assert!(eq_value(&result, &Value::True));
}

#[test]
fn default_value_bound() {
    let mut eval = super::super::eval::Evaluator::new();
    eval.obarray.set_symbol_value("x", Value::Int(77));
    let result = builtin_default_value(&mut eval, vec![Value::symbol("x")]).unwrap();
    assert!(eq_value(&result, &Value::Int(77)));
}

#[test]
fn default_value_void() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_default_value(&mut eval, vec![Value::symbol("unbound")]);
    assert!(result.is_err());
}

#[test]
fn default_value_wrong_arity() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_default_value(&mut eval, vec![]);
    assert!(result.is_err());
}

// ----- make-variable-buffer-local -----

#[test]
fn make_variable_buffer_local_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_make_variable_buffer_local(&mut eval, vec![Value::symbol("x")]).unwrap();
    assert!(eq_value(&result, &Value::symbol("x")));
}

#[test]
fn make_variable_buffer_local_wrong_type() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_make_variable_buffer_local(&mut eval, vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn make_variable_buffer_local_wrong_arity() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_make_variable_buffer_local(&mut eval, vec![]);
    assert!(result.is_err());
}

// ----- make-local-variable -----

#[test]
fn make_local_variable_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_make_local_variable(&mut eval, vec![Value::symbol("y")]).unwrap();
    assert!(eq_value(&result, &Value::symbol("y")));
}

#[test]
fn make_local_variable_wrong_type() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_make_local_variable(&mut eval, vec![Value::Int(42)]);
    assert!(result.is_err());
}

// ----- kill-local-variable -----

#[test]
fn kill_local_variable_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_kill_local_variable(&mut eval, vec![Value::symbol("z")]).unwrap();
    assert!(eq_value(&result, &Value::symbol("z")));
}

#[test]
fn kill_local_variable_wrong_type() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_kill_local_variable(&mut eval, vec![Value::Int(42)]);
    assert!(result.is_err());
}

// ----- local-variable-p -----

#[test]
fn local_variable_p_stub_nil() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_local_variable_p(&mut eval, vec![Value::symbol("x")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn local_variable_p_with_buffer_arg() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_local_variable_p(&mut eval, vec![Value::symbol("x"), Value::Nil]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn local_variable_p_too_many_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result =
        builtin_local_variable_p(&mut eval, vec![Value::symbol("x"), Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn local_variable_p_wrong_type() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_local_variable_p(&mut eval, vec![Value::Int(42)]);
    assert!(result.is_err());
}

// ----- buffer-local-value -----

#[test]
fn buffer_local_value_nil_sym() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_buffer_local_value(&mut eval, vec![Value::Nil, Value::Nil]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn buffer_local_value_t_sym() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_buffer_local_value(&mut eval, vec![Value::True, Value::Nil]).unwrap();
    assert!(eq_value(&result, &Value::True));
}

#[test]
fn buffer_local_value_fallback() {
    let mut eval = super::super::eval::Evaluator::new();
    eval.obarray.set_symbol_value("x", Value::Int(55));
    let result =
        builtin_buffer_local_value(&mut eval, vec![Value::symbol("x"), Value::Nil]).unwrap();
    assert!(eq_value(&result, &Value::Int(55)));
}

#[test]
fn buffer_local_value_void() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_buffer_local_value(&mut eval, vec![Value::symbol("unbound"), Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn buffer_local_value_wrong_arity() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_buffer_local_value(&mut eval, vec![Value::symbol("x")]);
    assert!(result.is_err());
}

// ----- makunbound -----

#[test]
fn makunbound_from_obarray() {
    let mut eval = super::super::eval::Evaluator::new();
    eval.obarray.set_symbol_value("x", Value::Int(42));
    assert!(eval.obarray.boundp("x"));
    let result = builtin_makunbound(&mut eval, vec![Value::symbol("x")]).unwrap();
    assert!(eq_value(&result, &Value::symbol("x")));
    assert!(!eval.obarray.boundp("x"));
}

#[test]
fn makunbound_from_dynamic() {
    let mut eval = super::super::eval::Evaluator::new();
    eval.dynamic.push(std::collections::HashMap::new());
    eval.dynamic
        .last_mut()
        .unwrap()
        .insert("y".to_string(), Value::Int(10));
    let result = builtin_makunbound(&mut eval, vec![Value::symbol("y")]).unwrap();
    assert!(eq_value(&result, &Value::symbol("y")));
    // Should have been removed from dynamic
    assert!(!eval.dynamic.last().unwrap().contains_key("y"));
}

#[test]
fn makunbound_unbound_is_ok() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_makunbound(&mut eval, vec![Value::symbol("z")]).unwrap();
    assert!(eq_value(&result, &Value::symbol("z")));
}

#[test]
fn makunbound_wrong_type() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_makunbound(&mut eval, vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn makunbound_wrong_arity() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_makunbound(&mut eval, vec![]);
    assert!(result.is_err());
}

// ----- fmakunbound -----

#[test]
fn fmakunbound_basic() {
    let mut eval = super::super::eval::Evaluator::new();
    eval.obarray
        .set_symbol_function("f", Value::Subr("+".to_string()));
    assert!(eval.obarray.fboundp("f"));
    let result = builtin_fmakunbound(&mut eval, vec![Value::symbol("f")]).unwrap();
    assert!(eq_value(&result, &Value::symbol("f")));
    assert!(!eval.obarray.fboundp("f"));
}

#[test]
fn fmakunbound_unbound_is_ok() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_fmakunbound(&mut eval, vec![Value::symbol("g")]).unwrap();
    assert!(eq_value(&result, &Value::symbol("g")));
}

#[test]
fn fmakunbound_wrong_type() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_fmakunbound(&mut eval, vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn fmakunbound_wrong_arity() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_fmakunbound(&mut eval, vec![]);
    assert!(result.is_err());
}

// =======================================================================
// Integration / edge case tests
// =======================================================================

#[test]
fn set_then_symbol_value() {
    let mut eval = super::super::eval::Evaluator::new();
    builtin_set(&mut eval, vec![Value::symbol("v"), Value::string("hello")]).unwrap();
    let result = builtin_symbol_value(&mut eval, vec![Value::symbol("v")]).unwrap();
    assert_eq!(result.as_str(), Some("hello"));
}

#[test]
fn set_then_boundp() {
    let mut eval = super::super::eval::Evaluator::new();
    let bp = builtin_boundp(&mut eval, vec![Value::symbol("w")]).unwrap();
    assert!(bp.is_nil());
    builtin_set(&mut eval, vec![Value::symbol("w"), Value::Int(1)]).unwrap();
    let bp = builtin_boundp(&mut eval, vec![Value::symbol("w")]).unwrap();
    assert!(bp.is_truthy());
}

#[test]
fn fset_then_symbol_function() {
    let mut eval = super::super::eval::Evaluator::new();
    let func = Value::Subr("*".to_string());
    builtin_fset(&mut eval, vec![Value::symbol("mul"), func.clone()]).unwrap();
    let result = builtin_symbol_function(&mut eval, vec![Value::symbol("mul")]).unwrap();
    assert!(eq_value(&result, &func));
}

#[test]
fn fset_then_fboundp() {
    let mut eval = super::super::eval::Evaluator::new();
    let fb = builtin_fboundp(&mut eval, vec![Value::symbol("my-fn")]).unwrap();
    assert!(fb.is_nil());
    builtin_fset(
        &mut eval,
        vec![Value::symbol("my-fn"), Value::Subr("+".to_string())],
    )
    .unwrap();
    let fb = builtin_fboundp(&mut eval, vec![Value::symbol("my-fn")]).unwrap();
    assert!(fb.is_truthy());
}

#[test]
fn set_then_makunbound_then_void() {
    let mut eval = super::super::eval::Evaluator::new();
    builtin_set(&mut eval, vec![Value::symbol("x"), Value::Int(1)]).unwrap();
    builtin_makunbound(&mut eval, vec![Value::symbol("x")]).unwrap();
    let result = builtin_symbol_value(&mut eval, vec![Value::symbol("x")]);
    assert!(result.is_err());
}

#[test]
fn fset_then_fmakunbound_then_void() {
    let mut eval = super::super::eval::Evaluator::new();
    builtin_fset(
        &mut eval,
        vec![Value::symbol("f"), Value::Subr("+".to_string())],
    )
    .unwrap();
    builtin_fmakunbound(&mut eval, vec![Value::symbol("f")]).unwrap();
    let result = builtin_symbol_function(&mut eval, vec![Value::symbol("f")]);
    assert!(result.is_err());
}

#[test]
fn set_default_then_default_value() {
    let mut eval = super::super::eval::Evaluator::new();
    builtin_set_default(&mut eval, vec![Value::symbol("z"), Value::Int(999)]).unwrap();
    let result = builtin_default_value(&mut eval, vec![Value::symbol("z")]).unwrap();
    assert!(eq_value(&result, &Value::Int(999)));
}

#[test]
fn logcount_large_positive() {
    // 0xFF = 255 = 8 bits set
    let result = builtin_logcount(vec![Value::Int(255)]).unwrap();
    assert!(eq_value(&result, &Value::Int(8)));
}

#[test]
fn logcount_large_negative() {
    // -256: !(-256) = 255, which has 8 bits set
    let result = builtin_logcount(vec![Value::Int(-256)]).unwrap();
    assert!(eq_value(&result, &Value::Int(8)));
}

#[test]
fn symbol_name_then_make_symbol_roundtrip() {
    let original = Value::symbol("test-sym");
    let name = builtin_symbol_name(vec![original.clone()]).unwrap();
    let recreated = builtin_make_symbol(vec![name]).unwrap();
    assert_eq!(recreated.as_symbol_name(), Some("test-sym"));
}
