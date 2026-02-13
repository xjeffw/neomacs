use super::*;

// =======================================================================
// getenv-internal tests
// =======================================================================

#[test]
fn getenv_internal_existing_var() {
    // PATH should exist on virtually all systems.
    let result = builtin_getenv_internal(vec![Value::string("PATH")]);
    assert!(result.is_ok());
    let val = result.unwrap();
    // PATH should be a non-empty string.
    assert!(val.as_str().is_some());
    assert!(!val.as_str().unwrap().is_empty());
}

#[test]
fn getenv_internal_nonexistent_var() {
    let result = builtin_getenv_internal(vec![Value::string("NEOVM_TEST_NONEXISTENT_VAR_12345")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn getenv_internal_with_frame_arg() {
    // FRAME argument is ignored but should not cause an error.
    let result = builtin_getenv_internal(vec![Value::string("PATH"), Value::Nil]);
    assert!(result.is_ok());
}

#[test]
fn getenv_internal_wrong_type() {
    let result = builtin_getenv_internal(vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn getenv_internal_no_args() {
    let result = builtin_getenv_internal(vec![]);
    assert!(result.is_err());
}

// =======================================================================
// setenv-internal tests
// =======================================================================

#[test]
fn setenv_internal_set_and_get() {
    let var = "NEOVM_TEST_SETENV_1";
    let val = "hello_world";

    // Set the variable.
    let result = builtin_setenv_internal(vec![Value::string(var), Value::string(val), Value::Nil]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_str(), Some(val));

    // Verify via getenv.
    let get_result = builtin_getenv_internal(vec![Value::string(var)]);
    assert!(get_result.is_ok());
    assert_eq!(get_result.unwrap().as_str(), Some(val));

    // Clean up.
    unsafe {
        std::env::remove_var(var);
    }
}

#[test]
fn setenv_internal_remove_var() {
    let var = "NEOVM_TEST_SETENV_2";

    // Set first.
    unsafe {
        std::env::set_var(var, "temp");
    }

    // Remove by passing nil.
    let result = builtin_setenv_internal(vec![Value::string(var), Value::Nil, Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());

    // Should be gone.
    let get_result = builtin_getenv_internal(vec![Value::string(var)]);
    assert!(get_result.is_ok());
    assert!(get_result.unwrap().is_nil());
}

#[test]
fn setenv_internal_returns_value() {
    let var = "NEOVM_TEST_SETENV_3";
    let val = "test_value";

    let result = builtin_setenv_internal(vec![Value::string(var), Value::string(val), Value::Nil]);
    assert!(result.is_ok());
    // Should return the VALUE argument.
    assert_eq!(result.unwrap().as_str(), Some(val));

    // Clean up.
    unsafe {
        std::env::remove_var(var);
    }
}

#[test]
fn setenv_internal_wrong_type_variable() {
    let result = builtin_setenv_internal(vec![Value::Int(42), Value::string("val"), Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn setenv_internal_wrong_type_value() {
    let result = builtin_setenv_internal(vec![
        Value::string("NEOVM_TEST_SETENV_BAD"),
        Value::Int(42),
        Value::Nil,
    ]);
    assert!(result.is_err());
}

#[test]
fn setenv_internal_no_args() {
    let result = builtin_setenv_internal(vec![]);
    assert!(result.is_err());
}

#[test]
fn setenv_internal_one_arg() {
    let result = builtin_setenv_internal(vec![Value::string("FOO")]);
    assert!(result.is_err());
}

// =======================================================================
// call-process tests
// =======================================================================

#[test]
fn call_process_true_returns_zero() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    // "true" exits with code 0 on all unix systems.
    let result = builtin_call_process(
        &mut eval,
        vec![
            Value::string("true"),
            Value::Nil, // INFILE
            Value::Nil, // DESTINATION
            Value::Nil, // DISPLAY
        ],
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_int(), Some(0));
}

#[test]
fn call_process_false_returns_nonzero() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    // "false" exits with code 1.
    let result = builtin_call_process(
        &mut eval,
        vec![Value::string("false"), Value::Nil, Value::Nil, Value::Nil],
    );
    assert!(result.is_ok());
    let code = result.unwrap().as_int().unwrap();
    assert_ne!(code, 0);
}

#[test]
fn call_process_with_args() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    // "echo" with arguments — just verify it exits successfully.
    let result = builtin_call_process(
        &mut eval,
        vec![
            Value::string("echo"),
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::string("hello"),
            Value::string("world"),
        ],
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_int(), Some(0));
}

#[test]
fn call_process_nonexistent_program() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_call_process(
        &mut eval,
        vec![
            Value::string("neovm_nonexistent_program_xyz_12345"),
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ],
    );
    // Should signal file-error.
    assert!(result.is_err());
}

#[test]
fn call_process_no_args() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_call_process(&mut eval, vec![]);
    assert!(result.is_err());
}

#[test]
fn call_process_program_only() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    // Just PROGRAM, no optional args — should work fine.
    let result = builtin_call_process(&mut eval, vec![Value::string("true")]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_int(), Some(0));
}

#[test]
fn call_process_wrong_type_program() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_call_process(&mut eval, vec![Value::Int(42)]);
    assert!(result.is_err());
}

// =======================================================================
// call-process-region tests
// =======================================================================

#[test]
fn call_process_region_delegates_to_call_process() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    // (call-process-region 1 100 "true") — START=1, END=100, PROGRAM="true"
    let result = builtin_call_process_region(
        &mut eval,
        vec![
            Value::Int(1),         // START
            Value::Int(100),       // END
            Value::string("true"), // PROGRAM
        ],
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_int(), Some(0));
}

#[test]
fn call_process_region_with_all_args() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_call_process_region(
        &mut eval,
        vec![
            Value::Int(1),          // START
            Value::Int(100),        // END
            Value::string("echo"),  // PROGRAM
            Value::Nil,             // DELETE
            Value::Nil,             // DESTINATION
            Value::Nil,             // DISPLAY
            Value::string("hello"), // ARG1
        ],
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_int(), Some(0));
}

#[test]
fn call_process_region_no_args() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_call_process_region(&mut eval, vec![]);
    assert!(result.is_err());
}

#[test]
fn call_process_region_too_few_args() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    // Only START and END, missing PROGRAM.
    let result = builtin_call_process_region(&mut eval, vec![Value::Int(1), Value::Int(100)]);
    assert!(result.is_err());
}

#[test]
fn call_process_region_nonexistent_program() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_call_process_region(
        &mut eval,
        vec![
            Value::Int(1),
            Value::Int(100),
            Value::string("neovm_nonexistent_program_xyz_12345"),
        ],
    );
    assert!(result.is_err());
}

// =======================================================================
// Edge case tests
// =======================================================================

#[test]
fn getenv_internal_empty_variable_name() {
    // Empty string is a valid argument; std::env::var("") returns Err.
    let result = builtin_getenv_internal(vec![Value::string("")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn setenv_internal_empty_value() {
    let var = "NEOVM_TEST_SETENV_EMPTY";
    let result = builtin_setenv_internal(vec![Value::string(var), Value::string(""), Value::Nil]);
    assert!(result.is_ok());

    let get_result = builtin_getenv_internal(vec![Value::string(var)]);
    assert!(get_result.is_ok());
    assert_eq!(get_result.unwrap().as_str(), Some(""));

    // Clean up.
    unsafe {
        std::env::remove_var(var);
    }
}

#[test]
fn call_process_integer_args_converted() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    // Pass integer args — they should be converted to strings.
    let result = builtin_call_process(
        &mut eval,
        vec![
            Value::string("echo"),
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Int(42),
        ],
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_int(), Some(0));
}
