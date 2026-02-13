use super::super::eval::Evaluator;
use super::args::{expect_args, expect_min_args, expect_range_args};
use super::{EvalResult, Value};

/// `(read-key-sequence PROMPT &optional CONTINUE-ECHO DONT-DOWNCASE-LAST
///   CAN-RETURN-SWITCH-FRAME COMMAND-LOOP)` -> string
///
/// Read a sequence of keystrokes and return as a string.
pub(crate) fn builtin_read_key_sequence(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_range_args("read-key-sequence", &args, 1, 5)?;
    Ok(Value::string(""))
}

/// `(read-key-sequence-vector PROMPT &optional CONTINUE-ECHO
///   DONT-DOWNCASE-LAST CAN-RETURN-SWITCH-FRAME COMMAND-LOOP)` -> vector
///
/// Like `read-key-sequence` but always returns a vector.
///
/// Stub implementation: returns an empty vector.
pub(crate) fn builtin_read_key_sequence_vector(
    _eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("read-key-sequence-vector", &args, 1, 5)?;
    Ok(Value::vector(vec![]))
}

/// `(this-command-keys)` -> vector
///
/// Return the key sequence that invoked this command, as a vector.
///
/// Stub implementation: returns an empty vector.
pub(crate) fn builtin_this_command_keys(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("this-command-keys", &args, 0)?;
    Ok(Value::vector(vec![]))
}

/// `(this-command-keys-vector)` -> vector
///
/// Return the key sequence that invoked this command, as a vector.
///
/// Stub implementation: returns an empty vector.
pub(crate) fn builtin_this_command_keys_vector(
    _eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("this-command-keys-vector", &args, 0)?;
    Ok(Value::vector(vec![]))
}

/// `(this-single-command-keys)` -> vector
///
/// Return the key sequence that invoked this command, without prefix args.
///
/// Stub implementation: returns an empty vector.
pub(crate) fn builtin_this_single_command_keys(
    _eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("this-single-command-keys", &args, 0)?;
    Ok(Value::vector(vec![]))
}

/// `(this-single-command-raw-keys)` -> vector
///
/// Return the raw events that were translated for this command.
///
/// Stub implementation: returns an empty vector.
pub(crate) fn builtin_this_single_command_raw_keys(
    _eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("this-single-command-raw-keys", &args, 0)?;
    Ok(Value::vector(vec![]))
}

/// `(execute-extended-command PREFIXARG &optional COMMAND-NAME TYPED)` -> nil
///
/// Read a command name from the minibuffer and execute it.
///
/// Stub implementation: returns nil.
pub(crate) fn builtin_execute_extended_command(
    _eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("execute-extended-command", &args, 1, 3)?;
    Ok(Value::Nil)
}

/// `(command-execute CMD &optional RECORD-FLAG KEYS SPECIAL)` -> nil
///
/// Execute CMD as an editor command.
///
/// Stub implementation: returns nil.
pub(crate) fn builtin_command_execute(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_range_args("command-execute", &args, 1, 4)?;
    Ok(Value::Nil)
}

/// `(set-input-mode INTERRUPT FLOW META &optional QUIT)` -> nil
///
/// Set mode of reading keyboard input.
///
/// Stub implementation: returns nil.
pub(crate) fn builtin_set_input_mode(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_range_args("set-input-mode", &args, 3, 4)?;
    Ok(Value::Nil)
}

/// `(current-input-mode)` -> list
///
/// Return information about the way Emacs currently reads keyboard input.
/// Returns a list of four elements: (INTERRUPT FLOW META QUIT).
///
/// Stub implementation: returns (nil nil nil 7) which represents:
/// - INTERRUPT = nil (no interrupt-driven input)
/// - FLOW = nil (no flow control)
/// - META = nil (8th bit is not meta)
/// - QUIT = 7 (C-g character code)
pub(crate) fn builtin_current_input_mode(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("current-input-mode", &args, 0)?;
    Ok(Value::list(vec![
        Value::Nil,    // interrupt
        Value::Nil,    // flow
        Value::Nil,    // meta
        Value::Int(7), // quit char (C-g)
    ]))
}

/// `(open-dribble-file FILE)` -> nil
///
/// Start writing all keyboard characters to FILE.
///
/// Stub implementation: returns nil.
pub(crate) fn builtin_open_dribble_file(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("open-dribble-file", &args, 1)?;
    Ok(Value::Nil)
}

/// `(set-input-interrupt-mode)` -> nil
///
/// Set interrupt mode for terminal input.
///
/// Stub implementation: returns nil.
pub(crate) fn builtin_set_input_interrupt_mode(
    _eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-input-interrupt-mode", &args, 0)?;
    Ok(Value::Nil)
}
