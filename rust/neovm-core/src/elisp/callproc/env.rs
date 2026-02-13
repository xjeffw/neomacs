use super::args::{expect_min_args, expect_string};
use super::{EvalResult, Value};

/// `(getenv-internal VARIABLE &optional FRAME)` -> string or nil
///
/// Retrieve the value of the environment variable VARIABLE.
/// FRAME is accepted for Emacs API compatibility but ignored.
/// Returns the variable's value as a string, or nil if unset.
pub(crate) fn builtin_getenv_internal(args: Vec<Value>) -> EvalResult {
    expect_min_args("getenv-internal", &args, 1)?;
    let var_name = expect_string(&args[0])?;
    // FRAME (args[1]) is ignored.
    match std::env::var(&var_name) {
        Ok(val) => Ok(Value::string(val)),
        Err(_) => Ok(Value::Nil),
    }
}

/// `(setenv-internal VARIABLE VALUE FRAME)` -> VALUE
///
/// Set the environment variable VARIABLE to VALUE.
/// If VALUE is nil, remove the variable.
/// FRAME is accepted for Emacs API compatibility but ignored.
/// Returns VALUE.
pub(crate) fn builtin_setenv_internal(args: Vec<Value>) -> EvalResult {
    expect_min_args("setenv-internal", &args, 2)?;
    let var_name = expect_string(&args[0])?;
    // FRAME (args[2]) is ignored.
    match &args[1] {
        Value::Nil => {
            // Remove the environment variable.
            // SAFETY: We are single-threaded in the Elisp VM.
            unsafe {
                std::env::remove_var(&var_name);
            }
            Ok(Value::Nil)
        }
        val => {
            let val_str = expect_string(val)?;
            // SAFETY: We are single-threaded in the Elisp VM.
            unsafe {
                std::env::set_var(&var_name, &val_str);
            }
            Ok(args[1].clone())
        }
    }
}
