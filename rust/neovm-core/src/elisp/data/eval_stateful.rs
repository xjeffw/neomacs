use super::super::eval::Evaluator;
use super::helpers::{expect_args, expect_range_args, expect_symbol_name};
use super::{signal, EvalResult, Value};

/// `(boundp SYMBOL)` -> t if SYMBOL is bound as a variable.
///
/// Checks the dynamic binding stack first, then the obarray value cell.
pub(crate) fn builtin_boundp(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("boundp", &args, 1)?;
    let name = expect_symbol_name(&args[0])?;

    // Check dynamic binding stack (inner to outer)
    for frame in eval.dynamic.iter().rev() {
        if frame.contains_key(&name) {
            return Ok(Value::True);
        }
    }

    // Check obarray value cell
    Ok(Value::bool(eval.obarray.boundp(&name)))
}

/// `(fboundp SYMBOL)` -> t if SYMBOL has a function definition.
pub(crate) fn builtin_fboundp(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("fboundp", &args, 1)?;
    let name = expect_symbol_name(&args[0])?;
    Ok(Value::bool(eval.obarray.fboundp(&name)))
}

/// `(symbol-value SYMBOL)` -> return the value of SYMBOL.
///
/// Checks dynamic bindings first, then the obarray value cell.
/// Signals `void-variable` if not bound.
pub(crate) fn builtin_symbol_value(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("symbol-value", &args, 1)?;
    let name = expect_symbol_name(&args[0])?;

    // Special constants
    if name == "nil" {
        return Ok(Value::Nil);
    }
    if name == "t" {
        return Ok(Value::True);
    }

    // Check dynamic binding stack (inner to outer)
    for frame in eval.dynamic.iter().rev() {
        if let Some(value) = frame.get(&name) {
            return Ok(value.clone());
        }
    }

    // Check obarray value cell
    match eval.obarray.symbol_value(&name) {
        Some(value) => Ok(value.clone()),
        None => Err(signal("void-variable", vec![Value::symbol(name)])),
    }
}

/// `(symbol-function SYMBOL)` -> return the function definition of SYMBOL.
///
/// Signals `void-function` if the symbol has no function definition.
pub(crate) fn builtin_symbol_function(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("symbol-function", &args, 1)?;
    let name = expect_symbol_name(&args[0])?;
    match eval.obarray.symbol_function(&name) {
        Some(func) => Ok(func.clone()),
        None => Err(signal("void-function", vec![Value::symbol(name)])),
    }
}

/// `(fset SYMBOL DEFINITION)` -> set the function definition of SYMBOL.
///
/// Returns DEFINITION.
pub(crate) fn builtin_fset(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("fset", &args, 2)?;
    let name = expect_symbol_name(&args[0])?;
    eval.obarray.set_symbol_function(&name, args[1].clone());
    Ok(args[1].clone())
}

/// `(set SYMBOL VALUE)` -> set SYMBOL's value to VALUE.
///
/// Returns VALUE.  Sets in the obarray (global) value cell.
pub(crate) fn builtin_set(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("set", &args, 2)?;
    let name = expect_symbol_name(&args[0])?;
    eval.obarray.set_symbol_value(&name, args[1].clone());
    Ok(args[1].clone())
}

/// `(set-default SYMBOL VALUE)` -> set the default value of SYMBOL.
///
/// Without buffer-local support, this is the same as `set`.
pub(crate) fn builtin_set_default(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("set-default", &args, 2)?;
    let name = expect_symbol_name(&args[0])?;
    eval.obarray.set_symbol_value(&name, args[1].clone());
    Ok(args[1].clone())
}

/// `(default-value SYMBOL)` -> return the default value of SYMBOL.
///
/// Without buffer-local support, this is the same as `symbol-value`.
pub(crate) fn builtin_default_value(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("default-value", &args, 1)?;
    let name = expect_symbol_name(&args[0])?;

    // Special constants
    if name == "nil" {
        return Ok(Value::Nil);
    }
    if name == "t" {
        return Ok(Value::True);
    }

    // Check obarray value cell (default = global)
    match eval.obarray.symbol_value(&name) {
        Some(value) => Ok(value.clone()),
        None => Err(signal("void-variable", vec![Value::symbol(name)])),
    }
}

/// `(make-variable-buffer-local VARIABLE)` -> make VARIABLE buffer-local.
///
/// Stub: returns VARIABLE unchanged.
pub(crate) fn builtin_make_variable_buffer_local(
    _eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("make-variable-buffer-local", &args, 1)?;
    let _name = expect_symbol_name(&args[0])?;
    Ok(args[0].clone())
}

/// `(make-local-variable VARIABLE)` -> make VARIABLE local in the current buffer.
///
/// Stub: returns VARIABLE unchanged.
pub(crate) fn builtin_make_local_variable(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("make-local-variable", &args, 1)?;
    let _name = expect_symbol_name(&args[0])?;
    Ok(args[0].clone())
}

/// `(kill-local-variable VARIABLE)` -> remove the buffer-local binding.
///
/// Stub: returns VARIABLE unchanged.
pub(crate) fn builtin_kill_local_variable(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("kill-local-variable", &args, 1)?;
    let _name = expect_symbol_name(&args[0])?;
    Ok(args[0].clone())
}

/// `(local-variable-p VARIABLE &optional BUFFER)` -> t if VARIABLE has a
/// buffer-local binding in BUFFER.
///
/// Stub: always nil.
pub(crate) fn builtin_local_variable_p(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_range_args("local-variable-p", &args, 1, 2)?;
    let _name = expect_symbol_name(&args[0])?;
    Ok(Value::Nil)
}

/// `(buffer-local-value VARIABLE BUFFER)` -> return VARIABLE's buffer-local
/// value in BUFFER.
///
/// Stub: falls back to `symbol-value` behavior (obarray lookup).
pub(crate) fn builtin_buffer_local_value(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("buffer-local-value", &args, 2)?;
    let name = expect_symbol_name(&args[0])?;

    // Special constants
    if name == "nil" {
        return Ok(Value::Nil);
    }
    if name == "t" {
        return Ok(Value::True);
    }

    // Fallback to obarray value cell
    match eval.obarray.symbol_value(&name) {
        Some(value) => Ok(value.clone()),
        None => Err(signal("void-variable", vec![Value::symbol(name)])),
    }
}

/// `(makunbound SYMBOL)` -> remove SYMBOL's value binding.
///
/// Returns SYMBOL.
pub(crate) fn builtin_makunbound(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("makunbound", &args, 1)?;
    let name = expect_symbol_name(&args[0])?;

    // Remove from dynamic binding stack
    for frame in eval.dynamic.iter_mut().rev() {
        if frame.remove(&name).is_some() {
            return Ok(args[0].clone());
        }
    }

    // Remove from obarray
    eval.obarray.makunbound(&name);
    Ok(args[0].clone())
}

/// `(fmakunbound SYMBOL)` -> remove SYMBOL's function definition.
///
/// Returns SYMBOL.
pub(crate) fn builtin_fmakunbound(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("fmakunbound", &args, 1)?;
    let name = expect_symbol_name(&args[0])?;
    eval.obarray.fmakunbound(&name);
    Ok(args[0].clone())
}
