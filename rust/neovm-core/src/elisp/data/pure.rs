use super::helpers::{expect_args, expect_int, expect_string, expect_symbol_name};
use super::{signal, EvalResult, Value};

/// `(symbol-plist SYMBOL)` -> return the property list of SYMBOL.
///
/// Stub: always returns nil (plist support is in the obarray, not exposed
/// as a flat list yet).
pub(crate) fn builtin_symbol_plist(args: Vec<Value>) -> EvalResult {
    expect_args("symbol-plist", &args, 1)?;
    let _name = expect_symbol_name(&args[0])?;
    Ok(Value::Nil)
}

/// `(setplist SYMBOL NEWPLIST)` -> set the property list of SYMBOL to NEWPLIST.
///
/// Stub: returns NEWPLIST without actually modifying anything.
pub(crate) fn builtin_setplist(args: Vec<Value>) -> EvalResult {
    expect_args("setplist", &args, 2)?;
    let _name = expect_symbol_name(&args[0])?;
    Ok(args[1].clone())
}

/// `(symbol-name SYMBOL)` -> return the name of SYMBOL as a string.
pub(crate) fn builtin_symbol_name(args: Vec<Value>) -> EvalResult {
    expect_args("symbol-name", &args, 1)?;
    let name = expect_symbol_name(&args[0])?;
    Ok(Value::string(name))
}

/// `(make-symbol NAME)` -> create a new uninterned symbol with NAME.
///
/// Returns a symbol Value.  In a full implementation this would be truly
/// uninterned; here we return a Symbol value with the given name.
pub(crate) fn builtin_make_symbol(args: Vec<Value>) -> EvalResult {
    expect_args("make-symbol", &args, 1)?;
    let name = expect_string(&args[0])?;
    Ok(Value::Symbol(name))
}

/// `(indirect-variable OBJECT)` -> follow variable aliases.
///
/// Stub: returns OBJECT unchanged (no variable aliasing support).
pub(crate) fn builtin_indirect_variable(args: Vec<Value>) -> EvalResult {
    expect_args("indirect-variable", &args, 1)?;
    Ok(args[0].clone())
}

/// `(subr-name SUBR)` -> return the name of the built-in function SUBR.
pub(crate) fn builtin_subr_name(args: Vec<Value>) -> EvalResult {
    expect_args("subr-name", &args, 1)?;
    match &args[0] {
        Value::Subr(name) => Ok(Value::string(name.clone())),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("subrp"), other.clone()],
        )),
    }
}

/// `(byte-code-function-p OBJECT)` -> t if OBJECT is a byte-compiled function.
pub(crate) fn builtin_byte_code_function_p(args: Vec<Value>) -> EvalResult {
    expect_args("byte-code-function-p", &args, 1)?;
    Ok(Value::bool(matches!(&args[0], Value::ByteCode(_))))
}

/// `(module-function-p OBJECT)` -> t if OBJECT is a module function.
///
/// Always nil: no module function support.
pub(crate) fn builtin_module_function_p(args: Vec<Value>) -> EvalResult {
    expect_args("module-function-p", &args, 1)?;
    Ok(Value::Nil)
}

/// `(number-or-marker-p VALUE)` -> t if VALUE is a number (or marker).
///
/// We have no marker type, so this is equivalent to `numberp`.
pub(crate) fn builtin_number_or_marker_p(args: Vec<Value>) -> EvalResult {
    expect_args("number-or-marker-p", &args, 1)?;
    Ok(Value::bool(args[0].is_number()))
}

/// `(integer-or-marker-p VALUE)` -> t if VALUE is an integer (or marker).
///
/// We have no marker type, so this is equivalent to `integerp`.
pub(crate) fn builtin_integer_or_marker_p(args: Vec<Value>) -> EvalResult {
    expect_args("integer-or-marker-p", &args, 1)?;
    Ok(Value::bool(args[0].is_integer()))
}

/// `(natnump VALUE)` -> t if VALUE is a non-negative integer.
pub(crate) fn builtin_natnump(args: Vec<Value>) -> EvalResult {
    expect_args("natnump", &args, 1)?;
    let is_nat = match &args[0] {
        Value::Int(n) => *n >= 0,
        _ => false,
    };
    Ok(Value::bool(is_nat))
}

/// `(fixnump VALUE)` -> t if VALUE is a fixnum (same as integerp for us).
pub(crate) fn builtin_fixnump(args: Vec<Value>) -> EvalResult {
    expect_args("fixnump", &args, 1)?;
    Ok(Value::bool(args[0].is_integer()))
}

/// `(bignump VALUE)` -> t if VALUE is a bignum.
///
/// Always nil: no bignum support.
pub(crate) fn builtin_bignump(args: Vec<Value>) -> EvalResult {
    expect_args("bignump", &args, 1)?;
    Ok(Value::Nil)
}

/// `(bare-symbol SYM)` -> return SYM itself (strip position if any).
///
/// We have no symbol-with-pos, so this is identity.
pub(crate) fn builtin_bare_symbol(args: Vec<Value>) -> EvalResult {
    expect_args("bare-symbol", &args, 1)?;
    Ok(args[0].clone())
}

/// `(symbol-with-pos-p OBJECT)` -> t if OBJECT is a symbol with position.
///
/// Always nil: no symbol-with-pos support.
pub(crate) fn builtin_symbol_with_pos_p(args: Vec<Value>) -> EvalResult {
    expect_args("symbol-with-pos-p", &args, 1)?;
    Ok(Value::Nil)
}

/// `(remove-pos-from-symbol SYM)` -> return SYM without position info.
///
/// Identity: no symbol-with-pos support.
pub(crate) fn builtin_remove_pos_from_symbol(args: Vec<Value>) -> EvalResult {
    expect_args("remove-pos-from-symbol", &args, 1)?;
    Ok(args[0].clone())
}

/// `(logcount INTEGER)` -> count the number of 1-bits in INTEGER.
///
/// For negative integers, counts the number of 0-bits in the two's complement
/// representation (matching Emacs behavior: `logcount` of a negative number
/// is the same as `logcount` of its bitwise complement).
pub(crate) fn builtin_logcount(args: Vec<Value>) -> EvalResult {
    expect_args("logcount", &args, 1)?;
    let n = expect_int(&args[0])?;
    let count = if n >= 0 {
        n.count_ones()
    } else {
        // For negative numbers, Emacs counts 0-bits (i.e. complement)
        (!n).count_ones()
    };
    Ok(Value::Int(count as i64))
}
