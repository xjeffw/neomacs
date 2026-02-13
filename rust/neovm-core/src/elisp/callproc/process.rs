use super::super::eval::Evaluator;
use super::args::{expect_min_args, expect_string};
use super::{signal, EvalResult, Value};

/// `(call-process PROGRAM &optional INFILE DESTINATION DISPLAY &rest ARGS)` -> exit code
///
/// Execute PROGRAM synchronously as a subprocess.
///
/// PROGRAM is the executable name or path (string).
/// INFILE, if non-nil, names a file to use as stdin (currently ignored).
/// DESTINATION controls where output goes:
///   - nil: discard output
///   - t or 0: insert into current buffer (stub: discarded for now)
///   - string: buffer name (stub: discarded for now)
/// DISPLAY, if non-nil, would update the display (ignored).
/// ARGS are additional command-line arguments passed to the program.
///
/// Returns the exit code as an integer, or signals `file-error` on failure.
pub(crate) fn builtin_call_process(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("call-process", &args, 1)?;

    let program = expect_string(&args[0])?;

    // INFILE (args[1]) — accepted but ignored.
    // DESTINATION (args[2]) — accepted but output handling is stubbed.
    // DISPLAY (args[3]) — accepted but ignored.

    // Collect &rest ARGS (indices 4..)
    let cmd_args: Vec<String> = args
        .iter()
        .skip(4)
        .map(|a| match a {
            Value::Str(s) => Ok((**s).clone()),
            Value::Int(n) => Ok(n.to_string()),
            Value::Nil => Ok(String::new()),
            other => Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            )),
        })
        .collect::<Result<Vec<_>, _>>()?;

    let output = std::process::Command::new(&program)
        .args(&cmd_args)
        .output();

    match output {
        Ok(result) => {
            // If DESTINATION is t or integer 0, we would insert stdout into
            // the current buffer.  For now this is stubbed — output is discarded.
            let exit_code = result.status.code().unwrap_or(-1);
            Ok(Value::Int(exit_code as i64))
        }
        Err(err) => Err(signal(
            "file-error",
            vec![
                Value::string("Searching for program"),
                Value::string(err.to_string()),
                Value::string(program),
            ],
        )),
    }
}

/// `(call-process-region START END PROGRAM &optional DELETE DESTINATION DISPLAY &rest ARGS)` -> exit code
///
/// Like `call-process`, but send the region from START to END as stdin.
///
/// Stub implementation: ignores START, END, and DELETE, and delegates to
/// `call-process` with PROGRAM, nil (infile), DESTINATION, DISPLAY, and ARGS.
pub(crate) fn builtin_call_process_region(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("call-process-region", &args, 3)?;

    // args[0] = START (ignored)
    // args[1] = END (ignored)
    // args[2] = PROGRAM
    // args[3] = DELETE (ignored)
    // args[4] = DESTINATION
    // args[5] = DISPLAY
    // args[6..] = ARGS

    // Reconstruct args for call-process: (PROGRAM nil DESTINATION DISPLAY &rest ARGS)
    let mut call_args = Vec::new();
    call_args.push(args[2].clone()); // PROGRAM
    call_args.push(Value::Nil); // INFILE = nil

    // DESTINATION
    if args.len() > 4 {
        call_args.push(args[4].clone());
    } else {
        call_args.push(Value::Nil);
    }

    // DISPLAY
    if args.len() > 5 {
        call_args.push(args[5].clone());
    } else {
        call_args.push(Value::Nil);
    }

    // &rest ARGS
    if args.len() > 6 {
        call_args.extend(args[6..].iter().cloned());
    }

    builtin_call_process(eval, call_args)
}
