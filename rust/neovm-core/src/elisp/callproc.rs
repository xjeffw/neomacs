//! Process creation builtins from Emacs callproc.c.
//!
//! Provides:
//! - `call-process` -- execute a program synchronously
//! - `call-process-region` -- like call-process with region as stdin (stub)
//! - `getenv-internal` -- retrieve an environment variable
//! - `setenv-internal` -- set an environment variable

use super::error::{signal, EvalResult, Flow};
use super::value::*;

#[path = "callproc/args.rs"]
mod args;
#[path = "callproc/env.rs"]
mod env;
#[path = "callproc/process.rs"]
mod process;

pub(crate) use env::*;
pub(crate) use process::*;

#[cfg(test)]
#[path = "callproc/tests.rs"]
mod tests;
