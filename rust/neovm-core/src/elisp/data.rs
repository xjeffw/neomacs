//! Variable management and type operations from Emacs `data.c`.
//!
//! Provides:
//! - Variable access: `boundp`, `symbol-value`, `set`, `set-default`, `default-value`,
//!   `makunbound`
//! - Function access: `fboundp`, `symbol-function`, `fset`, `fmakunbound`
//! - Buffer-local stubs: `make-variable-buffer-local`, `make-local-variable`,
//!   `kill-local-variable`, `local-variable-p`, `buffer-local-value`
//! - Symbol utilities: `symbol-name`, `symbol-plist`, `setplist`, `make-symbol`,
//!   `indirect-variable`, `bare-symbol`, `symbol-with-pos-p`, `remove-pos-from-symbol`
//! - Type predicates: `number-or-marker-p`, `integer-or-marker-p`, `natnump`,
//!   `fixnump`, `bignump`, `byte-code-function-p`, `module-function-p`
//! - Subr utilities: `subr-name`
//! - Bit operations: `logcount`

use super::error::{signal, EvalResult, Flow};
use super::value::*;

#[path = "data/eval_stateful.rs"]
mod eval_stateful;
#[path = "data/helpers.rs"]
mod helpers;
#[path = "data/pure.rs"]
mod pure;

pub(crate) use eval_stateful::*;
pub(crate) use pure::*;

#[cfg(test)]
#[path = "data/tests.rs"]
mod tests;
