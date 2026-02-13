//! Input/event handling builtins from Emacs keyboard.c.
//!
//! Provides stub implementations for keyboard and input-related primitives:
//! - Key reading: `read-key-sequence`, `read-key-sequence-vector`
//! - Command keys: `this-command-keys`, `this-command-keys-vector`,
//!   `this-single-command-keys`, `this-single-command-raw-keys`
//! - Command execution: `execute-extended-command`, `command-execute`
//! - Input mode: `set-input-mode`, `current-input-mode`, `set-input-interrupt-mode`
//! - Dribble: `open-dribble-file`
//! - Event info: `recent-keys`, `input-pending-p`, `discard-input`,
//!   `event-convert-list`, `internal-event-symbol-parse-modifiers`
//! - Recursion: `recursion-depth`, `top-level`, `exit-recursive-edit`,
//!   `abort-recursive-edit`
//! - Idle: `current-idle-time`
//! - Position: `posn-at-x-y`, `posn-at-point`
//! - Misc: `reset-this-command-lengths`, `clear-this-command-keys`

use super::error::{signal, EvalResult, Flow};
use super::value::*;

#[path = "keyboard/args.rs"]
mod args;
#[path = "keyboard/eval_stateful.rs"]
mod eval_stateful;
#[path = "keyboard/pure.rs"]
mod pure;

pub(crate) use eval_stateful::*;
pub(crate) use pure::*;

#[cfg(test)]
#[path = "keyboard/tests.rs"]
mod tests;
