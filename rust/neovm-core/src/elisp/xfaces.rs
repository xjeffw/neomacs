//! Face/attribute management builtins from Emacs xfaces.c.
//!
//! Provides stub implementations of face-related primitives:
//! - `internal-make-lisp-face`, `internal-get-lisp-face-attribute`,
//!   `internal-set-lisp-face-attribute`, `internal-lisp-face-equal-p`,
//!   `internal-lisp-face-empty-p`, `internal-lisp-face-p`,
//!   `internal-copy-lisp-face`, `internal-merge-in-global-face`
//! - `face-font`, `face-attribute-relative-p`, `face-attributes-as-vector`,
//!   `merge-face-attribute`
//! - `x-family-fonts`, `x-list-fonts`
//! - `color-distance`, `color-gray-p`, `color-supported-p`, `color-values`
//! - `display-supports-face-attributes-p`, `internal-face-x-get-resource`,
//!   `tty-suppress-bold-inverse-default-colors`, `dump-colors`,
//!   `face-spec-set-match-display`
//! - `internal-set-font-selection-order`,
//!   `internal-set-alternative-font-family-alist`,
//!   `internal-set-alternative-font-registry-alist`

use super::error::{signal, EvalResult, Flow};
use super::value::*;

#[path = "xfaces/args.rs"]
mod args;
#[path = "xfaces/builtins.rs"]
mod builtins;

pub(crate) use builtins::*;

#[cfg(test)]
#[path = "xfaces/tests.rs"]
mod tests;
