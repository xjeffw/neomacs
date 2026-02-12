//! Garbage Collector for the NeoVM Elisp runtime.
//!
//! # Architecture
//!
//! Semi-space copying collector with bump allocation:
//!
//! - **Two spaces**: `from_space` (active) and `to_space` (reserve).
//!   During GC, live objects are copied from `from_space` to `to_space`,
//!   then spaces are swapped.
//!
//! - **Bump allocation**: Objects allocated by incrementing a pointer in
//!   `from_space`. No per-object headers for allocation. Extremely fast.
//!
//! - **Precise root scanning**: The evaluator registers roots (stack frames,
//!   globals) with the GC. No conservative scanning.
//!
//! - **Compacting by design**: Copying collector inherently compacts memory,
//!   improving cache locality.
//!
//! - **No per-object locks**: Single-threaded within an isolate. Write barriers
//!   not needed for single-generation; added later for generational.
//!
//! # Object Layout
//!
//! Each GC-managed object has a header:
//! ```text
//! [tag: u8][forwarded: bool][size: u32][... payload ...]
//! ```
//!
//! Tags: Cons=0, String=1, Vector=2, Lambda=3, HashTable=4, Symbol=5
//!
//! During GC, `forwarded` is set and the payload is replaced with a forwarding
//! pointer to the new location in `to_space`.

pub mod heap;
pub mod types;

pub use heap::{GcHeap, GcRef};
pub use types::GcTag;
