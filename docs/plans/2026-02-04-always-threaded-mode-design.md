# Always Threaded Mode Design

## Overview

Migrate neomacs to always use the two-thread architecture, removing the old single-threaded polling path.

**Goals:**
- Replace old init/callback mechanism with threaded mode
- Fix input handling (modifiers, mouse position, function keys)
- Remove dead code (timerfd, callbacks)
- Achieve ~0% CPU when idle (no polling)

## Architecture Changes

### Initialization Flow

**Old flow:**
```
neomacs_open_display()
  → neomacs_display_init(BACKEND_TYPE_WGPU)
  → neomacs_display_get_event_fd()  // timerfd
  → neomacs_term_init()             // registers callback
```

**New flow:**
```
neomacs_open_display()
  → neomacs_display_init_threaded(800, 600, "Emacs")
  → add_read_fd(wakeup_fd, wakeup_handler)
```

### Input Flow

**Old:** Rust calls C callback directly on event
**New:** Render thread sends InputEvent via channel → wakeup pipe → Emacs drains events

### Code Removal

**Rust FFI (remove):**
- `neomacs_display_init()`
- `neomacs_display_get_event_fd()`
- `neomacs_display_set_event_callback()`
- Timerfd infrastructure

**C side (remove):**
- `neomacs_event_callback()`
- `neomacs_term_init()`

**Keep:**
- `neomacs_evq_enqueue()` / `neomacs_evq_flush()` - used by wakeup handler

## Input Handling Improvements

### Modifier Tracking

Add to RenderApp:
```rust
modifiers: u32,  // Current modifier state
```

Handle `WindowEvent::ModifiersChanged`, include in all input events.

### Mouse Position Tracking

Add to RenderApp:
```rust
mouse_pos: (f32, f32),  // Last known cursor position
```

Update on `CursorMoved`, use in `MouseButton` events.

### Function Key Translation

Expand `translate_key()` for:
- F1-F12: 0xffbe-0xffc9
- Insert: 0xff63
- Print: 0xff61
- Modifier keys themselves

## Testing

1. Emacs starts with window
2. Keyboard input works
3. C-x, M-x, C-M-x work
4. Mouse clicks at correct position
5. Window resize works
6. CPU ~0% when idle

## Files Changed

| File | Change |
|------|--------|
| `src/neomacsterm.c` | Replace init, remove callback |
| `rust/.../render_thread.rs` | Add modifier/mouse tracking |
| `rust/.../ffi.rs` | Remove old FFI functions |
| `src/neomacs_display.h` | Update declarations |
