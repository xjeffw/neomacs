# Two-Thread Architecture Design

## Overview

Replace the current single-threaded 60fps polling architecture with a two-thread event-driven model where winit owns the main loop and Emacs becomes a backend.

**Goals:**
- Performance: GC/Lisp no longer blocks rendering
- Power efficiency: Zero CPU when idle (no 60fps polling)
- Architecture cleanliness: Clear separation of concerns
- Future features: Foundation for smooth animations

**Scope:**
- Full cross-platform: Linux + macOS + Windows
- Big bang migration (design right, implement fully)
- Moderate C-side changes (refactor event loop, keep core redisplay)

## High-Level Architecture

```
┌─────────────────────────────────────┐
│           Emacs Thread              │
│                                     │
│  Owns:                              │
│  • Lisp interpreter                 │
│  • Buffer/text management           │
│  • Redisplay logic                  │
│  • Frame glyph generation           │
│                                     │
│  Does NOT touch:                    │
│  • GPU                              │
│  • Window system                    │
│  • WebKit/GLib                      │
└──────────────┬──────────────────────┘
               │
               │  Channels (lock-free)
               │  • frame_tx: FrameGlyphs
               │  • cmd_tx: Commands
               │  • input_rx: InputEvents
               │  • wakeup_pipe: fd
               │
┌──────────────▼──────────────────────┐
│          Render Thread              │
│                                     │
│  Owns:                              │
│  • winit EventLoop (native run())   │
│  • wgpu Device/Queue                │
│  • GLib MainContext                 │
│  • WebKit views                     │
│  • Video decoders                   │
│  • All GPU resources                │
│                                     │
│  Runs at VSync, never blocks        │
└─────────────────────────────────────┘
```

**Key principle:** Emacs builds *what* to display (frame glyphs), render thread handles *how* to display it (GPU, animations).

## Inter-Thread Communication

Four channels between threads:

```rust
struct ThreadComms {
    // Emacs → Render: frame data
    frame_tx: Sender<FrameGlyphs>,
    frame_rx: Receiver<FrameGlyphs>,

    // Emacs → Render: commands (webkit, video, etc.)
    cmd_tx: Sender<RenderCommand>,
    cmd_rx: Receiver<RenderCommand>,

    // Render → Emacs: input events
    input_tx: Sender<InputEvent>,
    input_rx: Receiver<InputEvent>,

    // Render → Emacs: wakeup signal
    wakeup_pipe: (RawFd, RawFd),  // (write, read)
}
```

**Why pipe for wakeup:**
Emacs uses `select()`/`pselect()` which only works with file descriptors. A pipe lets the render thread wake Emacs instantly.

**Channel choice:** `crossbeam-channel` - lock-free, fast, bounded capacity.

## Input Handling

Winit receives input on render thread, forwards to Emacs:

```
Render thread                    Emacs thread
     │                                │
  key pressed                         │ sleeping in select()
     │                                │
  input_tx.send(key)                  │
  write(wakeup_pipe, [1]) ───────────►│ select() returns!
     │                                │
     │                                │ read(wakeup_pipe)
     │                                │ drain input_rx
     │                                │ process via existing Emacs code
```

Emacs stays in control of input semantics (keybindings, commands). Render thread just captures and forwards raw events.

## Render Thread Main Loop

```rust
fn render_thread(comms: ThreadComms, window: Window) {
    // Own the GLib main context on this thread
    let glib_ctx = glib::MainContext::new();
    glib_ctx.push_thread_default();

    let mut current_frame: Option<FrameGlyphs> = None;

    event_loop.run(|event, elwt| {
        match event {
            // Input → forward to Emacs
            Event::WindowEvent { event: WindowEvent::KeyboardInput { .. }, .. } => {
                comms.input_tx.send(translate_input(event));
                comms.wakeup_pipe.write(&[1]);
            }

            // Check for new frame from Emacs
            Event::AboutToWait => {
                // Drain commands (non-blocking)
                while let Ok(cmd) = comms.cmd_rx.try_recv() {
                    handle_command(cmd);
                }

                // Get latest frame (non-blocking)
                while let Ok(frame) = comms.frame_rx.try_recv() {
                    current_frame = Some(frame);
                }

                // Pump GLib (non-blocking!)
                while glib_ctx.iteration(false) {}

                window.request_redraw();
            }

            // VSync - actual rendering
            Event::RedrawRequested => {
                if let Some(ref frame) = current_frame {
                    render_frame(frame);
                }
                present();
            }
        }
    });
}
```

**Key points:**
- Native VSync via `RedrawRequested`
- Non-blocking GLib via `iteration(false)`
- Latest frame wins (drop stale frames)

## Frame Glyph Passing

Bounded channel with capacity 2 (double buffer):

```rust
pub fn end_frame(frame_glyphs: FrameGlyphs) {
    match comms.frame_tx.try_send(frame_glyphs) {
        Ok(()) => {},
        Err(TrySendError::Full(_)) => {
            // Drop oldest, render thread will get newest
        }
        Err(TrySendError::Disconnected(_)) => {
            // Render thread died
        }
    }
}
```

No shared mutable state. Ownership transfers via channel.

## Animation Model

**Emacs sends state, render thread animates:**

Emacs doesn't know about animations. It just sends current state:

```rust
struct FrameGlyphs {
    glyphs: Vec<FrameGlyph>,
    scroll_y: f32,
    cursor_pos: (f32, f32),
    cursor_visible: bool,
}
```

Render thread detects changes and animates:

```rust
fn on_new_frame(old: &FrameGlyphs, new: &FrameGlyphs) {
    if old.scroll_y != new.scroll_y {
        start_animation(Animation::Scroll {
            from: old.scroll_y,
            to: new.scroll_y,
            duration: 150ms,
        });
    }
}
```

| Animation type | Who handles |
|----------------|-------------|
| Cursor blink | Render thread |
| Smooth scroll | Render thread |
| Window transitions | Render thread |
| Video/WebKit | Render thread |
| Text changes | Emacs (new FrameGlyphs) |

## Emacs C-Side Integration

```c
// New event loop (no timerfd)
while (1) {
    select(kbd_fd, wakeup_pipe_fd, ...);  // event-driven

    if (wakeup_pipe_fd ready) {
        drain_input_from_render_thread();
    }

    handle_keyboard();

    if (need_redisplay) {
        redisplay();
        build_frame_glyphs();
        send_frame_to_render_thread();  // non-blocking
    }
}
```

**New FFI functions:**

```c
int neomacs_display_init_threaded(void);  // returns wakeup_pipe_fd
int neomacs_display_drain_input(NeomacsInputEvent* events, int max);
void neomacs_display_send_frame(FrameGlyphs* glyphs);
void neomacs_display_webkit_load(uint32_t id, const char* url);
```

## WebKit/GLib Integration

GLib main context owned by render thread:

```rust
fn render_thread_init() {
    let glib_ctx = glib::MainContext::new();
    glib_ctx.push_thread_default();
    // All WebKit views use this context
}
```

Buffer rendering happens on same thread - no cross-thread texture transfer.

Heavy work (pixel fallback) offloaded to thread pool:

```rust
fn buffer_rendered_callback(buffer: *mut WpeBuffer) {
    thread_pool.spawn(move || {
        let pixels = extract_pixels(buffer);
        pixel_queue.push(pixels);
    });
}
```

## Startup and Shutdown

**Startup:**
1. Emacs calls `neomacs_display_init_threaded()`
2. Spawn render thread
3. Render thread creates GLib context, winit, window, wgpu
4. Render thread signals ready
5. Emacs receives wakeup_pipe_fd
6. Both threads running

**Shutdown:**
1. Emacs sends Shutdown command
2. Render thread destroys resources, exits event loop
3. Emacs joins thread
4. Close wakeup_pipe
5. Emacs exits

## Platform Considerations

**Wakeup mechanism:**

| Platform | Mechanism |
|----------|-----------|
| Linux | `pipe()` or `eventfd()` |
| macOS | `pipe()` |
| Windows | `Event` or socket pair |

**WebKit portability:**

WPE is Linux-only. Abstract behind trait:

```rust
trait WebViewBackend {
    fn create(id: u32, w: u32, h: u32) -> Self;
    fn load_uri(&self, url: &str);
    fn get_texture(&self) -> Option<&wgpu::Texture>;
}

#[cfg(target_os = "linux")]
type WebView = WpeWebView;

#[cfg(target_os = "macos")]
type WebView = WkWebView;

#[cfg(target_os = "windows")]
type WebView = WebView2;
```

## Summary

| Current | New |
|---------|-----|
| Single thread | Two threads |
| 60fps timerfd polling | Event-driven wakeup pipe |
| GPU on Emacs thread | GPU on render thread |
| GLib on Emacs thread | GLib on render thread |
| Lisp blocks rendering | Rendering independent |
| No VSync | Native VSync |
| No animations | Render-side animations |
