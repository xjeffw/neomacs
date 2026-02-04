# Two-Thread Architecture Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace single-threaded 60fps polling with event-driven two-thread architecture where winit owns the render thread.

**Architecture:** Emacs thread handles Lisp/buffers/redisplay and sends FrameGlyphs via channel. Render thread owns winit event loop, wgpu, GLib/WebKit, runs at native VSync. Communication via lock-free channels + wakeup pipe.

**Tech Stack:** Rust (crossbeam-channel, os_pipe), C (pselect integration), wgpu, winit, GLib

---

## Phase 1: Thread Communication Infrastructure

### Task 1: Add Dependencies

**Files:**
- Modify: `rust/neomacs-display/Cargo.toml`

**Step 1: Add channel and pipe dependencies**

```toml
# Add to [dependencies]
crossbeam-channel = "0.5"
os_pipe = "1.1"
```

**Step 2: Build to verify dependencies resolve**

Run: `cd rust/neomacs-display && cargo build --features "winit-backend"`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add rust/neomacs-display/Cargo.toml
git commit -m "deps: add crossbeam-channel and os_pipe for thread communication"
```

---

### Task 2: Create Thread Communication Types

**Files:**
- Create: `rust/neomacs-display/src/thread_comm.rs`
- Modify: `rust/neomacs-display/src/lib.rs`

**Step 1: Create thread_comm module with types**

```rust
//! Thread communication infrastructure for two-thread architecture.
//!
//! Provides lock-free channels and wakeup mechanism between Emacs and render threads.

use crossbeam_channel::{bounded, Receiver, Sender, TrySendError};
use std::os::unix::io::RawFd;

use crate::core::frame_glyphs::FrameGlyphBuffer;

/// Input event from render thread to Emacs
#[derive(Debug, Clone)]
pub enum InputEvent {
    Key {
        keysym: u32,
        modifiers: u32,
        pressed: bool,
    },
    MouseButton {
        button: u32,
        x: f32,
        y: f32,
        pressed: bool,
        modifiers: u32,
    },
    MouseMove {
        x: f32,
        y: f32,
        modifiers: u32,
    },
    MouseScroll {
        delta_x: f32,
        delta_y: f32,
        x: f32,
        y: f32,
        modifiers: u32,
    },
    WindowResize {
        width: u32,
        height: u32,
    },
    WindowClose,
    WindowFocus {
        focused: bool,
    },
}

/// Command from Emacs to render thread
#[derive(Debug)]
pub enum RenderCommand {
    /// Shutdown the render thread
    Shutdown,
    /// Create a WebKit view
    WebKitCreate { id: u32, width: u32, height: u32 },
    /// Load URL in WebKit view
    WebKitLoadUri { id: u32, url: String },
    /// Resize WebKit view
    WebKitResize { id: u32, width: u32, height: u32 },
    /// Destroy WebKit view
    WebKitDestroy { id: u32 },
    /// Create video player
    VideoCreate { id: u32, path: String },
    /// Control video playback
    VideoPlay { id: u32 },
    VideoPause { id: u32 },
    VideoDestroy { id: u32 },
}

/// Wakeup pipe for signaling Emacs from render thread
pub struct WakeupPipe {
    read_fd: RawFd,
    write_fd: RawFd,
}

impl WakeupPipe {
    /// Create a new wakeup pipe
    pub fn new() -> std::io::Result<Self> {
        let (read, write) = os_pipe::pipe()?;
        use std::os::unix::io::IntoRawFd;
        Ok(Self {
            read_fd: read.into_raw_fd(),
            write_fd: write.into_raw_fd(),
        })
    }

    /// Get the read fd for Emacs to select() on
    pub fn read_fd(&self) -> RawFd {
        self.read_fd
    }

    /// Signal Emacs to wake up (called from render thread)
    pub fn wake(&self) {
        unsafe {
            libc::write(self.write_fd, [1u8].as_ptr() as *const _, 1);
        }
    }

    /// Clear the wakeup signal (called from Emacs thread)
    pub fn clear(&self) {
        let mut buf = [0u8; 64];
        unsafe {
            // Non-blocking read to drain the pipe
            let flags = libc::fcntl(self.read_fd, libc::F_GETFL);
            libc::fcntl(self.read_fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
            while libc::read(self.read_fd, buf.as_mut_ptr() as *mut _, buf.len()) > 0 {}
            libc::fcntl(self.read_fd, libc::F_SETFL, flags);
        }
    }
}

impl Drop for WakeupPipe {
    fn drop(&mut self) {
        unsafe {
            libc::close(self.read_fd);
            libc::close(self.write_fd);
        }
    }
}

/// Channel capacities
const FRAME_CHANNEL_CAPACITY: usize = 2;  // Double buffer
const INPUT_CHANNEL_CAPACITY: usize = 256;
const COMMAND_CHANNEL_CAPACITY: usize = 64;

/// Communication channels between threads
pub struct ThreadComms {
    /// Frame glyphs: Emacs → Render
    pub frame_tx: Sender<FrameGlyphBuffer>,
    pub frame_rx: Receiver<FrameGlyphBuffer>,

    /// Commands: Emacs → Render
    pub cmd_tx: Sender<RenderCommand>,
    pub cmd_rx: Receiver<RenderCommand>,

    /// Input events: Render → Emacs
    pub input_tx: Sender<InputEvent>,
    pub input_rx: Receiver<InputEvent>,

    /// Wakeup pipe: Render → Emacs
    pub wakeup: WakeupPipe,
}

impl ThreadComms {
    /// Create new thread communication channels
    pub fn new() -> std::io::Result<Self> {
        let (frame_tx, frame_rx) = bounded(FRAME_CHANNEL_CAPACITY);
        let (cmd_tx, cmd_rx) = bounded(COMMAND_CHANNEL_CAPACITY);
        let (input_tx, input_rx) = bounded(INPUT_CHANNEL_CAPACITY);
        let wakeup = WakeupPipe::new()?;

        Ok(Self {
            frame_tx,
            frame_rx,
            cmd_tx,
            cmd_rx,
            input_tx,
            input_rx,
            wakeup,
        })
    }

    /// Split into Emacs-side and Render-side handles
    pub fn split(self) -> (EmacsComms, RenderComms) {
        let emacs = EmacsComms {
            frame_tx: self.frame_tx,
            cmd_tx: self.cmd_tx,
            input_rx: self.input_rx,
            wakeup_read_fd: self.wakeup.read_fd(),
            wakeup_clear: WakeupClear { fd: self.wakeup.read_fd },
        };

        let render = RenderComms {
            frame_rx: self.frame_rx,
            cmd_rx: self.cmd_rx,
            input_tx: self.input_tx,
            wakeup: self.wakeup,
        };

        (emacs, render)
    }
}

/// Emacs thread communication handle
pub struct EmacsComms {
    pub frame_tx: Sender<FrameGlyphBuffer>,
    pub cmd_tx: Sender<RenderCommand>,
    pub input_rx: Receiver<InputEvent>,
    pub wakeup_read_fd: RawFd,
    pub wakeup_clear: WakeupClear,
}

/// Handle for clearing wakeup pipe
pub struct WakeupClear {
    fd: RawFd,
}

impl WakeupClear {
    pub fn clear(&self) {
        let mut buf = [0u8; 64];
        unsafe {
            let flags = libc::fcntl(self.fd, libc::F_GETFL);
            libc::fcntl(self.fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
            while libc::read(self.fd, buf.as_mut_ptr() as *mut _, buf.len()) > 0 {}
            libc::fcntl(self.fd, libc::F_SETFL, flags);
        }
    }
}

/// Render thread communication handle
pub struct RenderComms {
    pub frame_rx: Receiver<FrameGlyphBuffer>,
    pub cmd_rx: Receiver<RenderCommand>,
    pub input_tx: Sender<InputEvent>,
    pub wakeup: WakeupPipe,
}

impl RenderComms {
    /// Send input event to Emacs and wake it up
    pub fn send_input(&self, event: InputEvent) {
        if self.input_tx.try_send(event).is_ok() {
            self.wakeup.wake();
        }
    }
}
```

**Step 2: Add module to lib.rs**

In `rust/neomacs-display/src/lib.rs`, add:

```rust
pub mod thread_comm;
```

**Step 3: Build to verify compilation**

Run: `cd rust/neomacs-display && cargo build --features "winit-backend"`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/thread_comm.rs rust/neomacs-display/src/lib.rs
git commit -m "feat: add thread communication infrastructure"
```

---

## Phase 2: Render Thread Implementation

### Task 3: Create Render Thread Module

**Files:**
- Create: `rust/neomacs-display/src/render_thread.rs`
- Modify: `rust/neomacs-display/src/lib.rs`

**Step 1: Create render thread module**

```rust
//! Render thread implementation.
//!
//! Owns winit event loop, wgpu, GLib/WebKit. Runs at native VSync.

use std::sync::Arc;
use std::thread::{self, JoinHandle};
use std::time::Duration;

use winit::application::ApplicationHandler;
use winit::event::{ElementState, KeyEvent, MouseButton, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::keyboard::{Key, NamedKey};
use winit::window::{Window, WindowId};

use crate::thread_comm::{InputEvent, RenderCommand, RenderComms};
use crate::core::frame_glyphs::FrameGlyphBuffer;

/// Render thread state
pub struct RenderThread {
    handle: Option<JoinHandle<()>>,
}

impl RenderThread {
    /// Spawn the render thread
    pub fn spawn(comms: RenderComms, width: u32, height: u32, title: String) -> Self {
        let handle = thread::spawn(move || {
            run_render_loop(comms, width, height, title);
        });

        Self {
            handle: Some(handle),
        }
    }

    /// Wait for render thread to finish
    pub fn join(mut self) {
        if let Some(handle) = self.handle.take() {
            let _ = handle.join();
        }
    }
}

/// Application state for winit event loop
struct RenderApp {
    comms: RenderComms,
    window: Option<Arc<Window>>,
    current_frame: Option<FrameGlyphBuffer>,
    width: u32,
    height: u32,
    title: String,
    // TODO: Add wgpu state, GLib context, etc.
}

impl RenderApp {
    fn new(comms: RenderComms, width: u32, height: u32, title: String) -> Self {
        Self {
            comms,
            window: None,
            current_frame: None,
            width,
            height,
            title,
        }
    }

    /// Process pending commands from Emacs
    fn process_commands(&mut self) -> bool {
        let mut should_exit = false;

        while let Ok(cmd) = self.comms.cmd_rx.try_recv() {
            match cmd {
                RenderCommand::Shutdown => {
                    log::info!("Render thread received shutdown command");
                    should_exit = true;
                }
                RenderCommand::WebKitCreate { id, width, height } => {
                    log::debug!("WebKit create: id={}, {}x{}", id, width, height);
                    // TODO: Create WebKit view
                }
                RenderCommand::WebKitLoadUri { id, url } => {
                    log::debug!("WebKit load: id={}, url={}", id, url);
                    // TODO: Load URL
                }
                RenderCommand::WebKitResize { id, width, height } => {
                    log::debug!("WebKit resize: id={}, {}x{}", id, width, height);
                    // TODO: Resize view
                }
                RenderCommand::WebKitDestroy { id } => {
                    log::debug!("WebKit destroy: id={}", id);
                    // TODO: Destroy view
                }
                RenderCommand::VideoCreate { id, path } => {
                    log::debug!("Video create: id={}, path={}", id, path);
                    // TODO: Create video
                }
                RenderCommand::VideoPlay { id } => {
                    log::debug!("Video play: id={}", id);
                    // TODO: Play video
                }
                RenderCommand::VideoPause { id } => {
                    log::debug!("Video pause: id={}", id);
                    // TODO: Pause video
                }
                RenderCommand::VideoDestroy { id } => {
                    log::debug!("Video destroy: id={}", id);
                    // TODO: Destroy video
                }
            }
        }

        should_exit
    }

    /// Get latest frame from Emacs (non-blocking)
    fn poll_frame(&mut self) {
        // Get the newest frame, discarding older ones
        while let Ok(frame) = self.comms.frame_rx.try_recv() {
            self.current_frame = Some(frame);
        }
    }

    /// Pump GLib events (non-blocking)
    #[cfg(feature = "wpe-webkit")]
    fn pump_glib(&self) {
        // TODO: Implement GLib pumping
        // while glib_ctx.iteration(false) {}
    }

    #[cfg(not(feature = "wpe-webkit"))]
    fn pump_glib(&self) {}

    /// Render the current frame
    fn render(&mut self) {
        if let Some(ref _frame) = self.current_frame {
            // TODO: Implement rendering
            log::trace!("Rendering frame");
        }
    }

    /// Translate winit key to keysym
    fn translate_key(key: &Key) -> u32 {
        match key {
            Key::Named(named) => match named {
                NamedKey::Escape => 0xff1b,
                NamedKey::Enter => 0xff0d,
                NamedKey::Tab => 0xff09,
                NamedKey::Backspace => 0xff08,
                NamedKey::Delete => 0xffff,
                NamedKey::Home => 0xff50,
                NamedKey::End => 0xff57,
                NamedKey::PageUp => 0xff55,
                NamedKey::PageDown => 0xff56,
                NamedKey::ArrowLeft => 0xff51,
                NamedKey::ArrowUp => 0xff52,
                NamedKey::ArrowRight => 0xff53,
                NamedKey::ArrowDown => 0xff54,
                NamedKey::Space => 0x20,
                _ => 0,
            },
            Key::Character(c) => {
                c.chars().next().map(|ch| ch as u32).unwrap_or(0)
            }
            _ => 0,
        }
    }
}

impl ApplicationHandler for RenderApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_none() {
            let attrs = Window::default_attributes()
                .with_title(&self.title)
                .with_inner_size(winit::dpi::PhysicalSize::new(self.width, self.height));

            match event_loop.create_window(attrs) {
                Ok(window) => {
                    log::info!("Render thread: window created");
                    self.window = Some(Arc::new(window));
                    // TODO: Initialize wgpu with this window
                }
                Err(e) => {
                    log::error!("Failed to create window: {:?}", e);
                }
            }
        }
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => {
                log::info!("Window close requested");
                self.comms.send_input(InputEvent::WindowClose);
                event_loop.exit();
            }

            WindowEvent::Resized(size) => {
                self.width = size.width;
                self.height = size.height;
                self.comms.send_input(InputEvent::WindowResize {
                    width: size.width,
                    height: size.height,
                });
                // TODO: Resize wgpu surface
            }

            WindowEvent::Focused(focused) => {
                self.comms.send_input(InputEvent::WindowFocus { focused });
            }

            WindowEvent::KeyboardInput {
                event: KeyEvent {
                    logical_key,
                    state,
                    ..
                },
                ..
            } => {
                let keysym = Self::translate_key(&logical_key);
                if keysym != 0 {
                    self.comms.send_input(InputEvent::Key {
                        keysym,
                        modifiers: 0, // TODO: Track modifiers
                        pressed: state == ElementState::Pressed,
                    });
                }
            }

            WindowEvent::MouseInput { state, button, .. } => {
                let btn = match button {
                    MouseButton::Left => 1,
                    MouseButton::Middle => 2,
                    MouseButton::Right => 3,
                    MouseButton::Back => 4,
                    MouseButton::Forward => 5,
                    MouseButton::Other(n) => n as u32,
                };
                self.comms.send_input(InputEvent::MouseButton {
                    button: btn,
                    x: 0.0, // TODO: Track mouse position
                    y: 0.0,
                    pressed: state == ElementState::Pressed,
                    modifiers: 0,
                });
            }

            WindowEvent::CursorMoved { position, .. } => {
                self.comms.send_input(InputEvent::MouseMove {
                    x: position.x as f32,
                    y: position.y as f32,
                    modifiers: 0,
                });
            }

            WindowEvent::MouseWheel { delta, .. } => {
                let (dx, dy) = match delta {
                    winit::event::MouseScrollDelta::LineDelta(x, y) => (x, y),
                    winit::event::MouseScrollDelta::PixelDelta(pos) => {
                        (pos.x as f32 / 10.0, pos.y as f32 / 10.0)
                    }
                };
                self.comms.send_input(InputEvent::MouseScroll {
                    delta_x: dx,
                    delta_y: dy,
                    x: 0.0,
                    y: 0.0,
                    modifiers: 0,
                });
            }

            WindowEvent::RedrawRequested => {
                self.render();
            }

            _ => {}
        }
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        // Check for shutdown
        if self.process_commands() {
            event_loop.exit();
            return;
        }

        // Get latest frame from Emacs
        self.poll_frame();

        // Pump GLib for WebKit
        self.pump_glib();

        // Request redraw for VSync
        if let Some(ref window) = self.window {
            window.request_redraw();
        }
    }
}

/// Run the render loop (called on render thread)
fn run_render_loop(comms: RenderComms, width: u32, height: u32, title: String) {
    log::info!("Render thread starting");

    let event_loop = EventLoop::new().expect("Failed to create event loop");
    event_loop.set_control_flow(ControlFlow::Poll);

    let mut app = RenderApp::new(comms, width, height, title);

    if let Err(e) = event_loop.run_app(&mut app) {
        log::error!("Event loop error: {:?}", e);
    }

    log::info!("Render thread exiting");
}
```

**Step 2: Add module to lib.rs**

```rust
#[cfg(feature = "winit-backend")]
pub mod render_thread;
```

**Step 3: Build to verify**

Run: `cd rust/neomacs-display && cargo build --features "winit-backend"`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs rust/neomacs-display/src/lib.rs
git commit -m "feat: add render thread with winit event loop"
```

---

### Task 4: Add FFI for Threaded Initialization

**Files:**
- Modify: `rust/neomacs-display/src/ffi.rs`

**Step 1: Add threaded init FFI function**

Add after existing FFI functions:

```rust
use crate::thread_comm::{ThreadComms, EmacsComms, RenderCommand, InputEvent};
use crate::render_thread::RenderThread;

/// Global state for threaded mode
static mut THREADED_STATE: Option<ThreadedState> = None;

struct ThreadedState {
    emacs_comms: EmacsComms,
    render_thread: Option<RenderThread>,
}

/// Initialize display in threaded mode
///
/// Returns the wakeup pipe fd that Emacs should select() on,
/// or -1 on error.
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_init_threaded(
    width: u32,
    height: u32,
    title: *const c_char,
) -> c_int {
    let _ = env_logger::try_init();
    log::info!("neomacs_display_init_threaded: {}x{}", width, height);

    let title = if title.is_null() {
        "Emacs".to_string()
    } else {
        CStr::from_ptr(title).to_string_lossy().into_owned()
    };

    // Create communication channels
    let comms = match ThreadComms::new() {
        Ok(c) => c,
        Err(e) => {
            log::error!("Failed to create thread comms: {:?}", e);
            return -1;
        }
    };

    let wakeup_fd = comms.wakeup.read_fd();
    let (emacs_comms, render_comms) = comms.split();

    // Spawn render thread
    let render_thread = RenderThread::spawn(render_comms, width, height, title);

    THREADED_STATE = Some(ThreadedState {
        emacs_comms,
        render_thread: Some(render_thread),
    });

    wakeup_fd
}

/// Drain input events from render thread
///
/// Returns number of events written to buffer.
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_drain_input(
    events: *mut NeomacsInputEvent,
    max_events: c_int,
) -> c_int {
    let state = match THREADED_STATE.as_ref() {
        Some(s) => s,
        None => return 0,
    };

    // Clear wakeup pipe
    state.emacs_comms.wakeup_clear.clear();

    let mut count = 0;
    while count < max_events {
        match state.emacs_comms.input_rx.try_recv() {
            Ok(event) => {
                let out = &mut *events.add(count as usize);
                match event {
                    InputEvent::Key { keysym, modifiers, pressed } => {
                        out.event_type = if pressed { 2 } else { 3 }; // KeyPress/KeyRelease
                        out.x = 0;
                        out.y = 0;
                        out.button = 0;
                        out.keysym = keysym;
                        out.modifiers = modifiers;
                    }
                    InputEvent::MouseButton { button, x, y, pressed, modifiers } => {
                        out.event_type = if pressed { 4 } else { 5 }; // ButtonPress/ButtonRelease
                        out.x = x as i32;
                        out.y = y as i32;
                        out.button = button;
                        out.keysym = 0;
                        out.modifiers = modifiers;
                    }
                    InputEvent::MouseMove { x, y, modifiers } => {
                        out.event_type = 6; // MotionNotify
                        out.x = x as i32;
                        out.y = y as i32;
                        out.button = 0;
                        out.keysym = 0;
                        out.modifiers = modifiers;
                    }
                    InputEvent::MouseScroll { delta_x, delta_y, x, y, modifiers } => {
                        // Encode scroll as button 4/5 (up/down) or 6/7 (left/right)
                        out.event_type = 4; // ButtonPress
                        out.x = x as i32;
                        out.y = y as i32;
                        out.button = if delta_y > 0.0 { 4 } else if delta_y < 0.0 { 5 }
                                    else if delta_x > 0.0 { 6 } else { 7 };
                        out.keysym = 0;
                        out.modifiers = modifiers;
                    }
                    InputEvent::WindowResize { width, height } => {
                        out.event_type = 22; // ConfigureNotify
                        out.x = width as i32;
                        out.y = height as i32;
                        out.button = 0;
                        out.keysym = 0;
                        out.modifiers = 0;
                    }
                    InputEvent::WindowClose => {
                        out.event_type = 33; // ClientMessage (close)
                        out.x = 0;
                        out.y = 0;
                        out.button = 0;
                        out.keysym = 0;
                        out.modifiers = 0;
                    }
                    InputEvent::WindowFocus { focused } => {
                        out.event_type = if focused { 9 } else { 10 }; // FocusIn/FocusOut
                        out.x = 0;
                        out.y = 0;
                        out.button = 0;
                        out.keysym = 0;
                        out.modifiers = 0;
                    }
                }
                count += 1;
            }
            Err(_) => break,
        }
    }

    count
}

/// Send frame glyphs to render thread
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_send_frame(
    handle: *mut NeomacsDisplay,
) {
    if handle.is_null() {
        return;
    }

    let display = &*handle;

    let state = match THREADED_STATE.as_ref() {
        Some(s) => s,
        None => return,
    };

    // Clone frame glyphs and send to render thread
    let frame = display.frame_glyphs.clone();
    let _ = state.emacs_comms.frame_tx.try_send(frame);
}

/// Send command to render thread
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_send_command(
    cmd_type: c_int,
    id: u32,
    param1: u32,
    param2: u32,
    str_param: *const c_char,
) {
    let state = match THREADED_STATE.as_ref() {
        Some(s) => s,
        None => return,
    };

    let cmd = match cmd_type {
        0 => RenderCommand::Shutdown,
        1 => RenderCommand::WebKitCreate { id, width: param1, height: param2 },
        2 => {
            let url = if str_param.is_null() {
                String::new()
            } else {
                CStr::from_ptr(str_param).to_string_lossy().into_owned()
            };
            RenderCommand::WebKitLoadUri { id, url }
        }
        3 => RenderCommand::WebKitResize { id, width: param1, height: param2 },
        4 => RenderCommand::WebKitDestroy { id },
        _ => return,
    };

    let _ = state.emacs_comms.cmd_tx.try_send(cmd);
}

/// Shutdown threaded display
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_shutdown_threaded() {
    if let Some(mut state) = THREADED_STATE.take() {
        // Send shutdown command
        let _ = state.emacs_comms.cmd_tx.try_send(RenderCommand::Shutdown);

        // Wait for render thread
        if let Some(rt) = state.render_thread.take() {
            rt.join();
        }
    }
}
```

**Step 2: Build to verify**

Run: `cd rust/neomacs-display && cargo build --features "winit-backend"`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add rust/neomacs-display/src/ffi.rs
git commit -m "feat: add FFI for threaded display initialization"
```

---

## Phase 3: Emacs C Integration

### Task 5: Add Threaded Mode to Emacs

**Files:**
- Modify: `src/neomacs_display.c`

**Step 1: Add threaded mode initialization**

Add after existing neomacs_display functions:

```c
/* Threaded mode state */
static int threaded_mode_active = 0;
static int wakeup_fd = -1;

/* Initialize display in threaded mode */
int
neomacs_display_init_threaded_mode (int width, int height, const char *title)
{
  int fd = neomacs_display_init_threaded (width, height, title);
  if (fd < 0)
    return -1;

  wakeup_fd = fd;
  threaded_mode_active = 1;

  /* Add wakeup_fd to Emacs's file descriptor set */
  add_read_fd (wakeup_fd, neomacs_display_wakeup_handler, NULL);

  return 0;
}

/* Handler called when wakeup_fd is readable */
static void
neomacs_display_wakeup_handler (int fd, void *data)
{
  struct NeomacsInputEvent events[64];
  int count;

  /* Drain input events from render thread */
  count = neomacs_display_drain_input (events, 64);

  /* Process events */
  for (int i = 0; i < count; i++)
    {
      struct NeomacsInputEvent *ev = &events[i];
      /* TODO: Convert to Emacs input events and queue them */
      /* This depends on existing neomacs input handling */
    }
}

/* Check if threaded mode is active */
int
neomacs_display_is_threaded (void)
{
  return threaded_mode_active;
}

/* Shutdown threaded mode */
void
neomacs_display_shutdown_threaded_mode (void)
{
  if (!threaded_mode_active)
    return;

  /* Remove wakeup_fd from Emacs's fd set */
  if (wakeup_fd >= 0)
    delete_read_fd (wakeup_fd);

  neomacs_display_shutdown_threaded ();

  wakeup_fd = -1;
  threaded_mode_active = 0;
}
```

**Step 2: Build Emacs to verify**

Run: `make -j8`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add src/neomacs_display.c
git commit -m "feat: add threaded mode integration to Emacs"
```

---

## Phase 4: Wire Up Rendering

### Task 6: Integrate wgpu Renderer with Render Thread

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs`

**Step 1: Add wgpu initialization to RenderApp**

Update the RenderApp struct and resumed() method to initialize wgpu:

```rust
// Add to RenderApp struct:
renderer: Option<WgpuRenderer>,
surface: Option<wgpu::Surface<'static>>,
surface_config: Option<wgpu::SurfaceConfiguration>,
device: Option<wgpu::Device>,
queue: Option<wgpu::Queue>,

// Update resumed() to initialize wgpu after window creation
```

(Full implementation depends on existing wgpu code structure)

**Step 2: Add frame rendering**

Update render() method to use WgpuRenderer::render_frame_glyphs()

**Step 3: Build and test**

Run: `cd rust/neomacs-display && cargo build --features "winit-backend"`

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat: integrate wgpu renderer with render thread"
```

---

## Phase 5: Testing

### Task 7: Create Integration Test

**Files:**
- Create: `rust/neomacs-display/tests/threaded_test.rs`

**Step 1: Write basic integration test**

```rust
//! Integration test for threaded display mode

use neomacs_display::thread_comm::ThreadComms;
use neomacs_display::render_thread::RenderThread;

#[test]
fn test_thread_communication() {
    let comms = ThreadComms::new().expect("Failed to create comms");
    let (emacs, render) = comms.split();

    // Test wakeup
    render.wakeup.wake();
    emacs.wakeup_clear.clear();

    // Test frame sending
    // (would need FrameGlyphBuffer to be constructible)
}

#[test]
fn test_render_thread_lifecycle() {
    let comms = ThreadComms::new().expect("Failed to create comms");
    let (emacs, render) = comms.split();

    // Spawn render thread
    let rt = RenderThread::spawn(render, 800, 600, "Test".to_string());

    // Give it time to start
    std::thread::sleep(std::time::Duration::from_millis(100));

    // Send shutdown
    use neomacs_display::thread_comm::RenderCommand;
    emacs.cmd_tx.send(RenderCommand::Shutdown).unwrap();

    // Join
    rt.join();
}
```

**Step 2: Run tests**

Run: `cd rust/neomacs-display && cargo test --features "winit-backend"`
Expected: Tests pass

**Step 3: Commit**

```bash
git add rust/neomacs-display/tests/threaded_test.rs
git commit -m "test: add integration tests for threaded mode"
```

---

## Summary

| Phase | Tasks | Description |
|-------|-------|-------------|
| 1 | 1-2 | Thread communication infrastructure |
| 2 | 3-4 | Render thread with winit event loop |
| 3 | 5 | Emacs C integration |
| 4 | 6 | Wire up wgpu rendering |
| 5 | 7 | Integration tests |

**After completing these tasks:**
- Two-thread architecture is functional
- Input flows: winit → channel → Emacs
- Frames flow: Emacs → channel → render thread → GPU
- VSync-driven rendering
- No more 60fps polling
