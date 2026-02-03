# Winit Text Rendering Design

## Problem

Winit windows render backgrounds and cursors but no text. The `WgpuRenderer` only has a `rect_pipeline` for drawing rectangles. The existing `WgpuGlyphAtlas` and `glyph.wgsl` shader are not integrated.

## Solution

Use the `frame_glyphs` buffer for winit windows (same as GTK path), then render it via wgpu instead of GTK widgets.

## Data Flow

```
Emacs redisplay → add_char_glyph() → frame_glyphs buffer → render_frame_glyphs() → wgpu surface
```

## Changes

### 1. Renderer (`renderer.rs`)

Add `glyph_pipeline` to `WgpuRenderer`:

```rust
pub struct WgpuRenderer {
    // existing...
    rect_pipeline: wgpu::RenderPipeline,
    glyph_pipeline: wgpu::RenderPipeline,  // NEW
}
```

Add `render_frame_glyphs()` method:

```rust
pub fn render_frame_glyphs(
    &self,
    view: &wgpu::TextureView,
    frame_glyphs: &FrameGlyphBuffer,
    glyph_atlas: &mut WgpuGlyphAtlas,
    faces: &HashMap<u32, Face>,
)
```

Rendering order:
1. Scene background
2. Window backgrounds (`FrameGlyph::Background`)
3. Stretch glyphs (whitespace)
4. Character glyphs (text via atlas)
5. Cursors and borders

### 2. Backend (`backend.rs`)

Add glyph atlas to `WgpuBackend`:

```rust
pub struct WgpuBackend {
    // existing...
    glyph_atlas: Option<WgpuGlyphAtlas>,
}
```

Initialize in `init_wgpu_headless()`.

Update `end_frame_for_window()` to call `render_frame_glyphs()`.

### 3. FFI (`ffi.rs`)

Revert hybrid bypass - remove `&& display.current_render_window_id == 0` conditions.

Add faces storage:

```rust
pub struct NeomacsDisplay {
    // existing...
    faces: HashMap<u32, Face>,
}
```

Update `neomacs_display_end_frame_window()` to pass `frame_glyphs` and `faces` to backend.

## Files Modified

- `rust/neomacs-display/src/backend/wgpu/renderer.rs`
- `rust/neomacs-display/src/backend/wgpu/backend.rs`
- `rust/neomacs-display/src/ffi.rs`

## Testing

1. Build: `./configure --with-neomacs && make`
2. Run: `src/emacs -Q`
3. Verify: text visible in scratch buffer, modeline, echo area
4. Verify: cursor renders correctly
5. Verify: bold/italic faces render correctly

## Success Criteria

- Text renders in winit window
- Modeline and echo area visible
- Cursor visible and correctly positioned
- No visual artifacts
