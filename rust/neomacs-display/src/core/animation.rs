//! Animation system for smooth scrolling, cursor blink, etc.

use std::time::{Duration, Instant};

/// Easing functions for animations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Easing {
    Linear,
    EaseIn,
    EaseOut,
    EaseInOut,
}

impl Easing {
    /// Apply easing function to a value t in [0, 1]
    pub fn apply(&self, t: f32) -> f32 {
        let t = t.clamp(0.0, 1.0);
        match self {
            Easing::Linear => t,
            Easing::EaseIn => t * t,
            Easing::EaseOut => 1.0 - (1.0 - t) * (1.0 - t),
            Easing::EaseInOut => {
                if t < 0.5 {
                    2.0 * t * t
                } else {
                    1.0 - (-2.0 * t + 2.0).powi(2) / 2.0
                }
            }
        }
    }
}

/// A single animation
#[derive(Debug, Clone)]
pub struct Animation {
    /// Start value
    pub from: f32,

    /// End value
    pub to: f32,

    /// Duration
    pub duration: Duration,

    /// Start time
    pub start_time: Instant,

    /// Easing function
    pub easing: Easing,

    /// Is this animation complete?
    pub completed: bool,
}

impl Animation {
    /// Create a new animation
    pub fn new(from: f32, to: f32, duration: Duration, easing: Easing) -> Self {
        Self {
            from,
            to,
            duration,
            start_time: Instant::now(),
            easing,
            completed: false,
        }
    }

    /// Get current value at time `now`
    pub fn value_at(&mut self, now: Instant) -> f32 {
        let elapsed = now.duration_since(self.start_time);

        if elapsed >= self.duration {
            self.completed = true;
            return self.to;
        }

        let t = elapsed.as_secs_f32() / self.duration.as_secs_f32();
        let eased_t = self.easing.apply(t);

        self.from + (self.to - self.from) * eased_t
    }

    /// Get current value (using current time)
    pub fn current_value(&mut self) -> f32 {
        self.value_at(Instant::now())
    }

    /// Check if animation is complete
    pub fn is_complete(&self) -> bool {
        self.completed
    }
}

/// Animation manager handles all active animations
#[derive(Debug)]
pub struct AnimationManager {
    /// Scroll animations by window ID
    scroll_animations: Vec<(i32, Animation)>,

    /// Cursor blink state
    cursor_blink_on: bool,
    last_cursor_toggle: Instant,
    cursor_blink_interval: Duration,

    /// Frame time tracking
    last_frame_time: Option<Instant>,
}

impl Default for AnimationManager {
    fn default() -> Self {
        Self::new()
    }
}

impl AnimationManager {
    pub fn new() -> Self {
        Self {
            scroll_animations: Vec::new(),
            cursor_blink_on: true,
            last_cursor_toggle: Instant::now(),
            cursor_blink_interval: Duration::from_millis(530),
            last_frame_time: None,
        }
    }

    /// Start a smooth scroll animation for a window
    pub fn animate_scroll(&mut self, window_id: i32, from: f32, to: f32) {
        // Remove any existing scroll animation for this window
        self.scroll_animations.retain(|(id, _)| *id != window_id);

        let animation = Animation::new(
            from,
            to,
            Duration::from_millis(150),
            Easing::EaseOut,
        );

        self.scroll_animations.push((window_id, animation));
    }

    /// Get current scroll offset for a window (returns None if no animation)
    pub fn get_scroll_offset(&mut self, window_id: i32) -> Option<f32> {
        let now = Instant::now();

        for (id, anim) in &mut self.scroll_animations {
            if *id == window_id {
                return Some(anim.value_at(now));
            }
        }

        None
    }

    /// Update all animations, returns true if any animation is active
    pub fn tick(&mut self) -> bool {
        let now = Instant::now();
        self.last_frame_time = Some(now);

        // Update cursor blink
        if now.duration_since(self.last_cursor_toggle) >= self.cursor_blink_interval {
            self.cursor_blink_on = !self.cursor_blink_on;
            self.last_cursor_toggle = now;
        }

        // Remove completed scroll animations
        self.scroll_animations.retain(|(_, anim)| !anim.is_complete());

        // Return true if there are active animations
        !self.scroll_animations.is_empty()
    }

    /// Get cursor visibility (for blinking)
    pub fn cursor_visible(&self) -> bool {
        self.cursor_blink_on
    }

    /// Reset cursor blink (call when cursor moves)
    pub fn reset_cursor_blink(&mut self) {
        self.cursor_blink_on = true;
        self.last_cursor_toggle = Instant::now();
    }

    /// Set cursor blink interval
    pub fn set_cursor_blink_interval(&mut self, interval: Duration) {
        self.cursor_blink_interval = interval;
    }

    /// Check if any animations are running
    pub fn has_active_animations(&self) -> bool {
        !self.scroll_animations.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread::sleep;

    #[test]
    fn test_easing() {
        assert_eq!(Easing::Linear.apply(0.5), 0.5);
        assert!(Easing::EaseIn.apply(0.5) < 0.5);
        assert!(Easing::EaseOut.apply(0.5) > 0.5);
    }

    #[test]
    fn test_animation() {
        let mut anim = Animation::new(0.0, 100.0, Duration::from_millis(100), Easing::Linear);

        // At start
        let v1 = anim.current_value();
        assert!(v1 < 50.0);

        // Wait and check progress
        sleep(Duration::from_millis(50));
        let v2 = anim.current_value();
        assert!(v2 > v1);

        // Wait until complete
        sleep(Duration::from_millis(60));
        let v3 = anim.current_value();
        assert_eq!(v3, 100.0);
        assert!(anim.is_complete());
    }

    // ----------------------------------------------------------------
    // Easing function tests
    // ----------------------------------------------------------------

    #[test]
    fn test_easing_boundary_values() {
        // All easing functions must map 0 -> 0 and 1 -> 1
        for easing in &[Easing::Linear, Easing::EaseIn, Easing::EaseOut, Easing::EaseInOut] {
            let at_zero = easing.apply(0.0);
            let at_one = easing.apply(1.0);
            assert!(
                (at_zero - 0.0).abs() < 1e-6,
                "{:?}.apply(0.0) = {}, expected 0.0",
                easing,
                at_zero
            );
            assert!(
                (at_one - 1.0).abs() < 1e-6,
                "{:?}.apply(1.0) = {}, expected 1.0",
                easing,
                at_one
            );
        }
    }

    #[test]
    fn test_easing_clamping() {
        // Values outside [0, 1] should be clamped
        for easing in &[Easing::Linear, Easing::EaseIn, Easing::EaseOut, Easing::EaseInOut] {
            let below = easing.apply(-0.5);
            let above = easing.apply(1.5);
            assert!(
                (below - easing.apply(0.0)).abs() < 1e-6,
                "{:?}.apply(-0.5) = {}, expected same as apply(0.0)",
                easing,
                below
            );
            assert!(
                (above - easing.apply(1.0)).abs() < 1e-6,
                "{:?}.apply(1.5) = {}, expected same as apply(1.0)",
                easing,
                above
            );
        }
    }

    #[test]
    fn test_easing_linear_midpoint() {
        assert!((Easing::Linear.apply(0.25) - 0.25).abs() < 1e-6);
        assert!((Easing::Linear.apply(0.5) - 0.5).abs() < 1e-6);
        assert!((Easing::Linear.apply(0.75) - 0.75).abs() < 1e-6);
    }

    #[test]
    fn test_easing_ease_in_curve_shape() {
        // EaseIn (t^2) starts slow, ends fast
        // At t=0.25: 0.0625, at t=0.5: 0.25, at t=0.75: 0.5625
        assert!((Easing::EaseIn.apply(0.25) - 0.0625).abs() < 1e-6);
        assert!((Easing::EaseIn.apply(0.5) - 0.25).abs() < 1e-6);
        assert!((Easing::EaseIn.apply(0.75) - 0.5625).abs() < 1e-6);
    }

    #[test]
    fn test_easing_ease_out_curve_shape() {
        // EaseOut: 1 - (1-t)^2 starts fast, ends slow
        // At t=0.25: 1 - 0.75^2 = 0.4375
        // At t=0.5: 1 - 0.5^2 = 0.75
        // At t=0.75: 1 - 0.25^2 = 0.9375
        assert!((Easing::EaseOut.apply(0.25) - 0.4375).abs() < 1e-6);
        assert!((Easing::EaseOut.apply(0.5) - 0.75).abs() < 1e-6);
        assert!((Easing::EaseOut.apply(0.75) - 0.9375).abs() < 1e-6);
    }

    #[test]
    fn test_easing_ease_in_out_symmetry() {
        // EaseInOut should be symmetric around (0.5, 0.5)
        let at_half = Easing::EaseInOut.apply(0.5);
        assert!(
            (at_half - 0.5).abs() < 1e-6,
            "EaseInOut at 0.5 = {}, expected 0.5",
            at_half
        );

        // Symmetry: apply(t) + apply(1-t) should equal 1
        for &t in &[0.1, 0.2, 0.3, 0.4] {
            let sum = Easing::EaseInOut.apply(t) + Easing::EaseInOut.apply(1.0 - t);
            assert!(
                (sum - 1.0).abs() < 1e-5,
                "EaseInOut symmetry broken: apply({}) + apply({}) = {}",
                t,
                1.0 - t,
                sum
            );
        }
    }

    #[test]
    fn test_easing_monotonicity() {
        // All easing functions should be monotonically non-decreasing on [0, 1]
        for easing in &[Easing::Linear, Easing::EaseIn, Easing::EaseOut, Easing::EaseInOut] {
            let mut prev = easing.apply(0.0);
            for i in 1..=100 {
                let t = i as f32 / 100.0;
                let val = easing.apply(t);
                assert!(
                    val >= prev - 1e-6,
                    "{:?} not monotonic: apply({}) = {} < apply({}) = {}",
                    easing,
                    t,
                    val,
                    (i - 1) as f32 / 100.0,
                    prev
                );
                prev = val;
            }
        }
    }

    // ----------------------------------------------------------------
    // Animation tests
    // ----------------------------------------------------------------

    #[test]
    fn test_animation_new_initial_state() {
        let anim = Animation::new(10.0, 50.0, Duration::from_millis(200), Easing::EaseOut);
        assert_eq!(anim.from, 10.0);
        assert_eq!(anim.to, 50.0);
        assert_eq!(anim.duration, Duration::from_millis(200));
        assert_eq!(anim.easing, Easing::EaseOut);
        assert!(!anim.completed);
    }

    #[test]
    fn test_animation_value_at_start_time() {
        let mut anim = Animation::new(0.0, 100.0, Duration::from_secs(1), Easing::Linear);
        // Query at the exact start time should return the from value
        let val = anim.value_at(anim.start_time);
        assert!(
            (val - 0.0).abs() < 1e-6,
            "value_at(start_time) = {}, expected 0.0",
            val
        );
        assert!(!anim.is_complete());
    }

    #[test]
    fn test_animation_value_at_end_time() {
        let mut anim = Animation::new(0.0, 100.0, Duration::from_secs(1), Easing::Linear);
        let end_time = anim.start_time + Duration::from_secs(1);
        let val = anim.value_at(end_time);
        assert_eq!(val, 100.0);
        assert!(anim.is_complete());
    }

    #[test]
    fn test_animation_value_past_end_time() {
        let mut anim = Animation::new(0.0, 100.0, Duration::from_millis(50), Easing::Linear);
        let way_after = anim.start_time + Duration::from_secs(10);
        let val = anim.value_at(way_after);
        assert_eq!(val, 100.0);
        assert!(anim.is_complete());
    }

    #[test]
    fn test_animation_linear_midpoint_value() {
        let mut anim = Animation::new(0.0, 200.0, Duration::from_secs(2), Easing::Linear);
        let mid = anim.start_time + Duration::from_secs(1);
        let val = anim.value_at(mid);
        assert!(
            (val - 100.0).abs() < 1.0,
            "Linear midpoint: expected ~100.0, got {}",
            val
        );
    }

    #[test]
    fn test_animation_negative_range() {
        // Animation can go from high to low
        let mut anim = Animation::new(100.0, 0.0, Duration::from_secs(1), Easing::Linear);
        let mid = anim.start_time + Duration::from_millis(500);
        let val = anim.value_at(mid);
        assert!(
            (val - 50.0).abs() < 1.0,
            "Reverse animation midpoint: expected ~50.0, got {}",
            val
        );

        let end = anim.start_time + Duration::from_secs(1);
        let val_end = anim.value_at(end);
        assert_eq!(val_end, 0.0);
    }

    #[test]
    fn test_animation_zero_duration() {
        // A zero-duration animation should immediately complete
        let mut anim = Animation::new(0.0, 42.0, Duration::from_millis(0), Easing::Linear);
        let val = anim.value_at(anim.start_time);
        assert_eq!(val, 42.0);
        assert!(anim.is_complete());
    }

    #[test]
    fn test_animation_same_from_to() {
        // Animation where from == to should always return that value
        let mut anim = Animation::new(77.0, 77.0, Duration::from_secs(1), Easing::EaseInOut);
        let mid = anim.start_time + Duration::from_millis(500);
        let val = anim.value_at(mid);
        assert!(
            (val - 77.0).abs() < 1e-6,
            "Same from/to: expected 77.0, got {}",
            val
        );
    }

    #[test]
    fn test_animation_completed_flag_stays_set() {
        let mut anim = Animation::new(0.0, 10.0, Duration::from_millis(10), Easing::Linear);
        assert!(!anim.is_complete());

        // Complete it
        let after = anim.start_time + Duration::from_millis(20);
        anim.value_at(after);
        assert!(anim.is_complete());

        // Calling value_at again, even with an earlier time, should keep completed=true
        // because completed is a one-way flag set by value_at
        let val = anim.value_at(after);
        assert!(anim.is_complete());
        assert_eq!(val, 10.0);
    }

    // ----------------------------------------------------------------
    // AnimationManager tests
    // ----------------------------------------------------------------

    #[test]
    fn test_animation_manager_new_state() {
        let mgr = AnimationManager::new();
        assert!(mgr.cursor_visible());
        assert!(!mgr.has_active_animations());
        assert_eq!(mgr.cursor_blink_interval, Duration::from_millis(530));
        assert!(mgr.last_frame_time.is_none());
    }

    #[test]
    fn test_animation_manager_default_matches_new() {
        let mgr_new = AnimationManager::new();
        let mgr_default = AnimationManager::default();
        assert_eq!(mgr_new.cursor_blink_on, mgr_default.cursor_blink_on);
        assert_eq!(
            mgr_new.cursor_blink_interval,
            mgr_default.cursor_blink_interval
        );
        assert_eq!(
            mgr_new.scroll_animations.len(),
            mgr_default.scroll_animations.len()
        );
        assert_eq!(mgr_new.last_frame_time, mgr_default.last_frame_time);
    }

    #[test]
    fn test_animate_scroll_creates_animation() {
        let mut mgr = AnimationManager::new();
        assert!(!mgr.has_active_animations());

        mgr.animate_scroll(1, 0.0, 100.0);
        assert!(mgr.has_active_animations());
    }

    #[test]
    fn test_get_scroll_offset_returns_none_for_unknown_window() {
        let mut mgr = AnimationManager::new();
        assert!(mgr.get_scroll_offset(999).is_none());
    }

    #[test]
    fn test_get_scroll_offset_returns_value_for_active_animation() {
        let mut mgr = AnimationManager::new();
        mgr.animate_scroll(1, 0.0, 100.0);

        let offset = mgr.get_scroll_offset(1);
        assert!(offset.is_some());
        // Just started, value should be close to 0 (the from value)
        let val = offset.unwrap();
        assert!(val >= 0.0 && val <= 100.0);
    }

    #[test]
    fn test_animate_scroll_replaces_existing_for_same_window() {
        let mut mgr = AnimationManager::new();
        mgr.animate_scroll(1, 0.0, 50.0);
        mgr.animate_scroll(1, 50.0, 200.0);

        // Should only have one animation for window 1
        assert_eq!(
            mgr.scroll_animations.len(),
            1,
            "Expected exactly 1 animation after replacement"
        );
        assert_eq!(mgr.scroll_animations[0].0, 1);
        assert_eq!(mgr.scroll_animations[0].1.from, 50.0);
        assert_eq!(mgr.scroll_animations[0].1.to, 200.0);
    }

    #[test]
    fn test_multiple_concurrent_scroll_animations() {
        let mut mgr = AnimationManager::new();
        mgr.animate_scroll(1, 0.0, 100.0);
        mgr.animate_scroll(2, 50.0, 200.0);
        mgr.animate_scroll(3, 10.0, 30.0);

        assert_eq!(mgr.scroll_animations.len(), 3);
        assert!(mgr.has_active_animations());

        // Each window should return its own offset
        assert!(mgr.get_scroll_offset(1).is_some());
        assert!(mgr.get_scroll_offset(2).is_some());
        assert!(mgr.get_scroll_offset(3).is_some());
        assert!(mgr.get_scroll_offset(4).is_none());
    }

    #[test]
    fn test_tick_removes_completed_animations() {
        let mut mgr = AnimationManager::new();
        mgr.animate_scroll(1, 0.0, 100.0);

        // Wait for the scroll animation (150ms) to complete
        sleep(Duration::from_millis(200));

        // Force completion by reading the value (which sets completed flag)
        let _ = mgr.get_scroll_offset(1);

        // tick should prune completed animations
        let has_active = mgr.tick();
        assert!(!has_active);
        assert!(!mgr.has_active_animations());
    }

    #[test]
    fn test_tick_sets_last_frame_time() {
        let mut mgr = AnimationManager::new();
        assert!(mgr.last_frame_time.is_none());

        mgr.tick();
        assert!(mgr.last_frame_time.is_some());
    }

    #[test]
    fn test_tick_returns_true_with_active_animations() {
        let mut mgr = AnimationManager::new();
        mgr.animate_scroll(1, 0.0, 100.0);

        let has_active = mgr.tick();
        assert!(has_active, "tick() should return true when animations are active");
    }

    #[test]
    fn test_cursor_blink_initial_visibility() {
        let mgr = AnimationManager::new();
        assert!(mgr.cursor_visible(), "Cursor should be visible initially");
    }

    #[test]
    fn test_cursor_blink_toggles_after_interval() {
        let mut mgr = AnimationManager::new();
        mgr.set_cursor_blink_interval(Duration::from_millis(50));
        assert!(mgr.cursor_visible());

        sleep(Duration::from_millis(60));
        mgr.tick();

        assert!(
            !mgr.cursor_visible(),
            "Cursor should toggle off after interval"
        );

        sleep(Duration::from_millis(60));
        mgr.tick();

        assert!(
            mgr.cursor_visible(),
            "Cursor should toggle back on after another interval"
        );
    }

    #[test]
    fn test_reset_cursor_blink_makes_visible() {
        let mut mgr = AnimationManager::new();
        mgr.set_cursor_blink_interval(Duration::from_millis(50));

        // Wait for blink to toggle off
        sleep(Duration::from_millis(60));
        mgr.tick();
        assert!(!mgr.cursor_visible());

        // Reset should make it visible again
        mgr.reset_cursor_blink();
        assert!(
            mgr.cursor_visible(),
            "Cursor should be visible after reset"
        );
    }

    #[test]
    fn test_set_cursor_blink_interval() {
        let mut mgr = AnimationManager::new();
        assert_eq!(mgr.cursor_blink_interval, Duration::from_millis(530));

        mgr.set_cursor_blink_interval(Duration::from_millis(1000));
        assert_eq!(mgr.cursor_blink_interval, Duration::from_millis(1000));
    }
}
