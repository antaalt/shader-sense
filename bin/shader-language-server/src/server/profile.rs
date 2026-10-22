//! Small and easy to use profiler
use std::time::Instant;

use log::info;

/// macro to define a profiler in scope and keep it until its dropped.
#[macro_export]
macro_rules! profile_scope {
    ($($arg:tt)+) => {
        // Need a variable to avoid dropping immediately.
        // https://github.com/rust-lang/rust/issues/40096
        let _guard = crate::server::profile::Profiler::new(format!($($arg)+));
    };
}

/// Small basic profiler based on Instant.
pub struct Profiler {
    start: Instant,
    message: String,
}
impl Profiler {
    /// On creation, log the message.
    pub fn new(message: String) -> Self {
        info!("vv {} vv", message);
        Self {
            start: Instant::now(),
            message: message,
        }
    }
}
impl Drop for Profiler {
    /// On drop, print the elapsed time.
    fn drop(&mut self) {
        info!(
            "^^ {} (duration {}ms) ^^",
            self.message,
            self.start.elapsed().as_millis()
        );
    }
}
