use std::fmt::Display;

pub enum RendererError {
    InternalError(String),
    /// Error reported by wgpu while creating a resource or rendering.
    /// These are mostly caused by the shader we were given, so they are expected
    /// to happen & must be reported back to the client instead of crashing the renderer.
    ValidationError(String),
}

impl Display for RendererError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RendererError::InternalError(message) => write!(f, "Internal error: {}", message),
            RendererError::ValidationError(message) => write!(f, "Validation error: {}", message),
        }
    }
}
