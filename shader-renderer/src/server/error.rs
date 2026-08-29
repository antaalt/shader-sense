use std::fmt::Display;

use lsp_server::ProtocolError;
use shader_sense::shader_error::ShaderError;

use crate::renderer::error::RendererError;

pub enum ServerError {
    InternalError(String),
    ShaderError(ShaderError),
    RendererError(RendererError),
    SerializationError(serde_json::Error),
    IoError(std::io::Error),
}

impl Display for ServerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ServerError::InternalError(message) => write!(f, "Internal error: {}", message),
            ServerError::ShaderError(error) => write!(f, "Shader error: {}", error),
            ServerError::RendererError(error) => write!(f, "Renderer error: {}", error),
            ServerError::SerializationError(error) => write!(f, "Serialization error: {}", error),
            ServerError::IoError(error) => write!(f, "IO error: {}", error),
        }
    }
}

impl From<RendererError> for ServerError {
    fn from(error: RendererError) -> Self {
        ServerError::RendererError(error)
    }
}

impl From<ShaderError> for ServerError {
    fn from(error: ShaderError) -> Self {
        ServerError::ShaderError(error)
    }
}
impl From<serde_json::Error> for ServerError {
    fn from(error: serde_json::Error) -> Self {
        ServerError::SerializationError(error)
    }
}
impl From<std::io::Error> for ServerError {
    fn from(err: std::io::Error) -> Self {
        ServerError::IoError(err)
    }
}
impl From<ProtocolError> for ServerError {
    fn from(value: ProtocolError) -> Self {
        ServerError::InternalError(value.to_string())
    }
}
