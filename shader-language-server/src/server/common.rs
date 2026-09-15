use std::{fmt, path::Path};

use lsp_types::{Location, Url};
use shader_sense::{
    position::{ShaderFileRange, ShaderPosition, ShaderRange},
    shader::ShadingLanguage,
    shader_error::ShaderError,
};

#[derive(Debug)]
pub enum ServerLanguageError {
    ShaderError(ShaderError),
    InvalidParams(String),
    FileNotWatched(Url),
    UnsupportedLanguage(ShadingLanguage),
    SerializationError(serde_json::Error),
    MethodNotFound(String),
    LastRequestCanceled,
    InternalError(String),
    IoErr(std::io::Error),
}

impl fmt::Display for ServerLanguageError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ServerLanguageError::ShaderError(err) => write!(f, "{}", err),
            ServerLanguageError::SerializationError(err) => {
                write!(f, "Error with serialization: {}", err)
            }
            ServerLanguageError::FileNotWatched(uri) => {
                write!(f, "File not watched: {}", uri)
            }
            ServerLanguageError::InvalidParams(err) => write!(f, "Invalid parameters: {}", err),
            ServerLanguageError::MethodNotFound(err) => write!(f, "Method not found: {}", err),
            ServerLanguageError::InternalError(err) => write!(f, "Internal error: {}", err),
            ServerLanguageError::LastRequestCanceled => write!(f, "Last request canceled"),
            ServerLanguageError::UnsupportedLanguage(lang) => write!(f, "Unsupported language: {:?}. Restart server with the correct parameter to enable this language.", lang),
            ServerLanguageError::IoErr(err) => write!(f, "Io Err : {}", err),
        }
    }
}

impl From<ShaderError> for ServerLanguageError {
    fn from(error: ShaderError) -> Self {
        ServerLanguageError::ShaderError(error)
    }
}
impl From<serde_json::Error> for ServerLanguageError {
    fn from(error: serde_json::Error) -> Self {
        ServerLanguageError::SerializationError(error)
    }
}
impl From<std::io::Error> for ServerLanguageError {
    fn from(err: std::io::Error) -> Self {
        ServerLanguageError::IoErr(err)
    }
}

pub fn shader_range_to_lsp_range(range: &ShaderRange) -> lsp_types::Range {
    lsp_types::Range {
        start: lsp_types::Position {
            line: range.start.line,
            character: range.start.pos,
        },
        end: lsp_types::Position {
            line: range.end.line,
            character: range.end.pos,
        },
    }
}

pub fn lsp_range_to_shader_range(range: &lsp_types::Range) -> ShaderRange {
    ShaderRange::new(
        ShaderPosition::new(range.start.line, range.start.character),
        ShaderPosition::new(range.end.line, range.end.character),
    )
}
pub fn shader_position_to_lsp_position(position: &ShaderPosition) -> lsp_types::Position {
    lsp_types::Position {
        line: position.line,
        character: position.pos,
    }
}

pub fn shader_range_to_location(range: &ShaderFileRange) -> Location {
    Location::new(
        Url::from_file_path(&range.file_path).unwrap(),
        shader_range_to_lsp_range(&range.range),
    )
}

// Handle non-utf8 characters
pub fn read_string_lossy(file_path: &Path) -> std::io::Result<String> {
    use std::io::Read;
    match std::fs::read_to_string(file_path) {
        Ok(content) => Ok(content),
        Err(err) => match err.kind() {
            std::io::ErrorKind::InvalidData => {
                // Load non utf8 file as lossy string.
                log::warn!(
                    "Non UTF8 characters detected in file {}. Loaded as lossy string.",
                    file_path.display()
                );
                let mut file = std::fs::File::open(file_path)?;
                let mut buf = vec![];
                file.read_to_end(&mut buf)?;
                Ok(String::from_utf8_lossy(&buf).into())
            }
            _ => Err(err),
        },
    }
}
