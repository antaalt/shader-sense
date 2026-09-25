//! Error handling for this crate.
use core::fmt;

use crate::position::ShaderFileRange;

/// Severity of a diagnostic
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ShaderDiagnosticSeverity {
    Error,
    Warning,
    Information,
    Hint,
}
impl fmt::Display for ShaderDiagnosticSeverity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ShaderDiagnosticSeverity::Error => write!(f, "error"),
            ShaderDiagnosticSeverity::Warning => write!(f, "warning"),
            ShaderDiagnosticSeverity::Information => write!(f, "info"),
            ShaderDiagnosticSeverity::Hint => write!(f, "hint"),
        }
    }
}

impl From<&str> for ShaderDiagnosticSeverity {
    fn from(value: &str) -> Self {
        match value {
            "error" => ShaderDiagnosticSeverity::Error,
            "warning" => ShaderDiagnosticSeverity::Warning,
            "info" => ShaderDiagnosticSeverity::Information,
            "hint" => ShaderDiagnosticSeverity::Hint,
            _ => ShaderDiagnosticSeverity::Error,
        }
    }
}

impl ShaderDiagnosticSeverity {
    /// Is this diagnostic required
    pub fn is_required(&self, required_severity: &ShaderDiagnosticSeverity) -> bool {
        self.get_enum_index() <= required_severity.get_enum_index()
    }
    fn get_enum_index(&self) -> u32 {
        match self {
            ShaderDiagnosticSeverity::Error => 0,
            ShaderDiagnosticSeverity::Warning => 1,
            ShaderDiagnosticSeverity::Information => 2,
            ShaderDiagnosticSeverity::Hint => 3,
        }
    }
}

/// A diagnostic returned by validation
#[derive(Debug, Clone)]
pub struct ShaderDiagnostic {
    pub severity: ShaderDiagnosticSeverity,
    pub error: String,
    pub range: ShaderFileRange,
}
/// A list of diagnostic returned by validation
#[derive(Debug, Default, Clone)]
pub struct ShaderDiagnosticList {
    pub diagnostics: Vec<ShaderDiagnostic>,
}

/// A generic error enum for this crate.
#[derive(Debug)]
pub enum ShaderError {
    ValidationError(String),
    NoSymbol,
    ParseSymbolError(String),
    SymbolQueryError(String, ShaderFileRange),
    IoErr(std::io::Error),
    InternalErr(String),
}

impl ShaderError {
    /// Convert an error into a diagnostic if its supported
    pub fn into_diagnostic(&self, severity: ShaderDiagnosticSeverity) -> Option<ShaderDiagnostic> {
        match self {
            ShaderError::SymbolQueryError(message, range) => Some(ShaderDiagnostic {
                error: format!(
                    "Symbol Query {}, symbol provider may be impacted: {}",
                    severity.to_string(),
                    message
                ),
                severity: severity,
                range: range.clone(),
            }),
            _ => None,
        }
    }
}

impl From<regex::Error> for ShaderError {
    fn from(error: regex::Error) -> Self {
        match error {
            regex::Error::CompiledTooBig(err) => {
                ShaderError::InternalErr(format!("Regex compile too big: {}", err))
            }
            regex::Error::Syntax(err) => {
                ShaderError::InternalErr(format!("Regex syntax invalid: {}", err))
            }
            err => ShaderError::InternalErr(format!("Regex error: {:#?}", err)),
        }
    }
}

impl fmt::Display for ShaderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ShaderError::IoErr(err) => write!(f, "IoError: {}", err),
            ShaderError::InternalErr(err) => write!(f, "Internal server error: {}", err),
            ShaderError::NoSymbol => write!(f, "No symbol found"),
            ShaderError::ParseSymbolError(err) => write!(f, "Failed to parse symbols: {}", err),
            ShaderError::ValidationError(err) => write!(f, "Validation error: {}", err),
            ShaderError::SymbolQueryError(err, range) => {
                write!(f, "SymbolQueryError: {} at {:?}", err, range)
            }
        }
    }
}

impl From<std::io::Error> for ShaderError {
    fn from(err: std::io::Error) -> Self {
        ShaderError::IoErr(err)
    }
}
impl From<ShaderDiagnostic> for ShaderDiagnosticList {
    fn from(err: ShaderDiagnostic) -> Self {
        Self {
            diagnostics: vec![err],
        }
    }
}
impl ShaderDiagnosticList {
    /// Generate an empty diagnostic list
    pub fn empty() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }
    /// Push a diagnostic in the list
    pub fn push(&mut self, error: ShaderDiagnostic) {
        self.diagnostics.push(error);
    }
    /// Is the diagnostic empty
    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }
    /// Join two diagnostics.
    pub fn join(mut lhs: ShaderDiagnosticList, rhs: ShaderDiagnosticList) -> Self {
        lhs.diagnostics.extend(rhs.diagnostics);
        lhs
    }
}
