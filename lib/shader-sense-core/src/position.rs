//! Position type for handling text position in this crate.
use std::{cmp::Ordering, path::PathBuf};

use serde::{Deserialize, Serialize};

use crate::include::canonicalize;

/// Position in a single file with line and character
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ShaderPosition {
    pub line: u32,
    pub pos: u32,
}
impl Eq for ShaderPosition {}

impl Ord for ShaderPosition {
    fn cmp(&self, other: &Self) -> Ordering {
        (&self.line, &self.pos).cmp(&(&other.line, &other.pos))
    }
}

impl PartialOrd for ShaderPosition {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for ShaderPosition {
    fn eq(&self, other: &Self) -> bool {
        (&self.line, &self.pos) == (&other.line, &other.pos)
    }
}

impl ShaderPosition {
    /// Create a new position
    pub fn new(line: u32, pos: u32) -> Self {
        Self { line, pos }
    }
    /// Get the zero position
    pub fn zero() -> Self {
        Self { line: 0, pos: 0 }
    }
    /// Convert a [`ShaderPosition`] into a [`ShaderFilePosition`]
    pub fn into_file(self, file_path: PathBuf) -> ShaderFilePosition {
        ShaderFilePosition::from(file_path, self)
    }
    /// Clone a [`ShaderPosition`] to a [`ShaderFilePosition`]
    pub fn clone_into_file(&self, file_path: PathBuf) -> ShaderFilePosition {
        ShaderFilePosition::from(file_path, self.clone())
    }
    /// Compute the line and pos in a given content from the given byte offset.
    /// This is handling UTF8 string aswell and should safely return a correct position.
    pub fn from_byte_offset(content: &str, byte_offset: usize) -> std::io::Result<ShaderPosition> {
        // https://en.wikipedia.org/wiki/UTF-8
        if byte_offset == 0 {
            Ok(ShaderPosition::zero())
        } else if content.len() == 0 {
            Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Content is empty.",
            ))
        } else if byte_offset > content.len() {
            Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "byte_offset is out of bounds.",
            ))
        } else {
            // lines iterator does the same, but skip the last empty line by relying on split_inclusive.
            // We need it so use split instead to keep it.
            // We only care about line start, so \r being there or not on Windows should not be an issue.
            let line = content[..byte_offset].split('\n').count() - 1;
            let line_start = content[..byte_offset]
                .split('\n')
                .rev()
                .next()
                .expect("No last line available.");
            let pos_in_byte =
                content[byte_offset..].as_ptr() as usize - line_start.as_ptr() as usize;
            if line_start.is_char_boundary(pos_in_byte) {
                Ok(ShaderPosition::new(
                    line as u32,
                    line_start[..pos_in_byte].chars().count() as u32,
                ))
            } else {
                Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "Pos in line is not at UTF8 char boundary.",
                ))
            }
        }
    }
    /// Compute the byte offset in a given content from the given line and pos.
    /// This is handling UTF8 string aswell and should safely return a byte offset at character boundary.
    pub fn to_byte_offset(&self, content: &str) -> std::io::Result<usize> {
        // https://en.wikipedia.org/wiki/UTF-8
        match content.lines().nth(self.line as usize) {
            Some(line) => {
                // This pointer operation is safe to operate because lines iterator should start at char boundary.
                let line_byte_offset = line.as_ptr() as usize - content.as_ptr() as usize;
                assert!(
                    content.is_char_boundary(line_byte_offset),
                    "Start of line is not char boundary."
                );
                // We have line offset, find pos offset.
                match content[line_byte_offset..]
                    .char_indices()
                    .nth(self.pos as usize)
                {
                    Some((byte_offset, _)) => {
                        let global_offset = line_byte_offset + byte_offset;
                        if content.len() <= global_offset {
                            Err(std::io::Error::new(
                                std::io::ErrorKind::InvalidData,
                                "Byte offset is not in content range.",
                            ))
                        } else if !content.is_char_boundary(global_offset) {
                            Err(std::io::Error::new(
                                std::io::ErrorKind::InvalidData,
                                "Position is not at UTF8 char boundary.",
                            ))
                        } else {
                            Ok(global_offset)
                        }
                    }
                    None => {
                        if self.pos as usize == line.chars().count() {
                            assert!(content.is_char_boundary(line_byte_offset + line.len()));
                            Ok(line_byte_offset + line.len())
                        } else {
                            Err(std::io::Error::new(
                                std::io::ErrorKind::InvalidInput,
                                format!("Position is not in range of line"),
                            ))
                        }
                    }
                }
            }
            // Last line in line iterator is skipped if its empty.
            None => Ok(content.len()), // Line is out of bounds, assume its at the end.
        }
    }
}

/// Wrapper for [`ShaderPosition`] with a [`PathBuf`] specified for context.
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ShaderFilePosition {
    pub file_path: PathBuf,
    pub position: ShaderPosition,
}
impl Eq for ShaderFilePosition {}

impl Ord for ShaderFilePosition {
    fn cmp(&self, other: &Self) -> Ordering {
        assert!(
            self.file_path == other.file_path,
            "Cannot compare file from different path"
        );
        (&self.file_path, &self.position.line, &self.position.pos).cmp(&(
            &other.file_path,
            &other.position.line,
            &other.position.pos,
        ))
    }
}

impl PartialOrd for ShaderFilePosition {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for ShaderFilePosition {
    fn eq(&self, other: &Self) -> bool {
        (&self.file_path, &self.position.line, &self.position.pos)
            == (&other.file_path, &other.position.line, &other.position.pos)
    }
}

impl ShaderFilePosition {
    /// Create a [`ShaderFilePosition`] from a [`ShaderPosition`] and a [`PathBuf`]
    pub fn from(file_path: PathBuf, position: ShaderPosition) -> Self {
        debug_assert_eq!(
            canonicalize(&file_path).unwrap(),
            file_path,
            "canonicalize the path before usage to enforce consistency."
        );
        Self {
            file_path,
            position,
        }
    }
    /// Create a [`ShaderFilePosition`] from line and pos
    pub fn new(file_path: PathBuf, line: u32, pos: u32) -> Self {
        debug_assert_eq!(
            canonicalize(&file_path).unwrap(),
            file_path,
            "canonicalize the path before usage to enforce consistency."
        );
        Self {
            file_path,
            position: ShaderPosition::new(line, pos),
        }
    }
    /// Create the zero position for this file
    pub fn zero(file_path: PathBuf) -> Self {
        debug_assert_eq!(
            canonicalize(&file_path).unwrap(),
            file_path,
            "canonicalize the path before usage to enforce consistency."
        );
        Self {
            file_path,
            position: ShaderPosition::zero(),
        }
    }
    // Get the character position in line
    pub fn pos(&self) -> u32 {
        self.position.pos
    }
    // Get the line
    pub fn line(&self) -> u32 {
        self.position.line
    }
}

/// Range in a single file from two [`ShaderPosition`]
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ShaderRange {
    pub start: ShaderPosition,
    pub end: ShaderPosition,
}

impl ShaderRange {
    /// Create a new range from two [`ShaderPosition`]
    pub fn new(start: ShaderPosition, end: ShaderPosition) -> Self {
        Self { start, end }
    }
    /// Create a zero range
    pub fn zero() -> Self {
        Self::new(ShaderPosition::zero(), ShaderPosition::zero())
    }
    /// Convert a [`ShaderRange`] into a [`ShaderFileRange`]
    pub fn into_file(self, file_path: PathBuf) -> ShaderFileRange {
        ShaderFileRange::from(file_path, self)
    }
    /// Clone a [`ShaderRange`] to a [`ShaderFileRange`]
    pub fn clone_into_file(&self, file_path: PathBuf) -> ShaderFileRange {
        ShaderFileRange::from(file_path, self.clone())
    }
    /// Get the total range for a given content.
    pub fn whole(content: &str) -> Self {
        let line_count = content.lines().count() as u32;
        let char_count = match content.lines().last() {
            Some(last_line) => (last_line.char_indices().count()) as u32, // Last line
            None => (content.char_indices().count()) as u32, // No last line, means no line, pick string length
        };
        Self {
            start: ShaderPosition::new(0, 0),
            end: ShaderPosition::new(line_count, char_count),
        }
    }
    /// Check if the range contain another range.
    pub fn contain_bounds(&self, range: &ShaderRange) -> bool {
        if range.start.line > self.start.line && range.end.line < self.end.line {
            true
        } else if range.start.line == self.start.line && range.end.line == self.end.line {
            range.start.pos >= self.start.pos && range.end.pos <= self.end.pos
        } else if range.start.line == self.start.line && range.end.line < self.end.line {
            range.start.pos >= self.start.pos
        } else if range.end.line == self.end.line && range.start.line > self.start.line {
            range.end.pos <= self.end.pos
        } else {
            false
        }
    }
    /// Check if the range contain a [`ShaderPosition`]
    pub fn contain(&self, position: &ShaderPosition) -> bool {
        // Check line & position bounds.
        if position.line > self.start.line && position.line < self.end.line {
            true
        } else if position.line == self.start.line && position.line == self.end.line {
            position.pos >= self.start.pos && position.pos <= self.end.pos
        } else if position.line == self.start.line && position.line < self.end.line {
            position.pos >= self.start.pos
        } else if position.line == self.end.line && position.line > self.start.line {
            position.pos <= self.end.pos
        } else {
            false
        }
    }
    // Join two range to a mutual range
    pub fn join(mut lhs: ShaderRange, rhs: ShaderRange) -> ShaderRange {
        lhs.start.line = std::cmp::min(lhs.start.line, rhs.start.line);
        lhs.start.pos = std::cmp::min(lhs.start.pos, rhs.start.pos);
        lhs.end.line = std::cmp::max(lhs.end.line, rhs.end.line);
        lhs.end.pos = std::cmp::max(lhs.end.pos, rhs.end.pos);
        lhs
    }
}

/// Wrapper for [`ShaderRange`] with a [`PathBuf`] specified for context.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ShaderFileRange {
    pub file_path: PathBuf,
    pub range: ShaderRange,
}

impl ShaderFileRange {
    /// Create a new range from a [`ShaderRange`] and a [`PathBuf`]
    pub fn from(file_path: PathBuf, range: ShaderRange) -> Self {
        debug_assert_eq!(
            canonicalize(&file_path).unwrap(),
            file_path,
            "canonicalize the path before usage to enforce consistency."
        );
        Self { file_path, range }
    }
    /// Create a new range from two [`ShaderPosition`] and a [`PathBuf`]
    pub fn new(file_path: PathBuf, start: ShaderPosition, end: ShaderPosition) -> Self {
        debug_assert_eq!(
            canonicalize(&file_path).unwrap(),
            file_path,
            "canonicalize the path before usage to enforce consistency."
        );
        Self {
            file_path,
            range: ShaderRange::new(start, end),
        }
    }
    /// Create a zero range
    pub fn zero(file_path: PathBuf) -> Self {
        Self::new(file_path, ShaderPosition::zero(), ShaderPosition::zero())
    }
    /// Get the total range for a given content.
    pub fn whole(file_path: PathBuf, content: &str) -> Self {
        Self::from(file_path, ShaderRange::whole(content))
    }
    /// Get the start position of the range
    pub fn start(&self) -> &ShaderPosition {
        &self.range.start
    }
    /// Get the end position of the range
    pub fn end(&self) -> &ShaderPosition {
        &self.range.end
    }
    // Get the start position as [`ShaderFilePosition`]
    pub fn start_as_file_position(&self) -> ShaderFilePosition {
        ShaderFilePosition::from(self.file_path.clone(), self.range.start.clone())
    }
    // Get the end position as [`ShaderFilePosition`]
    pub fn end_as_file_position(&self) -> ShaderFilePosition {
        ShaderFilePosition::from(self.file_path.clone(), self.range.end.clone())
    }
    /// Check if the range contain another range.
    pub fn contain_bounds(&self, range: &ShaderFileRange) -> bool {
        if self.file_path.as_os_str() == range.file_path.as_os_str() {
            debug_assert!(
                range.file_path == self.file_path,
                "Raw string identical but not components"
            );
            self.range.contain_bounds(&range.range)
        } else {
            debug_assert!(
                range.file_path != self.file_path,
                "Raw string different but not components"
            );
            false
        }
    }
    /// Check if the range contain a [`ShaderFilePosition`]
    pub fn contain(&self, position: &ShaderFilePosition) -> bool {
        // Check same file. Comparing components is hitting perf, so just compare raw path, which should already be canonical.
        if position.file_path.as_os_str() == self.file_path.as_os_str() {
            debug_assert!(
                position.file_path == self.file_path,
                "Raw string identical but not components"
            );
            self.range.contain(&position.position)
        } else {
            debug_assert!(
                position.file_path != self.file_path,
                "Raw string different but not components"
            );
            false
        }
    }
    // Join two range to a mutual range
    pub fn join(mut lhs: ShaderFileRange, rhs: ShaderFileRange) -> ShaderFileRange {
        lhs.range = ShaderRange::join(lhs.range, rhs.range);
        lhs
    }
}
