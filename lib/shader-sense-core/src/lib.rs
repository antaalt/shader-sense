use std::path::Path;

pub mod include;
pub mod position;
pub mod shader;
pub mod shader_error;

// Only used for test.
#[allow(dead_code)]
pub fn test_path<P: AsRef<Path>>(path: P) -> std::path::PathBuf {
    use crate::include::canonicalize;
    use std::path::PathBuf;
    // WASI need relative path. cwd passed to runner
    let path = if cfg!(target_os = "wasi") {
        PathBuf::from("/test").join(path)
    } else {
        PathBuf::from("../shader-sense/test").join(path)
    };
    canonicalize(&path).unwrap()
}
