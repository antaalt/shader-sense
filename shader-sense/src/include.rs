use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

pub struct IncludeHandler {
    includes: Vec<String>,
    directory_stack: Vec<PathBuf>, // Vec for keeping insertion order. Might own duplicate.
    path_remapping: HashMap<PathBuf, PathBuf>, // remapping of path / virtual path
}
// std::fs::canonicalize not supported on wasi target... Emulate it.
// On Windows, std::fs::canonicalize return a /? prefix that break hashmap.
// https://stackoverflow.com/questions/50322817/how-do-i-remove-the-prefix-from-a-canonical-windows-path
// Instead use a custom canonicalize.
pub fn canonicalize(p: &Path) -> std::io::Result<PathBuf> {
    // https://github.com/antmicro/wasi_ext_lib/blob/main/canonicalize.patch
    fn __canonicalize(path: &Path, buf: &mut PathBuf) {
        if path.is_absolute() {
            buf.clear();
        }
        for part in path {
            if part == ".." {
                buf.pop();
            } else if part != "." {
                buf.push(part);
                if let Ok(linkpath) = buf.read_link() {
                    buf.pop();
                    __canonicalize(&linkpath, buf);
                }
            }
        }
    }
    let mut path = if p.is_absolute() {
        PathBuf::new()
    } else {
        PathBuf::from(std::env::current_dir()?)
    };
    __canonicalize(p, &mut path);
    Ok(path)
}

impl IncludeHandler {
    pub fn default(file: &Path) -> Self {
        Self::new(file, Vec::new(), HashMap::new())
    }
    // New main file.
    pub fn new(
        file: &Path,
        includes: Vec<String>,
        path_remapping: HashMap<PathBuf, PathBuf>,
    ) -> Self {
        // Add local path to directory stack
        let cwd = file.parent().unwrap();
        let mut stack = Vec::new();
        stack.push(cwd.into());
        Self::new_from_stack(includes, path_remapping, stack)
    }
    pub fn new_from_stack(
        includes: Vec<String>,
        path_remapping: HashMap<PathBuf, PathBuf>,
        directory_stack: Vec<PathBuf>,
    ) -> Self {
        Self {
            includes: includes,
            directory_stack: directory_stack,
            path_remapping: path_remapping,
        }
    }
    pub fn get_directory_stack(&self) -> &Vec<PathBuf> {
        &self.directory_stack
    }
    pub fn search_in_includes(
        &mut self,
        relative_path: &Path,
        include_callback: &mut dyn FnMut(&Path) -> Option<String>,
    ) -> Option<(String, PathBuf)> {
        match self.search_path_in_includes(relative_path) {
            Some(absolute_path) => include_callback(&absolute_path).map(|e| (e, absolute_path)),
            None => None,
        }
    }
    pub fn search_path_in_includes(&mut self, relative_path: &Path) -> Option<PathBuf> {
        self.search_path_in_includes_relative(relative_path)
            .map(|e| canonicalize(&e).expect("Failed to convert relative path to absolute"))
    }
    pub fn search_path_in_includes_relative(&mut self, relative_path: &Path) -> Option<PathBuf> {
        // Checking for file existence is a bit costly.
        // Some options are available and have been tested
        // - path.exists(): approximatively 100us
        // - path.is_file(): approximatively 40us
        // - std::fs::exists(&path).unwrap_or(false): approximatively 40us but only stable with Rust>1.81
        if relative_path.is_file() {
            Some(PathBuf::from(relative_path))
        } else {
            // Check directory stack.
            // Reverse order to check first the latest added folders.
            for directory_stack in self.directory_stack.iter().rev() {
                let path = Path::new(directory_stack).join(&relative_path);
                if path.is_file() {
                    if let Some(parent) = path.parent() {
                        self.directory_stack.push(canonicalize(parent).unwrap());
                    }
                    return Some(path);
                }
            }
            // Check include paths
            for include_path in &self.includes {
                let path = Path::new(include_path).join(&relative_path);
                if path.is_file() {
                    if let Some(parent) = path.parent() {
                        self.directory_stack.push(canonicalize(parent).unwrap());
                    }
                    return Some(path);
                }
            }
            // Check virtual paths
            if let Some(target_path) =
                Self::resolve_virtual_path(relative_path, &self.path_remapping)
            {
                if target_path.is_file() {
                    // We should not add virtual path to stack, as they are well defined already.
                    // But we expect a correct directory stack.
                    if let Some(parent) = target_path.parent() {
                        self.directory_stack.push(canonicalize(parent).unwrap());
                    }
                    return Some(target_path);
                }
            }
            return None;
        }
    }
    fn resolve_virtual_path(
        virtual_path: &Path,
        virtual_folders: &HashMap<PathBuf, PathBuf>,
    ) -> Option<PathBuf> {
        // Virtual path need to start with /
        // Dxc automatically insert .\ in front of path that are not absolute.
        // We should simply strip it, but how do we know its a virtual path or a real relative path ?
        // Instead dirty hack to remove it and try to load it, as its the last step of include, should be fine...
        let virtual_path = if virtual_path.starts_with("./") || virtual_path.starts_with(".\\") {
            let mut comp = virtual_path.components();
            comp.next();
            Path::new("/").join(comp.as_path())
        } else {
            PathBuf::from(virtual_path)
        };

        // Check if it's a virtual path (starts with / or Packages/)
        if virtual_path.starts_with("/") || virtual_path.starts_with("\\") || virtual_path.starts_with("Packages/") {
            // Normalize the path to handle Packages/ uniformly
            let normalized_path = if virtual_path.starts_with("Packages/") {
                Path::new("/").join(virtual_path)
            } else {
                virtual_path
            };

            // Browse possible mapping & find a match.
            for (virtual_folder, target_path) in virtual_folders {
                let mut path_components = normalized_path.components();
                let mut found = true;
                for virtual_folder_component in virtual_folder.components() {
                    match path_components.next() {
                        Some(component) => {
                            if component != virtual_folder_component {
                                found = false;
                                break;
                            }
                        }
                        None => {
                            found = false;
                            break;
                        }
                    }
                }
                if found {
                    let resolved_path = target_path.join(path_components.as_path());
                    return Some(resolved_path.into());
                }
            }
            None
        } else {
            None
        }
    }
}
