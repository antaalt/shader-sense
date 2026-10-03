use std::path::{Path, PathBuf};

use tree_sitter::Node;

use crate::{
    include::{canonicalize, find_wesl_module_file},
    position::ShaderRange,
    shader::WgslCompilationParams,
    symbols::symbol_parser::get_name,
};

/// A WESL import, such as `import package::lighting::Light;`
#[derive(Debug, Clone)]
pub struct WgslImport {
    /// Module path of the imported item, such as `package::lighting::Light`.
    pub path: Vec<String>,
    /// Range of the import statement.
    pub range: ShaderRange,
}

impl WgslImport {
    pub fn get_path(&self) -> String {
        self.path.join("::")
    }
    /// Find the file of the module holding the imported item.
    /// Imports point either to an item of a module (`package::math::PI`) or to a module itself (`package::math`).
    pub fn resolve(&self, file_path: &Path, params: &WgslCompilationParams) -> Option<PathBuf> {
        let (origin, components) = self.path.split_first()?;
        let (root, components) = match origin.as_str() {
            "package" => (
                params
                    .package_root
                    .clone()
                    .or_else(|| file_path.parent().map(|parent| parent.into()))?,
                components,
            ),
            // super is relative to the directory of the current module.
            "super" => {
                let super_count = 1 + components.iter().take_while(|c| *c == "super").count();
                let mut root = file_path.to_path_buf();
                for _ in 0..super_count {
                    root = root.parent()?.into();
                }
                (root, &components[super_count - 1..])
            }
            package => (params.packages.get(package)?.clone(), components),
        };
        let root = canonicalize(&root).unwrap_or(root);
        // Wildcard import the whole module.
        let components = match components.split_last() {
            Some((last, module)) if last == "*" => module,
            _ => components,
        };
        find_wesl_module_file(&root, components).or_else(|| {
            find_wesl_module_file(&root, &components[..components.len().checked_sub(1)?])
        })
    }
}

/// Collect all imports from a module, flattening import collections.
pub fn query_imports(content: &str, node: Node) -> Vec<WgslImport> {
    fn get_import_path(content: &str, node: Node, path: &mut Vec<String>) {
        // import_path is recursive, with the first components being the deepest.
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            match child.kind() {
                "import_path" => get_import_path(content, child, path),
                "identifier" => path.push(get_name(content, child).into()),
                _ => {}
            }
        }
    }
    fn visit(
        content: &str,
        node: Node,
        path: &Vec<String>,
        range: &ShaderRange,
        imports: &mut Vec<WgslImport>,
    ) {
        let mut path = path.clone();
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            match child.kind() {
                "import_path" => get_import_path(content, child, &mut path),
                "import_collection" | "import" => visit(content, child, &path, range, imports),
                "import_item" => {
                    if let Some(name) = child.child_by_field_name("name") {
                        let mut path = path.clone();
                        path.push(get_name(content, name).into());
                        imports.push(WgslImport {
                            path,
                            range: range.clone(),
                        });
                    }
                }
                _ => {}
            }
        }
    }
    let mut imports = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() == "import_statement" {
            let range = ShaderRange::from(child.range());
            visit(content, child, &Vec::new(), &range, &mut imports);
        }
    }
    imports
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_query_imports() {
        let content = "import super::util::math::PI;\n\
            import package::{lighting::{Light, shade as shadeLight}, util::math};\n\
            import external::helpers::*;\n\
            fn main() {}";
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_wesl::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(content, None).unwrap();
        let imports: Vec<String> = query_imports(content, tree.root_node())
            .iter()
            .map(|import| import.get_path())
            .collect();
        assert_eq!(
            imports,
            vec![
                "super::util::math::PI",
                "package::lighting::Light",
                "package::lighting::shade",
                "package::util::math",
                "external::helpers::*",
            ]
        );
    }
}
