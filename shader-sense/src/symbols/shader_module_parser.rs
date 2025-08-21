use std::path::Path;

use tree_sitter::InputEdit;

use crate::{position::ShaderFileRange, shader::ShadingLanguage, shader_error::ShaderError};

use super::shader_module::ShaderModule;
pub struct ShaderModuleParser {
    tree_sitter_parser: tree_sitter::Parser,
}

impl ShaderModuleParser {
    pub fn glsl() -> Self {
        Self::from_shading_language(ShadingLanguage::Glsl)
    }
    pub fn hlsl() -> Self {
        Self::from_shading_language(ShadingLanguage::Hlsl)
    }
    pub fn wgsl() -> Self {
        Self::from_shading_language(ShadingLanguage::Wgsl)
    }
    pub fn from_shading_language(shading_language: ShadingLanguage) -> Self {
        let mut tree_sitter_parser = tree_sitter::Parser::new();
        tree_sitter_parser
            .set_language(Self::get_tree_sitter_language(shading_language))
            .expect("Error loading grammar");
        Self { tree_sitter_parser }
    }
    fn get_tree_sitter_language(shading_language: ShadingLanguage) -> tree_sitter::Language {
        match shading_language {
            ShadingLanguage::Wgsl => tree_sitter_wgsl_bevy::language(),
            ShadingLanguage::Hlsl => tree_sitter_hlsl::language(),
            ShadingLanguage::Glsl => tree_sitter_glsl::language(),
        }
    }
    // Create shader module from file.
    pub fn create_module(
        &mut self,
        file_path: &Path,
        shader_content: &str,
    ) -> Result<ShaderModule, ShaderError> {
        match self.tree_sitter_parser.parse(shader_content, None) {
            Some(tree) => Ok(ShaderModule {
                file_path: file_path.into(),
                content: shader_content.into(),
                tree,
            }),
            None => Err(ShaderError::ParseSymbolError(format!(
                "Failed to parse AST for file {}",
                file_path.display()
            ))),
        }
    }
    // Update whole content of symbol tree
    pub fn update_module(
        &mut self,
        module: &mut ShaderModule,
        new_text: &String,
    ) -> Result<(), ShaderError> {
        self.update_module_partial(
            module,
            &ShaderFileRange::whole(module.file_path.clone(), &module.content),
            new_text,
        )
    }
    // Update partial content of symbol tree
    pub fn update_module_partial(
        &mut self,
        module: &mut ShaderModule,
        old_range: &ShaderFileRange,
        new_text: &String,
    ) -> Result<(), ShaderError> {
        let mut new_shader_content = module.content.clone();
        let old_start_byte_offset = old_range.range.start.to_byte_offset(&module.content)?;
        let old_end_byte_offset = old_range.range.end.to_byte_offset(&module.content)?;
        new_shader_content.replace_range(old_start_byte_offset..old_end_byte_offset, &new_text);

        let line_count = new_text.lines().count();
        let tree_sitter_range = tree_sitter::Range {
            start_byte: old_start_byte_offset,
            end_byte: old_end_byte_offset,
            start_point: tree_sitter::Point {
                row: old_range.range.start.line as usize,
                column: old_range.range.start.pos as usize,
            },
            end_point: tree_sitter::Point {
                row: old_range.range.end.line as usize,
                column: old_range.range.end.pos as usize,
            },
        };
        module.tree.edit(&InputEdit {
            start_byte: tree_sitter_range.start_byte,
            old_end_byte: tree_sitter_range.end_byte,
            new_end_byte: tree_sitter_range.start_byte + new_text.len(),
            start_position: tree_sitter_range.start_point,
            old_end_position: tree_sitter_range.end_point,
            new_end_position: tree_sitter::Point {
                row: if line_count == 0 {
                    tree_sitter_range.start_point.row + new_text.len()
                } else {
                    new_text.lines().last().as_slice().len()
                },
                column: tree_sitter_range.start_point.column + line_count,
            },
        });
        // Update the tree.
        match self
            .tree_sitter_parser
            .parse(&new_shader_content, Some(&module.tree))
        {
            Some(new_tree) => {
                module.tree = new_tree;
                module.content = new_shader_content.clone();
                Ok(())
            }
            None => Err(ShaderError::ParseSymbolError(format!(
                "Failed to update AST for file {}.",
                module.file_path.display()
            ))),
        }
    }
}
