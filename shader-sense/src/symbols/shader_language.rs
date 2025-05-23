use std::path::Path;

use tree_sitter::InputEdit;

use crate::{
    shader::ShadingLanguage,
    shader_error::ShaderError,
    validator::{create_validator, validator::Validator},
};

use super::{
    glsl::create_glsl_symbol_provider,
    hlsl::create_hlsl_symbol_provider,
    symbol_provider::SymbolProvider,
    symbol_tree::ShaderModule,
    symbols::{ShaderBuiltinSymbol, ShaderRange, ShaderSymbolTree},
    wgsl::create_wgsl_symbol_provider,
};
pub struct ShaderLanguage {
    shading_language: ShadingLanguage,
    shader_intrinsics: ShaderBuiltinSymbol,
    tree_sitter_language: tree_sitter::Language,
    tree_sitter_parser: tree_sitter::Parser,
}
impl ShaderLanguage {
    pub fn new(shading_language: ShadingLanguage) -> Self {
        Self::from_tree_sitter_language(
            shading_language,
            Self::get_tree_sitter_language(shading_language),
        )
    }
    fn get_tree_sitter_language(shading_language: ShadingLanguage) -> tree_sitter::Language {
        match shading_language {
            ShadingLanguage::Wgsl => tree_sitter_wgsl_bevy::language(),
            ShadingLanguage::Hlsl => tree_sitter_hlsl::language(),
            ShadingLanguage::Glsl => tree_sitter_glsl::language(),
        }
    }
    fn get_symbol_intrinsic_path(shading_language: ShadingLanguage) -> &'static str {
        match shading_language {
            ShadingLanguage::Wgsl => include_str!("wgsl/wgsl-intrinsics.json"),
            ShadingLanguage::Hlsl => include_str!("hlsl/hlsl-intrinsics.json"),
            ShadingLanguage::Glsl => include_str!("glsl/glsl-intrinsics.json"),
        }
    }
    fn from_tree_sitter_language(
        shading_language: ShadingLanguage,
        tree_sitter_language: tree_sitter::Language,
    ) -> Self {
        let mut tree_sitter_parser = tree_sitter::Parser::new();
        tree_sitter_parser
            .set_language(tree_sitter_language.clone())
            .expect("Error loading grammar");
        Self {
            shading_language,
            tree_sitter_language,
            tree_sitter_parser,
            shader_intrinsics: ShaderSymbolTree::parse_from_json(
                Self::get_symbol_intrinsic_path(shading_language).into(),
            ),
        }
    }
    pub fn create_symbol_provider(&self) -> SymbolProvider {
        match self.shading_language {
            ShadingLanguage::Wgsl => create_wgsl_symbol_provider(self.tree_sitter_language.clone()),
            ShadingLanguage::Hlsl => create_hlsl_symbol_provider(self.tree_sitter_language.clone()),
            ShadingLanguage::Glsl => create_glsl_symbol_provider(self.tree_sitter_language.clone()),
        }
    }
    // TODO: would be nice to return a solid object (not trait) for cleaner API (which might be holding a trait.)
    pub fn create_validator(&self) -> Box<dyn Validator> {
        create_validator(self.shading_language)
    }
    pub fn get_intrinsics_symbol(&self) -> &ShaderBuiltinSymbol {
        &self.shader_intrinsics
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
        self.update_module_partial(module, &ShaderRange::whole(&module.content), new_text)
    }
    // Update partial content of symbol tree
    pub fn update_module_partial(
        &mut self,
        module: &mut ShaderModule,
        old_range: &ShaderRange,
        new_text: &String,
    ) -> Result<(), ShaderError> {
        let mut new_shader_content = module.content.clone();
        let old_start_byte_offset = old_range.start.to_byte_offset(&module.content)?;
        let old_end_byte_offset = old_range.end.to_byte_offset(&module.content)?;
        new_shader_content.replace_range(old_start_byte_offset..old_end_byte_offset, &new_text);

        let line_count = new_text.lines().count();
        let tree_sitter_range = tree_sitter::Range {
            start_byte: old_start_byte_offset,
            end_byte: old_end_byte_offset,
            start_point: tree_sitter::Point {
                row: old_range.start.line as usize,
                column: old_range.start.pos as usize,
            },
            end_point: tree_sitter::Point {
                row: old_range.end.line as usize,
                column: old_range.end.pos as usize,
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
