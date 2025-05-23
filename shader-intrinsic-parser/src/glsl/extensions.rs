use shader_sense::symbols::symbols::{ShaderBuiltinSymbol, ShaderSymbol};

use super::GlslIntrinsicParser;

impl GlslIntrinsicParser {
    #[allow(dead_code)]
    fn get_glsl_ext_mesh_shader(&self) -> ShaderBuiltinSymbol {
        // https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_mesh_shader.txt
        let mut list = ShaderBuiltinSymbol::default();
        #[allow(unreachable_code)]
        list.push(ShaderSymbol {
            label: "gl_PrimitivePointIndicesEXT".into(),
            description: todo!(),
            version: todo!(),
            stages: todo!(),
            link: todo!(),
            data: todo!(),
            range: None,
            content: None,
        });
        list
    }
}
