use shader_sense::symbols::{
    symbol_list::ShaderSymbolList,
    symbols::{ShaderSymbol, ShaderSymbolData, ShaderSymbolIntrinsic, ShaderSymbolMode},
};

use super::GlslIntrinsicParser;

impl GlslIntrinsicParser {
    pub fn add_keywords(&self, symbols: &mut ShaderSymbolList) {
        fn new_glsl_keyword(label: &str, description: &str) -> ShaderSymbol {
            ShaderSymbol {
                label: label.into(),
                mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                    description.into(),
                    Some("https://www.khronos.org/opengl/wiki/Type_Qualifier_(GLSL)".into()),
                )),
                requirement: None,
                data: ShaderSymbolData::Keyword {},
            }
        }
        symbols
            .keywords
            .push(new_glsl_keyword("uniform", "Declare an uniform variable"));
        symbols.keywords.push(new_glsl_keyword("layout", ""));
        symbols
            .keywords
            .push(new_glsl_keyword("const", "constant qualifier"));
        symbols.keywords.push(new_glsl_keyword("struct", ""));
        symbols.keywords.push(new_glsl_keyword(
            "in",
            "Mark a function parameter as an input",
        ));
        symbols.keywords.push(new_glsl_keyword(
            "out",
            "Mark a function parameter as an output",
        ));
        symbols.keywords.push(new_glsl_keyword(
            "inout",
            "Mark a function parameter as both an input and output",
        ));
        symbols.keywords.push(new_glsl_keyword("flat", "The value will not be interpolated. The value given to the fragment shader is the value from the Provoking Vertex for that primitive."));
        symbols.keywords.push(new_glsl_keyword("noperspective", "The value will be linearly interpolated in window-space. This is usually not what you want, but it can have its uses."));
        symbols.keywords.push(new_glsl_keyword("smooth", "The value will be interpolated in a perspective-correct fashion. This is the default if no qualifier is present."));
        symbols.keywords.push(new_glsl_keyword("precision", "Choose the precision of the given type. Possible values are highp, mediump, lowp. Only float and uint supported."));
        symbols
            .keywords
            .push(new_glsl_keyword("highp", "High precision modifier"));
        symbols
            .keywords
            .push(new_glsl_keyword("mediump", "Medium precision modifier"));
        symbols
            .keywords
            .push(new_glsl_keyword("lowp", "Low precision modifier"));
        // Memory qualifiers
        symbols.keywords.push(new_glsl_keyword("coherent", "Using this qualifier is required to allow dependent shader invocations to communicate with one another, as it enforces the coherency of memory accesses. Using this requires the appropriate memory barriers to be executed, so that visibility can be achieved."));
        symbols.keywords.push(new_glsl_keyword("volatile", "The compiler normally is free to assume that values accessed through variables will only change after memory barriers or other synchronization. With this qualifier, the compiler assumes that the contents of the storage represented by the variable could be changed at any time."));
        symbols.keywords.push(new_glsl_keyword("restrict", "Normally, the compiler must assume that you could access the same image/buffer object through separate variables in the same shader. Therefore, if you write to one variable, and read from a second, the compiler assumes that it is possible that you could be reading the value you just wrote. With this qualifier, you are telling the compiler that this particular variable is the only variable that can modify the memory visible through that variable within this shader invocation (other shader stages don't count here). This allows the compiler to optimize reads/writes better. You should use this wherever possible."));
        symbols.keywords.push(new_glsl_keyword("readonly", "Normally, the compiler allows you to read and write from variables as you wish. If you use this, the variable can only be used for reading operations (atomic operations are forbidden as they also count as writes)."));
        symbols.keywords.push(new_glsl_keyword("writeonly", "Normally, the compiler allows you to read and write from variables as you wish. If you use this, the variable can only be used for writing operations (atomic operations are forbidden as they also count as reads)."));
        symbols.keywords.push(new_glsl_keyword("invariant", ""));
        // Control flow
        symbols.keywords.push(new_glsl_keyword("if", ""));
        symbols.keywords.push(new_glsl_keyword("else", ""));
        symbols.keywords.push(new_glsl_keyword("while", ""));
        symbols.keywords.push(new_glsl_keyword("for", ""));
        symbols.keywords.push(new_glsl_keyword("break", ""));
        symbols.keywords.push(new_glsl_keyword("switch", ""));
        // Types
        symbols.keywords.push(new_glsl_keyword("long", ""));
        symbols.keywords.push(new_glsl_keyword("typedef", ""));
        symbols.keywords.push(new_glsl_keyword("unsigned", ""));
        symbols.keywords.push(new_glsl_keyword("signed", ""));
    }
}
