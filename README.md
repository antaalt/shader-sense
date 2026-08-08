# Shader sense

[![shader-sense](https://img.shields.io/crates/v/shader-sense?label=shader-sense)](https://crates.io/crates/shader-sense)
[![shader-sense-cli](https://img.shields.io/crates/v/shader-sense?label=shader-sense-cli)](https://crates.io/crates/shader-sense-cli)
[![shader-language-server](https://img.shields.io/crates/v/shader-language-server?label=shader-language-server)](https://crates.io/crates/shader-language-server)

Shader sense is a library for runtime validation and symbol inspection that can handle multiple shader languages, primarily intended for use in a [language server](https://github.com/antaalt/shader-sense/tree/main/shader-language-server). This works through the use of standard API for validation and tree-sitter for symbol inspection. It can be built to desktop or [WASI](https://wasi.dev/). WASI will let the extension run even in browser, but it suffer from limitations. See below for more informations.

- **GLSL** uses [glslang-rs](https://github.com/SnowflakePowered/glslang-rs) as backend. It provide complete linting for GLSL trough glslang API bindings from C.
- **HLSL** uses [hassle-rs](https://github.com/Traverse-Research/hassle-rs) as backend. It provides bindings to directx shader compiler in rust.
- **WGSL** uses [naga](https://github.com/gfx-rs/naga) as backend for linting.

For symbol inspection, the API is relying on abstract syntax tree. As we want to support different language, and also for performances reason as we need to edit AST at runtime, we are using the [tree-sitter](https://tree-sitter.github.io/tree-sitter/) API (instead of standard API), which generate AST with query support, and is already available in a lot of languages.

## Binaries

### Shader language server

This library is used in a language server at [shader-language-server](https://github.com/antaalt/shader-sense/tree/main/shader-language-server). 

### Shader sense cli

This command line interface application is using shader sense to validate and inspect shaders at [shader-sense-cli](https://github.com/antaalt/shader-sense/tree/main/shader-sense-cli).

### Shader intrinsic parser

This library is using an intrinsic json database that is parsed with [shader-intrinsic-parser](https://github.com/antaalt/shader-sense/tree/main/shader-intrinsic-parser).

## Documentation

Documentation can be found on [doc.rs](https://docs.rs/shader-sense/latest/shader_sense/)
<details>
<summary>Lsp configuration json </summary>
<br/>
  
json configuration for the server to be sent through `workspace/configuration` Lsp request or as input when starting the server.

```text
shader-validator: 
    {
      includes: string[] | null                     // Include folders to check
      defines: { string: string } | null            // Defines to set
      pathRemapping: { string: string } | null      // Virtual path remapping
      validate: bool | null                         // Validation via standard API
      symbols: bool | null                          // Query symbols
      symbolDiagnostics: bool | null                // Debug option to visualize issues with tree-sitter
      automaticVariantDiscovery: bool | null        // Reuse a dependent main-file context for document diagnostics
      experimentalMacroExpansion: bool | null       // Experimental test for macro expansion

      stageDefine: {
        ShaderStage: { string: string }
      } | null                                      // Macros defined per shader stage

      trace: {
        server: "off" | "messages" | "verbose"
      } | null                                      // Level of server tracing

      severity: string | null                       // Diagnostic severity to display
      configOverride: string | null                 // Override configuration file

      hlsl: {
        shaderModel: HlslShaderModel | null
        version: HlslVersion | null
        enable16bitTypes: bool | null
        spirv: bool | null
      } | null                                      // HLSL-specific configuration

      glsl: {
        targetClient: GlslTargetClient | null
        spirvVersion: GlslSpirvVersion | null
        preamble: string | null                     // Path to a preamble file
      } | null                                      // GLSL-specific configuration
    }

    ShaderStage =
      "vertex"
      | "fragment"                                  // aka pixel shader
      | "compute"
      | "tesselationControl"                        // aka hull shader
      | "tesselationEvaluation"                     // aka domain shader
      | "mesh"
      | "task"                                      // aka amplification shader
      | "geometry"
      | "rayGeneration"
      | "closestHit"
      | "anyHit"
      | "callable"
      | "miss"
      | "intersect"

    HlslShaderModel =
      "ShaderModel1"
      | "ShaderModel1_1"
      | "ShaderModel1_2"
      | "ShaderModel1_3"
      | "ShaderModel1_4"
      | "ShaderModel2"
      | "ShaderModel3"
      | "ShaderModel4"
      | "ShaderModel4_1"
      | "ShaderModel5"
      | "ShaderModel5_1"
      | "ShaderModel6"
      | "ShaderModel6_1"
      | "ShaderModel6_2"
      | "ShaderModel6_3"
      | "ShaderModel6_4"
      | "ShaderModel6_5"
      | "ShaderModel6_6"
      | "ShaderModel6_7"
      | "ShaderModel6_8"                            // default

    HlslVersion =
      "V2016"
      | "V2017"
      | "V2018"
      | "V2021"                                     // default

    GlslTargetClient =
      "Vulkan1_0"
      | "Vulkan1_1"
      | "Vulkan1_2"
      | "Vulkan1_3"                                 // default
      | "OpenGL450"

    GlslSpirvVersion =
      "SPIRV1_0"
      | "SPIRV1_1"
      | "SPIRV1_2"
      | "SPIRV1_3"
      | "SPIRV1_4"
      | "SPIRV1_5"
      | "SPIRV1_6"                                  // default
    }
```
</details>

## Build for WASI

The library can be built using [WASI](https://wasi.dev/) for web support. We are using threads so we target the thread version.

To build it, install target first :

```shell
rustup target add wasm32-wasip1-threads
```

Then build the app with:

```shell
cargo build --target wasm32-wasip1-threads
```

### Dependencies

You will need to install clang. You will also need to setup the environment variable `CC` as well targetting a clang executable that can handle WASI (default visual studio clang cannot). You can find one in [WASI SDK](https://github.com/WebAssembly/wasi-sdk) so that cc-rs can build c correctly.

### DirectX Shader Compiler issue

Right now, the server can lint hlsl sm 6 through [hassle-rs](https://github.com/Traverse-Research/hassle-rs). It relies on [DirectX Shader Compiler](https://github.com/microsoft/DirectXShaderCompiler) which cannot be built statically. Or, WASI cannot handle dll as of now, and so we need to compile it statically to link it. There is an [ongoing issue](https://github.com/Traverse-Research/hassle-rs/issues/57) for that at hassle rs, but it seems to be complicated, as explained [here](https://devlog.hexops.com/2024/building-the-directx-shader-compiler-better-than-microsoft/). So with WASI, this extension relies instead on glslang to lint hlsl. It only support basic features of shader models 6.0 and some of upper versions, but many recent added features will be missing from linter. As of now, there is not much way to fix this easily, except hoping that Microsoft does something about this.
