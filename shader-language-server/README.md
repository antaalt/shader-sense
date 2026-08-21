# Shader language server

[![shader_language_server](https://img.shields.io/crates/v/shader_language_server)](https://crates.io/crates/shader_language_server)

This application is a language server for shaders (HLSL, GLSL, WGSL) that is mainly meant to be used as a server for vscode extension [shader-validator](https://github.com/antaalt/shader-validator). It is following the [language server protocol](https://microsoft.github.io/language-server-protocol/) to communicate with the extension so it could be used with any editor supporting it. It can be built to desktop or [WASI](https://wasi.dev/). WASI will let the server run even in the browser, but it suffer from limitations. See below for more informations.

## How to use

It can be launched using the following options:
```bash
    --config <cfg>              Pass a custom config as a JSON string for server. See configuration section for the expected format.
    --config-file <file>        Pass a custom config as a file for server. See configuration section for the expected format.
    --cwd <dir>                 Set current working directory of server. If not set, will be the server executable path.
    --version | -v              Print server version and exit.
    --help | -h                 Print this helper and exit.
    --hlsl                      Add support for hlsl language id.
    --glsl                      Add support for glsl language id.
    --wgsl                      Add support for wgsl language id.
    --stdio                     Use the stdio transport. Default transport.
    --tcp-listen <IP addr>      Listen for a connection on given address.
    --tcp-connect <IP addr>     Connect to a tcp stream at given address.
```

## Features

This language server support a few options :

- **Diagnostics**: lint the code as you type.
- **Completion**: suggest completion values as you type.
- **Signature**: view the signatures of the current function.
- **Hover**: view the declaration of an element by hovering it.
- **Goto**: allow to go to declaration of an element.
- **Document symbol**: Request symbols for document.
- **Workspace symbol**: Request symbols for workspace.
- **Inactive regions**: Detect inactive preprocessor regions and disable them.

The server support HLSL, GLSL, WGSL diagnostics, but symbol requests are not implemented for WGSL yet.

## Configuration

The server can be passed configuration at startup via options `--config` or `--config-file`, and updated via notification `workspace/didChangeConfiguration`.

Note that in Rust, we use snake_case, but server expect configuration input in camelCase.

It expects the following structure (note that every field is optionnal):

```typescript
interface ServerConfig {
    includes: string[]?, // Includes folder to check. Default empty.
    defines: { string: string }?, // Defines to set. Default empty.
    pathRemapping: { string: string }?, // Virtual path remapping. Default empty.
    validate: boolean?, // Validation via standard API. Default is true.
    symbols: boolean?, // Query symbols. Default is true.
    symbolDiagnostics: boolean?, // Debug option to visualise issues with tree-sitter. Default is false.
    automaticVariantDiscovery: boolean?, // Reuse a dependent main-file context for document diagnostics. Default is false.
    experimentalMacroExpansion: boolean?, // Experimental test for macro expansion. Default is false.
    validateConfig: boolean?, // Validate configuration. Default is true.
    stageDefine: {
        'vertex': { string: string }?,
        'fragment': { string: string }?,
        'compute': { string: string }?,
        'tesselationControl': { string: string }?,
        'tesselationEvaluation': { string: string }?,
        'mesh': { string: string }?,
        'task': { string: string }?,
        'geometry': { string: string }?,
        'rayGeneration': { string: string }?,
        'closestHit': { string: string }?,
        'anyHit': { string: string }?,
        'callable': { string: string }?,
        'miss': { string: string }?,
        'intersect': { string: string }?,
    }?, // Specific macro defined per shader stage. Default empty.
    trace: 'off' | 'messages' | 'verbose' | null, // Level of error to display. Default to off.
    severity: 'error' | 'warning'| 'info'| 'hint' | null, // Severity of diagnostic to display. Default to error.
    configOverride: string?, // Path to a JSON containig some configuration formatted following `ServerConfigOverride`. Default empty.
    hlsl: {
        shaderModel: 'ShaderModel6' | 'ShaderModel6_1' | 'ShaderModel6_2' | 'ShaderModel6_3' | 'ShaderModel6_4' | 'ShaderModel6_5' | 'ShaderModel6_6' | 'ShaderModel6_7' | 'ShaderModel6_8' | null, // HLSL shader model to use. Default to ShaderModel6_8.
        version: 'V2016' | 'V2017' | 'V2018' | 'V2021' | null, // HLSL version to use. Default to V2021.
        enable16bitTypes: boolean?, // Enable 16 bit types with HLSL. Default to false.
        spirv: boolean?, // Use SPIRV with HLSL. Default to false.
    }?, // Hlsl specific configuration
    glsl: {
        targetClient: 'None' | 'Vulkan1_0' | 'Vulkan1_1' | 'Vulkan1_2' | 'Vulkan1_3' | 'OpenGL450' | null, // Target client to use. Default to Vulkan1_3.
        spirvVersion: 'None' | 'SPIRV1_0' | 'SPIRV1_1' | 'SPIRV1_2' | 'SPIRV1_3' | 'SPIRV1_4' | 'SPIRV1_5' | 'SPIRV1_6' | null, // Spirv version to target. Default to SPIRV1_6.
        preamble: string?, // Path to a file which content will be added at start of every GLSL files. Default empty.
    }?, // Glsl specific configuration
}
// Configuration that can be loaded by server through configOverride option. Useful for engine specific configuration to be swapped.
interface ServerConfigOverride {
    includes: string[]?,
    defines: { string: string }?,
    pathRemapping: { string: string }?,
    stageDefine: { ... }?, // Same as ServerConfig
    hlsl: { ... }?, // Same as ServerConfig
    glsl: { ... }?, // Same as ServerConfig
}
```

## Transport

### Stdio

The communication with the server is done via stdin and stdout, while the logs are printed into stderr.

### Tcp Listen / Connect

The communication with the server is done via tcp stream, it can listen for a client connection, or connect directly to a listening client. Logs are still print to stderr.

## Specific features

The server follows the language server protocol, but it also offer some custom commands specific to this server. Handling them is not mandatory but can improve the experience using the extension.

### Shader variant commands

This server offer a variant concept to handle shader database which can have a lot of entry points, even in a single shader file. 
In order to offer a better experience with all providers and active regions, you can specify the current variant, aka current entry point, along with some macro and includes for the permutation. 
Your client can have an interface letting user create variants and select the active one, which will be sent to server through the notification `textDocument/didChangeShaderVariant`.

- Change shader variant notification: `textDocument/didChangeShaderVariant`. Set it to null to remove current variant.

```typescript
interface DidChangeShaderVariantParams {
    shaderVariant: ShaderVariant | null
}

interface ShaderVariant {
    url: string, // file of variant
    shadingLanguage: string, // language id of variant
    entryPoint: string, // The name of the entry point function.
    stage: string | null, // Correspond to the value of the enum ShaderStage in shader-sense, case sensitive. 
    defines: Object, // defines and its values
    includes: string[], // include folders for this variant
}
```

### Compilation commands

As this server has everything in its hand to compile a shader, we can simply get the resulting binary via a custom request. Use request `textDocument/compilationResult` to get the compilation result as binary.

```typescript
interface CompilationRequestParams {
    uri: string
}
interface CompilationRequestResult {
    // The requested file might require to be a variant
    ty: 'Spirv' | 'Dxil' | 'None', // If None, compilation probably failed. Check diagnostics.
    data: Uint8Array // The result of the compilation.
}
```

### Debug commands:

The server offer some specific debug request to help inspect the current state of the server.

- Dump AST request: "debug/dumpAst"
```typescript
interface DumpAstParams {
    uri: string,
}
```
Result will be either a string or null

- Dump dependencies request: "debug/dumpDependency"
```typescript
interface DumpDependencyParams {
    uri: string,
}
```
Result will be either a string or null

## Behind the hood

### Diagnostics

Diagnostics are generated following language specifics API:

- **GLSL** uses [glslang-rs](https://github.com/SnowflakePowered/glslang-rs) as backend. It provide complete linting for GLSL trough glslang API bindings from C.
- **HLSL** uses [hassle-rs](https://github.com/Traverse-Research/hassle-rs) as backend. It provides bindings to directx shader compiler in rust.
- **WGSL** uses [naga](https://github.com/gfx-rs/naga) as backend for linting.

### Symbols

Symbols are retrieved using queries based on [tree-sitter](https://tree-sitter.github.io/tree-sitter/) API.

## Web support

This server can be run in the browser when compiled to [WASI](https://wasi.dev/). Because of this restriction, we can't use dxc here as it does not compile to WASI and instead rely on glslang, which is more limited in linting (Only support some basic features of SM 6.0, while DXC support all newly added SM (current 6.8)).
