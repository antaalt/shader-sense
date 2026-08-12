## Shader Sense CLI

Command line interface to interact with shader-sense. 

You can validate shaders using common API (HLSL via DXC, GLSL via glslang and WGSL via naga) and look for symbols into the file. Note that it is using tree-sitter, a third party library and is not relying on common API, which means the result might not be exact.

## How to use

Use `--help` argument to show all up to date available options.

Example:

```bash
shader-sense-cli --hlsl -E main -S vertex shader.hlsl
```