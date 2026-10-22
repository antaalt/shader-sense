import package::lighting::{ Light, shade as shadeLight };

struct Material {
    albedo: vec3f,
    roughness: f32,
}

struct VertexOutput {
    @builtin(position) position: vec4f,
    @location(0) normal: vec3f,
}

alias Color = vec4<f32>;

const PI: f32 = 3.14159;
override exposure = 1.0;
@group(0) @binding(0) var<uniform> material: Material;
@group(0) @binding(1) var albedoTexture: texture_2d<f32>;

fn computeColor(input: VertexOutput, scale: f32) -> Color {
    let base = material.albedo * scale;
    var result: vec3f = base;
    if (input.normal.y > 0.0) {
        let scopedValue = input.normal.y;
        result = result * scopedValue;
    }
    return Color(result, 1.0);
}

@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4f {
    return computeColor(input, exposure);
}
